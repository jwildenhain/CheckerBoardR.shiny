/* CheckerboardR capability interface regression (2D heatmap only). */
const { chromium } = require("playwright");
const crypto = require("node:crypto");
const fs = require("node:fs/promises");
const path = require("node:path");

const BASE_URL = process.env.CHECKERBOARDR_URL || "http://127.0.0.1:3847/";
const TIMEOUT = Number(process.env.CHECKERBOARDR_TIMEOUT_MS || 120000);
const ARTIFACT_DIR = path.resolve(process.env.CHECKERBOARDR_ARTIFACT_DIR || "test-results/checkerboardr-capabilities");
const results = [];
const events = [];

const sha256 = buffer => crypto.createHash("sha256").update(buffer).digest("hex");
const record = (id, passed, detail = {}) => {
  results.push({ id, status: passed ? "passed" : "failed", ...detail });
  console.log(`[${passed ? "PASS" : "FAIL"}] ${id}${detail.message ? ` — ${detail.message}` : ""}`);
};

async function waitIdle(page) {
  await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), null, { timeout: TIMEOUT });
  await page.waitForTimeout(250);
}

async function setSelect(page, selector, value) {
  await page.locator(selector).evaluate((element, next) => {
    if (element.selectize) element.selectize.setValue(next);
    else {
      element.value = next;
      element.dispatchEvent(new Event("change", { bubbles: true }));
    }
  }, value);
  await waitIdle(page);
  await waitHeatmap(page);
}

async function setSelectWithoutPlot(page, selector, value) {
  await page.locator(selector).evaluate((element, next) => {
    if (element.selectize) element.selectize.setValue(next);
    else {
      element.value = next;
      element.dispatchEvent(new Event("change", { bubbles: true }));
    }
  }, value);
  await waitIdle(page);
}

async function waitHeatmap(page, old = null) {
  await page.locator("#ggplotPlot img").waitFor({ state: "visible", timeout: TIMEOUT });
  await page.waitForFunction(previous => {
    const image = document.querySelector("#ggplotPlot img");
    return image?.complete && image.naturalWidth > 100 && image.naturalHeight > 100
      && (!previous || image.getAttribute("src") !== previous);
  }, old, { timeout: TIMEOUT });
  await waitIdle(page);
}

async function heatmapHash(page) {
  return sha256(await page.locator("#ggplotPlot").screenshot());
}

async function visibleErrors(page) {
  return page.locator(".shiny-output-error, .shiny-notification-error").evaluateAll(nodes => nodes
    .filter(node => {
      const box = node.getBoundingClientRect();
      const style = getComputedStyle(node);
      return box.width > 0 && box.height > 0 && style.display !== "none" && style.visibility !== "hidden";
    })
    .map(node => node.innerText.trim()).filter(Boolean));
}

function replicateCsv(offsets) {
  const base = [
    [-0.05, 0.12, 0.29, 0.50],
    [-0.02, 0.24, 0.40, 0.63],
    [0.08, 0.40, 0.61, 0.78],
    [0.22, 0.56, 0.74, 0.89],
  ];
  const rows = base.map((row, y) => `${["0uM", ".25uM", "1uM", "4uM"][y]},${row.map((v, x) => (v + offsets[y][x]).toFixed(5)).join(",")}`);
  return `dose,0uM,.5uM,2uM,8uM\n${rows.join("\n")}\n`;
}

async function run() {
  await fs.mkdir(ARTIFACT_DIR, { recursive: true });
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1440, height: 1100 } });
  page.on("pageerror", error => events.push({ type: "pageerror", message: error.message }));
  page.on("console", message => { if (message.type() === "error") events.push({ type: "console-error", message: message.text() }); });
  page.on("requestfailed", request => events.push({ type: "requestfailed", message: `${request.method()} ${request.url()} ${request.failure()?.errorText}` }));

  try {
    await page.goto(BASE_URL, { waitUntil: "domcontentloaded", timeout: TIMEOUT });
    await page.locator('a[data-value="Data upload"]').click();
    await page.locator('input[name="sampleData"][value="2"]').check();
    await waitIdle(page);
    const preview = await page.locator("#filetable table").innerText();
    record("CAP-01-sample-preview-labels", /\d+(?:\.\d+)?uM/.test(preview)
      && !/\bX(?:\d|\.)/.test(preview) && !/(?:^|\s)-?\.\d/.test(preview),
      { preview: preview.slice(0, 500) });

    await page.locator('a[data-value="Data visualization"]').click();
    await setSelect(page, "#plotEngine", "2d_ggplot");
    const matrixViews = {
      HSA: ["score", "reference", "observed"],
      Bliss: ["score", "reference", "observed"],
      Loewe: ["score", "reference", "observed"],
      ZIP: ["score", "reference", "fitted", "observed"],
      Consensus: ["score", "reference", "observed"],
    };
    for (const [model, views] of Object.entries(matrixViews)) {
      await setSelect(page, "#synergyModel", model);
      for (const view of views) {
        await page.waitForFunction(({ selector, value }) => {
          const element = document.querySelector(selector);
          return element?.selectize ? Object.hasOwn(element.selectize.options, value) : [...element.options].some(option => option.value === value);
        }, { selector: "#plotValue", value: view }, { timeout: TIMEOUT });
        await setSelect(page, "#plotValue", view);
        const summary = await page.locator("#checkerboardStatsTable").innerText();
        const errors = await visibleErrors(page);
        record(`CAP-02-${model}-${view}`, summary.toLowerCase().includes(view === "reference" ? "reference" : view === "fitted" ? "fitted" : view === "score" ? "score" : "inhibition") && !errors.length,
               { errors, heatmapSha256: await heatmapHash(page) });
      }
    }

    await setSelect(page, "#synergyModel", "Consensus");
    await setSelect(page, "#plotValue", "score");
    const baselineHash = await heatmapHash(page);
    await page.locator("#flipDataX").check(); await waitIdle(page); await waitHeatmap(page, null);
    const flipXHash = await heatmapHash(page);
    await page.locator("#flipDataY").check(); await waitIdle(page); await waitHeatmap(page, null);
    const flipXYHash = await heatmapHash(page);
    record("CAP-03-axis-flips", new Set([baselineHash, flipXHash, flipXYHash]).size === 3,
           { baselineHash, flipXHash, flipXYHash });
    await page.locator("#flipDataX").uncheck(); await page.locator("#flipDataY").uncheck(); await waitIdle(page);

    await page.waitForFunction(() => document.querySelectorAll("#barometerTable tbody tr").length === 5, null, { timeout: TIMEOUT });
    const barometerText = await page.locator("#barometerTable").innerText();
    record("CAP-04-synergy-barometer", ["HSA", "Bliss", "Loewe", "ZIP", "Consensus"].every(model => barometerText.includes(model)),
           { table: barometerText });

    await page.locator('a[data-value="Data upload"]').click();
    await page.locator('input[name="dataInput"][value="2"]').check();
    await page.locator('input[name="dataType"][value="inhibition"]').check();
    const zero = Array.from({ length: 4 }, () => Array(4).fill(0));
    const up = [[0,.01,-.01,.02],[-.01,.02,.01,-.02],[.01,-.02,.02,.01],[0,.01,-.02,.02]];
    const down = [[0,-.015,.012,-.01],[.012,-.01,.02,.01],[-.01,.015,-.01,.02],[.01,-.02,.01,-.015]];
    await page.locator("#upload").setInputFiles([
      { name: "replicate-1.csv", mimeType: "text/csv", buffer: Buffer.from(replicateCsv(zero)) },
      { name: "replicate-2.csv", mimeType: "text/csv", buffer: Buffer.from(replicateCsv(up)) },
      { name: "replicate-3.csv", mimeType: "text/csv", buffer: Buffer.from(replicateCsv(down)) },
    ]);
    await page.locator("#bootstrapIterations").fill("40");
    await page.locator("#bootstrapIterations").dispatchEvent("change");
    await setSelectWithoutPlot(page, "#baselineMethod", "all");
    await waitIdle(page);
    await page.locator('a[data-value="Data visualization"]').click();
    await setSelect(page, "#synergyModel", "Bliss");
    await setSelect(page, "#plotValue", "score");
    await setSelect(page, "#uncertaintyDisplay", "sem");
    const replicateSummary = await page.locator("#checkerboardStatsTable").innerText();
    record("CAP-05-replicates-baseline-sem", replicateSummary.includes("Independent replicate matrices\t3")
      && replicateSummary.includes("Baseline correction method\tall") && !(await visibleErrors(page)).length,
      { summary: replicateSummary });
    await setSelect(page, "#uncertaintyDisplay", "ci");
    record("CAP-06-bootstrap-ci", !(await visibleErrors(page)).length, { heatmapSha256: await heatmapHash(page) });

    const [download] = await Promise.all([
      page.waitForEvent("download", { timeout: TIMEOUT }),
      page.locator("#downloadMatrixCSV").click(),
    ]);
    const csvPath = path.join(ARTIFACT_DIR, "score-reference-export.csv");
    await download.saveAs(csvPath);
    const csv = await fs.readFile(csvPath, "utf8");
    const required = ["HSA_reference", "Bliss_score", "Loewe_reference", "ZIP_fitted", "Consensus_score", "Bliss_score_sem", "ZIP_fitted_ci_upper"];
    record("CAP-07-score-reference-export", required.every(name => csv.split("\n")[0].includes(name)),
           { bytes: Buffer.byteLength(csv), header: csv.split("\n")[0] });

    await page.screenshot({ path: path.join(ARTIFACT_DIR, "final-state.png"), fullPage: true });
  } finally {
    await browser.close();
  }

  const report = {
    baseURL: BASE_URL,
    generatedAt: new Date().toISOString(),
    summary: { total: results.length, passed: results.filter(r => r.status === "passed").length, failed: results.filter(r => r.status === "failed").length },
    results,
    events,
  };
  await fs.writeFile(path.join(ARTIFACT_DIR, "report.json"), `${JSON.stringify(report, null, 2)}\n`);
  const markdown = [`# CheckerboardR capability interface report`, ``, `- Total: ${report.summary.total}`, `- Passed: ${report.summary.passed}`, `- Failed: ${report.summary.failed}`, ``,
    ...results.map(result => `- [${result.status === "passed" ? "x" : " "}] ${result.id}`), ``].join("\n");
  await fs.writeFile(path.join(ARTIFACT_DIR, "report.md"), markdown);
  console.log(`Summary: ${JSON.stringify(report.summary)}`);
  if (report.summary.failed || events.some(event => event.type !== "requestfailed" || !event.message.includes("favicon"))) process.exitCode = 1;
}

run().catch(error => { console.error(error); process.exitCode = 1; });
