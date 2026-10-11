const { chromium } = require("playwright");
const fs = require("node:fs/promises");
const path = require("node:path");

const BASE_URL = process.env.CHECKERBOARDR_URL || "http://127.0.0.1:3847/";
const TIMEOUT = Number(process.env.CHECKERBOARDR_TIMEOUT_MS || 120000);
const ARTIFACT_DIR = path.resolve(process.env.CHECKERBOARDR_ARTIFACT_DIR || "test-results/checkerboardr-exports");

async function setSelect(page, selector, value) {
  await page.locator(selector).evaluate((element, selected) => element.selectize.setValue(selected), value);
  await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), null, { timeout: TIMEOUT });
}

async function run() {
  await fs.mkdir(ARTIFACT_DIR, { recursive: true });
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PLAYWRIGHT_CHROME_PATH || undefined,
  });
  const page = await browser.newPage({ acceptDownloads: true });
  const errors = [];
  const results = [];
  page.on("pageerror", error => errors.push(error.message));
  page.on("console", message => {
    if (message.type() === "error" && !message.text().includes("favicon")) errors.push(message.text());
  });

  try {
    await page.goto(BASE_URL, { waitUntil: "domcontentloaded", timeout: TIMEOUT });
    await page.locator('a[data-value="Data visualization"]').click();
    await page.locator("#checkerboardStatsTable table").waitFor({ timeout: TIMEOUT });
    await setSelect(page, "#synergyModel", "ZIP");

    const formats = [
      { id: "downloadPlotPDF", ext: "pdf", signature: buffer => buffer.subarray(0, 4).toString() === "%PDF" },
      { id: "downloadPlotSVG", ext: "svg", signature: buffer => buffer.toString("utf8", 0, 500).includes("<svg") },
      { id: "downloadPlotEPS", ext: "eps", signature: buffer => buffer.subarray(0, 10).toString().startsWith("%!PS-Adobe") },
    ];
    for (const engine of ["2d_ggplot", "1d_curves", "3d_base"]) {
      await setSelect(page, "#plotEngine", engine);
      for (const format of formats) {
        const [download] = await Promise.all([
          page.waitForEvent("download", { timeout: TIMEOUT }),
          page.locator(`#${format.id}`).click(),
        ]);
        const target = path.join(ARTIFACT_DIR, `${engine}.${format.ext}`);
        await download.saveAs(target);
        const content = await fs.readFile(target);
        const passed = content.length > 500 && format.signature(content)
          && download.suggestedFilename().endsWith(`.${format.ext}`);
        results.push({ engine, format: format.ext, passed, bytes: content.length });
        console.log(`[${passed ? "PASS" : "FAIL"}] ${engine} ${format.ext} (${content.length} bytes)`);
      }
    }
  } finally {
    await browser.close();
  }

  const report = {
    baseURL: BASE_URL,
    generatedAt: new Date().toISOString(),
    summary: { total: results.length, passed: results.filter(result => result.passed).length,
      failed: results.filter(result => !result.passed).length },
    results,
    errors,
  };
  await fs.writeFile(path.join(ARTIFACT_DIR, "report.json"), `${JSON.stringify(report, null, 2)}\n`);
  console.log(`Summary: ${JSON.stringify(report.summary)}`);
  if (report.summary.failed || errors.length) process.exitCode = 1;
}

run().catch(error => { console.error(error); process.exitCode = 1; });
