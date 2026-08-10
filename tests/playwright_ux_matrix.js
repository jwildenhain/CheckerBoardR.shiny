/*
 * Full run:
 *   CHECKERBOARDR_URL=http://checkerboardr.example/ node tests/playwright_ux_matrix.js
 *
 * Comma-separated filters:
 *   CHECKERBOARDR_DATASETS=edge-small CHECKERBOARDR_MODELS=Bliss,ZIP
 *   CHECKERBOARDR_ENGINES=2d_ggplot CHECKERBOARDR_FIT_MODES=fit,fallback
 */
const { chromium } = require("playwright");
const crypto = require("node:crypto");
const fs = require("node:fs/promises");
const path = require("node:path");

const BASE_URL = process.env.CHECKERBOARDR_URL || "http://127.0.0.1:3847/";
const ARTIFACT_DIR = path.resolve(
  process.env.CHECKERBOARDR_ARTIFACT_DIR || "test-results/checkerboardr-ux"
);
const TIMEOUT_MS = Number(process.env.CHECKERBOARDR_TIMEOUT_MS || 120000);
const ZERO_TOLERANCE = Number(process.env.CHECKERBOARDR_ZERO_TOLERANCE || 0.00005);
const HEATMAP_LABEL_DECIMALS = 2;

const LOW_MAGNITUDE_INTERACTION_JSON = JSON.stringify({
  drug_a: "Small-effect drug A",
  drug_b: "Small-effect drug B",
  concentrations_a: [0, 1, 2, 4],
  concentrations_b: [0, 1, 2, 4],
  data_type: "inhibition",
  matrix: [
    [0, 0.1, 0.2, 0.3],
    [0.12, 0.209, 0.297, 0.386],
    [0.22, 0.299, 0.377, 0.457],
    [0.32, 0.389, 0.456, 0.527],
  ],
  settings: {
    synergy_model: "Bliss",
    plot_engine: "2d_ggplot",
    orientation: "synergism",
  },
});

const DATASETS = [
  { id: "1", source: "sample", name: "anti-fungal-tab", label: "Anti-fungal Screening (testData3.tab)" },
  { id: "2", source: "sample", name: "anticancer-synergy-tab", label: "Synthetic Anticancer Synergy Grid (anticancer_synergy.tab)" },
  { id: "3", source: "sample", name: "antagonism-csv", label: "Antagonistic Combination (antagonism.csv)" },
  { id: "4", source: "sample", name: "chemotherapy-json", label: "Chemotherapy Grid (paclitaxel_carboplatin.json)" },
  { id: "5", source: "sample", name: "antifungal-xml", label: "Antifungal Grid (fluconazole_voriconazole.xml)" },
  { id: "6", source: "sample", name: "excel-grid", label: "Excel Spreadsheet Grid (testData.xlsx)" },
  {
    id: "edge-small",
    source: "paste",
    name: "low-magnitude-interaction-json",
    label: "Synthetic low-magnitude interaction JSON",
    content: LOW_MAGNITUDE_INTERACTION_JSON,
  },
];

const MODELS = ["Bliss", "HSA", "Loewe", "ZIP", "Data"];
const ENGINES = [
  { id: "2d_ggplot", name: "2D heatmap", output: "image" },
  { id: "3d_plotly", name: "3D Plotly surface", output: "plotly" },
  { id: "1d_curves", name: "1D single-agent curves", output: "image" },
  { id: "3d_base", name: "3D Base R surface", output: "image" },
];
const FIT_MODES = [
  { id: "fit", enabled: true },
  { id: "fallback", enabled: false },
];

function selectByEnvironment(items, variable, key = "id") {
  const requested = (process.env[variable] || "")
    .split(",")
    .map(value => value.trim())
    .filter(Boolean);
  return requested.length ? items.filter(item => requested.includes(item[key])) : items;
}

function numeric(value) {
  const parsed = Number.parseFloat(String(value).replace(/,/g, ""));
  return Number.isFinite(parsed) ? parsed : null;
}

function safeName(value) {
  return value.replace(/[^a-z0-9_.-]+/gi, "-").replace(/^-|-$/g, "").toLowerCase();
}

function sha256(buffer) {
  return crypto.createHash("sha256").update(buffer).digest("hex");
}

async function waitForShinyIdle(page) {
  await page.waitForTimeout(100);
  await page.waitForFunction(
    () => !document.documentElement.classList.contains("shiny-busy"),
    undefined,
    { timeout: TIMEOUT_MS }
  );
  await page.waitForTimeout(150);
}

async function setSelectize(page, selector, value) {
  await page.locator(selector).evaluate((element, nextValue) => {
    if (element.selectize) {
      element.selectize.setValue(nextValue);
      return;
    }
    element.value = nextValue;
    element.dispatchEvent(new Event("input", { bubbles: true }));
    element.dispatchEvent(new Event("change", { bubbles: true }));
  }, value);
  await waitForShinyIdle(page);
}

async function readDataPreview(page) {
  await page.locator("#filetable table").waitFor({ state: "visible", timeout: TIMEOUT_MS });
  return page.locator("#filetable table").evaluate(table => {
    const rows = [...table.querySelectorAll("tbody tr")];
    return {
      rows: rows.length,
      columns: rows[0]?.querySelectorAll("th, td").length || 0,
      text: table.innerText.slice(0, 800),
    };
  });
}

async function readStatistics(page, expectedModel) {
  await page.locator("#checkerboardStatsTable").waitFor({ state: "visible", timeout: TIMEOUT_MS });
  const expectedMetric = expectedModel === "Data" ? "Inhibition" : `${expectedModel} Score`;
  await page.waitForFunction(
    metric => {
      const rows = [...document.querySelectorAll("#checkerboardStatsTable tbody tr")];
      const firstMetric = rows[0]?.querySelector("th, td")?.innerText || "";
      return rows.length >= 9 && firstMetric.includes(metric);
    },
    expectedMetric,
    { timeout: TIMEOUT_MS }
  );
  const rows = await page.locator("#checkerboardStatsTable tbody tr").evaluateAll(elements =>
    elements.map(row => [...row.querySelectorAll("th, td")].map(cell => cell.innerText.trim()))
  );
  const entries = Object.fromEntries(rows.filter(row => row.length >= 2).map(row => [row[0], row[1]]));
  const maxEntry = Object.entries(entries).find(([name]) => name.startsWith("Max Synergy /"));
  const minEntry = Object.entries(entries).find(([name]) => name.startsWith("Max Antagonism / Min"));
  const meanEntry = Object.entries(entries).find(([name]) => name === "Mean Score across Screening Grid");
  const scores = {
    max: numeric(maxEntry?.[1]),
    min: numeric(minEntry?.[1]),
    mean: numeric(meanEntry?.[1]),
  };
  return { entries, rows, scores };
}

async function visibleShinyErrors(page) {
  return page.locator(".shiny-output-error, .shiny-notification-error").evaluateAll(elements =>
    elements
      .filter(element => {
        const style = getComputedStyle(element);
        const rect = element.getBoundingClientRect();
        return style.display !== "none" && style.visibility !== "hidden" && rect.width > 0 && rect.height > 0;
      })
      .map(element => element.innerText.trim() || element.textContent.trim())
      .filter(Boolean)
  );
}

async function inspectRenderedImage(page) {
  const image = page.locator("#ggplotPlot img");
  await image.waitFor({ state: "visible", timeout: TIMEOUT_MS });
  await page.waitForFunction(
    () => {
      const element = document.querySelector("#ggplotPlot img");
      return element?.complete && element.naturalWidth > 10 && element.naturalHeight > 10;
    },
    undefined,
    { timeout: TIMEOUT_MS }
  );
  const diagnostics = await image.evaluate(element => {
    const width = 140;
    const height = 100;
    const canvas = document.createElement("canvas");
    canvas.width = width;
    canvas.height = height;
    const context = canvas.getContext("2d", { willReadFrequently: true });
    context.drawImage(element, 0, 0, width, height);
    const pixels = context.getImageData(0, 0, width, height).data;
    const colours = new Set();
    let minLuminance = 255;
    let maxLuminance = 0;
    let nonWhite = 0;
    let sampled = 0;
    for (let y = 8; y < height - 8; y += 2) {
      for (let x = 8; x < width - 8; x += 2) {
        const offset = (y * width + x) * 4;
        const [red, green, blue, alpha] = pixels.slice(offset, offset + 4);
        if (alpha < 16) continue;
        const quantized = `${Math.round(red / 8)},${Math.round(green / 8)},${Math.round(blue / 8)}`;
        colours.add(quantized);
        const luminance = 0.2126 * red + 0.7152 * green + 0.0722 * blue;
        minLuminance = Math.min(minLuminance, luminance);
        maxLuminance = Math.max(maxLuminance, luminance);
        if (red < 245 || green < 245 || blue < 245) nonWhite += 1;
        sampled += 1;
      }
    }
    return {
      naturalWidth: element.naturalWidth,
      naturalHeight: element.naturalHeight,
      uniqueQuantizedColours: colours.size,
      luminanceRange: Number((maxLuminance - minLuminance).toFixed(2)),
      nonWhiteFraction: sampled ? Number((nonWhite / sampled).toFixed(4)) : 0,
    };
  });
  const screenshot = await page.locator("#ggplotPlot").screenshot();
  return {
    rendered: true,
    diagnostics,
    screenshot,
    screenshotBytes: screenshot.length,
    screenshotSha256: sha256(screenshot),
  };
}

async function inspectPlotly(page) {
  await page.locator("#plotlyPlot .plotly").waitFor({ state: "visible", timeout: TIMEOUT_MS });
  const diagnostics = await page.locator("#plotlyPlot").evaluate(root => {
    const candidates = [root, ...root.querySelectorAll("*")];
    const carrier = candidates.find(element => Array.isArray(element.data) && element.data.length);
    return {
      plotContainers: root.querySelectorAll(".plot-container.plotly").length,
      canvases: root.querySelectorAll("canvas").length,
      svgs: root.querySelectorAll("svg").length,
      traceTypes: carrier?.data?.map(trace => trace.type || "unknown") || [],
      width: Math.round(root.getBoundingClientRect().width),
      height: Math.round(root.getBoundingClientRect().height),
    };
  });
  const screenshot = await page.locator("#plotlyPlot").screenshot();
  return {
    rendered: diagnostics.plotContainers > 0 && diagnostics.width > 10 && diagnostics.height > 10,
    diagnostics,
    screenshot,
    screenshotBytes: screenshot.length,
    screenshotSha256: sha256(screenshot),
  };
}

function displayedScoresAreZero(scores) {
  return [scores.max, scores.min, scores.mean].every(
    value => value !== null && Math.abs(value) < ZERO_TOLERANCE
  );
}

function nonZeroScoresRoundToZeroAtHeatmapPrecision(scores) {
  const extrema = [scores.max, scores.min].filter(value => value !== null);
  if (!extrema.length) return false;
  const maxAbsoluteScore = Math.max(...extrema.map(Math.abs));
  const displayThreshold = 0.5 * (10 ** -HEATMAP_LABEL_DECIMALS);
  return maxAbsoluteScore >= ZERO_TOLERANCE && maxAbsoluteScore < displayThreshold;
}

function scoresMatch(left, right) {
  return ["max", "min", "mean"].every(key => left[key] === right[key]);
}

async function writeFailureScreenshot(page, context) {
  const name = [context.dataset, context.fitMode, context.model, context.engine]
    .map(safeName)
    .join("__");
  const target = path.join(ARTIFACT_DIR, "failures", `${name}.png`);
  await fs.mkdir(path.dirname(target), { recursive: true });
  await page.screenshot({ path: target, fullPage: true });
  return path.relative(process.cwd(), target);
}

function createMarkdownReport(report) {
  const lines = [
    "# CheckerBoardR UX matrix report",
    "",
    `- URL: ${report.baseURL}`,
    `- Generated: ${report.generatedAt}`,
    `- Combinations: ${report.summary.total}`,
    `- Passed: ${report.summary.passed}`,
    `- Failed: ${report.summary.failed}`,
    `- All-zero score combinations: ${report.summary.allZero}`,
    `- Zero-score + coloured-heatmap cases: ${report.summary.zeroWithColouredHeatmap}`,
    `- Nonzero scores rounded to zero on coloured heatmaps: ${report.summary.nonZeroRoundedToZero}`,
    `- Browser/Shiny events: ${report.events.length}`,
    "",
    "## Issues",
    "",
    "| Dataset | Fit mode | Model | Engine | Issue | Details |",
    "|---|---|---|---|---|---|",
  ];
  const issueRows = report.results.flatMap(result =>
    result.issues.map(issue => [
      result.dataset,
      result.fitMode,
      result.model,
      result.engine,
      issue.code,
      String(issue.detail || "").replace(/\|/g, "\\|").replace(/\n/g, " "),
    ])
  );
  if (!issueRows.length) lines.push("| — | — | — | — | None | All combinations passed | ");
  for (const row of issueRows) lines.push(`| ${row.join(" | ")} |`);
  lines.push("", "## Browser and network events", "");
  if (!report.events.length) lines.push("No browser, console, request, or HTTP errors were captured.");
  for (const event of report.events) {
    const context = [event.context?.dataset, event.context?.fitMode, event.context?.model, event.context?.engine]
      .filter(Boolean)
      .join(" / ");
    lines.push(`- **${event.type}** ${context ? `(${context}) ` : ""}${event.message}`);
  }
  return `${lines.join("\n")}\n`;
}

async function run() {
  const datasets = selectByEnvironment(DATASETS, "CHECKERBOARDR_DATASETS");
  const models = selectByEnvironment(MODELS.map(id => ({ id })), "CHECKERBOARDR_MODELS").map(item => item.id);
  const engines = selectByEnvironment(ENGINES, "CHECKERBOARDR_ENGINES");
  const fitModes = selectByEnvironment(FIT_MODES, "CHECKERBOARDR_FIT_MODES");
  await fs.mkdir(ARTIFACT_DIR, { recursive: true });

  const browser = await chromium.launch({ headless: process.env.HEADED !== "1" });
  const results = [];
  const events = [];
  let activeContext = {};

  try {
    for (const dataset of datasets) {
      const page = await browser.newPage({ acceptDownloads: false });
      page.setDefaultTimeout(TIMEOUT_MS);
      page.on("pageerror", error => events.push({ type: "pageerror", message: error.message, context: { ...activeContext } }));
      page.on("console", message => {
        if (["error", "warning", "warn"].includes(message.type())) {
          events.push({ type: `console-${message.type()}`, message: message.text(), context: { ...activeContext } });
        }
      });
      page.on("requestfailed", request => events.push({
        type: "requestfailed",
        message: `${request.method()} ${request.url()} — ${request.failure()?.errorText || "unknown error"}`,
        context: { ...activeContext },
      }));
      page.on("response", response => {
        if (response.status() >= 400) {
          events.push({
            type: `http-${response.status()}`,
            message: `${response.request().method()} ${response.url()}`,
            context: { ...activeContext },
          });
        }
      });

      try {
        activeContext = { dataset: dataset.name };
        await page.goto(BASE_URL, { waitUntil: "domcontentloaded", timeout: TIMEOUT_MS });
        await page.locator(".version-badge").waitFor({ state: "visible", timeout: TIMEOUT_MS });
        await page.locator('a[data-value="Data upload"]').waitFor({ state: "visible", timeout: TIMEOUT_MS });
        await page.locator('a[data-value="Data upload"]').click();
        await page.locator("#filetable table").waitFor({ state: "visible", timeout: TIMEOUT_MS });
        const initialPreviewText = await page.locator("#filetable table").innerText();
        if (dataset.source === "paste") {
          await page.locator('input[name="dataInput"][value="3"]').check();
          await page.locator("#myData").waitFor({ state: "visible", timeout: TIMEOUT_MS });
          await page.locator("#myData").fill(dataset.content);
          await page.locator("#myData").dispatchEvent("change");
          await page.waitForFunction(
            initialText => {
              const tableText = document.querySelector("#filetable table")?.innerText || "";
              return tableText !== initialText && tableText.includes("4uM") && tableText.includes("0.21");
            },
            initialPreviewText,
            { timeout: TIMEOUT_MS }
          );
        } else {
          await page.locator(`input[name="sampleData"][value="${dataset.id}"]`).check();
          if (dataset.id !== "1") {
            await page.waitForFunction(
              ({ id, initialText }) => {
                const selected = document.querySelector('input[name="sampleData"]:checked')?.value;
                const tableText = document.querySelector("#filetable table")?.innerText || "";
                return selected === id && tableText !== initialText;
              },
              { id: dataset.id, initialText: initialPreviewText },
              { timeout: TIMEOUT_MS }
            );
          }
        }
        await waitForShinyIdle(page);
        const preview = await readDataPreview(page);
        await page.locator('a[data-value="Data visualization"]').click();
        await waitForShinyIdle(page);

        for (const fitMode of fitModes) {
          await page.locator("#useFit").setChecked(fitMode.enabled);
          await waitForShinyIdle(page);

          for (const model of models) {
            activeContext = { dataset: dataset.name, fitMode: fitMode.id, model };
            let referenceStatistics;
            try {
              await setSelectize(page, "#synergyModel", model);
              referenceStatistics = await readStatistics(page, model);
            } catch (error) {
              const context = { ...activeContext, engine: "calculation" };
              results.push({
                ...context,
                datasetLabel: dataset.label,
                preview,
                status: "failed",
                issues: [{ code: "CALCULATION_ERROR", detail: error.message }],
                screenshot: await writeFailureScreenshot(page, context),
              });
              continue;
            }

            for (const engine of engines) {
              activeContext = { dataset: dataset.name, fitMode: fitMode.id, model, engine: engine.id };
              const eventStart = events.length;
              const issues = [];
              let renderedOutput = null;
              let engineStatistics = null;

              try {
                await setSelectize(page, "#plotEngine", engine.id);
                engineStatistics = await readStatistics(page, model);
                renderedOutput = engine.output === "plotly"
                  ? await inspectPlotly(page)
                  : await inspectRenderedImage(page);
                await waitForShinyIdle(page);
              } catch (error) {
                issues.push({ code: "VISUALIZATION_ERROR", detail: error.message });
              }

              const shinyErrors = await visibleShinyErrors(page);
              if (shinyErrors.length) {
                issues.push({ code: "VISIBLE_SHINY_ERROR", detail: shinyErrors.join(" | ") });
              }
              if (renderedOutput && !renderedOutput.rendered) {
                issues.push({ code: "OUTPUT_NOT_RENDERED", detail: JSON.stringify(renderedOutput.diagnostics) });
              }
              if (engineStatistics && !scoresMatch(referenceStatistics.scores, engineStatistics.scores)) {
                issues.push({
                  code: "ENGINE_CHANGED_SCORES",
                  detail: `Expected ${JSON.stringify(referenceStatistics.scores)}, received ${JSON.stringify(engineStatistics.scores)}`,
                });
              }

              const allZero = displayedScoresAreZero(referenceStatistics.scores);
              const nonZeroRoundedToZero = nonZeroScoresRoundToZeroAtHeatmapPrecision(
                referenceStatistics.scores
              );
              const heatmapColourful = engine.id === "2d_ggplot" && renderedOutput?.diagnostics
                && renderedOutput.diagnostics.uniqueQuantizedColours >= 16
                && renderedOutput.diagnostics.luminanceRange >= 20
                && renderedOutput.diagnostics.nonWhiteFraction >= 0.03;
              if (allZero && heatmapColourful) {
                issues.push({
                  code: "ZERO_SCORES_WITH_COLOURED_HEATMAP",
                  detail: `Displayed scores are ${JSON.stringify(referenceStatistics.scores)} while the heatmap has ${renderedOutput.diagnostics.uniqueQuantizedColours} quantized colours and luminance range ${renderedOutput.diagnostics.luminanceRange}`,
                });
              } else if (allZero) {
                issues.push({
                  code: "ALL_DISPLAYED_SCORES_ZERO",
                  detail: JSON.stringify(referenceStatistics.scores),
                });
              }
              if (nonZeroRoundedToZero && heatmapColourful) {
                issues.push({
                  code: "NONZERO_SCORES_RENDER_AS_ZERO_LABELS",
                  detail: `Scores ${JSON.stringify(referenceStatistics.scores)} are nonzero, but every cell value rounds to 0.${"0".repeat(HEATMAP_LABEL_DECIMALS)} at the heatmap's ${HEATMAP_LABEL_DECIMALS}-decimal label precision while the image has ${renderedOutput.diagnostics.uniqueQuantizedColours} quantized colours`,
                });
              }

              const combinationEvents = events.slice(eventStart);
              const errorEvents = combinationEvents.filter(event =>
                event.type === "pageerror"
                || event.type === "requestfailed"
                || event.type === "console-error"
                || /^http-5/.test(event.type)
              );
              if (errorEvents.length) {
                issues.push({
                  code: "BROWSER_OR_NETWORK_ERROR",
                  detail: errorEvents.map(event => `${event.type}: ${event.message}`).join(" | "),
                });
              }

              const context = { ...activeContext };
              const result = {
                ...context,
                datasetLabel: dataset.label,
                preview,
                scores: referenceStatistics.scores,
                statistics: referenceStatistics.entries,
                allZero,
                nonZeroRoundedToZero,
                heatmapColourful,
                renderedOutput: renderedOutput ? {
                  rendered: renderedOutput.rendered,
                  diagnostics: renderedOutput.diagnostics,
                  screenshotBytes: renderedOutput.screenshotBytes,
                  screenshotSha256: renderedOutput.screenshotSha256,
                } : null,
                events: combinationEvents,
                issues,
                status: issues.length ? "failed" : "passed",
              };
              if (issues.length) result.screenshot = await writeFailureScreenshot(page, context);
              results.push(result);
              console.log(`[${result.status.toUpperCase()}] ${dataset.name} / ${fitMode.id} / ${model} / ${engine.id}${issues.length ? ` — ${issues.map(issue => issue.code).join(", ")}` : ""}`);
            }
          }
        }
      } catch (error) {
        const context = { dataset: dataset.name, fitMode: "setup", model: "setup", engine: "setup" };
        results.push({
          ...context,
          datasetLabel: dataset.label,
          status: "failed",
          issues: [{ code: "DATASET_SETUP_ERROR", detail: error.message }],
          screenshot: await writeFailureScreenshot(page, context),
        });
      } finally {
        await page.close();
      }
    }
  } finally {
    await browser.close();
  }

  const report = {
    baseURL: BASE_URL,
    generatedAt: new Date().toISOString(),
    configuration: {
      datasets: datasets.map(dataset => dataset.name),
      models,
      engines: engines.map(engine => engine.id),
      fitModes: fitModes.map(mode => mode.id),
      zeroTolerance: ZERO_TOLERANCE,
    },
    summary: {
      total: results.length,
      passed: results.filter(result => result.status === "passed").length,
      failed: results.filter(result => result.status === "failed").length,
      allZero: results.filter(result => result.allZero).length,
      zeroWithColouredHeatmap: results.filter(result => result.heatmapColourful && result.allZero).length,
      nonZeroRoundedToZero: results.filter(
        result => result.heatmapColourful && result.nonZeroRoundedToZero
      ).length,
    },
    results,
    events,
  };

  const jsonPath = path.join(ARTIFACT_DIR, "report.json");
  const markdownPath = path.join(ARTIFACT_DIR, "report.md");
  await fs.writeFile(jsonPath, `${JSON.stringify(report, null, 2)}\n`);
  await fs.writeFile(markdownPath, createMarkdownReport(report));
  console.log(`\nJSON report: ${jsonPath}`);
  console.log(`Markdown report: ${markdownPath}`);
  console.log(`Summary: ${JSON.stringify(report.summary)}`);
  if (report.summary.failed) process.exitCode = 1;
}

run().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
