/* CheckerboardR bundled-download and condition-label regression. */
const { chromium } = require("playwright");
const fs = require("node:fs/promises");
const path = require("node:path");

const BASE_URL = process.env.CHECKERBOARDR_URL || "http://127.0.0.1:3847/";
const TIMEOUT = Number(process.env.CHECKERBOARDR_TIMEOUT_MS || 120000);
const ARTIFACT_DIR = path.resolve(process.env.CHECKERBOARDR_ARTIFACT_DIR || "test-results/checkerboardr-sample-downloads");
const expectedFiles = [
  "testData3.tab", "anticancer_synergy.tab", "antagonism.csv",
  "paclitaxel_carboplatin.json", "fluconazole_voriconazole.xml", "testData.xlsx",
];
const results = [];
const events = [];

function record(id, passed, detail = {}) {
  results.push({ id, status: passed ? "passed" : "failed", ...detail });
  console.log(`[${passed ? "PASS" : "FAIL"}] ${id}`);
}

async function waitIdle(page) {
  await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), null, { timeout: TIMEOUT });
  await page.waitForTimeout(250);
}

async function chooseSample(page, value) {
  await page.locator(`input[name="sampleData"][value="${value}"]`).check();
  await waitIdle(page);
}

async function conditionLabels(page) {
  return {
    flipX: await page.locator('label[for="flipDataX"]').innerText(),
    flipY: await page.locator('label[for="flipDataY"]').innerText(),
    barometerA: await page.locator('label[for="barometerA"]').innerText(),
    barometerB: await page.locator('label[for="barometerB"]').innerText(),
  };
}

async function assertSampleLabels(page, sample, expectedA, expectedB) {
  await page.locator('a[data-value="Data upload"]').click();
  await chooseSample(page, sample);
  await page.locator('a[data-value="Data visualization"]').click();
  await waitIdle(page);
  const labels = await conditionLabels(page);
  const passed = labels.flipX.includes(expectedA) && labels.flipY.includes(expectedB)
    && labels.barometerA.includes(expectedA) && labels.barometerB.includes(expectedB);
  record(`LABEL-${sample}-${expectedA}-${expectedB}`, passed, labels);
}

async function run() {
  await fs.mkdir(ARTIFACT_DIR, { recursive: true });
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1440, height: 1100 }, acceptDownloads: true });
  page.on("pageerror", error => events.push({ type: "pageerror", message: error.message }));
  page.on("console", message => { if (message.type() === "error") events.push({ type: "console-error", message: message.text() }); });
  page.on("requestfailed", request => events.push({ type: "requestfailed", message: `${request.method()} ${request.url()} ${request.failure()?.errorText}` }));

  try {
    await page.goto(BASE_URL, { waitUntil: "domcontentloaded", timeout: TIMEOUT });
    await page.locator('a[data-value="Data upload"]').click();
    for (let index = 0; index < expectedFiles.length; index += 1) {
      const filename = expectedFiles[index];
      const [download] = await Promise.all([
        page.waitForEvent("download", { timeout: TIMEOUT }),
        page.locator(`#downloadSample${index + 1}`).click(),
      ]);
      const target = path.join(ARTIFACT_DIR, filename);
      await download.saveAs(target);
      const stat = await fs.stat(target);
      record(`DOWNLOAD-${index + 1}-${filename}`, download.suggestedFilename() === filename && stat.size > 0,
        { suggestedFilename: download.suggestedFilename(), bytes: stat.size });
    }

    await assertSampleLabels(page, "4", "Paclitaxel", "Carboplatin");
    await assertSampleLabels(page, "6", "Condition A", "Condition B");
    await page.screenshot({ path: path.join(ARTIFACT_DIR, "excel-condition-labels.png"), fullPage: true });
  } finally {
    await browser.close();
  }

  const unexpectedEvents = events.filter(event => event.type !== "requestfailed" || !event.message.includes("favicon"));
  const report = {
    baseURL: BASE_URL,
    generatedAt: new Date().toISOString(),
    summary: { total: results.length, passed: results.filter(result => result.status === "passed").length,
      failed: results.filter(result => result.status === "failed").length },
    results,
    events,
  };
  await fs.writeFile(path.join(ARTIFACT_DIR, "report.json"), `${JSON.stringify(report, null, 2)}\n`);
  console.log(`Summary: ${JSON.stringify(report.summary)}`);
  if (report.summary.failed || unexpectedEvents.length) process.exitCode = 1;
}

run().catch(error => { console.error(error); process.exitCode = 1; });
