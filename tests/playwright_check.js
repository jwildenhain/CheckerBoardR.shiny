const { chromium } = require("playwright");
const assert = require("node:assert/strict");

(async () => {
  const baseURL = process.env.CHECKERBOARDR_URL || "http://127.0.0.1:3847";
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ acceptDownloads: true });
  const errors = [];
  const setSelect = (id, value) => page.locator(id).evaluate((el, v) => el.selectize.setValue(v), value);
  page.on("pageerror", error => errors.push(error.message));
  page.on("console", message => { if (message.type() === "error") errors.push(message.text()); });

  await page.goto(baseURL, { waitUntil: "domcontentloaded" });
  await page.locator("a[data-value=\"Data upload\"]").click();
  await page.locator("#filetable table").waitFor({ timeout: 120000 });
  assert.match(await page.locator("body").innerText(), /CheckerboardR/);

  await page.locator("a[data-value=\"Data visualization\"]").click();
  await page.locator("#checkerboardStatsTable table").waitFor({ timeout: 120000 });
  assert.match(await page.locator("#checkerboardStatsTable").innerText(), /Mean Score/);

  await setSelect("#synergyModel", "ZIP");
  await setSelect("#plotEngine", "3d_plotly");
  await page.locator("#plotlyPlot .plotly").waitFor({ timeout: 120000 });
  const traceType = await page.locator("#plotlyPlot").evaluate(el => el.data?.[0]?.type);
  assert.equal(traceType, "surface", "3D surface was not rendered");

  await setSelect("#plotEngine", "1d_curves");
  await page.locator("#ggplotPlot img").waitFor({ timeout: 120000 });

  await setSelect("#plotEngine", "3d_base");
  const downloadPromise = page.waitForEvent("download");
  await page.locator("#downloadPlotPDF").click();
  const download = await downloadPromise;
  const path = await download.path();
  assert.ok(path, "PDF download did not complete");

  await page.locator("a[data-value=\"Data upload\"]").click();
  await page.locator("input[name=sampleData][value=\"4\"]").check();
  await page.locator("#filetable table").waitFor({ timeout: 120000 });
  await page.waitForFunction(() => document.querySelector("#synergyModel")?.value === "Bliss");
  assert.equal(await page.locator("#synergyModel").inputValue(), "Bliss");

  assert.deepEqual(errors, [], `Browser errors: ${errors.join(" | ")}`);
  console.log("Playwright Shiny checks passed");
  await browser.close();
})().catch(error => { console.error(error); process.exit(1); });
