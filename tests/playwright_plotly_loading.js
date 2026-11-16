const { chromium } = require('playwright');
const assert = require('node:assert/strict');

(async () => {
  const browser = await chromium.launch({ headless: true });
  try {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    // Hold the actual Plotly library request to reproduce a slow first load.
    let release;
    const gate = new Promise(resolve => { release = resolve; });
    await page.route('**/plotly-latest.min.js', async route => {
      await gate;
      await route.continue();
    });
    await page.goto(process.env.CHECKERBOARDR_URL || 'http://127.0.0.1:3847');
    await page.locator('a[data-value="Data visualization"]').click();
    await page.locator('#checkerboardStatsTable table').waitFor({ timeout: 120000 });
    await page.locator('#plotEngine').evaluate(el => el.selectize.setValue('3d_plotly'));
    await page.locator('#plotly-loading').waitFor({ state: 'visible' });
    assert.equal(await page.locator('#plotly-container').getAttribute('aria-busy'), 'true');
    release();
    await page.locator('#plotly-loading').waitFor({ state: 'hidden', timeout: 120000 });
    assert.equal(await page.locator('#plotlyPlot').evaluate(el => el.data[0].type), 'surface');
    assert.equal(await page.locator('#plotly-container').getAttribute('aria-busy'), 'false');
    // A camera change must preserve the surface/canvas instead of rebuilding it.
    await page.evaluate(() => {
      window.originalSurface = document.getElementById('plotlyPlot').data[0];
      window.originalCanvas = document.querySelector('#plotlyPlot canvas');
      Shiny.setInputValue('plotlyTheta', 120, { priority: 'event' });
    });
    await page.waitForFunction(() => {
      const eye = document.getElementById('plotlyPlot').layout.scene.camera.eye;
      return Math.abs(eye.x - 1.35) < 0.001 && eye.y < 0;
    });
    assert(await page.evaluate(() =>
      document.getElementById('plotlyPlot').data[0] === window.originalSurface &&
      document.querySelector('#plotlyPlot canvas') === window.originalCanvas));
    // A theme change really does rebuild the surface; the indicator must clear again.
    await page.locator('#themePreset').evaluate(el => el.selectize.setValue('Financial Times'));
    await page.waitForFunction(() =>
      document.getElementById('plotlyPlot').layout.paper_bgcolor === '#fff1e5');
    await page.locator('#plotly-loading').waitFor({ state: 'hidden' });
    assert.deepEqual(errors, []);
    console.log('PASS: slow first load, ready state, camera reuse, and theme refresh');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
