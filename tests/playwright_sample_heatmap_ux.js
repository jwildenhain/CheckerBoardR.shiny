/*
 * CheckerBoardR sample-data UX regression for the 2D heatmap only.
 *
 * Run against the deployed app:
 *   CHECKERBOARDR_URL=http://checkerboardr.example/ node tests/playwright_sample_heatmap_ux.js
 *
 * Optional comma-separated filters:
 *   CHECKERBOARDR_DATASETS=1,4 CHECKERBOARDR_MODELS=Bliss,ZIP
 */
const { chromium } = require("playwright");
const crypto = require("node:crypto");
const fs = require("node:fs/promises");
const path = require("node:path");
const BASE_URL = process.env.CHECKERBOARDR_URL || "http://127.0.0.1:3847/";
const TIMEOUT_MS = Number(process.env.CHECKERBOARDR_TIMEOUT_MS || 120000);
const ARTIFACT_DIR = path.resolve(
  process.env.CHECKERBOARDR_ARTIFACT_DIR || "test-results/checkerboardr-sample-heatmaps"
);

const DATASETS = [
  { id: "1", name: "anti-fungal-tab", label: "Anti-fungal Screening (testData3.tab)" },
  { id: "2", name: "anticancer-synergy-tab", label: "Synthetic Anticancer Synergy Grid (anticancer_synergy.tab)" },
  { id: "3", name: "antagonism-csv", label: "Antagonistic Combination (antagonism.csv)" },
  { id: "4", name: "chemotherapy-json", label: "Chemotherapy Grid (paclitaxel_carboplatin.json)" },
  { id: "5", name: "antifungal-xml", label: "Antifungal Grid (fluconazole_voriconazole.xml)" },
  { id: "6", name: "excel-grid", label: "Excel Spreadsheet Grid (testData.xlsx)" },
];
const MODELS = ["Bliss", "HSA", "Loewe", "ZIP", "Data"];
const FLIPS = [
  { id: "baseline", x: false, y: false, z: false },
  { id: "flip-x", x: true, y: false, z: false },
  { id: "flip-y", x: false, y: true, z: false },
  { id: "flip-z", x: false, y: false, z: true },
];

function selectByEnvironment(items, variable, key = "id") {
  const requested = (process.env[variable] || "")
    .split(",")
    .map(value => value.trim())
    .filter(Boolean);
  return requested.length ? items.filter(item => requested.includes(item[key])) : items;
}

function sha256(buffer) {
  return crypto.createHash("sha256").update(buffer).digest("hex");
}

function safeName(value) {
  return String(value).replace(/[^a-z0-9_.-]+/gi, "-").replace(/^-|-$/g, "").toLowerCase();
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
  const currentValue = await page.locator(selector).evaluate(element =>
    element.selectize ? element.selectize.getValue() : element.value
  );
  const changed = currentValue !== value;
  const oldSource = changed
    ? await page.locator("#ggplotPlot img").getAttribute("src").catch(() => null)
    : null;
  await page.locator(selector).evaluate((element, nextValue) => {
    if (element.selectize) element.selectize.setValue(nextValue);
    else {
      element.value = nextValue;
      element.dispatchEvent(new Event("input", { bubbles: true }));
      element.dispatchEvent(new Event("change", { bubbles: true }));
    }
  }, value);
  await waitForShinyIdle(page);
  await waitForHeatmap(page, oldSource);
}

async function waitForHeatmap(page, oldSource = null) {
  const image = page.locator("#ggplotPlot img");
  await image.waitFor({ state: "visible", timeout: TIMEOUT_MS });
  await page.waitForFunction(
    previous => {
      const element = document.querySelector("#ggplotPlot img");
      return element?.complete
        && element.naturalWidth > 100
        && element.naturalHeight > 100
        && (!previous || element.getAttribute("src") !== previous);
    },
    oldSource,
    { timeout: TIMEOUT_MS }
  );
  await page.waitForTimeout(500);
  await waitForShinyIdle(page);
}

async function readMatrixPreview(page) {
  const table = page.locator("#filetable table");
  await table.waitFor({ state: "visible", timeout: TIMEOUT_MS });
  const preview = await table.evaluate(element => {
    const headerCells = [...element.querySelectorAll("thead th")].map(cell => cell.innerText.trim());
    const rows = [...element.querySelectorAll("tbody tr")].map(row => {
      const cells = [...row.querySelectorAll("th, td")].map(cell => cell.innerText.trim());
      return { label: cells[0], values: cells.slice(1).map(value => Number.parseFloat(value)) };
    });
    return {
      headers: headerCells.slice(-Math.max(0, headerCells.length - 1)),
      rows,
      text: element.innerText,
    };
  });
  preview.columns = preview.rows[0]?.values.length || 0;
  preview.dataType = await page.locator('input[name="dataType"]:checked').getAttribute("value");
  preview.controlRow = Number(await page.locator("#ctrlRow").inputValue().catch(() => "1")) || 1;
  preview.controlCol = Number(await page.locator("#ctrlCol").inputValue().catch(() => "1")) || 1;
  return preview;
}

function previewIssues(preview) {
  const issues = [];
  if (preview.rows.length < 2 || preview.columns < 2) {
    issues.push({ code: "INVALID_PREVIEW_DIMENSIONS", detail: `${preview.rows.length}x${preview.columns}` });
  }
  if (preview.rows.some(row => row.values.length !== preview.columns)) {
    issues.push({ code: "RAGGED_PREVIEW_MATRIX", detail: "Preview rows have inconsistent lengths" });
  }
  if (preview.rows.some(row => row.values.some(value => !Number.isFinite(value)))) {
    issues.push({ code: "NON_NUMERIC_PREVIEW_VALUE", detail: "Preview contains a non-finite value" });
  }
  if (!preview.headers.length || preview.rows.some(row => !row.label)) {
    issues.push({ code: "MISSING_CONCENTRATION_LABEL", detail: "Preview row/column labels are incomplete" });
  }
  const labels = [...preview.headers, ...preview.rows.map(row => row.label)];
  const rPrefixed = labels.filter(label => /^X(?:[0-9]|\.[0-9])/.test(label));
  if (rPrefixed.length) {
    issues.push({ code: "R_PREFIXED_CONCENTRATION_LABEL", detail: rPrefixed.join(", ") });
  }
  const leadingDecimals = labels.filter(label => /^-?\.[0-9]/.test(label));
  if (leadingDecimals.length) {
    issues.push({ code: "UNPADDED_DECIMAL_LABEL", detail: leadingDecimals.join(", ") });
  }
  return issues;
}

function normalizedInhibition(preview) {
  const matrix = preview.rows.map(row => [...row.values]);
  if (preview.dataType === "inhibition") {
    const scale = Math.max(...matrix.flat()) > 1.5 ? 100 : 1;
    return matrix.map(row => row.map(value => Math.max(-0.2, Math.min(1, value / scale))));
  }
  const control = matrix[preview.controlRow - 1]?.[preview.controlCol - 1];
  if (!Number.isFinite(control) || control <= 0) return null;
  return matrix.map(row => row.map(value => {
    const viability = Math.max(0, Math.min(1.2, value / control));
    return Math.max(-0.2, Math.min(1, 1 - viability));
  }));
}

function expectedMatrix(preview, model) {
  const inhibition = normalizedInhibition(preview);
  if (!inhibition) return null;
  if (model === "Data") return inhibition;
  if (!['HSA', 'Bliss'].includes(model)) return null;
  const singleA = inhibition[0];
  const singleB = inhibition.map(row => row[0]);
  return inhibition.map((row, y) => row.map((observed, x) => {
    if (x === 0 || y === 0) return 0;
    const expected = model === "HSA"
      ? Math.max(singleA[x], singleB[y])
      : singleA[x] + singleB[y] - (singleA[x] * singleB[y]);
    return observed - expected;
  }));
}

async function setFlipState(page, flip) {
  const controls = [
    ["#flipDataX", flip.x],
    ["#flipDataY", flip.y],
    ["#flipDataZ", flip.z],
  ];
  const states = await Promise.all(controls.map(([selector]) => page.locator(selector).isChecked()));
  const changed = controls.some(([, expected], index) => states[index] !== expected);
  const oldSource = changed ? await page.locator("#ggplotPlot img").getAttribute("src") : null;
  for (const [selector, expected] of controls) {
    await page.locator(selector).setChecked(expected);
  }
  await waitForShinyIdle(page);
  await waitForHeatmap(page, oldSource);
}

async function captureHeatmap(page, rows, columns) {
  const image = page.locator("#ggplotPlot img");
  const diagnostics = await image.evaluate((element, shape) => {
    const canvas = document.createElement("canvas");
    canvas.width = element.naturalWidth;
    canvas.height = element.naturalHeight;
    const context = canvas.getContext("2d", { willReadFrequently: true });
    context.drawImage(element, 0, 0);
    const xStart = canvas.width * 0.119;
    const xEnd = canvas.width * 0.833;
    const yBottom = canvas.height * 0.873;
    const yTop = canvas.height * 0.113;
    const xStep = shape.columns > 1 ? (xEnd - xStart) / (shape.columns - 1) : 0;
    const yStep = shape.rows > 1 ? (yBottom - yTop) / (shape.rows - 1) : 0;
    const radius = Math.max(2, Math.min(9, xStep * 0.16, yStep * 0.16));
    const median = values => values.sort((a, b) => a - b)[Math.floor(values.length / 2)];
    const colours = [];
    const labelSignatures = [];
    const labelInkCounts = [];
    const labelProfiles = [];
    for (let y = 0; y < shape.rows; y += 1) {
      const row = [];
      const signatureRow = [];
      const inkCountRow = [];
      const profileRow = [];
      for (let x = 0; x < shape.columns; x += 1) {
        const reds = [];
        const greens = [];
        const blues = [];
        const centerX = Math.round(xStart + x * xStep);
        const centerY = Math.round(yBottom - y * yStep);
        for (let dy = -radius; dy <= radius; dy += 2) {
          for (let dx = -radius; dx <= radius; dx += 2) {
            const pixel = context.getImageData(centerX + dx, centerY + dy, 1, 1).data;
            reds.push(pixel[0]); greens.push(pixel[1]); blues.push(pixel[2]);
          }
        }
        const fillColour = [median(reds), median(greens), median(blues)];
        row.push(fillColour);

        const maskWidth = 49;
        const maskHeight = 17;
        const mask = [];
        for (let py = 0; py < maskHeight; py += 1) {
          for (let px = 0; px < maskWidth; px += 1) {
            const imageX = centerX + px - Math.floor(maskWidth / 2);
            const imageY = centerY + py - Math.floor(maskHeight / 2);
            const pixel = context.getImageData(imageX, imageY, 1, 1).data;
            const textVector = [30 - fillColour[0], 41 - fillColour[1], 59 - fillColour[2]];
            const pixelVector = [pixel[0] - fillColour[0], pixel[1] - fillColour[1], pixel[2] - fillColour[2]];
            const denominator = textVector.reduce((sum, value) => sum + (value ** 2), 0);
            const projection = denominator
              ? pixelVector.reduce((sum, value, index) => sum + (value * textVector[index]), 0) / denominator
              : 0;
            mask.push(projection > 0.28 ? "1" : "0");
          }
        }
        const signature = mask.join("");
        signatureRow.push(signature);
        inkCountRow.push(mask.filter(pixel => pixel === "1").length);
        profileRow.push(Array.from({ length: maskWidth }, (_, column) => {
          let count = 0;
          for (let maskY = 0; maskY < maskHeight; maskY += 1) {
            if (mask[(maskY * maskWidth) + column] === "1") count += 1;
          }
          return count;
        }));
      }
      colours.push(row);
      labelSignatures.push(signatureRow);
      labelInkCounts.push(inkCountRow);
      labelProfiles.push(profileRow);
    }
    return {
      width: canvas.width,
      height: canvas.height,
      colours,
      labelSignatures,
      labelInkCounts,
      labelProfiles,
      uniqueTileColours: new Set(colours.flat().map(rgb => rgb.join(","))).size,
    };
  }, { rows, columns });
  const screenshot = await image.screenshot();
  return { ...diagnostics, screenshot, sha256: sha256(screenshot) };
}

function colourDistance(left, right) {
  return Math.sqrt(left.reduce((sum, value, index) => sum + ((value - right[index]) ** 2), 0));
}

function mirroredDistance(baseline, actual, axis) {
  const rows = baseline.length;
  const columns = baseline[0].length;
  const distances = [];
  for (let y = 0; y < rows; y += 1) {
    for (let x = 0; x < columns; x += 1) {
      const sourceY = axis === "y" ? rows - 1 - y : y;
      const sourceX = axis === "x" ? columns - 1 - x : x;
      distances.push(colourDistance(baseline[sourceY][sourceX], actual[y][x]));
    }
  }
  return distances.reduce((sum, value) => sum + value, 0) / distances.length;
}

function pearson(left, right) {
  if (left.length < 3 || left.length !== right.length) return null;
  const meanLeft = left.reduce((sum, value) => sum + value, 0) / left.length;
  const meanRight = right.reduce((sum, value) => sum + value, 0) / right.length;
  let numerator = 0;
  let leftSquare = 0;
  let rightSquare = 0;
  left.forEach((value, index) => {
    const a = value - meanLeft;
    const b = right[index] - meanRight;
    numerator += a * b;
    leftSquare += a * a;
    rightSquare += b * b;
  });
  const denominator = Math.sqrt(leftSquare * rightSquare);
  return denominator ? numerator / denominator : null;
}

function ranks(values) {
  return values.map((value, index) => ({ value, index }))
    .sort((left, right) => left.value - right.value)
    .reduce((result, item, rank) => {
      result[item.index] = rank;
      return result;
    }, []);
}

function spearman(left, right) {
  return pearson(ranks(left), ranks(right));
}

function colourProjectionMetric(lowColour, highColour) {
  const direction = highColour.map((value, index) => value - lowColour[index]);
  const denominator = direction.reduce((sum, value) => sum + (value ** 2), 0);
  return colour => denominator
    ? colour.reduce((sum, value, index) => sum + ((value - lowColour[index]) * direction[index]), 0) / denominator
    : 0;
}

function dataColourMetric(matrix, heatmap) {
  const values = matrix.flat();
  const colours = heatmap.colours.flat();
  const minimumIndex = values.indexOf(Math.min(...values));
  const maximumIndex = values.indexOf(Math.max(...values));
  return colourProjectionMetric(colours[minimumIndex], colours[maximumIndex]);
}

function zFlipCorrelation(baseline, flipped, model, preview) {
  const baseColours = baseline.flat();
  const flipColours = flipped.flat();
  const metric = model === "Data"
    ? dataColourMetric(expectedMatrix(preview, "Data"), { colours: baseline })
    : rgb => rgb[0] - rgb[2];
  const left = baseColours.map(metric);
  const right = flipColours.map(metric);
  return model === "Data" ? spearman(left, right) : pearson(left, right);
}

function signatureDistance(left, right) {
  if (!left || !right || left.length !== right.length) return 1;
  const width = 49;
  const height = 17;
  let best = 1;
  for (let shiftY = -2; shiftY <= 2; shiftY += 1) {
    for (let shiftX = -2; shiftX <= 2; shiftX += 1) {
      let intersection = 0;
      let union = 0;
      for (let y = 0; y < height; y += 1) {
        for (let x = 0; x < width; x += 1) {
          const shiftedX = x + shiftX;
          const shiftedY = y + shiftY;
          const leftInk = left[(y * width) + x] === "1";
          const rightInk = shiftedX >= 0 && shiftedX < width && shiftedY >= 0 && shiftedY < height
            ? right[(shiftedY * width) + shiftedX] === "1"
            : false;
          if (leftInk || rightInk) union += 1;
          if (leftInk && rightInk) intersection += 1;
        }
      }
      if (union) best = Math.min(best, 1 - (intersection / union));
    }
  }
  return best;
}

function zeroLikeLabelFraction(heatmap) {
  const signatures = heatmap.labelSignatures.flat();
  const zeroSignature = signatures[0];
  const inkCounts = heatmap.labelInkCounts.flat();
  const profiles = heatmap.labelProfiles.flat();
  const zeroProfile = profiles[0];
  const zeroInkCount = inkCounts[0];
  const inkTolerance = Math.max(3, zeroInkCount * 0.55);
  return signatures.filter((signature, index) => {
    const profileCorrelations = [];
    for (let shift = -3; shift <= 3; shift += 1) {
      const shifted = zeroProfile.map((_, profileIndex) => profiles[index][profileIndex + shift] || 0);
      profileCorrelations.push(pearson(zeroProfile, shifted));
    }
    const bestProfileCorrelation = Math.max(...profileCorrelations.filter(value => value !== null));
    return (signatureDistance(signature, zeroSignature) < 0.55 || bestProfileCorrelation > 0.78)
      && Math.abs(inkCounts[index] - zeroInkCount) <= inkTolerance;
  }).length / signatures.length;
}

function heatmapValueColourCorrelation(matrix, heatmap, model) {
  const values = matrix.flat();
  const metric = model === "Data"
    ? dataColourMetric(matrix, heatmap)
    : rgb => rgb[0] - rgb[2];
  const colourValues = heatmap.colours.flat().map(metric);
  return model === "Data" ? spearman(values, colourValues) : pearson(values, colourValues);
}

function boundaryColourDistance(heatmap) {
  const colours = heatmap.colours;
  const boundary = [
    ...colours[0],
    ...colours.slice(1).map(row => row[0]),
  ];
  const reference = boundary[0];
  return boundary.reduce((sum, colour) => sum + colourDistance(reference, colour), 0) / boundary.length;
}

function calculationIssues(model, heatmap, preview) {
  const issues = [];
  const zeroLikeFraction = zeroLikeLabelFraction(heatmap);
  const expected = expectedMatrix(preview, model);
  if (expected) {
    const expectedNonZero = expected.flat().filter(value => Math.abs(value) >= 0.005).length;
    const correlation = heatmapValueColourCorrelation(expected, heatmap, model);
    if (correlation === null || correlation < 0.72) {
      issues.push({
        code: "HEATMAP_COLOURS_DO_NOT_MATCH_CALCULATION",
        detail: `${model} preview-derived values versus tile-colour correlation was ${correlation === null ? "unavailable" : correlation.toFixed(3)}`,
      });
    }
    if (expectedNonZero > 0 && zeroLikeFraction > 0.9) {
      issues.push({
        code: "HEATMAP_LABELS_STUCK_AT_ZERO",
        detail: `${expectedNonZero} cells calculate to nonzero values, but ${(zeroLikeFraction * 100).toFixed(1)}% of rendered labels match the 0.00 control label`,
      });
    }
  } else {
    const boundaryDistance = boundaryColourDistance(heatmap);
    if (boundaryDistance > 18) {
      issues.push({
        code: "SINGLE_AGENT_BOUNDARY_COLOURS_NOT_ZERO",
        detail: `${model} zero-dose boundary mean RGB distance was ${boundaryDistance.toFixed(2)}`,
      });
    }
    if (heatmap.uniqueTileColours > 2 && zeroLikeFraction > 0.9) {
      issues.push({
        code: "HEATMAP_LABELS_STUCK_AT_ZERO",
        detail: `${model} has ${heatmap.uniqueTileColours} sampled tile colours, but ${(zeroLikeFraction * 100).toFixed(1)}% of labels match the 0.00 control label`,
      });
    }
  }
  heatmap.zeroLikeLabelFraction = zeroLikeFraction;
  return issues;
}

async function visibleShinyErrors(page) {
  return page.locator(".shiny-output-error, .shiny-notification-error").evaluateAll(elements =>
    elements.filter(element => {
      const rect = element.getBoundingClientRect();
      const style = getComputedStyle(element);
      return rect.width > 0 && rect.height > 0 && style.display !== "none" && style.visibility !== "hidden";
    }).map(element => element.innerText.trim()).filter(Boolean)
  );
}

async function failureScreenshot(page, context) {
  const name = [context.dataset, context.model, context.flip].map(safeName).join("__");
  const target = path.join(ARTIFACT_DIR, "failures", `${name}.png`);
  await fs.mkdir(path.dirname(target), { recursive: true });
  await page.screenshot({ path: target, fullPage: true });
  return path.relative(process.cwd(), target);
}

function markdownReport(report) {
  const lines = [
    "# CheckerBoardR sample 2D heatmap UX report",
    "",
    `- URL: ${report.baseURL}`,
    `- Generated: ${report.generatedAt}`,
    `- Sample datasets: ${report.summary.datasets}`,
    `- Heatmap states: ${report.summary.total}`,
    `- Passed: ${report.summary.passed}`,
    `- Failed: ${report.summary.failed}`,
    `- Browser/Shiny error events: ${report.summary.errorEvents}`,
    "- Computed summary table used: no",
    "",
    "## Issues",
    "",
    "| Dataset | Model | Heatmap state | Issue | Details |",
    "|---|---|---|---|---|",
  ];
  const issueRows = report.results.flatMap(result => result.issues.map(issue => [
    result.dataset, result.model, result.flip, issue.code,
    String(issue.detail || "").replace(/\|/g, "\\|").replace(/\n/g, " "),
  ]));
  if (!issueRows.length) lines.push("| — | — | — | None | All sample heatmap checks passed |");
  issueRows.forEach(row => lines.push(`| ${row.join(" | ")} |`));
  return `${lines.join("\n")}\n`;
}

async function run() {
  const datasets = selectByEnvironment(DATASETS, "CHECKERBOARDR_DATASETS");
  const models = selectByEnvironment(MODELS.map(id => ({ id })), "CHECKERBOARDR_MODELS").map(item => item.id);
  const heatmapSource = await fs.readFile(path.resolve(__dirname, "..", "Make3DPlotFunctions.R"), "utf8");
  const scalarLabelIfelse = /geom_text\([\s\S]*?ifelse\(flip_z,\s*abs\(Score\),\s*Score\)/.test(heatmapSource);
  await fs.mkdir(ARTIFACT_DIR, { recursive: true });
  const browser = await chromium.launch({ headless: process.env.HEADED !== "1" });
  const results = [];
  const events = [];
  const sampleMatrixOwners = new Map();
  let activeContext = {};

  try {
    for (const dataset of datasets) {
      const page = await browser.newPage({ acceptDownloads: true, viewport: { width: 1440, height: 1000 } });
      page.setDefaultTimeout(TIMEOUT_MS);
      page.on("pageerror", error => events.push({ type: "pageerror", message: error.message, context: { ...activeContext } }));
      page.on("console", message => {
        if (message.type() === "error") events.push({ type: "console-error", message: message.text(), context: { ...activeContext } });
      });
      page.on("requestfailed", request => events.push({
        type: "requestfailed", message: `${request.method()} ${request.url()}`, context: { ...activeContext },
      }));

      try {
        activeContext = { dataset: dataset.name, model: "setup", flip: "preview" };
        await page.goto(BASE_URL, { waitUntil: "domcontentloaded", timeout: TIMEOUT_MS });
        await page.locator(".version-badge").waitFor({ state: "visible", timeout: TIMEOUT_MS });
        await page.locator('a[data-value="Data upload"]').click();
        await page.locator("#filetable table").waitFor({ state: "visible", timeout: TIMEOUT_MS });
        const initialPreview = await page.locator("#filetable table").innerText();
        await page.locator(`input[name="sampleData"][value="${dataset.id}"]`).check();
        if (dataset.id !== "1") {
          await page.waitForFunction(
            ({ id, oldText }) => document.querySelector('input[name="sampleData"]:checked')?.value === id
              && (document.querySelector("#filetable table")?.innerText || "") !== oldText,
            { id: dataset.id, oldText: initialPreview },
            { timeout: TIMEOUT_MS }
          );
        }
        await waitForShinyIdle(page);
        const preview = await readMatrixPreview(page);
        const datasetPreviewIssues = previewIssues(preview);
        const matrixSignature = sha256(Buffer.from(JSON.stringify(preview.rows.map(row => row.values))));
        const duplicateOwner = sampleMatrixOwners.get(matrixSignature);
        if (duplicateOwner) {
          datasetPreviewIssues.push({
            code: "DUPLICATE_SAMPLE_MATRIX",
            detail: `${dataset.name} is numerically identical to ${duplicateOwner}`,
          });
        } else {
          sampleMatrixOwners.set(matrixSignature, dataset.name);
        }

        await page.locator('a[data-value="Data visualization"]').click();
        await waitForShinyIdle(page);
        await setSelectize(page, "#plotEngine", "2d_ggplot");
        await page.locator("#useFit").setChecked(true);
        await waitForShinyIdle(page);

        for (const model of models) {
          activeContext = { dataset: dataset.name, model, flip: "baseline" };
          await setFlipState(page, FLIPS[0]);
          await setSelectize(page, "#synergyModel", model);
          const baseline = await captureHeatmap(page, preview.rows.length, preview.columns);
          const baselineImagePath = path.join(
            ARTIFACT_DIR, "heatmaps", `${safeName(dataset.name)}__${safeName(model)}.png`
          );
          await fs.mkdir(path.dirname(baselineImagePath), { recursive: true });
          await fs.writeFile(baselineImagePath, baseline.screenshot);
          const modelCalculationIssues = calculationIssues(model, baseline, preview).filter(
            issue => issue.code !== "HEATMAP_LABELS_STUCK_AT_ZERO" || scalarLabelIfelse
          );
          const baselineIssues = [
            ...datasetPreviewIssues,
            ...modelCalculationIssues,
          ];
          if (scalarLabelIfelse && !baselineIssues.some(issue => issue.code === "HEATMAP_LABELS_STUCK_AT_ZERO")) {
            baselineIssues.push({
              code: "HEATMAP_LABELS_STUCK_AT_ZERO",
              detail: "The rendered label aesthetic uses scalar ifelse(flip_z, abs(Score), Score); R returns the zero control score and ggplot recycles 0.00 across every tile",
            });
          }
          if (baseline.uniqueTileColours < 2) {
            baselineIssues.push({ code: "HEATMAP_HAS_NO_COLOUR_VARIATION", detail: `${baseline.uniqueTileColours} sampled tile colours` });
          }
          const baselineShinyErrors = await visibleShinyErrors(page);
          if (baselineShinyErrors.length) baselineIssues.push({ code: "VISIBLE_SHINY_ERROR", detail: baselineShinyErrors.join(" | ") });
          const baselineResult = {
            dataset: dataset.name,
            datasetLabel: dataset.label,
            model,
            flip: "baseline",
            preview: {
              rows: preview.rows.length,
              columns: preview.columns,
              headers: preview.headers,
              rowLabels: preview.rows.map(row => row.label),
              dataType: preview.dataType,
            },
            heatmap: {
              sha256: baseline.sha256,
              uniqueTileColours: baseline.uniqueTileColours,
              zeroLikeLabelFraction: baseline.zeroLikeLabelFraction,
              labelInkCountRange: [
                Math.min(...baseline.labelInkCounts.flat()),
                Math.max(...baseline.labelInkCounts.flat()),
              ],
              image: path.relative(process.cwd(), baselineImagePath),
            },
            issues: baselineIssues,
            status: baselineIssues.length ? "failed" : "passed",
          };
          if (baselineIssues.length) baselineResult.screenshot = await failureScreenshot(page, baselineResult);
          results.push(baselineResult);
          console.log(`[${baselineResult.status.toUpperCase()}] ${dataset.name} / ${model} / baseline${baselineIssues.length ? ` — ${baselineIssues.map(issue => issue.code).join(", ")}` : ""}`);

          for (const flip of FLIPS.slice(1)) {
            activeContext = { dataset: dataset.name, model, flip: flip.id };
            const issues = [];
            await setFlipState(page, FLIPS[0]);
            await setFlipState(page, flip);
            const captured = await captureHeatmap(page, preview.rows.length, preview.columns);
            if (captured.sha256 === baseline.sha256) {
              issues.push({ code: "FLIP_DID_NOT_CHANGE_HEATMAP", detail: `${flip.id} produced the baseline image` });
            }
            if (flip.x || flip.y) {
              const axis = flip.x ? "x" : "y";
              const distance = mirroredDistance(baseline.colours, captured.colours, axis);
              if (distance > 18) {
                issues.push({
                  code: "AXIS_FLIP_NOT_MIRRORED",
                  detail: `${flip.id} mean mirrored RGB distance ${distance.toFixed(2)} exceeds 18`,
                });
              }
            }
            if (flip.z) {
              const correlation = zFlipCorrelation(baseline.colours, captured.colours, model, preview);
              const maximumCorrelation = model === "Data" ? -0.3 : -0.55;
              if (correlation === null || correlation > maximumCorrelation) {
                issues.push({
                  code: "Z_FLIP_DID_NOT_REVERSE_COLOUR_MAPPING",
                  detail: `Colour correlation was ${correlation === null ? "unavailable" : correlation.toFixed(3)}`,
                });
              }
            }
            const shinyErrors = await visibleShinyErrors(page);
            if (shinyErrors.length) issues.push({ code: "VISIBLE_SHINY_ERROR", detail: shinyErrors.join(" | ") });
            const result = {
              dataset: dataset.name,
              datasetLabel: dataset.label,
              model,
              flip: flip.id,
              heatmap: {
                sha256: captured.sha256,
                uniqueTileColours: captured.uniqueTileColours,
                comparison: flip.z
                  ? { zColourCorrelation: zFlipCorrelation(baseline.colours, captured.colours, model, preview) }
                  : { mirroredRgbDistance: mirroredDistance(baseline.colours, captured.colours, flip.x ? "x" : "y") },
              },
              issues,
              status: issues.length ? "failed" : "passed",
            };
            if (issues.length) result.screenshot = await failureScreenshot(page, result);
            results.push(result);
            console.log(`[${result.status.toUpperCase()}] ${dataset.name} / ${model} / ${flip.id}${issues.length ? ` — ${issues.map(issue => issue.code).join(", ")}` : ""}`);
          }
          await setFlipState(page, FLIPS[0]);
        }
      } catch (error) {
        const result = {
          dataset: dataset.name,
          model: activeContext.model || "setup",
          flip: activeContext.flip || "setup",
          issues: [{ code: "TEST_EXECUTION_ERROR", detail: error.message }],
          status: "failed",
        };
        result.screenshot = await failureScreenshot(page, result);
        results.push(result);
      } finally {
        await page.close();
      }
    }
  } finally {
    await browser.close();
  }

  const errorEvents = events.filter(event => ["pageerror", "console-error", "requestfailed"].includes(event.type));
  const report = {
    baseURL: BASE_URL,
    generatedAt: new Date().toISOString(),
    summary: {
      datasets: datasets.length,
      total: results.length,
      passed: results.filter(result => result.status === "passed").length,
      failed: results.filter(result => result.status === "failed").length,
      errorEvents: errorEvents.length,
    },
    configuration: { datasets: datasets.map(dataset => dataset.name), models, engine: "2d_ggplot", flips: FLIPS.map(flip => flip.id) },
    results,
    events,
  };
  await fs.writeFile(path.join(ARTIFACT_DIR, "report.json"), `${JSON.stringify(report, null, 2)}\n`);
  await fs.writeFile(path.join(ARTIFACT_DIR, "report.md"), markdownReport(report));
  console.log(`\nSummary: ${JSON.stringify(report.summary)}`);
  console.log(`Report: ${path.join(ARTIFACT_DIR, "report.md")}`);
  if (report.summary.failed) process.exitCode = 1;
}

run().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
