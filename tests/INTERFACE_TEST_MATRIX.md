# CheckerboardR interface validation list

Use this list for each release candidate. Record the deployed URL, commit, browser version, date, and report paths before approval.

## Automated suites

| ID | Area | States iterated | Expected result | Command |
|---|---|---|---|---|
| UX-01 | Bundled samples, 2D heatmap | 6 datasets × 6 models × baseline/X/Y/Z | Numeric labelled preview; rendered non-empty heatmap; correct flips; no zero-label/colour contradiction | `npm run test:ux:sample-heatmaps` |
| UX-02 | Plot engine compatibility | 7 datasets × 6 models × 4 engines × fitted/fallback modes | Stable scores between engines; visible output; no browser, network, or Shiny errors | `npm run test:ux:matrix` |
| UX-03 | Capability workflow | Score/reference/observed for HSA, Bliss, Loewe, ZIP, Consensus; ZIP fitted response | Correct matrix title and heatmap; no output errors | `npm run test:ux:capabilities` |
| UX-04 | Replicates and uncertainty | 3 matched CSV replicates × SEM/95% CI | Replicate count and bootstrap metadata shown; uncertainty heatmap renders | `npm run test:ux:capabilities` |
| UX-05 | Baseline correction | None/negative-only/full-matrix | Original matrix retained; method and fitted baseline are reported; adjusted view changes only as selected | `npm run test:ux:capabilities` |
| UX-06 | Synergy barometer | Selected Drug A × Drug B dose | HSA, Bliss, Loewe, ZIP, and Consensus reference/observed/delta rows are finite | `npm run test:ux:capabilities` |
| UX-07 | Matrix export | Single matrix and replicate analysis | CSV has one row per dose pair and all reference, fitted, score, SEM, and CI columns | `npm run test:ux:capabilities` |
| UX-08 | Sample downloads and condition labels | 6 bundled downloads; underscore-derived JSON names; Excel metadata fallback | Correct filename/content; non-empty file; dynamic condition, axis, flip, and barometer labels | `npm run test:ux:sample-downloads` |

## Numerical and manual release checks

- [ ] `npm run test:unit` passes the legacy and capability numerical suites.
- [ ] Preview labels preserve meaningful text, remove only R's numeric `X` prefix, and pad leading decimals (`.25uM` → `0.25uM`).
- [ ] Uploading replicate files with mismatched dimensions or concentration labels produces a clear validation error.
- [ ] Single-file input retains the previous default: no baseline correction, fitted single-agent curves enabled, Bliss score heatmap.
- [ ] Score maps use a zero-centred divergent scale; observed, reference, and fitted-response maps use a sequential scale.
- [ ] X/Y flips reorder both values and concentration labels; Z inversion changes signs/colour direction without changing the underlying calculation.
- [ ] PDF, SVG, and EPS exports match the active model, matrix type, uncertainty labels, and flips.
- [ ] CSV values retain calculation precision; rounding is limited to visual labels and summary display.
- [ ] Score/reference CSV includes the resolved Condition A and Condition B names for every dose pair.
- [ ] Apache-facing and native Shiny Server routes return HTTP 200 after deployment.
- [ ] Shiny Server logs contain no calculation, render, or session errors during all automated suites.

## Release evidence

- `test-results/checkerboardr-sample-heatmaps/report.json`
- `test-results/checkerboardr-ux/report.json`
- `test-results/checkerboardr-capabilities/report.json`
- `test-results/checkerboardr-sample-downloads/report.json`
- Screenshots and downloaded CSV under the corresponding result directories.
