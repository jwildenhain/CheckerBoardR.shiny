# Preparing and loading data

## Matrix layout

CheckerboardR expects a rectangular dose matrix:

- Columns are concentrations of **Agent A**.
- Rows are concentrations of **Agent B**.
- Exactly one column and one row must represent zero concentration.
- For viability or OD input, the default untreated control is row 1, column 1.
- Concentration labels must contain numeric doses, such as `0`, `0.25`, `1uM` or `8 µg/mL`.

Example CSV:

```csv
Agent_B,0,1,2,4
0,0.00,0.20,0.60,0.95
2,0.10,0.90,0.65,0.92
4,0.50,0.90,0.94,0.97
8,0.95,0.96,0.98,0.99
```

The values above are normalized inhibition fractions. For percentages, use `0` to `100`; for fractions, use `0` to `1`.

## Data sources

### Sample data

The bundled examples cover antifungal, anticancer, antagonistic, JSON, XML and Excel-style checkerboards. Click the displayed filename to download the exact source fixture.

### Upload file

Upload CSV, TAB, TSV or text matrices. Select whether the first row contains concentration headers and choose comma, tab or semicolon separation. Multiple uploaded files are treated as independent replicates and must have identical dimensions and concentration labels.

### Paste data

Paste a delimited matrix, JSON payload or XML payload. JSON and XML can also set agent names, concentration vectors, data representation and visualization defaults.

## Preprocessing settings

| Setting | Meaning | Guidance |
|---|---|---|
| Input Data Representation | `viability` normalizes against the selected control and converts to inhibition as `1 - viability`; `inhibition` uses normalized inhibition/cell-death values directly | Select the meaning of the uploaded assay values, not the desired plot direction |
| Control Row / Column | 1-indexed untreated control coordinates used for viability/OD normalization | Confirm these coordinates before interpreting any score |
| Fitted baseline correction: None | Preserves the normalized inhibition matrix | Recommended default and most auditable choice |
| Correct negative inhibition values only | Applies the fitted baseline adjustment only to negative inhibition values | Useful when small below-control responses are attributable to baseline drift |
| Correct full inhibition matrix | Applies the baseline adjustment across the matrix | Use only with a documented experimental rationale |
| FICI inhibition endpoint | Percentage inhibition used to determine single-agent endpoint concentrations and qualifying combination wells | Default is 90%; MIC-oriented studies should use a prespecified endpoint |
| Bootstrap iterations | Resamples independent replicate matrices to calculate cell-level 95% intervals | Available for multiple uploaded files; 200 is the UI default |

## Replicates and uncertainty

Replicate files are analysed independently and then summarized cell by cell. The 2D heatmap can label cells with the mean ± SEM or bootstrap 95% CI. Increasing bootstrap iterations improves Monte Carlo stability but increases run time. Do not upload technical exports of the same matrix as if they were independent biological replicates.

## FICI calculation

For a qualifying combination well:

```text
FICI = concentration A in combination / endpoint concentration A alone
     + concentration B in combination / endpoint concentration B alone
```

CheckerboardR reports the minimum finite value among wells reaching the chosen inhibition endpoint:

- **Synergy:** FICI ≤ 0.5
- **Indifference:** 0.5 < FICI ≤ 4
- **Antagonism:** FICI > 4

If either single agent does not reach the selected endpoint within the tested dose range, FICI is reported as not calculable. This avoids extrapolating an unobserved MIC.

