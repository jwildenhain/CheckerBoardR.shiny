# CheckerboardR

CheckerboardR is a browser-based R Shiny application for analysing two-agent checkerboard experiments. It accepts viability/OD or normalized inhibition matrices, calculates endpoint-based FICI and response-surface interaction scores, and produces publication-ready plots.

**Live application:** <https://checkerboardr.chemgrid.org/>  
**Source:** <https://github.com/jwildenhain/CheckerBoardR.shiny>

## Start here

1. Read [Preparing and loading data](Data-input-and-preprocessing.md).
2. Choose a method using [Algorithms and scientific references](Algorithms-and-references.md).
3. Configure the display using [Plots and visualization settings](Plots-and-settings.md).
4. For programmatic analysis, follow [MCP setup and LLM workflow](MCP-setup-and-LLM-workflow.md).

## What CheckerboardR reports

- Minimum fractional inhibitory concentration index (FICI/ΣFIC) at a selectable inhibition endpoint.
- HSA, Bliss, Loewe, ZIP and conservative consensus reference and score matrices.
- Observed, fitted and expected response matrices.
- Single-agent 4-parameter logistic fits, IC50 estimates and Hill slopes.
- Replicate SD, SEM and bootstrap 95% confidence intervals when matched replicates are uploaded.
- A dose-pair barometer comparing the selected well against every response-surface model.
- PDF, SVG and EPS figures plus an auditable CSV containing scores, reference effects, FICI and uncertainty statistics.

## Important interpretation note

The methods use different definitions of *no interaction*. A positive Bliss score and a positive Loewe score need not agree because they answer different biological questions. Inspect the raw data, single-agent curves, uncertainty and more than one reference model before drawing a biological conclusion. CheckerboardR is an analysis aid, not a substitute for replication, experimental controls or domain-specific validation.

