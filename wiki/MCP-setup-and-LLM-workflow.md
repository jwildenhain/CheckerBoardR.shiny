# MCP setup and LLM workflow

CheckerboardR includes a local stdio MCP server, `checkerboardr_mcp_server.py`. It exposes one tool:

```text
calculate_synergy_mcp
```

The tool accepts a structured JSON or XML checkerboard payload, runs the R calculation backend, writes a PNG and returns summary statistics including minimum FICI. It does not upload assay data to the public CheckerboardR website.

## Requirements

- Python 3
- R and `Rscript`
- The R packages used by CheckerboardR
- A local clone of the repository
- An MCP-capable LLM client with permission to read the data file and write the requested PNG path

## MCP client configuration

Use the absolute path to your clone. A generic MCP configuration is:

```json
{
  "mcpServers": {
    "checkerboardr": {
      "command": "python3",
      "args": [
        "/absolute/path/to/CheckerBoardR.shiny/checkerboardr_mcp_server.py"
      ]
    }
  }
}
```

Restart the LLM client after changing its MCP configuration. The client should discover `calculate_synergy_mcp` from the `checkerboardr` server.

## Supported tool settings

The payload's `settings` object controls the result:

| Field | Values |
|---|---|
| `synergy_model` | `Data`, `HSA`, `Bliss`, `Loewe`, `ZIP`, `Consensus` |
| `plot_engine` | `2d_ggplot`, `1d_curves`, `3d_base` |
| `theme_preset` | `Nature`, `Science`, `The Economist`, `Financial Times` |
| `orientation` | `synergism`, `antagonism` |

The MCP renderer intentionally omits interactive Plotly because a stdio tool must write a static image. Ask for `3d_base` when you need a 3D PNG.

## Run a CSV from an LLM console

The current MCP tool accepts raw **JSON or XML**, not CSV directly. The LLM can still start from CSV: ask it to read the file, interpret the header/row labels, convert the matrix to the documented JSON schema and call the MCP tool. This conversion step should preserve every numeric value.

Example prompt:

```text
Read /absolute/path/to/my_checkerboard.csv.

The first column contains Agent B concentrations and the remaining column
headers contain Agent A concentrations. The values are normalized inhibition
fractions. Agent A is ampicillin and Agent B is gentamicin.

Convert the CSV to a CheckerboardR JSON payload without changing or rounding
any values. Run the checkerboardr calculate_synergy_mcp tool using Bliss,
the 2D heatmap, Nature theme and synergism orientation. Save the plot to
/absolute/path/to/results/ampicillin-gentamicin-bliss.png.

Report minimum FICI, its interpretation, maximum and minimum Bliss score,
and the output image path. Show me the generated image.
```

To request another plot, state the desired engine explicitly:

```text
Run the same CSV again using Loewe and plot_engine 3d_base. Save it as
/absolute/path/to/results/ampicillin-gentamicin-loewe-3d.png.
```

or:

```text
Run the same CSV with plot_engine 1d_curves so I can inspect the two
single-agent dose-response fits before interpreting synergy.
```

## JSON payload produced by the LLM

For the example CSV in the data guide, the converted payload is:

```json
{
  "drug_a": "Illustrative Agent A",
  "drug_b": "Illustrative Agent B",
  "concentrations_a": [0, 1, 2, 4],
  "concentrations_b": [0, 2, 4, 8],
  "data_type": "inhibition",
  "matrix": [
    [0.00, 0.20, 0.60, 0.95],
    [0.10, 0.90, 0.65, 0.92],
    [0.50, 0.90, 0.94, 0.97],
    [0.95, 0.96, 0.98, 0.99]
  ],
  "settings": {
    "synergy_model": "Bliss",
    "plot_engine": "2d_ggplot",
    "theme_preset": "Nature",
    "orientation": "synergism"
  }
}
```

The LLM passes the serialized JSON as `data_content`, sets `data_format` to `json`, and supplies an absolute `.png` `output_path`.

## Direct tool-call shape

MCP clients normally construct this call automatically:

```json
{
  "name": "calculate_synergy_mcp",
  "arguments": {
    "data_format": "json",
    "data_content": "{...serialized payload...}",
    "output_path": "/absolute/path/to/results/checkerboard.png"
  }
}
```

## Validate the MCP server

From the repository root:

```bash
python3 tests/mcp_integration_test.py
```

The integration test covers initialization, tool discovery, JSON and XML runs, FICI reporting, PNG signatures, invalid formats, invalid paths and unknown tools.

## Troubleshooting

- **Tool not discovered:** restart the LLM client and confirm the server path is absolute.
- **`Rscript` not found:** install R or expose `Rscript` on the MCP server process PATH.
- **Unsupported setting:** use the exact case-sensitive values in the table above.
- **Matrix dimensions error:** the row count must equal `concentrations_b`; every row length must equal `concentrations_a`.
- **No FICI:** both single agents must reach the configured endpoint within the tested range.
- **Output path error:** use an absolute path ending in `.png` and ensure its parent is writable.
- **CSV values appear transposed:** columns are Agent A and rows are Agent B; verify this before the LLM converts the file.

