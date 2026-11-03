# Installing CheckerboardR

CheckerboardR can run as a native R Shiny application, a Docker container, or an application managed by Shiny Server. Its Python stdio MCP server uses the same R calculation files and can be registered independently with an MCP-capable LLM client.

## 1. Clone the repository

```bash
git clone git@github.com:jwildenhain/CheckerBoardR.shiny.git
cd CheckerBoardR.shiny
```

HTTPS alternative:

```bash
git clone https://github.com/jwildenhain/CheckerBoardR.shiny.git
cd CheckerBoardR.shiny
```

## 2. Native R installation

### Requirements

- R 4.x
- A C/C++ build toolchain and system development headers required by the selected R packages
- A modern browser

Install the R packages used by the application:

```bash
R -q -e "install.packages(c('shiny', 'plotly', 'ggplot2', 'RColorBrewer', 'jsonlite', 'xml2', 'readxl'), repos='https://cloud.r-project.org/')"
```

Start CheckerboardR from the repository root:

```bash
R -q -e "shiny::runApp('.', host='127.0.0.1', port=3838)"
```

Open <http://127.0.0.1:3838>.

To make the service reachable from another machine, bind to `0.0.0.0` only when the host firewall and reverse proxy are configured appropriately:

```bash
R -q -e "shiny::runApp('.', host='0.0.0.0', port=3838)"
```

## 3. Docker installation

Docker installs the application and required R packages in an image based on `rocker/shiny`:

```bash
docker build -t checkerboardr:2.1.5 .
docker run --name checkerboardr --rm -p 3839:3838 checkerboardr:2.1.5
```

Open <http://127.0.0.1:3839>.

For a background container:

```bash
docker run --name checkerboardr -d --restart unless-stopped -p 3839:3838 checkerboardr:2.1.5
```

Inspect its logs with:

```bash
docker logs checkerboardr
```

## 4. Shiny Server installation

Install R, the packages listed above and Shiny Server according to the operating system's supported procedure. Copy or clone the application into a dedicated directory such as:

```text
/srv/shiny-server/checkerboardr
```

The Shiny Server account must be able to read the application files and write to any explicitly configured export directory. Keep the Git checkout and deployment directory separate if releases need to be promoted atomically.

After deploying a release:

1. Restart or reload Shiny Server.
2. Verify the native Shiny route.
3. Verify the HTTPS reverse-proxy route.
4. Upload a known matrix and check the preview, FICI summary and each plot engine.
5. Run the browser regression tests against the deployed URL.

The canonical production URL is <https://checkerboardr.chemgrid.org/>.

## 5. MCP server installation

### Requirements

- Python 3
- `Rscript` available on `PATH`
- The native R packages listed above
- An MCP-capable LLM client

The MCP server uses stdio and does not need a listening network port. Register it with an absolute path:

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

Restart the LLM client and confirm that it discovers `calculate_synergy_mcp`.

The tool accepts JSON or XML payloads. To begin with CSV, allow the LLM client to read the CSV and ask it to convert the values to the CheckerboardR JSON schema without transposing or rounding them. Then state the required model and plot engine. Supported static MCP engines are:

- `2d_ggplot`
- `1d_curves`
- `3d_base`

Interactive Plotly is available in the Shiny web interface, not as a static MCP PNG renderer. The complete console workflow and example prompts are in [MCP setup and LLM workflow](wiki/MCP-setup-and-LLM-workflow.md).

Validate the MCP server:

```bash
python3 tests/mcp_integration_test.py
```

## 6. Test installation

Install the pinned Playwright development dependency:

```bash
npm install
npx playwright install chromium
```

Run calculation and MCP tests:

```bash
npm run test:unit
npm run test:mcp
```

Start CheckerboardR locally, then run browser tests in another terminal:

```bash
CHECKERBOARDR_URL=http://127.0.0.1:3838 npm run test:ux:sample-heatmaps
CHECKERBOARDR_URL=http://127.0.0.1:3838 npm run test:ux:sample-downloads
CHECKERBOARDR_URL=http://127.0.0.1:3838 npm run test:ux:matrix
CHECKERBOARDR_URL=http://127.0.0.1:3838 npm run test:ux:capabilities
CHECKERBOARDR_URL=http://127.0.0.1:3838 npm run test:ux:exports
CHECKERBOARDR_URL=http://127.0.0.1:3838 npm run test:seo
```

Generated reports and screenshots are written below `test-results/` and are excluded from Git.

## 7. Publish the GitHub Wiki

The repository contains Wiki-ready Markdown under `wiki/`. GitHub stores a repository Wiki in a separate Git repository. First enable **Wiki** under the GitHub repository settings and create the initial page in GitHub. Then:

```bash
git clone git@github.com:jwildenhain/CheckerBoardR.shiny.wiki.git /tmp/checkerboardr-wiki
cp wiki/*.md /tmp/checkerboardr-wiki/
mkdir -p /tmp/checkerboardr-wiki/images
cp wiki/images/*.png /tmp/checkerboardr-wiki/images/
cd /tmp/checkerboardr-wiki
git add .
git commit -m "Document CheckerboardR plots, algorithms and MCP workflow"
git push origin master
```

If GitHub initializes the Wiki with `main` rather than `master`, push the checked-out branch instead. Verify image rendering and sidebar navigation after publication.

## 8. Troubleshooting

### R package load failure

Start R and run `sessionInfo()`, then reinstall the named missing package. System libraries may be required when compiling `xml2` or graphical dependencies from source.

### Port already in use

Choose another local port:

```bash
R -q -e "shiny::runApp('.', host='127.0.0.1', port=3839)"
```

### MCP cannot find R

Run `Rscript --version` in the environment that launches the MCP client. GUI applications may inherit a smaller `PATH` than an interactive shell.

### FICI is not calculable

Both single agents must reach the selected inhibition endpoint within the tested concentrations. Expand the experimentally tested range or choose a scientifically justified prespecified endpoint; do not infer an untested MIC.

### CSV appears transposed

Columns represent Agent A and rows represent Agent B. Confirm that the LLM preserved this orientation during CSV-to-JSON conversion.

