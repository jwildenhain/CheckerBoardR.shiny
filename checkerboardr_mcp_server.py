#!/usr/bin/env python3
import sys
import json
import math
import os
import subprocess
import tempfile
import xml.etree.ElementTree as ET

def log(msg):
    sys.stderr.write(f"LOG: {msg}\n")
    sys.stderr.flush()

def parse_xml_payload(xml_str):
    root = ET.fromstring(xml_str)
    
    # Drug A
    drug_a_node = root.find("drug_a")
    drug_a_name = drug_a_node.attrib.get("name", "Drug A")
    conc_a_str = drug_a_node.find("concentrations").text.strip()
    concentrations_a = [float(x.strip()) for x in conc_a_str.split(",")]
    
    # Drug B
    drug_b_node = root.find("drug_b")
    drug_b_name = drug_b_node.attrib.get("name", "Drug B")
    conc_b_str = drug_b_node.find("concentrations").text.strip()
    concentrations_b = [float(x.strip()) for x in conc_b_str.split(",")]
    
    # Data type
    data_type = root.find("data_representation").text.strip()
    
    # Matrix rows
    matrix_node = root.find("matrix")
    matrix = []
    for row in matrix_node.findall("row"):
        row_vals = [float(x.strip()) for x in row.text.strip().split(",")]
        matrix.append(row_vals)
        
    # Settings
    settings_node = root.find("settings")
    settings = {
        "synergy_model": settings_node.find("synergy_model").text.strip(),
        "plot_engine": settings_node.find("plot_engine").text.strip(),
        "theme_preset": settings_node.find("theme_preset").text.strip(),
        "orientation": settings_node.find("orientation").text.strip()
    }
    
    return {
        "drug_a": drug_a_name,
        "drug_b": drug_b_name,
        "concentrations_a": concentrations_a,
        "concentrations_b": concentrations_b,
        "data_type": data_type,
        "matrix": matrix,
        "settings": settings
    }

def execute_synergy_R(params, output_path):
    drug_a = params.get("drug_a", "Drug A")
    drug_b = params.get("drug_b", "Drug B")
    conc_a = params.get("concentrations_a", [])
    conc_b = params.get("concentrations_b", [])
    matrix = params.get("matrix", [])
    data_type = params.get("data_type", "viability")
    
    settings = params.get("settings", {})
    synergy_model = settings.get("synergy_model", "Bliss")
    plot_engine = settings.get("plot_engine", "2d_ggplot")
    theme_preset = settings.get("theme_preset", "Nature")
    orientation = settings.get("orientation", "synergism")

    allowed = {"data_type": {"viability", "inhibition"}, "synergy_model": {"Data", "HSA", "Bliss", "Loewe", "ZIP"}, "plot_engine": {"2d_ggplot", "1d_curves", "3d_base"}, "theme_preset": {"Nature", "Science", "The Economist", "Financial Times"}, "orientation": {"synergism", "antagonism"}}
    chosen = {"data_type": data_type, "synergy_model": synergy_model, "plot_engine": plot_engine, "theme_preset": theme_preset, "orientation": orientation}
    if any(value not in allowed[key] for key, value in chosen.items()):
        raise ValueError("Unsupported analysis setting")
    if len(conc_a) < 2 or len(conc_b) < 2 or len(matrix) != len(conc_b) or any(len(row) != len(conc_a) for row in matrix):
        raise ValueError("Matrix dimensions must match concentration vectors")
    numeric_values = conc_a + conc_b + [value for row in matrix for value in row]
    if any(isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value) for value in numeric_values):
        raise ValueError("Concentrations and matrix values must be numeric")
    r_data_type, r_model, r_engine = map(json.dumps, (data_type, synergy_model, plot_engine))
    r_theme, r_orientation = map(json.dumps, (theme_preset, orientation))
    r_title = json.dumps(f"{drug_a} + {drug_b} Synergy")
    r_output_path = json.dumps(output_path)
    r_project_root = json.dumps(os.path.dirname(os.path.abspath(__file__)))
    
    # Flatten matrix
    flat_matrix = []
    for row in matrix:
        flat_matrix.extend(row)
        
    # Formulate R script
    r_template = f"""
source(file.path({r_project_root}, "SynergyCalculations.R"))
source(file.path({r_project_root}, "Make3DPlotFunctions.R"))

# Recreate data matrix
flat_vals <- c({", ".join(map(str, flat_matrix))})
conc_a <- c({", ".join(map(str, conc_a))})
conc_b <- c({", ".join(map(str, conc_b))})

xx <- matrix(flat_vals, nrow = length(conc_b), ncol = length(conc_a), byrow = TRUE)
colnames(xx) <- paste0(conc_a, "uM")
rownames(xx) <- paste0(conc_b, "uM")
xx <- as.data.frame(xx)

res <- calculate_synergy(
  xx, 
  data_type = {r_data_type},
  use_fit = TRUE, 
  control_row = 1, 
  control_col = 1
)

# Output summary stats
scores <- if ({r_model} == "Data") res$raw_inhibition else res[[{r_model}]]$scores
max_score <- max(scores, na.rm = TRUE)
min_score <- min(scores, na.rm = TRUE)
mean_score <- mean(scores, na.rm = TRUE)

cat("STATS_START\\n")
cat(sprintf('{{"max_synergy": %.4f, "min_synergy": %.4f, "mean_synergy": %.4f, "drug_a_ic50": "%s", "drug_b_ic50": "%s"}}\\n',
            max_score, min_score, mean_score,
            if (!is.null(res$single_fit_A)) sprintf("%.4f", res$single_fit_A[3]) else "N/A",
            if (!is.null(res$single_fit_B)) sprintf("%.4f", res$single_fit_B[3]) else "N/A"))
cat("STATS_END\\n")

# Render plot
png({r_output_path}, width = 800, height = 600, res = 120)
if ({r_engine} == "2d_ggplot") {{
  p <- ggplot_synergy_heatmap(res, {r_model}, {r_orientation}, {r_theme}, {r_title})
  print(p)
}} else if ({r_engine} == "1d_curves") {{
  p <- ggplot_single_agent_fits(res, {r_theme})
  print(p)
}} else {{
  raw_plot(res, {r_model}, {r_theme}, theta = -60, phi = 30)
}}
dev.off()
"""

    with tempfile.NamedTemporaryFile(suffix=".R", mode="w", delete=False) as f:
        f.write(r_template)
        temp_script_path = f.name
        
    try:
        log(f"Running Rscript on {temp_script_path}")
        result = subprocess.run(
            ["Rscript", temp_script_path],
            capture_output=True,
            text=True,
            timeout=120
        )
        if result.returncode != 0:
            log(f"Rscript failed: {result.stderr}")
            raise RuntimeError(f"R synergy calculations failed: {result.stderr}")
            
        # Parse metrics out of stdout
        output_lines = result.stdout.split("\n")
        stats_json = None
        in_stats = False
        for line in output_lines:
            if line.strip() == "STATS_START":
                in_stats = True
                continue
            if line.strip() == "STATS_END":
                in_stats = False
                continue
            if in_stats:
                stats_json = json.loads(line.strip())
                break
                
        return stats_json
    finally:
        if os.path.exists(temp_script_path):
            os.remove(temp_script_path)

def main():
    log("CheckerBoardR MCP Server starting...")
    while True:
        try:
            line = sys.stdin.readline()
            if not line:
                break
                
            message = json.loads(line)
            method = message.get("method")
            msg_id = message.get("id")
            
            if method == "initialize":
                response = {
                    "jsonrpc": "2.0",
                    "id": msg_id,
                    "result": {
                        "protocolVersion": "2024-11-05",
                        "capabilities": {
                            "tools": {}
                        },
                        "serverInfo": {
                            "name": "checkerboardr-mcp-server",
                            "version": "2.1.0"
                        }
                    }
                }
                sys.stdout.write(json.dumps(response) + "\n")
                sys.stdout.flush()
                
            elif method == "notifications/initialized":
                pass
                
            elif method == "tools/list":
                response = {
                    "jsonrpc": "2.0",
                    "id": msg_id,
                    "result": {
                        "tools": [
                            {
                                "name": "calculate_synergy_mcp",
                                "description": "Loads a structured drug combination payload (JSON or XML), runs synergy calculations (HSA/Bliss/Loewe/ZIP) using the R backend, and saves the rendered plot as an image.",
                                "inputSchema": {
                                    "type": "object",
                                    "properties": {
                                        "data_format": {
                                            "type": "string",
                                            "enum": ["json", "xml"],
                                            "description": "Format of the drug combination payload"
                                        },
                                        "data_content": {
                                            "type": "string",
                                            "description": "The raw JSON or XML string containing drug names, concentrations, and the viability/inhibition matrix"
                                        },
                                        "output_path": {
                                            "type": "string",
                                            "description": "Absolute path where the resulting PNG plot image should be saved"
                                        }
                                    },
                                    "required": ["data_format", "data_content", "output_path"]
                                }
                            }
                        ]
                    }
                }
                sys.stdout.write(json.dumps(response) + "\n")
                sys.stdout.flush()
                
            elif method == "tools/call":
                params = message.get("params", {})
                tool_name = params.get("name")
                arguments = params.get("arguments", {})
                
                if tool_name == "calculate_synergy_mcp":
                    data_format = arguments.get("data_format")
                    data_content = arguments.get("data_content")
                    output_path = os.path.abspath(arguments.get("output_path"))
                    os.makedirs(os.path.dirname(output_path), exist_ok=True)
                    
                    try:
                        if data_format == "json":
                            parsed_data = json.loads(data_content)
                        else:
                            parsed_data = parse_xml_payload(data_content)
                            
                        stats = execute_synergy_R(parsed_data, output_path)
                        
                        summary_text = (
                            f"Success! Synergy plot generated and saved to: {output_path}\n\n"
                            f"=== Computed Synergy Summary ===\n"
                            f"Drug A: {parsed_data.get('drug_a')} | Drug B: {parsed_data.get('drug_b')}\n"
                            f"Reference Model: {parsed_data.get('settings', {}).get('synergy_model')}\n"
                            f"Max Synergy: {stats.get('max_synergy')}\n"
                            f"Max Antagonism: {stats.get('min_synergy')}\n"
                            f"Mean Grid Score: {stats.get('mean_synergy')}\n"
                            f"Drug A IC50 (Fitted 4PL): {stats.get('drug_a_ic50')}\n"
                            f"Drug B IC50 (Fitted 4PL): {stats.get('drug_b_ic50')}\n"
                        )
                        
                        response = {
                            "jsonrpc": "2.0",
                            "id": msg_id,
                            "result": {
                                "content": [
                                    {
                                        "type": "text",
                                        "text": summary_text
                                    }
                                ],
                                "isError": False
                            }
                        }
                    except Exception as e:
                        response = {
                            "jsonrpc": "2.0",
                            "id": msg_id,
                            "result": {
                                "content": [
                                    {
                                        "type": "text",
                                        "text": f"Error running synergy MCP: {str(e)}"
                                    }
                                ],
                                "isError": True
                            }
                        }
                    sys.stdout.write(json.dumps(response) + "\n")
                    sys.stdout.flush()
            else:
                if msg_id is not None:
                    response = {
                        "jsonrpc": "2.0",
                        "id": msg_id,
                        "error": {
                            "code": -32601,
                            "message": f"Method not found: {method}"
                        }
                    }
                    sys.stdout.write(json.dumps(response) + "\n")
                    sys.stdout.flush()
        except Exception as e:
            log(f"Unhandled error in loop: {str(e)}")

if __name__ == "__main__":
    main()
