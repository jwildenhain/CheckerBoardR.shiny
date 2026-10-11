#!/usr/bin/env python3
import json
import pathlib
import subprocess
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[1]
SERVER = ROOT / "checkerboardr_mcp_server.py"


def exchange(messages):
    completed = subprocess.run(
        ["python3", str(SERVER)],
        input="".join(json.dumps(message) + "\n" for message in messages),
        text=True,
        capture_output=True,
        timeout=180,
        check=False,
    )
    responses = [json.loads(line) for line in completed.stdout.splitlines() if line.strip()]
    return completed, responses


def tool_call(request_id, data_format, data_content, output_path):
    return {
        "jsonrpc": "2.0",
        "id": request_id,
        "method": "tools/call",
        "params": {
            "name": "calculate_synergy_mcp",
            "arguments": {
                "data_format": data_format,
                "data_content": data_content,
                "output_path": output_path,
            },
        },
    }


def assert_png(path):
    assert path.exists(), f"Expected PNG was not created: {path}"
    content = path.read_bytes()
    assert len(content) > 500
    assert content[:8] == b"\x89PNG\r\n\x1a\n"


with tempfile.TemporaryDirectory(prefix="checkerboardr-mcp-") as directory:
    output_dir = pathlib.Path(directory)
    json_payload = (ROOT / "paclitaxel_carboplatin.json").read_text()
    xml_payload = (ROOT / "fluconazole_voriconazole.xml").read_text()
    json_png = output_dir / "json.png"
    xml_png = output_dir / "xml.png"

    completed, responses = exchange([
        {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}},
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
        tool_call(3, "json", json_payload, str(json_png)),
        tool_call(4, "xml", xml_payload, str(xml_png)),
        tool_call(5, "yaml", json_payload, str(output_dir / "invalid.png")),
        tool_call(6, "json", json_payload, "relative.png"),
        {"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "unknown", "arguments": {}}},
    ])

    assert completed.returncode == 0, completed.stderr
    assert len(responses) == 7, responses
    assert responses[0]["result"]["serverInfo"]["version"] == "2.1.3"
    assert responses[1]["result"]["tools"][0]["name"] == "calculate_synergy_mcp"
    assert responses[2]["result"]["isError"] is False
    assert "Paclitaxel" in responses[2]["result"]["content"][0]["text"]
    assert responses[3]["result"]["isError"] is False
    assert "Fluconazole" in responses[3]["result"]["content"][0]["text"]
    assert responses[4]["result"]["isError"] is True
    assert "data_format" in responses[4]["result"]["content"][0]["text"]
    assert responses[5]["result"]["isError"] is True
    assert "absolute path" in responses[5]["result"]["content"][0]["text"]
    assert responses[6]["error"]["code"] == -32602
    assert_png(json_png)
    assert_png(xml_png)
    assert not (output_dir / "invalid.png").exists()
    print("PASS: MCP initialize, discovery, JSON, XML, validation, and PNG output")
