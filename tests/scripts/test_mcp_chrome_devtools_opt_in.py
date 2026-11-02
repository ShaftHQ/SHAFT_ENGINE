"""Chrome DevTools MCP is optional, pinned, and telemetry-off (#reel-tools)."""

from __future__ import annotations

import importlib.util
import json
import sys
import tomllib
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OPTIONAL_ID = "chrome-devtools-mcp"
PIN = "chrome-devtools-mcp@1.12.0"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


HOSTS = _load("hosts_cdp_opt", ROOT / "chaos-engine/hosts.py")
POLICY = _load("policy_cdp_opt", ROOT / "chaos-engine/mcp_policy.py")
DEPS = json.loads((ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8"))


class ChromeDevToolsOptionalMcpTest(unittest.TestCase):
    def test_policy_lists_optional_id_not_cli_equivalent(self):
        self.assertIn(OPTIONAL_ID, POLICY.OPTIONAL_MCP_IDS)
        self.assertNotIn(OPTIONAL_ID, POLICY.CLI_EQUIVALENT_MCP_IDS)

    def test_dependencies_declare_pinned_npx_shape(self):
        contract = DEPS["dependencies"][OPTIONAL_ID]
        self.assertEqual("1.12.0", contract["minimumVersion"])
        self.assertEqual([], contract["executables"])
        self.assertEqual("npm-npx", contract["provider"])
        self.assertIn("chrome-devtools-mcp", contract["stableChannel"])

    def test_default_catalog_omits_server(self):
        self.assertNotIn(OPTIONAL_ID, HOSTS.owned_servers())
        tracked = json.loads((ROOT / ".mcp.json").read_text(encoding="utf-8"))
        self.assertNotIn(OPTIONAL_ID, tracked.get("mcpServers", {}))

    def test_with_mcp_publishes_pin_and_telemetry_flags(self):
        server = HOSTS.owned_servers(with_mcp=True)[OPTIONAL_ID]
        self.assertEqual("npx", server["command"])
        self.assertEqual(
            ["-y", PIN, "--no-usage-statistics", "--no-performance-crux"],
            server["args"],
        )
        doc = json.loads(HOSTS.json_content(None, with_mcp=True))
        self.assertEqual(server, doc["mcpServers"][OPTIONAL_ID])
        codex = tomllib.loads(HOSTS.codex_content(None, with_mcp=True).decode("utf-8"))
        self.assertEqual(
            ["-y", PIN, "--no-usage-statistics", "--no-performance-crux"],
            codex["mcp_servers"][OPTIONAL_ID]["args"],
        )

    def test_prefer_cli_doc_names_debug_aid_not_product_tests(self):
        text = (ROOT / "chaos-engine/references/prefer-cli-over-mcp.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("Chrome DevTools MCP", text)
        self.assertIn("--no-usage-statistics", text)
        self.assertIn("product automation regression tests", text)
        self.assertIn("Off by default", text)


if __name__ == "__main__":
    unittest.main()
