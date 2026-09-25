"""CLI-equivalent MCP servers are opt-in; tracked host files stay portable (#6199)."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
import tomllib
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
OPTIONAL = {"chaosengine-memory", "chaosengine-mempalace", "context7"}


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


HOSTS = _load("hosts_6199", ROOT / "chaos-engine/hosts.py")


class DefaultCatalogTest(unittest.TestCase):
    def setUp(self):
        patcher = mock.patch.dict(os.environ)
        patcher.start()
        self.addCleanup(patcher.stop)
        os.environ.pop(HOSTS.WITH_MCP_ENV, None)

    def test_default_publishes_no_cli_equivalent_server(self):
        self.assertFalse(OPTIONAL & set(HOSTS.owned_servers()))
        rendered = json.loads(HOSTS.json_content(None))
        self.assertFalse(OPTIONAL & set(rendered["mcpServers"]))
        codex = tomllib.loads(HOSTS.codex_content(None).decode("utf-8"))
        self.assertFalse(OPTIONAL & set(codex.get("mcp_servers", {})))

    def test_with_mcp_opts_back_in(self):
        self.assertLessEqual(OPTIONAL, set(HOSTS.owned_servers(with_mcp=True)))
        codex = tomllib.loads(HOSTS.codex_content(None, with_mcp=True).decode("utf-8"))
        self.assertLessEqual(OPTIONAL, set(codex["mcp_servers"]))

    def test_opt_in_persists_through_a_project_marker_or_env(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.assertFalse(HOSTS.mcp_opt_in(project))
            HOSTS.write_mcp_opt_in(project, True)
            self.assertTrue(HOSTS.mcp_opt_in(project))
            HOSTS.write_mcp_opt_in(project, False)
            self.assertFalse(HOSTS.mcp_opt_in(project))
            os.environ[HOSTS.WITH_MCP_ENV] = "1"
            self.assertTrue(HOSTS.mcp_opt_in(project))

    def test_default_install_drops_previously_owned_optional_servers(self):
        before = HOSTS.json_content(None, with_mcp=True)
        document = json.loads(before)
        document["mcpServers"]["user-server"] = {"command": "mine"}
        after = json.loads(HOSTS.json_content(json.dumps(document).encode()))
        self.assertFalse(OPTIONAL & set(after["mcpServers"]))
        self.assertIn("user-server", after["mcpServers"])

    def test_foreign_same_name_server_is_kept(self):
        foreign = {"mcpServers": {"context7": {"command": "my-own-context7"}}}
        after = json.loads(HOSTS.json_content(json.dumps(foreign).encode()))
        self.assertEqual({"command": "my-own-context7"}, after["mcpServers"]["context7"])

    def test_tracked_mcp_json_carries_no_cli_equivalent_server(self):
        tracked = json.loads((ROOT / ".mcp.json").read_text(encoding="utf-8"))
        self.assertFalse(OPTIONAL & set(tracked.get("mcpServers", {})))

    def test_doctor_mcp_health_does_not_require_opted_out_servers(self):
        with tempfile.TemporaryDirectory() as temporary:
            status = HOSTS.mcp_runtime_status(Path(temporary))
        self.assertEqual({"status": "healthy", "detail": "mcp-opted-out"}, status)

    def test_install_cli_accepts_with_mcp(self):
        install = (ROOT / "chaos-engine/install.py").read_text(encoding="utf-8")
        self.assertIn('"--with-mcp"', install)


class PortableHostFilesTest(unittest.TestCase):
    def test_codex_config_never_embeds_the_managed_interpreter(self):
        managed = Path("/owned/runtime/bin/python")
        block = HOSTS.codex_content(None, managed_python=managed, with_mcp=True).decode()
        self.assertNotIn(str(managed), block)
        self.assertIn('command = "python3"', block)

    def test_node_hook_documents_never_embed_the_managed_node(self):
        node = Path("/owned/node/bin/node")
        copilot = HOSTS.copilot_hooks_document(node).decode()
        gemini = HOSTS.gemini_hooks_document(node).decode()
        self.assertNotIn(str(node), copilot)
        self.assertNotIn(str(node), gemini)
        self.assertIn("node .chaos-engine/hooks/launch.js copilot", copilot)

    def test_node_launcher_prefers_the_hook_python_pointer(self):
        launcher = (ROOT / "chaos-engine/hooks/launch.js").read_text(encoding="utf-8")
        self.assertIn(HOSTS.HOOK_PYTHON_POINTER, launcher)


if __name__ == "__main__":
    unittest.main()
