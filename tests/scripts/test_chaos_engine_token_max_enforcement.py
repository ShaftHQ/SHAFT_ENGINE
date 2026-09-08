"""Token-max enforcement: MCP uniqueness, discovery gate, Grok SessionStart pin."""

from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TokenMaxEnforcementTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mcp = load(ROOT / "chaos-engine/mcp_policy.py", "ce_mcp_policy")
        cls.gate = load(ROOT / "chaos-engine/discovery_gate.py", "ce_discovery_gate")
        cls.kernel = load(ROOT / "chaos-engine/hooks/kernel.py", "ce_kernel_tmax")

    def test_github_alias_pair_is_an_error(self):
        self.assertIsNone(self.mcp.uniqueness_error(["chaosengine-memory", "github"]))
        err = self.mcp.uniqueness_error(["github", "github-gh"])
        self.assertIsNotNone(err)
        self.assertIn("Duplicate MCP", err)

    def test_discovery_gate_denies_grep_until_retrieve_when_graph_exists(self):
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            (root / "graphify-out").mkdir()
            (root / "graphify-out" / "graph.json").write_text("{}", encoding="utf-8")
            reason = self.gate.deny_reason(
                event_name="PreToolUse",
                tool_name="Grep",
                commands=("rg foo",),
                session_id="s1",
                root=root,
            )
            self.assertIsNotNone(reason)
            self.assertIn("retrieve", reason.casefold())
            self.gate.record_retrieve("s1", root=root)
            self.assertIsNone(
                self.gate.deny_reason(
                    event_name="PreToolUse",
                    tool_name="Grep",
                    commands=("rg foo",),
                    session_id="s1",
                    root=root,
                )
            )

    def test_grok_session_start_stdout_not_honored(self):
        grok = self.kernel.HOST_CAPABILITIES["grok"]
        self.assertFalse(grok.session_start_stdout_honored)
        self.assertTrue(self.kernel.HOST_CAPABILITIES["claude"].session_start_stdout_honored)
        adapted = self.kernel.adapt_hook_output(
            {"additionalContext": "card"}, "PreToolUse", "grok"
        )
        self.assertEqual(
            "card",
            adapted["hookSpecificOutput"]["additionalContext"],
        )


if __name__ == "__main__":
    unittest.main()
