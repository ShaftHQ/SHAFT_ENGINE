"""GAP-EXIT2 sentence, dual hook report, and one activation card (#6137)."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load(name: str, path: Path):
    import sys
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


kernel = _load("kernel_6137", ROOT / "chaos-engine/hooks/kernel.py")
install = _load("install_6137", ROOT / "chaos-engine/install.py")
lifecycle = _load("lifecycle_6137", ROOT / "chaos-engine/hooks/lifecycle.py")


def _sentence() -> str:
    for line in (ROOT / "chaos-engine/references/host-parity-matrix.md").read_text(encoding="utf-8").splitlines():
        if line.startswith("GAP-EXIT2-SENTENCE:"):
            return line.split(":", 1)[1].strip()
    raise AssertionError("sentence missing")


class GapExit2PayloadTest(unittest.TestCase):
    def test_grok_and_copilot_deny_payloads_use_the_matrix_sentence_and_exit_2(self):
        sentence = _sentence()
        self.assertEqual(kernel.gap_exit2_sentence(), sentence)
        for host in ("grok", "copilot"):
            capability = kernel.HOST_CAPABILITIES[host]
            self.assertEqual(2, capability.deny_exit_code)
            self.assertFalse(capability.process_exit2_honored)
            self.assertEqual(sentence, capability.blocking_gap)
            payload = {"decision": "block", "reason": capability.blocking_gap}
            adapted = kernel.adapt_hook_output(payload, "PreToolUse", host)
            rendered = json.dumps(adapted)
            self.assertIn(sentence, rendered)
            self.assertIn("deny", rendered)
        card = (ROOT / "chaos-engine/references/lifecycle-hooks.md").read_text(encoding="utf-8")
        self.assertEqual(1, card.count("## Ultra activation card"))
        self.assertIn("does not paste full vendor skill bodies", card)
        context = lifecycle.session_start_context("token", "Follow the router.")
        self.assertEqual(1, context.count("ChaosEngine:"))
        self.assertNotIn("# Caveman", context)
        self.assertNotIn("name: caveman", context)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".claude").mkdir()
            (project / ".grok" / "hooks").mkdir(parents=True)
            (project / ".claude" / "settings.json").write_text('{"hooks":"chaos"}', encoding="utf-8")
            (project / ".grok" / "hooks" / "lifecycle.json").write_text('{"chaos":true}', encoding="utf-8")
            result = {"status": "healthy", "components": {}}
            install.apply_dual_grok_hook_doctor(result, project)
            row = result["components"]["grok-hook-surfaces"]
            self.assertEqual("sync-advisory", row["status"])
            self.assertIn("both Claude-compat and native .grok hooks are active", row["reason"])


if __name__ == "__main__":
    unittest.main()
