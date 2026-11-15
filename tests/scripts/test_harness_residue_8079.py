"""SHAFT leftovers after the harness move: shim, parity cites, ignore block, PR head ref."""

from __future__ import annotations

import json
import unittest
from pathlib import Path

import sys as _sys
from pathlib import Path as _Path

_sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402

ROOT = Path(__file__).resolve().parents[2]
INSTRUCTION_ONLY = ("opencode", "cursor", "grok-bot")


class HarnessResidueTest(unittest.TestCase):
    def test_powershell_shim_launches_the_harness_installer_out_of_process(self):
        text = (ROOT / "chaos-engine" / "install.ps1").read_text(encoding="utf-8")
        folded = text.casefold()
        self.assertNotIn("scriptblock", folded)
        self.assertNotIn("invoke-expression", folded)
        self.assertIn("-ExecutionPolicy Bypass", text)
        self.assertIn("-File", text)
        self.assertIn("PSCommandPath", text)
        self.assertIn("$savedErrorAction = $ErrorActionPreference", text)
        self.assertIn("$ErrorActionPreference = $savedErrorAction", text)
        self.assertNotIn("ShaftHQ/SHAFT_ENGINE", text.split("-File", 1)[-1])
        self.assertIn(
            "https://raw.githubusercontent.com/ShaftHQ/chaosengine-harness/main/install.ps1",
            text,
        )

    def test_instruction_only_hosts_cite_agents_md(self):
        parity = json.loads((ROOT / "scripts/ci/agent_harness_parity.json").read_text(encoding="utf-8"))
        for row in parity["capabilities"]:
            for host in INSTRUCTION_ONLY:
                cites = row.get(host) or []
                self.assertNotIn("chaos-engine/README.md", cites, row.get("id"))
                self.assertTrue(all(not str(path).startswith("chaos-engine/") for path in cites), row.get("id"))

    def test_origin_overlay_block_does_not_ignore_the_plugin_changelog(self):
        text = (ROOT / ".gitignore").read_text(encoding="utf-8")
        start = text.index("# CHAOSENGINE-ORIGIN-OVERLAY:START")
        end = text.index("# CHAOSENGINE-ORIGIN-OVERLAY:END")
        block = text[start:end]
        self.assertNotIn("agent-plugins/chaos-engine/", block)

    def test_pr_body_guard_receives_the_head_ref(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        marker = "python3 scripts/ci/validate_pr_closing_keywords.py"
        before = workflow[: workflow.index(marker)]
        step = before[before.rfind("- name:"):]
        self.assertIn("PR_HEAD_REF:", step)

    def test_box_disk_guard_loads_never_touch_from_worktree_hygiene(self):
        text = (ROOT / "scripts/ci/box_disk_guard.py").read_text(encoding="utf-8")
        self.assertIn("from scripts.ci.worktree_hygiene import load_never_touch", text)
        self.assertNotIn('sys.path.insert(0, str(root / "chaos-engine"))', text)
        self.assertNotIn("sys.path.insert(0, str(ROOT / \"chaos-engine\"))", text)

    def test_acceptance_selection_drops_unread_chaos_engine_outputs(self):
        workflow = (ROOT / ".github/workflows/agent-plugin-acceptance.yml").read_text(encoding="utf-8")
        selection = workflow.split("deterministic-harness-full:", 1)[0]
        self.assertNotIn("run_chaos_engine_", selection)
        self.assertNotIn("cross_exclude:", selection)
        self.assertNotIn("installer part 1", workflow.casefold())


if __name__ == "__main__":
    unittest.main()
