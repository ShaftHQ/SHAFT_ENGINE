"""Pre-done self-review gate (#7325): status-table drift, uncited gaps, counts, adversarial pass."""

from __future__ import annotations

import json
import subprocess  # nosec B404 - fixed argv only.
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
sys.path.insert(0, str(CE))
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

import harness_reachability as reach  # noqa: E402
import pre_done_review as gate  # noqa: E402

SKILL = "chaos-engine/skills/pre-done-self-review/SKILL.md"
GOOD = """| ID | Scenario | Status | Note |
|---|---|---|---|
| S1 | login | Passed | |
| S2 | pay | Blocked | rule: blocked-on-data needs mocks; alt: mock gateway fixture added |

Passed: 1, Blocked: 1

```mermaid
flowchart LR
  S1[Passed] --> S2[Blocked]
  class S2 blocked
```

## What's missing
No untracked scenarios; every rule in memory checked.
"""


class PreDoneReviewTest(unittest.TestCase):
    def test_clean_report_passes(self) -> None:
        self.assertEqual([], gate.review(GOOD))

    def test_stale_mermaid_status_fails(self) -> None:
        findings = gate.review(GOOD.replace("S2[Blocked]", "S2[Passed]"))
        self.assertTrue(any(f.startswith("drift: S2") for f in findings), findings)

    def test_stale_mermaid_class_fails(self) -> None:
        findings = gate.review(GOOD.replace("class S2 blocked", "class S2 done"))
        self.assertTrue(any(f.startswith("drift: S2") for f in findings), findings)

    def test_blocked_row_without_rule_or_alternative_fails(self) -> None:
        text = GOOD.replace("rule: blocked-on-data needs mocks; alt: mock gateway fixture added", "needs data")
        findings = gate.review(text)
        self.assertTrue(any(f.startswith("uncited gap: S2") for f in findings), findings)
        self.assertTrue(any(f.startswith("no alternative: S2") for f in findings), findings)

    def test_alternative_must_name_a_substitute(self) -> None:
        findings = gate.review(GOOD.replace("alt: mock gateway fixture added", "alt: waited"))
        self.assertTrue(any(f.startswith("no alternative: S2") for f in findings), findings)

    def test_skipped_counts_as_a_gap(self) -> None:
        findings = gate.review(GOOD.replace("| S1 | login | Passed | |", "| S1 | login | Skipped | |")
                               .replace("S1[Passed]", "S1[Skipped]").replace("Passed: 1, ", ""))
        self.assertTrue(any(f.startswith("uncited gap: S1") for f in findings), findings)

    def test_count_drift_fails(self) -> None:
        findings = gate.review(GOOD.replace("Blocked: 1", "Blocked: 10"))
        self.assertTrue(any(f.startswith("count drift") for f in findings), findings)

    def test_missing_or_empty_adversarial_section_fails(self) -> None:
        for text in (GOOD.split("## What's missing")[0], GOOD.split("## What's missing")[0] + "## What's missing\n\n"):
            with self.subTest(text=text[-30:]):
                self.assertIn("no adversarial pass", " ".join(gate.review(text)))

    def test_no_status_table_fails(self) -> None:
        self.assertEqual(1, len(gate.review("Done.\n")))

    def test_cli_exit_codes_and_json(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            good, bad = Path(tmp, "good.md"), Path(tmp, "bad.md")
            good.write_text(GOOD, encoding="utf-8")
            bad.write_text(GOOD.replace("S2[Blocked]", "S2[Passed]"), encoding="utf-8")
            script = str(CE / "pre_done_review.py")
            self.assertEqual(0, subprocess.run([sys.executable, script, str(good)], capture_output=True).returncode)
            out = subprocess.run([sys.executable, script, str(bad), "--json"], capture_output=True, text=True)
            self.assertEqual(1, out.returncode)
            self.assertEqual("fail", json.loads(out.stdout)["status"])
            self.assertEqual(2, subprocess.run([sys.executable, script], capture_output=True).returncode)

    def test_tool_exposes_pre_done(self) -> None:
        self.assertIn('"pre-done": "pre_done_review.py"', (CE / "tool.py").read_text(encoding="utf-8"))

    def test_done_and_handoff_paths_link_the_skill(self) -> None:
        for path in ("references/bot-entry.md", "references/iterative-flow.md", "references/multi-agent-pdca.md"):
            with self.subTest(path=path):
                self.assertIn("../skills/pre-done-self-review/SKILL.md", (CE / path).read_text(encoding="utf-8"))

    def test_every_host_reaches_the_skill(self) -> None:
        contexts = {"agents": "AGENTS.md", "grok-bot": "chaos-engine/references/bot-entry.md"}
        for host, context in contexts.items():
            with self.subTest(host=host):
                self.assertIn(SKILL, reach.link_walk(ROOT, context)[0])

    def test_skill_is_generic(self) -> None:
        text = (ROOT / SKILL).read_text(encoding="utf-8").lower()
        for name in ("hrsd", "mohab", "shaft"):
            self.assertNotIn(name, text)


if __name__ == "__main__":
    unittest.main()
