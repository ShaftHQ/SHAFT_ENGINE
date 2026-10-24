"""Pre-done self-review gate (#7325): status-table drift, uncited gaps, counts, adversarial pass."""

from __future__ import annotations

import contextlib
import io
import json
import sys
import tempfile
import unittest
from pathlib import Path

from tests.scripts.ce_neutral_terms import private_terms

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
| S2 | pay | Blocked | rule: blocked-on-data needs mocks; alt: mock gateway fixture `tests/scripts/test_pre_done_review.py::PreDoneReviewTest` |

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
        self.assertEqual([], gate.review(GOOD, [ROOT]))

    def test_stale_mermaid_status_fails(self) -> None:
        findings = gate.review(GOOD.replace("S2[Blocked]", "S2[Passed]"))
        self.assertTrue(any(f.startswith("drift: S2") for f in findings), findings)

    def test_stale_mermaid_class_fails(self) -> None:
        findings = gate.review(GOOD.replace("class S2 blocked", "class S2 done"))
        self.assertTrue(any(f.startswith("drift: S2") for f in findings), findings)

    def test_blocked_row_without_rule_or_alternative_fails(self) -> None:
        text = GOOD.replace("rule: blocked-on-data needs mocks; alt: mock gateway fixture `tests/scripts/test_pre_done_review.py::PreDoneReviewTest`", "needs data")
        findings = gate.review(text)
        self.assertTrue(any(f.startswith("uncited gap: S2") for f in findings), findings)
        self.assertTrue(any(f.startswith("no alternative: S2") for f in findings), findings)

    def test_alternative_must_name_a_substitute(self) -> None:
        findings = gate.review(GOOD.replace("alt: mock gateway fixture `tests/scripts/test_pre_done_review.py::PreDoneReviewTest`", "alt: waited"))
        self.assertTrue(any(f.startswith("no alternative: S2") for f in findings), findings)

    def test_alternative_negations_fail(self) -> None:
        for alt in ("alt: no mock written yet", "alt: mock candidate (fixture not yet written)",
                    "alt: TODO mock `tests/scripts/test_pre_done_review.py`",
                    "alt: pending fixture `tests/scripts/test_pre_done_review.py`"):
            with self.subTest(alt=alt):
                findings = gate.review(GOOD.replace(
                    "alt: mock gateway fixture `tests/scripts/test_pre_done_review.py::PreDoneReviewTest`", alt), [ROOT])
                self.assertTrue(any(f.startswith("no alternative: S2") for f in findings), findings)

    def test_alternative_must_reference_existing_test(self) -> None:
        for ref in ("tests/scripts/absent_fixture.py", "tests/scripts/test_pre_done_review.py::" + "Absent" + "Probe"):
            with self.subTest(ref=ref):
                findings = gate.review(GOOD.replace("tests/scripts/test_pre_done_review.py::PreDoneReviewTest", ref), [ROOT])
                self.assertTrue(any("no existing covering test" in f for f in findings), findings)
        self.assertEqual([], gate.review(GOOD, [ROOT]))

    def test_nothing_missing_needs_verification(self) -> None:
        base = GOOD.split("## What's missing")[0] + "## What's missing\n"
        self.assertIn("unverified adversarial pass", " ".join(gate.review(base + "- nothing\n", [ROOT])))
        self.assertEqual([], gate.review(base + "- nothing\n\nverified: rescanned rule list and table\n", [ROOT]))

    def test_hrsd_repro_fails(self) -> None:
        repro = ("| ID | Status | Notes |\n|---|---|---|\n"
                 "| S1 | Blocked | rule: G51 no test data. alt: no mock written yet |\n"
                 "## What's missing\n- nothing\n")
        findings = gate.review(repro, [ROOT])
        self.assertTrue(any(f.startswith("no alternative: S1") for f in findings), findings)
        self.assertTrue(any("unverified adversarial pass" in f for f in findings), findings)

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
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(0, gate.main([str(good)]))
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                self.assertEqual(1, gate.main([str(bad), "--json"]))
            self.assertEqual("fail", json.loads(out.getvalue())["status"])
            with contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(2, gate.main([]))
                self.assertEqual(2, gate.main([str(Path(tmp, "absent.md"))]))

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
        for name in ("mohab", "shaft", *private_terms()):
            self.assertNotIn(name, text)


if __name__ == "__main__":
    unittest.main()
