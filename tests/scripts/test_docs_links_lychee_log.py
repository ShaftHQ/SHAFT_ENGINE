"""docs-links.yml logs lychee's own exit and prints the report (#7861)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github/workflows/docs-links.yml"


class DocsLinksLycheeLogTest(unittest.TestCase):
    def test_check_step_logs_lychee_exit_before_xargs_masks_it(self):
        text = WORKFLOW.read_text(encoding="utf-8")
        check = text.split("- name: Check links", 1)[1].split("- name: Summary", 1)[0]
        self.assertIn(":!.memory", check)
        self.assertIn(
            "chaos-engine/vendor/icm-architect/skills/icm-architect/assets/templates",
            check,
        )
        self.assertIn("lychee_exit=$code", check)
        self.assertIn("xargs -0", check)
        self.assertNotIn('xargs -0 "$RUNNER_TEMP/lychee-bin"', check)
        self.assertLess(check.index('"$RUNNER_TEMP/lychee-bin"'), check.index("lychee_exit=$code"))
        self.assertLess(check.index("lychee_exit=$code"), check.index('exit "$code"'))

    def test_summary_tees_the_report_into_the_job_log(self):
        text = WORKFLOW.read_text(encoding="utf-8")
        summary = text.split("- name: Summary", 1)[1].split(
            "- name: Open or update the external-links tracker", 1
        )[0]
        self.assertIn("lychee-report.md", summary)
        self.assertIn("head -c 60000", summary)
        self.assertIn('tee -a "$GITHUB_STEP_SUMMARY"', summary)


if __name__ == "__main__":
    unittest.main()
