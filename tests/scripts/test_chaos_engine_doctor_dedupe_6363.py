"""#6363: doctor must not print the same warning row twice."""

from __future__ import annotations

import unittest

from tests.scripts.test_chaos_engine_installer import MODULE

GAP = "GAP-EXIT2: verify host trust."


def _document(*, failing: bool) -> dict[str, object]:
    components: dict[str, object] = {"core": {"status": "healthy"}}
    if failing:
        components["memory"] = {"status": "missing", "taskImpact": "required"}
    return {
        "kind": "doctor",
        "status": "unhealthy" if failing else "healthy",
        "commit": "abc123",
        "components": components,
        "kernel": {
            "capabilities": {
                "copilot": {"blockingGap": GAP, "processExit2Honored": False},
                "grok": {"blockingGap": GAP, "processExit2Honored": False},
            }
        },
    }


class DoctorWarningDedupeTest(unittest.TestCase):
    def _assert_unique_rows(self, text: str) -> None:
        rows = [line for line in text.splitlines() if line.startswith("[")]
        self.assertEqual(len(rows), len(set(rows)), text)

    def test_happy_path_report_has_unique_warning_rows(self) -> None:
        text = MODULE.format_health_report(_document(failing=False))
        self.assertEqual(text.count(f"[warning] host/copilot — {GAP}"), 1)
        self._assert_unique_rows(text)

    def test_failure_report_has_unique_warning_rows(self) -> None:
        text = MODULE.format_health_report(_document(failing=True))
        self.assertEqual(text.count(f"[warning] host/copilot — {GAP}"), 1)
        self._assert_unique_rows(text)

    def test_dedupe_helper_keeps_first_order_and_plain_lines(self) -> None:
        lines = ["[warning] a", "  fix-next: x", "[warning] a", "  fix-next: x", "[info] b"]
        self.assertEqual(
            MODULE._dedupe_severity_rows(lines),
            ["[warning] a", "  fix-next: x", "  fix-next: x", "[info] b"],
        )


if __name__ == "__main__":
    unittest.main()
