"""Agent Plugin Live Acceptance dispatch filter and conclusion-only watch (#6283)."""

from __future__ import annotations

import unittest

from scripts.ci.acceptance_job_filter import (
    INSTALLER_PART1_GATE,
    acceptance_selection,
    dispatch_command,
    watch_acceptance_jobs,
)


class AcceptanceJobFilterTest(unittest.TestCase):
    def test_installer_part1_dispatch_skips_the_rest_of_the_matrix_and_weekly_jobs(self):
        selection = acceptance_selection(
            "workflow_dispatch",
            "",
            ",".join(INSTALLER_PART1_GATE),
        )
        self.assertEqual(
            [{"os": "windows-2025", "part": 1}, {"os": "macos-15", "part": 1}],
            selection["cross_matrix"],
        )
        self.assertEqual(["chaos-engine-cross-platform"], selection["enabled"])
        self.assertNotIn("deterministic-harness-full", selection["enabled"])
        self.assertNotIn("chaos-engine-live-installer", selection["enabled"])
        self.assertNotIn("ubuntu-22.04", {cell["os"] for cell in selection["cross_matrix"]})
        self.assertNotIn(2, {cell["part"] for cell in selection["cross_matrix"]})
        excluded = {(cell["os"], cell["part"]) for cell in selection["cross_exclude"]}
        self.assertIn(("ubuntu-22.04", 1), excluded)
        self.assertIn(("windows-2025", 2), excluded)
        self.assertNotIn(("windows-2025", 1), excluded)
        self.assertNotIn(("macos-15", 1), excluded)

    def test_empty_dispatch_keeps_weekly_jobs_and_schedule_does_not(self):
        dispatched = acceptance_selection("workflow_dispatch", "", "")
        self.assertIn("deterministic-harness-full", dispatched["enabled"])
        self.assertEqual(6, len(dispatched["cross_matrix"]))
        weekday = acceptance_selection("schedule", "15 4 * * 2", "")
        self.assertNotIn("deterministic-harness-full", weekday["enabled"])
        self.assertIn("chaos-engine-cross-platform", weekday["enabled"])
        monday = acceptance_selection("schedule", "15 4 * * 1", "")
        self.assertIn("deterministic-harness-full", monday["enabled"])

    def test_dispatch_command_names_only_the_gate(self):
        command = dispatch_command("ShaftHQ/SHAFT_ENGINE", "abc", INSTALLER_PART1_GATE)
        self.assertEqual(
            [
                "gh",
                "workflow",
                "run",
                "agent-plugin-acceptance.yml",
                "--repo",
                "ShaftHQ/SHAFT_ENGINE",
                "--ref",
                "abc",
                "-f",
                "jobs=installer-part-1-windows-2025,installer-part-1-macos-15",
            ],
            command,
        )

    def test_watcher_prints_conclusions_and_drops_intermediate_rows(self):
        windows = "ChaosEngine installer acceptance (windows-2025, part 1)"
        macos = "ChaosEngine installer acceptance (macos-15, part 1)"
        snapshots = [
            [
                {"name": windows, "conclusion": "", "status": "in_progress"},
                {"name": macos, "conclusion": None, "status": "queued"},
            ],
            [
                {"name": windows, "conclusion": "success"},
                {"name": macos, "conclusion": "success"},
            ],
        ]
        captured = watch_acceptance_jobs(snapshots)
        self.assertIn(f"{windows}: success", captured)
        self.assertIn(f"{macos}: success", captured)
        self.assertNotIn("in_progress", captured)
        self.assertNotIn("queued", captured)
        self.assertNotIn("gh run view", captured)
        self.assertNotIn("|", captured)


if __name__ == "__main__":
    unittest.main()
