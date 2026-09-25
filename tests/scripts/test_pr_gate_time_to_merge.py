"""
Workflow pins for the PR time-to-merge epic (#6184).

Each class pins one child issue so a later edit cannot silently drop a leg,
move coverage off every path, or re-couple a slow job to the merge path.
"""

from __future__ import annotations

import unittest
from pathlib import Path

import yaml

from scripts.ci import chaos_installer_tier as tier

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github/workflows"


def load(name: str) -> dict:
    # YAML 1.1 reads a bare `on:` key as boolean True; restore the workflow name.
    workflow = yaml.safe_load((WORKFLOWS / name).read_text(encoding="utf-8"))
    if True in workflow:
        workflow["on"] = workflow.pop(True)
    return workflow


class MacosInstallerTierTest(unittest.TestCase):
    """#6187: macOS fresh installer is post-merge, nightly, or opt-in on PRs."""

    def test_pull_request_without_label_skips_macos(self) -> None:
        self.assertEqual(["ubuntu-22.04", "windows-2025"], tier.installer_os("pull_request", set()))
        self.assertEqual(["windows-2025"], tier.contracts_os("pull_request", {"enhancement"}))

    def test_labelled_pull_request_and_main_push_include_macos(self) -> None:
        for event, labels in (("pull_request", {"ci:installer-macos"}), ("push", set())):
            with self.subTest(event=event):
                self.assertIn("macos-15", tier.installer_os(event, labels))
                self.assertIn("macos-15", tier.contracts_os(event, labels))
                self.assertIn("ubuntu-22.04", tier.installer_os(event, labels))
                self.assertIn("windows-2025", tier.installer_os(event, labels))

    def test_label_parsing_tolerates_push_null_and_garbage(self) -> None:
        self.assertEqual(set(), tier.parse_labels("null"))
        self.assertEqual(set(), tier.parse_labels(""))
        self.assertEqual(set(), tier.parse_labels("{not json"))
        self.assertEqual({"a"}, tier.parse_labels('["a", 1]'))

    def test_outputs_feed_both_matrices(self) -> None:
        rendered = tier.outputs("push", set()).splitlines()
        self.assertEqual(
            'installer_os=["ubuntu-22.04", "windows-2025", "macos-15"]', rendered[0]
        )
        self.assertEqual('installer_contracts_os=["windows-2025", "macos-15"]', rendered[1])

    def test_pr_gate_matrices_come_from_the_tier_step(self) -> None:
        jobs = load("pr-gate.yml")["jobs"]
        changes = jobs["changes"]
        self.assertIn("installer-tier", changes["outputs"]["installer_os"])
        tier_step = next(step for step in changes["steps"] if step.get("id") == "installer-tier")
        self.assertIn("scripts/ci/chaos_installer_tier.py", tier_step["run"])
        self.assertIn("/labels", tier_step["run"])
        self.assertEqual(
            "${{ fromJSON(needs.changes.outputs.installer_os) }}",
            jobs["chaos-installer-acceptance"]["strategy"]["matrix"]["os"],
        )
        self.assertEqual(
            "${{ fromJSON(needs.changes.outputs.installer_contracts_os) }}",
            jobs["chaos-installer-contracts"]["strategy"]["matrix"]["os"],
        )

    def test_nightly_runs_three_os_installer_daily_with_notify(self) -> None:
        workflow = load("agent-plugin-acceptance.yml")
        crons = [entry["cron"] for entry in workflow["on"]["schedule"]]
        self.assertIn("15 4 * * 1", crons)
        self.assertIn("15 4 * * 0,2-6", crons)
        jobs = workflow["jobs"]
        for name in ("chaos-engine-cross-platform", "chaos-engine-live-installer"):
            with self.subTest(job=name):
                self.assertNotIn("if", jobs[name])
                self.assertIn("macos-15", jobs[name]["strategy"]["matrix"]["os"])
                self.assertIn(name, jobs["notify"]["needs"])
        self.assertIn("github.event_name == 'schedule'", jobs["notify"]["if"])
        self.assertTrue(
            any("notify-nightly-failure" in step.get("uses", "") for step in jobs["notify"]["steps"])
        )


class ReleaseNoteGovernanceSplitTest(unittest.TestCase):
    """#6190: label edits re-run only the release-note check, which always reports."""

    def test_pr_gate_ignores_label_events_and_has_no_release_leg(self) -> None:
        workflow = load("pr-gate.yml")
        types = workflow["on"]["pull_request"]["types"]
        self.assertNotIn("labeled", types)
        self.assertNotIn("unlabeled", types)
        self.assertNotIn("release-governance", workflow["jobs"])
        self.assertNotIn("release-governance", workflow["jobs"]["summary"]["needs"])

    def test_release_workflow_is_a_single_always_reporting_job(self) -> None:
        workflow = load("release-note-governance.yml")
        trigger = workflow["on"]["pull_request"]
        for event_type in ("opened", "edited", "synchronize", "reopened", "labeled", "unlabeled"):
            self.assertIn(event_type, trigger["types"])
        self.assertNotIn("paths", trigger)
        self.assertNotIn("paths-ignore", trigger)
        self.assertEqual(["release-governance"], list(workflow["jobs"]))
        job = workflow["jobs"]["release-governance"]
        self.assertEqual("Release-note governance", job["name"])
        self.assertNotIn("if", job)
        run = " ".join(step.get("run", "") for step in job["steps"])
        self.assertIn("scripts/ci/validate_release_notes.py --event", run)
        self.assertIn("tests.scripts.test_validate_release_notes", run)
        self.assertEqual(
            "${{ github.event_name == 'pull_request' }}", workflow["concurrency"]["cancel-in-progress"]
        )


class CodeqlScopeTest(unittest.TestCase):
    """#6192: PR CodeQL only for Maven Java diffs; main and weekly stay unfiltered."""

    def test_pull_request_paths_pin_maven_java_inputs(self) -> None:
        trigger = load("security.yml")["on"]
        self.assertEqual(
            [
                "**/*.java",
                "**/pom.xml",
                "**/src/main/resources/**",
                ".mvn/**",
                ".github/codeql/**",
                ".github/workflows/security.yml",
            ],
            trigger["pull_request"]["paths"],
        )
        self.assertNotIn("paths-ignore", trigger["pull_request"])

    def test_main_push_and_weekly_schedule_scan_everything(self) -> None:
        trigger = load("security.yml")["on"]
        self.assertNotIn("paths", trigger["push"])
        self.assertEqual(["**.md"], trigger["push"].get("paths-ignore"))
        self.assertEqual(["main"], trigger["push"]["branches"])
        self.assertEqual(1, len(trigger["schedule"]))
        self.assertIn("workflow_dispatch", trigger)



class QualityConfigurationGuardTest(unittest.TestCase):
    """#6206: the quality validator runs in PR Gate and the release leg uploads coverage."""

    def test_release_intellij_verify_uploads_coverage_after_the_build(self) -> None:
        steps = load("mavenCentral_cd.yml")["jobs"]["cd_intellij_verify"]["steps"]
        uses = [step.get("uses", "") for step in steps]
        verify = uses.index("./.github/actions/intellij-verify")
        upload = uses.index("./.github/actions/upload-jacoco-coverage")
        self.assertGreater(upload, verify)
        self.assertTrue(steps[upload]["if"].startswith("always()"))

    def test_pr_gate_runs_the_quality_validator_on_workflow_and_pom_edits(self) -> None:
        job = load("pr-gate.yml")["jobs"]["workflow-timeouts"]
        runs = " ".join(step.get("run", "") for step in job["steps"])
        self.assertIn("scripts/ci/validate_quality_configuration.py", runs)
        self.assertIn("tests.scripts.test_validate_quality_configuration", runs)
        self.assertIn("needs.changes.outputs.dependencies == 'true'", job["if"])



class InstallerAcceptancePartsTest(unittest.TestCase):
    """#6205: each OS runs the installer acceptance in two parts that cover every module."""

    def test_parts_cover_the_whole_installer_module_and_the_other_modules_once(self) -> None:
        job = load("agent-plugin-acceptance.yml")["jobs"]["chaos-engine-cross-platform"]
        self.assertEqual([1, 2], job["strategy"]["matrix"]["part"])
        runs = {step["name"]: step for step in job["steps"] if "run" in step}
        shard = next(step for name, step in runs.items() if "shard" in name)
        self.assertNotIn("if", shard)
        self.assertIn("scripts.ci.unittest_shard", shard["run"])
        self.assertIn("tests.scripts.test_chaos_engine_installer", shard["run"])
        self.assertIn("--shard ${{ matrix.part }}/2", shard["run"])
        others = runs["Run fresh-install, dependency, and wrapper acceptance"]
        self.assertEqual("matrix.part == 1", others["if"])
        for module in ("bootstrap", "dependencies", "install_wrappers"):
            self.assertIn(f"tests.scripts.test_chaos_engine_{module}", others["run"])


if __name__ == "__main__":
    unittest.main()
