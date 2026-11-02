"""Pinned agnix conformance promotion tests (#4702)."""

import copy
import json
import subprocess
import shutil
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

try:
    from scripts.ci.agnix_conformance import (
        assess_diagnostics,
        build_trial_command,
        generated_overlay,
        load_contract,
        run_conformance,
        score_evaluation,
        stage_harness,
        staged_file_counts,
        validate_contract,
    )
except ImportError:
    assess_diagnostics = None
    build_trial_command = None
    generated_overlay = None
    load_contract = None
    run_conformance = None
    score_evaluation = None
    stage_harness = None
    staged_file_counts = None
    validate_contract = None


ROOT = Path(__file__).resolve().parents[2]
CMD = shutil.which("cmd") or "cmd"
CONTRACT_PATH = ROOT / "scripts/ci/agnix_conformance.json"
STAGED = {"candidates": 100, "total": 150}
LEDGER_ENTRY = {
    "rule": "XML-001",
    "level": "error",
    "path": "fixtures/chaos-engine/references/work-github-playbook.md",
    "message": "Unclosed XML tag '<n>'",
    "expected_count": 1,
    "reason": "Synthetic ledger entry for the exact-count contract.",
}


class AgnixConformanceTest(unittest.TestCase):
    def test_contract_and_evaluator_api_are_available(self):
        self.assertTrue(callable(load_contract))
        self.assertTrue(callable(validate_contract))

    def test_contract_pins_source_image_artifacts_and_exact_false_positive_fingerprints(self):
        contract = load_contract(ROOT)

        self.assertEqual(validate_contract(contract), [])
        self.assertEqual(
            contract["source"]["revision"],
            "3792c6cfdf2e56ddf205340fb5afe8f272218501",
        )
        self.assertEqual(contract["source"]["release_tag"], "v0.48.0")
        self.assertEqual(
            contract["source"]["release_commit"],
            "79e7753745f5e77e780fc0663014acbe5e33ee9a",
        )
        self.assertEqual(
            contract["image"]["id"],
            "sha256:561618e2c15bf2397621dd04f96926663a3b5616c189cf7e38db7e82f5c538ea",
        )
        self.assertEqual(
            {artifact["platform"] for artifact in contract["artifacts"]},
            {"linux-x86_64", "macos-aarch64", "windows-x86_64"},
        )
        # #6221: only real, still-present intentional findings may be listed.
        self.assertEqual(contract["allowlisted_findings"], [])
        self.assertEqual(contract["files_checked_floor"]["extensions"], [".md"])
        self.assertEqual(contract["files_checked_floor"]["minimum_percent"], 90)
        self.assertNotIn("expected_files_checked", contract)

    def test_contract_rejects_missing_sibling_bad_digest_unpinned_image_and_broad_allowlist(self):
        contract = load_contract(ROOT)

        mutations = []
        missing_platform = copy.deepcopy(contract)
        missing_platform["artifacts"].pop()
        mutations.append(missing_platform)
        bad_digest = copy.deepcopy(contract)
        bad_digest["artifacts"][0]["sha256"] = "0" * 63
        mutations.append(bad_digest)
        foreign_artifact = copy.deepcopy(contract)
        foreign_artifact["artifacts"][0]["url"] = "https://example.com/agnix.tar.gz"
        mutations.append(foreign_artifact)
        foreign_evaluation = copy.deepcopy(contract)
        foreign_evaluation["evaluation"]["corpus_url"] = "https://example.com/eval.tar.gz"
        mutations.append(foreign_evaluation)
        floating_image = copy.deepcopy(contract)
        floating_image["image"] = "ubuntu:24.04"
        mutations.append(floating_image)
        broad_allowlist = copy.deepcopy(contract)
        broad_allowlist["allowlisted_findings"] = [dict(LEDGER_ENTRY, path="")]
        mutations.append(broad_allowlist)
        zero_count = copy.deepcopy(contract)
        zero_count["allowlisted_findings"] = [dict(LEDGER_ENTRY, expected_count=0)]
        mutations.append(zero_count)
        not_a_ledger = copy.deepcopy(contract)
        not_a_ledger["allowlisted_findings"] = None
        mutations.append(not_a_ledger)
        exact_count_pin = copy.deepcopy(contract)
        exact_count_pin.pop("files_checked_floor")
        exact_count_pin["expected_files_checked"] = 273
        mutations.append(exact_count_pin)
        for bad_floor in (
            {"extensions": [], "minimum_percent": 90, "reason": "r"},
            {"extensions": ["md"], "minimum_percent": 90, "reason": "r"},
            {"extensions": [".md"], "minimum_percent": 0, "reason": "r"},
            {"extensions": [".md"], "minimum_percent": 101, "reason": "r"},
            {"extensions": [".md"], "minimum_percent": 90, "reason": " "},
        ):
            floor = copy.deepcopy(contract)
            floor["files_checked_floor"] = bad_floor
            mutations.append(floor)
        traversal = copy.deepcopy(contract)
        traversal["staging_paths"][0] = "../outside"
        mutations.append(traversal)

        for mutation in mutations:
            with self.subTest(mutation=mutation):
                self.assertNotEqual(validate_contract(mutation), [])

    def test_trial_command_reuses_candidate_intake_containment_and_disables_telemetry(self):
        contract = load_contract(ROOT)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            candidate = root / "candidate"
            fixtures = root / "fixtures"
            output = root / "output"
            candidate.mkdir()
            fixtures.mkdir()
            output.mkdir()

            command = build_trial_command(candidate, fixtures, output, contract)

        rendered = " ".join(command)
        for required in (
            "--pull never",
            "--network none",
            "--read-only",
            "--cap-drop ALL",
            "--security-opt no-new-privileges",
            "--user 65532:65532",
            "DO_NOT_TRACK=1",
            "/candidate/agnix",
            "--format json",
        ):
            self.assertIn(required, rendered)
        self.assertIn(contract["image"]["reference"], command)

    def test_empty_ledger_is_valid_and_accepts_a_clean_run(self):
        contract = load_contract(ROOT)
        self.assertEqual(validate_contract(contract), [])
        report = assess_diagnostics({"files_checked": 95, "diagnostics": []}, contract, STAGED)
        self.assertTrue(report["accepted"], report)
        new_error = {"file": "AGENTS.md", "rule": "NEW-001", "level": "error", "message": "new"}
        self.assertFalse(
            assess_diagnostics({"files_checked": 95, "diagnostics": [new_error]}, contract, STAGED)["accepted"]
        )

    def test_only_exact_allowlisted_errors_pass_and_count_drift_or_staleness_fails(self):
        contract = load_contract(ROOT)
        contract["allowlisted_findings"] = [dict(LEDGER_ENTRY, expected_count=2)]
        self.assertEqual(validate_contract(contract), [])
        diagnostics = [
            {
                "file": LEDGER_ENTRY["path"],
                "rule": LEDGER_ENTRY["rule"],
                "level": LEDGER_ENTRY["level"],
                "message": LEDGER_ENTRY["message"],
            }
            for _ in range(2)
        ]

        report = assess_diagnostics({"files_checked": 95, "diagnostics": diagnostics}, contract, STAGED)
        self.assertTrue(report["accepted"])
        self.assertEqual(report["unexpected_errors"], [])

        nested_substitution = copy.deepcopy(diagnostics)
        nested_substitution[0]["file"] = "nested/" + nested_substitution[0]["file"]
        self.assertFalse(
            assess_diagnostics(
                {"files_checked": 95, "diagnostics": nested_substitution}, contract, STAGED
            )["accepted"]
        )

        missing = assess_diagnostics({"files_checked": 95, "diagnostics": diagnostics[:-1]}, contract, STAGED)
        self.assertFalse(missing["accepted"])
        self.assertTrue(missing["allowlist_count_mismatches"])

        # #6221: an entry whose finding no longer occurs is stale and fails.
        stale = assess_diagnostics({"files_checked": 95, "diagnostics": []}, contract, STAGED)
        self.assertFalse(stale["accepted"])
        self.assertEqual(
            stale["allowlist_count_mismatches"],
            [{"rule": "XML-001", "path": LEDGER_ENTRY["path"], "expected": 2, "actual": 0}],
        )

        extra = copy.deepcopy(diagnostics)
        extra.append({"file": "AGENTS.md", "rule": "NEW-001", "level": "error", "message": "new"})
        self.assertFalse(
            assess_diagnostics({"files_checked": 95, "diagnostics": extra}, contract, STAGED)["accepted"]
        )
        unknown_level = copy.deepcopy(diagnostics)
        unknown_level.append({"file": "AGENTS.md", "rule": "NEW-002", "level": "critical", "message": "new"})
        self.assertFalse(
            assess_diagnostics({"files_checked": 95, "diagnostics": unknown_level}, contract, STAGED)["accepted"]
        )

    def test_files_checked_floor_is_derived_from_the_staged_surface(self):
        contract = load_contract(ROOT)
        # 100 staged Markdown candidates at 90 % -> at least 90; at most the 150 staged files.
        for files_checked, accepted in ((89, False), (90, True), (150, True), (151, False), (None, False)):
            with self.subTest(files_checked=files_checked):
                report = assess_diagnostics(
                    {"files_checked": files_checked, "diagnostics": []}, contract, STAGED
                )
                self.assertEqual(report["accepted"], accepted, report)
        # Growing ChaosEngine content moves the floor with it; no contract edit needed.
        grown = {"candidates": 300, "total": 600}
        self.assertTrue(assess_diagnostics({"files_checked": 290, "diagnostics": []}, contract, grown)["accepted"])
        self.assertFalse(assess_diagnostics({"files_checked": 95, "diagnostics": []}, contract, grown)["accepted"])

    def test_staged_file_counts_use_declared_extensions(self):
        contract = load_contract(ROOT)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "a").mkdir()
            (root / "a/SKILL.md").write_text("x", encoding="utf-8")
            (root / "README.MD").write_text("x", encoding="utf-8")
            (root / "a/tool.py").write_text("x", encoding="utf-8")
            self.assertEqual(staged_file_counts(root, contract), {"candidates": 2, "total": 3})

    def test_evaluation_floors_fail_closed_on_false_positive_or_negative(self):
        contract = load_contract(ROOT)
        passing = {
            "cases_run": 61,
            "cases_passed": 61,
            "cases_failed": 0,
            "overall_precision": 1.0,
            "overall_recall": 1.0,
            "overall_f1": 1.0,
            "rules": {
                "CC-001": {"rule_id": "CC-001", "tp": 1, "fp": 0, "fn_count": 0},
                "CDX-001": {"rule_id": "CDX-001", "tp": 1, "fp": 0, "fn_count": 0},
                "XP-001": {"rule_id": "XP-001", "tp": 1, "fp": 0, "fn_count": 0},
            },
        }

        self.assertTrue(score_evaluation(passing, contract)["accepted"])
        for field in ("overall_precision", "overall_recall"):
            failing = copy.deepcopy(passing)
            failing[field] = 0.94
            self.assertFalse(score_evaluation(failing, contract)["accepted"])
        missing_family = copy.deepcopy(passing)
        missing_family["rules"].pop("CDX-001")
        self.assertFalse(score_evaluation(missing_family, contract)["accepted"])

    def test_staging_copies_only_declared_harness_inputs_and_rejects_mixed_destination(self):
        contract = load_contract(ROOT)
        with tempfile.TemporaryDirectory() as directory:
            destination = Path(directory) / "fixtures"

            # #6202: generated host files come from the installer's overlay.
            copied = stage_harness(ROOT, destination, contract, generated_overlay(ROOT))

            self.assertIn("AGENTS.md", copied)
            self.assertTrue((destination / ".claude/settings.json").is_file())
            self.assertTrue((destination / ".memory/events.jsonl").is_file())
            self.assertFalse((destination / ".memory/memory").exists())
            with self.assertRaises(ValueError):
                stage_harness(ROOT, destination, contract)

    def test_staging_rejects_a_symlink_anywhere_in_declared_inputs(self):
        contract = load_contract(ROOT)
        with tempfile.TemporaryDirectory() as directory:
            destination = Path(directory) / "fixtures"

            def marks_agents_as_symlink(path):
                return path.name == "AGENTS.md"

            with mock.patch.object(Path, "is_symlink", autospec=True, side_effect=marks_agents_as_symlink):
                with self.assertRaisesRegex(ValueError, "symlink"):
                    stage_harness(ROOT, destination, contract)

    def test_contract_excludes_user_harness_templates_from_staging(self):
        """#6226: the ~/.claude templates are not repository agents."""
        contract = load_contract(ROOT)
        self.assertEqual(
            [exclusion["path"] for exclusion in contract["staging_exclusions"]],
            ["scripts/agents/user-harness"],
        )
        self.assertIn("scripts/agents", contract["staging_paths"])
        self.assertEqual(
            [], [row for row in contract["allowlisted_findings"] if "user-harness" in row["path"]]
        )

    def test_invalid_staging_exclusions_are_rejected(self):
        contract = load_contract(ROOT)
        for bad in (
            None,
            [{"path": "outside/dir", "reason": "r"}],
            [{"path": "scripts/agents", "reason": "r"}],
            [{"path": "scripts/agents/user-harness", "reason": " "}],
            [{"path": "scripts/agents/../x", "reason": "r"}],
            [{"path": "scripts/agents/user-harness", "reason": "r"}] * 2,
            [{"path": "scripts/agents/user-harness"}],
        ):
            mutated = copy.deepcopy(contract)
            mutated["staging_exclusions"] = bad
            with self.subTest(bad=bad):
                self.assertNotEqual(validate_contract(mutated), [])

    def test_staging_omits_excluded_subtree_and_fails_on_a_stale_exclusion(self):
        contract = load_contract(ROOT)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            (source / "declared/keep").mkdir(parents=True)
            (source / "declared/skip").mkdir(parents=True)
            (source / "declared/keep/AGENTS.md").write_text("keep\n", encoding="utf-8")
            (source / "declared/skip/README.md").write_text("skip\n", encoding="utf-8")
            contract["staging_paths"] = ["declared"]
            contract["staging_exclusions"] = [{"path": "declared/skip", "reason": "fixture"}]

            stage_harness(source, root / "fixtures", contract)

            self.assertTrue((root / "fixtures/declared/keep/AGENTS.md").is_file())
            self.assertFalse((root / "fixtures/declared/skip").exists())

            contract["staging_exclusions"] = [{"path": "declared/gone", "reason": "fixture"}]
            with self.assertRaisesRegex(ValueError, "stale"):
                stage_harness(source, root / "fixtures-2", contract)

    def test_staging_rejects_an_entry_that_resolves_outside_the_source_root(self):
        contract = load_contract(ROOT)
        contract["staging_paths"] = ["AGENTS.md"]
        contract["staging_exclusions"] = []
        with tempfile.TemporaryDirectory() as directory:
            destination = Path(directory) / "fixtures"
            outside = Path(directory) / "junction-target" / "AGENTS.md"
            real_resolve = Path.resolve

            def resolves_junction_outside(path, strict=False):
                if path.name == "AGENTS.md":
                    return outside
                return real_resolve(path, strict=strict)

            with mock.patch.object(
                Path, "resolve", autospec=True, side_effect=resolves_junction_outside
            ):
                with self.assertRaisesRegex(ValueError, "outside the source root"):
                    stage_harness(ROOT, destination, contract)

    @unittest.skipUnless(sys.platform == "win32", "directory junctions are Windows-specific")
    def test_staging_rejects_a_windows_directory_junction(self):
        contract = load_contract(ROOT)
        contract["staging_paths"] = ["declared"]
        contract["staging_exclusions"] = []
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            declared = source / "declared"
            outside = root / "outside"
            declared.mkdir(parents=True)
            outside.mkdir()
            (outside / "host-only.txt").write_text("HOST_ONLY", encoding="utf-8")
            junction = declared / "junction"
            created = subprocess.run(
                [CMD, "/c", "mklink", "/J", str(junction), str(outside)],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(created.returncode, 0, created.stderr or created.stdout)

            with self.assertRaisesRegex(ValueError, "outside the source root"):
                stage_harness(source, root / "fixtures", contract)

    @unittest.skipUnless(sys.platform == "win32", "directory junctions are Windows-specific")
    def test_staging_rejects_a_junction_to_an_undeclared_in_root_directory(self):
        contract = load_contract(ROOT)
        contract["staging_paths"] = ["declared"]
        contract["staging_exclusions"] = []
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            declared = source / "declared"
            undeclared = source / "undeclared"
            declared.mkdir(parents=True)
            undeclared.mkdir()
            (undeclared / "sibling-only.txt").write_text("SIBLING_ONLY", encoding="utf-8")
            junction = declared / "junction"
            created = subprocess.run(
                [CMD, "/c", "mklink", "/J", str(junction), str(undeclared)],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(created.returncode, 0, created.stderr or created.stdout)

            with self.assertRaisesRegex(ValueError, "reparse point"):
                stage_harness(source, root / "fixtures", contract)

    def test_runtime_executes_efficacy_evaluation_and_rejects_a_low_floor(self):
        contract = load_contract(ROOT)
        diagnostics = []
        for allowlist in contract["allowlisted_findings"]:
            diagnostics.extend(
                {
                    "file": allowlist["path"],
                    "rule": allowlist["rule"],
                    "level": allowlist["level"],
                    "message": allowlist["message"],
                }
                for _ in range(allowlist["expected_count"])
            )
        telemetry = subprocess.CompletedProcess(
            [], 0, "Configured: disabled\nEffective: disabled\n", ""
        )
        lint = subprocess.CompletedProcess(
            [],
            1,
            json.dumps(
                {
                    "files_checked": 1,
                    "diagnostics": diagnostics,
                }
            ),
            "",
        )
        low_efficacy = subprocess.CompletedProcess(
            [],
            0,
            "Evaluating: /fixtures/eval.yaml\n\n"
            + json.dumps(
                {
                    "cases_run": 61,
                    "cases_failed": 0,
                    "overall_precision": 0.94,
                    "overall_recall": 1.0,
                    "rules": {
                        "CC-001": {"rule_id": "CC-001"},
                        "CDX-001": {"rule_id": "CDX-001"},
                        "XP-001": {"rule_id": "XP-001"},
                    },
                }
            )
            + "\n\nFAIL efficacy floor missed\n",
            "",
        )
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name in ("candidate", "fixtures", "evaluation", "output"):
                (root / name).mkdir()
            (root / "fixtures/AGENTS.md").write_text("# fixture\n", encoding="utf-8")
            runner = mock.Mock(side_effect=[telemetry, lint, low_efficacy])

            report = run_conformance(
                root / "candidate",
                root / "fixtures",
                root / "evaluation",
                root / "output",
                contract,
                runner=runner,
            )

        self.assertEqual(runner.call_count, 3)
        self.assertFalse(report["accepted"])
        self.assertFalse(report["evaluation"]["accepted"])
        self.assertEqual(report["evaluation"]["precision"], 0.94)

    def test_scheduled_workflow_and_docs_reach_the_promotion(self):
        pr_gate = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        acceptance = (ROOT / ".github/workflows/agent-plugin-acceptance.yml").read_text(
            encoding="utf-8"
        )
        guidance = (ROOT / ".agents/skills/README.md").read_text(encoding="utf-8")

        self.assertNotIn("tests.scripts.test_agnix_conformance", pr_gate)
        self.assertNotIn("python scripts/ci/agnix_conformance.py --check-contract", pr_gate)
        self.assertIn("tests.scripts.test_agnix_conformance", acceptance)
        self.assertIn("agnix-conformance", acceptance)
        self.assertIn("--github-env", acceptance)
        self.assertNotIn(
            "https://github.com/agent-sh/agnix/releases/download/v0.48.0/agnix-x86_64-unknown-linux-gnu.tar.gz",
            acceptance,
        )
        self.assertNotIn(
            "da8a0fd2389f2fa442721ca1ecf447bc0de64bf629014f11336eaccbfe8aa2e8",
            acceptance,
        )
        self.assertNotIn(
            "sha256:561618e2c15bf2397621dd04f96926663a3b5616c189cf7e38db7e82f5c538ea",
            acceptance,
        )
        self.assertIn("$AGNIX_IMAGE_REFERENCE", acceptance)
        self.assertIn("--evaluation-root", acceptance)
        self.assertIn("scripts/ci/agnix_conformance.py", guidance)
        self.assertIn("scripts/ci/agnix_conformance.json", guidance)
        self.assertIn("tests/scripts/test_agnix_conformance.py", guidance)

    def test_direct_contract_entrypoint_is_portable(self):
        result = subprocess.run(
            [sys.executable, "scripts/ci/agnix_conformance.py", "--check-contract"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("agnix conformance contract is valid", result.stdout)

    def test_github_environment_is_derived_without_erasing_prior_entries(self):
        with tempfile.TemporaryDirectory() as directory:
            environment_file = Path(directory) / "github.env"
            environment_file.write_text("PREEXISTING=value\n", encoding="utf-8")

            result = subprocess.run(
                [
                    sys.executable,
                    "scripts/ci/agnix_conformance.py",
                    "--github-env",
                    str(environment_file),
                ],
                cwd=ROOT,
                capture_output=True,
                text=True,
                check=False,
            )
            values = environment_file.read_text(encoding="utf-8")

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertTrue(values.startswith("PREEXISTING=value\n"))
        self.assertIn("AGNIX_ARTIFACT_URL=https://github.com/agent-sh/agnix/", values)
        self.assertIn("AGNIX_IMAGE_ID=sha256:", values)


if __name__ == "__main__":
    unittest.main()
