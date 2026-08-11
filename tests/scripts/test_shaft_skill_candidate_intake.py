"""Quarantined online skill-candidate intake tests (#4643)."""

import json
import os
import tempfile
import unittest
from datetime import date
from inspect import signature
from pathlib import Path

try:
    from scripts.ci.shaft_skill_candidate_intake import (
        quarantine_command,
        freshness_findings,
        scan_candidate,
        validate_policy,
        validate_repository,
        validate_review,
    )
except ImportError:
    quarantine_command = None
    freshness_findings = None
    scan_candidate = None
    validate_repository = None
    validate_review = None
    validate_policy = None


ROOT = Path(__file__).resolve().parents[2]
POLICY_PATH = ROOT / "agent-plugins/shaft-skills/candidate-intake/policy.json"
REVIEW_PATH = ROOT / "agent-plugins/shaft-skills/candidate-intake/candidates.json"


class ShaftSkillCandidateIntakeTest(unittest.TestCase):
    def test_v2_policy_owns_categories_and_tool_adoption(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))

        self.assertEqual(policy["schema_version"], 2)
        self.assertIn("adopt-tool", policy["decision_kinds"])
        self.assertEqual(policy["freshness_contract"], {"max_age_days": 90})
        self.assertEqual(
            set(policy["tool_contract"]["required_platforms"]),
            {"linux-x86_64", "macos-aarch64", "windows-x86_64"},
        )
        self.assertEqual(
            set(policy["allowed_categories"]),
            {
                "agent-runtime",
                "catalog",
                "cross-client-harness",
                "cross-client-packaging",
                "documents",
                "guardrail-evaluation",
                "guidance-linter",
                "outcome-memory",
                "plugin-evaluation",
                "security-scanner",
                "skill-routing",
            },
        )

    def test_every_candidate_has_consistent_freshness_metadata(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))

        self.assertEqual(validate_review(review, policy), [])
        for candidate in review["candidates"]:
            freshness = candidate["freshness"]
            self.assertRegex(freshness["upstream_head"], r"^[0-9a-f]{40}$")
            if freshness["status"] == "current":
                self.assertEqual(freshness["upstream_head"], candidate["revision"])
            elif freshness["status"] == "outdated":
                self.assertNotEqual(freshness["upstream_head"], candidate["revision"])

    def test_adopt_tool_requires_pinned_platform_artifacts_and_promotion(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        candidate = review["candidates"][0]
        candidate["material_kind"] = "tool"
        candidate["decision"] = "adopt-tool"
        candidate["version"] = "v1.2.3"
        candidate["tool"] = {
            "version": "v1.2.3",
            "execution": "read-only no-network container",
            "artifacts": [
                {
                    "platform": "linux-x86_64",
                    "url": "https://example.test/tool.tar.gz",
                    "sha256": "0" * 64,
                },
                {
                    "platform": "macos-aarch64",
                    "url": "https://example.test/tool-macos.tar.gz",
                    "sha256": "1" * 64,
                },
                {
                    "platform": "windows-x86_64",
                    "url": "https://example.test/tool-windows.zip",
                    "sha256": "2" * 64,
                }
            ],
        }
        candidate["promotion_pr"] = "https://github.com/ShaftHQ/SHAFT_ENGINE/pull/9999"
        candidate["stage_results"] = {
            stage: {"status": "pass", "evidence": f"passed {stage}"}
            for stage in policy["required_stages"]
        }
        review["review_scope"]["rejected_candidate_ids"] = [
            row["id"] for row in review["candidates"] if row["decision"] == "reject"
        ]

        def validate_tool_review():
            return validate_review(review, policy, as_of=date(2026, 8, 11))

        self.assertEqual(validate_tool_review(), [])

        candidate["tool"]["artifacts"][0]["sha256"] = "short"
        defects = validate_tool_review()
        self.assertIn("tool-artifact", {defect["code"] for defect in defects})
        candidate["tool"]["artifacts"][0]["sha256"] = "0" * 64

        linux_artifact = candidate["tool"]["artifacts"].pop(0)
        self.assertIn("tool-artifact", {defect["code"] for defect in validate_tool_review()})
        candidate["tool"]["artifacts"].insert(0, linux_artifact)

        candidate["tool"]["version"] = ""
        self.assertIn("tool-evidence", {defect["code"] for defect in validate_tool_review()})
        candidate["tool"]["version"] = candidate["version"]

        candidate["promotion_pr"] = None
        self.assertIn("promotion-pr", {defect["code"] for defect in validate_tool_review()})
        candidate["promotion_pr"] = "https://github.com/ShaftHQ/SHAFT_ENGINE/pull/9999"

        candidate["freshness"]["status"] = "outdated"
        candidate["freshness"]["upstream_head"] = "f" * 40
        self.assertIn("promotion-freshness", {defect["code"] for defect in validate_tool_review()})
        candidate["freshness"] = {
            "checked_at": "2026-05-11",
            "upstream_head": candidate["revision"],
            "status": "current",
        }
        self.assertIn("promotion-freshness", {defect["code"] for defect in validate_tool_review()})

    def test_candidate_intake_api_is_available(self):
        self.assertTrue(callable(validate_repository))
        self.assertTrue(callable(validate_review))
        self.assertTrue(callable(scan_candidate))
        self.assertTrue(callable(quarantine_command))

    def test_repository_policy_and_review_are_valid(self):
        self.assertEqual(validate_repository(ROOT), [])

        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        decisions = {candidate["decision"] for candidate in review["candidates"]}
        self.assertEqual(
            set(policy["decision_kinds"]),
            {"adopt-code", "adopt-pattern", "adopt-tool", "retain-test-target", "reject"},
        )
        self.assertTrue({"adopt-pattern", "retain-test-target", "reject"}.issubset(decisions))
        self.assertFalse(review["code_adopted"])
        self.assertTrue(all(candidate["official_source"] for candidate in review["candidates"]))

    def test_policy_schema_and_security_booleans_require_exact_types(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        policy["schema_version"] = True
        self.assertIn("policy-schema", {row["code"] for row in validate_policy(policy)})

        for field, numeric_alias in (
            ("container_only", 1),
            ("read_only_root", 1),
            ("canonical_roots_mounted", 0),
        ):
            with self.subTest(field=field):
                policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
                policy["trial_contract"][field] = numeric_alias
                self.assertIn("trial-contract", {row["code"] for row in validate_policy(policy)})

    def test_review_covers_each_required_candidate_category_and_every_rejection(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        candidates = review["candidates"]

        self.assertEqual(
            {candidate["category"] for candidate in candidates},
            set(policy["allowed_categories"]),
        )
        self.assertEqual(
            set(review["review_scope"]["candidate_ids"]),
            {candidate["id"] for candidate in candidates},
        )
        self.assertEqual(
            set(review["review_scope"]["rejected_candidate_ids"]),
            {candidate["id"] for candidate in candidates if candidate["decision"] == "reject"},
        )

    def test_freshness_check_reports_outdated_and_expired_records(self):
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        review["candidates"] = review["candidates"][:2]
        review["candidates"][0]["freshness"] = {
            "checked_at": "2026-08-11",
            "upstream_head": "f" * 40,
            "status": "outdated",
        }
        review["candidates"][1]["freshness"] = {
            "checked_at": "2026-01-01",
            "upstream_head": review["candidates"][1]["revision"],
            "status": "current",
        }

        findings = freshness_findings(review, as_of=date(2026, 8, 11), max_age_days=90)

        self.assertEqual([finding["code"] for finding in findings], ["candidate-stale", "candidate-stale"])

    def test_review_rejects_future_dated_freshness(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        review["reviewed_at"] = "2099-01-01"
        review["candidates"][0]["freshness"]["checked_at"] = "2099-01-01"

        codes = {
            defect["code"]
            for defect in validate_review(review, policy, as_of=date(2026, 8, 11))
        }
        finding_codes = {
            finding["code"]
            for finding in freshness_findings(
                review,
                as_of=date(2026, 8, 11),
                max_age_days=90,
            )
        }

        self.assertIn("candidate-freshness", codes)
        self.assertIn("review-date", codes)
        self.assertEqual(finding_codes, {"candidate-stale", "review-future"})

    def test_review_rejects_mutable_or_incomplete_provenance(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        review["candidates"][0]["revision"] = "main"
        review["candidates"][0].pop("license")

        defects = validate_review(review, policy)

        self.assertTrue(any(defect["code"] == "immutable-revision" for defect in defects))
        self.assertTrue(any(defect["code"] == "candidate-field" for defect in defects))

    def test_review_rejects_empty_discovery_paths_and_incomplete_category_coverage(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        review["candidates"][0]["discovered_via"] = []
        removed = review["candidates"].pop()
        review["review_scope"]["candidate_ids"].remove(removed["id"])
        if removed["id"] in review["review_scope"]["rejected_candidate_ids"]:
            review["review_scope"]["rejected_candidate_ids"].remove(removed["id"])

        codes = {defect["code"] for defect in validate_review(review, policy)}

        self.assertIn("candidate-evidence", codes)
        self.assertIn("candidate-categories", codes)

    def test_adopt_code_requires_all_gates_and_a_separate_promotion_pr(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        candidate = review["candidates"][0]
        candidate["decision"] = "adopt-code"
        candidate["stage_results"]["quarantine_trial"] = {
            "status": "not_run",
            "evidence": "skipped",
        }
        candidate["promotion_pr"] = None
        review["review_scope"]["rejected_candidate_ids"] = [
            row["id"] for row in review["candidates"] if row["decision"] == "reject"
        ]

        defects = validate_review(review, policy)

        self.assertTrue(any(defect["code"] == "adopt-code-gates" for defect in defects))
        self.assertTrue(any(defect["code"] == "promotion-pr" for defect in defects))

    def test_pattern_adoption_requires_cleared_provenance_static_review_and_local_eval(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        candidate = next(row for row in review["candidates"] if row["decision"] == "adopt-pattern")
        candidate["stage_results"]["static_review"] = {
            "status": "not_applicable",
            "evidence": "skipped",
        }

        defects = validate_review(review, policy)

        self.assertTrue(any(defect["code"] == "pattern-gates" for defect in defects))

    def test_candidate_evidence_fields_cannot_be_empty(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        review["candidates"][0]["permissions"] = ""

        defects = validate_review(review, policy)

        self.assertTrue(any(defect["code"] == "candidate-evidence" for defect in defects))

    def test_review_rejects_schema_type_and_out_of_order_stage_bypasses(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        candidate = review["candidates"][0]
        review["schema_version"] = "1"
        review["reviewed_at"] = "not-a-date"
        candidate["official_source"] = "yes"
        candidate["adopted_files"] = ""
        candidate["stage_results"]["provenance_license"] = {
            "status": "not_run",
            "evidence": "skipped without a preceding HALT",
        }

        defects = validate_review(review, policy)
        codes = {defect["code"] for defect in defects}

        self.assertIn("review-schema", codes)
        self.assertIn("review-date", codes)
        self.assertIn("official-source", codes)
        self.assertIn("adopted-files", codes)
        self.assertIn("stage-order", codes)

    def test_review_schema_version_requires_a_concrete_integer(self):
        class IntegerSubclass(int):
            pass

        class SpoofedInteger:
            @property
            def __class__(self):
                return int

            def __eq__(self, other):
                return other == 1

        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        for schema_version in (True, IntegerSubclass(1), SpoofedInteger()):
            with self.subTest(schema_version=repr(schema_version)):
                review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
                review["schema_version"] = schema_version

                defects = validate_review(review, policy)

                self.assertTrue(any(defect["code"] == "review-schema" for defect in defects))

    def test_not_applicable_is_only_valid_for_the_quarantine_trial(self):
        policy = json.loads(POLICY_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        candidate = review["candidates"][0]
        candidate["stage_results"]["provenance_license"] = {
            "status": "not_applicable",
            "evidence": "attempted mandatory-stage waiver",
        }
        candidate["stage_results"]["static_review"] = {
            "status": "halt",
            "evidence": "later halt",
        }
        candidate["stage_results"]["quarantine_trial"] = {
            "status": "not_run",
            "evidence": "stopped",
        }
        candidate["stage_results"]["local_evaluation"] = {
            "status": "not_run",
            "evidence": "stopped",
        }

        defects = validate_review(review, policy)

        self.assertTrue(any(defect["code"] == "stage-status" for defect in defects))

    def test_static_scan_reports_executables_and_secret_material(self):
        with tempfile.TemporaryDirectory() as directory:
            candidate = Path(directory) / "candidate"
            candidate.mkdir()
            script = candidate / "run.py"
            script.write_text("print('safe')\n", encoding="utf-8")
            script.chmod(script.stat().st_mode | 0o111)
            (candidate / "fixture.txt").write_text(
                "-----BEGIN PRIVATE KEY-----\nnot-a-real-key\n",
                encoding="utf-8",
            )
            (candidate / "package.json").write_text(
                json.dumps({"scripts": {"postinstall": "node setup.js"}}),
                encoding="utf-8",
            )

            report = scan_candidate(candidate)

        self.assertIn("run.py", report["executable_files"])
        self.assertEqual(report["secret_findings"][0]["path"], "fixture.txt")
        self.assertEqual(report["install_hooks"], ["package.json:postinstall"])
        self.assertEqual(report["containment_violations"], [])

    def test_static_scan_reports_opaque_and_oversized_files(self):
        with tempfile.TemporaryDirectory() as directory:
            candidate = Path(directory) / "candidate"
            candidate.mkdir()
            (candidate / "binary.dat").write_bytes(b"\xff\xfe\x00")
            (candidate / "oversized.txt").write_bytes(b"x" * 1_000_001)

            report = scan_candidate(candidate)

        self.assertEqual(
            report["opaque_files"],
            [
                {"path": "binary.dat", "reason": "not-utf8"},
                {"path": "oversized.txt", "reason": "over-1mb"},
            ],
        )

    @unittest.skipIf(os.name == "nt", "ordinary Windows test users cannot create symlinks")
    def test_static_scan_rejects_a_symlink_escape(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            candidate = root / "candidate"
            candidate.mkdir()
            outside = root / "outside.txt"
            outside.write_text("outside\n", encoding="utf-8")
            (candidate / "escape").symlink_to(outside)

            report = scan_candidate(candidate)

        self.assertEqual(report["containment_violations"], ["escape"])

    def test_quarantine_command_is_no_network_read_only_nonroot_and_credential_free(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            candidate = root / "candidate"
            fixtures = root / "fixtures"
            output = root / "output"
            candidate.mkdir()
            fixtures.mkdir()
            output.mkdir()

            command = quarantine_command(
                candidate,
                fixtures,
                output,
                "sha256:" + ("0" * 64),
                ["python", "/candidate/check.py"],
            )

        rendered = " ".join(command)
        self.assertIn("--network none", rendered)
        self.assertIn("--pull never", rendered)
        self.assertIn("--read-only", command)
        self.assertIn("--cap-drop ALL", rendered)
        self.assertIn("--security-opt no-new-privileges", rendered)
        self.assertIn("--user 65532:65532", rendered)
        self.assertIn("dst=/candidate,readonly", rendered)
        self.assertIn("dst=/fixtures,readonly", rendered)
        self.assertNotIn("API_KEY", rendered)
        self.assertNotIn("TOKEN", rendered)

    def test_quarantine_command_rejects_a_short_image_digest(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            candidate = root / "candidate"
            fixtures = root / "fixtures"
            output = root / "output"
            candidate.mkdir()
            fixtures.mkdir()
            output.mkdir()

            with self.assertRaises(ValueError):
                quarantine_command(
                    candidate,
                    fixtures,
                    output,
                    "sha256:0123456789abcdef",
                    ["true"],
                )

    def test_quarantine_output_cannot_overlap_a_canonical_root(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            candidate = root / "candidate"
            fixtures = root / "fixtures"
            candidate.mkdir()
            fixtures.mkdir()

            with self.assertRaises(ValueError):
                quarantine_command(
                    candidate,
                    fixtures,
                    ROOT / "agent-plugins/shaft-skills",
                    "sha256:" + ("0" * 64),
                    ["true"],
                )

    def test_quarantine_canonical_roots_cannot_be_disabled_by_the_caller(self):
        with self.assertRaises(TypeError):
            signature(quarantine_command).bind(
                Path("candidate"),
                Path("fixtures"),
                Path("output"),
                "sha256:" + ("0" * 64),
                ["true"],
                canonical_roots=[],
            )

    def test_pr_gate_runs_candidate_intake_tests_and_validator(self):
        pr_gate = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")

        self.assertIn("tests.scripts.test_shaft_skill_candidate_intake", pr_gate)
        self.assertIn("python scripts/ci/shaft_skill_candidate_intake.py", pr_gate)


if __name__ == "__main__":
    unittest.main()
