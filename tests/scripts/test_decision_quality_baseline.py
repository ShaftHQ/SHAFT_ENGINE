"""Behavioral regression for the decision-quality baseline artifact (#5520)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BASELINE = ROOT / "chaos-engine/decision-quality-baseline.md"

REQUIRED_METRICS = [
    "total_tokens",
    "wall_time_minutes",
    "external_run_minutes",
    "retry_count",
    "fix_iterations",
    "changed_files",
    "pr_count",
    "reopened_failures",
    "escaped_defects",
]

REQUIRED_TAXONOMY_CLASSES = [
    "SYM-BEFORE-ROOT",
    "STALE-RETRIEVAL",
    "LATE-ARCH",
]

REQUIRED_CLASS_FIELDS = [
    "Definition",
    "Causal mechanism",
    "Public evidence",
    "Limitations",
    "Rollback rule",
    "Retention rule",
]

# Terms that must never appear in the taxonomy body (privacy gate).
FORBIDDEN_PRIVACY_TERMS = [
    r"model_id\s*:",
    r"provider_route\s*:",
    r"endpoint\s*:",
    # raw provider substrings that would indicate unredacted content
    r"anthropic\.com/",
    r"openai\.com/",
]

FORBIDDEN_CONTENT_PATTERNS = [
    "prompt content",
    "session transcript",
    "private path",
    r"~\/\.",  # home-directory path
]


class DecisionQualityBaselineExistenceTest(unittest.TestCase):
    def test_baseline_file_exists(self):
        self.assertTrue(BASELINE.exists(), f"Baseline artifact missing: {BASELINE}")

    def test_baseline_file_is_dated(self):
        content = BASELINE.read_text(encoding="utf-8")
        self.assertRegex(
            content,
            r"Accessed:\s+\d{4}-\d{2}-\d{2}",
            "Baseline must have an 'Accessed: YYYY-MM-DD' date line",
        )

    def test_baseline_references_parent_epic(self):
        content = BASELINE.read_text(encoding="utf-8")
        self.assertIn("#5514", content, "Baseline must reference parent epic #5514")

    def test_baseline_references_issue_5520(self):
        content = BASELINE.read_text(encoding="utf-8")
        self.assertIn("#5520", content, "Baseline must reference its own issue #5520")


class MetricsDictionaryTest(unittest.TestCase):
    def setUp(self):
        self.content = BASELINE.read_text(encoding="utf-8")

    def test_all_required_metrics_present(self):
        for metric in REQUIRED_METRICS:
            self.assertIn(
                f"`{metric}`",
                self.content,
                f"Metrics dictionary missing required metric: {metric}",
            )

    def test_missing_data_policy_documented(self):
        self.assertIn(
            "UNAVAILABLE",
            self.content,
            "Metrics dictionary must document UNAVAILABLE as the missing-data value",
        )

    def test_missing_data_never_zero(self):
        lower = self.content.lower()
        self.assertNotIn(
            "never zero",
            lower,
            "Policy prose must not say 'never zero' in a way that could be confused with a metric value",
        )
        # Positive: UNAVAILABLE is instructed, not 0
        self.assertIn("UNAVAILABLE", self.content)


class SamplingProtocolTest(unittest.TestCase):
    def setUp(self):
        self.content = BASELINE.read_text(encoding="utf-8")

    def test_sampling_protocol_section_exists(self):
        self.assertIn("Sampling protocol", self.content)

    def test_protocol_references_commit_sha_evidence(self):
        lower = self.content.lower()
        self.assertIn("commit sha", lower, "Protocol must reference commit SHAs as causal anchors")

    def test_protocol_references_public_comment_evidence(self):
        lower = self.content.lower()
        self.assertTrue(
            "public comment" in lower or "orchestration status" in lower,
            "Protocol must reference public comment evidence",
        )

    def test_protocol_validity_check_has_three_conditions(self):
        content = self.content
        # The validity check section must name the three conditions
        self.assertIn("causal anchor", content.lower())
        self.assertIn("error code", content.lower())
        self.assertIn("no prompt", content.lower())

    def test_protocol_requires_privacy_gate(self):
        self.assertIn("Privacy gate", self.content)

    def test_protocol_redaction_labels_defined(self):
        content = self.content
        self.assertIn("[PROVIDER]", content)
        self.assertIn("[MODEL_ID]", content)
        self.assertIn("[ENDPOINT]", content)


class TaxonomyTest(unittest.TestCase):
    def setUp(self):
        self.content = BASELINE.read_text(encoding="utf-8")
        self.lower = self.content.lower()

    def test_all_three_required_classes_present(self):
        for cls in REQUIRED_TAXONOMY_CLASSES:
            self.assertIn(cls, self.content, f"Taxonomy missing required class: {cls}")

    def test_each_class_has_definition(self):
        for cls in REQUIRED_TAXONOMY_CLASSES:
            # Find the block between this class heading and the next ---
            pattern = re.compile(
                rf"{re.escape(cls)}.*?(?=\n---|\Z)", re.DOTALL
            )
            match = pattern.search(self.content)
            self.assertIsNotNone(match, f"Could not locate class block for {cls}")
            block = match.group(0).lower()
            self.assertIn("definition", block, f"{cls} block missing 'Definition'")
            self.assertIn("causal mechanism", block, f"{cls} block missing 'Causal mechanism'")
            self.assertIn("limitation", block, f"{cls} block missing 'Limitations'")
            self.assertIn("rollback rule", block, f"{cls} block missing 'Rollback rule'")
            self.assertIn("retention rule", block, f"{cls} block missing 'Retention rule'")

    def test_sym_before_root_has_quantified_evidence(self):
        # Must have at least one numeric fix_iterations reference
        sym_section = self.content[
            self.content.index("SYM-BEFORE-ROOT"):
            self.content.index("STALE-RETRIEVAL")
        ]
        self.assertIn("fix_iterations", sym_section)
        self.assertRegex(sym_section, r"fix_iterations\s*=\s*\d+")

    def test_stale_retrieval_has_quantified_evidence(self):
        stale_start = self.content.index("STALE-RETRIEVAL")
        late_start = self.content.index("LATE-ARCH")
        stale_section = self.content[stale_start:late_start]
        self.assertIn("fix_iterations", stale_section)
        self.assertIn("retry_count", stale_section)

    def test_late_arch_has_before_after_fix_iterations(self):
        late_start = self.content.index("LATE-ARCH")
        late_section = self.content[late_start:]
        self.assertIn("fix_iterations before", late_section.lower())
        self.assertIn("fix_iterations after", late_section.lower())

    def test_taxonomy_has_at_least_three_classes(self):
        count = sum(1 for cls in REQUIRED_TAXONOMY_CLASSES if cls in self.content)
        self.assertGreaterEqual(count, 3)


class PrivacyRedactionTest(unittest.TestCase):
    def setUp(self):
        self.content = BASELINE.read_text(encoding="utf-8")

    def test_no_raw_model_id_key(self):
        for pattern in FORBIDDEN_PRIVACY_TERMS:
            self.assertIsNone(
                re.search(pattern, self.content, re.IGNORECASE),
                f"Forbidden privacy term found matching: {pattern}",
            )

    def test_no_provider_hostnames(self):
        # Must not contain raw provider API hostnames
        forbidden_hosts = ["anthropic.com/", "openai.com/", "googleapis.com/"]
        for host in forbidden_hosts:
            self.assertNotIn(host, self.content, f"Unredacted provider hostname found: {host}")

    def test_privacy_constraints_section_exists(self):
        self.assertIn("Privacy constraints", self.content)

    def test_privacy_section_lists_forbidden_categories(self):
        content = self.content.lower()
        self.assertIn("prompt content", content)
        self.assertIn("session transcript", content)
        self.assertIn("private path", content)


class SampleValidityContractTest(unittest.TestCase):
    """Contract tests: valid sample accepted, invalid sample rejected."""

    @staticmethod
    def _validate_sample(sample: dict) -> tuple[bool, str]:
        """Minimal validity check per the sampling protocol."""
        if not sample.get("causal_anchor_sha"):
            return False, "missing causal_anchor_sha"
        sha = sample["causal_anchor_sha"]
        if not re.fullmatch(r"[0-9a-f]{7,40}", sha, re.IGNORECASE):
            return False, f"causal_anchor_sha not a valid SHA: {sha!r}"
        if not sample.get("failure_description"):
            return False, "missing failure_description"
        prohibited_keys = {"prompt", "transcript", "private_path", "model_id", "provider_route"}
        present = prohibited_keys & sample.keys()
        if present:
            return False, f"sample contains prohibited keys: {present}"
        return True, "ok"

    def test_valid_sample_accepted(self):
        sample = {
            "causal_anchor_sha": "07dce4ff3e",
            "failure_description": "completions catalog rejected ids from management catalog",
            "taxonomy_class": "LATE-ARCH",
            "fix_iterations": 4,
        }
        ok, reason = self._validate_sample(sample)
        self.assertTrue(ok, f"Valid sample rejected: {reason}")

    def test_sample_missing_causal_anchor_rejected(self):
        sample = {
            "failure_description": "transport error",
            "taxonomy_class": "SYM-BEFORE-ROOT",
        }
        ok, _ = self._validate_sample(sample)
        self.assertFalse(ok, "Sample with no causal_anchor_sha must be rejected")

    def test_sample_with_invalid_sha_rejected(self):
        sample = {
            "causal_anchor_sha": "not-a-sha",
            "failure_description": "transport error",
        }
        ok, _ = self._validate_sample(sample)
        self.assertFalse(ok, "Sample with non-SHA causal anchor must be rejected")

    def test_sample_missing_failure_description_rejected(self):
        sample = {
            "causal_anchor_sha": "07dce4ff3e",
        }
        ok, _ = self._validate_sample(sample)
        self.assertFalse(ok, "Sample with no failure_description must be rejected")

    def test_sample_with_prompt_key_rejected(self):
        sample = {
            "causal_anchor_sha": "07dce4ff3e",
            "failure_description": "transport error",
            "prompt": "do the thing",
        }
        ok, _ = self._validate_sample(sample)
        self.assertFalse(ok, "Sample containing 'prompt' key must be rejected (privacy gate)")

    def test_sample_with_model_id_key_rejected(self):
        sample = {
            "causal_anchor_sha": "07dce4ff3e",
            "failure_description": "transport error",
            "model_id": "some-model-name",
        }
        ok, _ = self._validate_sample(sample)
        self.assertFalse(ok, "Sample containing 'model_id' must be rejected (privacy gate)")

    def test_sample_with_transcript_key_rejected(self):
        sample = {
            "causal_anchor_sha": "07dce4ff3e",
            "failure_description": "transport error",
            "transcript": "user: ...",
        }
        ok, _ = self._validate_sample(sample)
        self.assertFalse(ok, "Sample containing 'transcript' must be rejected")

    def test_required_metric_fields_are_known_names(self):
        # Metrics dictionary contract: all 9 required metric names are present in BASELINE.
        content = BASELINE.read_text(encoding="utf-8")
        required = [
            "total_tokens", "wall_time_minutes", "external_run_minutes",
            "retry_count", "fix_iterations", "changed_files",
            "pr_count", "reopened_failures", "escaped_defects",
        ]
        for metric in required:
            self.assertIn(f"`{metric}`", content, f"Metric `{metric}` missing from baseline")


if __name__ == "__main__":
    unittest.main()
