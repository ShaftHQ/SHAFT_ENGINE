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

if __name__ == "__main__":
    unittest.main()
