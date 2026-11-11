"""Codecov upload receipt must name the requested flag (#7924)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ACTION = ROOT / ".github/actions/upload-jacoco-coverage/action.yml"
_SPEC = importlib.util.spec_from_file_location(
    "upload_jacoco_coverage",
    ROOT / "scripts/ci/upload_jacoco_coverage.py",
)
assert _SPEC is not None and _SPEC.loader is not None
_RECEIPT = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_RECEIPT)
receipt_errors = _RECEIPT.receipt_errors
record_codecov_receipt = _RECEIPT.record_codecov_receipt

# Shard-2 on run 42912520949 queued this command and HTTP 200. The PR comment
# still omitted the flag; that processing gap is upstream. This receipt is valid.
HISTORICAL_SHARD_2_RECEIPT = (
    "./codecov --verbose upload-coverage -t <redacted> --fail-on-error "
    "--git-service github --parent-sha 57863b7d7c67081a1af0a61b7809ca63dab0a302 "
    "--sha ab488fbf7acd7e8d09254ea24f19870d2017894a --disable-search "
    "--file ./shaft-engine/target/jacoco/jacoco.xml --flag shaft-engine-shard-2 "
    "--gcov-executable gcov"
)


class UploadJacocoCoverageReceiptTest(unittest.TestCase):
    def test_historical_shard_2_receipt_names_the_requested_flag(self) -> None:
        self.assertEqual(
            [],
            receipt_errors(HISTORICAL_SHARD_2_RECEIPT, "shaft-engine-shard-2"),
        )

    def test_success_line_without_the_requested_flag_is_a_failed_upload(self) -> None:
        receipt = (
            "Upload queued for processing complete\n"
            '{"result": "RequestResult(error=None, warnings=[], status_code=200, text=\'\')"}\n'
            "--flag shaft-engine-shard-1\n"
        )
        errors = receipt_errors(receipt, "shaft-engine-shard-2")
        self.assertTrue(errors)
        self.assertIn("shaft-engine-shard-2", errors[0])

    def test_empty_receipt_is_a_failed_upload_when_a_flag_was_requested(self) -> None:
        self.assertTrue(receipt_errors("", "shaft-engine-shard-2"))
        self.assertTrue(receipt_errors("   ", "main-suites"))

    def test_no_requested_flag_does_not_fail_an_unflagged_receipt(self) -> None:
        self.assertEqual([], receipt_errors("Upload queued for processing complete", ""))
        self.assertEqual([], receipt_errors("", "  "))

    def test_flag_token_must_match_exactly(self) -> None:
        self.assertTrue(receipt_errors("--flag shaft-engine-shard-20", "shaft-engine-shard-2"))
        self.assertTrue(receipt_errors("--flag shaft-engine-shard", "shaft-engine-shard-2"))
        self.assertEqual([], receipt_errors("--flag=shaft-engine-shard-2", "shaft-engine-shard-2"))

    def test_recorded_receipt_hides_the_token_and_names_the_flag(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "receipt.txt"
            record_codecov_receipt(
                [
                    "--verbose",
                    "upload-coverage",
                    "-t",
                    "secret-token-value",
                    "--token=other-secret",
                    "--flag",
                    "shaft-engine-shard-2",
                    "--file",
                    "./shaft-engine/target/jacoco/jacoco.xml",
                ],
                path,
            )
            text = path.read_text(encoding="utf-8")
        self.assertNotIn("secret-token-value", text)
        self.assertNotIn("other-secret", text)
        self.assertIn("<redacted>", text)
        self.assertEqual([], receipt_errors(text, "shaft-engine-shard-2"))

    def test_action_checks_the_shipped_receipt_script(self) -> None:
        action = ACTION.read_text(encoding="utf-8")
        self.assertIn("scripts/ci/upload_jacoco_coverage.py", action)
        self.assertIn("--record-receipt", action)
        self.assertIn("--receipt", action)
        pin = "303a32d7a59b442fa8d48b6a1cc6825c09c847a5"
        self.assertGreaterEqual(action.count(pin), 2)
        self.assertIn(
            "flags: ${{ inputs.flags || (github.event_name != 'pull_request' && 'main-suites' || '') }}",
            action,
        )


if __name__ == "__main__":
    unittest.main()
