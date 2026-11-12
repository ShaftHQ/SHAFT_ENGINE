"""Codecov upload receipt must name the requested flag (#7924)."""

from __future__ import annotations

import importlib.util
import re
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ACTION = ROOT / ".github/actions/upload-jacoco-coverage/action.yml"
PR_GATE = ROOT / ".github/workflows/pr-gate.yml"
_STEP = re.compile(r"^(\s*)- name: (.*)$")
_REJECT_STEP = "Reject a queued Codecov upload whose receipt omits the flag"
_SPEC = importlib.util.spec_from_file_location(
    "upload_jacoco_coverage",
    ROOT / "scripts/ci/upload_jacoco_coverage.py",
)
assert _SPEC is not None and _SPEC.loader is not None
_RECEIPT = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_RECEIPT)
receipt_errors = _RECEIPT.receipt_errors
record_codecov_receipt = _RECEIPT.record_codecov_receipt


def _named_blocks(text: str) -> list[tuple[str, str]]:
    lines = text.splitlines()
    found: list[tuple[int, int, str]] = []
    for index, line in enumerate(lines):
        match = _STEP.match(line)
        if match:
            found.append((index, len(match.group(1)), match.group(2).strip()))
    blocks: list[tuple[str, str]] = []
    for index, indent, name in found:
        end = len(lines)
        for later in range(index + 1, len(lines)):
            line = lines[later]
            if not line.strip():
                continue
            leading = len(line) - len(line.lstrip(" "))
            if leading <= indent:
                end = later
                break
        blocks.append((name, "\n".join(lines[index:end])))
    return blocks


def _action_output(action: str, key: str) -> str:
    lines = action.splitlines()
    try:
        start = lines.index("outputs:")
    except ValueError as exc:
        raise AssertionError("action has no outputs") from exc
    end = len(lines)
    for index in range(start + 1, len(lines)):
        if lines[index] and not lines[index].startswith(" "):
            end = index
            break
    body = lines[start + 1 : end]
    prefix = f"  {key}:"
    for index, line in enumerate(body):
        if line.split(":", 1)[0] != f"  {key}":
            continue
        chunk = [line]
        for later in body[index + 1 :]:
            if re.match(r"^  \S", later):
                break
            chunk.append(later)
        return "\n".join(chunk)
    raise AssertionError(f"action output {key} missing; looked for {prefix}")


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
        # --receipt is a substring of --record-receipt. The summarize gate is
        # the call that turns a missing flag into failed_phases+=(receipt).
        self.assertIn(
            'scripts/ci/upload_jacoco_coverage.py --flag "$REQUESTED_FLAG" '
            '--receipt "${CODECOV_RECEIPT:-}"',
            action,
        )
        self.assertIn("failed_phases+=(receipt)", action)
        self.assertIn('CC_FAIL_ON_ERROR=true bash "$script"', action)
        pin = "303a32d7a59b442fa8d48b6a1cc6825c09c847a5"
        self.assertGreaterEqual(action.count(pin), 2)
        self.assertIn(
            "flags: ${{ inputs.flags || (github.event_name != 'pull_request' && 'main-suites' || '') }}",
            action,
        )

    def test_unit_job_rejects_a_queued_receipt_that_omits_the_flag(self) -> None:
        action = ACTION.read_text(encoding="utf-8")
        workflow = PR_GATE.read_text(encoding="utf-8")
        receipt = _action_output(action, "receipt-status")
        self.assertIn("steps.summarize.outputs.receipt-status", receipt)
        summarize = dict(_named_blocks(action))["Summarize coverage result"]
        self.assertIn('echo "receipt-status=missing-flag" >> "$GITHUB_OUTPUT"', summarize)
        self.assertIn('echo "receipt-status=ok" >> "$GITHUB_OUTPUT"', summarize)
        self.assertIn('echo "receipt-status=unchecked" >> "$GITHUB_OUTPUT"', summarize)
        strict = dict(_named_blocks(action))["Enforce optional strict mode"]
        self.assertIn("inputs.fail-ci-if-error == 'true'", strict)

        steps = _named_blocks(workflow)
        uploads = [
            index
            for index, (name, body) in enumerate(steps)
            if name == "Upload module coverage to Codecov"
            and "flags: ${{ matrix.leg || matrix.module }}" in body
        ]
        self.assertEqual(1, len(uploads))
        upload = steps[uploads[0]][1]
        self.assertIn("\n        id: codecov\n", f"\n{upload}\n")
        self.assertIn("continue-on-error: true", upload)
        self.assertLess(uploads[0] + 1, len(steps))
        reject_name, reject = steps[uploads[0] + 1]
        self.assertEqual(_REJECT_STEP, reject_name)
        self.assertNotIn("continue-on-error", reject)
        self.assertIn(
            "if: steps.codecov.outputs.receipt-status == 'missing-flag'",
            reject,
        )
        self.assertIn("exit 1", reject)
        self.assertEqual(1, workflow.count(_REJECT_STEP))


if __name__ == "__main__":
    unittest.main()
