"""The IntelliJ plugin gate survives one transient connect failure.

Issue #4494: `:verifyPlugin` resolves the JetBrains Plugin Verifier and the IDE
distributions it verifies against at dependency-resolution time, so one
`java.net.ConnectException: Connection timed out` sank PR #4491 -- a diff of 14
Python comments and two memory files that cannot influence a Gradle plugin
build. Re-running it turned it green.

What is asserted here is the wiring, because the flake itself cannot be
summoned on a runner: the verify build runs through the fresh-process retry
helper, the publish build deliberately does not, and every job that calls the
composite action leaves room for a second attempt instead of trading a red
check for a timed-out one.
"""

import unittest
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parents[2]
ACTION = REPO_ROOT / ".github/actions/intellij-verify/action.yml"
RETRY_SCRIPT = REPO_ROOT / "scripts/ci/build_retry.sh"
WORKFLOWS = REPO_ROOT / ".github/workflows"

# One failed attempt (2m38s in the run this issue was filed from) plus a pause
# plus a full successful build (5m11s in run 30940070004) does not fit in ten
# minutes. A retry under a ten-minute ceiling converts a red check into a
# timed-out check and fixes nothing.
MINIMUM_TIMEOUT_MINUTES = 20


def _run_steps() -> list[dict]:
    document = yaml.safe_load(ACTION.read_text(encoding="utf-8"))
    return [step for step in document["runs"]["steps"] if "run" in step]


class IntellijVerifyRetryTest(unittest.TestCase):
    def test_the_retry_helper_the_action_calls_exists(self):
        self.assertTrue(RETRY_SCRIPT.is_file(), f"missing: {RETRY_SCRIPT}")

    def test_the_verify_build_runs_through_the_retry_helper(self):
        verifying = [step for step in _run_steps() if "verifyPlugin" in step["run"]]
        self.assertTrue(verifying, "the action no longer runs verifyPlugin")
        for step in verifying:
            self.assertIn(
                "scripts/ci/build_retry.sh",
                step["run"],
                f"step {step.get('name')!r} runs verifyPlugin without the retry helper",
            )

    def test_publishing_is_never_retried(self):
        # A JetBrains Marketplace version number is single-use. Re-running a
        # publish that failed after the upload landed cannot succeed, and a
        # retry would bury the real reason under a second rejection.
        for step in _run_steps():
            if "publishPlugin" in step["run"]:
                self.assertNotIn(
                    "build_retry.sh",
                    step["run"],
                    f"step {step.get('name')!r} retries a Marketplace publish",
                )

    def test_every_caller_leaves_room_for_a_second_attempt(self):
        callers = []
        for path in sorted(WORKFLOWS.glob("*.yml")):
            document = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
            for name, job in (document.get("jobs") or {}).items():
                if not isinstance(job, dict):
                    continue
                steps = job.get("steps") or []
                if any("intellij-verify" in str(step.get("uses", "")) for step in steps):
                    callers.append((path.name, name, job.get("timeout-minutes")))
        self.assertTrue(callers, "no workflow calls the intellij-verify action")
        for workflow, job, timeout in callers:
            self.assertIsNotNone(timeout, f"{workflow}:{job} declares no timeout")
            self.assertGreaterEqual(
                timeout,
                MINIMUM_TIMEOUT_MINUTES,
                f"{workflow}:{job} cannot fit a retried IntelliJ build",
            )


if __name__ == "__main__":
    unittest.main()
