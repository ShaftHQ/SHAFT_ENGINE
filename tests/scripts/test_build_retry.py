"""Behaviour of scripts/ci/build_retry.sh."""
# The loop lives in a standalone script rather than inline in each workflow step
# so it can be exercised here against a fake command, on both runners the agent
# guidance gate uses. A cold-cache Maven Central 429 cannot be reproduced on
# demand (issue #4445), so the contract proven here is the control flow: what
# retries, what does not, and how many attempts each costs.
#
# Issue #4494 added the second caller: the Gradle IntelliJ plugin gate, whose
# remote fails with a connect error rather than a rate limit.

import os
import shutil
import subprocess  # nosec B404 - runs the checked-in retry script under bash.
import tempfile
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
RETRY_SCRIPT = REPO_ROOT / "scripts" / "ci" / "build_retry.sh"

# The real thing, trimmed from run 30876948089's failing job.
CENTRAL_429 = (
    "[ERROR] Non-resolvable import POM: The following artifacts could not be "
    "resolved: org.junit:junit-bom:pom:6.1.2 (absent): Could not transfer "
    "artifact org.junit:junit-bom:pom:6.1.2 from/to central "
    "(https://repo.maven.apache.org/maven2): status code: 429, reason phrase: "
    "Too Many Requests (429)"
)
GENUINE_FAILURE = (
    "[ERROR] Failed to execute goal on project shaft-visual: Compilation failure"
)
# The real thing, from the PR #4491 run this script's second caller was added
# for (issue #4494). Linux reports a connect that never opened as ConnectException
# 'Connection timed out'; a JDK socket connect timeout reports the same event as
# SocketTimeoutException 'Connect timed out'. Both are the same refusal.
GRADLE_CONNECT_REFUSED = (
    "Could not determine the dependencies of task ':verifyPlugin'. > "
    "java.net.ConnectException: Connection timed out"
)
GRADLE_CONNECT_TIMEOUT = (
    "Could not resolve org.jetbrains.intellij.plugins:verifier-cli. > "
    "java.net.SocketTimeoutException: Connect timed out"
)
GRADLE_GENUINE_FAILURE = (
    "Execution failed for task ':verifyPlugin'. > "
    "DEPRECATED_API_USAGES: 1 problem found"
)


def _bash() -> str | None:
    return shutil.which("bash")


class BuildRetryScriptTest(unittest.TestCase):
    def setUp(self):
        if _bash() is None:
            self.skipTest("bash is unavailable on this host")
        if not RETRY_SCRIPT.is_file():
            self.fail(f"retry script is missing: {RETRY_SCRIPT}")
        # ignore_cleanup_errors: on Windows the bash process can still hold the
        # directory briefly after it exits, and a cleanup race must not be
        # reported as a failure of the behaviour under test.
        self._directory = tempfile.TemporaryDirectory(ignore_cleanup_errors=True)
        self.workspace = Path(self._directory.name)
        self.addCleanup(self._directory.cleanup)
        # Everything the run touches is copied into one directory and invoked
        # relative to it. `bash` on a Windows host may be Git Bash (which reads
        # `C:/...`) or WSL (which reads `/mnt/c/...`), and only a relative path
        # from a translated working directory means the same thing to both.
        self.script = self.workspace / "retry.sh"
        self.script.write_text(
            RETRY_SCRIPT.read_text(encoding="utf-8"), encoding="utf-8", newline="\n"
        )

    def _fake_command(self, *, output: str, fail_times: int) -> str:
        # Records one line per invocation, then fails `fail_times` times before
        # succeeding, so the test can count attempts rather than infer them.
        script = self.workspace / "fake-mvn.sh"
        script.write_text(
            "#!/usr/bin/env bash\n"
            'echo "attempt" >> attempts.log\n'
            "count=$(wc -l < attempts.log)\n"
            f'if [ "$count" -le {fail_times} ]; then\n'
            f'  echo "{output}"\n'
            "  exit 1\n"
            "fi\n"
            'echo "BUILD SUCCESS"\n',
            encoding="utf-8",
            newline="\n",
        )
        os.chmod(script, 0o755)  # nosec B103 - test fixture the test itself runs.
        return "./fake-mvn.sh"

    def _run(self, command: str, *, attempts: int = 3, pause: int = 0):
        completed = subprocess.run(  # nosec B603 - fixed bash invocation in a temp dir.
            [_bash(), "./retry.sh", str(attempts), str(pause), "bash", command],
            cwd=self.workspace,
            capture_output=True,
            text=True,
        )
        log = self.workspace / "attempts.log"
        made = len(log.read_text(encoding="utf-8").splitlines()) if log.is_file() else 0
        return completed, made

    def test_a_command_that_succeeds_runs_once(self):
        command = self._fake_command(output="", fail_times=0)
        completed, attempts = self._run(command)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(1, attempts)

    def test_a_central_429_is_retried_in_a_fresh_process_until_it_succeeds(self):
        command = self._fake_command(output=CENTRAL_429, fail_times=2)
        completed, attempts = self._run(command, attempts=3)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(3, attempts)

    def test_a_gradle_connect_refusal_is_retried_until_it_succeeds(self):
        command = self._fake_command(output=GRADLE_CONNECT_REFUSED, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_gradle_connect_timeout_is_retried_until_it_succeeds(self):
        command = self._fake_command(output=GRADLE_CONNECT_TIMEOUT, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_genuine_verifier_finding_is_not_retried(self):
        # A deprecated-API usage is the gate doing its job. Retrying it costs
        # another full IDE verification to reach the same verdict.
        command = self._fake_command(output=GRADLE_GENUINE_FAILURE, fail_times=99)
        completed, attempts = self._run(command, attempts=3)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(1, attempts)

    def test_a_genuine_build_failure_is_not_retried(self):
        # Retrying a compilation error would burn the job's timeout to reach the
        # same failure, and would hide it behind two more minutes of noise.
        command = self._fake_command(output=GENUINE_FAILURE, fail_times=99)
        completed, attempts = self._run(command, attempts=3)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(1, attempts)

    def test_a_persistent_429_exhausts_the_attempts_and_fails(self):
        command = self._fake_command(output=CENTRAL_429, fail_times=99)
        completed, attempts = self._run(command, attempts=2)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(2, attempts)

    def test_the_failing_output_is_still_visible_to_the_job_log(self):
        command = self._fake_command(output=CENTRAL_429, fail_times=99)
        completed, _ = self._run(command, attempts=2)
        self.assertIn("429", completed.stdout + completed.stderr)


if __name__ == "__main__":
    unittest.main()
