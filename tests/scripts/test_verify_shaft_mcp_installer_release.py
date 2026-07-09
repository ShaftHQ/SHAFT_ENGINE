import subprocess
import unittest
from pathlib import Path
from unittest import mock

import scripts.ci.verify_shaft_mcp_installer_release as verify

run_installer = verify.run_installer


class RunInstallerRetryTest(unittest.TestCase):
    def test_run_installer_retries_transient_failure_then_succeeds(self):
        error = subprocess.CalledProcessError(4, ["install"], output="", stderr="HTTP 429")
        success = subprocess.CompletedProcess(["install"], 0)
        with mock.patch.object(verify.subprocess, "run", side_effect=[error, success]) as mocked_run, \
                mock.patch.object(verify.time, "sleep") as mocked_sleep:
            result = run_installer(
                ["install"], cwd=Path("."), environment={}, attempts=3, retry_delay_seconds=1.0
            )

        self.assertIs(result, success)
        self.assertEqual(mocked_run.call_count, 2)
        mocked_sleep.assert_called_once_with(1.0)

    def test_run_installer_raises_after_exhausting_attempts(self):
        error = subprocess.CalledProcessError(4, ["install"], output="out", stderr="err")
        with mock.patch.object(verify.subprocess, "run", side_effect=[error, error, error]) as mocked_run, \
                mock.patch.object(verify.time, "sleep") as mocked_sleep:
            with self.assertRaisesRegex(RuntimeError, "failed after 3 attempts"):
                run_installer(
                    ["install"], cwd=Path("."), environment={}, attempts=3, retry_delay_seconds=1.0
                )

        self.assertEqual(mocked_run.call_count, 3)
        self.assertEqual(mocked_sleep.call_count, 2)


if __name__ == "__main__":
    unittest.main()
