import subprocess
import unittest.mock
from pathlib import Path

import scripts.ci.verify_shaft_mcp_installer_release as verify

run_installer = verify.run_installer


class RunInstallerRetryTest(unittest.TestCase):
    def test_run_installer_retries_transient_failure_then_succeeds(self):
        error = subprocess.CalledProcessError(4, ["install"], output="", stderr="HTTP 429")
        success = subprocess.CompletedProcess(["install"], 0)
        with unittest.mock.patch.object(verify.subprocess, "run", side_effect=[error, success]) as mocked_run, \
                unittest.mock.patch.object(verify.time, "sleep") as mocked_sleep:
            result = run_installer(
                ["install"], cwd=Path("."), environment={}, attempts=3, retry_delay_seconds=1.0
            )

        self.assertIs(result, success)
        self.assertEqual(mocked_run.call_count, 2)
        mocked_sleep.assert_called_once_with(1.0)

    def test_run_installer_raises_after_exhausting_attempts(self):
        error = subprocess.CalledProcessError(4, ["install"], output="out", stderr="err")
        with unittest.mock.patch.object(verify.subprocess, "run", side_effect=[error, error, error]) as mocked_run, \
                unittest.mock.patch.object(verify.time, "sleep") as mocked_sleep:
            with self.assertRaisesRegex(RuntimeError, "failed after 3 attempts"):
                run_installer(
                    ["install"], cwd=Path("."), environment={}, attempts=3, retry_delay_seconds=1.0
                )

        self.assertEqual(mocked_run.call_count, 3)
        self.assertEqual(mocked_sleep.call_count, 2)

    def test_run_installer_rejects_non_positive_attempts(self):
        with self.assertRaisesRegex(ValueError, "attempts must be at least 1"):
            run_installer(["install"], cwd=Path("."), environment={}, attempts=0)


class ShaftCliPublishedTest(unittest.TestCase):
    def test_metadata_present_means_published(self):
        response = unittest.mock.MagicMock()
        with unittest.mock.patch.object(verify.urllib.request, "urlopen", return_value=response):
            self.assertTrue(verify.shaft_cli_published())
        response.__enter__.assert_called_once()

    def test_missing_metadata_means_unpublished(self):
        error = verify.urllib.error.HTTPError(
            verify.SHAFT_CLI_METADATA_URL, 404, "Not Found", None, None
        )
        with unittest.mock.patch.object(verify.urllib.request, "urlopen", side_effect=error):
            self.assertFalse(verify.shaft_cli_published())

    def test_other_http_errors_propagate(self):
        error = verify.urllib.error.HTTPError(
            verify.SHAFT_CLI_METADATA_URL, 503, "Unavailable", None, None
        )
        with unittest.mock.patch.object(verify.urllib.request, "urlopen", side_effect=error):
            with self.assertRaises(verify.urllib.error.HTTPError):
                verify.shaft_cli_published()


if __name__ == "__main__":
    unittest.main()
