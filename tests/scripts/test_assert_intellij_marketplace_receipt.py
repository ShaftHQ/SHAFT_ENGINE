import importlib.util
import inspect
import io
import tempfile
import unittest
from contextlib import redirect_stderr
from pathlib import Path
from urllib.error import URLError

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/assert_intellij_marketplace_receipt.py"
SPEC = importlib.util.spec_from_file_location("assert_intellij_marketplace_receipt", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)

SUCCESS_LOG = "> Task :signPlugin\n> Task :publishPlugin\nBUILD SUCCESSFUL in 1m 2s\n"
PROPERTIES = "pluginVersion=10.3.20260818\npluginSinceBuild=243\n"
LISTED = '[{"version":"10.3.20260818"},{"version":"10.3.20260817"}]'
MISSING = '[{"version":"10.3.20260817"}]'


class IntellijMarketplaceReceiptTest(unittest.TestCase):
    def test_successful_log_and_listed_version_pass(self):
        self.assertEqual(
            [],
            MODULE.receipt_errors(SUCCESS_LOG, PROPERTIES, LISTED),
        )

    def test_missing_build_successful_fails(self):
        errors = MODULE.receipt_errors(
            "> Task :publishPlugin\nBUILD FAILED in 12s\n",
            PROPERTIES,
            LISTED,
        )
        self.assertTrue(any("BUILD SUCCESSFUL" in error for error in errors), errors)

    def test_marketplace_without_the_version_is_advisory_after_gradle_success(self):
        errors = MODULE.receipt_errors(SUCCESS_LOG, PROPERTIES, MISSING)
        self.assertEqual([], errors)

    def test_empty_log_fails_even_if_marketplace_lists_the_version(self):
        errors = MODULE.receipt_errors("", PROPERTIES, LISTED)
        self.assertTrue(any("BUILD SUCCESSFUL" in error for error in errors), errors)

    def test_missing_plugin_version_fails(self):
        errors = MODULE.receipt_errors(SUCCESS_LOG, "pluginSinceBuild=243\n", LISTED)
        self.assertTrue(any("pluginVersion" in error for error in errors), errors)

    def test_unreadable_updates_json_fails(self):
        errors = MODULE.receipt_errors(SUCCESS_LOG, PROPERTIES, "not-json")
        self.assertTrue(any("unreadable" in error for error in errors), errors)

    def test_cli_accepts_fixture_updates_json(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            log = root / "publish.log"
            properties = root / "gradle.properties"
            updates = root / "updates.json"
            log.write_text(SUCCESS_LOG, encoding="utf-8")
            properties.write_text(PROPERTIES, encoding="utf-8")
            updates.write_text(LISTED, encoding="utf-8")
            self.assertEqual(
                0,
                MODULE.main(
                    [
                        "--log",
                        str(log),
                        "--properties",
                        str(properties),
                        "--updates-json",
                        str(updates),
                    ]
                ),
            )

    def test_cli_live_fetch_passes_plugin_version_into_retry(self):
        seen: dict[str, str | None] = {}
        original = MODULE.fetch_updates_with_retry

        def wrapper(*args, **kwargs):
            seen["version"] = kwargs.get("version")
            return LISTED

        MODULE.fetch_updates_with_retry = wrapper
        try:
            with tempfile.TemporaryDirectory() as temp:
                root = Path(temp)
                log = root / "publish.log"
                properties = root / "gradle.properties"
                log.write_text(SUCCESS_LOG, encoding="utf-8")
                properties.write_text(PROPERTIES, encoding="utf-8")
                self.assertEqual(
                    0,
                    MODULE.main(
                        [
                            "--log",
                            str(log),
                            "--properties",
                            str(properties),
                        ]
                    ),
                )
        finally:
            MODULE.fetch_updates_with_retry = original
        self.assertEqual("10.3.20260818", seen.get("version"))

    def test_cli_accepts_upload_when_marketplace_listing_lags(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            log = root / "publish.log"
            properties = root / "gradle.properties"
            updates = root / "updates.json"
            log.write_text(SUCCESS_LOG, encoding="utf-8")
            properties.write_text(PROPERTIES, encoding="utf-8")
            updates.write_text(MISSING, encoding="utf-8")
            stderr = io.StringIO()
            with redirect_stderr(stderr):
                result = MODULE.main(
                    [
                        "--log",
                        str(log),
                        "--properties",
                        str(properties),
                        "--updates-json",
                        str(updates),
                    ]
                )
            self.assertEqual(0, result)
            self.assertIn("listing is still pending", stderr.getvalue())
            self.assertIn("Do not re-dispatch", stderr.getvalue())

    def test_fetch_retries_then_raises(self):
        attempts = {"n": 0}

        def fetcher(_url: str) -> str:
            attempts["n"] += 1
            raise URLError("timed out")

        sleeps: list[float] = []
        with self.assertRaises(RuntimeError):
            MODULE.fetch_updates_with_retry(
                attempts=3,
                delay_seconds=0.01,
                sleeper=sleeps.append,
                fetcher=fetcher,
            )
        self.assertEqual(3, attempts["n"])
        self.assertEqual([0.01, 0.01], sleeps)

    def test_fetch_retries_when_version_is_absent_then_listed(self):
        payloads = [MISSING, LISTED]
        attempts = {"n": 0}

        def fetcher(_url: str) -> str:
            attempts["n"] += 1
            return payloads[attempts["n"] - 1]

        sleeps: list[float] = []
        body = MODULE.fetch_updates_with_retry(
            attempts=5,
            delay_seconds=0.01,
            sleeper=sleeps.append,
            fetcher=fetcher,
            version="10.3.20260818",
        )
        self.assertEqual(LISTED, body)
        self.assertEqual(2, attempts["n"])
        self.assertEqual([0.01], sleeps)

    def test_fetch_fails_closed_when_version_stays_absent(self):
        attempts = {"n": 0}

        def fetcher(_url: str) -> str:
            attempts["n"] += 1
            return MISSING

        sleeps: list[float] = []
        body = MODULE.fetch_updates_with_retry(
            attempts=3,
            delay_seconds=0.01,
            sleeper=sleeps.append,
            fetcher=fetcher,
            version="10.3.20260818",
        )
        self.assertEqual(MISSING, body)
        self.assertEqual(3, attempts["n"])
        self.assertEqual([0.01, 0.01], sleeps)
        errors = MODULE.marketplace_receipt_errors(body, "10.3.20260818")
        self.assertTrue(any("10.3.20260818" in error for error in errors), errors)

    def test_default_visibility_probe_does_not_poll_after_accepted_upload(self):
        parameters = inspect.signature(MODULE.fetch_updates_with_retry).parameters
        attempts = parameters["attempts"].default
        delay_seconds = parameters["delay_seconds"].default
        wait_seconds = (attempts - 1) * delay_seconds
        self.assertEqual(
            0,
            wait_seconds,
            f"accepted upload still waits {wait_seconds}s for an eventually consistent listing",
        )

    def test_gradle_success_with_omitted_version_is_not_a_receipt_error(self):
        errors = MODULE.receipt_errors(SUCCESS_LOG, PROPERTIES, MISSING)
        self.assertEqual([], errors)

    def test_missing_gradle_receipt_does_not_claim_listing_lag(self):
        errors = MODULE.receipt_errors("", PROPERTIES, MISSING)
        blob = "\n".join(errors)
        self.assertTrue(any("BUILD SUCCESSFUL" in error for error in errors), errors)
        self.assertNotIn("listing still lagging", blob)
        self.assertNotIn("Do not re-dispatch", blob)

    def test_cli_skips_marketplace_poll_when_gradle_receipt_missing(self):
        calls = {"n": 0}
        original = MODULE.fetch_updates_with_retry

        def wrapper(*args, **kwargs):
            calls["n"] += 1
            return LISTED

        MODULE.fetch_updates_with_retry = wrapper
        try:
            with tempfile.TemporaryDirectory() as temp:
                root = Path(temp)
                log = root / "publish.log"
                properties = root / "gradle.properties"
                log.write_text("> Task :publishPlugin\nBUILD FAILED in 12s\n", encoding="utf-8")
                properties.write_text(PROPERTIES, encoding="utf-8")
                self.assertEqual(
                    1,
                    MODULE.main(
                        [
                            "--log",
                            str(log),
                            "--properties",
                            str(properties),
                        ]
                    ),
                )
        finally:
            MODULE.fetch_updates_with_retry = original
        self.assertEqual(0, calls["n"])

    def test_cli_retries_listing_when_gradle_succeeded_and_version_is_omitted(self):
        calls = {"n": 0}
        original = MODULE.fetch_updates_with_retry

        def wrapper(*args, **kwargs):
            calls["n"] += 1
            return MISSING

        MODULE.fetch_updates_with_retry = wrapper
        try:
            with tempfile.TemporaryDirectory() as temp:
                root = Path(temp)
                log = root / "publish.log"
                properties = root / "gradle.properties"
                log.write_text(SUCCESS_LOG, encoding="utf-8")
                properties.write_text(PROPERTIES, encoding="utf-8")
                self.assertEqual(
                    1,
                    MODULE.main(
                        [
                            "--log",
                            str(log),
                            "--properties",
                            str(properties),
                        ]
                    ),
                )
        finally:
            MODULE.fetch_updates_with_retry = original
        self.assertEqual(1, calls["n"])


if __name__ == "__main__":
    unittest.main()
