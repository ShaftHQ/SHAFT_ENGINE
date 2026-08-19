import importlib.util
import inspect
import tempfile
import unittest
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

    def test_marketplace_without_the_version_fails(self):
        errors = MODULE.receipt_errors(SUCCESS_LOG, PROPERTIES, MISSING)
        self.assertTrue(
            any("10.3.20260818" in error for error in errors),
            errors,
        )

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

    def test_cli_fails_when_marketplace_omits_the_version(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            log = root / "publish.log"
            properties = root / "gradle.properties"
            updates = root / "updates.json"
            log.write_text(SUCCESS_LOG, encoding="utf-8")
            properties.write_text(PROPERTIES, encoding="utf-8")
            updates.write_text(MISSING, encoding="utf-8")
            self.assertEqual(
                1,
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

    def test_default_retry_budget_covers_observed_marketplace_listing_lag(self):
        # Run 32247439124: BUILD SUCCESSFUL at 11:35:30Z, receipt miss at 11:36:32Z (~62s).
        parameters = inspect.signature(MODULE.fetch_updates_with_retry).parameters
        attempts = parameters["attempts"].default
        delay_seconds = parameters["delay_seconds"].default
        wait_seconds = (attempts - 1) * delay_seconds
        self.assertGreaterEqual(
            wait_seconds,
            90,
            f"default listing-lag wait is {wait_seconds}s; observed miss was ~62s",
        )

    def test_gradle_success_with_omitted_version_names_listing_lag(self):
        errors = MODULE.receipt_errors(SUCCESS_LOG, PROPERTIES, MISSING)
        blob = "\n".join(errors)
        self.assertTrue(any("10.3.20260818" in error for error in errors), errors)
        self.assertIn("listing still lagging", blob)
        self.assertIn("Do not re-dispatch", blob)
        self.assertNotIn("Gradle publish log is missing BUILD SUCCESSFUL", blob)

    def test_missing_gradle_receipt_does_not_claim_listing_lag(self):
        errors = MODULE.receipt_errors("", PROPERTIES, MISSING)
        blob = "\n".join(errors)
        self.assertTrue(any("BUILD SUCCESSFUL" in error for error in errors), errors)
        self.assertNotIn("listing still lagging", blob)
        self.assertNotIn("Do not re-dispatch", blob)


if __name__ == "__main__":
    unittest.main()
