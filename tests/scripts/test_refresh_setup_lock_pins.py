"""Setup lock SHA-256 pin refresh contract tests (#6357)."""

import hashlib
import io
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from scripts.ci import refresh_setup_lock_pins as pins

OLD = "a" * 64
LOCK = '{\r\n  "name": "fixture"\r\n}\r\n'
CANONICAL = hashlib.sha256(LOCK.replace("\r\n", "\n").encode("utf-8")).hexdigest()


class RefreshSetupLockPinsTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        (self.root / "res").mkdir()
        (self.root / "res/package-lock.json").write_bytes(LOCK.encode("utf-8"))
        self.java = self.root / "Planner.java"
        self.java.write_text(
            "class Planner {\n"
            f'    public static final String BARE_LOCK_SHA256 =\n            "{OLD}";\n'
            f'    static final String PREFIXED_LOCK_SHA256 = "sha256:{OLD}";\n'
            f'    static final String OTHER_SHA256 = "{OLD}";\n'
            "}\n",
            encoding="utf-8",
        )
        self.pins = (
            pins.LockPin("res/package-lock.json", "Planner.java", "BARE_LOCK_SHA256"),
            pins.LockPin("res/package-lock.json", "Planner.java", "PREFIXED_LOCK_SHA256"),
        )

    def test_canonical_digest_normalizes_crlf_and_cr_to_lf(self):
        self.assertEqual(CANONICAL, pins.canonical_digest(self.root / "res/package-lock.json"))

    def test_check_reports_drift_with_fingerprint_and_fails(self):
        output = io.StringIO()
        with redirect_stdout(output):
            code = pins.main(["--root", str(self.root), "--check"], self.pins)
        self.assertEqual(1, code)
        self.assertIn(pins.FINGERPRINT, output.getvalue())
        self.assertIn(OLD, self.java.read_text(encoding="utf-8"))

    def test_write_rewrites_only_pinned_constants_and_keeps_sha256_prefix(self):
        with redirect_stdout(io.StringIO()):
            code = pins.main(["--root", str(self.root), "--write"], self.pins)
        text = self.java.read_text(encoding="utf-8")
        self.assertEqual(0, code)
        self.assertIn(f'BARE_LOCK_SHA256 =\n            "{CANONICAL}";', text)
        self.assertIn(f'PREFIXED_LOCK_SHA256 = "sha256:{CANONICAL}";', text)
        self.assertIn(f'OTHER_SHA256 = "{OLD}";', text)

    def test_check_passes_after_write(self):
        with redirect_stdout(io.StringIO()):
            pins.main(["--root", str(self.root), "--write"], self.pins)
            self.assertEqual(0, pins.main(["--root", str(self.root), "--check"], self.pins))

    def test_missing_constant_is_an_error_not_a_silent_skip(self):
        broken = (pins.LockPin("res/package-lock.json", "Planner.java", "ABSENT_LOCK_SHA256"),)
        with self.assertRaises(pins.PinError):
            pins.refresh(self.root, broken, write=False)

    def test_repository_pins_cover_every_bundled_setup_lockfile(self):
        resources = pins.ROOT / "shaft-infrastructure/src/main/resources"
        bundled = {path.relative_to(pins.ROOT).as_posix() for path in resources.rglob("package-lock.json")}
        self.assertEqual(bundled, {pin.lockfile for pin in pins.PINS})

    def test_repository_pins_match_their_lockfiles(self):
        self.assertEqual([], pins.refresh(pins.ROOT, pins.PINS, write=False))


if __name__ == "__main__":
    unittest.main()


class SetupNpmAutomationContractTest(unittest.TestCase):
    """#6369 Dependabot npm coverage and #6370 BOT_TOKEN guard."""

    ROOT = Path(__file__).resolve().parents[2]

    def test_dependabot_npm_entry_covers_every_bundled_setup_lockfile(self):
        text = (self.ROOT / ".github/dependabot.yml").read_text(encoding="utf-8")
        self.assertEqual(1, text.count('package-ecosystem: "npm"'))
        block = text.split('package-ecosystem: "npm"', 1)[1].split("package-ecosystem:", 1)[0]
        for lock in (self.ROOT / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure").glob(
            "*/package-lock.json"
        ):
            with self.subTest(lock=lock.parent.name):
                self.assertIn(f'- "/{lock.parent.relative_to(self.ROOT).as_posix()}"', block)
        self.assertIn('interval: "weekly"', block)
        self.assertIn("groups:", block)

    def test_refresh_job_guards_bot_token_and_names_fallback(self):
        text = (self.ROOT / ".github/workflows/setup-lock-pins.yml").read_text(encoding="utf-8")
        self.assertIn("Verify BOT_TOKEN can push", text)
        self.assertIn("BOT_TOKEN expired or lacks push", text)
        self.assertIn("Lock-pin push rejected", text)
        self.assertGreaterEqual(text.count("refresh_setup_lock_pins.py --write' on the PR branch"), 2)
        self.assertLess(text.index("Verify BOT_TOKEN can push"), text.index("Checkout Dependabot head"))
