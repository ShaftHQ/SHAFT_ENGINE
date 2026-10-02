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


TARBALL = b"fixture tarball bytes"
TARBALL_SHA256 = hashlib.sha256(TARBALL).hexdigest()
TARBALL_INTEGRITY = "sha512-" + __import__("base64").b64encode(hashlib.sha512(TARBALL).digest()).decode("ascii")
URL = "https://registry.npmjs.org/demo/-/demo-2.0.0.tgz"


class RefreshSetupPackagePinsTest(unittest.TestCase):
    """#6381: planner version, tarball SHA-256 and size pins follow the bundled manifests."""

    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        for bundle in ("one", "two"):
            self._bundle(bundle, "2.0.0", URL, TARBALL_INTEGRITY)
        (self.root / "A.java").write_text(
            'class A {\n    public static final String DEMO_VERSION = "1.0.0";\n'
            f'    private static final String DEMO_SHA256 =\n            "{OLD}";\n'
            "    private static final long DEMO_ARTIFACT_BYTES = 1_234L;\n}\n",
            encoding="utf-8",
        )
        (self.root / "B.java").write_text(
            f'class B {{\n    private static final String DEMO_SHA256 = "{OLD}";\n}}\n', encoding="utf-8"
        )
        self.pins = (
            pins.PackagePin("demo", ("one", "two"), ("A.java", "DEMO_VERSION"),
                            (("A.java", "DEMO_SHA256"), ("B.java", "DEMO_SHA256")), ("A.java", "DEMO_ARTIFACT_BYTES")),
        )
        self.fetched = []

    def _bundle(self, name, version, resolved, integrity, manifest_version=None):
        directory = self.root / name
        directory.mkdir(exist_ok=True)
        (directory / "package.json").write_text(
            '{"dependencies": {"demo": "%s"}}' % (manifest_version or version), encoding="utf-8")
        (directory / "package-lock.json").write_text(
            '{"packages": {"": {"dependencies": {"demo": "%s"}}, "node_modules/demo": '
            '{"version": "%s", "resolved": "%s", "integrity": "%s"}}}'
            % (manifest_version or version, version, resolved, integrity), encoding="utf-8")

    def fetch(self, url):
        self.fetched.append(url)
        return TARBALL

    def test_check_reports_version_drift_offline_with_fingerprint(self):
        drift = pins.refresh_packages(self.root, self.pins, write=False, fetch=self.fetch)
        self.assertEqual(1, len(drift))
        self.assertIn(pins.PACKAGE_FINGERPRINT, drift[0])
        self.assertIn("1.0.0 -> 2.0.0", drift[0])
        self.assertEqual([], self.fetched)

    def test_write_rewrites_version_every_digest_copy_and_size_from_verified_tarball(self):
        pins.refresh_packages(self.root, self.pins, write=True, fetch=self.fetch)
        a = (self.root / "A.java").read_text(encoding="utf-8")
        b = (self.root / "B.java").read_text(encoding="utf-8")
        self.assertEqual([URL], self.fetched)
        self.assertIn('DEMO_VERSION = "2.0.0";', a)
        self.assertIn(f'DEMO_SHA256 =\n            "{TARBALL_SHA256}";', a)
        self.assertIn(f"DEMO_ARTIFACT_BYTES = {len(TARBALL)}L;", a)
        self.assertIn(f'DEMO_SHA256 = "{TARBALL_SHA256}";', b)
        self.assertEqual([], pins.refresh_packages(self.root, self.pins, write=False, fetch=self.fetch,
                                                   verify_artifacts=True))

    def test_tarball_not_matching_lockfile_integrity_is_rejected_and_nothing_written(self):
        before = (self.root / "A.java").read_text(encoding="utf-8")
        with self.assertRaises(pins.PinError):
            pins.refresh_packages(self.root, self.pins, write=True, fetch=lambda url: b"tampered")
        self.assertEqual(before, (self.root / "A.java").read_text(encoding="utf-8"))

    def test_bundles_disagreeing_on_a_shared_package_version_is_an_error(self):
        self._bundle("two", "2.0.1", URL, TARBALL_INTEGRITY)
        with self.assertRaises(pins.PinError):
            pins.refresh_packages(self.root, self.pins, write=False, fetch=self.fetch)

    def test_manifest_and_lock_disagreeing_is_an_error(self):
        self._bundle("one", "2.0.0", URL, TARBALL_INTEGRITY, manifest_version="2.0.1")
        with self.assertRaises(pins.PinError):
            pins.refresh_packages(self.root, self.pins, write=False, fetch=self.fetch)

    def test_non_registry_tarball_url_is_rejected(self):
        for bundle in ("one", "two"):
            self._bundle(bundle, "2.0.0", "https://evil.example/demo-2.0.0.tgz", TARBALL_INTEGRITY)
        with self.assertRaises(pins.PinError):
            pins.refresh_packages(self.root, self.pins, write=True, fetch=self.fetch)
        self.assertEqual([], self.fetched)

    def test_verify_artifacts_reports_digest_drift_at_an_unchanged_version(self):
        pins.refresh_packages(self.root, self.pins, write=True, fetch=self.fetch)
        b = self.root / "B.java"
        b.write_text(b.read_text(encoding="utf-8").replace(TARBALL_SHA256, OLD), encoding="utf-8")
        drift = pins.refresh_packages(self.root, self.pins, write=False, fetch=self.fetch, verify_artifacts=True)
        self.assertEqual(1, len(drift))
        self.assertIn("B.java DEMO_SHA256", drift[0])

    def test_main_check_fails_on_package_drift_and_write_fixes_it(self):
        with redirect_stdout(io.StringIO()):
            self.assertEqual(1, pins.main(["--root", str(self.root), "--check"], (), self.pins, self.fetch))
            self.assertEqual(0, pins.main(["--root", str(self.root), "--write"], (), self.pins, self.fetch))
            self.assertEqual(0, pins.main(["--root", str(self.root), "--check"], (), self.pins, self.fetch))

    def test_repository_package_pins_cover_every_bundled_top_level_dependency(self):
        import json
        resources = pins.ROOT / pins._RESOURCES
        bundled = {(path.parent.relative_to(pins.ROOT).as_posix(), name)
                   for path in resources.glob("*/package.json")
                   for name in json.loads(path.read_text(encoding="utf-8"))["dependencies"]}
        pinned = {(bundle, pin.package) for pin in pins.PACKAGE_PINS for bundle in pin.bundles}
        self.assertEqual(bundled, pinned)

    def test_repository_package_pins_match_the_bundled_manifests(self):
        self.assertEqual([], pins.refresh_packages(pins.ROOT, pins.PACKAGE_PINS, write=False,
                                                   fetch=self.fetch))
        self.assertEqual([], self.fetched)

    def test_check_job_verifies_tarball_pins_against_the_lockfiles(self):
        text = (pins.ROOT / ".github/workflows/setup-lock-pins.yml").read_text(encoding="utf-8")
        self.assertIn("refresh_setup_lock_pins.py --check --verify-artifacts", text)
        self.assertIn("'shaft-infrastructure/src/main/resources/com/shaft/infrastructure/**/package.json'", text)


if __name__ == "__main__":
    unittest.main()
