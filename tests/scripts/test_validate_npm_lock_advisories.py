"""Tests for scripts/ci/validate_npm_lock_advisories.py and repair-shell-quote.js (#7451)."""

from __future__ import annotations

import json
import os
import shutil
import subprocess  # nosec B404 -- resolved node and repository-owned repair script only.
import tempfile
import unittest
from pathlib import Path

import sys as _sys

_sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from scripts.ci import validate_npm_lock_advisories as advisories

ROOT = Path(__file__).resolve().parents[2]
FIXTURE_LOCK = ROOT / "tests/fixtures/npm_lock_advisories/inbundle_shell_quote_1_10_0.package-lock.json"
REPAIR = ROOT / "scripts/ci/repair_bundled_shell_quote.js"
NODE = shutil.which("node")
NPM = shutil.which("npm")
BUNDLES = ("appium", "appium-ios", "appium-windows")
INSTALLED_FLOORS = {
    "glob": (14, 0, 2),
    "minimatch": (11, 0, 1),
    "follow-redirects": (1, 16, 2),
    "shell-quote": (1, 11, 0),
}
# npm ci --ignore-scripts extracts these bundled copies even when the lock node was rewritten.
NPM_CI_STILL_EXTRACTS = {
    "appium": ("appium-uiautomator2-driver", "minimatch", "10.2.6"),
    "appium-ios": ("appium-xcuitest-driver", "glob", "13.0.6"),
    "appium-windows": ("appium-windows-driver", "follow-redirects", "1.16.1"),
}


def _write_good_bundle(root: Path, name: str) -> None:
    lock_dir = root / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure" / name
    lock_dir.mkdir(parents=True)
    (lock_dir / "package-lock.json").write_text(
        json.dumps({"packages": {"node_modules/shell-quote": {"version": "1.11.0"}}}),
        encoding="utf-8",
    )
    (lock_dir / "package.json").write_text(
        json.dumps(
            {
                "overrides": {"shell-quote": "1.11.0"},
                "scripts": {"postinstall": "node ./repair-shell-quote.js"},
            }
        ),
        encoding="utf-8",
    )
    (lock_dir / "repair-shell-quote.js").write_text("// fixture\n", encoding="utf-8")


def _write_pin_and_repair(root: Path) -> None:
    pin = root / "scripts/ci/npm-pins/appium-3.5.2"
    pin.mkdir(parents=True)
    (pin / "package-lock.json").write_text(
        json.dumps({"packages": {"node_modules/shell-quote": {"version": "1.11.0"}}}),
        encoding="utf-8",
    )
    (root / "scripts/ci").mkdir(parents=True, exist_ok=True)
    (root / "scripts/ci/repair_bundled_shell_quote.js").write_text("// fixture\n", encoding="utf-8")


class ValidateNpmLockAdvisoriesTest(unittest.TestCase):
    def test_exact_declared_vulnerable_shell_quote_fails(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            lock_dir = root / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/appium"
            lock_dir.mkdir(parents=True)
            (lock_dir / "package-lock.json").write_text(
                json.dumps(
                    {
                        "packages": {
                            "node_modules/@appium/support": {
                                "version": "7.2.8",
                                "dependencies": {"shell-quote": "1.8.4"},
                            },
                            "node_modules/shell-quote": {
                                "version": "1.11.0",
                                "resolved": "https://registry.npmjs.org/shell-quote/-/shell-quote-1.11.0.tgz",
                            },
                        }
                    }
                ),
                encoding="utf-8",
            )
            (lock_dir / "package.json").write_text(
                json.dumps(
                    {
                        "overrides": {"shell-quote": "1.11.0"},
                        "scripts": {"postinstall": "node ./repair-shell-quote.js"},
                    }
                ),
                encoding="utf-8",
            )
            (lock_dir / "repair-shell-quote.js").write_text("// fixture\n", encoding="utf-8")
            for name in ("appium-ios", "appium-windows"):
                _write_good_bundle(root, name)
            _write_pin_and_repair(root)
            self.assertEqual(1, advisories.main(["--root", str(root)]))

    def test_inbundle_shell_quote_below_1_11_fixture_fails_validator(self):
        """#7451 failure mode: nested inBundle shell-quote@1.10.0 must fail the lock audit."""
        self.assertTrue(FIXTURE_LOCK.is_file(), FIXTURE_LOCK)
        errors = advisories.audit_lock(FIXTURE_LOCK, ROOT)
        self.assertTrue(
            any("inBundle" in e or "bundled" in e for e in errors),
            errors,
        )
        self.assertTrue(any("1.10.0" in e for e in errors), errors)

    def test_repair_fails_when_hoisted_missing_and_nested_vulnerable(self):
        """Repair must not exit 0 when nested shell-quote <1.11.0 remains without a hoisted source."""
        self.assertIsNotNone(NODE, "node executable required")
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            nested = (
                root
                / "node_modules/appium-windows-driver/node_modules/teen_process/node_modules/shell-quote"
            )
            nested.mkdir(parents=True)
            (nested / "package.json").write_text(
                json.dumps({"name": "shell-quote", "version": "1.10.0"}),
                encoding="utf-8",
            )
            result = subprocess.run(  # nosec B603 -- resolved node and repository-owned repair script.
                [NODE, str(REPAIR)],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(1, result.returncode, result.stdout + result.stderr)
            self.assertIn("1.10.0", result.stderr)

    def test_repair_replaces_nested_inbundle_copy_from_hoisted(self):
        self.assertIsNotNone(NODE, "node executable required")
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            hoisted = root / "node_modules/shell-quote"
            hoisted.mkdir(parents=True)
            (hoisted / "package.json").write_text(
                json.dumps({"name": "shell-quote", "version": "1.11.0"}),
                encoding="utf-8",
            )
            (hoisted / "index.js").write_text("module.exports = {}\n", encoding="utf-8")
            nested = root / "node_modules/appium-windows-driver/node_modules/shell-quote"
            nested.mkdir(parents=True)
            (nested / "package.json").write_text(
                json.dumps({"name": "shell-quote", "version": "1.10.0"}),
                encoding="utf-8",
            )
            result = subprocess.run(  # nosec B603 -- resolved node and repository-owned repair script.
                [NODE, str(REPAIR)],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(0, result.returncode, result.stdout + result.stderr)
            repaired = json.loads((nested / "package.json").read_text(encoding="utf-8"))
            self.assertEqual("1.11.0", repaired["version"])

    def test_repair_replaces_nested_minimatch_from_hoisted(self):
        """inBundle minimatch 10.2.6 is replaced from the hoisted patched tree."""
        self.assertIsNotNone(NODE, "node executable required")
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            hoisted = root / "node_modules/minimatch"
            hoisted.mkdir(parents=True)
            (hoisted / "package.json").write_text(
                json.dumps({"name": "minimatch", "version": "11.0.5"}),
                encoding="utf-8",
            )
            nested = root / "node_modules/appium-uiautomator2-driver/node_modules/minimatch"
            nested.mkdir(parents=True)
            (nested / "package.json").write_text(
                json.dumps({"name": "minimatch", "version": "10.2.6"}),
                encoding="utf-8",
            )
            result = subprocess.run(  # nosec B603 -- resolved node and repository-owned repair script.
                [NODE, str(REPAIR)],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(0, result.returncode, result.stdout + result.stderr)
            repaired = json.loads((nested / "package.json").read_text(encoding="utf-8"))
            self.assertEqual("11.0.5", repaired["version"])

    def test_three_appium_locks_clear_nested_advisory_floors(self):
        names = {"appium", "appium-ios", "appium-windows"}
        seen: set[str] = set()
        for lock in advisories.LOCKS:
            if lock.parent.name not in names:
                continue
            seen.add(lock.parent.name)
            errors = [
                error
                for error in advisories.audit_lock(lock)
                if any(package in error for package in ("minimatch@", "glob@", "follow-redirects@"))
            ]
            self.assertEqual([], errors, lock)
        self.assertEqual(names, seen)

    def test_repo_locks_currently_pass(self):
        self.assertEqual(0, advisories.main([]))

    def test_installers_run_the_shipped_repair_after_npm_ci_ignore_scripts(self):
        for relative in (
            "shaft-infrastructure/src/main/java/com/shaft/infrastructure/DefaultAndroidToolchainOperations.java",
            "shaft-infrastructure/src/main/java/com/shaft/infrastructure/DefaultDesktopMobileToolchainOperations.java",
        ):
            text = (ROOT / relative).read_text(encoding="utf-8")
            self.assertIn('"repair-shell-quote.js"', text, relative)
            self.assertLess(text.index("--ignore-scripts"), text.index("repair-shell-quote.js"), relative)

    def test_npm_ci_extracts_bundled_copies_and_the_shipped_repair_replaces_them(self):
        """The installer path is npm ci --ignore-scripts, then the bundled repair script."""
        self.assertIsNotNone(NODE, "node executable required")
        self.assertIsNotNone(NPM, "npm executable required")
        resources = ROOT / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure"
        for name in BUNDLES:
            with tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                source = resources / name
                for filename in ("package.json", "package-lock.json", "repair-shell-quote.js"):
                    shutil.copy(source / filename, root / filename)
                install = subprocess.run(  # nosec B603 -- same npm ci the toolchain runs, scripts ignored.
                    [NPM, "ci", "--ignore-scripts", "--no-audit", "--no-fund", "--legacy-peer-deps"],
                    cwd=root,
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(0, install.returncode, install.stderr[-4000:])
                driver, package, version = NPM_CI_STILL_EXTRACTS[name]
                before = _installed_advisory_packages(root)
                extracted = [
                    item for item in before
                    if item[1] == package and item[2] == version and driver in item[0]
                ]
                self.assertTrue(extracted, before)
                repair = subprocess.run(  # nosec B603 -- shipped repair-shell-quote.js next to the lock.
                    [NODE, str(root / "repair-shell-quote.js")],
                    cwd=root,
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(0, repair.returncode, repair.stdout + repair.stderr)
                bad = [
                    (rel, ver) for rel, pkg, ver in _installed_advisory_packages(root)
                    if (_version_tuple(ver) or (0, 0, 0)) < INSTALLED_FLOORS[pkg]
                ]
                self.assertEqual([], bad)


def _version_tuple(version: str) -> tuple[int, int, int] | None:
    parts = str(version).split(".")
    if len(parts) < 3:
        return None
    try:
        return tuple(int(part) for part in parts[:3])  # type: ignore[return-value]
    except ValueError:
        return None


def _installed_advisory_packages(root: Path) -> list[tuple[str, str, str]]:
    found: list[tuple[str, str, str]] = []
    modules = root / "node_modules"
    if not modules.is_dir():
        return found
    for dirpath, _dirnames, filenames in os.walk(modules):
        if "package.json" not in filenames:
            continue
        current = Path(dirpath)
        if current.name not in INSTALLED_FLOORS:
            continue
        manifest = json.loads((current / "package.json").read_text(encoding="utf-8"))
        found.append((current.relative_to(root).as_posix(), current.name, str(manifest.get("version") or "")))
    return found


if __name__ == "__main__":
    unittest.main()
