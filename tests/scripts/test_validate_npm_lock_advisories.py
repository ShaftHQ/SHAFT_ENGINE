"""Tests for scripts/ci/validate_npm_lock_advisories.py and repair-shell-quote.js (#7451)."""

from __future__ import annotations

import json
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

    def test_repo_locks_currently_pass(self):
        self.assertEqual(0, advisories.main([]))


if __name__ == "__main__":
    unittest.main()
