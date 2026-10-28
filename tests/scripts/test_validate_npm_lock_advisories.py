"""Tests for scripts/ci/validate_npm_lock_advisories.py (#7451)."""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

import sys as _sys
from pathlib import Path as _Path

_sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from scripts.ci import validate_npm_lock_advisories as advisories


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
            # Minimal siblings so LOCKS paths exist
            for name in ("appium-ios", "appium-windows"):
                sibling = root / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure" / name
                sibling.mkdir(parents=True)
                (sibling / "package-lock.json").write_text(
                    json.dumps({"packages": {"node_modules/shell-quote": {"version": "1.11.0"}}}),
                    encoding="utf-8",
                )
                (sibling / "package.json").write_text(
                    json.dumps(
                        {
                            "overrides": {"shell-quote": "1.11.0"},
                            "scripts": {"postinstall": "node ./repair-shell-quote.js"},
                        }
                    ),
                    encoding="utf-8",
                )
                (sibling / "repair-shell-quote.js").write_text("// fixture\n", encoding="utf-8")
            pin = root / "scripts/ci/npm-pins/appium-3.5.2"
            pin.mkdir(parents=True)
            (pin / "package-lock.json").write_text(
                json.dumps({"packages": {"node_modules/shell-quote": {"version": "1.11.0"}}}),
                encoding="utf-8",
            )
            (root / "scripts/ci").mkdir(parents=True, exist_ok=True)
            (root / "scripts/ci/repair_bundled_shell_quote.js").write_text("// fixture\n", encoding="utf-8")
            self.assertEqual(1, advisories.main(["--root", str(root)]))

    def test_repo_locks_currently_pass(self):
        self.assertEqual(0, advisories.main([]))


if __name__ == "__main__":
    unittest.main()
