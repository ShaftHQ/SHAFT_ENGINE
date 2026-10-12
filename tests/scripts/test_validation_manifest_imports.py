"""Pre-push catches a shipped validator importing a module the installer does not ship (#6843)."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts.ci import validation_manifest_imports as manifest
from scripts.ci.overlay_pre_push import shipped_validator_import_failures

ROOT = Path(__file__).resolve().parents[2]


class ValidationManifestImportsTest(unittest.TestCase):
    def write(self, root: Path, relative: str, text: str) -> None:
        path = root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")

    def test_reports_unshipped_scripts_import(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self.write(root, "scripts/ci/validator.py", "from scripts.ci.helper import thing\n")
            self.write(root, "scripts/ci/helper.py", "thing = 1\n")
            self.assertEqual(["scripts/ci/validator.py imports scripts.ci.helper"],
                             manifest.missing_imports(root, ("scripts/ci/validator.py",)))
            self.assertEqual([], manifest.missing_imports(
                root, ("scripts/ci/validator.py", "scripts/ci/helper.py")))

    def test_real_manifest_is_complete(self):
        self.assertEqual([], manifest.missing_imports(ROOT, manifest.shipped_files(ROOT)))

    def test_pre_push_runs_only_for_shipped_validator_changes(self):
        self.assertTrue(manifest.touches_shipped_validators(ROOT, ["scripts/ci/overlay_pre_push.py"]))
        self.assertFalse(manifest.touches_shipped_validators(ROOT, ["shaft-engine/pom.xml"]))
        self.assertEqual([], shipped_validator_import_failures(ROOT, ["scripts/ci/overlay_pre_push.py"]))


if __name__ == "__main__":
    unittest.main()
