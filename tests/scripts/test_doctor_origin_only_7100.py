"""Doctor's policy-hash check uses the installer's source-only list (regression from #7097)."""

from __future__ import annotations

import shutil
import tempfile
import unittest
from pathlib import Path

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from tests.scripts.test_doctor_overlay_drift_6136 import _load

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
overlay_match = _load("overlay_match_7100", SOURCE / "overlay_match.py")
install = overlay_match._install_module()


class DoctorOriginOnlyTest(unittest.TestCase):
    def test_fresh_install_payload_has_no_policy_drift(self) -> None:
        """Copy exactly what the installer copies; doctor must then see zero drift."""
        with tempfile.TemporaryDirectory() as tmp:
            overlay = Path(tmp) / ".chaos-engine"
            for path in overlay_match.owned_source_files(SOURCE):
                target = overlay / install.payload_relative(SOURCE, path)
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(path, target)
            self.assertFalse((overlay / "references/harness-files.md").exists())
            self.assertEqual([], overlay_match.policy_hash_mismatches(SOURCE, overlay))

    def test_source_only_list_is_the_installers(self) -> None:
        self.assertTrue(install.is_origin_only(Path("references/harness-files.md")))


if __name__ == "__main__":
    unittest.main()
