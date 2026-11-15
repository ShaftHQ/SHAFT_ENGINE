"""CE 2026-11-02 lessons: red+green fixtures for #7692–#7721 gates (L74)."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from unittest import TestCase, main

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DETAIL = CE / "references/details/ce-lessons-2026-11-02"
sys.path.insert(0, str(ROOT))

def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod

def _ce_key(value: str):
    spec = importlib.util.spec_from_file_location(
        "ce_version", ROOT / "scripts/ci/ce_version.py"
    )
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.version_key(value)

class BoxDiskGuardAptExtensionTest(TestCase):
    def test_apt_5xx_and_unsigned(self) -> None:
        from scripts.ci import box_disk_guard as bdg

        reasons = bdg.blocked_infra_reasons(
            ".",
            free=50 * 1024**3,
            used_percent=10.0,
            swap_kib=1024,
            apt_mirror_5xx=True,
            apt_lists_unsigned=True,
        )
        self.assertIn("apt-mirror-5xx", reasons)
        self.assertIn("apt-lists-unsigned", reasons)
        clean = bdg.blocked_infra_reasons(
            ".",
            free=50 * 1024**3,
            used_percent=10.0,
            swap_kib=1024,
            apt_mirror_ok=True,
        )
        self.assertEqual([], [r for r in clean if r.startswith("apt-")])

if __name__ == "__main__":
    main()
