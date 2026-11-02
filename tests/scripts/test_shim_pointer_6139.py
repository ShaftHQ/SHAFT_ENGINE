"""Pointer shim, one hardware probe, and ICM vendor entrypoint (#6139)."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load(name: str, path: Path):
    import sys
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


class ShimPointerTest(unittest.TestCase):
    def test_pointer_catalog_reexport_and_vendor_icm_path(self):
        router = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        skill = (ROOT / "chaos-engine/skills/local-coding-delegate/SKILL.md").read_text(encoding="utf-8")
        vendor = (ROOT / "chaos-engine/vendor/icm-architect/skills/icm-architect/SKILL.md").read_text(encoding="utf-8")
        # The router card delegates skill paths to the generated catalog (#6173 wave 2).
        catalog = (ROOT / "chaos-engine/references/catalog.md").read_text(encoding="utf-8")
        self.assertIn("pointer", skill.casefold())
        self.assertIn("references/catalog.md", router)
        self.assertIn("vendor/icm-architect/skills/icm-architect/SKILL.md", catalog)
        self.assertNotIn("references/icm-architect.md) |", catalog)
        self.assertIn("references/icm-architect.md", vendor)
        shim = _load("hardware_probe_6139", ROOT / "chaos-engine/skills/local-coding-delegate/hardware_probe.py")
        home = _load("probe_home_6139", ROOT / "chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py")
        import inspect
        self.assertEqual(
            Path(inspect.getsourcefile(shim.probe)).resolve(),
            (ROOT / "chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py").resolve(),
        )
        self.assertEqual(shim.classify(8 * 1024**3, None, "linux"), home.classify(8 * 1024**3, None, "linux"))


if __name__ == "__main__":
    unittest.main()
