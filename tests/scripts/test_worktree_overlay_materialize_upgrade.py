"""worktree_overlay materialize: upgrade stale files; leave current untouched."""
from __future__ import annotations

import importlib.util
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine" / "worktree_overlay.py"


def load():
    spec = importlib.util.spec_from_file_location("ce_overlay_upgrade", SOURCE)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class MaterializeUpgradeTest(unittest.TestCase):
    def test_stale_upgraded_current_untouched(self) -> None:
        overlay = load()
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            # Minimal overlay so materialize accepts primary.
            router = primary / ".chaos-engine/skills/chaos-engine/SKILL.md"
            router.parent.mkdir(parents=True)
            router.write_text("v1\n", encoding="utf-8")
            (primary / ".chaos-engine/hooks").mkdir(parents=True)
            (primary / ".chaos-engine/hooks/guard.py").write_text("# guard v1\n", encoding="utf-8")
            (primary / ".chaos-engine/VERSION").write_text("2026.10.30.1\n", encoding="utf-8")
            # First materialize seeds session + manifest.
            written = overlay.materialize(primary, session)
            self.assertTrue(written)
            skill = session / ".chaos-engine/skills/chaos-engine/SKILL.md"
            guard = session / ".chaos-engine/hooks/guard.py"
            self.assertEqual("v1\n", skill.read_text(encoding="utf-8"))
            mtime_guard = guard.stat().st_mtime_ns
            # Primary advances skill only.
            router.write_text("v2-upgraded\n", encoding="utf-8")
            (primary / ".chaos-engine/VERSION").write_text("2026.10.30.2\n", encoding="utf-8")
            # Second materialize: stale skill upgrades; current guard untouched.
            time.sleep(0.01)
            again = overlay.materialize(primary, session)
            self.assertEqual("v2-upgraded\n", skill.read_text(encoding="utf-8"))
            self.assertEqual("# guard v1\n", guard.read_text(encoding="utf-8"))
            self.assertEqual(mtime_guard, guard.stat().st_mtime_ns)
            self.assertTrue(any(skill.as_posix() in p or p.endswith("SKILL.md") for p in again))
            # Idempotent: third call writes nothing new for current files.
            third = overlay.materialize(primary, session)
            self.assertEqual([], third)

    def test_local_edit_refused(self) -> None:
        overlay = load()
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            router = primary / ".chaos-engine/skills/chaos-engine/SKILL.md"
            router.parent.mkdir(parents=True)
            router.write_text("upstream\n", encoding="utf-8")
            (primary / ".chaos-engine/hooks").mkdir(parents=True)
            (primary / ".chaos-engine/hooks/guard.py").write_text("# g\n", encoding="utf-8")
            overlay.materialize(primary, session)
            # Local edit + primary change without matching manifest recorded hash for edit.
            skill = session / ".chaos-engine/skills/chaos-engine/SKILL.md"
            skill.write_text("local-edit\n", encoding="utf-8")
            router.write_text("upstream-v2\n", encoding="utf-8")
            with self.assertRaises(overlay.OverlayLocalEditError):
                overlay.materialize(primary, session)

    def test_timing_budget_constant_documented(self) -> None:
        overlay = load()
        self.assertLessEqual(overlay.MATERIALIZE_BUDGET_S_PER_WORKTREE, 5.0)
        self.assertLessEqual(overlay.UPGRADE_WITH_PLUGINS_BUDGET_S, 15.0)
        doc = (ROOT / "chaos-engine/references/ce-upgrade-on-release.md").read_text(encoding="utf-8")
        self.assertIn("upgrade-session", doc)
        self.assertIn("5s", doc)


if __name__ == "__main__":
    unittest.main()
