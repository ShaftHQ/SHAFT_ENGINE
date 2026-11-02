"""Uninstall/rollback clarity for first-time users (#5576)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
INSTALLER = SOURCE / "install.py"


def load_install():
    spec = importlib.util.spec_from_file_location("ce_uninstall_clarity", INSTALLER)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load install.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class UninstallClarityTests(unittest.TestCase):
    def test_install_docs_name_removed_vs_retained(self):
        text = (SOURCE / "INSTALL.md").read_text(encoding="utf-8")
        self.assertIn("Uninstall / rollback (first-time recovery)", text)
        self.assertIn("What is removed vs retained", text)
        self.assertIn("Clean reinstall after a bad first run", text)
        self.assertIn("user-account packages", text)
        self.assertIn("project knowledge data", text)
        self.assertIn("Maven Tools MCP cache", text)
        self.assertIn("uninstall --project .", text)
        self.assertIn("rollback --project .", text)

    def test_fixture_uninstall_removes_core_keeps_knowledge_data(self):
        module = load_install()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            memory = project / ".memory"
            memory.mkdir()
            memory.joinpath("keep.txt").write_text("knowledge", encoding="utf-8")
            graphify = project / "graphify-out"
            graphify.mkdir()
            graphify.joinpath("keep.txt").write_text("index", encoding="utf-8")
            module.install_with_dependencies(
                project,
                SOURCE,
                "c" * 40,
                provisioner=lambda *_a, **_k: None,
            )
            self.assertTrue((project / ".chaos-engine/skills/chaos-engine/SKILL.md").is_file())
            module.uninstall_with_dependencies(project)
            self.assertFalse((project / ".chaos-engine").exists())
            self.assertEqual("knowledge", memory.joinpath("keep.txt").read_text(encoding="utf-8"))
            self.assertEqual("index", graphify.joinpath("keep.txt").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()

