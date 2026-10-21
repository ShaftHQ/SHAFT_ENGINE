"""#7242 chunk 4: entry receipt line and the core doctor token-economy row."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class TokenEconomyTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.economy = load(CE / "token_economy.py", "ce_token_economy_7242")
        cls.install = load(CE / "install.py", "ce_install_7242")

    def _root(self, tmp: Path) -> Path:
        root = tmp / ".chaos-engine"
        for relative in self.economy.CORE_CARDS.values():
            (root / relative).parent.mkdir(parents=True, exist_ok=True)
            shutil.copy(CE / relative, root / relative)
        shutil.copy(CE / "harness_map.py", root / "harness_map.py")
        shutil.copy(CE / "stores.py", root / "stores.py")
        return root

    def test_receipt_fields_on_a_fresh_workspace_without_add_ons(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = self._root(Path(tmp))
            env = {"CHAOS_ENGINE_MEMPALACE": str(Path(tmp) / "no-palace"),
                   "CHAOS_ENGINE_GRAPHIFY_OUT": str(Path(tmp) / "no-graph")}
            with unittest.mock.patch.dict(os.environ, env):
                line = self.economy.receipt(root, project=Path(tmp)).splitlines()[0]
        self.assertRegex(
            line,
            r"^entry: caveman=ultra ponytail=ultra ethics=loaded persona=none index=\d+ "
            r"retrieve=mempalace:missing-fresh-install graphify:missing-fresh-install$",
        )

    def test_stop_phrase_turns_a_companion_off_in_the_receipt(self):
        line = self.economy.receipt(CE, project=ROOT, stopped=frozenset({"caveman"}))
        self.assertIn("caveman=off ponytail=ultra", line)

    def test_add_on_persona_is_loaded_and_bounded(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = self._root(Path(tmp))
            addon = root / "addons" / "demo-bot"
            (addon / "skills/demo-persona").mkdir(parents=True)
            (addon / "skills/demo-persona/SKILL.md").write_text("# Persona\n", encoding="utf-8")
            (addon / "addon.json").write_text(json.dumps(
                {"schemaVersion": 1, "kind": "addon", "name": "demo-bot",
                 "persona": "skills/demo-persona/SKILL.md"}), encoding="utf-8")
            self.assertIn("persona=loaded", self.economy.receipt(root, project=Path(tmp)))
            (addon / "skills/demo-persona/SKILL.md").write_text("x" * 3000, encoding="utf-8")
            self.assertIn("persona=none", self.economy.receipt(root, project=Path(tmp)))

    def test_doctor_row_passes_on_source_and_fails_without_a_card(self):
        self.assertEqual("pass", self.economy.doctor_check(CE)[0])
        with tempfile.TemporaryDirectory() as tmp:
            root = self._root(Path(tmp))
            (root / "companions/ethics-card.md").unlink()
            self.assertEqual("fail", self.economy.doctor_check(root)[0])
            self.assertEqual("fail", self.install.token_economy_status(root)[0])

    def test_doctor_row_tolerates_repo_only_files_but_not_unlisted_ones(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = self._root(Path(tmp))
            (root / "references").mkdir(exist_ok=True)
            (root / "references/harness-map.md").write_text(
                (CE / "references/harness-map.md").read_text(encoding="utf-8"), encoding="utf-8")
            self.assertEqual("pass", self.economy.doctor_check(root)[0])
            (root / "references/new-unlisted.md").write_text("# New\n", encoding="utf-8")
            self.assertEqual("fail", self.economy.doctor_check(root)[0])

    def test_agent_summary_prints_the_token_economy_line(self):
        document = {"status": "healthy", "policySha256": "a" * 64,
                    "components": {"core": {"status": "healthy"}}}
        rendered = self.install.format_agent_summary(document)
        self.assertIn("token-economy: pass", rendered)
        self.assertLessEqual(len(rendered.splitlines()), self.install.AGENT_SUMMARY_MAX_LINES)


if __name__ == "__main__":
    unittest.main()
