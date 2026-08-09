"""Portable act-as-mohab package assembly tests (#4576)."""

import tempfile
import unittest
from pathlib import Path

try:
    from scripts.ci.assemble_act_as_mohab_plugin import assemble
except ModuleNotFoundError:
    assemble = None

from scripts.ci.validate_agent_plugins import validate_package


ROOT = Path(__file__).resolve().parents[2]
CANONICAL_SKILLS = ROOT / ".agents/skills"


class AssembleActAsMohabPluginTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.package_root = Path(self.temporary_directory.name) / "act-as-mohab"

    def tearDown(self):
        self.temporary_directory.cleanup()

    def test_assembly_creates_a_valid_self_contained_package_from_canonical_sources(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        assemble(ROOT, self.package_root)

        self.assertEqual(validate_package(self.package_root), [])
        self.assertEqual(
            (self.package_root / "plugin.json").read_text(encoding="utf-8"),
            '{"$schema":"https://agent-plugins.org/schemas/1.0.0/plugin.schema.json","name":"act-as-mohab"}\n',
        )
        for skill in ("act-as-mohab", "consult-first", "retrieve-first"):
            self.assertEqual(
                (self.package_root / "skills" / skill / "SKILL.md").read_bytes(),
                (CANONICAL_SKILLS / skill / "SKILL.md").read_bytes(),
            )
        for source in (CANONICAL_SKILLS / "act-as-mohab/references").rglob("*"):
            if source.is_file():
                target = self.package_root / "skills/act-as-mohab/references" / source.relative_to(
                    CANONICAL_SKILLS / "act-as-mohab/references"
                )
                self.assertEqual(target.read_bytes(), source.read_bytes(), target)
        self.assertFalse((self.package_root / "skills/act-as-mohab/agents").exists())

    def test_assembly_is_deterministic(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        second_root = self.package_root.parent / "second"
        assemble(ROOT, self.package_root)
        assemble(ROOT, second_root)

        first = sorted(path.relative_to(self.package_root) for path in self.package_root.rglob("*") if path.is_file())
        second = sorted(path.relative_to(second_root) for path in second_root.rglob("*") if path.is_file())
        self.assertEqual(first, second)
        for path in first:
            self.assertEqual((self.package_root / path).read_bytes(), (second_root / path).read_bytes(), path)


if __name__ == "__main__":
    unittest.main()
