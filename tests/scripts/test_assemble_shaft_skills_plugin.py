"""Portable shaft-skills package assembly tests (#4576)."""

import json
import inspect
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

try:
    from scripts.ci.assemble_shaft_skills_plugin import assemble, tracked_source_files
except ModuleNotFoundError:
    assemble = None
    tracked_source_files = None

from scripts.ci.validate_agent_plugins import validate_package


ROOT = Path(__file__).resolve().parents[2]
CANONICAL_SKILLS = ROOT / "shaft-skills"
MARKDOWN_LINK = re.compile(r"\]\(([^)#?]+)")


class AssembleShaftSkillsPluginTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.package_root = Path(self.temporary_directory.name) / "shaft-skills"

    def tearDown(self):
        self.temporary_directory.cleanup()

    def test_assembly_creates_a_valid_self_contained_package_from_canonical_sources(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        assemble(ROOT, self.package_root)

        self.assertEqual(validate_package(self.package_root), [])
        self.assertEqual(
            json.loads((self.package_root / "plugin.json").read_text(encoding="utf-8")),
            {
                "$schema": "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json",
                "name": "shaft-skills",
                "version": "1.0.0",
                "description": "User-facing SHAFT test-automation skills.",
                "author": {"name": "ShaftHQ", "url": "https://github.com/ShaftHQ/SHAFT_ENGINE"},
                "repository": "https://github.com/ShaftHQ/SHAFT_ENGINE",
                "license": "MIT",
            },
        )
        for source in tracked_source_files(ROOT):
            target = self.package_root / "skills" / source.relative_to(CANONICAL_SKILLS)
            self.assertEqual(target.read_bytes(), source.read_bytes(), target)

    def test_assembly_includes_native_discovery_adapters(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        assemble(ROOT, self.package_root)

        claude = json.loads((self.package_root / ".claude-plugin/plugin.json").read_text(encoding="utf-8"))
        codex = json.loads((self.package_root / ".codex-plugin/plugin.json").read_text(encoding="utf-8"))
        marketplace = json.loads((self.package_root / ".agents/plugins/marketplace.json").read_text(encoding="utf-8"))

        self.assertEqual(claude["name"], "shaft-skills")
        self.assertEqual(codex, {"name": "shaft-skills", "version": "1.0.0", "skills": "./skills/"})
        self.assertEqual(
            marketplace["plugins"],
            [{"name": "shaft-skills", "source": {"source": "local", "path": "./"}}],
        )

    def test_assembly_uses_the_declared_release_version_and_release_files(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        self.assertIn("version", inspect.signature(assemble).parameters)
        source_root = self.package_root.parent / "source"
        skill = source_root / "shaft-skills/example/SKILL.md"
        skill.parent.mkdir(parents=True)
        skill.write_text("---\nname: example\ndescription: Example.\n---\n", encoding="utf-8")
        (source_root / "LICENSE").write_text("test license\n", encoding="utf-8")
        changelog = source_root / "agent-plugins/shaft-skills/CHANGELOG.md"
        changelog.parent.mkdir(parents=True)
        changelog.write_text("# Test changelog\n", encoding="utf-8")
        compatibility = source_root / "agent-plugins/shaft-skills/COMPATIBILITY.md"
        compatibility.write_text("# Test compatibility\n", encoding="utf-8")
        manifest = source_root / "agent-plugins/release.json"
        manifest.write_text(
            '{"packages":[{"name":"act-as-mohab","version":"1.0.0"},'
            '{"name":"shaft-skills","version":"1.2.3"}]}\n',
            encoding="utf-8",
        )
        git = shutil.which("git")
        self.assertIsNotNone(git)
        subprocess.run([git, "init", "--quiet"], cwd=source_root, check=True)  # nosec B603
        subprocess.run([git, "add", "."], cwd=source_root, check=True)  # nosec B603
        assemble(source_root, self.package_root)

        for relative in ("plugin.json", ".claude-plugin/plugin.json", ".codex-plugin/plugin.json"):
            manifest = json.loads((self.package_root / relative).read_text(encoding="utf-8"))
            self.assertEqual(manifest["version"], "1.2.3")
        self.assertEqual((self.package_root / "LICENSE").read_text(encoding="utf-8"), (source_root / "LICENSE").read_text(encoding="utf-8"))
        self.assertEqual(
            (self.package_root / "CHANGELOG.md").read_text(encoding="utf-8"),
            (source_root / "agent-plugins/shaft-skills/CHANGELOG.md").read_text(encoding="utf-8"),
        )
        self.assertEqual(
            (self.package_root / "COMPATIBILITY.md").read_text(encoding="utf-8"),
            (source_root / "agent-plugins/shaft-skills/COMPATIBILITY.md").read_text(encoding="utf-8"),
        )

    def test_assembly_is_deterministic_and_ignores_untracked_sources(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        untracked = CANONICAL_SKILLS / "session-notes.md"
        untracked.write_text("host state", encoding="utf-8")
        self.addCleanup(untracked.unlink)
        second_root = self.package_root.parent / "second"

        assemble(ROOT, self.package_root)
        assemble(ROOT, second_root)

        self.assertFalse((self.package_root / "skills/session-notes.md").exists())
        first = sorted(path.relative_to(self.package_root) for path in self.package_root.rglob("*") if path.is_file())
        second = sorted(path.relative_to(second_root) for path in second_root.rglob("*") if path.is_file())
        self.assertEqual(first, second)
        for path in first:
            self.assertEqual((self.package_root / path).read_bytes(), (second_root / path).read_bytes(), path)

    def test_assembly_keeps_every_relative_markdown_link_inside_the_package(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        assemble(ROOT, self.package_root)

        for document in (self.package_root / "skills").rglob("*.md"):
            for target in MARKDOWN_LINK.findall(document.read_text(encoding="utf-8")):
                if "://" in target or target.startswith("mailto:"):
                    continue
                resolved = (document.parent / target).resolve()
                self.assertTrue(resolved.is_relative_to(self.package_root.resolve()), (document, target))
                self.assertTrue(resolved.exists(), (document, target))

    def test_assembly_rejects_output_inside_canonical_sources(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        output = CANONICAL_SKILLS / "assembled-output"

        with self.assertRaises(ValueError):
            assemble(ROOT, output)

        self.assertFalse(output.exists())

    def test_assembly_rejects_an_untracked_release_file(self):
        git = shutil.which("git")
        self.assertIsNotNone(git, "Git is required for portable assembly")
        source_root = self.package_root.parent / "source"
        skill = source_root / "shaft-skills/example/SKILL.md"
        skill.parent.mkdir(parents=True)
        skill.write_text("---\nname: example\ndescription: Example.\n---\n", encoding="utf-8")
        (source_root / "LICENSE").write_text("test license\n", encoding="utf-8")
        changelog = source_root / "agent-plugins/shaft-skills/CHANGELOG.md"
        changelog.parent.mkdir(parents=True)
        changelog.write_text("# Untracked\n", encoding="utf-8")
        subprocess.run([git, "init", "--quiet"], cwd=source_root, check=True)  # nosec B603
        manifest = source_root / "agent-plugins/release.json"
        manifest.write_text(
            '{"packages":[{"name":"act-as-mohab","version":"1.0.0"},'
            '{"name":"shaft-skills","version":"1.0.0"}]}\n',
            encoding="utf-8",
        )
        subprocess.run([git, "add", "shaft-skills", "LICENSE", "agent-plugins/release.json"], cwd=source_root, check=True)  # nosec B603

        with self.assertRaisesRegex(ValueError, "tracked"):
            assemble(source_root, self.package_root)

    def test_assembly_rejects_a_symlinked_release_file(self):
        git = shutil.which("git")
        self.assertIsNotNone(git, "Git is required for portable assembly")
        source_root = self.package_root.parent / "source"
        skill = source_root / "shaft-skills/example/SKILL.md"
        skill.parent.mkdir(parents=True)
        skill.write_text("---\nname: example\ndescription: Example.\n---\n", encoding="utf-8")
        (source_root / "LICENSE").write_text("test license\n", encoding="utf-8")
        changelog = source_root / "agent-plugins/shaft-skills/CHANGELOG.md"
        changelog.parent.mkdir(parents=True)
        private = source_root / "private-changelog.md"
        private.write_text("private release note\n", encoding="utf-8")
        try:
            changelog.symlink_to(private)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")
        manifest = source_root / "agent-plugins/release.json"
        manifest.write_text(
            '{"packages":[{"name":"act-as-mohab","version":"1.0.0"},'
            '{"name":"shaft-skills","version":"1.0.0"}]}\n',
            encoding="utf-8",
        )
        subprocess.run([git, "init", "--quiet"], cwd=source_root, check=True)  # nosec B603
        subprocess.run([git, "add", "shaft-skills", "LICENSE", "agent-plugins"], cwd=source_root, check=True)  # nosec B603

        with self.assertRaisesRegex(ValueError, "symlink"):
            assemble(source_root, self.package_root)


if __name__ == "__main__":
    unittest.main()
