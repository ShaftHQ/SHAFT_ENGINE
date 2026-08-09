"""Portable act-as-mohab package assembly tests (#4576)."""

import json
import re
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

try:
    from scripts.ci.assemble_act_as_mohab_plugin import assemble, git_executable, tracked_source_files
except ModuleNotFoundError:
    assemble = None
    git_executable = None
    tracked_source_files = None

from scripts.ci.validate_agent_plugins import validate_package


ROOT = Path(__file__).resolve().parents[2]
CANONICAL_SKILLS = ROOT / ".agents/skills"
MARKDOWN_LINK = re.compile(r"\]\(([^)#?]+)")


class AssembleActAsMohabPluginTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.package_root = Path(self.temporary_directory.name) / "act-as-mohab"

    def tearDown(self):
        self.temporary_directory.cleanup()

    def create_source_repository(self, source_root: Path) -> Path:
        source_skills = source_root / ".agents/skills"
        for skill in ("act-as-mohab", "consult-first", "retrieve-first"):
            target = source_skills / skill / "SKILL.md"
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(
                "---\nname: " + skill + "\ndescription: Use when testing assembly.\n---\n",
                encoding="utf-8",
            )
        (source_skills / "act-as-mohab/references").mkdir()
        subprocess.run([git_executable(), "init", "--quiet"], cwd=source_root, check=True)  # nosec B603
        subprocess.run([git_executable(), "add", ".agents/skills"], cwd=source_root, check=True)  # nosec B603
        return source_skills

    def test_assembly_creates_a_valid_self_contained_package_from_canonical_sources(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        assemble(ROOT, self.package_root)

        self.assertEqual(validate_package(self.package_root), [])
        self.assertEqual(
            json.loads((self.package_root / "plugin.json").read_text(encoding="utf-8")),
            {
                "$schema": "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json",
                "name": "act-as-mohab",
                "version": "1.0.0",
                "description": "Maintainer workflow and harness skills for SHAFT.",
                "author": {"name": "ShaftHQ", "url": "https://github.com/ShaftHQ/SHAFT_ENGINE"},
                "repository": "https://github.com/ShaftHQ/SHAFT_ENGINE",
                "license": "MIT",
            },
        )
        for skill in ("act-as-mohab", "consult-first", "retrieve-first"):
            self.assertEqual(
                (self.package_root / "skills" / skill / "SKILL.md").read_bytes(),
                (CANONICAL_SKILLS / skill / "SKILL.md").read_bytes(),
            )
        canonical_references = CANONICAL_SKILLS / "act-as-mohab/references"
        for source in sorted(tracked_source_files(ROOT)):
            if source.is_relative_to(canonical_references) and source.suffix in {".md", ".LICENSE"}:
                target = self.package_root / "skills/act-as-mohab/references" / source.relative_to(canonical_references)
                self.assertEqual(target.read_bytes(), source.read_bytes(), target)
        self.assertFalse((self.package_root / "skills/act-as-mohab/agents").exists())

    def test_assembly_includes_a_claude_discovery_manifest(self):
        assemble(ROOT, self.package_root)

        manifest_path = self.package_root / ".claude-plugin/plugin.json"
        self.assertTrue(manifest_path.is_file())
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))

        self.assertEqual(manifest["name"], "act-as-mohab")
        self.assertEqual(manifest["version"], "1.0.0")
        self.assertIn("maintainer", manifest["description"].lower())
        self.assertEqual(manifest.get("author", {}).get("name"), "ShaftHQ")

    def test_assembly_includes_a_codex_discovery_manifest(self):
        assemble(ROOT, self.package_root)

        manifest_path = self.package_root / ".codex-plugin/plugin.json"
        self.assertTrue(manifest_path.is_file())
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))

        self.assertEqual(manifest["name"], "act-as-mohab")
        self.assertEqual(manifest["version"], "1.0.0")
        self.assertEqual(manifest["skills"], "./skills/")

    def test_assembly_includes_a_codex_marketplace_entry(self):
        assemble(ROOT, self.package_root)

        marketplace_path = self.package_root / ".agents/plugins/marketplace.json"
        self.assertTrue(marketplace_path.is_file())
        marketplace = json.loads(marketplace_path.read_text(encoding="utf-8"))

        self.assertEqual(marketplace["name"], "act-as-mohab")
        self.assertEqual(
            marketplace["plugins"],
            [{"name": "act-as-mohab", "source": {"source": "local", "path": "./"}}],
        )

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

    def test_tracked_source_files_uses_a_resolved_git_executable(self):
        self.assertTrue(callable(tracked_source_files), "tracked_source_files must be available")
        with mock.patch("scripts.ci.assemble_act_as_mohab_plugin.subprocess.run") as run:
            run.return_value.stdout = b""

            tracked_source_files(ROOT)

        self.assertTrue(Path(run.call_args.args[0][0]).is_absolute())

    def test_assembly_keeps_every_relative_markdown_link_inside_the_package(self):
        assemble(ROOT, self.package_root)

        for document in self.package_root.rglob("*.md"):
            for target in MARKDOWN_LINK.findall(document.read_text(encoding="utf-8")):
                if "://" in target or target.startswith("mailto:"):
                    continue
                resolved = (document.parent / target).resolve()
                self.assertTrue(resolved.is_relative_to(self.package_root.resolve()), (document, target))
                self.assertTrue(resolved.exists(), (document, target))

    def test_assembly_skips_a_symlinked_source_file(self):
        source_root = self.package_root.parent / "source"
        source_skills = self.create_source_repository(source_root)
        source_references = source_skills / "act-as-mohab/references"
        outside = self.package_root.parent / "outside.md"
        outside.write_text("host secret", encoding="utf-8")
        (source_references / "session-token.json").write_text("host secret", encoding="utf-8")
        try:
            (source_references / "leak.md").symlink_to(outside)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        assemble(source_root, self.package_root)

        self.assertFalse((self.package_root / "skills/act-as-mohab/references/leak.md").exists())
        self.assertFalse((self.package_root / "skills/act-as-mohab/references/session-token.json").exists())

    def test_assembly_ignores_untracked_markdown_source_files(self):
        source_root = self.package_root.parent / "source"
        source_skills = self.create_source_repository(source_root)
        notes = source_skills / "act-as-mohab/references/session-notes.md"
        notes.write_text("host secret", encoding="utf-8")

        assemble(source_root, self.package_root)

        self.assertFalse((self.package_root / "skills/act-as-mohab/references/session-notes.md").exists())

    def test_assembly_rejects_a_symlinked_canonical_skill(self):
        source_root = self.package_root.parent / "source"
        source_skills = self.create_source_repository(source_root)
        outside = self.package_root.parent / "outside.md"
        outside.write_text("host secret", encoding="utf-8")
        skill_path = source_skills / "consult-first/SKILL.md"
        skill_path.unlink()
        try:
            skill_path.symlink_to(outside)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        with self.assertRaises(ValueError):
            assemble(source_root, self.package_root)

    def test_assembly_rejects_a_symlinked_canonical_skill_directory(self):
        source_root = self.package_root.parent / "source"
        source_skills = self.create_source_repository(source_root)
        outside = self.package_root.parent / "outside"
        outside.mkdir()
        (outside / "SKILL.md").write_text("host secret", encoding="utf-8")
        skill_directory = source_skills / "consult-first"
        (skill_directory / "SKILL.md").unlink()
        skill_directory.rmdir()
        try:
            skill_directory.symlink_to(outside, target_is_directory=True)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        with self.assertRaises(ValueError):
            assemble(source_root, self.package_root)

    def test_assembly_rejects_a_symlinked_reference_directory(self):
        source_root = self.package_root.parent / "source"
        source_skills = self.create_source_repository(source_root)
        references = source_skills / "act-as-mohab/references"
        references.rmdir()
        outside = self.package_root.parent / "outside"
        outside.mkdir()
        (outside / "host-notes.md").write_text("host secret", encoding="utf-8")
        try:
            references.symlink_to(outside, target_is_directory=True)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        with self.assertRaises(ValueError):
            assemble(source_root, self.package_root)

    def test_assembly_rejects_output_inside_its_canonical_sources(self):
        source_root = self.package_root.parent / "source"
        source_skills = self.create_source_repository(source_root)
        output = source_skills / "act-as-mohab/references/assembled-output"

        with self.assertRaises(ValueError):
            assemble(source_root, output)

        self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
