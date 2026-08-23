"""Portable act-as-mohab package assembly tests (#4576)."""

import json
import inspect
import re
import subprocess
import tempfile
import unittest
import xml.etree.ElementTree as ET
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
CANONICAL_ROOT = ROOT / "chaos-engine"
MARKDOWN_LINK = re.compile(r"\]\(([^)#?]+)")
ENGINE_VERSION = ET.parse(ROOT / "pom.xml").getroot().findtext(
    "{http://maven.apache.org/POM/4.0.0}version"
)


class AssembleActAsMohabPluginTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.package_root = Path(self.temporary_directory.name) / "act-as-mohab"

    def tearDown(self):
        self.temporary_directory.cleanup()

    def create_source_repository(self, source_root: Path) -> Path:
        canonical_root = source_root / "chaos-engine"
        target = canonical_root / "skills/chaos-engine/SKILL.md"
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(
            "---\nname: chaos-engine\ndescription: Use when testing assembly.\n---\n"
            "\n[references](../../references/roles.md)\n",
            encoding="utf-8",
        )
        references = canonical_root / "references"
        references.mkdir()
        (references / "roles.md").write_text("# Roles\n", encoding="utf-8")
        profile = canonical_root / "profiles/shaft"
        profile.mkdir(parents=True)
        (profile / "entrypoint.md").write_text("# Test profile\n", encoding="utf-8")
        (profile / "profile.json").write_text('{"schemaVersion":1}\n', encoding="utf-8")
        (source_root / "LICENSE").write_text("test license\n", encoding="utf-8")
        changelog = source_root / "agent-plugins/act-as-mohab/CHANGELOG.md"
        changelog.parent.mkdir(parents=True)
        changelog.write_text("# Test changelog\n", encoding="utf-8")
        compatibility = source_root / "agent-plugins/act-as-mohab/COMPATIBILITY.md"
        compatibility.write_text("# Test compatibility\n", encoding="utf-8")
        runtime_sources = source_root / "scripts/agents"
        runtime_sources.mkdir(parents=True)
        for name in (
            "act_as_mohab_cli.py", "delivery_status.py", "github_client.py", "issue_filing.py", "planning_contract.py", "pr_audit.py",
            "repository_context.py", "watch_pr_checks.py",
        ):
            (runtime_sources / name).write_text("# fixture canonical runtime\n", encoding="utf-8")
        manifest = source_root / "agent-plugins/release.json"
        manifest.write_text(
            '{"packages":[{"name":"act-as-mohab","version":"1.0.0"},'
            '{"name":"shaft-skills","version":"1.0.0"}]}\n',
            encoding="utf-8",
        )
        (source_root / "pom.xml").write_text(
            '<project xmlns="http://maven.apache.org/POM/4.0.0">'
            '<modelVersion>4.0.0</modelVersion><version>1.0.0</version></project>',
            encoding="utf-8",
        )
        subprocess.run([git_executable(), "init", "--quiet"], cwd=source_root, check=True)  # nosec B603
        subprocess.run(
            [git_executable(), "add", "chaos-engine", "LICENSE", "agent-plugins", "scripts/agents"],
            cwd=source_root,
            check=True,
        )  # nosec B603
        return canonical_root

    def test_discovery_content_is_bound_to_engine_version(self):
        version = ENGINE_VERSION
        self.assertEqual(version, ENGINE_VERSION)
        changelog = (ROOT / "agent-plugins/act-as-mohab/CHANGELOG.md").read_text(encoding="utf-8")
        compatibility = (ROOT / "agent-plugins/act-as-mohab/COMPATIBILITY.md").read_text(encoding="utf-8")
        self.assertIn(f"## {ENGINE_VERSION}", changelog)
        self.assertIn("breaking", changelog.lower())
        self.assertIn(ENGINE_VERSION, compatibility)

        assemble(ROOT, self.package_root)
        packaged_skills = {
            path.name for path in (self.package_root / "skills").iterdir()
            if (path / "SKILL.md").is_file()
        }
        self.assertEqual(
            packaged_skills,
            {"act-as-mohab", "chaos-engine", "local-coding-delegate", "work-item"},
        )
        for relative in ("plugin.json", ".claude-plugin/plugin.json", ".codex-plugin/plugin.json"):
            manifest = json.loads((self.package_root / relative).read_text(encoding="utf-8"))
            self.assertEqual(manifest["version"], version)

    def test_assembly_creates_a_valid_self_contained_package_from_canonical_sources(self):
        self.assertTrue(callable(assemble), "assemble must be available")
        assemble(ROOT, self.package_root)

        self.assertEqual(validate_package(self.package_root), [])
        self.assertEqual(
            json.loads((self.package_root / "plugin.json").read_text(encoding="utf-8")),
            {
                "$schema": "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json",
                "name": "act-as-mohab",
                "version": ENGINE_VERSION,
                "description": "Maintainer workflow and harness skills for SHAFT.",
                "author": {"name": "ShaftHQ", "url": "https://github.com/ShaftHQ/SHAFT_ENGINE"},
                "repository": "https://github.com/ShaftHQ/SHAFT_ENGINE",
                "license": "MIT",
            },
        )
        packaged_core = (self.package_root / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        canonical_core = (CANONICAL_ROOT / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertTrue(packaged_core.startswith(canonical_core.rstrip()))
        self.assertIn("../../profiles/shaft/entrypoint.md", packaged_core)
        alias = (self.package_root / "skills/act-as-mohab/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("../chaos-engine/SKILL.md", alias)
        self.assertIn("../../profiles/shaft/entrypoint.md", alias)
        self.assertFalse((self.package_root / "skills/consult-first").exists())
        self.assertFalse((self.package_root / "skills/retrieve-first").exists())
        canonical_references = CANONICAL_ROOT / "references"
        for source in sorted(tracked_source_files(ROOT)):
            if source.is_relative_to(canonical_references) and source.suffix in {".md", ".LICENSE"}:
                target = self.package_root / "references" / source.relative_to(canonical_references)
                self.assertEqual(target.read_bytes(), source.read_bytes(), target)
        self.assertFalse((self.package_root / "skills/act-as-mohab/agents").exists())

    def test_assembly_includes_a_claude_discovery_manifest(self):
        assemble(ROOT, self.package_root)

        manifest_path = self.package_root / ".claude-plugin/plugin.json"
        self.assertTrue(manifest_path.is_file())
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))

        self.assertEqual(manifest["name"], "act-as-mohab")
        self.assertEqual(manifest["version"], ENGINE_VERSION)
        self.assertIn("maintainer", manifest["description"].lower())
        self.assertEqual(manifest.get("author", {}).get("name"), "ShaftHQ")
        marketplace = json.loads(
            (self.package_root / ".claude-plugin/marketplace.json").read_text(
                encoding="utf-8"
            )
        )
        self.assertEqual(marketplace["name"], "act-as-mohab")
        self.assertEqual(marketplace["plugins"][0]["source"], "./")

    def test_assembly_includes_a_codex_discovery_manifest(self):
        assemble(ROOT, self.package_root)

        manifest_path = self.package_root / ".codex-plugin/plugin.json"
        self.assertTrue(manifest_path.is_file())
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))

        self.assertEqual(manifest["name"], "act-as-mohab")
        self.assertEqual(manifest["version"], ENGINE_VERSION)
        self.assertEqual(manifest["skills"], "./skills/")
        self.assertEqual(manifest["mcpServers"], "./.mcp.json")

    def test_assembly_includes_the_relative_mcp_runtime_contract(self):
        assemble(ROOT, self.package_root)

        mcp = json.loads((self.package_root / ".mcp.json").read_text(encoding="utf-8"))
        self.assertEqual(
            mcp,
            {
                "mcpServers": {
                    "chaosengine": {
                        "command": "python",
                        "args": ["./bin/act-as-mohab.pyz", "mcp"],
                        "cwd": ".",
                    }
                }
            },
        )
        self.assertTrue((self.package_root / "bin/act-as-mohab.pyz").is_file())

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

    def test_assembly_uses_the_declared_release_version_and_release_files(self):
        self.assertIn("version", inspect.signature(assemble).parameters)
        source_root = self.package_root.parent / "source"
        self.create_source_repository(source_root)
        manifest = source_root / "agent-plugins/release.json"
        manifest.write_text(
            '{"packages":[{"name":"act-as-mohab","version":"1.2.3"},'
            '{"name":"shaft-skills","version":"1.2.3"}]}\n',
            encoding="utf-8",
        )
        (source_root / "pom.xml").write_text(
            '<project xmlns="http://maven.apache.org/POM/4.0.0">'
            '<modelVersion>4.0.0</modelVersion><version>1.2.3</version></project>',
            encoding="utf-8",
        )
        subprocess.run([git_executable(), "add", "agent-plugins/release.json", "pom.xml"], cwd=source_root, check=True)  # nosec B603
        assemble(source_root, self.package_root)

        for relative in ("plugin.json", ".claude-plugin/plugin.json", ".codex-plugin/plugin.json"):
            manifest = json.loads((self.package_root / relative).read_text(encoding="utf-8"))
            self.assertEqual(manifest["version"], "1.2.3")
        self.assertEqual((self.package_root / "LICENSE").read_text(encoding="utf-8"), (source_root / "LICENSE").read_text(encoding="utf-8"))
        self.assertEqual(
            (self.package_root / "CHANGELOG.md").read_text(encoding="utf-8"),
            (source_root / "agent-plugins/act-as-mohab/CHANGELOG.md").read_text(encoding="utf-8"),
        )
        self.assertEqual(
            (self.package_root / "COMPATIBILITY.md").read_text(encoding="utf-8"),
            (source_root / "agent-plugins/act-as-mohab/COMPATIBILITY.md").read_text(encoding="utf-8"),
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
        canonical_root = self.create_source_repository(source_root)
        source_references = canonical_root / "references"
        outside = self.package_root.parent / "outside.md"
        outside.write_text("host secret", encoding="utf-8")
        (source_references / "session-token.json").write_text("host secret", encoding="utf-8")
        try:
            (source_references / "leak.md").symlink_to(outside)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        assemble(source_root, self.package_root)

        self.assertFalse((self.package_root / "references/leak.md").exists())
        self.assertFalse((self.package_root / "references/session-token.json").exists())

    def test_assembly_ignores_untracked_markdown_source_files(self):
        source_root = self.package_root.parent / "source"
        canonical_root = self.create_source_repository(source_root)
        notes = canonical_root / "references/session-notes.md"
        notes.write_text("host secret", encoding="utf-8")

        assemble(source_root, self.package_root)

        self.assertFalse((self.package_root / "references/session-notes.md").exists())

    def test_assembly_rejects_a_symlinked_canonical_skill(self):
        source_root = self.package_root.parent / "source"
        canonical_root = self.create_source_repository(source_root)
        outside = self.package_root.parent / "outside.md"
        outside.write_text("host secret", encoding="utf-8")
        skill_path = canonical_root / "skills/chaos-engine/SKILL.md"
        skill_path.unlink()
        try:
            skill_path.symlink_to(outside)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        with self.assertRaises(ValueError):
            assemble(source_root, self.package_root)

    def test_assembly_rejects_a_symlinked_release_file(self):
        source_root = self.package_root.parent / "source"
        self.create_source_repository(source_root)
        untracked = source_root / "private-changelog.md"
        untracked.write_text("private release note\n", encoding="utf-8")
        changelog = source_root / "agent-plugins/act-as-mohab/CHANGELOG.md"
        changelog.unlink()
        try:
            changelog.symlink_to(untracked)
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        with self.assertRaisesRegex(ValueError, "symlink"):
            assemble(source_root, self.package_root)

    def test_assembly_rejects_a_symlinked_canonical_skill_directory(self):
        source_root = self.package_root.parent / "source"
        canonical_root = self.create_source_repository(source_root)
        outside = self.package_root.parent / "outside"
        outside.mkdir()
        (outside / "SKILL.md").write_text("host secret", encoding="utf-8")
        skill_directory = canonical_root / "skills/chaos-engine"
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
        canonical_root = self.create_source_repository(source_root)
        references = canonical_root / "references"
        (references / "roles.md").unlink()
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
        canonical_root = self.create_source_repository(source_root)
        output = canonical_root / "references/assembled-output"

        with self.assertRaises(ValueError):
            assemble(source_root, output)

        self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
