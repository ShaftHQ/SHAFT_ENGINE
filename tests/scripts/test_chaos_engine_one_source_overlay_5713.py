"""One source-controlled harness tree; generated overlay; origin gitignore (#5713)."""

from __future__ import annotations

import importlib.util
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine/hosts.py"
INSTALL = ROOT / "chaos-engine/install.py"
INVENTORY = ROOT / "scripts/ci/skill_inventory.py"
OVERLAY_TEMP = ROOT / "scripts/ci/overlay_in_temp.py"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    if specification is None or specification.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class OneSourceOverlayTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.hosts = load(HOSTS, "ce_hosts_5713")
        cls.inventory = load(INVENTORY, "ce_skill_inventory_5713")
        cls.overlay = load(OVERLAY_TEMP, "ce_overlay_temp_5713")

    def test_adopter_gitignore_force_includes_harness(self):
        text = self.hosts.gitignore_content(None).decode("utf-8")
        self.assertIn("!.chaos-engine/", text)
        self.assertIn("!.agents/", text)
        self.assertIn(self.hosts.GITIGNORE_START, text)
        self.assertIn(self.hosts.GITIGNORE_END, text)
        self.assertNotIn(self.hosts.ORIGIN_OVERLAY_START, text)

    def test_installer_preserves_origin_extra_ignore_after_ce_block(self):
        extra = (
            f"{self.hosts.ORIGIN_OVERLAY_START}\n"
            ".chaos-engine/\n"
            f"{self.hosts.ORIGIN_OVERLAY_END}\n"
        )
        before = self.hosts.gitignore_content(None) + extra.encode()
        after = self.hosts.gitignore_content(before).decode("utf-8")
        self.assertIn(self.hosts.ORIGIN_OVERLAY_START, after)
        self.assertGreater(
            after.index(self.hosts.ORIGIN_OVERLAY_START),
            after.index(self.hosts.GITIGNORE_END),
        )
        self.assertIn(".chaos-engine/", after.split(self.hosts.GITIGNORE_END, 1)[1])

    def test_origin_gitignore_extra_ignore_is_after_ce_block(self):
        text = (ROOT / ".gitignore").read_text(encoding="utf-8")
        self.assertIn(self.hosts.ORIGIN_OVERLAY_START, text)
        self.assertGreater(
            text.index(self.hosts.ORIGIN_OVERLAY_START),
            text.index(self.hosts.GITIGNORE_END),
        )

    def test_origin_does_not_track_generated_overlay(self):
        result = subprocess.run(
            ["git", "ls-files", "--", ".chaos-engine", ".agents", ".claude",
             ".claude-plugin", ".codex", ".gemini", ".grok/hooks",
             ".github/skills", ".github/hooks", "plugins/chaos-engine",
             "plugins/caveman", "plugins/ponytail", "agent-plugins/chaos-engine"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
        tracked = [line for line in result.stdout.splitlines() if line]
        allowed = {
            "agent-plugins/chaos-engine/CHANGELOG.md",
            "agent-plugins/chaos-engine/COMPATIBILITY.md",
            ".agents/skills/README.md",
            ".claude-plugin/marketplace.json",
        }
        self.assertEqual(set(tracked) - allowed, set())

    def test_marker_bodies_are_policy_identical(self):
        errors = self.hosts.competing_policy_errors(ROOT)
        self.assertEqual(errors, [])

    def test_claude_md_has_no_graphify_essay(self):
        text = (ROOT / "CLAUDE.md").read_text(encoding="utf-8")
        self.assertNotIn("graphify-out/", text)
        self.assertIn(self.hosts.START, text)

    def test_skill_inventory_passes_on_source_tree(self):
        errors = self.inventory.validate_skill_inventory(ROOT)
        self.assertEqual(errors, [])

    def test_skill_inventory_fails_orphan_and_divergent(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            skill = root / "chaos-engine/skills/chaos-engine/SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_text("canonical\n", encoding="utf-8")
            overlay = root / ".chaos-engine/skills/chaos-engine/SKILL.md"
            overlay.parent.mkdir(parents=True)
            overlay.write_text("divergent\n", encoding="utf-8")
            orphan = root / ".agents/skills/orphan-skill/SKILL.md"
            orphan.parent.mkdir(parents=True)
            orphan.write_text("orphan\n", encoding="utf-8")
            plugin = root / "plugins/chaos-engine/skills/chaos-engine/SKILL.md"
            plugin.parent.mkdir(parents=True)
            plugin.write_text("plugin-divergent\n", encoding="utf-8")
            merged = root / "chaos-engine/shaft-skills/SKILL.md"
            merged.parent.mkdir(parents=True)
            merged.write_text("merged\n", encoding="utf-8")
            errors = self.inventory.validate_skill_inventory(root)
            codes = {item["code"] for item in errors}
            self.assertIn("skill-body-divergent", codes)
            self.assertIn("skill-orphan", codes)
            self.assertIn("skill-product-pack", codes)

    def test_ensure_overlay_does_not_mutate_origin_readme(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            readme = root / ".agents/skills/README.md"
            readme.parent.mkdir(parents=True)
            readme.write_text("origin harness map\n", encoding="utf-8")
            overlay_root = self.overlay.ensure_overlay(root)
            self.assertNotEqual(overlay_root, root)
            self.assertFalse((root / ".agents/skills/chaos-engine/SKILL.md").is_file())
            self.assertEqual(readme.read_text(encoding="utf-8"), "origin harness map\n")
            self.assertTrue((overlay_root / ".agents/skills/chaos-engine/SKILL.md").is_file())
            self.overlay.cleanup_overlay(overlay_root)

    def test_empty_project_tracks_generated_overlay(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "empty"
            project.mkdir()
            subprocess.run(["git", "init"], cwd=project, check=True, capture_output=True)
            installer = load(INSTALL, "ce_install_5713")
            installer.install_with_dependencies(
                project,
                ROOT / "chaos-engine",
                "0" * 40,
                provisioner=lambda *_a, **_k: None,
            )
            ignored = subprocess.run(
                ["git", "check-ignore", "-v", ".chaos-engine/skills/chaos-engine/SKILL.md"],
                cwd=project,
                capture_output=True,
                text=True,
            )
            detail = ignored.stdout + ignored.stderr
            self.assertTrue(
                ignored.returncode != 0 or "!.chaos-engine" in detail,
                detail,
            )
            subprocess.run(["git", "add", "-A"], cwd=project, check=True, capture_output=True)
            listed = subprocess.run(
                ["git", "ls-files"],
                cwd=project,
                check=True,
                capture_output=True,
                text=True,
            )
            for relative in (
                ".chaos-engine/skills/chaos-engine/SKILL.md",
                ".agents/skills/chaos-engine/SKILL.md",
                ".claude/skills/chaos-engine/SKILL.md",
            ):
                self.assertIn(relative, listed.stdout, listed.stdout)
            gitignore = (project / ".gitignore").read_text(encoding="utf-8")
            self.assertNotIn(self.hosts.ORIGIN_OVERLAY_START, gitignore)

    def test_overlay_in_temp_materializes_skill(self):
        overlay_root = self.overlay.materialize_overlay(ROOT, copy_pom=True)
        try:
            skill = overlay_root / ".chaos-engine/skills/chaos-engine/SKILL.md"
            self.assertTrue(skill.is_file(), skill)
            mcp = (overlay_root / ".mcp.json").read_text(encoding="utf-8")
            self.assertIn("maven-tools-mcp", mcp)
            self.assertTrue((overlay_root / "pom.xml").is_file())
        finally:
            self.overlay.cleanup_overlay(overlay_root)

    def test_extra_load_outside_marker_is_competing_policy(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            block = self.hosts.instruction_block(".chaos-engine")
            (project / "AGENTS.md").write_text(block, encoding="utf-8")
            (project / "CLAUDE.md").write_text(
                "Load [ChaosEngine](.claude/skills/chaos-engine/SKILL.md)\n" + block,
                encoding="utf-8",
            )
            (project / "GEMINI.md").write_text(block, encoding="utf-8")
            errors = self.hosts.competing_policy_errors(project)
            self.assertTrue(any("extra ChaosEngine Load" in item for item in errors))

    def test_install_docs_path_table(self):
        text = (ROOT / "chaos-engine/INSTALL.md").read_text(encoding="utf-8")
        self.assertIn("Source vs generated paths", text)
        for name in (
            "chaos-engine/",
            ".chaos-engine/",
            ".agents/",
            ".claude/",
            ".codex/",
            ".gemini/",
            ".grok/",
            ".github/skills/",
            "plugins/chaos-engine/",
            "AGENTS.md",
            "CLAUDE.md",
            "GEMINI.md",
        ):
            self.assertIn(name, text)

    def test_kanban_is_default_method(self):
        text = (ROOT / "chaos-engine/references/process-owner-scrum-master.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("Kanban", text)
        self.assertIn("process-owner is the role name", text.casefold())
        self.assertIn("alias only when the user explicitly asks", text.casefold())
        self.assertNotIn("is the process owner and Scrum-master", text)

    def test_root_pom_still_is_reactor_aggregator(self):
        text = (ROOT / "pom.xml").read_text(encoding="utf-8")
        self.assertIn("<packaging>pom</packaging>", text)
        self.assertIn("reactor aggregator", text)


if __name__ == "__main__":
    unittest.main()
