"""Generated host pointers always name the installed overlay (#5727)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine/hosts.py"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    if specification is None or specification.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class OverlayOnlyHostPointerTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.hosts = load(HOSTS, "ce_hosts_5727")

    def test_guidance_tree_is_overlay_on_origin_checkout(self):
        self.assertEqual(".chaos-engine", self.hosts.guidance_tree(ROOT))
        self.assertEqual(".chaos-engine", self.hosts.guidance_tree(None))

    def test_instruction_block_ignores_source_tree_name(self):
        block = self.hosts.instruction_block("chaos-engine")
        self.assertIn(".chaos-engine/skills/chaos-engine/SKILL.md", block)
        self.assertIn("`.chaos-engine/tool.py`", block)
        self.assertNotIn("(chaos-engine/", block)
        self.assertNotIn("`chaos-engine/tool.py`", block)

    def test_copilot_rewrite_does_not_mangle_nested_skill_segment(self):
        block = self.hosts.copilot_instruction_block()
        self.assertIn(
            "../.chaos-engine/skills/chaos-engine/SKILL.md",
            block,
        )
        self.assertIn("`../.chaos-engine/tool.py`", block)
        self.assertNotIn("skills/../chaos-engine", block)
        self.assertNotIn("../chaos-engine/", block.replace("../.chaos-engine/", ""))

    def test_origin_style_markers_share_one_normalized_policy(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            agents = self.hosts.instruction_block()
            copilot = self.hosts.copilot_instruction_block()
            (project / "AGENTS.md").write_text(agents, encoding="utf-8")
            (project / "CLAUDE.md").write_text(agents, encoding="utf-8")
            (project / "GEMINI.md").write_text(agents, encoding="utf-8")
            github = project / ".github"
            github.mkdir()
            (github / "copilot-instructions.md").write_text(copilot, encoding="utf-8")
            self.assertEqual([], self.hosts.competing_policy_errors(project))

    def test_desired_content_on_source_checkout_writes_overlay_pointers(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            skill = project / "chaos-engine/skills/chaos-engine/SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_text("canonical\n", encoding="utf-8")
            profile = project / "chaos-engine/profiles/shaft/entrypoint.md"
            profile.parent.mkdir(parents=True)
            profile.write_text("profile\n", encoding="utf-8")
            before = {key: None for key in self.hosts.managed_paths()}
            after = self.hosts.desired_content(
                before,
                maven_runtime=None,
                project_name="probe",
                project=project,
            )
            agents = after["AGENTS.md"].decode("utf-8")
            copilot = after[".github/copilot-instructions.md"].decode("utf-8")
            adapter = after[".agents/skills/chaos-engine/SKILL.md"].decode("utf-8")
            self.assertIn(".chaos-engine/skills/chaos-engine/SKILL.md", agents)
            self.assertNotIn("(chaos-engine/", agents)
            self.assertIn("../.chaos-engine/skills/chaos-engine/SKILL.md", copilot)
            self.assertNotIn("skills/../chaos-engine", copilot)
            self.assertIn("../../../.chaos-engine/skills/chaos-engine/SKILL.md", adapter)
            self.assertIn("profiles/shaft/entrypoint.md", adapter)


if __name__ == "__main__":
    unittest.main()
