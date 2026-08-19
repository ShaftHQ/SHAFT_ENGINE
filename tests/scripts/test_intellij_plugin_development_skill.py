"""Structural contract for the IntelliJ plugin-development skill (#5247).

The plugin-development surface is a light wrapper around the existing mastery
chapter: adapters stay thin, the playbook owns Marketplace and installer-exec
rules, and routing sends IntelliJ plugin work there instead of duplicating
incident history.
"""

from __future__ import annotations

import json
import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SKILL_NAME = "intellij-plugin-development"
ADAPTER = ROOT / ".agents/skills" / SKILL_NAME / "SKILL.md"
PLAYBOOK = (
    ROOT
    / "chaos-engine/profiles/shaft/references/playbooks"
    / f"{SKILL_NAME}.md"
)
MASTERY = ROOT / "chaos-engine/profiles/shaft/references/shaft-mastery/intellij-plugin.md"
ROUTING = ROOT / "chaos-engine/profiles/shaft/references/routing.md"
BUDGET = ROOT / "scripts/ci/agent_guidance_budget.json"
SKILLS_MAP = ROOT / ".agents/skills/README.md"

JETBRAINS_PLUGIN_DOC = "https://plugins.jetbrains.com/docs/intellij/developing-plugins.html"
MARKETPLACE_DOC = (
    "https://plugins.jetbrains.com/docs/marketplace/"
    "jetbrains-marketplace-approval-guidelines.html"
)
MASTERY_ONLY_TOKENS = (
    "LookAndFeelIsolationExtension",
    "getEmptyText()",
    "OpenJDK 25.0.1",
)


def routing_row_for(deliverable_needle: str) -> tuple[str, str]:
    """Return the (deliverable, target) routing row matching needle."""
    for line in ROUTING.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line.startswith("|") or set(line) <= set("|- "):
            continue
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        if len(cells) != 2:
            continue
        if cells[0].lower() in {"deliverable in front of you", "task touches"}:
            continue
        if deliverable_needle.lower() in cells[0].lower():
            return cells[0], cells[1]
    raise AssertionError(f"no routing row mentions {deliverable_needle!r}")


def markdown_link_targets(markdown: str) -> list[str]:
    """Return hrefs from markdown links, ignoring images."""
    return re.findall(r"(?<!!)\[[^]]*\]\(([^)]+)\)", markdown)


class IntelliJPluginDevelopmentSkillTest(unittest.TestCase):
    def test_playbook_exists_without_a_first_level_agents_adapter(self) -> None:
        self.assertFalse(ADAPTER.is_file(), f"Codex must not discover {ADAPTER}")
        self.assertFalse(
            ADAPTER.parent.is_dir(),
            f"first-level skill directory must not exist: {ADAPTER.parent}",
        )
        self.assertTrue(PLAYBOOK.is_file(), f"missing playbook {PLAYBOOK}")

    def test_intellij_routing_row_loads_the_playbook_not_mastery_alone(self) -> None:
        _, target = routing_row_for("The IntelliJ plugin")
        match = re.search(r"\[[^]]+\]\(([^)]+)\)", target)
        self.assertIsNotNone(match, f"row target is not a link: {target}")
        first_href = match.group(1).split("#", 1)[0]
        self.assertEqual(
            first_href,
            "playbooks/intellij-plugin-development.md",
            "IntelliJ row must load the plugin-development playbook first",
        )
        self.assertIn("shaft-mastery/intellij-plugin.md", ROUTING.read_text(encoding="utf-8"))

    def test_playbook_cites_jetbrains_docs_forbids_installer_exec_and_requires_screenshots(
        self,
    ) -> None:
        self.assertTrue(PLAYBOOK.is_file(), f"missing playbook {PLAYBOOK}")
        body = PLAYBOOK.read_text(encoding="utf-8")
        self.assertIn(JETBRAINS_PLUGIN_DOC, body)
        self.assertIn(MARKETPLACE_DOC, body)
        self.assertIn("shaft-mastery/intellij-plugin.md", body)
        self.assertIn("ProcessBuilder", body)
        self.assertRegex(
            body,
            r"(?i)never .{0,80}(execut(?:e|ing)|ProcessBuilder|Runtime\.exec).{0,80}install",
        )
        self.assertIn("ShaftPluginScreenshotRendererTest", body)
        self.assertIn("-Dshaft.intellij.screenshotDir", body)
        self.assertIn("-Dallure.automaticallyOpen=false", body)
        self.assertRegex(body, r"(?i)never.{0,40}bare.{0,20}(full[- ]suite|gradlew test)")
        for token in MASTERY_ONLY_TOKENS:
            with self.subTest(token=token):
                self.assertNotIn(token, body)
                self.assertIn(token, MASTERY.read_text(encoding="utf-8"))

    def test_expected_skill_names_exclude_adapter_and_map_lists_playbook(self) -> None:
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        self.assertNotIn(
            SKILL_NAME,
            budget["expected_skill_names"][".agents/skills"],
        )
        map_text = SKILLS_MAP.read_text(encoding="utf-8")
        self.assertNotIn(f".agents/skills/{SKILL_NAME}/SKILL.md", map_text)
        self.assertIn(f"playbooks/{SKILL_NAME}.md", map_text)


if __name__ == "__main__":
    unittest.main()
