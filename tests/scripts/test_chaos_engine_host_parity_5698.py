"""Host parity #5698: one router, catalog, CLI-over-MCP, project-mode, hooks."""

from __future__ import annotations

import importlib.util
import json
import re
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
from scripts.ci.overlay_in_temp import session_overlay  # noqa: E402

OVERLAY = session_overlay(ROOT)
CANONICAL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
HOOK_MAP = ROOT / "chaos-engine/references/hook-trigger-map.md"
ROLES = ROOT / "chaos-engine/references/roles.md"
SKILLS_ROOT = ROOT / "chaos-engine/skills"
VENDOR_SKILLS = (
    ROOT / "chaos-engine/vendor/caveman/skills/caveman",
    ROOT / "chaos-engine/vendor/ponytail/skills/ponytail",
)

# Contract markers that must not be copied into thin adapters.
CONTRACT_MARKERS = (
    "## Iron laws",
    "### Ethical conduct",
    "EC1: Tell the truth",
    "## Operating contract",
)


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    if spec.loader is None:
        raise ImportError(f"unable to load {path}")
    spec.loader.exec_module(module)
    return module



def catalog_rows(skill_text: str) -> dict[str, dict[str, str]]:
    """Parse `| name | description | path |` rows under ## Catalog."""
    section = re.search(r"(?ms)^## Catalog\n(.*?)(?=^## |\Z)", skill_text)
    if section is None:
        raise ValueError("router missing ## Catalog")
    rows: dict[str, dict[str, str]] = {}
    for line in section.group(1).splitlines():
        if not line.startswith("|"):
            continue
        if re.match(r"^\|\s*name\s*\|", line, re.I) or line.startswith("| ---"):
            continue
        parts = [part.strip() for part in line.strip().strip("|").split("|")]
        if len(parts) < 3:
            continue
        name, description, path = parts[0], parts[1], parts[2]
        if name.casefold() in {"name", "----", ""}:
            continue
        rows[name] = {"description": description, "path": path}
    return rows


class HostParity5698Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = load(ROOT / "chaos-engine/mcp_policy.py", "ce_mcp_policy_5698")
        cls.hosts = load(ROOT / "chaos-engine/hosts.py", "ce_hosts_5698")
        cls.kernel = load(ROOT / "chaos-engine/hooks/kernel.py", "ce_kernel_5698")
        cls.skill = CANONICAL.read_text(encoding="utf-8")

    def test_router_catalog_lists_every_skill_and_role(self):
        rows = catalog_rows(self.skill)
        skill_dirs = sorted(
            path.parent.name
            for path in SKILLS_ROOT.glob("*/SKILL.md")
        )
        for name in skill_dirs:
            self.assertIn(name, rows, f"skill directory missing from catalog: {name}")
            self.assertTrue(rows[name]["description"], name)
            self.assertLessEqual(
                len(rows[name]["description"]),
                200,
                f"catalog description too long for host listing caps: {name}",
            )
            self.assertIn("skills/", rows[name]["path"])
        for vendor in VENDOR_SKILLS:
            name = vendor.name
            self.assertIn(name, rows, f"companion skill missing from catalog: {name}")
        role_headings = re.findall(r"(?m)^##\s+(.+)$", ROLES.read_text(encoding="utf-8"))
        for heading in role_headings:
            slug = re.sub(r"[^a-z0-9]+", "-", heading.strip().casefold()).strip("-")
            self.assertTrue(
                any(slug in key.casefold() or heading.casefold() in key.casefold() for key in rows),
                f"role missing from catalog: {heading}",
            )

    def test_companions_are_cataloged_not_body_loaded_by_default(self):
        lowered = self.skill.casefold()
        self.assertNotIn("load both companion skills at the start of every task", lowered)
        self.assertIn("must not load companion skill bodies by default", lowered)
        rows = catalog_rows(self.skill)
        self.assertIn("caveman", rows)
        self.assertIn("ponytail", rows)

    def test_host_adapters_and_roles_have_no_second_contract_copy(self):
        adapters = [
            ROOT / "AGENTS.md",
            ROOT / "CLAUDE.md",
            ROOT / "GEMINI.md",
            ROOT / ".github/copilot-instructions.md",
            OVERLAY / ".agents/skills/chaos-engine/SKILL.md",
            OVERLAY / ".claude/skills/chaos-engine/SKILL.md",
            OVERLAY / ".gemini/skills/chaos-engine/SKILL.md",
            OVERLAY / ".github/skills/chaos-engine/SKILL.md",
        ]
        adapters.extend(sorted((OVERLAY / ".claude/agents").glob("*.md")))
        adapters.extend(sorted((OVERLAY / ".codex/agents").glob("*.toml")))
        for path in adapters:
            text = path.read_text(encoding="utf-8")
            for marker in CONTRACT_MARKERS:
                with self.subTest(path=str(path), marker=marker):
                    self.assertNotIn(marker, text)

    def test_role_adapters_load_canonical_router_not_agents_pointer(self):
        for path in sorted((OVERLAY / ".claude/agents").glob("*.md")):
            text = path.read_text(encoding="utf-8")
            self.assertIn("chaos-engine/skills/chaos-engine/SKILL.md", text)
            self.assertNotIn(".agents/skills/chaos-engine/SKILL.md", text)
        for path in sorted((OVERLAY / ".codex/agents").glob("*.toml")):
            text = path.read_text(encoding="utf-8")
            self.assertIn("chaos-engine/skills/chaos-engine/SKILL.md", text)
            self.assertNotIn(".agents/skills/chaos-engine/SKILL.md", text)

    def test_agents_skill_pointer_cannot_drift_from_canonical(self):
        pointer = (OVERLAY / ".agents/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("chaos-engine/skills/chaos-engine/SKILL.md", pointer)
        self.assertNotIn("## Iron laws", pointer)
        # desired_content must regenerate the same class of pointer, never preserve foreign bodies
        source = (ROOT / "chaos-engine/hosts.py").read_text(encoding="utf-8")
        self.assertNotIn(
            'before.get(".agents/skills/chaos-engine/SKILL.md")',
            source,
        )

    def test_cli_owned_github_and_graphify_mcp_are_refused(self):
        for server in ("github", "github-gh", "github_gh", "graphify"):
            with self.subTest(server=server):
                error = self.policy.cli_owned_conflict_error([server])
                self.assertIsNotNone(error)
                self.assertIn("No duplicate GitHub MCP", error)
        self.assertIsNone(self.policy.cli_owned_conflict_error(["maven-tools-mcp"]))
        self.assertEqual(
            self.policy.HEAL_PROMPT,
            "No duplicate GitHub MCP. Repair: disable extras in host MCP config.",
        )

    def test_doctor_collects_cli_owned_conflict_from_temp_mcp(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".mcp.json").write_text(
                json.dumps({"mcpServers": {"github": {"command": "npx"}, "maven-tools-mcp": {"command": "java"}}}),
                encoding="utf-8",
            )
            ids = self.policy.collect_server_ids(project, home=project / "home")
            error = self.policy.cli_owned_conflict_error(ids)
            self.assertIsNotNone(error)
            self.assertIn("github", error.casefold())

    def test_instruction_block_matches_heal_prompt(self):
        block = self.hosts.instruction_block("chaos-engine")
        self.assertIn(self.policy.HEAL_PROMPT, block)
        self.assertIn("chaos-engine/skills/chaos-engine/SKILL.md", block)

    def test_copilot_cloud_ide_share_cli_policy_pointer(self):
        capability = self.kernel.HOST_CAPABILITIES["copilot"]
        self.assertEqual(("cloud", "ide"), capability.static_surfaces)
        copilot = (ROOT / ".github/copilot-instructions.md").read_text(encoding="utf-8")
        self.assertIn("<!-- CHAOSENGINE:START -->", copilot)
        self.assertIn("skills/chaos-engine/SKILL.md", copilot)
        for marker in CONTRACT_MARKERS:
            self.assertNotIn(marker, copilot)

    def test_hook_trigger_map_names_event_hosts_and_mode(self):
        text = HOOK_MAP.read_text(encoding="utf-8")
        required = (
            "SessionStart",
            "UserPromptSubmit",
            "PreToolUse",
            "PostToolUse",
            "PostToolUseFailure",
            "Stop",
            "SubagentStop",
            "PreCompact",
            "SessionEnd",
        )
        for event in required:
            self.assertIn(event, text)
        self.assertIn("| enforce |", text.casefold().replace("enforce", "enforce"))
        # Normalize: require mode column values
        self.assertRegex(text, r"(?i)\benforce\b")
        self.assertRegex(text, r"(?i)\bguide\b")
        self.assertIn("Grok", text)
        self.assertIn("SessionStart stdout", text)
        # PostToolUse must stay silent unless next decision changes
        self.assertRegex(text, r"(?i)PostToolUse.*silent|silent.*PostToolUse")

    def test_caveman_project_mode_skips_user_config_fallthrough(self):
        activate = (
            ROOT / "chaos-engine/vendor/caveman/src/hooks/caveman-activate.js"
        ).read_text(encoding="utf-8")
        config = (
            ROOT / "chaos-engine/vendor/caveman/src/hooks/caveman-config.js"
        ).read_text(encoding="utf-8")
        for text in (activate, config):
            self.assertIn("ChaosEngine project mode", text)
            self.assertIn("isChaosEngineProject", text)
            # Project mode must not call user-config path when project detected
            self.assertIn("project mode only", text.casefold())

    def test_user_instruction_conflict_detected_in_temp_home(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "project"
            home = root / "home"
            project.mkdir()
            (project / "AGENTS.md").write_text(
                "<!-- CHAOSENGINE:START -->\nproject block\n<!-- CHAOSENGINE:END -->\n",
                encoding="utf-8",
            )
            claude = home / ".claude"
            claude.mkdir(parents=True)
            (claude / "CLAUDE.md").write_text(
                "<!-- CHAOSENGINE:START -->\nproject block\n<!-- CHAOSENGINE:END -->\n",
                encoding="utf-8",
            )
            error = self.policy.user_instruction_conflict_error(project, home=home)
            self.assertIsNotNone(error)
            self.assertIn("duplicates the project instruction block", error)


if __name__ == "__main__":
    unittest.main()
