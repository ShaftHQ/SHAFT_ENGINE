"""Cross-host contract for the source-controlled agent harness."""

from __future__ import annotations

import json
import os
import re
import shutil
import subprocess  # nosec B404 - tests exercise trusted local commands.
import sys
import tempfile
import tomllib
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
GUARD = ROOT / "scripts/agents/guard.py"


def markdown_body(path: Path) -> str:
    content = path.read_text(encoding="utf-8")
    if not content.startswith("---\n"):
        return content.strip()
    marker = content.find("\n---\n", 4)
    return content[marker + 5 :].strip() if marker >= 0 else content.strip()


def hook_groups(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))["hooks"]


class AgentHarnessPortabilityTest(unittest.TestCase):
    def test_act_as_mohab_has_one_substantive_body_and_relative_adapter(self):
        canonical = ROOT / ".agents/skills/act-as-mohab/SKILL.md"
        adapter = ROOT / ".claude/skills/act-as-mohab/SKILL.md"
        self.assertTrue(canonical.is_file())
        self.assertGreater(len(markdown_body(canonical)), 1000)
        self.assertLess(len(markdown_body(adapter)), 500)

        candidates = list(ROOT.glob(".*/skills/act-as-mohab/SKILL.md"))
        substantive = [path for path in candidates if len(markdown_body(path)) > 500]
        self.assertEqual(substantive, [canonical])

        match = re.search(r"\[[^]]+\]\(([^)]+)\)", adapter.read_text(encoding="utf-8"))
        self.assertIsNotNone(match)
        target = match.group(1)
        self.assertFalse(Path(target).is_absolute())
        self.assertEqual((adapter.parent / target).resolve(), canonical.resolve())

    def test_all_hosts_reach_the_same_entrypoint_without_grok_duplication(self):
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        claude = (ROOT / "CLAUDE.md").read_text(encoding="utf-8")
        self.assertIn(".agents/skills/act-as-mohab/SKILL.md", agents)
        self.assertIn("@AGENTS.md", claude)
        self.assertFalse((ROOT / "GROK.md").exists())
        self.assertFalse((ROOT / ".grok").exists())

    def test_active_guidance_has_no_personal_or_absolute_operational_paths(self):
        paths = [ROOT / "AGENTS.md", ROOT / "CLAUDE.md", ROOT / ".mcp.json"]
        for directory in (ROOT / ".agents", ROOT / ".claude", ROOT / ".codex"):
            paths.extend(path for path in directory.rglob("*") if path.is_file())
        forbidden = re.compile(
            r"(?:(?<![A-Za-z0-9])[A-Za-z]:[\\/]|/(?:Users|home)/[^/\s]+|\$\{CLAUDE_PROJECT_DIR\})"
        )
        offenders = [
            path.relative_to(ROOT).as_posix()
            for path in paths
            if forbidden.search(path.read_text(encoding="utf-8", errors="ignore"))
        ]
        self.assertEqual(offenders, [])

    def test_delegation_policy_uses_capability_tiers_not_fixed_models_or_effort(self):
        paths = [ROOT / "AGENTS.md", ROOT / ".claude/user-harness/settings.json"]
        paths.extend((ROOT / ".agents/skills/act-as-mohab").rglob("*.md"))
        paths.extend((ROOT / ".claude/agents").glob("*.md"))
        forbidden = re.compile(
            r"(?i)\b(?:sonnet|haiku|opus|fable|gpt-[\w.-]+|grok-[\w.-]+)\b"
            r"|\beffortLevel\b|\bHIGH effort\b|^model:\s*",
            re.MULTILINE,
        )
        offenders = [
            path.relative_to(ROOT).as_posix()
            for path in paths
            if forbidden.search(path.read_text(encoding="utf-8"))
        ]
        self.assertEqual(offenders, [])

    def test_pdca_personas_are_main_thread_phases_that_assign_implementation(self):
        pdca = (
            ROOT
            / ".agents/skills/act-as-mohab/references/playbooks/agentic-pdca-loop.md"
        ).read_text(encoding="utf-8")
        for forbidden in ("Bob implements", "closing remaining gaps himself"):
            self.assertNotIn(forbidden, pdca)
        compact = re.sub(r"\s+", " ", pdca)
        self.assertRegex(compact, r"Bob phase[^.]*dispatches[^.]*middle-tier implementation owner")
        self.assertRegex(compact, r"Bruce[^.]*assigns?[^.]*patch")
        self.assertIn("personas are phases, not agent identities", pdca.lower())

    def test_hook_configs_share_one_cwd_independent_pretooluse_contract(self):
        claude_hooks = hook_groups(ROOT / ".claude/settings.json")
        codex_hooks = hook_groups(ROOT / ".codex/hooks.json")
        self.assertEqual(claude_hooks, codex_hooks)
        for hooks in (claude_hooks, codex_hooks):
            self.assertEqual(set(hooks), {"PreToolUse"})
            commands = {
                handler["command"]
                for groups in hooks.values()
                for group in groups
                for handler in group["hooks"]
            }
            self.assertEqual(len(commands), 1)
            command = commands.pop()
            self.assertNotIn(str(ROOT), command)
            completed = subprocess.run(
                command,
                shell=True,  # nosec B602 - execute tracked hook command exactly.
                input=json.dumps(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "shell_command",
                        "tool_input": {"command": "mvn test"},
                    }
                ),
                cwd=ROOT / "shaft-engine",
                env=dict(os.environ, SHAFT_GUARD_HOST="codex"),
                capture_output=True,
                text=True,
                timeout=10,
                check=False,
            )
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertIn("R1", completed.stdout)
        self.assertFalse((ROOT / ".claude/hooks/guard.py").exists())
        self.assertTrue(GUARD.is_file())

    def test_equivalent_host_hook_events_produce_equivalent_outcomes(self):
        fixtures = {
            "claude": {
                "hook_event_name": "PreToolUse",
                "tool_name": "Bash",
                "tool_input": {"command": "mvn -pl shaft-engine test"},
                "session_id": "portable-claude",
            },
            "codex": {
                "hook_event_name": "PreToolUse",
                "tool_name": "shell_command",
                "tool_input": {"command": "mvn -pl shaft-engine test"},
                "session_id": "portable-codex",
            },
            "grok": {
                "hookEventName": "PreToolUse",
                "toolName": "Bash",
                "toolInput": {"command": "mvn -pl shaft-engine test"},
                "sessionId": "portable-grok",
            },
        }
        decisions = []
        for host, payload in fixtures.items():
            output = self.run_guard(payload, host)
            decisions.append(self.logical_decision(output))
        self.assertEqual(decisions, [decisions[0]] * 3)
        self.assertEqual(decisions[0][0], "deny")
        self.assertIn("R1", decisions[0][1])

    def test_guard_has_only_portable_explicit_deny_semantics(self):
        source = GUARD.read_text(encoding="utf-8")
        for removed in (
            "graphify_nudge",
            "tdd_nudge",
            "check_r7_orchestration_skill",
            "additionalContext",
            "SessionStart",
            "SubagentStart",
        ):
            self.assertNotIn(removed, source)
        for payload in (
            {"hook_event_name": "PreToolUse", "tool_name": "Read", "tool_input": {}},
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "shaft-engine/src/main/java/Example.java"},
            },
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "Skill",
                "tool_input": {"skill": "work-github"},
                "agent_type": "coder",
            },
        ):
            completed = self.run_guard_completed(payload, "claude")
            self.assertEqual(completed.stdout, "")

    def test_deployed_canonical_subtree_has_no_external_or_broken_markdown_links(self):
        canonical = ROOT / ".agents/skills/act-as-mohab"
        with tempfile.TemporaryDirectory() as temporary_directory:
            deployed = Path(temporary_directory) / ".agents/skills/act-as-mohab"
            shutil.copytree(canonical, deployed)
            broken = []
            for path in deployed.rglob("*.md"):
                for raw in re.findall(r"(?<!!)\[[^]]*\]\(([^)]+)\)", path.read_text(encoding="utf-8")):
                    target = raw.strip().strip("<>").split("#", 1)[0]
                    if not target or re.match(r"^[a-z][a-z0-9+.-]*:", target, re.I):
                        continue
                    resolved = (path.parent / target).resolve()
                    try:
                        resolved.relative_to(deployed.resolve())
                    except ValueError:
                        broken.append(f"{path.relative_to(deployed)} -> {raw}")
                        continue
                    if not resolved.exists():
                        broken.append(f"{path.relative_to(deployed)} -> {raw}")
            self.assertEqual(broken, [])

    def test_mempalace_config_is_tracked_while_generated_state_is_ignored(self):
        tracked = subprocess.run(  # nosec B603 B607 - fixed read-only git command.
            ["git", "ls-files", "--error-unmatch", "mempalace.yaml"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        palace = (ROOT / "mempalace.yaml").read_text(encoding="utf-8")
        self.assertIn("exclude_patterns:", palace)
        self.assertNotRegex(palace, r"(?m)^- name: (?:target|graphify_out|allure_results)$")
        claude_mcp = json.loads((ROOT / ".mcp.json").read_text(encoding="utf-8"))
        user_settings = json.loads(
            (ROOT / ".claude/user-harness/settings.json").read_text(encoding="utf-8")
        )
        self.assertIs(user_settings["enabledPlugins"]["mempalace@mempalace"], False)
        self.assertEqual(
            claude_mcp["mcpServers"]["mempalace"]["env"]["MEMPALACE_EMBEDDING_MODEL"],
            "minilm",
        )
        codex = tomllib.loads((ROOT / ".codex/config.toml").read_text(encoding="utf-8"))
        project_mcp = claude_mcp["mcpServers"]["mempalace"]
        codex_mcp = codex["mcp_servers"]["mempalace"]
        self.assertEqual(codex_mcp["command"], project_mcp["command"])
        self.assertEqual(codex_mcp["env"], project_mcp["env"])
        ignore = (ROOT / ".gitignore").read_text(encoding="utf-8")
        self.assertNotRegex(ignore, r"(?m)^mempalace\.yaml$")
        for pattern in ("entities.json", "graphify-out/", "**/target/"):
            self.assertIn(pattern, ignore)

    def test_active_memory_has_no_retired_harness_contracts(self):
        memory_root = ROOT / ".memory"
        retired = re.compile(
            r"\.claude/hooks/guard\.py|\.claude/skills/graphify/SKILL\.md|"
            r"\.agents/routing-bridges\.txt|check_r7_orchestration_skill|"
            r"run_graphify_self_test|run_tdd_self_test"
        )
        fixed_routing = re.compile(
            r"\b(?:Sonnet|Haiku|Opus|Fable)\b|HIGH effort|effortLevel"
        )
        offenders = []
        for metadata_path in (memory_root / "memory").rglob("*.json"):
            metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
            if metadata.get("status", "active") != "active":
                continue
            body_path = memory_root / metadata["body_path"]
            searchable = "\n".join(
                (
                    body_path.read_text(encoding="utf-8"),
                    json.dumps(metadata.get("facets", {})),
                    json.dumps(metadata.get("evidence", [])),
                )
            )
            if retired.search(searchable):
                offenders.append(str(metadata_path.relative_to(ROOT)))
            if metadata.get("type") in {"decision", "constraint"} and fixed_routing.search(
                searchable
            ):
                offenders.append(str(metadata_path.relative_to(ROOT)))
        self.assertEqual(sorted(set(offenders)), [])

        unified = (
            memory_root
            / "memory/decisions/unified-agent-harness-single-entrypoint-capability-tiers.json"
        )
        self.assertTrue(unified.is_file())
        self.assertEqual(
            json.loads(unified.read_text(encoding="utf-8")).get("status", "active"),
            "active",
        )

    def run_guard(self, payload: dict, host: str) -> dict:
        completed = self.run_guard_completed(payload, host)
        self.assertTrue(completed.stdout.strip())
        return json.loads(completed.stdout)

    def run_guard_completed(self, payload: dict, host: str) -> subprocess.CompletedProcess:
        env = dict(os.environ, SHAFT_GUARD_HOST=host)
        if host == "grok":
            env["GROK_HOOK_EVENT"] = payload.get("hookEventName", "")
        with tempfile.TemporaryDirectory() as state_dir:
            env["SHAFT_GUARD_STATE_DIR"] = state_dir
            completed = subprocess.run(  # nosec B603 - trusted interpreter and repo script.
                [sys.executable, str(GUARD)],
                input=json.dumps(payload),
                cwd=ROOT,
                env=env,
                capture_output=True,
                text=True,
                timeout=10,
                check=False,
            )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        return completed

    @staticmethod
    def logical_decision(output: dict) -> tuple[str, str]:
        if "hookSpecificOutput" in output:
            specific = output["hookSpecificOutput"]
            return specific.get("permissionDecision", "allow"), specific.get(
                "permissionDecisionReason", specific.get("additionalContext", "")
            )
        return output.get("decision", "allow"), output.get("reason", "")


if __name__ == "__main__":
    unittest.main()
