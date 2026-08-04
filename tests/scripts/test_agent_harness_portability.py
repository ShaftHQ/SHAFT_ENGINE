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
ACTIVE_GUIDANCE_PATHS = ("AGENTS.md", "CLAUDE.md", ".mcp.json", ".agents", ".claude", ".codex")

# --- superseded-policy scan -------------------------------------------------
#
# Module scope on purpose. These were function-locals, and the test that claimed
# to prove they worked declared its own private copies -- so it pinned a
# duplicate literal while the shipped patterns could be gutted entirely and the
# module stayed green. The live-data check below passes vacuously once the store
# is reconciled, so the only thing that proves these still detect anything is a
# test that calls the same objects the scan calls.

# An id is a pointer, not an assertion, and ids are immutable here:
# `.memory/events.jsonl` records `memory.created` against them, so a rename
# orphans history. A `[[wiki-link]]` naming an object whose policy has moved is
# therefore correct, and only prose is judged.
WIKI_LINK = re.compile(r"\[\[[^\]]+\]\]")
SENTENCE_BREAK = re.compile(r"(?<=[.;:!?])\s+|\n+")

# The Learning-loop table tells an agent to store a decision "superseding the
# entry it replaces", so a memory object whose whole purpose is to record that a
# policy was retired MUST be able to name that policy. A sentence carrying one of
# these markers is a historical record, not a claim that the policy still holds.
# Chosen over a tag-based opt-out because it needs nothing of the author: the
# sentence that records a supersession already says so.
SUPERSESSION_RECORD = re.compile(
    r"(?i)\b(?:supersed\w+|retired?|rescinded|rejected|revoked|abandoned|obsolete|"
    r"deprecated|no longer|replaced by|is gone|was dropped|used to)\b"
)

SUPERSEDED_ONE_PR_PER_SESSION = re.compile(
    r"(?i)one[- ]pr[- ]per[- ]session"
    r"|\b1 PR per session"
    r"|single (?:final|session) PR"
    r"|one-branch-one-worktree-one-pr"
    r"|session'?s single branch/pr"
)
ONE_PR_PER_SESSION_REASON = (
    "one PR per session: superseded by work-github-playbook.md Sec. 3b, "
    "which lets a session open one PR per group of related subtasks"
)

# All three must co-occur in one sentence. The subject alone plus any number
# near "min" fired on ordinary prose -- "returned 3 minor findings" matched
# inside "3 minor", and "takes about 40 min" is a timing observation, not a
# policy. The Learning loop writes memory constantly, so a check that trips on
# an unrelated observation gets edited away, and editing the check to reach
# green is what iron law 4 forbids.
#
# The directive list holds check-in vocabulary only. "every" and "after" were
# tried and are too weak: "Compaction after a merge takes 2 minutes and needs no
# delegate" carries all three signals and states no cadence. What actually marks
# a cadence is a sentence that says someone goes and looks.
#
# The cost of that choice, stated rather than left to be discovered: this list is
# fitted to the two offenders it was written against, so a cadence phrased any
# other way is missed -- "Ask a delegated agent for a progress report every 30
# minutes" and "A delegate gets 30 minutes before the orchestrator steps in" both
# pass. Incident narratives still trip it the other way ("The delegate went
# silent for 90 minutes before I checked in"), which is why the reconciled body
# above says "ran silent for over an hour" rather than a figure. The trade is
# deliberate -- a false positive reddens an unrelated PR and invites weakening
# the check, a false negative only fails to catch -- but it is a trade, not a
# solved problem. #4469.
DELEGATE_SUBJECT = re.compile(r"(?i)\b(?:delegate[ds]?|subagent|background (?:task|job))\b")
INTERVAL_FIGURE = re.compile(r"(?i)\b\d{1,3}\s*min(?:ute)?s?\b")
INTERVAL_DIRECTIVE = re.compile(
    r"(?i)\b(?:check(?:s|ed|ing)?[ -]?in|ping(?:s|ed|ing)?|status snapshot|"
    r"unexamined|interim signal|no signal|silent|intervene)\b"
)
DELEGATE_INTERVAL_REASON = (
    "a delegate check-in interval: delegation.md owns the single figure, "
    "and memory restating a second one is how 20 and 30 both became true"
)


def superseded_policy_offences(title: str, body: str) -> list[str]:
    """Return the superseded policies this text asserts as still current.

    Judged sentence by sentence so a supersession marker exempts only the
    sentence that carries it, not the whole object.
    """
    offences: set[str] = set()
    for sentence in SENTENCE_BREAK.split(WIKI_LINK.sub(" ", f"{title}\n{body}")):
        if SUPERSESSION_RECORD.search(sentence):
            continue
        if SUPERSEDED_ONE_PR_PER_SESSION.search(sentence):
            offences.add(ONE_PR_PER_SESSION_REASON)
        if (
            DELEGATE_SUBJECT.search(sentence)
            and INTERVAL_FIGURE.search(sentence)
            and INTERVAL_DIRECTIVE.search(sentence)
        ):
            offences.add(DELEGATE_INTERVAL_REASON)
    return sorted(offences)


def active_memory_policy_offenders(root: Path) -> list[str]:
    """Return `path: reason` for every active memory object restating a superseded policy."""
    memory_root = root / ".memory"
    offenders = []
    for metadata_path in sorted((memory_root / "memory").rglob("*.json")):
        metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
        if metadata.get("status", "active") != "active":
            continue
        body = (memory_root / metadata["body_path"]).read_text(encoding="utf-8")
        for reason in superseded_policy_offences(metadata.get("title", ""), body):
            offenders.append(f"{metadata_path.relative_to(root).as_posix()}: {reason}")
    return sorted(offenders)


def markdown_body(path: Path) -> str:
    content = path.read_text(encoding="utf-8")
    if not content.startswith("---\n"):
        return content.strip()
    marker = content.find("\n---\n", 4)
    return content[marker + 5 :].strip() if marker >= 0 else content.strip()


def hook_groups(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))["hooks"]


def absolute_guidance_path_offenders(
    root: Path, tracked_paths: list[Path] | None = None
) -> list[str]:
    if tracked_paths is None:
        tracked = subprocess.run(  # nosec B603 B607 - fixed read-only git command.
            ["git", "ls-files", "-z", "--", *ACTIVE_GUIDANCE_PATHS],
            cwd=root,
            capture_output=True,
            text=True,
            check=True,
        )
        tracked_paths = [Path(path) for path in tracked.stdout.split("\0") if path]
    forbidden = re.compile(
        r"(?:(?<![A-Za-z0-9])[A-Za-z]:[\\/]|/(?:Users|home)/[^/\s]+|\$\{CLAUDE_PROJECT_DIR\})"
    )
    return [
        path.as_posix()
        for path in tracked_paths
        if (root / path).is_file()
        and forbidden.search((root / path).read_text(encoding="utf-8", errors="ignore"))
    ]


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

    def test_act_as_mohab_embeds_core_working_rules(self):
        content = (ROOT / ".agents/skills/act-as-mohab/SKILL.md").read_text(
            encoding="utf-8"
        )
        for heading in ("### Caveman", "### Ponytail", "### Test-driven development"):
            self.assertIn(heading, content)
        for retired_link in (
            "references/caveman.md",
            "references/ponytail.md",
            "references/test-driven-development.md",
        ):
            self.assertNotIn(retired_link, content)

    def test_act_as_mohab_requires_fresh_task_branch_from_fetched_main(self):
        content = (ROOT / ".agents/skills/act-as-mohab/SKILL.md").read_text(
            encoding="utf-8"
        )
        compact = re.sub(r"\s+", " ", content)
        for required in (
            "## Task isolation",
            "Before task-specific discovery or edits",
            "successfully fetch and prune",
            "fresh `ChaosEngine/*` branch/worktree",
            "fetched `origin/main`",
            "Never reuse that branch for a later user task",
        ):
            self.assertIn(required, compact)

    def test_host_token_budgets_include_mandatory_entrypoint(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        mandatory = ".agents/skills/act-as-mohab/SKILL.md"
        for host, paths in budget["host_contexts"].items():
            self.assertIn(mandatory, paths, host)

    def test_rule_docs_are_embedded_rather_than_redirected(self):
        """Redirect stubs cost a read and carry no content, so the rules live in
        the entrypoint and the stubs are gone. Attribution must survive."""
        references = ROOT / ".agents/skills/act-as-mohab/references"
        for retired in (
            "caveman.md",
            "ponytail.md",
            "test-driven-development.md",
            "tdd/resisting-rationalization.md",
            "tdd/testing-anti-patterns.md",
        ):
            self.assertFalse((references / retired).exists(), retired)
        for licence in (
            "caveman.LICENSE",
            "ponytail.LICENSE",
            "test-driven-development.LICENSE",
        ):
            self.assertTrue((references / licence).is_file(), licence)

    def test_all_hosts_reach_the_same_entrypoint_without_grok_duplication(self):
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        claude = (ROOT / "CLAUDE.md").read_text(encoding="utf-8")
        self.assertIn(".agents/skills/act-as-mohab/SKILL.md", agents)
        self.assertIn("@AGENTS.md", claude)
        self.assertFalse((ROOT / "GROK.md").exists())
        self.assertFalse((ROOT / ".grok").exists())

    def test_active_guidance_has_no_personal_or_absolute_operational_paths(self):
        self.assertEqual(absolute_guidance_path_offenders(ROOT), [])

    def test_absolute_path_scan_ignores_untracked_local_guidance(self):
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            (root / "AGENTS.md").write_text("Use relative paths.\n", encoding="utf-8")
            local = root / ".claude/settings.local.json"
            local.parent.mkdir(parents=True)
            local.write_text('{"localPath":"C:\\\\Users\\\\owner"}\n', encoding="utf-8")

            self.assertEqual(absolute_guidance_path_offenders(root, [Path("AGENTS.md")]), [])
            self.assertEqual(
                absolute_guidance_path_offenders(
                    root, [Path("AGENTS.md"), Path(".claude/settings.local.json")]
                ),
                [".claude/settings.local.json"],
            )

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

    def test_pdca_personas_are_main_thread_phases_that_follow_the_mode(self):
        """PDCA is one task, so it is normally worked solo.

        This previously required Bob to dispatch unconditionally, which put the
        playbook in direct conflict with the entrypoint's solo-or-orchestrate
        rule: a single task means one work stream, and one stream is worked by
        the same thread. Bob now follows the mode instead of overriding it.
        """
        pdca = (
            ROOT
            / ".agents/skills/act-as-mohab/references/playbooks/agentic-pdca-loop.md"
        ).read_text(encoding="utf-8")
        compact = re.sub(r"\s+", " ", pdca)
        self.assertIn("personas are phases, not agent identities", pdca.lower())
        self.assertRegex(
            compact,
            r"solo-or-orchestrate rule",
            "the playbook must defer to the mode rule rather than state its own",
        )
        self.assertRegex(
            compact,
            r"Bob phase[^.]*through observed TDD[^.]*when orchestrating",
            "Bob implements in solo mode and shepherds only when orchestrating",
        )
        self.assertRegex(compact, r"Bruce[^.]*judges the actual diff")
        for forbidden in ("orchestrator never edits", "closing remaining gaps himself"):
            self.assertNotIn(forbidden, pdca)

    def test_hook_configs_share_one_cwd_independent_lifecycle_contract(self):
        claude_hooks = hook_groups(ROOT / ".claude/settings.json")
        codex_hooks = hook_groups(ROOT / ".codex/hooks.json")
        self.assertEqual(set(claude_hooks), set(codex_hooks))
        for hooks in (claude_hooks, codex_hooks):
            self.assertEqual(set(hooks), {"PreToolUse", "SessionStart", "Stop"})
            commands = {
                handler["command"]
                for groups in hooks.values()
                for group in groups
                for handler in group["hooks"]
            }
            self.assertEqual(len(commands), 1)
            command = commands.pop()
            self.assertNotIn(str(ROOT), command)
            handler = hooks["PreToolUse"][0]["hooks"][0]
            invocation = [handler["command"], *handler.get("args", [])]
            completed = subprocess.run(
                invocation if handler.get("args") else command,
                shell=not bool(handler.get("args")),  # nosec B602 - tracked hook.
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
        for groups in claude_hooks.values():
            for group in groups:
                for handler in group["hooks"]:
                    self.assertEqual(handler["command"], "python3")
                    self.assertEqual(handler["args"][0], "-c")
                    self.assertIn("scripts/agents/guard.py", handler["args"][1])
        for groups in codex_hooks.values():
            for group in groups:
                for handler in group["hooks"]:
                    self.assertTrue(handler["commandWindows"].startswith("py -3 "))
                    self.assertNotIn(str(ROOT), handler["commandWindows"])
        self.assertFalse((ROOT / ".claude/hooks/guard.py").exists())
        self.assertTrue(GUARD.is_file())

    def test_hook_configs_are_tracked_for_host_local_trust(self):
        tracked = subprocess.run(  # nosec B603 B607 - fixed read-only git command.
            ["git", "ls-files", "--error-unmatch", ".claude/settings.json", ".codex/hooks.json", "scripts/agents/guard.py"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        for path in (ROOT / ".claude/settings.json", ROOT / ".codex/hooks.json"):
            self.assertNotIn("bypass-hook-trust", path.read_text(encoding="utf-8"))

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

    @unittest.skipUnless(os.name == "nt", "executes the tracked Windows hook commands")
    def test_windows_hook_commands_execute_from_a_nested_directory(self):
        payload = json.dumps(
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "shell_command",
                "tool_input": {"command": "mvn test"},
            }
        )
        for config_path in (ROOT / ".claude/settings.json", ROOT / ".codex/hooks.json"):
            handler = hook_groups(config_path)["PreToolUse"][0]["hooks"][0]
            command = handler.get("commandWindows") or subprocess.list2cmdline(
                [handler["command"], *handler.get("args", [])]
            )
            completed = subprocess.run(
                command,
                shell=True,  # nosec B602 - executes the repository's tracked hook definition.
                input=payload,
                cwd=ROOT / "shaft-engine",
                env=dict(os.environ, SHAFT_GUARD_HOST=config_path.parent.name),
                capture_output=True,
                text=True,
                timeout=10,
                check=False,
            )
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertIn("R1", completed.stdout)

    def test_guard_ignores_events_and_tools_outside_its_contract(self):
        source = GUARD.read_text(encoding="utf-8")
        for removed in (
            "graphify_nudge",
            "tdd_nudge",
            "check_r7_orchestration_skill",
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
            {"hook_event_name": "PostToolUse", "tool_name": "Read", "tool_input": {}},
        ):
            completed = self.run_guard_completed(payload, "claude")
            self.assertEqual(completed.stdout, "")

    def test_deployed_canonical_subtree_has_no_external_or_broken_markdown_links(self):
        # The deployment unit is the whole skills tree, not one skill: the
        # router links sibling skills, so a per-skill copy would report false
        # breaks and a per-skill deploy would create real ones.
        canonical = ROOT / ".agents/skills"
        with tempfile.TemporaryDirectory() as temporary_directory:
            deployed = Path(temporary_directory) / ".agents/skills"
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

    def test_active_memory_does_not_restate_a_superseded_policy(self):
        """Retired *policy*, not just retired paths and model names.

        Its sibling above compares memory against guidance on two axes:
        filesystem paths that no longer exist, and capability named as a product.
        Policy that current guidance explicitly supersedes is uncovered by both,
        and that is the axis that misleads: routing tells an agent a retrieved
        claim is a lead and never outranks the live file, so resolving a
        contradiction costs a read an agent under time pressure will not do. It
        reads one of them, and which one is luck.
        """
        self.assertEqual(
            active_memory_policy_offenders(ROOT),
            [],
            "active memory restates a superseded policy",
        )

    def test_the_policy_scan_walks_the_store_and_honours_the_status_filter(self):
        """Covers the walk itself, not just the patterns.

        The live-data assertion one method up cannot be guarded by any test in
        this module -- neutering an `assertEqual` is invisible from the outside.
        What can be guarded is everything it calls, so a synthetic store proves
        the walk finds an offender, reports it as `path: reason`, and skips a
        non-active object rather than silently skipping every object.
        """
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            objects = root / ".memory/memory/constraints"
            objects.mkdir(parents=True)
            for name, status in (("live", "active"), ("retired", "superseded")):
                (objects / f"{name}.md").write_text(
                    "Still one PR per session: a single final PR.\n", encoding="utf-8"
                )
                (objects / f"{name}.json").write_text(
                    json.dumps(
                        {
                            "id": f"constraint.{name}",
                            "status": status,
                            "title": "",
                            "body_path": f"memory/constraints/{name}.md",
                        }
                    ),
                    encoding="utf-8",
                )
            self.assertEqual(
                active_memory_policy_offenders(root),
                [f".memory/memory/constraints/live.json: {ONE_PR_PER_SESSION_REASON}"],
            )

    def test_the_superseded_policy_scan_detects_a_claim(self):
        """The live-data check above passes vacuously once the store is clean.

        Nothing in it proves the patterns still match anything, so gutting them
        would leave the module green. These cases call the same module-level
        objects that check calls -- not a private copy -- so an emptied or
        broken pattern fails here.
        """
        self.assertEqual(
            superseded_policy_offences("", "Still one PR per session: a single final PR."),
            [ONE_PR_PER_SESSION_REASON],
        )
        self.assertEqual(
            superseded_policy_offences(
                "Orchestrator checks in on any delegated background task after 30 minutes",
                "Check in every 30 minutes on a silent delegate.",
            ),
            [DELEGATE_INTERVAL_REASON],
        )

    def test_the_superseded_policy_scan_judges_prose_and_not_identifiers(self):
        """A guard that cannot tell a claim from a pointer is not usable here.

        Renaming a memory object is not free -- its id is written into
        `.memory/events.jsonl` -- so a link to an object whose policy has since
        moved has to stay legal while the sentence asserting that policy does
        not.
        """
        self.assertEqual(
            superseded_policy_offences(
                "", "See [[one-branch-one-worktree-one-pr-per-session]] for the incident."
            ),
            [],
        )
        self.assertEqual(
            superseded_policy_offences(
                "", "The one-PR-per-session default still applies to this session."
            ),
            [ONE_PR_PER_SESSION_REASON],
        )

    def test_the_superseded_policy_scan_allows_recording_the_supersession(self):
        """The Learning-loop table asks for exactly the sentence this could ban.

        It routes "a decision with a rationale someone will otherwise
        re-litigate" to memory "superseding the entry it replaces" -- which
        means naming the retired policy. A check that forbids the record the
        harness prescribes would make the two contradict each other, and the
        only escape would be `status != "active"`, a hatch this store has never
        used (335 active, 1 open, zero superseded).
        """
        for record in (
            "Decision 2026-07-20: the owner retired the one-PR-per-session default"
            " in favour of grouped PRs per Sec. 3b.",
            "The #3643 session shipped a single final PR and the owner rejected it;"
            " that shape is gone.",
            "One PR per session is superseded by work-github-playbook.md Sec. 3b.",
        ):
            with self.subTest(record=record[:40]):
                self.assertEqual(superseded_policy_offences("", record), [])

    def test_the_supersession_exemption_is_scoped_to_one_sentence(self):
        """Per-sentence scoping is the whole reason this exemption is safe.

        Whole-object scoping was the rejected design: it lets one historical
        aside launder every claim in the same object. Nothing tested that
        choice, though -- every fixture above is a single sentence, so replacing
        `SENTENCE_BREAK` with a never-matching pattern silently reverts to the
        rejected design and they all still pass. This is the one case that can
        tell the two apart, and it is checked in both orderings because the
        splitter must not care which side the marker falls on.

        Known limitation, inherent to sentence granularity: a marker anywhere in
        a sentence exempts that whole sentence, so ".. was retired, and one PR
        per session is still the default." reads clean. Noted in #4468, not
        fixed here.
        """
        for record in (
            "The per-phase pattern was retired. One PR per session is still the default.",
            "One PR per session is still the default. The per-phase pattern was retired.",
        ):
            with self.subTest(record=record[:40]):
                self.assertEqual(
                    superseded_policy_offences("", record), [ONE_PR_PER_SESSION_REASON]
                )

    def test_the_interval_scan_ignores_prose_that_merely_mentions_minutes(self):
        """A false positive here gets the check edited away, not the memory fixed.

        The Learning loop writes memory objects routinely, so a scan that trips
        on an ordinary timing observation turns the harness step red on an
        unrelated PR -- and the cheapest repair an agent sees is to weaken the
        check, which iron law 4 forbids. All three signals must co-occur in one
        sentence: the subject, a real minute figure, and a directive.
        """
        for benign in (
            "The reviewer returned 3 minor findings on the delegate's diff.",
            "A delegate's first pass takes about 40 min; size the wakeup accordingly.",
            "Compaction after a merge takes 2 minutes and needs no delegate.",
        ):
            with self.subTest(text=benign[:40]):
                self.assertEqual(superseded_policy_offences("", benign), [])
        self.assertNotRegex("3 minor findings", INTERVAL_FIGURE)

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
