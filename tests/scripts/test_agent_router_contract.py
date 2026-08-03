"""Contract for act-as-mohab as the single cross-host skill router.

One canonical entrypoint is the only surface every host reaches: Copilot and
Grok have no skill auto-discovery, and Codex drops skills from its listing
under budget pressure. These checks pin the properties that make that
entrypoint work the same way everywhere -- a consult gate before work starts,
a routing table that provably reaches every skill surface, read chains short
enough to survive a truncated preview, and descriptions that state triggers
instead of summarising the body.
"""

from __future__ import annotations

import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CANONICAL_SKILLS = ROOT / ".agents/skills"
CLAUDE_SKILLS = ROOT / ".claude/skills"
CLAUDE_AGENTS = ROOT / ".claude/agents"
ENTRYPOINT = CANONICAL_SKILLS / "act-as-mohab/SKILL.md"
REFERENCES = CANONICAL_SKILLS / "act-as-mohab/references"
ROUTING = REFERENCES / "routing.md"
ROLES = REFERENCES / "roles.md"
DELEGATION = REFERENCES / "delegation.md"
CONSULT = CANONICAL_SKILLS / "consult-first/SKILL.md"
BUDGET = ROOT / "scripts/ci/agent_guidance_budget.json"

# agentskills.io/specification: keep SKILL.md under 500 lines so a host never
# truncates it, and keep file references one level deep from SKILL.md.
MAX_SKILL_BODY_LINES = 500
# Hops after SKILL.md before real content. SKILL.md -> routing.md -> playbook
# is the deepest chain the router is allowed to build.
MAX_READ_CHAIN_HOPS = 2
# A file whose entire body is a pointer costs a read and teaches nothing.
REDIRECT_STUB_MAX_CHARS = 250


def frontmatter(path: Path) -> dict[str, str]:
    """Parse the flat YAML frontmatter every skill in this repository uses."""
    content = path.read_text(encoding="utf-8")
    if not content.startswith("---\n"):
        return {}
    marker = content.find("\n---\n", 4)
    if marker < 0:
        return {}
    values: dict[str, str] = {}
    key = ""
    for line in content[4:marker].splitlines():
        if re.match(r"^[A-Za-z][\w-]*:", line):
            key, _, value = line.partition(":")
            key = key.strip()
            values[key] = value.strip().strip("\"'").lstrip(">|-").strip()
        elif key and line.strip():
            values[key] = f"{values[key]} {line.strip()}".strip()
    return values


def markdown_body(path: Path) -> str:
    """Return the Markdown that follows optional frontmatter."""
    content = path.read_text(encoding="utf-8")
    if not content.startswith("---\n"):
        return content.strip()
    marker = content.find("\n---\n", 4)
    return content[marker + 5 :].strip() if marker >= 0 else content.strip()


def compact(path: Path) -> str:
    """Return whitespace-collapsed lowercase content for phrase assertions."""
    return re.sub(r"\s+", " ", path.read_text(encoding="utf-8")).lower()


def local_links(path: Path) -> list[str]:
    """Return repository-local Markdown link targets, ignoring images and URLs."""
    targets = []
    for raw in re.findall(r"(?<!!)\[[^]]*\]\(([^)]+)\)", path.read_text(encoding="utf-8")):
        target = raw.strip().strip("<>").split("#", 1)[0]
        if target and not re.match(r"^[a-z][a-z0-9+.-]*:", target, re.I):
            targets.append(target)
    return targets


def read_chain_depth(start: Path) -> dict[Path, int]:
    """Breadth-first hop count from a skill body to every reachable local doc."""
    depths = {start.resolve(): 0}
    frontier = [start]
    while frontier:
        current = frontier.pop()
        for target in local_links(current):
            resolved = (current.parent / target).resolve()
            if resolved.suffix != ".md" or not resolved.is_file():
                continue
            hop = depths[current.resolve()] + 1
            if resolved not in depths or hop < depths[resolved]:
                depths[resolved] = hop
                frontier.append(resolved)
    return depths


class ConsultGateTest(unittest.TestCase):
    """The deliberation skill exists and runs before any task-specific work."""

    def test_consult_claude_exists_on_every_host_discovery_path(self):
        for path in (CONSULT, CLAUDE_SKILLS / "consult-first/SKILL.md"):
            self.assertTrue(path.is_file(), f"missing skill: {path}")
        self.assertTrue((CONSULT.parent / "agents/openai.yaml").is_file())

    def test_only_the_canonical_consult_skill_carries_a_substantive_body(self):
        adapter = CLAUDE_SKILLS / "consult-first/SKILL.md"
        self.assertGreater(len(markdown_body(CONSULT)), 1000)
        self.assertLess(len(markdown_body(adapter)), 500)
        targets = local_links(adapter)
        self.assertTrue(targets, "adapter must link the canonical body")
        self.assertEqual((adapter.parent / targets[0]).resolve(), CONSULT.resolve())

    def test_entrypoint_opens_every_task_by_consulting_before_acting(self):
        content = compact(ENTRYPOINT)
        self.assertIn("consult-first", content)
        self.assertRegex(
            content, r"before (?:any |task-specific )?(?:discovery|work|edits|implementation)"
        )

    def test_consult_gate_scales_depth_instead_of_fixed_ceremony(self):
        content = compact(CONSULT)
        self.assertRegex(content, r"blast radius")
        self.assertRegex(content, r"reversib")
        self.assertRegex(
            content,
            r"(?:depth|weight|scale|proportional)",
            "the gate must state that its depth scales with the change",
        )

    def test_consult_skill_states_the_enforced_delivery_lifecycle(self):
        content = compact(CONSULT)
        for phase in (
            "analyze",
            "plan",
            "design",
            "red",
            "green",
            "refactor",
            "commit",
            "pull request",
            "merge",
        ):
            self.assertIn(phase, content, f"lifecycle phase missing: {phase}")

    def test_consult_skill_requires_rival_approaches_before_committing_to_one(self):
        content = re.sub(r"\s+", " ", markdown_body(CONSULT)).lower()
        self.assertRegex(content, r"(?:two|2) .{0,40}(?:approach|option|candidate)")
        self.assertIn("steelman", content)


class RouterTableTest(unittest.TestCase):
    """The router reaches every skill surface it owns, in one hop from itself."""

    def test_routing_reference_carries_a_deterministic_table(self):
        rows = [line for line in ROUTING.read_text(encoding="utf-8").splitlines()
                if line.strip().startswith("|")]
        self.assertGreaterEqual(len(rows), 12, "routing table is missing or too small")

    def test_router_links_every_repository_playbook_directly(self):
        linked = {
            (ROUTING.parent / target).resolve()
            for target in local_links(ROUTING)
        }
        playbooks = sorted((REFERENCES / "playbooks").glob("*.md"))
        self.assertTrue(playbooks, "no playbooks found to route to")
        unreachable = [
            path.name for path in playbooks if path.resolve() not in linked
        ]
        self.assertEqual(unreachable, [], "playbooks not linked directly from routing.md")

    def test_router_hands_shaft_product_work_to_the_product_router(self):
        self.assertIn("shaft-developer", ROUTING.read_text(encoding="utf-8"))

    def test_entrypoint_reaches_routing_roles_and_delegation(self):
        content = ENTRYPOINT.read_text(encoding="utf-8")
        for surface in ("routing.md", "delegation.md", "roles.md"):
            self.assertIn(surface, content, f"entrypoint does not reach {surface}")

    def test_retired_indirection_files_are_gone(self):
        for retired in (
            "caveman.md",
            "ponytail.md",
            "test-driven-development.md",
            "tdd/resisting-rationalization.md",
            "tdd/testing-anti-patterns.md",
            "playbooks/README.md",
        ):
            self.assertFalse(
                (REFERENCES / retired).exists(),
                f"{retired} is a redirect layer the router replaces",
            )

    def test_attribution_licences_survive_the_stub_deletion(self):
        for licence in (
            "caveman.LICENSE",
            "ponytail.LICENSE",
            "test-driven-development.LICENSE",
        ):
            self.assertTrue((REFERENCES / licence).is_file(), f"missing {licence}")

    def test_no_guidance_file_still_links_a_retired_indirection_file(self):
        retired = re.compile(
            r"references/(?:caveman|ponytail|test-driven-development)\.md"
            r"|references/tdd/|playbooks/README\.md"
        )
        offenders = [
            path.relative_to(ROOT).as_posix()
            for path in [*ROOT.glob(".agents/**/*.md"), *ROOT.glob(".claude/**/*.md"),
                         *ROOT.glob(".github/skills/**/*.md")]
            if "worktrees" not in path.parts and retired.search(path.read_text(encoding="utf-8"))
        ]
        self.assertEqual(offenders, [])


class ProgressiveDisclosureTest(unittest.TestCase):
    """Bodies and read chains stay inside documented host read limits."""

    def canonical_skills(self) -> list[Path]:
        return sorted(CANONICAL_SKILLS.glob("*/SKILL.md"))

    def test_every_canonical_skill_body_fits_one_read(self):
        for path in self.canonical_skills():
            with self.subTest(skill=path.parent.name):
                self.assertLess(len(markdown_body(path).splitlines()), MAX_SKILL_BODY_LINES)

    def test_read_chains_stay_within_two_hops_of_the_skill_body(self):
        offenders = []
        for skill in self.canonical_skills():
            for path, hops in read_chain_depth(skill).items():
                if hops > MAX_READ_CHAIN_HOPS:
                    offenders.append(f"{skill.parent.name} -> {path.name} ({hops} hops)")
        self.assertEqual(offenders, [], "read chain is too deep to survive a truncated preview")

    def test_no_reference_is_a_pure_redirect_stub(self):
        offenders = []
        for path in REFERENCES.rglob("*.md"):
            body = markdown_body(path)
            if len(body) < REDIRECT_STUB_MAX_CHARS and re.search(r"\[[^]]+\]\([^)]+\)", body):
                offenders.append(path.relative_to(ROOT).as_posix())
        self.assertEqual(offenders, [], "a reference whose whole body is a pointer is dead weight")

    def test_skill_descriptions_state_triggers_not_workflow_summaries(self):
        for path in self.canonical_skills():
            description = frontmatter(path).get("description", "")
            with self.subTest(skill=path.parent.name):
                self.assertTrue(description, "description is required")
                self.assertLessEqual(len(description), 1024)
                self.assertRegex(
                    description,
                    r"(?i)\buse (?:when|at|for|before)\b",
                    "description must state when to trigger",
                )
                self.assertNotRegex(
                    description,
                    r"(?i)\b(?:then|first,|step \d|after that)\b",
                    "description must not summarise the workflow",
                )


class HostParityTest(unittest.TestCase):
    """Codex and Claude resolve the same policy from their own discovery paths."""

    def test_both_hosts_expose_the_same_skill_set(self):
        canonical = {path.parent.name for path in CANONICAL_SKILLS.glob("*/SKILL.md")}
        claude = {path.parent.name for path in CLAUDE_SKILLS.glob("*/SKILL.md")}
        self.assertEqual(canonical, claude)

    def test_claude_skills_are_redirects_to_the_canonical_body(self):
        for adapter in sorted(CLAUDE_SKILLS.glob("*/SKILL.md")):
            with self.subTest(skill=adapter.parent.name):
                targets = local_links(adapter)
                self.assertTrue(targets, "adapter must link its canonical body")
                self.assertEqual(
                    (adapter.parent / targets[0]).resolve(),
                    (CANONICAL_SKILLS / adapter.parent.name / "SKILL.md").resolve(),
                )

    def test_every_role_has_one_portable_definition_and_a_host_adapter(self):
        roles = {
            heading.strip().lower()
            for heading in re.findall(r"(?m)^##\s+(.+)$", ROLES.read_text(encoding="utf-8"))
        }
        self.assertTrue(roles, "roles.md must define the role set")
        adapters = {path.stem for path in CLAUDE_AGENTS.glob("*.md")}
        for adapter in adapters:
            with self.subTest(adapter=adapter):
                body = (CLAUDE_AGENTS / f"{adapter}.md").read_text(encoding="utf-8")
                self.assertIn("roles.md#", body, "adapter must anchor a portable role")

    def test_hosts_without_a_subagent_primitive_carry_the_covenant_in_prompt(self):
        content = compact(DELEGATION)
        self.assertRegex(
            content,
            r"host(?:s)? without|no subagent|every dispatch",
            "delegation must say how a host lacking subagents carries the covenant",
        )

    def test_budget_measures_characters_against_documented_host_limits(self):
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        for key in ("max_always_loaded_body_chars", "max_skill_listing_chars"):
            self.assertIn(key, budget)
        self.assertNotIn(
            "max_estimated_tokens_per_host",
            budget,
            "the pooled token estimate is replaced by character ceilings",
        )
        self.assertIn("limit_sources", budget, "each ceiling must name its documented source")

    def test_every_host_context_loads_the_entrypoint(self):
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        for host, paths in budget["host_contexts"].items():
            with self.subTest(host=host):
                self.assertIn(".agents/skills/act-as-mohab/SKILL.md", paths)


class DisciplineTest(unittest.TestCase):
    """Rules the published failure evidence says an agent will otherwise break."""

    def test_entrypoint_forbids_weakening_a_test_to_reach_green(self):
        self.assertRegex(compact(ENTRYPOINT), r"never [^.]*weaken[^.]*test|weaken a test")

    def test_entrypoint_provides_an_explicit_escalation_path(self):
        self.assertRegex(compact(ENTRYPOINT), r"stop and (?:report|escalate|ask)")

    def test_entrypoint_lists_self_check_red_flags_in_the_agents_own_words(self):
        content = compact(ENTRYPOINT)
        for phrase in ("should", "probably", "just this once"):
            self.assertIn(phrase, content, f"red-flag phrase missing: {phrase}")

    def test_delegation_states_the_parallel_agent_cap(self):
        self.assertRegex(compact(DELEGATION), r"(?:four|4) (?:active |concurrent |parallel )?agents")

    def test_delegation_requires_an_independent_adversarial_review_per_step(self):
        content = compact(DELEGATION)
        self.assertIn("adversarial", content)
        self.assertRegex(
            content,
            r"never the author|separate agent|independent",
            "the reviewer must be independent of the author",
        )
        self.assertRegex(
            content,
            r"refute",
            "the reviewer must be prompted to refute, not to approve",
        )


if __name__ == "__main__":
    unittest.main()
