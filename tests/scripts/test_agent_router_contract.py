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
CODEX_AGENTS = ROOT / ".codex/agents"
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

    def test_consult_skill_exists_on_every_host_discovery_path(self):
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

    def test_triage_lives_in_the_always_loaded_entrypoint(self):
        """Triage must be readable without loading the gate, or a trivial task
        pays a full skill read just to learn it was trivial."""
        sections = re.split(r"(?m)^## ", ENTRYPOINT.read_text(encoding="utf-8"))
        triage = [body for body in sections if body.lower().startswith("triage")]
        self.assertEqual(len(triage), 1, "entrypoint needs exactly one Triage section")
        content = re.sub(r"\s+", " ", triage[0]).lower()
        self.assertIn("blast radius", content)
        self.assertRegex(content, r"reversib")
        rows = [line for line in triage[0].splitlines() if line.strip().startswith("|")]
        self.assertGreaterEqual(len(rows), 5, "triage needs a depth table")
        self.assertIn("consult-first", content, "deeper triage rows must route to the gate")

    def test_gate_does_not_restate_the_triage_it_is_routed_by(self):
        """One rule, one home: the gate owns the full pass, not the triage."""
        gate = compact(CONSULT)
        self.assertNotIn("| triage result |", gate)

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

    def routing_rows(self) -> list[tuple[str, str]]:
        """Return (deliverable, target) for every real routing row."""
        rows = []
        for line in ROUTING.read_text(encoding="utf-8").splitlines():
            line = line.strip()
            if not line.startswith("|") or set(line) <= set("|- "):
                continue
            cells = [cell.strip() for cell in line.strip("|").split("|")]
            if len(cells) == 2 and cells[0].lower() not in {"deliverable in front of you", "task touches"}:
                rows.append((cells[0], cells[1]))
        return rows

    def test_routing_reference_carries_a_deterministic_table(self):
        rows = self.routing_rows()
        self.assertGreaterEqual(len(rows), 12, "routing table is missing or too small")
        for deliverable, target in rows:
            with self.subTest(row=deliverable):
                self.assertTrue(deliverable, "every row names a deliverable")
                match = re.search(r"\[[^]]+\]\(([^)]+)\)", target)
                self.assertIsNotNone(match, f"row target is not a link: {target}")
                resolved = (ROUTING.parent / match.group(1).split("#", 1)[0]).resolve()
                self.assertTrue(resolved.is_file(), f"row points at a missing file: {target}")

    def test_router_links_every_mastery_chapter_directly(self):
        linked = {(ROUTING.parent / target).resolve() for target in local_links(ROUTING)}
        chapters = sorted((REFERENCES / "shaft-mastery").glob("*.md"))
        self.assertTrue(chapters, "no mastery chapters found to route to")
        unreachable = [path.name for path in chapters if path.resolve() not in linked]
        self.assertEqual(unreachable, [], "mastery chapters not linked directly from routing.md")

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


class FrontmatterIsRealYamlTest(unittest.TestCase):
    """Hosts parse frontmatter with a real YAML parser, so the repo must too.

    The in-repo helpers here and in validate_agent_guidance.py are naive
    split-on-first-colon parsers. They happily accept `description: Use for X:
    Y`, which a YAML parser rejects with "mapping values are not allowed here"
    -- and the host, not the helper, is what decides whether a skill loads.
    """

    def frontmatter_blocks(self) -> list[tuple[Path, str]]:
        paths = [
            *CLAUDE_AGENTS.glob("*.md"),
            *CANONICAL_SKILLS.glob("*/SKILL.md"),
            *CLAUDE_SKILLS.glob("*/SKILL.md"),
            *(ROOT / ".github/skills").glob("*/SKILL.md"),
            *(ROOT / "shaft-skills").glob("*/SKILL.md"),
        ]
        blocks = []
        for path in sorted(paths):
            content = path.read_text(encoding="utf-8")
            if not content.startswith("---\n"):
                continue
            marker = content.find("\n---\n", 4)
            if marker >= 0:
                blocks.append((path, content[4:marker]))
        return blocks

    def test_every_frontmatter_block_parses_as_yaml(self):
        yaml = __import__("yaml")
        invalid = []
        for path, block in self.frontmatter_blocks():
            try:
                parsed = yaml.safe_load(block)
            except yaml.YAMLError as error:
                invalid.append(f"{path.relative_to(ROOT).as_posix()}: {type(error).__name__}")
                continue
            if not isinstance(parsed, dict) or not parsed.get("name"):
                invalid.append(f"{path.relative_to(ROOT).as_posix()}: missing name")
        self.assertEqual(invalid, [])

    def test_no_skill_name_uses_a_reserved_word(self):
        """Anthropic's skill spec rejects "anthropic" and "claude" in a name."""
        offenders = []
        for path in sorted(CANONICAL_SKILLS.glob("*/SKILL.md")):
            name = frontmatter(path).get("name", "")
            if re.search(r"anthropic|claude", name, re.I) or not re.fullmatch(r"[a-z0-9]+(-[a-z0-9]+)*", name):
                offenders.append(f"{path.parent.name}: {name}")
        self.assertEqual(offenders, [])


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

    def role_headings(self) -> set[str]:
        return {
            heading.strip().lower()
            for heading in re.findall(r"(?m)^##\s+(.+)$", ROLES.read_text(encoding="utf-8"))
        }

    def test_every_claude_adapter_anchors_a_real_portable_role(self):
        """Anchors must resolve to actual roles.md headings, so a renamed role
        breaks the build instead of silently orphaning its adapter."""
        slugs = {re.sub(r"[^a-z0-9]+", "-", name).strip("-") for name in self.role_headings()}
        adapters = sorted(CLAUDE_AGENTS.glob("*.md"))
        self.assertTrue(adapters, "no Claude role adapters found")
        for adapter in adapters:
            with self.subTest(adapter=adapter.stem):
                anchors = re.findall(r"roles\.md#([a-z0-9-]+)", adapter.read_text(encoding="utf-8"))
                self.assertTrue(anchors, "adapter must anchor a portable role")
                for anchor in anchors:
                    self.assertIn(anchor, slugs, f"anchor matches no roles.md heading: {anchor}")

    def test_both_subagent_hosts_ship_the_same_role_adapters(self):
        """Codex has a subagent primitive too -- built-in explorer/worker/default
        plus project-scoped `.codex/agents/*.toml`. Parity means the same roles
        exist on both, not that one host improvises from prose."""
        claude = {path.stem for path in CLAUDE_AGENTS.glob("*.md")}
        codex = {path.stem for path in CODEX_AGENTS.glob("*.toml")}
        self.assertEqual(claude, codex, "role adapters differ between subagent hosts")

    def test_codex_role_adapters_use_the_documented_schema(self):
        tomllib = __import__("tomllib")
        headings = self.role_headings()
        adapters = sorted(CODEX_AGENTS.glob("*.toml"))
        self.assertTrue(adapters, "no Codex role adapters found")
        for adapter in adapters:
            with self.subTest(adapter=adapter.stem):
                parsed = tomllib.loads(adapter.read_text(encoding="utf-8"))
                for key in ("name", "description", "developer_instructions"):
                    self.assertIn(key, parsed, f"missing required key: {key}")
                self.assertEqual(parsed["name"], adapter.stem, "name is the source of truth")
                instructions = parsed["developer_instructions"]
                self.assertNotIn("\r", instructions, "carriage return leaked into the string")
                self.assertIn("act-as-mohab/SKILL.md", instructions)
                self.assertIn("roles.md", instructions)
                named = [role for role in headings if role in instructions.lower()]
                self.assertTrue(named, "adapter must name a portable role")

    def test_every_portable_role_is_reachable(self):
        """A role nobody can select is dead guidance: it must either have a
        host adapter or be named by delegation as a prompt-carried role."""
        headings = [
            heading.strip()
            for heading in re.findall(r"(?m)^##\s+(.+)$", ROLES.read_text(encoding="utf-8"))
        ]
        anchored = set()
        for adapter in CLAUDE_AGENTS.glob("*.md"):
            anchored.update(re.findall(r"roles\.md#([a-z0-9-]+)", adapter.read_text(encoding="utf-8")))
        delegation = compact(DELEGATION)
        orphaned = []
        for heading in headings:
            slug = re.sub(r"[^a-z0-9]+", "-", heading.lower()).strip("-")
            if slug not in anchored and heading.lower() not in delegation:
                orphaned.append(heading)
        self.assertEqual(orphaned, [], "role is defined but nothing routes to it")

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


class CiGateIsBlockingTest(unittest.TestCase):
    """A leg in `needs` that the summary script never reads cannot block a merge.

    The summary job is the required status check and runs with `if: always()`,
    so adding a leg to `needs` alone is cosmetic: the script decides the exit
    code from a hardcoded list of result variables. This pins that every needed
    leg is actually evaluated.
    """

    WORKFLOW = ROOT / ".github/workflows/pr-gate.yml"

    def summary_step(self) -> dict:
        yaml = __import__("yaml")
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        return workflow["jobs"]["summary"], workflow["jobs"]

    def test_every_needed_leg_is_evaluated_by_the_summary(self):
        summary, jobs = self.summary_step()
        step = summary["steps"][0]
        environment, script = step["env"], step["run"]
        loop = script.split("for result in", 1)[1].split("; do", 1)[0]
        unevaluated = []
        for leg in summary["needs"]:
            if leg == "changes":
                continue  # checked separately, and fails the summary outright
            variables = [
                name for name, value in environment.items()
                if f"needs.{leg}.result" in str(value)
            ]
            if not variables:
                unevaluated.append(f"{leg}: no result variable")
            elif not any(f"${{{name}}}" in loop for name in variables):
                unevaluated.append(f"{leg}: variable never read by the loop")
        self.assertEqual(unevaluated, [], "needed legs that cannot fail the required check")

    def test_the_guidance_gate_installs_what_its_tests_import(self):
        """The runner's tool-cache Python has no PyYAML; the frontmatter test
        imports it, so the job must install it or fail on every run."""
        _, jobs = self.summary_step()
        steps = jobs["agent-guidance"]["steps"]
        commands = " ".join(str(step.get("run", "")) for step in steps)
        self.assertIn("pyyaml", commands.lower())

    def test_the_guidance_filter_covers_what_the_validator_checks(self):
        yaml = __import__("yaml")
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        filters = yaml.safe_load(workflow["jobs"]["changes"]["steps"][1]["with"]["filters"])
        guarded = set(filters["agent_guidance"])
        for required in ("AGENTS.md", ".agents/**", ".claude/**", ".memory/**", "shaft-skills/**"):
            self.assertIn(required, guarded, f"guidance filter misses {required}")
        outputs = workflow["jobs"]["changes"]["outputs"]
        self.assertIn("agent_guidance", outputs, "filter result is never exported")


class NoDuplicationTest(unittest.TestCase):
    """One rule, one home.

    The validator's duplicate-paragraph check only catches verbatim blocks of
    180+ characters, which paraphrased restatements slip under -- and a
    paraphrase that drifts is how two files end up asserting opposite policy.
    These checks close the cheaper half of that gap: identical lines, and
    orphaned files nothing routes to.
    """

    GUIDANCE_GLOBS = (
        "AGENTS.md",
        "CLAUDE.md",
        ".agents/skills/*/SKILL.md",
        ".agents/skills/act-as-mohab/references/**/*.md",
        ".claude/skills/*/SKILL.md",
        ".claude/agents/*.md",
        ".github/skills/*/SKILL.md",
        ".github/copilot-instructions.md",
        ".github/instructions/*.instructions.md",
    )
    # Host discovery requires each adapter to carry its own pointer line; there
    # is no include mechanism, so this repetition is the correct price.
    ALLOWED_REPEATS = ("Load [act-as-mohab](", "Do not restate policy here.")
    MIN_DUPLICATE_LINE_CHARS = 40

    def guidance_files(self) -> list[Path]:
        paths: set[Path] = set()
        for pattern in self.GUIDANCE_GLOBS:
            paths.update(path for path in ROOT.glob(pattern) if path.is_file())
        return sorted(paths)

    def test_no_substantive_line_is_repeated_across_guidance_files(self):
        seen: dict[str, list[str]] = {}
        for path in self.guidance_files():
            for raw in path.read_text(encoding="utf-8").splitlines():
                line = raw.strip()
                if len(line) < self.MIN_DUPLICATE_LINE_CHARS:
                    continue
                if set(line) <= set("|- "):
                    continue
                if any(line.startswith(allowed) for allowed in self.ALLOWED_REPEATS):
                    continue
                seen.setdefault(line, []).append(path.relative_to(ROOT).as_posix())
        duplicates = {
            line: sorted(set(files)) for line, files in seen.items() if len(set(files)) > 1
        }
        self.assertEqual(duplicates, {}, "identical guidance line in more than one file")

    def test_every_reference_file_is_routed_or_linked(self):
        """A reference nothing points at is guidance no agent will ever read."""
        reachable = set()
        for skill in CANONICAL_SKILLS.glob("*/SKILL.md"):
            reachable.update(read_chain_depth(skill))
        orphaned = [
            path.relative_to(ROOT).as_posix()
            for path in REFERENCES.rglob("*.md")
            if path.resolve() not in reachable
        ]
        self.assertEqual(orphaned, [], "reference file is not reachable from any skill")


class SkillsMapTest(unittest.TestCase):
    """The map is the contributor-facing summary; it must stay complete.

    A map that silently omits a surface is worse than none: a contributor reads
    it as the whole system and never learns what it left out.
    """

    MAP = CANONICAL_SKILLS / "README.md"

    def test_the_map_exists_and_is_linked_from_the_contribution_guide(self):
        self.assertTrue(self.MAP.is_file())
        contributing = (ROOT / "CONTRIBUTING.md").read_text(encoding="utf-8")
        self.assertIn(".agents/skills/README.md", contributing)

    def test_the_map_covers_every_skill_playbook_and_chapter(self):
        content = self.MAP.read_text(encoding="utf-8")
        surfaces = [
            *CANONICAL_SKILLS.glob("*/SKILL.md"),
            *(REFERENCES / "playbooks").glob("*.md"),
            *(REFERENCES / "shaft-mastery").glob("*.md"),
            *(path for path in REFERENCES.glob("*.md")),
        ]
        missing = []
        for path in sorted(surfaces):
            target = path.relative_to(CANONICAL_SKILLS).as_posix()
            if target not in content:
                missing.append(target)
        self.assertEqual(missing, [], "skills map omits a surface")

    def test_the_map_links_resolve(self):
        broken = [
            target
            for target in local_links(self.MAP)
            if not (self.MAP.parent / target.split("#", 1)[0]).exists()
        ]
        self.assertEqual(broken, [])

    def test_the_map_tells_agents_to_load_the_entrypoint_rather_than_itself(self):
        content = compact(self.MAP)
        self.assertIn("act-as-mohab/skill.md", content)
        self.assertRegex(content, r"do not work from this file|map, not the territory")


class RetrievalParityTest(unittest.TestCase):
    """Both hosts must gate memory writes the same way.

    Codex restricts the shaft-memory surface to four tools and prompts before
    `remember_memory`. Claude reached the same server with no gate at all, so a
    write that one host asks about the other performed silently -- the same
    policy resolving two different ways.
    """

    def test_memory_writes_are_gated_on_every_host(self):
        tomllib = __import__("tomllib")
        codex = tomllib.loads((ROOT / ".codex/config.toml").read_text(encoding="utf-8"))
        remember = codex["mcp_servers"]["shaft-memory"]["tools"]["remember_memory"]
        self.assertEqual(remember["approval_mode"], "prompt")

        settings = json.loads((ROOT / ".claude/settings.json").read_text(encoding="utf-8"))
        permissions = settings["permissions"]
        self.assertIn("mcp__shaft-memory__remember_memory", permissions.get("ask", []))
        self.assertNotIn("mcp__shaft-memory__remember_memory", permissions.get("allow", []))

    def test_both_hosts_declare_the_same_retrieval_servers(self):
        tomllib = __import__("tomllib")
        codex = set(tomllib.loads((ROOT / ".codex/config.toml").read_text(encoding="utf-8"))["mcp_servers"])
        claude = set(json.loads((ROOT / ".mcp.json").read_text(encoding="utf-8"))["mcpServers"])
        # tomllib nests `[mcp_servers.x.tools.y]` under x, so these are the
        # server names only.
        self.assertEqual(codex, claude, "retrieval servers differ between hosts")

    def test_routing_states_when_each_knowledge_store_is_required(self):
        """"Use every source" is unenforceable; a trigger per store is not."""
        sections = re.split(r"(?m)^## ", ROUTING.read_text(encoding="utf-8"))
        knowledge = [body for body in sections if body.lower().startswith("knowledge")]
        self.assertEqual(len(knowledge), 1)
        content = re.sub(r"\s+", " ", knowledge[0]).lower()
        for store in ("native memory", "mempalace", "graphify", "rg"):
            self.assertIn(store, content, f"knowledge table omits {store}")
        self.assertIn("query it when", content, "each store needs a stated trigger")
        self.assertIn("degraded", content, "unavailable stores must be reported")

    def test_the_learning_loop_routes_each_outcome_to_one_destination(self):
        sections = re.split(r"(?m)^## ", ENTRYPOINT.read_text(encoding="utf-8"))
        loop = [body for body in sections if body.lower().startswith("learning loop")]
        self.assertEqual(len(loop), 1, "entrypoint needs exactly one Learning loop section")
        content = re.sub(r"\s+", " ", loop[0]).lower()
        for destination in ("native memory", "mempalace", "graphify", "issue"):
            self.assertIn(destination, content, f"learning loop omits {destination}")
        self.assertRegex(content, r"nothing durable is a valid result|no durable learning")
        self.assertRegex(content, r"search before writing|search first", "must prevent duplicates")


class SoloOrOrchestrateTest(unittest.TestCase):
    """Whether main thread implements has exactly one rule, in one place.

    Two statements previously disagreed: "the orchestrator never implements"
    against "do the work yourself". Both are right in their own mode and wrong
    as absolutes, so the discriminator -- how many independent work streams the
    session owns -- lives in the entrypoint and every other file defers to it.
    """

    GUIDANCE_GLOBS = (
        "AGENTS.md",
        "CLAUDE.md",
        ".agents/skills/*/SKILL.md",
        ".agents/skills/act-as-mohab/references/**/*.md",
        ".claude/agents/*.md",
    )

    def section(self) -> str:
        sections = re.split(r"(?m)^#{2,3} ", ENTRYPOINT.read_text(encoding="utf-8"))
        found = [body for body in sections if body.lower().startswith("solo or orchestrate")]
        self.assertEqual(len(found), 1, "entrypoint needs exactly one solo-or-orchestrate rule")
        return found[0]

    def test_the_rule_keys_on_concurrent_work_streams(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertIn("independent work streams", content)
        self.assertIn("one", content)
        self.assertIn("two or more", content)
        rows = [line for line in self.section().splitlines() if line.strip().startswith("|")]
        self.assertGreaterEqual(len(rows), 4, "the rule needs a mode table")

    def test_solo_mode_forbids_delegating_the_work(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertRegex(content, r"implement it yourself")
        self.assertRegex(content, r"do not delegate")

    def test_orchestrated_mode_keeps_main_thread_out_of_the_edits(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertRegex(content, r"implement nothing yourself")
        self.assertRegex(content, r"reachable", "the rule must state why main thread stays free")
        self.assertRegex(content, r"four", "orchestrated mode states the concurrency cap")

    def test_review_never_flips_the_mode(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertRegex(
            content,
            r"reviewer is not a work stream|never makes a solo session",
            "a review must not be counted as a second work stream",
        )

    def test_no_other_file_states_an_unconditional_implement_rule(self):
        """Every other mention must be scoped to a mode or defer to the rule."""
        absolute = re.compile(
            r"(?:orchestrator|main thread)[^.]{0,80}\bnever implements\b"
            r"|\bit does not implement\b(?![^.]{0,60}mode)",
            re.I,
        )
        offenders = []
        for pattern in self.GUIDANCE_GLOBS:
            for path in ROOT.glob(pattern):
                if not path.is_file() or path.resolve() == ENTRYPOINT.resolve():
                    continue
                text = re.sub(r"\s+", " ", path.read_text(encoding="utf-8"))
                if absolute.search(text):
                    offenders.append(path.relative_to(ROOT).as_posix())
        self.assertEqual(offenders, [], "unconditional implement rule outside the entrypoint")


class DisciplineTest(unittest.TestCase):
    """Rules the published failure evidence says an agent will otherwise break."""

    def test_entrypoint_forbids_weakening_a_test_to_reach_green(self):
        self.assertRegex(compact(ENTRYPOINT), r"never [^.]*weaken[^.]*test|weaken a test")

    def test_entrypoint_provides_an_explicit_escalation_path(self):
        self.assertRegex(compact(ENTRYPOINT), r"stop and (?:report|escalate|ask)")

    def test_entrypoint_lists_self_check_red_flags_in_the_agents_own_words(self):
        """Scoped to the Red flags section: asserting "should" appears anywhere
        in the file would pass on almost any English prose."""
        sections = re.split(r"(?m)^## ", ENTRYPOINT.read_text(encoding="utf-8"))
        red_flags = [body for body in sections if body.lower().startswith("red flags")]
        self.assertEqual(len(red_flags), 1, "entrypoint needs exactly one Red flags section")
        content = re.sub(r"\s+", " ", red_flags[0]).lower()
        for phrase in ("should", "probably", "just this once", "close enough"):
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
