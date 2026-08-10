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

import ast
import json
import re
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts.ci.validate_agent_guidance import (
    expand_reported_globs,
    require_glob_list,
    validate_repository,
)

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
LENS = REFERENCES / "verification-gap-lens.md"
CONSULT = REFERENCES / "consult-first.md"
BUDGET = ROOT / "scripts/ci/agent_guidance_budget.json"

# agentskills.io/specification: keep SKILL.md under 500 lines so a host never
# truncates it, and keep file references one level deep from SKILL.md.
MAX_SKILL_BODY_LINES = 500
# Hops after SKILL.md before real content. SKILL.md -> routing.md -> playbook
# is the deepest chain the router is allowed to build.
MAX_READ_CHAIN_HOPS = 2
# A file whose entire body is a pointer costs a read and teaches nothing.
REDIRECT_STUB_MAX_CHARS = 250

# Sections that own a pinned clause, by heading prefix.
IRON_LAWS = "iron laws"
RED_FLAGS = "red flags"
TDD = "test-driven development"
GAP_SHAPES = "the four gap shapes"
BINDING = "proving a check binds"
REVIEW = "independent adversarial review"

# Every clause this repository's guidance is not allowed to lose, as
# (file, section, literal).
#
# The third element is verbatim shipped text, so this is a copy -- but the
# direction is what matters: the copy is the specification and the file is
# checked against it, never the reverse. What the mutation tests never hold is
# a copy of the *surrounding* text; they splice into whatever the shipped file
# says at run time, so a pin cannot pass by agreeing with itself. Each clause's
# justification is in the named test that asserts it.
PINNED_CLAUSES: tuple[tuple[Path, str, str], ...] = (
    (ENTRYPOINT, IRON_LAWS, "evidence over inference"),
    (ENTRYPOINT, IRON_LAWS, "inspect or run before claiming"),
    (ENTRYPOINT, IRON_LAWS, "no production code before an observed failing test"),
    (ENTRYPOINT, IRON_LAWS, "never weaken, delete, or rewrite a test to reach green"),
    (ENTRYPOINT, IRON_LAWS, "never claim a check you did not run"),
    # #4545 defect 1. Law 6 required a review per "behavior-changing step" and
    # nothing defines a step, so the rule had no countable trigger and nobody
    # could say how many a change owed. Measured: #4539 carried 24 commits and
    # eleven new enforcement rules and received zero reviews until the owner
    # asked for one. The per-pull-request floor is weaker in principle and
    # strictly stronger in practice, because a pull request can be counted.
    (ENTRYPOINT, IRON_LAWS, "every pull request gets at least one before it is armed"),
    (ENTRYPOINT, TDD, "only an expected assertion failure"),
    (ENTRYPOINT, TDD, "a pass or setup, syntax, or environment error is not red"),
    (ENTRYPOINT, TDD, "revert that new code and restart"),
    (ENTRYPOINT, TDD, "asserts nothing, prints instead of asserting, or mocks the behavior under test"),
    (ENTRYPOINT, TDD, "never backfill tests after implementation"),
    (ENTRYPOINT, RED_FLAGS, "the check covers it"),
    (LENS, GAP_SHAPES, "unbound-check gap"),
    (LENS, BINDING, "apply it, run it, read the failure, revert"),
    # #4548. Independence was stated and the *subject* was not: a read-only
    # reviewer owns no worktree, so unsaid it reads whatever branch the shared
    # tree happens to hold. Both halves are pinned, because a reviewer that is
    # independent of the author and pointed at the wrong revision produces the
    # most confident wrong answer available -- a clean no-match that looks
    # exactly like a real absence.
    (DELEGATION, REVIEW, "separate agent instance, never the author"),
    (DELEGATION, REVIEW, "the exact revision under review"),
    (DELEGATION, REVIEW, "first action confirms the revision is what it thinks it is"),
)


class ActiveMemoryContractTest(unittest.TestCase):
    def test_active_memory_names_only_the_single_repo_local_skill_layout(self):
        memory_root = ROOT / ".memory"
        constraint = json.loads((
            memory_root / "memory/constraints/approved-designs-proceed-with-most-intelligent-consultation-without-repeat-user-approval.json"
        ).read_text(encoding="utf-8"))
        gotcha = json.loads((
            memory_root / "memory/gotchas/marketplace-json-skills-entries-must-list-individual-skill-dirs-never-for-vercel-skills-cli-interop.json"
        ).read_text(encoding="utf-8"))

        self.assertEqual(constraint["status"], "active")
        self.assertNotIn(
            ".agents/skills/consult-first/SKILL.md",
            constraint["facets"]["applies_to"],
        )
        gotcha_body = (memory_root / gotcha["body_path"]).read_text(encoding="utf-8")
        self.assertNotIn("4 SKILL.md today", gotcha_body)
        self.assertIn("one repo-local", gotcha_body.lower())


class PlanArtifactRoutingTest(unittest.TestCase):
    def test_plan_artifacts_use_repository_safe_destinations(self):
        content = compact(CONSULT)

        self.assertIn("target github issue", content)
        self.assertIn("transient", content)
        self.assertIn("existing operational-guidance location", content)
        self.assertIn("docs/superpowers", content)
        self.assertRegex(content, r"never (?:create|write).{0,80}docs/superpowers")


# How many rules each pinned section is supposed to have (#4534).
#
# `numbering_defects` reads a list as 1..N, so it sees an item removed from the
# middle -- 1, 2, 4 -- and cannot see one removed from the end, because 1, 2 is
# as contiguous as 1, 2, 3. The clause pins were meant to cover that, and here
# they do not: `PINNED_CLAUSES` binds iron laws 2, 3, 4 and 5, so laws 1 and 6
# have no pin, and law 6 is the last item. Deleting the rule that requires an
# independent adversarial review passed the numbering check and every clause
# pin at once, reported by nothing.
#
# A count rather than two more clause pins, for two reasons. A clause pin binds
# words, and words get rewritten for good reasons, so pinning every law by text
# makes ordinary editing expensive and teaches authors to route around the pins.
# And a count fails in the other direction too: a law *added* without review is
# a policy change with no gate in front of it, and no clause pin can ask about
# a rule nobody has written down yet.
#
# A section with no ordered list has no entry, and `test_every_pinned_section_
# with_rules_is_counted` keeps that from becoming a place to hide: it fails if
# a pinned section grows a list nobody counted, and if a count names a section
# that is not pinned.
PINNED_RULE_COUNTS: dict[tuple[Path, str], int] = {
    (ENTRYPOINT, IRON_LAWS): 6,
    (ENTRYPOINT, TDD): 3,
    (LENS, GAP_SHAPES): 4,
}

# Every required-action rule, against what actually enforces it (#4541).
#
# The harness tests verify that the rulebook is intact -- present, linked,
# unqualified, correctly numbered, not superseded. Not one of them verifies
# that a rule was *obeyed*, and a green suite therefore reads as "the harness
# is working" when it means "the harness's text is undamaged". That gap
# matters most for exactly the rules that decay with context.
#
# Three honest statuses, and the third is not a failure:
#   performed   -- a hook does it, so no adherence is required at all
#   gated       -- a PreToolUse rule REFUSES the call until it has happened
#   reports     -- a Stop rule interrupts the turn but cannot refuse anything
#   prose-only  -- no practical mechanism, with the reason recorded here
#
# `reports` exists because the first version had only `gated`, and five rows
# claimed it for rules whose own docstrings say the opposite -- R16 "not a hard
# gate", R20 "reports rather than refuses", R21 "reports, never refuses". A
# registry whose whole deliverable is honest classification cannot round
# "interrupts once" up to "refuses". The distinction is mechanical, not a
# judgement: a gated rule is dispatched from `run_pretooluse` and returns a
# denial; a reporting rule is dispatched from `run_stop`.
#
# `law` ties a row to a numbered iron law. Every law needs exactly one row and
# no row may name a law that does not exist, so adding a seventh iron law fails
# this until it is classified -- which is what makes the registry bind on future
# expansion rather than only on today's rules. Rows with `law: None` cover
# required actions stated elsewhere in the entrypoint.
#
# `mechanism` names a symbol that must exist in `guard.py`. `reason` is required
# for prose-only rows and is checked for substance, because "n/a" is non-empty
# and says nothing.
#
# `scope` is the fourth field and the newest, from #4567 item 7. A status says
# what *kind* of enforcement a rule has and deliberately says nothing about how
# much of the repository that enforcement reaches, and for law 3 the difference
# is most of the work: `_PRODUCTION_PATH` matches `src/main/` only, so R12
# could not fire once on the 3138-line branch #4554, where `guard.py` and its
# tests were 76% of the diff. `gated` was true and read as more than it meant.
#
# It is a separate field rather than a qualified status because the status
# vocabulary is mechanical -- `REGISTRY_STATUSES` is a closed set and
# `test_every_row_is_honestly_classified` keys on `== "gated"` to look the
# mechanism up in `run_pretooluse` -- so a value like "gated (src/main only)"
# would not be a status at all, it would be a status that matches nothing and
# quietly exempts its own row from both dispatch checks. `scope` qualifies a
# true status the way `reason` qualifies `prose-only`, and it is pinned against
# what the mechanism actually matches rather than left as prose.
#
# The recorded gap is deliberately not closed by widening `_PRODUCTION_PATH`.
# That would gate every guidance and script edit behind an observed test run --
# a rule firing on correct work, the shape this whole registry exists to stop
# claiming.
#
# Deliberately NOT required: that prose-only be empty, or that the registry be
# non-empty. `gotcha.a-check-whose-healthy-end-state-is-unreachable-is-a-check-
# that-will-be-weakened` records what that produces -- guards that are each
# defensible and leave no legal configuration, where the cheapest exit is
# deleting the guard. Row count here is derived from the entrypoint, never
# floored by this file.
REQUIRED_ACTION_REGISTRY: tuple[dict, ...] = (
    {
        "law": 1,
        "rule": "consult before acting; triage first",
        "status": "prose-only",
        "reason": (
            "Triage is a judgement about blast radius and reversibility. A hook sees "
            "one tool call and cannot know whether the agent weighed anything, and one "
            "that demanded a triage artifact would be satisfied by writing the artifact "
            "rather than by doing the thinking."
        ),
    },
    {
        "law": 2,
        "rule": "evidence over inference -- inspect or run before claiming",
        "status": "prose-only",
        "reason": (
            "No hook observes an assertion. The claim and the evidence for it are both "
            "prose in the same message, and a check that could tell them apart would be "
            "reading for meaning, which every lexical attempt in this repository has "
            "lost -- see the superseded-policy scan's three failed review rounds."
        ),
    },
    {
        "law": 3,
        "rule": "no production code before an observed failing test",
        "status": "gated",
        "mechanism": "check_r12_test_before_production",
        "scope": (
            "Compiled source under a module's src/main/ only. Everything else the "
            "entrypoint exempts itself -- but that exemption is by directory, so it "
            "also covers scripts/agents/guard.py, a 3,800-line enforcement program "
            "whose defects this harness keeps finding. On #4554 R12 could not fire "
            "once: zero files under src/main/, against 2,214 lines of guard and test "
            "churn. Outside src/main/ law 3 is enforced by nothing here."
        ),
    },
    {
        "law": 4,
        "rule": "never weaken, delete, or rewrite a test to reach green",
        "status": "prose-only",
        "reason": (
            "A weakened assertion and a corrected one are the same edit to a hook. "
            "Distinguishing them needs the intent behind the change, and gating test "
            "edits at all would block the corrections this repository legitimately "
            "makes -- a gate firing on correct work, which is the shape that gets "
            "guards deleted."
        ),
    },
    {
        "law": 5,
        "rule": "never claim a check you did not run",
        "status": "prose-only",
        "reason": (
            "The session ledger records that a test run happened, never that the run "
            "supports the sentence an agent then wrote. Binding those together needs "
            "the claim parsed and matched to the evidence, which is the same semantic "
            "read law 2 fails on."
        ),
    },
    {
        "law": 6,
        "rule": "independent adversarial review before the next step, and once per pull request before arming",
        "status": "gated",
        "mechanism": "check_r15_review_before_arming",
    },
    {
        "law": None,
        "rule": "main thread receives native-Memory preflight before broad discovery",
        "status": "performed",
        "mechanism": "_standing_constraints",
    },
    {
        "law": None,
        "rule": "delegates query the stores before broad discovery",
        "status": "prose-only",
        "reason": (
            "SessionStart belongs to the main thread. Role adapters deliver the entrypoint, "
            "but a hook cannot observe whether a delegate used the retrieved context."
        ),
    },
    {
        "law": None,
        "rule": "start from a fresh base; never edit on the default branch",
        "status": "gated",
        "mechanism": "check_r19_fresh_base",
    },
    {
        "law": None,
        "rule": "push anything a remote has never seen before deleting it",
        "status": "gated",
        "mechanism": "check_r13_push_before_delete",
    },
    {
        "law": None,
        "rule": "never discard uncommitted work with a hard reset",
        "status": "gated",
        "mechanism": "check_r14_hard_reset",
    },
    {
        "law": None,
        "rule": "run the learning loop before reporting done",
        "status": "reports",
        "mechanism": "check_r16_learning_loop",
    },
    {
        "law": None,
        "rule": "arm auto-merge once the review gate passes, then watch until merged",
        "status": "reports",
        "mechanism": "check_r17_unarmed_pull_request",
    },
    {
        "law": None,
        "rule": "push before the session can end; unpushed work is unrecoverable",
        "status": "reports",
        "mechanism": "check_r18_unpushed_work",
    },
    {
        "law": None,
        "rule": "keep the deployed harness in step with the tracked one",
        "status": "reports",
        "mechanism": "check_r20_user_harness_drift",
    },
    {
        "law": None,
        "rule": "put in-flight run state on the tracker when work is delegated",
        "status": "reports",
        "mechanism": "check_r21_run_state_not_recorded",
    },
    {
        "law": None,
        "rule": "record an owner decision, and a sequencing constraint, where the next agent will find it",
        "status": "prose-only",
        "reason": (
            "The remaining half of #4536. Delegation is a tool call a hook can see; "
            "'an owner decided something in conversation' and 'a sequencing "
            "constraint was discovered' are not events any hook observes, so they "
            "stay prose and the issue stays open rather than being counted as done."
        ),
    },
    {
        "law": None,
        "rule": "complete the executable specification before RED/GREEN and record it remotely before commit",
        "status": "prose-only",
        "reason": (
            "A deterministic local hook cannot infer semantic risk or prove that a "
            "remote issue comment preceded RED/GREEN or a commit before a diff or "
            "action exists. Runtime enforcement caused false positives and new trust "
            "failures, so CI enforces the source-controlled structure and wording only."
        ),
    },
)

REGISTRY_STATUSES = frozenset({"performed", "gated", "reports", "prose-only"})

# Words that leave every pinned word in place and turn the rule into a
# suggestion.
#
# This is a denylist, so it is incomplete by construction and the tests below
# say so rather than implying coverage. Every entry past the first block was
# added because it was demonstrated to walk through the list, not guessed at.
# It stays as tight as the demonstrations allow: the TDD section legitimately
# carves out documentation with "may skip test-first", and a list wide enough
# to catch that phrasing would push an author to obscure a correct exemption in
# order to pass -- the failure this check exists to prevent, inverted.
EXEMPTION_MARKERS = (
    "unless",
    "except",
    "exempt",
    "optional",
    "where practical",
    "when time allows",
    "if time",
    "at your discretion",
    "best effort",
    "use judgment",
    "advisory",
    "guideline rather",
    "where possible",
    "as appropriate",
    "not strictly",
    "in most cases",
    "need not",
    "waived",
    "does not apply",
    "you may skip",
    "judgement",
)


def clause_pattern(clause: str) -> str:
    """Whitespace-tolerant literal matcher; the entrypoint wraps clauses."""
    return r"\s+".join(re.escape(word) for word in clause.split())


def headed_sections(source: str, heading: str) -> list[str]:
    """Return every level-2 or level-3 section whose heading starts with `heading`."""
    return [
        body
        for body in re.split(r"(?m)^#{2,3} ", source)
        if body.lower().startswith(heading)
    ]


def rule_blocks(section: str) -> list[str]:
    """Split a section into its rule units: list items and paragraphs.

    This is the scope an exemption is judged against, and neither obvious
    alternative works. Sentence scope misses `"...did not run. Routine checks
    are exempt."`, because the escape hatch is a *new* sentence. Section scope
    reports the TDD section's legitimate documentation carve-out. The rule item
    is the unit an author actually edits, and both weakenings named in #4467
    land inside the very law they weaken.
    """
    parts = re.split(r"(?m)^(?=\s*(?:\d+\.|[-*])\s)|\n\s*\n", section)
    return [re.sub(r"\s+", " ", part).strip().lower() for part in parts if part and part.strip()]


def ordered_list_runs(section: str) -> list[list[int]]:
    """Return the numbers of each top-level ordered list in `section`.

    Both delimiters Markdown allows are read, because a check an unrelated
    formatting choice can switch off is worse than no check: the first cut of
    this matched a literal `.`, so rewriting a list as `1)` made it inert while
    the docstring went on claiming coverage.

    A run ends at the first top-level line that is not an item -- prose, a
    table, a bullet -- and not at the next `1.`. Splitting on the value was the
    other half of the same mistake: `1. 1. 1.` is legal Markdown that renders
    as 1, 2, 3, and read as three runs of one it reported nothing, so a list
    written that way could lose a rule silently. Read as one run it is
    `[1, 1, 1]` against `[1, 2, 3]`, which is the honest reading -- a list whose
    numbers come from the renderer cannot show a deletion.

    Indented items are sub-lists and fenced blocks are examples, so neither is
    collected.
    """
    runs: list[list[int]] = []
    open_run = False
    fenced = False
    for line in section.splitlines():
        if line.lstrip().startswith("```"):
            fenced = not fenced
            continue
        if fenced:
            continue
        match = re.match(r"(\d+)[.)]\s", line)
        if match:
            if not open_run:
                runs.append([])
                open_run = True
            runs[-1].append(int(match.group(1)))
        elif line.strip() and not line[:1].isspace():
            open_run = False
    return runs


def numbering_defects(section: str, where: str) -> list[str]:
    """Report an ordered list in `section` that is not numbered 1..N.

    A law can be unmade without deleting a word of it: move it out of the
    numbered list into prose below the list, inside the same section, and it is
    still present, still in its owning section, still unqualified, so every
    pin above stays quiet (#4490). What changes is the numbering -- 1, 2, 3, 4,
    6 -- and that is structural, so it is checked structurally rather than by
    growing a grammar over English prose (#4484). The same check catches a
    duplicated number and a list that starts anywhere but 1.

    It is not a replacement for the clause pins and does not overlap them.
    Deleting a rule and closing the gap leaves a contiguous list, which is why
    `test_deleting_a_law_and_renumbering_cleanly_is_still_reported` exists.
    """
    defects = []
    for run in ordered_list_runs(section):
        expected = list(range(1, len(run) + 1))
        if run != expected:
            defects.append(f"{where}: rule numbering is {run}, not {expected}")
    return defects


def count_defects(section: str, where: str, key: tuple[Path, str]) -> list[str]:
    """Report a pinned section whose rule count is not the one on record.

    Counts every item across the section's ordered lists rather than the
    longest run, so splitting one list into two does not quietly halve the
    figure the check compares against.

    Silent for a section with no pinned count. That is not a gap to be closed
    here: `test_every_pinned_section_with_rules_is_counted` is what makes the
    silence deliberate, by failing when a pinned section has rules and no
    count.
    """
    expected = PINNED_RULE_COUNTS.get(key)
    if expected is None:
        return []
    found = sum(len(run) for run in ordered_list_runs(section))
    if found != expected:
        return [f"{where}: rule count is {found}, not the pinned {expected}"]
    return []


def clause_defects(overrides: dict[Path, str] | None = None) -> list[str]:
    """Report what is wrong with a pinned section: its clauses and its numbering.

    Reads each pinned file from disk unless `overrides` supplies a mutated
    body, which is what lets the mutation tests below prove the pins bind
    without ever holding a copy of the text they protect.

    Presence alone was the whole check until #4467: a pin says a phrase is
    there and says nothing about what surrounds it, so an edit that *adds* an
    escape hatch next to the clause passed every pin while gutting the law.
    Presence plus phrasing was the whole check until #4490, which is the same
    lesson a third time: both look at the clause, and neither looks at whether
    it is still one of the rules. `numbering_defects` is that third question,
    asked once per pinned section rather than once per clause.
    """
    overrides = overrides or {}
    defects = []
    numbered: set[tuple[Path, str]] = set()
    for path, heading, clause in PINNED_CLAUSES:
        where = f"{path.name}:{heading!r}"
        source = overrides.get(path, path.read_text(encoding="utf-8"))
        sections = headed_sections(source, heading)
        if len(sections) != 1:
            # Every defect names its clause. A defect that did not was filtered
            # out by callers keying on the clause, so a renamed heading emptied
            # the pins silently -- see the rename test below.
            defects.append(
                f"{where}: expected one section, found {len(sections)}; "
                f"clause unverifiable: {clause!r}"
            )
            continue
        if (path, heading) not in numbered:
            numbered.add((path, heading))
            defects.extend(numbering_defects(sections[0], where))
            defects.extend(count_defects(sections[0], where, (path, heading)))
        pattern = clause_pattern(clause)
        matched = [block for block in rule_blocks(sections[0]) if re.search(pattern, block)]
        if not matched:
            defects.append(f"{where}: clause missing: {clause!r}")
            continue
        for block in matched:
            hits = [marker for marker in EXEMPTION_MARKERS if marker in block]
            if hits:
                defects.append(f"{where}: {clause!r} qualified by {hits[0]!r}")
    return defects


def patched_symbols(node: ast.AST) -> set[str]:
    """Symbols a `patch(...)` call anywhere inside `node` replaces with a mock.

    The last dotted segment only: `patch("scripts.agents.guard._x")` and
    `patch("other.module._x")` both name `_x`, and a test class asserting on
    the wrong `_x` is the same vacuous pass either way.
    """
    found: set[str] = set()
    for call in ast.walk(node):
        if not isinstance(call, ast.Call):
            continue
        function = call.func
        target = None
        if isinstance(function, ast.Attribute) and function.attr == "object":
            if len(call.args) > 1 and isinstance(call.args[1], ast.Constant):
                target = call.args[1].value
        else:
            name = (
                function.attr
                if isinstance(function, ast.Attribute)
                else getattr(function, "id", "")
            )
            if name == "patch" and call.args and isinstance(call.args[0], ast.Constant):
                target = call.args[0].value
        if isinstance(target, str) and target:
            found.add(target.rsplit(".", 1)[-1])
    return found


def asserted_calls(node: ast.AST) -> set[str]:
    """Symbols *invoked* inside a test assertion.

    Invoked, not merely mentioned, and that narrowing is what keeps the check
    free of false positives. `self.assertIn("_helper(cwd)", inspect.getsource(
    ...))` names the symbol in a string and asserts on the shipped source, not
    on the mock; `guard.check_r20(...)` reaching the patched helper internally
    is the entire point of patching it. Only a direct call of the patched
    symbol inside the assertion makes the assertion observe the mock's own
    `return_value`.
    """
    called: set[str] = set()
    for outer in ast.walk(node):
        if isinstance(outer, ast.Assert):
            arguments = [outer.test]
        elif (
            isinstance(outer, ast.Call)
            and isinstance(outer.func, ast.Attribute)
            and outer.func.attr.startswith("assert")
            and isinstance(outer.func.value, ast.Name)
            and outer.func.value.id == "self"
        ):
            arguments = [*outer.args, *(keyword.value for keyword in outer.keywords)]
        else:
            continue
        for argument in arguments:
            for inner in ast.walk(argument):
                if not isinstance(inner, ast.Call):
                    continue
                target = inner.func
                if isinstance(target, ast.Attribute):
                    called.add(target.attr)
                elif isinstance(target, ast.Name):
                    called.add(target.id)
    return called


def setup_patch_defects(source: str, where: str) -> list[str]:
    """Report a test asserting on a symbol its own class's `setUp` patched off.

    #4567 section 4.2, and the check that would have caught the finding behind
    round two's DO NOT MERGE verdict at commit time. The shape:
    `UserHarnessDriftStopGateTest.setUp` patched `_branch_edits_harness_sources`
    to a fixed `False` so the class's real subject stayed deterministic, and a
    test in the same class then asserted on `_branch_edits_harness_sources`
    itself. It observed the mock, so it passed whatever the helper did, and it
    went on passing through the round-one fix.

    Reproduced across five revisions of `tests/scripts/test_guard_lifecycle.py`
    before this was written: 1 finding at `d346bac4b0` (build) and at
    `254a830710` (the round-one fix that missed it), 0 at `5f6c00fb76` (the
    real fix), `1981a3fded` and `origin/main`. Zero false positives over all
    934 test methods in the 48 modules under `tests/scripts/`.

    A test that re-patches the symbol itself is exempt: it has taken control of
    the value, which is how the legitimate "this is about the suppression"
    tests in that very class are written.

    Scanned across every test module rather than the one that produced the
    defect, per `decision.fix-a-live-state-test-defect-by-pinning-the-whole-
    rule-class-with-an-equality-check-never-by-naming-the-rule-that-bit-you`:
    naming the instance is how the same defect walked back in one rule later.
    """
    defects: list[str] = []
    for klass in ast.walk(ast.parse(source)):
        if not isinstance(klass, ast.ClassDef):
            continue
        shared: set[str] = set()
        for decorator in klass.decorator_list:
            shared |= patched_symbols(decorator)
        for member in klass.body:
            if isinstance(member, ast.FunctionDef) and member.name in ("setUp", "setUpClass"):
                shared |= patched_symbols(member)
        if not shared:
            continue
        for member in klass.body:
            if not isinstance(member, ast.FunctionDef) or not member.name.startswith("test"):
                continue
            own = patched_symbols(member)
            for decorator in member.decorator_list:
                own |= patched_symbols(decorator)
            for symbol in sorted(asserted_calls(member) & (shared - own)):
                defects.append(
                    f"{where}: {klass.name}.{member.name} asserts on {symbol}(), "
                    f"which {klass.name}.setUp patched off -- the assertion reads "
                    "the mock's own return_value and cannot fail"
                )
    return defects


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


def glob_files(root: Path, patterns: tuple[str, ...]) -> list[Path]:
    """
    Expand a fixed list of guidance globs, raising if any pattern misses.

    Callers use this to build the file set an ``all(...)``-shaped assertion
    (no duplicate line, no unscoped absolute) checks across every configured
    surface. A pattern that resolves to zero files -- a moved or renamed
    directory, a typo -- would otherwise shrink that set silently: "for every
    file in []" is trivially true, so the assertion keeps passing having
    checked nothing for that pattern (#4481). Raising here is what makes a
    moved surface fail the test instead of dropping out of it unnoticed. A
    bare ``assert`` would do the same today, but it compiles away under
    ``python -O``/``PYTHONOPTIMIZE`` (bandit B101) -- this is the only
    enforcement this helper has, so it must survive that flag even though
    nothing in this repository currently sets it.
    """
    paths: set[Path] = set()
    for pattern in patterns:
        matched = [path for path in root.glob(pattern) if path.is_file()]
        if not matched:
            raise AssertionError(f"guidance glob matched no files: {pattern}")
        paths.update(matched)
    return sorted(paths)


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
    """The internal deliberation gate runs before any task-specific work."""

    def test_consult_gate_is_internal_to_the_single_discovery_skill(self):
        self.assertTrue(CONSULT.is_file())
        self.assertFalse((CANONICAL_SKILLS / "consult-first/SKILL.md").exists())
        self.assertFalse((CLAUDE_SKILLS / "consult-first/SKILL.md").exists())

    def test_internal_consult_gate_carries_the_substantive_body(self):
        self.assertGreater(len(markdown_body(CONSULT)), 1000)
        targets = [(ENTRYPOINT.parent / target).resolve() for target in local_links(ENTRYPOINT)]
        self.assertIn(CONSULT.resolve(), targets)

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

    def test_entrypoint_requires_a_complete_research_receipt_before_implementation(self):
        content = compact(ENTRYPOINT)
        required = (
            "read live files",
            "load the routed skill",
            "native memory",
            "mempalace",
            "graphify",
            "authoritative online research",
            "compare proven approaches",
            "record a concrete plan",
        )
        positions = [content.index(item) for item in required]
        self.assertEqual(positions, sorted(positions))
        self.assertRegex(content, r"missing .{0,80} blocks implementation")

    def test_caveman_preserves_exact_meaning_before_compression(self):
        content = compact(ENTRYPOINT)
        caveman = content.split("### caveman", 1)[1].split("### ponytail", 1)[0]
        for exact in ("negation", "numbers", "units", "user language", "commands", "errors"):
            self.assertIn(exact, caveman)

    def test_ponytail_has_separate_reuse_native_dependency_and_code_rungs(self):
        content = compact(ENTRYPOINT)
        ponytail = content.split("### ponytail", 1)[1].split("### test-driven", 1)[0]
        rungs = (
            "reuse the existing owner",
            "standard library",
            "native platform",
            "already-installed dependency",
            "minimum new code",
        )
        positions = [ponytail.index(rung) for rung in rungs]
        self.assertEqual(positions, sorted(positions))

    def test_every_ponytail_marker_names_its_ceiling_and_upgrade_trigger(self):
        markers = []
        for path in ROOT.rglob("*"):
            if (
                not path.is_file()
                or path.suffix not in {".java", ".py"}
                or any(part in {".git", "target", "graphify-out"} for part in path.parts)
            ):
                continue
            try:
                lines = path.read_text(encoding="utf-8").splitlines()
            except (OSError, UnicodeDecodeError):
                continue
            for index, line in enumerate(lines):
                if "ponytail:" in line.lower() and path != Path(__file__).resolve():
                    markers.append((path, index, " ".join(lines[index:index + 4]).lower()))
        self.assertTrue(markers)
        for path, line, marker in markers:
            with self.subTest(path=path, line=line + 1):
                self.assertIn("ceiling:", marker)
                self.assertIn("upgrade trigger:", marker)


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

    def test_mechanical_helper_has_a_host_adapter_on_both_subagent_hosts(self):
        """A dispatch gate needs a legal mechanical role before it can refuse one."""
        for path in (CLAUDE_AGENTS / "helper.md", CODEX_AGENTS / "helper.toml"):
            with self.subTest(path=path):
                self.assertTrue(path.is_file(), "mechanical-helper adapter is missing")

    def test_role_adapters_make_first_row_retrieval_mandatory_for_delegates(self):
        """#4570 A5: a delegate misses SessionStart, so its adapter path owns retrieval."""
        clause = (
            "When this entrypoint was loaded through a role adapter, load "
            "[retrieve-first](references/retrieve-first.md) before task-specific discovery, "
            "including one-file reversible work."
        )
        entrypoint = ENTRYPOINT.read_text(encoding="utf-8")
        self.assertIn(re.sub(r"\s+", " ", clause), re.sub(r"\s+", " ", entrypoint))
        for adapter in sorted(CLAUDE_AGENTS.glob("*.md")):
            with self.subTest(adapter=adapter):
                self.assertIn("act-as-mohab/SKILL.md", adapter.read_text(encoding="utf-8"))
        tomllib = __import__("tomllib")
        for adapter in sorted(CODEX_AGENTS.glob("*.toml")):
            with self.subTest(adapter=adapter):
                instructions = tomllib.loads(adapter.read_text(encoding="utf-8"))["developer_instructions"]
                self.assertIn("act-as-mohab/SKILL.md", instructions)

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

    def test_budget_measures_documented_host_units(self):
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        for key in ("max_always_loaded_body_chars", "max_skill_listing_chars"):
            self.assertIn(key, budget)
        self.assertNotIn(
            "max_estimated_tokens_per_host",
            budget,
            "the pooled token estimate is replaced by character ceilings",
        )
        self.assertIn("limit_sources", budget, "each ceiling must name its documented source")

    def test_always_loaded_ceiling_keeps_a_documented_quarter_cap_reserve(self):
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        self.assertEqual(budget["max_always_loaded_body_chars"], 24_576)
        source = budget["limit_sources"]["max_always_loaded_body_chars"]
        for clause in ("32768", "75 percent", "8192", "UTF-8 bytes"):
            with self.subTest(clause=clause):
                self.assertIn(clause, source)

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

    def test_module_boundary_gate_is_needed_by_the_summary(self):
        summary, _ = self.summary_step()
        self.assertIn("module-boundary", summary["needs"])

    def test_the_guidance_gate_installs_the_managed_ci_dependencies(self):
        """The runner's tool-cache Python has no PyYAML; the frontmatter test
        imports it, so the job must install it or fail on every run."""
        _, jobs = self.summary_step()
        steps = jobs["agent-guidance"]["steps"]
        commands = " ".join(str(step.get("run", "")) for step in steps)
        self.assertIn("--requirement requirements-ci.txt", commands)
        self.assertIn("--no-deps", commands)

    def test_the_guidance_filter_covers_what_the_validator_checks(self):
        yaml = __import__("yaml")
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        filters = yaml.safe_load(workflow["jobs"]["changes"]["steps"][1]["with"]["filters"])
        guarded = set(filters["agent_guidance"])
        for required in ("AGENTS.md", ".agents/**", ".claude/**", ".memory/**", "shaft-skills/**"):
            self.assertIn(required, guarded, f"guidance filter misses {required}")
        outputs = workflow["jobs"]["changes"]["outputs"]
        self.assertIn("agent_guidance", outputs, "filter result is never exported")

    def test_agent_plugin_contract_validator_is_registered_in_the_guidance_gate(self):
        yaml = __import__("yaml")
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        filters = yaml.safe_load(workflow["jobs"]["changes"]["steps"][1]["with"]["filters"])
        guarded = set(filters["agent_guidance"])
        for required in (
            ".github/workflows/mavenCentral_cd.yml",
            "agent-plugins/**",
            "scripts/ci/agent_plugin_release.py",
            "scripts/ci/validate_agent_plugins.py",
            "tests/scripts/test_agent_plugin_release.py",
            "tests/scripts/test_validate_agent_plugins.py",
        ):
            self.assertIn(required, guarded, f"guidance filter misses {required}")
        commands = " ".join(
            str(step.get("run", "")) for step in workflow["jobs"]["agent-guidance"]["steps"]
        )
        self.assertIn("tests.scripts.test_agent_plugin_release", commands)
        self.assertIn("tests.scripts.test_validate_agent_plugins", commands)

    def test_dependency_review_runs_only_for_dependency_bearing_diffs(self):
        yaml = __import__("yaml")
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        changes = workflow["jobs"]["changes"]
        filters = yaml.safe_load(changes["steps"][1]["with"]["filters"])
        self.assertIn("dependencies", changes["outputs"])
        self.assertEqual(
            set(filters["dependencies"]),
            {
                "**/pom.xml",
                "**/build.gradle.kts",
                "**/gradle.properties",
                "requirements-ci.txt",
                ".github/dependabot.yml",
                ".github/dependency-review-config.yml",
                ".github/workflows/agent-plugin-acceptance.yml",
                ".github/workflows/mavenCentral_cd.yml",
                ".github/workflows/pr-gate.yml",
                ".github/workflows/publish-shaft-mcp.yml",
                "scripts/ci/dependency_review_changes.py",
                "tests/scripts/test_ci_python_dependencies.py",
                "tests/scripts/test_dependency_review_changes.py",
            },
        )
        self.assertIn("dependency-changes", workflow["jobs"])
        classifier = workflow["jobs"].get("dependency-changes", {})
        self.assertIn("dependencies", classifier["if"])
        self.assertEqual(classifier["outputs"]["review"], "${{ steps.diff.outputs.review }}")
        self.assertIn(
            "tests.scripts.test_dependency_review_changes",
            " ".join(str(step.get("run", "")) for step in classifier["steps"]),
        )
        self.assertIn(
            "needs.dependency-changes.outputs.review == 'true'",
            workflow["jobs"]["dependency-review"]["if"],
        )


class GlobFilesHelperTest(unittest.TestCase):
    """The resolver every GUIDANCE_GLOBS consumer in this file shares.

    NoDuplicationTest and SoloOrOrchestrateTest each declare their own tuple
    of guidance globs -- that duplication is the documented cost of host
    discovery having no include mechanism -- but both feed their tuple
    through this one resolver, so a pattern matching zero files reports the
    same way regardless of which check's list it dropped out of (#4481).
    """

    def test_returns_every_matched_file_across_patterns(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "docs").mkdir()
            (root / "docs/a.md").write_text("a", encoding="utf-8")
            (root / "docs/b.md").write_text("b", encoding="utf-8")
            (root / "AGENTS.md").write_text("agents", encoding="utf-8")
            found = glob_files(root, ("AGENTS.md", "docs/*.md"))
        self.assertEqual(
            {path.name for path in found}, {"AGENTS.md", "a.md", "b.md"}
        )

    def test_raises_when_a_pattern_matches_nothing(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "AGENTS.md").write_text("agents", encoding="utf-8")
            with self.assertRaises(AssertionError):
                glob_files(root, ("AGENTS.md", "moved-away/**/*.md"))


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
        ".agents/skills/README.md",
        ".agents/skills/*/SKILL.md",
        ".agents/skills/act-as-mohab/references/**/*.md",
        ".claude/skills/*/SKILL.md",
        ".claude/agents/*.md",
        ".github/skills/*/SKILL.md",
        ".github/skills/README.md",
        ".github/copilot-instructions.md",
        ".github/instructions/*.instructions.md",
    )
    # Host discovery requires each adapter to carry its own pointer line; there
    # is no include mechanism, so this repetition is the correct price.
    ALLOWED_REPEATS = ("Load [act-as-mohab](", "Do not restate policy here.")
    MIN_DUPLICATE_LINE_CHARS = 40

    def guidance_files(self) -> list[Path]:
        return glob_files(ROOT, self.GUIDANCE_GLOBS)

    def test_no_substantive_line_is_repeated_across_guidance_files(self):
        files = self.guidance_files()
        # A gutted glob_files() (e.g. hardcoded to return []) would make this
        # loop scan nothing and the duplicate check below pass trivially --
        # the resolver's own tests catch that mutation, but this consumer
        # must catch it too rather than rely on it (#4481).
        self.assertTrue(files, "guidance_files() resolved to no files")
        seen: dict[str, list[str]] = {}
        for path in files:
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

    def test_the_skills_map_is_one_of_the_scanned_files(self):
        """#4480: the map is guidance, so this scan has to actually read it.

        It was absent from this tuple while sitting in the budget's
        `total_guidance_globs`, so it was counted and link-checked and never
        read for a duplicated line.
        """
        self.assertIn(
            (CANONICAL_SKILLS / "README.md").resolve(),
            {path.resolve() for path in self.guidance_files()},
        )

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

    def test_the_map_is_inside_the_active_guidance_scan(self):
        """Counted and link-checked is not scanned (#4480).

        The map was named by `total_guidance_globs` and `reference_scan_globs`
        but not by `active_guidance_globs`, which is the list that feeds
        `validate_forbidden_patterns` and `validate_duplicate_paragraphs`. So
        the one file a contributor is pointed at directly was the one file no
        intent check read, and the count made the hole hard to see.

        The file set is built by the validator's own expansion, not by reading
        the JSON list, because a list is what the file was already on.
        """
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        patterns, key_errors = require_glob_list(budget, "active_guidance_globs")
        self.assertEqual(key_errors, [])
        scanned, glob_errors = expand_reported_globs(ROOT, patterns, "active_guidance_globs")
        self.assertEqual(glob_errors, [])
        self.assertIn(self.MAP.resolve(), {path.resolve() for path in scanned})

    def test_a_forbidden_mandate_planted_in_the_map_is_reported(self):
        """Being in the list has to reach the scan, so run the scan.

        `test_the_map_is_inside_the_active_guidance_scan` proves the expansion
        selects the file; this proves the expansion is what the forbidden
        pattern check consumes, by planting a violation and reading the issue
        back out of the shipped entry point.
        """
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            planted = root / ".agents/skills/README.md"
            planted.parent.mkdir(parents=True)
            planted.write_text(
                "# Agent skills map\n\nOpen a draft PR before you start work.\n",
                encoding="utf-8",
            )
            reported = [
                found
                for found in validate_repository(root=root, budget_path=BUDGET)
                if found["code"] == "forbidden-mandate"
                and found["path"] == ".agents/skills/README.md"
            ]
        self.assertTrue(reported, "a forbidden mandate in the skills map is not reported")

    def test_the_map_tells_agents_to_load_the_entrypoint_rather_than_itself(self):
        content = compact(self.MAP)
        self.assertIn("act-as-mohab/skill.md", content)
        self.assertRegex(content, r"do not work from this file|map, not the territory")


class CopilotRedirectPackIndexTest(unittest.TestCase):
    """#4532: the same hole as #4480, in the host that was left out of its fix.

    `.agents/skills/README.md` was pulled into `active_guidance_globs` by
    #4480. `.github/skills/README.md` has the identical shape -- it is the
    index of Copilot's redirect pack, and the skills map cites it by name as
    one of that host's adapter files -- and was deliberately left out of that
    pull request to keep the diff to the issue as filed.

    So it was counted by `total_guidance_globs` and link-checked by
    `reference_scan_globs`, and no intent check ever opened it:
    `validate_forbidden_patterns` and `validate_duplicate_paragraphs` read
    `active_guidance_globs`, and it was on neither that list nor
    `NoDuplicationTest.GUIDANCE_GLOBS`.

    "Counted and link-checked" reading as "checked" is what made the hole hard
    to see, and it is the same shape as the rest of this batch: a file inside
    the boundary, with nothing consuming it.
    """

    MAP = ROOT / ".github/skills/README.md"

    def test_the_index_exists(self):
        """Everything below is vacuous if the host pack was renamed away."""
        self.assertTrue(self.MAP.is_file(), f"{self.MAP} is missing")

    def test_the_index_is_inside_the_active_guidance_scan(self):
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        patterns, key_errors = require_glob_list(budget, "active_guidance_globs")
        self.assertEqual(key_errors, [])
        scanned, glob_errors = expand_reported_globs(ROOT, patterns, "active_guidance_globs")
        self.assertEqual(glob_errors, [])
        self.assertIn(self.MAP.resolve(), {path.resolve() for path in scanned})

    def test_a_forbidden_mandate_planted_in_the_index_is_reported(self):
        """Being on the list has to reach the scan, so run the scan.

        The test above proves the expansion selects the file; this proves the
        expansion is what the forbidden-pattern check consumes, by planting a
        violation and reading the finding back out of the shipped entry point.
        """
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            planted = root / ".github/skills/README.md"
            planted.parent.mkdir(parents=True)
            planted.write_text(
                "# Copilot redirect pack\n\nOpen a draft PR before you start work.\n",
                encoding="utf-8",
            )
            reported = [
                found
                for found in validate_repository(root=root, budget_path=BUDGET)
                if found["code"] == "forbidden-mandate"
                and found["path"] == ".github/skills/README.md"
            ]
        self.assertTrue(reported, "a forbidden mandate in the Copilot index is not reported")

    def test_the_duplicate_checks_also_cover_it(self):
        """`NoDuplicationTest` keeps its own tuple, so being on one list is not both."""
        self.assertIn(".github/skills/README.md", NoDuplicationTest.GUIDANCE_GLOBS)


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
        self.assertRegex(content, r"scan.{0,80}(failure|trap|guard block)")


class SoloOrOrchestrateTest(unittest.TestCase):
    """Whether main thread implements has exactly one rule, in one place.

    Two statements previously disagreed: "the orchestrator never implements"
    against "do the work yourself". Both are right in their own mode and wrong
    as absolutes, so the discriminator -- how many independent work streams the
    session owns -- lives in the entrypoint and every other file defers to it.
    """

    # Every tracked surface an agent can read, not just the files that happened
    # to state the rule when this test was written.
    GUIDANCE_GLOBS = (
        "AGENTS.md",
        "CLAUDE.md",
        ".agents/skills/README.md",
        ".agents/skills/*/SKILL.md",
        ".agents/skills/act-as-mohab/references/**/*.md",
        ".claude/agents/*.md",
        ".claude/skills/*/SKILL.md",
        ".claude/user-harness/*.md",
        ".codex/agents/*.toml",
        ".github/copilot-instructions.md",
        ".github/instructions/*.md",
        ".github/skills/*/SKILL.md",
        ".github/skills/README.md",
        "shaft-skills/*/SKILL.md",
    )
    # Both poles of the original conflict, with the subject anchored so the
    # reviewer role's legitimate "Never edits" does not trip it.
    SUBJECT = r"(?:orchestrator|main thread|main session)"
    ACTION = r"(?:implement|edit|writ\w+ (?:the )?code|do(?:es|ing)? the work)"
    ABSOLUTE = re.compile(
        rf"{SUBJECT}[^.]{{0,90}}?(?:never|does not|do not|must not|cannot)[^.]{{0,40}}?{ACTION}"
        rf"|(?:never|do not|don't) delegate"
        rf"|always (?:do|implement)[^.]{{0,40}}yourself",
        re.I,
    )
    # A sentence that names the mode it applies to is scoped, not absolute.
    # "orchestrator" is deliberately absent: it is the SUBJECT of the rule, so
    # treating it as a mode name made every sentence about the orchestrator
    # look already-scoped and let the original conflict back through.
    SCOPED = re.compile(r"\b(?:mode|solo|orchestrate|orchestrating|orchestrated)\b", re.I)

    def section(self) -> str:
        sections = re.split(r"(?m)^#{2,3} ", ENTRYPOINT.read_text(encoding="utf-8"))
        found = [body for body in sections if body.lower().startswith("solo or orchestrate")]
        self.assertEqual(len(found), 1, "entrypoint needs exactly one solo-or-orchestrate rule")
        return found[0]

    def test_the_rule_keys_on_unrelated_tasks_in_flight(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertIn("unrelated tasks", content, "the trigger is the owner's asks, not any decomposition")
        self.assertRegex(content, r"subtasks of a single task are \*?\*?one", "subtasks are one stream")
        self.assertIn("two or more", content)
        rows = [line for line in self.section().splitlines() if line.strip().startswith("|")]
        self.assertGreaterEqual(len(rows), 4, "the rule needs a mode table")

    def test_the_rule_resolves_mode_transitions_and_hostless_delegation(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertRegex(
            content,
            r"while any delegate still owns a stream",
            "a finishing delegate must not silently flip the mode",
        )
        self.assertRegex(
            content,
            r"no subagent primitive cannot orchestrate",
            "a host that cannot delegate needs a stated fallback",
        )

    def test_solo_mode_forbids_delegating_the_work(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertRegex(content, r"do the work yourself")
        self.assertRegex(content, r"do not delegate")

    def test_orchestrated_mode_keeps_main_thread_out_of_all_task_work(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertRegex(content, r"do no task work yourself")
        self.assertRegex(content, r"no edits, run no long job, and install nothing",
                         "barring only implementation leaves installs and long jobs allowed")
        self.assertRegex(content, r"reachable", "the rule must state why main thread stays free")
        self.assertRegex(content, r"four", "orchestrated mode states the concurrency cap")

    def test_review_never_flips_the_mode(self):
        content = re.sub(r"\s+", " ", self.section()).lower()
        self.assertRegex(
            content,
            r"reviewer is never a work stream|not turn a solo session",
            "a review must not be counted as a second work stream",
        )

    def unscoped_absolutes(self, text: str) -> list[str]:
        """Return sentences stating either pole without naming the mode."""
        offenders = []
        for sentence in re.split(r"(?<=[.:])\s+", re.sub(r"\s+", " ", text)):
            if self.ABSOLUTE.search(sentence) and not self.SCOPED.search(sentence):
                offenders.append(sentence.strip()[:90])
        return offenders

    def test_no_other_file_states_an_unconditional_implement_or_delegate_rule(self):
        """Every other mention must name the mode it applies to.

        The original conflict had two poles -- "never implement" and "never
        delegate" -- so both are checked, across every tracked surface an agent
        can read rather than only the files that stated it first.
        """
        files = glob_files(ROOT, self.GUIDANCE_GLOBS)
        # Same guard as NoDuplicationTest: a gutted resolver must fail this
        # test directly, not just the resolver's own unit tests (#4481).
        self.assertTrue(files, "glob_files() resolved to no files")
        offenders = []
        for path in files:
            if path.resolve() == ENTRYPOINT.resolve():
                continue
            for sentence in self.unscoped_absolutes(path.read_text(encoding="utf-8")):
                offenders.append(f"{path.relative_to(ROOT).as_posix()}: {sentence}")
        self.assertEqual(offenders, [], "unscoped implement/delegate absolute outside the entrypoint")

    def test_the_guard_catches_the_phrasings_that_previously_slipped_through(self):
        """A guard that only matches its own example sentence is not a guard."""
        must_catch = (
            "Main thread does not implement. Delegate everything.",
            "The orchestrator must never implement; always dispatch.",
            "The main thread never writes code; delegate every change.",
            "The main session, whose orchestrator never edits, runs the phases.",
            "Always do the work yourself; never delegate anything.",
            "Do not delegate; the main thread owns every edit.",
        )
        missed = [text for text in must_catch if not self.unscoped_absolutes(text)]
        self.assertEqual(missed, [], "guard does not catch a reintroduced conflict")

        must_allow = (
            "Read-only. Reads the full diff, verifies claims. Never edits.",
            "In orchestrated mode main thread does no task work while it dispatches.",
            "A host with no subagent primitive cannot orchestrate; it works solo.",
        )
        false_positives = [text for text in must_allow if self.unscoped_absolutes(text)]
        self.assertEqual(false_positives, [], "guard trips on legitimate scoped text")


class DisciplineTest(unittest.TestCase):
    """Rules the published failure evidence says an agent will otherwise break.

    This repository's own selection history is the argument for pinning them by
    phrase. Rules that were only asserted got traded away -- the guard's rule
    numbering still skips R4 through R7, and two of the deleted ones were TDD
    and Graphify nudges. Rules with a failing check behind them survived.

    Each pin below asserts the clause that carries the rule, inside the section
    that owns it -- a pin that passes on any prose is decoration.

    The proof that a pin binds used to be a claim in a pull request body: the
    author deleted the clause once, watched the suite go red, and said so.
    #4467 is what that bought -- an adversarial review found that the pins
    caught deletion and renaming but not weakening by addition, because nothing
    re-ran the proof and nothing had ever tried the *other* mutation. So the
    proof now runs in CI. `test_deleting_any_pinned_clause_is_reported` and
    `test_qualifying_any_pinned_clause_is_reported` splice their mutations into
    the shipped file at run time and fail if `clause_defects` stays quiet.

    Scope, stated plainly so the next reader does not overrate these. This
    paragraph has been corrected twice; both times it claimed more than the
    code did, which is the failure this class exists to catch.

    What is actually caught: deleting a pinned clause, renaming or duplicating
    the section that owns it, moving it to another section, hedging it with
    vocabulary that has already been demonstrated, and -- since #4490 -- a
    pinned section's own rule list not reading exactly 1..N, whether it has a
    gap, a repeated number, a wrong first number, or numbers the renderer
    supplies rather than the file (`1. 1. 1.`). Both Markdown delimiters count,
    so `1)` is no way around it.

    That last entry exists because the one before it was read as covering more
    than it did. A clause moved to *another* section is reported, so
    "relocation is caught" looked complete; a clause moved within its own
    section -- lifted out of the numbered list into prose just below it -- was
    reported by nothing, and reads as 1, 2, 3, 4, 6. An undisclosed gap
    standing next to a disclosed one is worse than an undisclosed gap alone,
    because the disclosure is what makes a reader stop looking. That failure
    then happened again inside the fix: the first cut of it claimed a repeat
    was caught while `1. 1. 1.` and `1)` both walked through, which is the same
    error one level down and is why each of those now has its own mutation
    test.

    So the numbering catches that move, and only while the move leaves the list
    unable to read 1..N. Demote the *last* item and the rest stay contiguous.
    Demote any item and renumber to close the hole and it is contiguous again.
    Both are uncaught here and are the clause pins' problem, not this check's.
    So is any rule a section states as prose rather than as a numbered item:
    Red flags and `proving a check binds` carry no ordered list, so the check
    binds three of the five pinned sections and is silent on those two.

    What is not. `EXEMPTION_MARKERS` is a denylist, so it is both incomplete
    and brittle -- "at your discretion" and "use judgment" were markers while
    "at your own judgement" still walked through. More importantly, a rule can
    be gutted with no hedge vocabulary at all, and none of these are reported:
    a scope-narrowing prefix ("For public API changes: no production code
    before an observed failing test"); an `### Exceptions` subsection after the
    laws; a definition elsewhere in the file that redefines a term the law
    depends on; a permissive restatement in the law's own item ("Relaying a
    delegate's result is fine"); or demoting the whole section under a
    `## Background` heading, which still matches.

    So these pins raise the cost of losing a rule by accident. They do not make
    one impossible to remove on purpose, and nothing here observes whether an
    agent obeyed a law -- only what the file still says.
    """

    def source(self) -> str:
        return ENTRYPOINT.read_text(encoding="utf-8")

    def assert_clause_holds(self, clause: str) -> None:
        """Assert `clause` is pinned, present, and unqualified in the entrypoint.

        Going through the registry rather than asserting the phrase inline is
        what keeps the mutation tests honest: a clause they never mutate cannot
        silently be the one this test checks.
        """
        self.assertIn(
            clause,
            [pinned for _, _, pinned in PINNED_CLAUSES],
            f"clause is asserted but not registered for mutation: {clause!r}",
        )
        offending = [defect for defect in clause_defects() if clause in defect]
        self.assertEqual(offending, [])

    def iron_laws(self) -> str:
        """Return the Iron laws section, whitespace-collapsed and lowercased.

        Scoped like the red-flag pin below: asserting these phrases against the
        whole file would also pass on a stray mention in an example or a red
        flag, so the assertion has to land on the law itself.
        """
        laws = headed_sections(self.source(), IRON_LAWS)
        self.assertEqual(len(laws), 1, "entrypoint needs exactly one Iron laws section")
        return re.sub(r"\s+", " ", laws[0]).lower()

    def test_entrypoint_pins_evidence_over_inference(self):
        """Iron law 2, unpinned until now.

        Every other discipline here is downstream of it: "never claim a check
        you did not run" and the RED definition are both special cases of
        inspecting before asserting. The law also has to name the remedy --
        inspect *or run* -- because an agent that reads only "evidence over
        inference" can satisfy itself that reasoning carefully is evidence.
        """
        self.assert_clause_holds("evidence over inference")
        # The law must also name the act that produces evidence, not just the value.
        self.assert_clause_holds("inspect or run before claiming")

    def test_entrypoint_pins_no_production_code_before_an_observed_failing_test(self):
        """Iron law 3, unpinned until now.

        "Observed" is the load-bearing word and the one an edit optimising for
        bytes would drop first. Without it the law reads as "write a test
        first", which a test that was never executed satisfies -- and a test
        nobody ran is exactly how a suite fills with assertions that never
        could have failed.
        """
        # Dropping "observed" downgrades the law to "write a test first".
        self.assert_clause_holds("no production code before an observed failing test")

    def test_entrypoint_pins_never_claiming_an_unrun_check(self):
        """Iron law 5, unpinned until now.

        This is the law the harness can least afford to lose, because it is the
        one that makes every other report trustworthy: no check in this
        repository verifies that a run happened, only that the rule is written
        down. It is also the cheapest law to break silently, which is why it
        needs the most explicit sentence.
        """
        self.assert_clause_holds("never claim a check you did not run")

    def test_entrypoint_pins_what_does_and_does_not_count_as_red(self):
        """The RED definition, unpinned until now.

        Iron law 3 is unenforceable without it. An import error, a syntax
        error, or a missing fixture all produce a red run, and accepting one as
        RED means the "failing test" was never evidence of missing behaviour --
        it was evidence of a broken harness. The exclusion list is therefore
        the substance of the rule, not commentary on it, and so is the
        instruction to revert when production code was written first: without
        it, "restart" is advice rather than a step.
        """
        self.assertEqual(
            len(headed_sections(self.source(), TDD)), 1, "entrypoint needs exactly one TDD section"
        )
        # RED has to be narrowed to an assertion failure, or any red run qualifies.
        self.assert_clause_holds("only an expected assertion failure")
        # The exclusions are the rule; without them a broken harness reads as RED.
        self.assert_clause_holds("a pass or setup, syntax, or environment error is not red")
        # Code-first needs a stated remedy, not just disapproval.
        self.assert_clause_holds("revert that new code and restart")

    def test_entrypoint_pins_what_counts_as_a_test_at_all(self):
        """The two TDD clauses #4467 found unpinned.

        The first is the definition every other TDD rule rests on: without it a
        tautological assertion satisfies RED and GREEN both, and the whole cycle
        can be walked through while verifying nothing. The second closes the
        cheapest way to claim the cycle without running it -- write the code,
        write the test afterwards, and report TDD.
        """
        self.assert_clause_holds(
            "asserts nothing, prints instead of asserting, or mocks the behavior under test"
        )
        self.assert_clause_holds("never backfill tests after implementation")

    def test_entrypoint_forbids_weakening_a_test_to_reach_green(self):
        """Iron law 4.

        Scoped to the law rather than to the whole file, which the previous
        regex was not: `weaken a test` matching anywhere in `SKILL.md` would
        have passed on a red flag or an aside that merely mentioned the idea.
        """
        self.assert_clause_holds("never weaken, delete, or rewrite a test to reach green")

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

    def mutate(self, path: Path, clause: str, insertion: str) -> dict[Path, str]:
        """Splice `insertion` in after `clause` in the shipped file's own text.

        The mutation is built from the file, never from a literal in this
        module. A test that carries its own copy of the text it protects
        verifies the copy, which is how a gutted check stays green.
        """
        source = path.read_text(encoding="utf-8")
        match = re.search(clause_pattern(clause), source, re.I)
        self.assertIsNotNone(match, f"clause is not in {path.name} at all: {clause!r}")
        return {path: source[: match.end()] + insertion + source[match.end() :]}

    def assert_reports(self, overrides: dict[Path, str], clause: str, why: str) -> None:
        """Assert the mutation is reported *for this clause*, not merely reported.

        `assertTrue(clause_defects(...))` was the original assertion and it was
        vacuous: any unrelated defect anywhere in the registry satisfied it, so
        a single renamed heading made every mutation proof in this class pass
        without testing anything. The defect has to name the clause that was
        mutated.
        """
        defects = clause_defects(overrides)
        self.assertTrue(any(clause in defect for defect in defects), f"{why}: {defects}")

    def test_deleting_any_pinned_clause_is_reported(self):
        for path, _, clause in PINNED_CLAUSES:
            with self.subTest(clause=clause):
                source = path.read_text(encoding="utf-8")
                mutated = re.sub(clause_pattern(clause), "", source, count=1, flags=re.I)
                self.assertTrue(mutated != source, "the mutation did not apply")
                self.assert_reports({path: mutated}, clause, "deleting the clause is not reported")

    def test_qualifying_any_pinned_clause_is_reported(self):
        """#4467: a pin that only checks presence passes on a gutted rule.

        Deletion is the easy half. `"..., unless time is short"` leaves every
        pinned word in place, and every pin in this class passed on it until
        this test existed.
        """
        for path, _, clause in PINNED_CLAUSES:
            with self.subTest(clause=clause):
                self.assert_reports(
                    self.mutate(path, clause, ", unless time is short"),
                    clause,
                    "qualifying the clause is not reported",
                )

    # Hedges demonstrated to survive the marker list as first written. Each is
    # a real weakening of a law that left every pinned word in place.
    SURVIVING_HEDGES = (
        ". Use judgment on routine checks",
        ". This is advisory",
        ". Treat it as a guideline rather than a rule",
        ", where possible",
        ", as appropriate",
        ". Not strictly required for small changes",
        # Second round, from the same review. "at your own judgement" is the
        # instructive one: "at your discretion" and "use judgment" were both
        # already markers, and the British spelling with a different
        # preposition still walked through. Denylists are brittle as well as
        # incomplete.
        ", in most cases",
        ", though you need not for routine checks",
        ". This is waived for routine checks",
        ". This does not apply to guidance edits",
        "; you may skip this when the change is small",
        ". Apply it at your own judgement",
    )

    # The law used for the numbering mutations. It is the middle of the list,
    # so demoting or renumbering it leaves a gap that the sequence shows.
    NUMBERED_LAW = "never claim a check you did not run"

    def iron_laws_body(self, source: str) -> str:
        """Return the Iron laws section exactly as it sits in `source`."""
        heading = re.search(r"(?mi)^## iron laws$", source)
        self.assertIsNotNone(heading, "entrypoint has no Iron laws heading")
        end = source.find("\n## ", heading.end())
        self.assertGreater(end, 0, "Iron laws is not followed by another section")
        return source[heading.end() : end]

    def law_numbers(self, source: str) -> list[int]:
        """Read the list numbers back out, so a mutation is proved to have landed.

        Deliberately its own two-line regex rather than the helper under test:
        a mutation verified by the check it is meant to break would agree with
        itself whichever way both were wrong.
        """
        return [int(number) for number in re.findall(r"(?m)^(\d+)\.\s", self.iron_laws_body(source))]

    def mutate_iron_laws(self, transform, source: str | None = None) -> str:
        """Return the entrypoint with `transform` applied to its Iron laws body.

        Every mutation below edits the shipped section rather than a fixture,
        so none of them can pass by agreeing with a copy this module carries.
        """
        source = ENTRYPOINT.read_text(encoding="utf-8") if source is None else source
        body = self.iron_laws_body(source)
        mutated = transform(body)
        self.assertNotEqual(mutated, body, "the mutation did not apply")
        return source.replace(body, mutated, 1)

    def demote_law_to_trailing_prose(self, source: str | None = None) -> str:
        """Move the pinned law out of the list into prose in its own section.

        Built from the shipped file, like every other mutation here: the item's
        own text is lifted and re-inserted after the list, so the mutation
        cannot pass by carrying its own copy of the law.
        """

        def demote(body: str) -> str:
            item = re.search(
                rf"(?mi)^\d+[.)][ \t]+({clause_pattern(self.NUMBERED_LAW)}[^\n]*)\n", body
            )
            self.assertIsNotNone(item, "the pinned law is not a numbered item")
            return f"{(body[: item.start()] + body[item.end() :]).rstrip()}\n\n{item.group(1)}\n"

        return self.mutate_iron_laws(demote, source)

    def test_demoting_a_law_into_trailing_prose_is_reported(self):
        """#4490: a law can be unmade without deleting a word of it.

        Move it below the list and it is still present, still in its owning
        section, still unqualified -- every pin in this class passes and the
        numbering reads 1, 2, 3, 4, 6. The tell is structural, so the check is
        structural: the ordered list a pinned section carries must number its
        rules 1..N with no gap, no repeat and no restart.
        """
        mutated = self.demote_law_to_trailing_prose()
        self.assertNotIn(5, self.law_numbers(mutated), "the mutation did not apply")
        self.assertIn(self.NUMBERED_LAW, mutated.lower(), "the law must survive the demotion")
        self.assertTrue(
            [defect for defect in clause_defects({ENTRYPOINT: mutated}) if "numbering" in defect],
            "demoting a law out of the list is not reported",
        )

    def test_renumbering_a_law_over_its_neighbour_is_reported(self):
        """A duplicate number is the same defect from the other side.

        It is what a careless demotion or insertion leaves behind, and reading
        1, 2, 3, 4, 4, 6 as six laws is a mistake a human reviewer makes too.
        """
        source = ENTRYPOINT.read_text(encoding="utf-8")
        mutated = re.sub(
            rf"(?mi)^5\.(?=[ \t]+{clause_pattern(self.NUMBERED_LAW)})", "4.", source, count=1
        )
        self.assertEqual(
            self.law_numbers(mutated).count(4), 2, "the mutation did not apply"
        )
        self.assertTrue(
            [defect for defect in clause_defects({ENTRYPOINT: mutated}) if "numbering" in defect],
            "a duplicated rule number is not reported",
        )

    def test_a_list_that_renumbers_itself_is_reported(self):
        """The review's refutation of the first cut of this check.

        `1. 1. 1.` is legal Markdown and renders as 1, 2, 3 -- the renderer
        supplies the numbers. Which means a list written that way cannot show
        a deletion: pull one rule out and it still renders contiguously, so
        nothing structural is left to notice. A section whose numbering is
        derived rather than written is inert for this check, so writing it that
        way is itself the defect, and every pinned section states its own
        numbers today.
        """
        mutated = self.mutate_iron_laws(lambda body: re.sub(r"(?m)^\d+\.(?=\s)", "1.", body))
        numbers = self.law_numbers(mutated)
        self.assertGreater(len(numbers), 1, "the mutation did not apply")
        self.assertEqual(set(numbers), {1}, "the mutation did not apply")
        self.assertTrue(
            [defect for defect in clause_defects({ENTRYPOINT: mutated}) if "numbering" in defect],
            "a list that lets the renderer number it is not reported",
        )

    def test_a_gap_in_a_paren_delimited_list_is_reported(self):
        """`1)` renders the same as `1.` and must not be a way out.

        The first cut of this check matched a literal `.`, so rewriting the
        delimiter turned it off for that section entirely -- and a check that
        an unrelated formatting choice can switch off is worse than no check,
        because the docstring goes on claiming it.
        """
        demoted = self.demote_law_to_trailing_prose()
        mutated = self.mutate_iron_laws(
            lambda body: re.sub(r"(?m)^(\d+)\.(?=\s)", r"\1)", body), demoted
        )
        self.assertEqual(
            re.findall(r"(?m)^(\d+)\)", self.iron_laws_body(mutated)),
            ["1", "2", "3", "4", "6"],
            "the mutation did not apply",
        )
        self.assertTrue(
            [defect for defect in clause_defects({ENTRYPOINT: mutated}) if "numbering" in defect],
            "a gap in a paren-delimited list is not reported",
        )

    def test_a_list_that_starts_at_the_wrong_number_is_reported(self):
        """The third branch, which nothing named until now.

        A list numbered 2..7 is what deleting the first rule and renumbering
        nothing leaves behind. `numbering_defects` reports it because it
        compares against 1..N rather than against the run's own first value,
        but only the shipped-guidance control failed on it, and a control that
        fails does not say which branch stopped working.
        """
        mutated = self.mutate_iron_laws(
            lambda body: re.sub(
                r"(?m)^(\d+)\.(?=\s)", lambda item: f"{int(item.group(1)) + 1}.", body
            )
        )
        numbers = self.law_numbers(mutated)
        self.assertEqual(numbers[0], 2, "the mutation did not apply")
        self.assertEqual(numbers, list(range(2, len(numbers) + 2)), "the list must stay contiguous")
        self.assertTrue(
            [defect for defect in clause_defects({ENTRYPOINT: mutated}) if "numbering" in defect],
            "a list that does not start at 1 is not reported",
        )

    def test_deleting_a_law_and_renumbering_cleanly_is_still_reported(self):
        """The numbering check must not become the only check (#4490 item 3).

        Delete the law outright and close the gap and the list is contiguous
        again, so the new check is silent by design. What has to catch it is
        the clause pin that always did -- this test exists to prove the
        structural check did not quietly displace it.
        """
        source = ENTRYPOINT.read_text(encoding="utf-8")
        without = re.sub(
            rf"(?mi)^5\.[ \t]+{clause_pattern(self.NUMBERED_LAW)}[^\n]*\n", "", source, count=1
        )
        self.assertNotEqual(without, source, "the mutation did not apply")
        mutated = re.sub(r"(?m)^6\. ", "5. ", without, count=1)
        self.assertEqual(self.law_numbers(mutated), [1, 2, 3, 4, 5], "the renumbering did not apply")
        self.assertNotIn(self.NUMBERED_LAW, mutated.lower(), "the law must be gone")
        defects = clause_defects({ENTRYPOINT: mutated})
        self.assertEqual(
            [defect for defect in defects if "numbering" in defect],
            [],
            "a cleanly renumbered list is not a numbering defect",
        )
        self.assert_reports(
            {ENTRYPOINT: mutated}, self.NUMBERED_LAW, "deleting the law outright is not reported"
        )

    def guard_source(self) -> str:
        path = ROOT / "scripts/agents/guard.py"
        # Asserted, not assumed: a wrong path here makes every mechanism check
        # error rather than fail, and an errored check proves nothing about the
        # registry it was supposed to be verifying.
        self.assertTrue(path.is_file(), f"guard.py not found at {path}")
        return path.read_text(encoding="utf-8")

    def test_every_iron_law_has_exactly_one_registry_row(self):
        """#4541: a rule with no recorded enforcement status is the gap itself.

        Equality against the entrypoint, not a hand-kept count: the law numbers
        come from the file, so adding a seventh iron law fails this until it is
        classified as performed, gated, or prose-only-because. That is what
        makes the registry bind on future expansion rather than on today's six.
        """
        source = ENTRYPOINT.read_text(encoding="utf-8")
        runs = ordered_list_runs(headed_sections(source, IRON_LAWS)[0])
        laws = {number for run in runs for number in run}
        registered = [row["law"] for row in REQUIRED_ACTION_REGISTRY if row["law"] is not None]

        self.assertEqual(set(registered), laws, "every iron law needs exactly one row")
        self.assertEqual(
            len(registered), len(set(registered)), "a law must not be registered twice"
        )

    def test_every_row_is_honestly_classified(self):
        """The name is the claim, so it has to test both halves of it.

        The first version asserted only that `status` was a member of the
        allowed set and that `rule` was non-empty -- which is satisfied by
        every possible misclassification. Adversarial review found five rows
        claiming `gated` for rules whose own docstrings say they only report.

        The distinction is mechanical rather than a judgement: a `gated` rule
        is dispatched from `run_pretooluse`, where returning a string denies
        the call; a `reports` rule is dispatched from `run_stop`, which cannot
        refuse anything.
        """
        pretooluse = self.dispatched_from("run_pretooluse")
        stop = self.dispatched_from("run_stop")
        self.assertTrue(pretooluse and stop, "the dispatch scan found nothing")
        self.assertNotEqual(
            pretooluse,
            stop,
            "the two entry points cannot dispatch identical sets; the scan is too wide",
        )

        for row in REQUIRED_ACTION_REGISTRY:
            with self.subTest(rule=row["rule"][:50]):
                self.assertIn(row["status"], REGISTRY_STATUSES)
                self.assertTrue(row["rule"].strip())
                mechanism = row.get("mechanism", "")
                if row["status"] == "gated":
                    self.assertIn(
                        mechanism,
                        pretooluse,
                        f"{mechanism} is classified gated but does not refuse a call",
                    )
                if row["status"] == "reports":
                    self.assertIn(
                        mechanism,
                        stop,
                        f"{mechanism} is classified reports but is not a Stop rule",
                    )

    def dispatched_from(self, entry: str) -> set[str]:
        """Rule names appearing in one hook entry point's body.

        `inspect.getsource` rather than slicing the file between `def` markers.
        The slicing version silently returned every rule in the module for
        `run_stop`, which made the `reports` half of the classification check
        pass for any input -- a vacuous check inside the test whose whole
        subject is dishonest classification.
        """
        import inspect

        from scripts.agents import guard

        return set(re.findall(r"check_r[0-9]+_[a-z_]+", inspect.getsource(getattr(guard, entry))))

    def test_every_enforced_row_names_a_mechanism_that_exists(self):
        """A row claiming enforcement by a symbol nobody wrote is worse than no row."""
        source = self.guard_source()
        for row in REQUIRED_ACTION_REGISTRY:
            if row["status"] not in ("gated", "performed"):
                continue
            with self.subTest(rule=row["rule"][:50]):
                mechanism = row.get("mechanism", "")
                self.assertTrue(mechanism, "an enforced row must name its mechanism")
                self.assertIn(
                    f"def {mechanism}(",
                    source,
                    f"{mechanism} is claimed as the mechanism and is not defined in guard.py",
                )

    def test_a_narrowed_row_records_what_its_mechanism_does_not_reach(self):
        """`scope` is honest only while it carries substance, exactly like `reason`."""
        for row in REQUIRED_ACTION_REGISTRY:
            if "scope" not in row:
                continue
            with self.subTest(rule=row["rule"][:50]):
                self.assertIn(
                    row["status"],
                    ("gated", "reports", "performed"),
                    "only an enforced row has a reach to qualify",
                )
                self.assertGreaterEqual(
                    len(row["scope"].split()), 12, "state what the mechanism does not reach"
                )

    def test_law_threes_recorded_scope_is_the_one_its_mechanism_enforces(self):
        """The claim binds to behaviour, so widening R12 cannot leave it stale.

        #4567 item 7. The row reads `gated`, which is mechanically true, and on
        #4554 -- 3138 lines, 20 files, zero of them under `src/main/` -- the
        mechanism could not fire once. The `scope` field records that, and this
        pins the record against `_PRODUCTION_PATH` itself rather than against a
        sentence somebody has to remember to update.

        Deliberately fails if `_PRODUCTION_PATH` is widened. Widening is a real
        option #4567 weighs and rejects, because gating every guidance and
        script edit behind an observed test run is a rule that fires on correct
        work. If it is ever taken anyway, the registry must stop claiming
        `src/main/` in the same commit, and this is what makes that happen.
        """
        from scripts.agents import guard

        row = next(row for row in REQUIRED_ACTION_REGISTRY if row["law"] == 3)
        self.assertIn(
            "src/main/",
            row.get("scope", ""),
            "law 3 reads gated and its mechanism reaches src/main/ only; "
            "the row has to say so",
        )
        self.assertTrue(
            guard._PRODUCTION_PATH.search("shaft-engine/src/main/java/A.java"),
            "the mechanism no longer matches the paths its scope claims",
        )
        for outside in (
            "scripts/agents/guard.py",
            "tests/scripts/test_guard_lifecycle.py",
            ".agents/skills/act-as-mohab/SKILL.md",
            ".github/workflows/pr-gate.yml",
        ):
            with self.subTest(path=outside):
                self.assertIsNone(
                    guard._PRODUCTION_PATH.search(outside),
                    f"R12 now reaches {outside}; the registry's recorded scope is stale",
                )

    def test_every_prose_only_row_records_why(self):
        """The third status is honest only while it carries its reason."""
        for row in REQUIRED_ACTION_REGISTRY:
            if row["status"] != "prose-only":
                continue
            with self.subTest(rule=row["rule"][:50]):
                reason = row.get("reason", "")
                # A word floor, because "not practical" is non-empty and says
                # nothing. Low on purpose: this catches a placeholder, it does
                # not have opinions about prose.
                self.assertGreaterEqual(
                    len(reason.split()), 12, "state why no mechanism is practical"
                )

    def test_the_registry_does_not_require_its_own_categories_to_be_populated(self):
        """The deadlock this design was warned about, asserted as absent.

        `gotcha.a-check-whose-healthy-end-state-is-unreachable-is-a-check-that-
        will-be-weakened`: requiring prose-only to be empty would make the
        healthy end state illegal the moment a rule genuinely has no mechanism,
        and the cheapest exit an agent sees is deleting the guard.

        So the checks above must pass on a registry with no prose-only rows,
        and on one with nothing but them.
        """
        for variant in ("all gated", "all prose-only", "empty"):
            with self.subTest(variant=variant):
                if variant == "all gated":
                    rows = tuple(
                        row for row in REQUIRED_ACTION_REGISTRY if row["status"] != "prose-only"
                    )
                elif variant == "all prose-only":
                    rows = tuple(
                        row for row in REQUIRED_ACTION_REGISTRY if row["status"] == "prose-only"
                    )
                else:
                    rows = ()
                with mock.patch(f"{__name__}.REQUIRED_ACTION_REGISTRY", rows):
                    self.test_every_row_is_honestly_classified()
                    self.test_every_enforced_row_names_a_mechanism_that_exists()
                    self.test_every_prose_only_row_records_why()

    def test_every_pinned_section_with_rules_is_counted(self):
        """An uncounted section must not become the place to hide a deletion.

        `count_defects` is silent where no count is pinned, which is correct
        for Red flags and `proving a check binds` -- neither carries an
        ordered list, so there is nothing to count. Left at that, the way to
        evade the check would be to add rules to an uncounted section, or to
        add a pinned section and never count it.

        Equality in both directions. A pinned section that grows an ordered
        list and no count is the evasion; a count naming a section that is not
        pinned is a check aimed at nothing, which reads like coverage and is
        not.
        """
        with_rules = set()
        for path, heading, _ in PINNED_CLAUSES:
            if (path, heading) in with_rules:
                continue
            sections = headed_sections(path.read_text(encoding="utf-8"), heading)
            if len(sections) == 1 and ordered_list_runs(sections[0]):
                with_rules.add((path, heading))
        self.assertEqual(
            with_rules,
            set(PINNED_RULE_COUNTS),
            "every pinned section that has rules needs a count, and vice versa",
        )

    def test_each_pinned_count_matches_the_file_today(self):
        """A count that drifted from the file reports on every run or never.

        Wrong high and the section is permanently in defect; wrong low and it
        has already absorbed a deletion. Either way the figure stops meaning
        what the other tests here assume it means.
        """
        for (path, heading), expected in PINNED_RULE_COUNTS.items():
            with self.subTest(section=f"{path.name}:{heading}"):
                sections = headed_sections(path.read_text(encoding="utf-8"), heading)
                self.assertEqual(len(sections), 1)
                found = sum(len(run) for run in ordered_list_runs(sections[0]))
                self.assertEqual(found, expected)

    def test_deleting_the_last_law_is_reported(self):
        """#4534: an end-deletion is contiguous, and the last law has no pin.

        `numbering_defects` reads 1..N, so removing an item from the *middle*
        leaves a gap it catches while removing the item at the *end* leaves
        1, 2, 3 becoming 1, 2 -- still contiguous, still 1..N, silent. The
        module already said so honestly; what it did not say is what that
        costs here.

        `PINNED_CLAUSES` pins iron laws 2, 3, 4 and 5. Laws 1 and 6 carry no
        clause pin, and law 6 is the last item -- so deleting the rule that
        requires an independent adversarial review passes the numbering check
        and every clause pin at once. The one rule whose absence nothing else
        in the harness would notice is exactly the one the structure could not
        see going.

        Fixed with a count rather than by pinning those two clauses. A clause
        pin binds the words, and words get rewritten for good reasons; a count
        binds the shape, which is what an end-deletion changes. It also fails
        for a law added without review, which no clause pin can do.
        """
        source = ENTRYPOINT.read_text(encoding="utf-8")
        mutated = re.sub(r"(?m)^6\. [^\n]*\n(?:[ \t]+[^\n]*\n)*", "", source, count=1)
        self.assertNotEqual(mutated, source, "the mutation did not apply")
        self.assertEqual(
            self.law_numbers(mutated),
            [1, 2, 3, 4, 5],
            "the point of this test is that the remaining list stays contiguous",
        )
        self.assertNotIn(
            "independent adversarial review", mutated.lower(), "the law must be gone"
        )
        self.assertEqual(
            [defect for defect in clause_defects({ENTRYPOINT: mutated}) if "numbering" in defect],
            [],
            "an end-deletion is not a numbering defect, which is why this test exists",
        )
        self.assertTrue(
            clause_defects({ENTRYPOINT: mutated}),
            "deleting the last iron law must be reported by something",
        )

    def test_adding_a_law_nobody_pinned_is_reported(self):
        """The same count, in the direction a clause pin cannot look.

        Every pin asks whether a rule it already knows about is still there.
        None of them can ask whether a rule appeared that no one reviewed, and
        a silently added iron law is a policy change with no gate in front of
        it.
        """
        source = ENTRYPOINT.read_text(encoding="utf-8")
        mutated = re.sub(
            r"(?m)^(6\. [^\n]*\n(?:[ \t]+[^\n]*\n)*)",
            r"\g<1>7. Ship it if the delegate says it passed.\n",
            source,
            count=1,
        )
        self.assertNotEqual(mutated, source, "the mutation did not apply")
        self.assertEqual(self.law_numbers(mutated), [1, 2, 3, 4, 5, 6, 7])
        self.assertTrue(
            clause_defects({ENTRYPOINT: mutated}),
            "an iron law appearing from nowhere must be reported",
        )

    def test_hedges_that_slipped_past_the_first_marker_list_are_reported(self):
        """A denylist of hedge words is inherently incomplete.

        The first version of `EXEMPTION_MARKERS` was written from the two
        counter-examples #4467 happened to name, and six ordinary English
        hedges walked straight through it -- each one leaving iron law 5
        intact word for word and gutting it anyway. That is the same mistake
        #4467 caught one level down: a guard built from the examples in front
        of you, mistaken for a guard against the category.

        Widening the list does not fix the category and this test does not
        claim it does. What it fixes is regression: a hedge that has actually
        been demonstrated stays demonstrated.
        """
        clause = "never claim a check you did not run"
        for hedge in self.SURVIVING_HEDGES:
            with self.subTest(hedge=hedge):
                self.assert_reports(
                    self.mutate(ENTRYPOINT, clause, hedge),
                    clause,
                    "a hedge inside the law's own rule item is not reported",
                )

    def test_the_shipped_guidance_carries_no_clause_defect_at_all(self):
        """The control every mutation proof in this class depends on.

        Without it, `assert_clause_holds` filtering defects by clause meant a
        defect that named no clause was computed and thrown away, and the
        mutation loops -- which only asked whether *some* defect existed --
        passed on a file that was already broken.
        """
        self.assertEqual(clause_defects(), [])

    def test_renaming_a_pinned_section_is_reported_against_its_clauses(self):
        """#4479's review: the hole this class had while claiming to close one.

        Renaming `## Iron laws` to `## Laws` produced five section defects,
        every one of them filtered out because it did not name a clause. The
        suite stayed green with all five pins in that section vacuous, and both
        mutation proofs stayed green too, because a section defect satisfied
        their "some defect exists" assertion for whatever clause they were
        testing. One word in a heading silenced the whole apparatus.
        """
        for path, heading, clause in PINNED_CLAUSES:
            with self.subTest(section=heading, clause=clause):
                source = path.read_text(encoding="utf-8")
                renamed = re.sub(
                    rf"(?mi)^(#{{2,3}} ){re.escape(heading)}\b", r"\1Renamed", source
                )
                self.assertTrue(renamed != source, "the rename did not apply")
                self.assertTrue(
                    any(clause in defect for defect in clause_defects({path: renamed})),
                    "renaming the section leaves this clause's pin vacuous",
                )

    def test_a_pinned_clause_outside_its_own_section_is_reported(self):
        """Section scoping is advertised by this class, so it needs its own check.

        With `headed_sections` gutted to hand back the whole file, every other
        test here still passed -- which made "inside the section that owns it"
        an unbound claim about the very property the pins are sold on.
        """
        for path, heading, clause in PINNED_CLAUSES:
            with self.subTest(section=heading, clause=clause):
                source = path.read_text(encoding="utf-8")
                relocated = re.sub(clause_pattern(clause), "", source, count=1, flags=re.I)
                self.assertTrue(relocated != source, "the removal did not apply")
                relocated += f"\n\n## Appendix\n\n{clause}\n"
                self.assertTrue(
                    any(clause in defect for defect in clause_defects({path: relocated})),
                    "a clause moved out of its own section is not reported",
                )

    def test_the_two_weakenings_named_in_the_review_are_reported(self):
        """The literal counter-examples from #4467, not a paraphrase of them.

        One qualifies inside the sentence and one appends a whole new sentence,
        which is why the exemption scan is judged per rule item rather than per
        sentence.
        """
        cases = (
            ("never claim a check you did not run", ". Routine checks are exempt"),
            ("no production code before an observed failing test", ", unless time is short"),
        )
        for clause, insertion in cases:
            with self.subTest(clause=clause):
                self.assert_reports(
                    self.mutate(ENTRYPOINT, clause, insertion),
                    clause,
                    f"the review's own counter-example passes: {insertion!r}",
                )

    def test_red_flags_name_the_sentence_that_ships_a_check_protecting_nothing(self):
        """The always-loaded half of #4471.

        Every other red flag here is something an agent says instead of running
        a check. This one is what it says *after* running one: the check was
        green, so the thing it names is covered. Four defects in a single
        session had that shape -- a metric whose input was absent reporting the
        absence as a value, a test asserting against its own copy of the
        regexes the shipped script uses, a fix whose mechanism could be
        reverted with the suite green, and an author routing around a linter
        they wrote. Green proves the check ran; only breaking the protected
        thing proves it binds.

        It belongs in the always-loaded list rather than only in the review
        lens because the failure happens while authoring the check, hours
        before any reviewer loads a reference.
        """
        self.assert_clause_holds("the check covers it")

    def test_the_review_lens_keeps_the_unbound_check_shape_and_its_proof(self):
        """The on-demand half of #4471, and the one rule here with no home
        anywhere else in the harness.

        The lens's other three gap shapes are about production behaviour. This
        one is about the check itself, and its proof step is the only place the
        harness says an obligation is discharged by *running* the mutation
        rather than imagining it -- which is the distinction the four defects
        in #4471 all fell through. Left as prose it would be the kind of rule
        this repository has already watched erode, so it is pinned like an iron
        law and mutated like one.
        """
        self.assert_clause_holds("unbound-check gap")
        self.assert_clause_holds("apply it, run it, read the failure, revert")

    def test_delegation_states_the_parallel_agent_cap(self):
        self.assertRegex(compact(DELEGATION), r"(?:four|4) (?:active |concurrent |parallel |writing )*agents")

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


class VacuousSetupPatchTest(unittest.TestCase):
    """No test may assert on a symbol its own class's `setUp` patched off.

    #4567 item 4. A source-scanning pin, in the file that already owns them.

    The class it protects against is `vacuous-check`: at least eight instances
    in the session #4567 measures, of which this one produced a DO NOT MERGE
    verdict and survived a round of review aimed straight at it. A reviewer is
    an expensive way to run a lint, and this one runs in milliseconds at commit
    time.
    """

    MODULES = ("tests/scripts/test_*.py",)

    def sources(self) -> list[Path]:
        return glob_files(ROOT, self.MODULES)

    def test_no_shipped_test_asserts_on_its_own_setups_mock(self):
        files = self.sources()
        self.assertGreater(len(files), 40, "the module scan collapsed to almost nothing")
        defects: list[str] = []
        for path in files:
            defects.extend(
                setup_patch_defects(
                    path.read_text(encoding="utf-8"), path.relative_to(ROOT).as_posix()
                )
            )
        self.assertEqual(defects, [], "\n".join(defects))

    def test_the_defect_this_was_written_for_is_reported(self):
        """Mutation proof. Green proves the scan ran; only this proves it binds.

        Reduced from `UserHarnessDriftStopGateTest` at `d346bac4b0` verbatim in
        shape: `setUp` pins the helper, and a test then asserts on the helper.
        """
        source = (
            "class T(unittest.TestCase):\n"
            "    def setUp(self):\n"
            "        patcher = patch('scripts.agents.guard._helper', return_value=False)\n"
            "        patcher.start()\n"
            "        self.addCleanup(patcher.stop)\n"
            "\n"
            "    def test_it(self):\n"
            "        with patch('scripts.agents.guard._git_output', return_value=None):\n"
            "            self.assertFalse(guard._helper())\n"
        )
        defects = setup_patch_defects(source, "synthetic.py")
        self.assertEqual(len(defects), 1, defects)
        self.assertIn("_helper", defects[0])

    def test_a_bare_assert_on_the_mock_is_reported(self):
        source = (
            "class T(unittest.TestCase):\n"
            "    def setUp(self):\n"
            "        patch('scripts.agents.guard._helper', return_value=False).start()\n"
            "\n"
            "    def test_it(self):\n"
            "        assert guard._helper() is False\n"
        )
        self.assertEqual(len(setup_patch_defects(source, "synthetic.py")), 1)

    def test_a_test_that_repatches_the_symbol_itself_is_not_reported(self):
        """The legitimate shape in the same class must stay legal.

        Without this exemption the check would report the tests that are *about*
        the suppression, which is the `fires-on-correct-work` shape the whole
        of #4567 exists to stop shipping.
        """
        source = (
            "class T(unittest.TestCase):\n"
            "    def setUp(self):\n"
            "        patcher = patch('scripts.agents.guard._helper', return_value=False)\n"
            "        patcher.start()\n"
            "\n"
            "    def test_it(self):\n"
            "        with patch('scripts.agents.guard._helper', return_value=True):\n"
            "            self.assertTrue(guard._helper())\n"
        )
        self.assertEqual(setup_patch_defects(source, "synthetic.py"), [])

    def test_naming_the_symbol_in_a_string_is_not_asserting_on_it(self):
        """A source assertion mentions the symbol and never calls it.

        This is a real idiom in the protected modules -- asserting that a rule's
        shipped source reads its working directory through one helper -- and
        reporting it would have made the check unshippable.
        """
        source = (
            "class T(unittest.TestCase):\n"
            "    def setUp(self):\n"
            "        patch('scripts.agents.guard._helper', return_value=False).start()\n"
            "\n"
            "    def test_it(self):\n"
            "        self.assertIn('_helper(cwd)', inspect.getsource(guard.rule))\n"
        )
        self.assertEqual(setup_patch_defects(source, "synthetic.py"), [])

    def test_a_class_with_no_setup_patch_is_never_reported(self):
        """The fix for the measured defect was to move the test to such a class."""
        source = (
            "class T(unittest.TestCase):\n"
            "    def test_it(self):\n"
            "        with patch('scripts.agents.guard._git_output', return_value=None):\n"
            "            self.assertTrue(guard._helper('.'))\n"
        )
        self.assertEqual(setup_patch_defects(source, "synthetic.py"), [])


class ExecutableSpecificationStructureTest(unittest.TestCase):
    """#4656's planning template is three independently pinned matrices."""

    SCHEMA = (
        ("Resolved caller matrix", ("Site", "Effective cwd/path", "Runtime/version/platform", "Permissions/trust", "Configuration precedence", "Input existence")),
        ("State/failure matrix", ("State", "Immutable ownership", "Preflight", "Mutation order", "Mixed state", "Atomicity", "Concurrency", "Idempotency", "Recovery", "Fail-closed")),
        ("Acceptance-to-proof map", ("Criterion or invariant", "Positive proof", "Negative or mutation proof", "Command")),
    )

    def test_real_guidance_has_exact_named_6_10_4_tables(self):
        text = CONSULT.read_text(encoding="utf-8")
        for name, columns in self.SCHEMA:
            with self.subTest(table=name):
                marker = f"### {name}"
                self.assertEqual(text.count(marker), 1)
                block = text.split(marker, 1)[1]
                header = next((line for line in block.splitlines() if line.startswith("|")), "")
                self.assertEqual(tuple(cell.strip() for cell in header.strip("|").split("|")), columns)

    def test_registry_honestly_marks_semantic_risk_and_remote_ordering_prose_only(self):
        rows = [row for row in REQUIRED_ACTION_REGISTRY if "executable specification" in row["rule"]]
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["status"], "prose-only")
        self.assertIn("before red/green", rows[0]["rule"].lower())
        self.assertIn("before commit", rows[0]["rule"].lower())
        reason = rows[0]["reason"].lower()
        self.assertIn("semantic risk", reason)
        self.assertIn("remote", reason)
        self.assertIn("false", reason)

    def test_real_guidance_requires_exhaustive_rows_and_concrete_regressions(self):
        text = CONSULT.read_text(encoding="utf-8")
        compact_text = " ".join(text.split())
        self.assertIn(
            "Add one resolved data row for every caller/site, every state/transition/failure mode, "
            "and every acceptance criterion/invariant.",
            compact_text,
        )
        for scenario in (
            "Effective working-directory/path resolution.",
            "Interpreter/version/conditional dependency marker.",
            "Mixed owned+unknown preflight.",
            "Immutable ownership.",
            "Atomic backup/concurrent replacement.",
            "Post-migration adapter/link resolution.",
        ):
            with self.subTest(scenario=scenario):
                self.assertIn(f"- {scenario}", text)


if __name__ == "__main__":
    unittest.main()
