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
from pathlib import Path, PurePosixPath
from unittest import mock


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

# Three review rounds, three defects, and every one of them sat in a regex asked
# to make a semantic judgement about English prose: is this period a sentence
# end (#4468), does this sentence *record* a supersession or *assert* a policy,
# is this a cadence *policy* or an incident *narrative* (#4469). The row that
# never failed a round is the mechanical one -- does this text contain a known
# phrase. So the grammar is gone (#4484): no sentence splitter, no abbreviation
# lexicon, no supersession-marker list, no verb class, no tense.
#
# What replaces the inference is `POLICY_RECORD_ALLOWLIST` below. An object that
# legitimately names a retired policy says so in one reviewable line naming its
# own id, instead of hoping an unbounded grammar recognises the phrasing it
# happened to use. #4461 rejected an opt-out because it "needs nothing of the
# author"; three rounds falsified that -- the automatic inference cost a review
# round each time, and in #4477 it rejected the exact record the Learning-loop
# table asks an agent to write.

# An id is a pointer, not an assertion, and ids are immutable here:
# `.memory/events.jsonl` records `memory.created` against them, so a rename
# orphans history. A `[[wiki-link]]` naming an object whose policy has moved is
# therefore correct, and only prose is judged.
#
# #4484 asked for this to go along with the grammar, and it was deleted and put
# back, because it is not grammar. What deleting it did was turn the house
# style for citing a memory object into a build failure. Live objects cite by
# `[[id]]`, and two live ids name a retired policy in their own slug -- the
# retired constraint, and
# `orchestrator-checks-in-on-any-delegated-background-task-after-30-minutes`,
# which the hyphenated-interval fix below newly brought into range. That second
# one is the canonical memory about check-in cadence, so it is exactly the
# object a future memory would cross-reference. Trading an unbounded grammar
# for an allowlist is the point of #4484; trading routine correct citation for
# one is not.
#
# What was wrong with judging the markup by its delimiters alone: the strip was
# content-blind, so it laundered any claim someone wrapped in brackets just as
# readily as it cleared a citation (#4521). `[[one-PR-per-session]]` read clean.
# Two bracket pairs silenced any claim in this file, and unlike an allowlist
# line -- which names an id and is visible in review -- a bracket wrap diffuses
# into memory prose and reads as house style. It is the cheapest repair an agent
# under pressure reaches for, which is iron law 4 pressure with nothing to catch
# it.
#
# So the strip is now a set membership, the same mechanical device the allowlist
# uses, and no more of a semantic call than a phrase list: `[[...]]` is stripped
# only when what it contains names a memory object that exists. A citation is a
# pointer; a bracket wrap around a policy phrase points at nothing and is
# judged as the prose it is. That also closes the malformed-first-link hole,
# where an unclosed pair swallowed the prose between two citations and the whole
# span read clean -- the swallowed span is not an id, so it no longer strips.
#
# Once the strip is a membership rather than a delimiter, the markup stops being
# the thing that matters and the *object named* starts being it -- so the second
# way this store names an object gets the same treatment. `[[id]]` is the house
# style and a path is the exception, but two live ids carry a retired policy in
# their own slug, and citing either as
# `.memory/memory/constraints/<slug>.md` flagged where `[[<slug>]]` did not
# (#4516). One object cites another by path today, and its slug happens to be
# harmless, so this is a trap rather than a live failure -- which is the moment
# to close it, since the escape it would otherwise cost is an allowlist entry
# that exempts a whole object for a citation.
#
# The path form is anchored on `.memory/` and its last character must be a word
# character or a hyphen, so a sentence-ending period is not swallowed into the
# stem. Nothing else about it is grammar: the stem either names an object that
# exists or it does not.
WIKI_LINK = re.compile(r"\[\[([^\]]+)\]\]")
MEMORY_PATH_CITATION = re.compile(r"\.memory/[\w./-]*[\w-]")

SUPERSEDED_ONE_PR_PER_SESSION = re.compile(
    r"(?i)one[- ]pr[- ]per[- ]session"
    r"|\b1 PR per session"
    r"|single (?:final|session) PR"
    r"|one-branch-one-worktree-one-pr"
    r"|session'?s single branch/pr"
)
ONE_PR_PER_SESSION_REASON = (
    "one PR per session: superseded by work-github-planning.md Sec. 3b, "
    "which lets a session open one PR per group of related subtasks"
)

# A delegate subject and a minute figure, and that is the entire rule. The third
# signal used to be a directive list, which #4469 showed was fitted to the two
# objects it was written against: six genuinely phrased cadence policies walked
# past it. Widening it into a verb class was tried in #4477 and withdrawn --
# `read` is its own past tense, as are `cost`, `put`, `set` and `hit`, and
# `review`, `wake` and `ping` are nouns as readily as verbs, so no lexical rule
# separates "I read the delegate's output 20 minutes in" from "Read a
# subagent's output every 20 minutes". That is not a gap in the list; it is
# proof the list was the wrong instrument.
#
# Dropping it costs false positives on timing observations -- "Compaction takes
# 2 minutes and needs no delegate" now flags -- and that cost is paid to the
# allowlist rather than to a grammar, because a false positive with a one-line
# reviewable escape does not tempt an agent into weakening the check, which iron
# law 4 forbids. The trade is measured, not assumed: this pair flags two of the
# 341 active objects in the live store, and both hold an allowlist entry below.
#
# Three spellings were simply absent. Two are #4484 Part B residuals that
# outlive the grammar: `\s*` matched no `30-minute`, so the most idiomatic
# English cadence -- the compound modifier -- was the one spelling that walked
# past; and `subagent\b` rejected `subagents`, as `background (?:task|job)\b`
# rejected `background jobs`, so a policy written about more than one delegate
# missed on its plural.
#
# The third is `background agent`, and it is the same fitting problem #4469
# found in the directive list, one layer down: the list was written from the two
# objects in front of it, and a live object says "It idles as a frozen
# background agent" while carrying `~20-30 min`. Renaming that phrase to
# `background task` would have turned an unrelated PR red, which is a margin one
# word wide rather than the comfortable zero the count suggested (#4516). The
# subject is half the rule now, so a missing spelling here is half a rule
# nothing enforces.
#
# The separator is an alternation and not a character class, because the two
# things it has to admit are not one class. A body wraps, so "every 30\n
# minutes" is one cadence written across two lines and `?` cannot see it; a
# compound modifier is "30-minute". `[\s-]*` bought both by admitting a *mixed
# run* of whitespace and hyphens, and that run is this repository's own
# punctuation: ` -- ` is house style in nearly every docstring in this module
# and throughout `.memory` bodies, and `\n- ` is an ordinary markdown bullet.
# So the widening flagged the two commonest ways prose here puts a figure near
# the word "minutes" -- neither of them a cadence (#4521).
#
# `(?:\s+|-|)` takes the wrapped line, the single hyphen and no separator at
# all, and refuses the mixed run. Both false positives carry a negative fixture
# below, because nothing in the suite could previously tell the two spellings
# apart: every positive fixture is satisfied by either, so the over-wide form
# shipped unenforced. A boundary or a separator can only be proved by what it
# refuses -- and the subject is half the rule now, so its edges have to be held
# by a test rather than by whatever the store happens to contain today.
DELEGATE_SUBJECT = re.compile(
    r"(?i)\b(?:delegate[ds]?|subagents?|background (?:task|job|agent)s?)\b"
)
INTERVAL_FIGURE = re.compile(r"(?i)\b\d{1,3}(?:\s+|-|)min(?:ute)?s?\b")
DELEGATE_INTERVAL_REASON = (
    "a delegate check-in interval: delegation.md owns the single figure, "
    "and memory restating a second one is how 20 and 30 both became true"
)

# The explicit escape that makes the rule above acceptable: memory object ids
# permitted to name a retired policy, one entry per (policy, object). Ids are
# immutable here -- `.memory/events.jsonl` records `memory.created` against
# them, so a rename orphans history -- which makes the id the one stable key an
# allowlist can use.
#
# Keyed by policy, not by object, so an object cleared for one retired policy
# still fails on another. A reason with no permitted object has no entry at all,
# because an empty entry is a rule nothing enforces -- the defect all three
# review rounds found. Every entry is pinned by
# `test_every_allowlist_entry_is_individually_load_bearing`, which fails if the
# object it names stops needing it.
#
# Empty is the healthy state, not a sign the mechanism is unused. Its job is to
# be there when an object has to say something the lexical rule reads as a
# claim -- most likely an incident narrative carrying a delegate and a figure,
# which A1 cannot tell from a cadence policy and deliberately does not try to.
# An earlier revision required at least one live entry, on the theory that an
# unexercised escape is an unenforced one. That was a deadlock: reconciling the
# last exempt object left no legal state, since keeping its entry tripped the
# staleness check and removing it tripped the non-empty check, and the only way
# out was to weaken a check -- the very pressure this module warns about in
# four places. The mechanism is exercised against a synthetic store instead.
# The `why` lives in the entry rather than in a comment above it (#4535). Both
# entries below were already justified, at length, in prose that nothing bound
# to the id it explained -- reorder the entries, insert a third between them or
# delete one, and the paragraph stays where it was, now attached to whatever
# moved into its place. A justification that can drift away from its subject is
# a comment, not a record.
#
# `why` is not required to quote the sentence that trips the scan, which was the
# obvious stronger rule. The workflow entry below trips A1 on two strings in
# different parts of the object -- the subject in one, the figure in another --
# so no single sentence trips it, and requiring a quote would leave a live entry
# with no legal answer. That is the same deadlock shape as the non-empty
# requirement this module already removed.
# The shipped list maps id to why. Callers may pass a bare set of ids instead
# where the justification is not what is under test, and the union says so
# rather than leaving fixtures quietly mistyped against the annotation.
AllowlistByPolicy = dict[str, "dict[str, str] | frozenset[str]"]
POLICY_RECORD_ALLOWLIST: dict[str, dict[str, str]] = {
    DELEGATE_INTERVAL_REASON: {
        "decision.a-lexical-guard-judges-tokens-only-legitimacy-comes-from"
        "-an-explicit-id-keyed-allowlist-not-from-parsing-prose": (
            "The decision recording why this scan stopped parsing prose. It has "
            "to quote the phrasings that defeated the verb class -- \"Read a "
            "subagent's output every 20 minutes\" against \"I read the "
            "delegate's output 20 minutes in\" -- because those two strings are "
            "the whole argument, and paraphrasing the figures out of them would "
            "leave a reader unable to see what broke. A subject beside a figure, "
            "asserting no cadence: the exact false positive A1 accepts by "
            "design, taking the exact escape A2 exists to give it."
        ),
        "workflow.after-opening-a-pr-rely-on-ci-push-notifications-not-a"
        "-self-polling-monitor": (
            "The live false negative from #4516. It says \"It idles as a frozen "
            "background agent\" of a self-polling monitor, and separately gives "
            "`~20-30 min` as the size of a safety-net fallback. Neither is a "
            "delegate check-in cadence -- the whole object argues *against* "
            "polling -- and the figure belongs to a wakeup heartbeat, not to an "
            "interval anyone is meant to restate. The narrative-beside-a-figure "
            "shape the rule accepts by design."
        ),
    },
}


def string_leaves(value: object) -> list[str]:
    """Flatten a JSON value to its strings, so nesting cannot hide prose."""
    if isinstance(value, str):
        return [value]
    if isinstance(value, dict):
        return [leaf for item in value.values() for leaf in string_leaves(item)]
    if isinstance(value, list):
        return [leaf for item in value for leaf in string_leaves(item)]
    return []


def searchable_text(memory_root: Path, metadata: dict) -> str:
    """Return every field of a memory object a retrieval puts in front of an agent.

    One helper for both memory-vs-guidance checks, because they had already
    drifted apart: one read title and body, the other body, `facets` and
    `evidence`, so which field a claim sat in decided whether it was caught
    (#4464). Facet and evidence strings are joined as prose rather than
    `json.dumps`-ed -- serialising injects quotes, braces and escaped
    whitespace into the middle of the prose, which is exactly where a phrase
    match needs the words to stay adjacent.
    """
    return "\n".join(
        (
            metadata.get("title", ""),
            (memory_root / metadata["body_path"]).read_text(encoding="utf-8"),
            *string_leaves(metadata.get("facets", {})),
            *string_leaves(metadata.get("evidence", [])),
        )
    )


def memory_object_identifiers(memory_root: Path) -> frozenset[str]:
    """Every name a memory object can be cited by, for the citation strip.

    Two spellings, because the live store uses both: the full `id`, and the
    slug after its kind prefix. Both appear in `[[...]]` citations today, so a
    set holding only one of them turns the other into a build failure -- which
    is the failure deleting the strip caused in the first place.

    Not the metadata filename stem, which was tried and dropped. It is
    redundant for all but two objects, which made the slug spelling above
    untestable, and for those two it is not a citation spelling at all: it
    contributes the bare words `architecture` and `project` (from
    `architecture.current` and `project.shaft-engine`), and every word this set
    holds is a word a bracket wrap can launder. The set exists to be narrow.

    Status is not consulted. An id is immutable here (`.memory/events.jsonl`
    records `memory.created` against it, so a rename orphans history), so a
    retired object is still citable and citing one is still a pointer.
    """
    identifiers: set[str] = set()
    for metadata_path in (memory_root / "memory").rglob("*.json"):
        identifier = json.loads(metadata_path.read_text(encoding="utf-8")).get("id", "")
        if identifier:
            identifiers.add(identifier)
            identifiers.add(identifier.split(".", 1)[-1])
    return frozenset(identifiers)


def strip_citations(text: str, citable: frozenset[str]) -> str:
    """Replace each citation of a known object with a separator, in either spelling.

    `[[id]]` is the house style and a `.memory/` path is the other way this
    store names an object; both are pointers, and both are judged the same way
    -- by whether what they name exists.

    A separator and not an empty string: closing the text up around a strip
    fuses the tail of one word to the head of the next and manufactures both
    subjects and phrases that nobody wrote.
    """
    linked = WIKI_LINK.sub(
        lambda match: " " if match.group(1) in citable else match.group(0), text
    )
    return MEMORY_PATH_CITATION.sub(
        lambda match: " " if PurePosixPath(match.group(0)).stem in citable else match.group(0),
        linked,
    )


def superseded_policy_offences(text: str, citable: frozenset[str] = frozenset()) -> list[str]:
    """Return the superseded policies this text names, by lexical match alone.

    Whether naming one is legitimate is not decided here, because deciding it
    from the prose is what failed three review rounds. This answers only the
    mechanical question; `memory_object_offences` applies the explicit
    allowlist that answers the other one.

    `citable` defaults to empty, which exempts no citation at all. That is the
    fail-closed direction on purpose: a caller that forgets to pass the store's
    identifiers gets loud false positives, never silent laundering.
    """
    prose = strip_citations(text, citable)
    offences: set[str] = set()
    if SUPERSEDED_ONE_PR_PER_SESSION.search(prose):
        offences.add(ONE_PR_PER_SESSION_REASON)
    if DELEGATE_SUBJECT.search(prose) and INTERVAL_FIGURE.search(prose):
        offences.add(DELEGATE_INTERVAL_REASON)
    return sorted(offences)


def memory_object_offences(
    memory_root: Path,
    metadata: dict,
    allowlist: AllowlistByPolicy | None = None,
    citable: frozenset[str] | None = None,
) -> list[str]:
    """Return what a memory object still offends on once its allowlist entries apply."""
    permitted = POLICY_RECORD_ALLOWLIST if allowlist is None else allowlist
    known = memory_object_identifiers(memory_root) if citable is None else citable
    identifier = metadata.get("id", "")
    return [
        reason
        for reason in superseded_policy_offences(searchable_text(memory_root, metadata), known)
        # Membership, not lookup: `in` reads the ids either way, so the shipped
        # `{id: why}` and a fixture's bare set of ids behave identically here.
        # The default is `{}` for the same reason -- an empty mapping and an
        # empty set are the same answer, and the mapping matches what ships.
        if identifier not in permitted.get(reason, {})
    ]


def active_memory_policy_offenders(
    root: Path, allowlist: AllowlistByPolicy | None = None
) -> list[str]:
    """Return `path: reason` for every active memory object restating a superseded policy."""
    memory_root = root / ".memory"
    # Built once for the whole store, not once per object: every object may
    # cite every other, so the set is a property of the store and not of the
    # object being judged.
    citable = memory_object_identifiers(memory_root)
    offenders = []
    for metadata_path in sorted((memory_root / "memory").rglob("*.json")):
        metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
        if metadata.get("status", "active") != "active":
            continue
        for reason in memory_object_offences(memory_root, metadata, allowlist, citable):
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
    def test_codex_discovers_only_the_repo_local_router(self):
        discovered = sorted(ROOT.glob(".agents/skills/*/SKILL.md"))

        self.assertEqual(
            discovered,
            [
                ROOT / ".agents/skills/act-as-mohab/SKILL.md",
                ROOT / ".agents/skills/chaos-engine/SKILL.md",
            ],
        )

    def test_act_as_mohab_has_one_substantive_body_and_relative_adapter(self):
        canonical = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
        adapter = ROOT / ".claude/skills/act-as-mohab/SKILL.md"
        self.assertTrue(canonical.is_file())
        self.assertGreater(len(markdown_body(canonical)), 1000)
        self.assertLess(len(markdown_body(adapter)), 500)

        candidates = [canonical, *ROOT.glob(".*/skills/act-as-mohab/SKILL.md")]
        substantive = [path for path in candidates if len(markdown_body(path)) > 500]
        self.assertEqual(substantive, [canonical])

        match = re.search(r"\[[^]]+\]\(([^)]+)\)", adapter.read_text(encoding="utf-8"))
        self.assertIsNotNone(match)
        target = match.group(1)
        self.assertFalse(Path(target).is_absolute())
        repository_adapter = ROOT / ".agents/skills/chaos-engine/SKILL.md"
        self.assertEqual((adapter.parent / target).resolve(), repository_adapter.resolve())

    def test_act_as_mohab_embeds_core_working_rules(self):
        content = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(
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

    def test_act_as_mohab_preserves_bounded_drive_sharing_authorization(self):
        content = (ROOT / "chaos-engine/profiles/shaft/entrypoint.md").read_text(
            encoding="utf-8"
        )
        section = re.search(
            r"## Standing artifact sharing authorization\s+(.*)",
            content,
            re.DOTALL,
        )
        self.assertIsNotNone(section)
        self.assertEqual(
            re.sub(r"\s+", " ", section.group(1)).strip(),
            "The standing authorization applies to artifacts produced for SHAFT "
            "repository tasks: they may be uploaded to Google Drive and set to "
            "**Anyone with the link — Viewer** without asking again, in present and "
            "future sessions. It covers only the intended task artifacts. It does "
            "not cover unrelated Drive content, folder-wide permission changes, "
            "editor access, or secret-bearing artifacts. Verify the intended "
            "artifact and resulting permission after each upload; if either scope "
            "or secret safety is uncertain, stop before publishing.",
        )

    def test_act_as_mohab_requires_fresh_task_branch_from_fetched_main(self):
        content = (ROOT / "chaos-engine/profiles/shaft/entrypoint.md").read_text(
            encoding="utf-8"
        )
        compact = re.sub(r"\s+", " ", content)
        for required in (
            "Task branches use `ChaosEngine/*`",
            "fetched `origin/main`",
        ):
            self.assertIn(required, compact)

    def test_host_token_budgets_include_mandatory_entrypoint(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        mandatory = ".agents/skills/chaos-engine/SKILL.md"
        for host, paths in budget["host_contexts"].items():
            self.assertIn(mandatory, paths, host)

    def test_rule_docs_are_embedded_rather_than_redirected(self):
        """Redirect stubs cost a read and carry no content, so the rules live in
        the entrypoint and the stubs are gone. Attribution must survive."""
        references = ROOT / "chaos-engine/references"
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
        self.assertIn(".agents/skills/chaos-engine/SKILL.md", agents)
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
        paths.extend((ROOT / "chaos-engine").rglob("*.md"))
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
            / "chaos-engine/profiles/shaft/references/playbooks/agentic-pdca-loop.md"
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
        self.assertEqual(set(claude_hooks), set(codex_hooks) | {"PostToolUseFailure"})
        for hooks, extra in ((claude_hooks, {"PostToolUseFailure"}), (codex_hooks, set())):
            self.assertEqual(
                set(hooks),
                {
                    "PreToolUse",
                    "PostToolUse",
                    "SessionStart",
                    "Stop",
                    "SubagentStop",
                    "UserPromptSubmit",
                } | extra,
            )
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
                "tool_input": {"file_path": str(Path(tempfile.gettempdir()) / "guard-scratch.txt")},
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
            self.assertEqual(json.loads(completed.stdout), {})

    def test_guard_denies_an_in_repository_write_on_the_default_branch(self):
        """R19 is in contract: unrelated events get an empty JSON acknowledgement."""
        with tempfile.TemporaryDirectory() as temporary_directory:
            repository = Path(temporary_directory)
            (repository / "tracked.txt").write_text("fixture", encoding="utf-8")
            git = shutil.which("git")
            self.assertIsNotNone(git, "the R19 fixture needs Git")
            for command in (
                [git, "init", "--quiet", "--initial-branch", "main"],
                [git, "add", "tracked.txt"],
                [
                    git,
                    "-c",
                    "user.name=fixture",
                    "-c",
                    "user.email=fixture@example.test",
                    "commit",
                    "--quiet",
                    "-m",
                    "initial",
                ],
            ):
                subprocess.run(command, cwd=repository, check=True)  # nosec B603

            completed = self.run_guard_completed(
                {
                    "hook_event_name": "PreToolUse",
                    "tool_name": "Write",
                    "cwd": str(repository),
                    "tool_input": {"file_path": "Example.java"},
                },
                "claude",
            )

        output = json.loads(completed.stdout)
        decision, reason = self.logical_decision(output)
        self.assertEqual(decision, "deny")
        self.assertIn("R19", reason)

    def test_deployed_canonical_subtree_has_no_external_or_broken_markdown_links(self):
        # The deployment unit is the whole portable ChaosEngine tree, not one
        # skill: the entrypoint links generic references and project profiles.
        canonical = ROOT / "chaos-engine"
        with tempfile.TemporaryDirectory() as temporary_directory:
            deployed = Path(temporary_directory) / "chaos-engine"
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

    def test_default_mcp_configs_do_not_launch_docker(self):
        claude = json.loads((ROOT / ".mcp.json").read_text(encoding="utf-8"))[
            "mcpServers"
        ]
        codex = tomllib.loads((ROOT / ".codex/config.toml").read_text(encoding="utf-8"))[
            "mcp_servers"
        ]

        offenders = []
        for host, servers in (("claude", claude), ("codex", codex)):
            offenders.extend(
                f"{host}:{name}"
                for name, config in servers.items()
                if config.get("command", "").casefold() in {"docker", "docker.exe"}
            )

        self.assertEqual(
            offenders,
            [],
            "Default stdio MCP declarations start once per client; Docker-backed "
            "servers must be an explicit opt-in, not a default harness cost.",
        )

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
            searchable = searchable_text(memory_root, metadata)
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

    def test_the_policy_scan_reads_every_field_a_retrieval_shows(self):
        """The two memory-vs-guidance checks were reading different fields.

        Its sibling searched body plus `facets` plus `evidence`; this one gained
        `title` and never gained the other two, so a policy restated in an
        evidence note escaped while the identical sentence in a body failed the
        build (#4464). 89 of the 338 active objects carry more than 120
        characters of facet or evidence *prose*, and a retrieval puts all of it
        in front of the agent. (#4464 says 164; that figure measures the JSON
        serialisation, whose braces, quotes and keys are not text anyone reads.
        Both are right about their own unit.) Strings are joined as prose, not
        `json.dumps`-ed: serialising drops quotes, braces and escapes between
        words the phrase patterns need adjacent.

        One object per field that only that field can fail on, because a fixture
        carrying the claim in two places proves neither. `evidence` nests the
        claim inside a list of objects, so dropping the recursion fails here too.
        """
        fields = {
            "evidenced": {"evidence": [{"note": "Confirmed in #3643: still one PR per session."}]},
            "faceted": {"facets": {"shape": "Still one PR per session."}},
            "titled": {"title": "Still one PR per session"},
        }
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            objects = root / ".memory/memory/constraints"
            objects.mkdir(parents=True)
            for name, field in fields.items():
                (objects / f"{name}.md").write_text(
                    "Nothing to see in the body.\n", encoding="utf-8"
                )
                (objects / f"{name}.json").write_text(
                    json.dumps(
                        {
                            "id": f"constraint.{name}",
                            "status": "active",
                            "title": "A title that names no policy",
                            "body_path": f"memory/constraints/{name}.md",
                        }
                        | field
                    ),
                    encoding="utf-8",
                )
            self.assertEqual(
                active_memory_policy_offenders(root),
                [
                    f".memory/memory/constraints/{name}.json: {ONE_PR_PER_SESSION_REASON}"
                    for name in sorted(fields)
                ],
            )

    def test_the_superseded_policy_scan_detects_a_claim(self):
        """The live-data check above passes vacuously once the store is clean.

        Nothing in it proves the patterns still match anything, so gutting them
        would leave the module green. These cases call the same module-level
        objects that check calls -- not a private copy -- so an emptied or
        broken pattern fails here.
        """
        self.assertEqual(
            superseded_policy_offences("Still one PR per session: a single final PR."),
            [ONE_PR_PER_SESSION_REASON],
        )
        self.assertEqual(
            superseded_policy_offences(
                "Orchestrator checks in on any delegated background task after 30 minutes\n"
                "Check in every 30 minutes on a silent delegate."
            ),
            [DELEGATE_INTERVAL_REASON],
        )

    def test_recording_a_supersession_is_no_longer_a_grammar_exemption(self):
        """The marker list is gone, and with it the judgement it was making.

        Deciding from the prose whether a sentence *records* a supersession or
        *asserts* a policy is a semantic call, and it failed twice: it needed an
        unbounded abbreviation lexicon to find its sentence boundaries (#4468),
        and in #4477 a current-tense refinement of it rejected the exact record
        the Learning-loop table asks an agent to write. Naming a retired policy
        is now simply naming it; whether that is legitimate is answered by
        `POLICY_RECORD_ALLOWLIST`, one reviewable line, not by a word list that
        has to guess the author's intent (#4484).
        """
        for record in (
            "One PR per session is superseded as of 2026-07-20.",
            "One PR per session was retired on 2026-07-20.",
            "One PR per session is no longer the rule.",
            "We used to open one PR per session.",
        ):
            with self.subTest(record=record[:40]):
                self.assertEqual(superseded_policy_offences(record), [ONE_PR_PER_SESSION_REASON])

    def test_citing_a_memory_object_by_id_is_a_pointer_not_a_claim(self):
        """Two live ids name a retired policy in their own slug.

        #4484 asked for the strip to go with the grammar; it was deleted and
        put back, because deleting it turned the house style for citing a
        memory object into a build failure. The second id below is the
        canonical memory about check-in cadence, so it is exactly the object a
        future memory would cross-reference, and the hyphenated-interval fix is
        what brought it into range.

        Both spellings the live store actually uses are pinned -- the full id
        and the bare slug -- because the strip is a set membership now and a set
        that holds only one of them turns the other into a build failure, which
        is the failure deleting the strip caused in the first place. These are
        read from the live store rather than typed as literals, so renaming
        either object fails here.

        The claim outside the markup is judged normally, which is the half that
        matters. That fixture is caught because the phrase sits *outside* the
        brackets, and it would have been caught under per-sentence scoping too
        -- it is two sentences, so it distinguishes nothing about scope. The
        docstring used to credit whole-object scanning for it, which was simply
        wrong (#4521 R3); what actually pins scope is
        `test_the_scan_reaches_its_verdict_without_parsing_sentences`, where the
        subject and the figure sit in different sentences.
        """
        citable = memory_object_identifiers(ROOT / ".memory")
        for citation in (
            "See [[constraint.one-branch-one-worktree-one-pr-per-session]] for the incident.",
            "See [[one-branch-one-worktree-one-pr-per-session]] for the incident.",
            "See [[workflow.orchestrator-checks-in-on-any-delegated-background-task"
            "-after-30-minutes]].",
            "See [[orchestrator-checks-in-on-any-delegated-background-task-after-30-minutes]].",
        ):
            with self.subTest(citation=citation[:45]):
                self.assertEqual(superseded_policy_offences(citation, citable), [])
        self.assertEqual(
            superseded_policy_offences(
                "See [[one-branch-one-worktree-one-pr-per-session]]."
                " Still one PR per session for this work.",
                citable,
            ),
            [ONE_PR_PER_SESSION_REASON],
        )
        # One link at a time: `[^\]]+` stops at the first `]`, and a greedy
        # `.+` would swallow the prose between two citations along with them --
        # so an object that cites twice could say anything in between. Real ids
        # on both sides, or the strip declines them on membership and the case
        # proves nothing about greed.
        self.assertEqual(
            superseded_policy_offences(
                "Between [[constraint.one-branch-one-worktree-one-pr-per-session]] and still"
                " one PR per session, see [[gotcha.java25-isuite-mocking]].",
                citable,
            ),
            [ONE_PR_PER_SESSION_REASON],
        )
        # And a link is replaced by a separator, not closed up. Substituting an
        # empty string fuses the text on either side of it into one token,
        # which manufactures both a subject and a phrase that no one wrote.
        # Real ids again, for the same reason.
        for fused in (
            "Wake the sub[[constraint.one-branch-one-worktree-one-pr-per-session]]agent"
            " every 30 minutes.",
            "The one[[constraint.one-branch-one-worktree-one-pr-per-session]] PR per session"
            " rule.",
        ):
            with self.subTest(fused=fused[:40]):
                self.assertEqual(superseded_policy_offences(fused, citable), [])

    def test_citing_a_memory_object_by_file_path_is_a_pointer_too(self):
        """The residual the markup-only strip left behind (#4516).

        `[[id]]` is the house style and a path is the exception, but the two
        live ids that carry a retired policy in their own slug were exempt in
        one spelling and flagged in the other -- and the escape that would have
        cost was an allowlist entry exempting a whole object for a citation.
        Once the strip is a membership rather than a delimiter, the object named
        is what matters and the spelling is not.

        Nothing in the store cites either of those two by path today, so this
        closes a trap rather than a live failure. That is the moment to close
        it: the alternative is discovering it as a red gate mid-edit, where the
        cheapest repair is the allowlist.
        """
        citable = memory_object_identifiers(ROOT / ".memory")
        for citation in (
            "See .memory/memory/constraints/"
            "one-branch-one-worktree-one-pr-per-session.md for the incident.",
            "See .memory/memory/workflows/orchestrator-checks-in-on-any-delegated"
            "-background-task-after-30-minutes.json.",
        ):
            with self.subTest(citation=citation[:45]):
                self.assertEqual(superseded_policy_offences(citation, citable), [])
        # A path that names no object is not a pointer, in either direction:
        # this one reads like a citation and points at nothing.
        self.assertEqual(
            superseded_policy_offences(
                "See .memory/memory/constraints/one-pr-per-session.md.", citable
            ),
            [ONE_PR_PER_SESSION_REASON],
        )
        # And the path ends where the path ends. A pattern that ran on into the
        # sentence would take the claim with it -- which is the whole hazard of
        # exempting anything by shape.
        self.assertEqual(
            superseded_policy_offences(
                "See .memory/memory/constraints/one-branch-one-worktree-one-pr-per-session.md"
                " and still one PR per session.",
                citable,
            ),
            [ONE_PR_PER_SESSION_REASON],
        )
        # A separator, not an empty string, for this spelling too.
        self.assertEqual(
            superseded_policy_offences(
                "The one.memory/memory/constraints/"
                "one-branch-one-worktree-one-pr-per-session.md PR per session rule.",
                citable,
            ),
            [],
        )

    def test_a_bracket_wrap_around_a_policy_phrase_is_not_a_citation(self):
        """The cheapest repair an agent under pressure reaches for (#4521).

        The strip was content-blind, so two bracket pairs silenced any claim in
        this file: `[[one-PR-per-session]]` read clean, and so did a wrapped
        subject beside a live figure. Unlike an allowlist line -- which names an
        immutable id and is visible in review -- a bracket wrap diffuses into
        memory prose and reads as house style, so it is iron law 4 pressure with
        nothing to catch it.

        Membership is what separates the two, and it is no more of a semantic
        call than the phrase list: a citation points at an object that exists,
        and a wrap around a policy phrase points at nothing. The last case is
        the malformed-first-link hole -- an unclosed pair swallowed the prose
        between two citations and the whole span read clean, because `[^\\]]+`
        ran from the first `[[` to the first `]]`. The swallowed span is not an
        id, so it no longer strips.
        """
        citable = memory_object_identifiers(ROOT / ".memory")
        for wrapped, expected in (
            ("The [[one-PR-per-session]] default still governs this session.",
             [ONE_PR_PER_SESSION_REASON]),
            ("Check in on a [[delegate]] every 30 minutes.", [DELEGATE_INTERVAL_REASON]),
            ("Check in on a delegate every [[30 minutes]].", [DELEGATE_INTERVAL_REASON]),
            ("See [[constraint.one-branch-one-worktree-one-pr-per-session and still one PR"
             " per session, see [[gotcha.java25-isuite-mocking]].",
             [ONE_PR_PER_SESSION_REASON]),
        ):
            with self.subTest(wrapped=wrapped[:45]):
                self.assertEqual(superseded_policy_offences(wrapped, citable), expected)
        # An id that no longer exists is not a pointer either. This is the
        # direction that keeps the set honest: without it, a set built from
        # anything at all -- or from nothing -- would look identical here.
        self.assertEqual(
            superseded_policy_offences(
                "See [[constraint.one-branch-one-worktree-one-pr-per-session]].", frozenset()
            ),
            [ONE_PR_PER_SESSION_REASON],
        )

    def test_an_allowlist_entry_clears_one_object_for_one_policy(self):
        """Not a blanket pardon, in either direction.

        An entry names a policy and an id, so it must clear that object for
        that policy only: the same object still fails on a policy it was not
        cleared for, and a different object still fails on the policy it was.
        Both directions are checked because a mistake in either -- keying by id
        alone, or applying any entry to every object -- turns one reviewable
        line into a hole with no visible edge, which is the failure mode the
        automatic inference had.
        """
        claim = (
            "Still one PR per session for this work, and check in on a "
            "delegate every 30 minutes.\n"
        )
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = self.synthetic_store(temporary_directory, {"cleared": claim, "other": claim})
            self.assertEqual(
                active_memory_policy_offenders(
                    root,
                    {ONE_PR_PER_SESSION_REASON: frozenset({"constraint.cleared"})},
                ),
                [
                    f".memory/memory/constraints/cleared.json: {DELEGATE_INTERVAL_REASON}",
                    f".memory/memory/constraints/other.json: {DELEGATE_INTERVAL_REASON}",
                    f".memory/memory/constraints/other.json: {ONE_PR_PER_SESSION_REASON}",
                ],
            )
            # An explicitly empty allowlist is not the shipped one, and only a
            # non-empty shipped list can show the difference -- so one is put
            # there for the length of this assertion. Without it `allowlist or
            # POLICY_RECORD_ALLOWLIST` is indistinguishable from the shipped
            # code, and a caller asking for no exemptions would silently get
            # the standing ones the day the list stops being empty.
            with mock.patch.dict(
                POLICY_RECORD_ALLOWLIST,
                {ONE_PR_PER_SESSION_REASON: frozenset({"constraint.cleared"})},
            ):
                self.assertEqual(
                    active_memory_policy_offenders(root, {}),
                    [
                        f".memory/memory/constraints/cleared.json: {DELEGATE_INTERVAL_REASON}",
                        f".memory/memory/constraints/cleared.json: {ONE_PR_PER_SESSION_REASON}",
                        f".memory/memory/constraints/other.json: {DELEGATE_INTERVAL_REASON}",
                        f".memory/memory/constraints/other.json: {ONE_PR_PER_SESSION_REASON}",
                    ],
                )
                # …and `None` does take it, which is what makes the pair a test
                # of the default rather than of the empty case alone.
                self.assertEqual(
                    active_memory_policy_offenders(root),
                    [
                        f".memory/memory/constraints/cleared.json: {DELEGATE_INTERVAL_REASON}",
                        f".memory/memory/constraints/other.json: {DELEGATE_INTERVAL_REASON}",
                        f".memory/memory/constraints/other.json: {ONE_PR_PER_SESSION_REASON}",
                    ],
                )

    def test_an_incident_narrative_clears_through_the_allowlist(self):
        """The escape that is the whole premise of dropping the verb.

        A1 flags a delegate subject beside a minute figure and makes no attempt
        to tell a cadence policy from an incident, because #4477 established
        that no lexical rule can. That is only acceptable if the narrative has
        somewhere to go, and #4469's own narrative is the fixture for it. Pinned
        because nothing else clears an object for `DELEGATE_INTERVAL_REASON`:
        with only the policy-phrase side exercised, an allowlist that could
        never clear a cadence false positive would look identical from the
        outside, and the argument for A1 would rest on an untested claim.
        """
        narrative = "The delegate went silent for 90 minutes before I checked in.\n"
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = self.synthetic_store(temporary_directory, {"incident": narrative})
            self.assertEqual(
                active_memory_policy_offenders(root),
                [f".memory/memory/constraints/incident.json: {DELEGATE_INTERVAL_REASON}"],
            )
            self.assertEqual(
                active_memory_policy_offenders(
                    root, {DELEGATE_INTERVAL_REASON: frozenset({"constraint.incident"})}
                ),
                [],
            )

    def test_the_field_set_is_joined_as_separate_lines_not_run_together(self):
        """A join is a match surface, so the separator is a rule like any other.

        `searchable_text` concatenates title, body, facets and evidence. Joining
        them with a space fuses the tail of one field to the head of the next
        and manufactures a phrase that no field contains -- the fields are
        separate strings a retrieval renders separately, and a claim has to sit
        inside one of them to be a claim. Joining with nothing at all fuses them
        harder, into single tokens. `\\n` was load-bearing for the sentence
        splitter and nothing re-pinned it once the splitter went, so both
        directions get a case: one where a space would manufacture the policy
        phrase, one where an empty join would manufacture the subject.
        """
        for name, title, body in (
            ("phrase", "The rule is one", "PR per session applies to this work.\n"),
            ("subject", "Wake the sub", "agent every 30 minutes.\n"),
        ):
            with self.subTest(fuses=name), tempfile.TemporaryDirectory() as temporary_directory:
                root = Path(temporary_directory)
                objects = root / ".memory/memory/constraints"
                objects.mkdir(parents=True)
                (objects / f"{name}.md").write_text(body, encoding="utf-8")
                (objects / f"{name}.json").write_text(
                    json.dumps(
                        {
                            "id": f"constraint.{name}",
                            "status": "active",
                            "title": title,
                            "body_path": f"memory/constraints/{name}.md",
                        }
                    ),
                    encoding="utf-8",
                )
                self.assertEqual(active_memory_policy_offenders(root), [])

    def synthetic_store(self, temporary_directory: str, bodies: dict[str, str]) -> Path:
        """Write a throwaway `.memory` store of active constraints and return its root."""
        root = Path(temporary_directory)
        objects = root / ".memory/memory/constraints"
        objects.mkdir(parents=True)
        for name, body in bodies.items():
            (objects / f"{name}.md").write_text(body, encoding="utf-8")
            (objects / f"{name}.json").write_text(
                json.dumps(
                    {
                        "id": f"constraint.{name}",
                        "status": "active",
                        "title": "",
                        "body_path": f"memory/constraints/{name}.md",
                    }
                ),
                encoding="utf-8",
            )
        return root

    def test_every_allowlist_entry_is_individually_load_bearing(self):
        """An entry that clears nothing is a rule nothing enforces.

        That is the defect all three review rounds found, wearing a different
        hat each time, so the allowlist gets the same treatment every surviving
        pattern member gets: each entry must name a live active object that
        really does trip the scan without it. A stale entry -- object renamed,
        object retired, prose rewritten -- fails here rather than sitting on as
        a silent standing exemption.

        This docstring used to say the allowlist was empty and that this test
        therefore ran vacuously. Two entries have been added since (#4516,
        #4521) and the sentence was not updated, so it claimed the check was
        unexercised while it was in fact pinning both -- a doc defect that
        invites the next reader to treat a live check as dead weight.

        Empty remains the healthy *end* state. Requiring a live entry was tried
        and removed: it left reconciling the last exempt object with no legal
        state, because keeping its entry tripped the staleness check here and
        removing it tripped the non-empty one, and the only exit was to weaken
        a check.
        """
        memory_root = ROOT / ".memory"
        objects = {}
        for metadata_path in (memory_root / "memory").rglob("*.json"):
            metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
            objects[metadata["id"]] = metadata
        for reason, identifiers in POLICY_RECORD_ALLOWLIST.items():
            self.assertTrue(identifiers, reason)
            for identifier in identifiers:
                with self.subTest(identifier=identifier[:60]):
                    self.assertIn(identifier, objects)
                    metadata = objects[identifier]
                    self.assertEqual(metadata.get("status", "active"), "active")
                    self.assertIn(
                        reason,
                        superseded_policy_offences(
                            searchable_text(memory_root, metadata),
                            memory_object_identifiers(memory_root),
                        ),
                    )

    def test_every_allowlist_entry_carries_its_own_stated_reason(self):
        """#4535: the justification has to live in the entry, not beside it.

        An exemption is only reviewable if a reader can see what it is for.
        Both shipped entries were justified -- at length -- but in comments
        above the declaration, which nothing binds to the id they explain.
        Reorder the entries, add a third between them, or delete one, and the
        prose stays where it was, now attached to whatever moved into its
        place. A justification that can drift away from its subject is a
        comment, not a record.

        Keyed as `{reason: {object id: why}}` so the pairing is structural.
        The call site is unchanged: `in` tests keys either way, which is also
        why fixtures passing a bare set keep working.

        **Why `why` is not required to quote the sentence that trips the
        scan.** That was the obvious stronger rule -- a quote makes staleness
        mechanically detectable rather than merely visible -- and it is
        unsatisfiable here. `workflow.after-opening-a-pr-rely-on-ci-push-
        notifications-not-a-self-polling-monitor` trips A1 on two strings in
        different places: "It idles as a frozen background agent" supplies the
        subject and a separate `~20-30 min` supplies the figure. No single
        sentence trips it, so the requirement would have no legal answer for a
        live entry -- the same deadlock shape that removing the non-empty
        requirement fixed, and the module warns about in four places.

        Staleness of the entry is already covered by
        `test_every_allowlist_entry_is_individually_load_bearing`, which fails
        when the object stops needing its exemption. This test covers the
        different failure of an entry that is still load-bearing and no longer
        says anything a reviewer can act on.
        """
        for reason, permitted in POLICY_RECORD_ALLOWLIST.items():
            self.assertIsInstance(
                permitted,
                dict,
                f"{reason[:40]}: entries map object id -> why, not a bare set",
            )
        for reason, permitted in POLICY_RECORD_ALLOWLIST.items():
            for identifier, why in permitted.items():
                with self.subTest(identifier=identifier[:60]):
                    self.assertTrue(
                        why.strip(), "an empty why is an exemption nobody can review"
                    )
                    # A word count, because "n/a", "see above" and "legacy" are
                    # all non-empty and all say nothing. Deliberately low: the
                    # check is against a placeholder, not a house style, and a
                    # threshold high enough to have opinions about good prose
                    # is one people pad to satisfy.
                    self.assertGreaterEqual(
                        len(why.split()),
                        8,
                        f"{identifier[:60]}: state what this object says and why it is not a claim",
                    )

    def test_every_superseded_policy_phrasing_is_individually_load_bearing(self):
        """One phrasing per case, for the same reason.

        "Still one PR per session: a single final PR." matches two alternatives
        at once, so four of the five could be deleted with the suite green.
        Both spellings of the separator get a case too, since `[- ]` collapsing
        to either one alone is a silent narrowing.
        """
        for phrasing, claim in (
            ("one PR per session", "Still one PR per session for this work."),
            ("one-PR-per-session", "The one-PR-per-session default governs this work."),
            ("1 PR per session", "Still 1 PR per session for this work."),
            ("single final PR", "This session ends with a single final PR."),
            ("single session PR", "This work ends with a single session PR."),
            (
                "one-branch-one-worktree-one-pr",
                "The one-branch-one-worktree-one-pr constraint governs this work.",
            ),
            ("session's single branch/pr", "Work lands inside the session's single branch/pr."),
            ("sessions single branch/pr", "Work lands inside the sessions single branch/pr."),
        ):
            with self.subTest(phrasing=phrasing):
                self.assertEqual(superseded_policy_offences(claim), [ONE_PR_PER_SESSION_REASON])
        # The `1 PR` alternative needs its leading boundary, which nothing else
        # pins: without it any figure ending in 1 reads as the retired policy,
        # and issue numbers sit next to this phrase constantly in this store.
        self.assertNotRegex("#31 PR per session batches shipped", SUPERSEDED_ONE_PR_PER_SESSION)
        # And the separator is exactly one character. `[- ]*` reads "onepr per
        # session" as the policy; a class that admits runs it never needs is a
        # widening no positive fixture can catch.
        self.assertNotRegex("onepr per session shipped", SUPERSEDED_ONE_PR_PER_SESSION)

    def test_the_scan_reaches_its_verdict_without_parsing_sentences(self):
        """The property that ends three rounds of grammar defects.

        Restoring either half of the deleted grammar changes one of these, so
        this is where a fourth attempt to reason about prose gets caught. The
        first record carries a supersession marker in the same breath as the
        claim, so any marker list -- however it is scoped -- reads it as clean.
        The second puts the subject and the figure in different sentences, so
        any sentence splitter loses the cadence.
        """
        self.assertEqual(
            superseded_policy_offences(
                "Still one PR per session, and the per-phase pattern was retired."
            ),
            [ONE_PR_PER_SESSION_REASON],
        )
        self.assertEqual(
            superseded_policy_offences(
                "A delegate went quiet. The orchestrator waited 30 minutes."
            ),
            [DELEGATE_INTERVAL_REASON],
        )

    def test_the_interval_scan_needs_both_of_its_two_signals(self):
        """Either signal alone is ordinary prose, and both are common alone.

        A memory object mentioning a delegate is unremarkable, and so is one
        mentioning minutes. The conjunction is the rule, and dropping either
        conjunct would turn a large share of the store red -- which is the
        pressure that gets a check weakened rather than a memory fixed, and
        iron law 4 forbids the repair an agent would reach for.
        """
        for benign in (
            "The reviewer returned 3 minor findings on the delegate's diff.",
            "A delegate rebased onto main and pushed.",
            "Compaction after a merge takes 2 minutes.",
        ):
            with self.subTest(text=benign[:40]):
                self.assertEqual(superseded_policy_offences(benign), [])

    def test_the_interval_scan_no_longer_tries_to_tell_policy_from_narrative(self):
        """The cost A1 pays, stated rather than hidden.

        #4469 wanted these two narratives clean and #4477 added the three below
        them as proof that no lexical rule could keep them clean: `read` is its
        own past tense, and `review` and `wake` are nouns as readily as verbs.
        All five now flag. That is deliberate -- the alternative is a grammar
        that costs a review round per phrasing, and none of these five appears
        in the live store, where the rule flags zero of 338 active objects. An
        object that really does narrate an incident this way adds one
        allowlisted line, and unlike a grammar hole that line is visible in
        review.
        """
        for narrative in (
            "The delegate went silent for 90 minutes before I checked in.",
            "A silent background job cost 45 minutes of wall clock this session.",
            "A review of the background job took 40 minutes.",
            "I read the delegate's partial output 20 minutes in and it was looping.",
            "The subagent wake cost 30 minutes of wall clock.",
            # The plainest of the lot, and the one the shipped check used to
            # pass. It is the ordinary shape of a Learning-loop note, so it
            # belongs on this list rather than deleted from the module.
            "A delegate's first pass takes about 40 min; size the wakeup accordingly.",
        ):
            with self.subTest(text=narrative[:40]):
                self.assertEqual(superseded_policy_offences(narrative), [DELEGATE_INTERVAL_REASON])

    def test_every_interval_figure_spelling_is_individually_load_bearing(self):
        """One spelling per case, so no piece of the figure can be gutted unnoticed.

        The pattern is small and every piece of it earns a fixture, because an
        unpinned piece is an unenforced rule -- the defect all three review
        rounds found. `30-minute` is the piece that was missing outright in the
        base check and in #4477's (#4484 Part B); the rest guard the separator,
        the optional `ute` and `s`, both digit bounds and the case fold.
        """
        for spelling, policy in (
            ("space", "Check in on a delegate every 30 minutes."),
            ("hyphen", "A delegate is polled on a 30-minute cadence."),
            ("no separator", "Wake on a subagent every 20min."),
            ("bare min", "Wake on a subagent every 20 min."),
            ("singular minute", "Review a delegated task at 30 minute intervals."),
            ("upper case", "Check in on a delegate every 30 MINUTES."),
            ("one digit", "Check in on a delegate every 5 minutes."),
            ("three digits", "Check in on a delegate every 120 minutes."),
            # A wrapped line is one cadence, not two things. This is why the
            # separator is `*` and not `?`, and it is the only fixture that can
            # tell those two apart.
            ("wrapped line", "Check in on a delegate every 30\n    minutes."),
        ):
            with self.subTest(spelling=spelling):
                self.assertEqual(superseded_policy_offences(policy), [DELEGATE_INTERVAL_REASON])
        # A figure has to be a figure of minutes and a whole one. Without the
        # trailing boundary "3 minor" reads as three minutes; without the
        # leading one a four-digit duration reads as its last three digits; and
        # the separator is whitespace, or one hyphen, or nothing -- widen it
        # into a class and a filename becomes a cadence.
        self.assertNotRegex("3 minor findings", INTERVAL_FIGURE)
        self.assertNotRegex("the build ran 1440 minutes", INTERVAL_FIGURE)
        self.assertNotRegex("artifact-3_minutes.json", INTERVAL_FIGURE)
        # And the separator is not a *mixed run* of whitespace and hyphens.
        # `[\s-]*` bought the wrapped line by admitting these two, which are
        # this repository's own punctuation rather than any cadence: ` -- ` is
        # house style in nearly every docstring in this module, and `\n- ` is a
        # markdown bullet. Both fixtures are the only thing in the suite that
        # can tell `(?:\s+|-|)` from `[\s-]*` -- every positive spelling above
        # is satisfied by either, which is how the over-wide form shipped
        # unenforced (#4521).
        self.assertNotRegex("the retry cap is 3 -- minutes of backoff", INTERVAL_FIGURE)
        self.assertNotRegex("wakeups:\n- retries: 3\n- minutes are the wrong unit", INTERVAL_FIGURE)

    def test_this_repositorys_own_punctuation_is_not_a_cadence(self):
        """The two false positives `[\\s-]*` was flagging on `main` (#4521).

        The separator fixtures above pin the pattern; these pin the whole rule,
        because that is where the cost landed. Both carry a delegate subject
        and a figure near the word "minutes", and neither states a cadence --
        the first is an em-dash aside in this repository's house punctuation,
        the second an ordinary markdown bullet list. Between them they are the
        two commonest ways prose here puts a number next to that word, so the
        widening did not flag an edge case, it flagged the file it was written
        in.
        """
        self.assertEqual(
            superseded_policy_offences(
                "The retry cap is 3 -- minutes of backoff are the runner's job"
                " -- the delegate is untouched."
            ),
            [],
        )
        self.assertEqual(
            superseded_policy_offences(
                "Delegate wakeups:\n- retries: 3\n- minutes are the wrong unit here\n"
            ),
            [],
        )

    def test_every_delegate_subject_is_individually_load_bearing(self):
        """One subject per case, so no alternative can be gutted unnoticed.

        Every interval fixture said "delegate" somewhere, so the whole
        `background task|job` branch could be deleted with the suite green --
        and that branch is the one an incident about a shell job would use. The
        plural and the case fold went unpinned the same way, and now that the
        subject is half the entire rule rather than one of three signals, an
        unenforced branch here is half a rule nothing enforces.
        """
        for subject, policy in (
            ("delegate", "Check in on a delegate every 30 minutes."),
            ("delegates", "Check in on delegates every 30 minutes."),
            ("delegated", "Check in on a delegated worker every 30 minutes."),
            ("subagent", "Check in on a subagent every 30 minutes."),
            ("subagents", "Wake on subagents every 20 minutes."),
            ("background task", "Check in on a background task every 30 minutes."),
            ("background tasks", "Poll background tasks every 25 minutes."),
            ("background job", "Check in on a background job every 30 minutes."),
            ("background jobs", "Poll background jobs every 25 minutes."),
            # The live false negative (#4516). A single object in the store
            # calls a delegate a "background agent", so renaming that phrase
            # anywhere would have turned an unrelated PR red -- a margin one
            # word wide, not the zero the flagged count suggested.
            ("background agent", "Check in on a background agent every 30 minutes."),
            ("background agents", "Poll background agents every 25 minutes."),
            ("case fold", "Subagent output is read every 30 minutes."),
        ):
            with self.subTest(subject=subject):
                self.assertEqual(
                    superseded_policy_offences(policy), [DELEGATE_INTERVAL_REASON]
                )
        # A positive fixture cannot prove a boundary; only a refusal can. Every
        # widening direction here was open -- both word boundaries, and each
        # stem loosened to `\\w*` -- and the only mutations that died, died
        # against the live-store assertion, which this module's own docstring
        # says "passes vacuously once the store is clean". That guards the
        # subject by what the store happens to contain today rather than by a
        # rule, and the subject is half the cadence rule now.
        for token in (
            "delegatee",  # trailing boundary, and `delegate\w*`
            "undelegated",  # leading boundary
            "subagentic",  # `subagent\w*`
            "background noise",  # `background \w+`
            "background agency",  # trailing boundary on the new alternative
        ):
            with self.subTest(refuses=token):
                self.assertNotRegex(token, DELEGATE_SUBJECT)

    def test_every_cadence_phrasing_a_grammar_could_not_reach_is_flagged(self):
        """The six #4469 listed, none of which a directive list ever caught.

        Each states exactly the policy this check exists to keep a second copy
        of out of memory, and each phrases the act differently: `ask`, `poll`,
        `look at`, `wake`, `steps in`, `review`. Widening the list to a verb
        class was tried in #4477 and withdrawn -- `read` is its own past tense,
        so no lexical rule separated a policy from a narrative. The subject and
        the figure are the whole rule now (#4484).
        """
        for policy in (
            "Ask a delegated agent for a progress report every 30 minutes",
            "Poll any background job that has produced no output for 25 minutes",
            "The orchestrator must look at a delegate worktree every 45 minutes",
            "Wake on a subagent every 20 min to read its partial output",
            "A delegate gets 30 minutes before the orchestrator steps in",
            "Review any delegated background task at 30 minute intervals",
        ):
            with self.subTest(policy=policy[:40]):
                self.assertEqual(superseded_policy_offences(policy), [DELEGATE_INTERVAL_REASON])

    def test_a_hyphenated_interval_is_a_minute_figure(self):
        """`30-minute` never matched, in the base check or in #4477's.

        `\\s*` between the digits and `min` admits a space or nothing and
        rejects the hyphen, so the most idiomatic way to write a cadence in
        English -- as a compound modifier -- was the one spelling that walked
        past (#4484 Part B).
        """
        self.assertEqual(
            superseded_policy_offences(
                "A 30-minute check-in cadence applies to every delegate."
            ),
            [DELEGATE_INTERVAL_REASON],
        )

    def test_the_four_objects_that_motivated_the_scan_are_still_caught(self):
        """The narrowing that would look most like a fix.

        Every later change to these patterns widens or tightens them, and a
        tightening that quietly stops detecting one of the four objects PR #4461
        reconciled would pass every other case in this module while removing the
        reason the check exists. So the pre-reconciliation prose is pinned here
        verbatim, with the reason each one has to produce, and the reconciled
        objects are read from the live store to prove the same patterns leave
        the rewrite alone.
        """
        for title, body, expected in (
            (
                "One branch, one worktree, one PR per session",
                "Default: do all of a session's work on one branch inside one "
                "worktree, and open exactly one PR for that session's final code -- "
                "do not spin up a new branch/worktree/PR per task, issue, or phase "
                "within a session, even for multiple unrelated fixes/features "
                "requested in the same session. If a PR from earlier in the session "
                "is already open, keep using that branch rather than opening a second "
                "one. This does not override the worktree-housekeeping cleanup rules "
                "for stale LEFTOVER worktrees from past sessions -- it is "
                "specifically about how many branches/worktrees/PRs a session should "
                "actively create going forward. Supersedes an earlier per-phase "
                "pattern (a separate branch/PR per phase of a large issue), which is "
                "no longer the default unless explicitly asked for again.\n"
                "\n"
                "EXCEPTION (user directive, 2026-07-17, #3643 session): when a batch "
                "of sub-issues has file-level dependencies between them (a later item "
                "needs a file/API a not-yet-merged earlier item changed), the "
                "one-PR-per-session default is the wrong shape -- it is exactly what "
                "invites parallel isolation:\"worktree\" dispatch against a stale base "
                "(see "
                "[[agent-tool-isolation-worktree-branches-from-session-start-head-not"
                "-the-evolving-session-branch]]). In that specific situation the user "
                "directed: merge the currently-completed, non-conflicting work first "
                "(PR, wait green, merge to origin/main), then for each remaining "
                "dependent item, cut a FRESH branch off the just-updated origin/main, "
                "implement that one item alone, PR it, wait for green CI, merge it, "
                "then cut the next fresh branch off the new origin/main for the "
                "following item -- one branch+PR+merge per item, sequential, never "
                "two file-dependent items in parallel worktrees. Treat this exception "
                "as standing guidance for any session with file-dependent sub-issues, "
                "not a one-off.",
                [ONE_PR_PER_SESSION_REASON],
            ),
            (
                "Agent /loop operating rhythm: compact after every merge, overlap CI, ping-or-close delegates",
                "The operating rhythm for a development `/loop`: (1) Compact after "
                "every merge -- the moment a PR merges cleanly (green CI + merge "
                "confirmed), the next action is /compact, then the next work item "
                "(each merge is a checkpoint; pre-merge detail is spent context). Do "
                "NOT compact mid-PR. (2) CI notifications, not polling, and never "
                "idle-wait on green -- after opening a PR do not arm a self-polling "
                "checks Monitor; the environment pushes events only on FAILURE or a "
                "review comment, so an all-green PR sends NO push. Overlap/pipeline: "
                "while child N's CI runs, start child N+1's work; if you must gate on "
                "CI, size the wakeup to actual CI duration (~300s) and proactively "
                "check + merge, never a long idle fallback. (3) Ping-or-close "
                "long-running delegates -- any forked subagent or background task "
                "running >~20 min gets pinged for status (SendMessage / read its "
                "output) and is closed if idle/stuck; while a delegate is in flight "
                "set the loop's fallback wakeup to ~1200s.",
                [DELEGATE_INTERVAL_REASON],
            ),
            (
                "Commit and push incrementally within a session's single branch/PR, not just once at the end",
                "Within the standing one-branch-one-worktree-one-pr-per-session "
                "constraint, commit and push each logically complete, "
                "independently-compiling/testing increment of work as it's finished, "
                "rather than batching everything into one commit at session end. In "
                "this session (#3409/PR #3411) that meant a separate commit+push for: "
                "(1) verbose-detail parser enrichment + toggle-safety fix together "
                "(they touched the same two files and were verified together), (2) "
                "the new LocalAgentApprovalBridge class + its own tests, (3) wiring "
                "the bridge into the runner, (4) wiring it into the panel -- each "
                "pushed immediately after its own compile+test pass, before moving to "
                "the next piece. This does not change the one-PR-per-session rule -- "
                "still a single branch, single final PR -- it only changes "
                "commit/push cadence within that one PR.",
                [ONE_PR_PER_SESSION_REASON],
            ),
            (
                "Orchestrator checks in on any delegated background task after 30 minutes",
                "Owner directive (2026-07-18): background tasks must not run "
                "unattended indefinitely. Standard practice: if a delegated agent or "
                "background job has produced no completion or interim signal for 30 "
                "minutes, the orchestrator checks in -- SendMessage to the agent "
                "asking for a status snapshot, or inspect its partial "
                "output/worktree. Repeat every 30 minutes; at ~90 minutes with no "
                "convergence, intervene: narrow the scope, split the task, or stop "
                "and respawn. CI monitors streaming per-check events are exempt (they "
                "have their own cadence); the rule targets silent long-running "
                "agents. Rationale: a 69-minute Allure agent ran silent this session "
                "and the owner flagged it.",
                [DELEGATE_INTERVAL_REASON],
            ),
        ):
            with self.subTest(before=title[:40]):
                self.assertEqual(superseded_policy_offences(f"{title}\n{body}"), expected)
        memory_root = ROOT / ".memory"
        for reconciled in (
            "memory/constraints/one-branch-one-worktree-one-pr-per-session.json",
            "memory/workflows/agent-loop-operating-rhythm-compact-after-every"
            "-merge-overlap-ci-ping-or-close-delegates.json",
            "memory/workflows/commit-and-push-incrementally-within-a-sessions"
            "-single-branch-pr-not-just-once-at-the-end.json",
            "memory/workflows/orchestrator-checks-in-on-any-delegated"
            "-background-task-after-30-minutes.json",
        ):
            with self.subTest(after=reconciled.rsplit("/", 1)[-1][:40]):
                metadata = json.loads((memory_root / reconciled).read_text(encoding="utf-8"))
                self.assertEqual(
                    superseded_policy_offences(
                        searchable_text(memory_root, metadata),
                        memory_object_identifiers(memory_root),
                    ),
                    [],
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
