"""No harness element is orphaned from the entrypoint, and the duties it owes are unqualified.

Tracker #4485. The owner's directive is that an agent loading
`.agents/skills/act-as-mohab/SKILL.md` reaches the *whole* harness from that
one skill, on any host. "No orphans" is worthless as a sentence in a guidance
file -- #4488 measured 38 tracked harness files unreachable from the
entrypoint while the guidance claimed completeness -- so this module is the
graph walk that fails on the next unlinked file.

WHAT THIS CATCHES

1. A tracked harness element that is neither reachable from the entrypoint nor
   registered as an exemption with a stated reason. The element set comes from
   globs over every committed and every untracked-but-unignored path that
   exists on disk, never a hand-written list, so a file added tomorrow is an
   element tomorrow -- a hand list silently omits the next file, which is
   exactly how the 38 accumulated. Reachability itself is never bought by a
   wildcard: only a real link or an exact path token counts, because a
   wildcard re-derives itself from the tree it is meant to be checking.
2. A markdown link in the reachable graph whose target does not exist. A broken
   link is worse than a missing one: it reads as coverage.
3. A path token, in a code span or fence of a reachable file, that names a
   harness root but resolves to nothing tracked. #4481 recorded four of five
   references-glob copies failing open when the path moved; this is the half of
   that failure this module can close, because the out-of-tree half of
   reachability is carried by named paths rather than links (below).
4. An exemption with no reason, an exemption that is not a string path, or an
   exemption that matches no tracked file. An exemption with no reason is an
   orphan wearing a hat; a dead exemption is a hatch nobody closed.
5. The entrypoint's arm/watch/fix/confirm PR duty (#4486) and the two named
   workflows (#4487) deleted, renamed, or weakened by qualification -- and the
   workflow anchors pointing at a section heading that no longer exists.

WHY TWO REACHABILITY MECHANISMS

#4489 asked for links, not mentions, and inside `.agents/skills` that is what
this enforces: a backtick mention of an in-tree file buys nothing, only a real
markdown link does. It cannot be links everywhere, because
`test_agent_harness_portability.py::test_deployed_canonical_subtree_has_no_external_or_broken_markdown_links`
treats any link escaping `.agents/skills` as broken -- correctly, since the
tree deploys to a user-level skills directory where `../../../scripts/...`
points at nothing. So an out-of-tree element is reached by being named as a
path token in a reachable file, and check 3 above is what stops that from
degrading into an unverified mention.

WHAT THIS DOES NOT CATCH -- read before inferring coverage from a green run

- **Reachable is not read.** A link proves an agent *can* follow it. Nothing
  here proves the linking sentence gives it a reason to, or that the target
  says anything useful. A link from a section an agent skips counts as
  reachable.
- **An over-broad exemption glob passes.** Each exemption is checked for a
  reason and for matching something real; `**/*` with a plausible sentence
  would satisfy both and exempt the entire harness. Nothing here judges an
  exemption's width or the quality of its reason.
- **A path token outside a code span is invisible.** Only spans and fences are
  scanned, so a stale path written as plain prose is neither reachability nor a
  failure. That is deliberate -- prose paths are how "mentions" got rejected --
  but it means a prose path that moved stays silently wrong.
- **`element_globs` bounds the whole question.** A harness surface added under
  a path no glob covers is not an element, so it cannot be reported as an
  orphan. This is the one place a hand-maintained decision still lives, and it
  is the failure mode most likely to bite next. `EXPECTED_ELEMENT_COUNT` makes
  the boundary *shrinking* loud; it does nothing about a surface that was never
  inside it. Two exclusions were argued for in review and both were wrong.
- **A detoured root is normalised, not rejected.** `root` is resolved at entry
  so a symlinked or short-name checkout walks correctly. Nothing asserts that
  the caller's spelling and the resolved one name the same tree -- if a caller
  passes a root that resolves somewhere else entirely, the walk reports on
  wherever it resolved to.
- **The run-list pin cannot guard its own removal.** Every pin here lives in a
  module whose execution that same run list decides. The
  `harness_reachability` row in `agent_harness_parity.json` routes the
  question through `validate_agent_setup.py`, a separate PR Gate step, which
  is what actually closes the circle. Edit the run list and that row together
  and nothing notices.
- **The walk itself is not run by `validate_agent_setup.py`.** Only its
  membership in the gate is, through the parity row. `py -3
  scripts/ci/validate_agent_setup.py --skip-external` still exits 0 on a fully
  orphaned harness; the unittest module is what reports orphans.
- **Not every link in the entrypoint is load-bearing.** Reachability flows
  through the skills map, so a reference the map also lists survives losing
  its entrypoint link. The hub link to the map is guarded; the parallel ones
  are redundant by design.
- **Anchors are checked against headings, not rendered output.** A duplicate
  heading in one file produces `-1`-suffixed slugs on GitHub that this would
  call broken, and a heading reachable only through a link this walk cannot see
  is not considered.
- **The clause pins in `EntrypointDutyTest` are word-level.** They fail on
  deletion, on renaming, and on the hedges listed in `OPTIONALITY_HEDGE`, which
  is the weakening shape #4469 named. A duty rewritten into uselessness with
  vocabulary nobody listed still passes.
"""

from __future__ import annotations

import json
import re
import subprocess  # nosec B404 - tests exercise trusted local commands.
import tempfile
import unittest
from pathlib import Path

from scripts.ci.harness_reachability import (
    glob_regex,
    harness_report,
    link_walk,
    load_config,
    path_tokens,
    tracked_files,
)


ROOT = Path(__file__).resolve().parents[2]
BUDGET = ROOT / "scripts/ci/agent_guidance_budget.json"

# The exact size of the harness surface, asserted by equality rather than as a
# floor. A floor of 84 against a real 85 meant one whole glob could be deleted
# from `element_globs` and stay green -- dropping `scripts/ci/watch_pr_checks.py`
# from the boundary was invisible. Equality makes shrinking the boundary a
# deliberate two-line edit that shows up in review, which is the only place the
# question "why is the harness smaller today" gets asked.
EXPECTED_ELEMENT_COUNT = 170

ATX_HEADING = re.compile(r"(?m)^#{1,6}\s+(.+?)\s*$")

# Weakening by addition is the mutation a presence pin misses (#4469): every
# pinned word survives "where practical". These are checked inside the section
# that carries the duty, so an unrelated hedge elsewhere in the entrypoint is
# not this test's business.
OPTIONALITY_HEDGE = re.compile(
    r"(?i)\b(?:where|when|if)\s+(?:practical|feasible|possible|convenient|time)"
    r"|\bfor significant\b|\bas appropriate\b|\bconsider (?:arming|watching)\b"
    r"|\bshould consider\b|\bwhere it makes sense\b|\bat your discretion\b"
    r"|\bbest[- ]effort\b|\boptional\b|\bnice to have\b|\bif you can\b"
)



def heading_slugs(path: Path) -> set[str]:
    """Return GitHub-style anchor slugs for one markdown file's ATX headings."""
    slugs = set()
    for heading in ATX_HEADING.findall(path.read_text(encoding="utf-8")):
        slug = re.sub(r"[^a-z0-9 -]", "", heading.lower()).strip().replace(" ", "-")
        slugs.add(re.sub(r"-{2,}", "-", slug))
    return slugs


def section_body(content: str, heading: str) -> str:
    """Return one ATX section's body, up to the next heading of the same or higher level."""
    level = len(heading) - len(heading.lstrip("#"))
    match = re.search(
        rf"(?m)^{re.escape(heading)}\s*$\n(.*?)(?=^#{{1,{level}}}\s|\Z)",
        content,
        re.DOTALL,
    )
    return match.group(1) if match else ""


class HarnessReachabilityTest(unittest.TestCase):
    def test_every_harness_element_is_reachable_or_exempt_with_a_reason(self):
        """The tracker's headline measurement, as a gate rather than a sentence.

        #4488 measured 33 of 71 tracked harness files reachable from the
        entrypoint. Anything still unreachable is listed by path here, with
        which mechanism it failed, so the fix is not a search.
        """
        self.assertEqual(harness_report(ROOT)["orphans"], [])

    def test_no_link_in_the_reachable_graph_is_broken(self):
        """A link whose target is gone reads as coverage while providing none."""
        self.assertEqual(harness_report(ROOT)["broken_links"], [])

    def test_every_named_harness_path_resolves_to_something_tracked(self):
        """The half of reachability that links cannot carry must not fail open.

        Out-of-tree elements are reached by being named, and #4481 is the
        record of what an unchecked name is worth: four of five copies of a
        references glob kept passing after the path moved. A named path that
        resolves to nothing fails here instead.
        """
        self.assertEqual(harness_report(ROOT)["stale_named_paths"], [])

    def test_an_instrument_the_guidance_names_becomes_an_element_without_a_glob(self):
        """#4531 gap 1, proven by mutation rather than by a clean live run.

        Asserting the live report only shows today's tree agrees with today's
        config; it would stay green if absorption were deleted and the globs
        widened to compensate, which is the hand-maintained boundary the gap
        describes. So the rule is exercised where it can be made to fail: a
        synthetic harness whose guidance names `scripts/ci/tool.py` while
        `element_globs` covers only the skills tree.

        Both directions are pinned. Naming the file must enrol it *and* make it
        an orphan once the naming goes away -- an absorption that only ever adds
        would let the harness quietly shed a tool by deleting one backtick.
        """
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            (root / ".agents/skills/act-as-mohab").mkdir(parents=True)
            (root / "scripts/ci").mkdir(parents=True)
            entrypoint = root / ".agents/skills/act-as-mohab/SKILL.md"
            entrypoint.write_text(
                "# Entry\n\nRun `scripts/ci/tool.py` to check.\n", encoding="utf-8"
            )
            (root / "scripts/ci/tool.py").write_text("x\n", encoding="utf-8")
            (root / "scripts/ci/unnamed.py").write_text("x\n", encoding="utf-8")
            (root / "scripts/ci/agent_guidance_budget.json").write_text(
                json.dumps(
                    {
                        "harness_reachability": {
                            "entrypoint": ".agents/skills/act-as-mohab/SKILL.md",
                            "deployable_root": ".agents/skills",
                            # Matches nothing, and that is the point: it puts
                            # `scripts` among the harness roots without making
                            # any file an element. Absorption only considers
                            # tokens whose first segment is already a root, so
                            # a directory with no glob at all cannot be
                            # absorbed however loudly the guidance names it --
                            # the residual half of gap 1, and why `.memory`
                            # needed a glob rather than only a mention.
                            "element_globs": [
                                ".agents/skills/**",
                                "scripts/ci/absent_*.py",
                            ],
                            "exemptions": [],
                        }
                    }
                ),
                encoding="utf-8",
            )
            for command in (["git", "init", "-q"], ["git", "add", "-A"]):
                subprocess.run(  # nosec B603 B607 - fixed local git commands.
                    command, cwd=root, capture_output=True, text=True, check=True
                )

            report = harness_report(root)
            self.assertIn(
                "scripts/ci/tool.py",
                report["elements"],
                "a named instrument must join the element set without a glob",
            )
            self.assertEqual(
                report["absorbed_by_name"],
                ["scripts/ci/tool.py (named in .agents/skills/act-as-mohab/SKILL.md)"],
            )
            self.assertEqual(report["orphans"], [])
            self.assertNotIn(
                "scripts/ci/unnamed.py",
                report["elements"],
                "absorption must follow the guidance, not swallow the directory",
            )

            # The naming removed: the instrument leaves the set rather than
            # silently staying in it, and the stale-name scan stays quiet
            # because nothing names it any more.
            entrypoint.write_text("# Entry\n\nNothing to run.\n", encoding="utf-8")
            subprocess.run(  # nosec B603 B607 - fixed local git command.
                ["git", "add", "-A"], cwd=root, capture_output=True, text=True, check=True
            )
            after = harness_report(root)
            self.assertNotIn("scripts/ci/tool.py", after["elements"])
            self.assertEqual(after["absorbed_by_name"], [])

    def test_every_exemption_states_a_reason_and_matches_a_live_element(self):
        """An exemption with no reason is an orphan wearing a hat (#4489)."""
        self.assertEqual(harness_report(ROOT)["exemption_problems"], [])

    def test_the_exemption_list_is_empty_so_the_headline_figure_is_not_bought(self):
        """The owner asked for 100%, and an exemption list can manufacture it.

        Every element is reachable today, so the list is empty and the figure
        is earned rather than declared. This is not a rule against ever
        exempting anything -- it is a tripwire, so that adding the first
        exemption is a visible decision in a diff rather than a quiet way to
        make the number look right. Whoever adds one deletes this test and
        says why in the PR.
        """
        self.assertEqual(load_config(ROOT)["exemptions"], [])
        report = harness_report(ROOT)
        self.assertEqual(len(report["orphans"]), 0)
        self.assertIn("tools/repository-map/graphify_maintenance.py", report["elements"])
        self.assertEqual(len(report["elements"]), EXPECTED_ELEMENT_COUNT)
        self.assertEqual(report["wildcard_only"], [])

    def test_the_deployable_root_names_a_real_subtree_reached_from_the_adapter(self):
        """One JSON string decides whether "links, not mentions" means anything.

        `deployable_root` selects which elements must be reached by a real
        markdown link. Nothing pinned its value, so setting it to
        `.agents/skillsX` -- a directory that does not exist -- moved every
        in-tree file into the mention-satisfied branch, and a new reference
        that `SKILL.md` merely name-dropped in backticks reported clean. An
        author who hits the unlinked-file failure could repair it by editing
        one string instead of adding one link, and the diff would look like
        configuration.

        Pinned three ways, because each catches a different edit: it exists,
        it is a directory, and the repository adapter reaches a real skill in
        it. A typo fails the first, a file path fails the second, and a
        real-but-wrong subtree such as `.claude` fails the third.
        """
        config = load_config(ROOT)
        deployable = ROOT / config["deployable_root"]
        self.assertTrue(deployable.exists(), config["deployable_root"])
        self.assertTrue(deployable.is_dir(), config["deployable_root"])
        reached, broken = link_walk(ROOT, config["entrypoint"])
        self.assertEqual(broken, [])
        portable_entrypoint = "chaos-engine/skills/chaos-engine/SKILL.md"
        self.assertIn(portable_entrypoint, reached)
        self.assertTrue(
            portable_entrypoint.startswith(config["deployable_root"].rstrip("/") + "/"),
            "the repository adapter must reach an entrypoint inside the deployable root",
        )

    def test_every_harness_test_module_is_run_by_the_pull_request_gate(self):
        """An enforcement module CI never runs is a rule nobody enforces.

        `tests/scripts/test_shaft_skill_cli_examples.py` was exactly that --
        an element of the harness, in no workflow's run list. Every test
        module in the element set is now checked against the gate's own
        `python -m unittest` invocation.

        Known limit, and it is why the parity matrix carries a
        `harness_reachability` row as well: this pin lives inside a module
        that the same run list decides whether to run, so dropping *this*
        module silences its own check. That row routes the same question
        through `validate_agent_setup.py`, which PR Gate runs as a separate
        step and which agents run by hand, and `parity_check_errors` there
        asserts the named test is real and that PR Gate runs it. The circle
        is broken outside the unittest layer or not at all.
        """
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        modules = [
            element
            for element in harness_report(ROOT)["elements"]
            if element.startswith("tests/scripts/")
        ]
        self.assertGreaterEqual(len(modules), 10, "the element set lost the test modules")
        missing = [
            module
            for module in modules
            if f"tests.scripts.{Path(module).stem}\n" not in workflow
        ]
        self.assertEqual(missing, [], "harness test modules PR Gate never runs")

    def test_history_backed_review_advisories_are_reachable_from_pr_gate(self):
        """#4567 items 1, 5 and 8 need history; an unwired check is inert."""
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        commit_guard = workflow.split("pr-body-autoclose-guard:", 1)[1].split(
            "dependency-review:", 1
        )[0]
        guidance_paths = workflow.split("agent_guidance:", 1)[1].split("infra:", 1)[0]
        guidance_gate = workflow.split("agent-guidance:", 1)[1].split("installer-verify:", 1)[0]
        self.assertIn("fetch-depth: 0", commit_guard)
        self.assertIn("fetch-depth: 0", guidance_gate)
        self.assertIn("--docstring-siblings", guidance_gate)
        self.assertIn("scripts/agents/guard.py", guidance_gate)
        self.assertIn("tests.scripts.test_validate_red_before_green", guidance_gate)
        self.assertIn("validate_red_before_green.py", guidance_gate)
        self.assertIn(":(glob)tests/scripts/test_guard*.py", guidance_gate)
        self.assertIn("--diff-filter=AMR", guidance_gate)
        self.assertIn("scripts/ci/validate_red_before_green.py", guidance_paths)
        self.assertIn("tests/scripts/test_validate_red_before_green.py", guidance_paths)
        self.assertIn("- 'tests/scripts/test_guard*.py'", guidance_paths)
        self.assertIn("history_file=$(mktemp)", guidance_gate)
        self.assertIn("trap 'rm -f \"$history_file\" \"$paths_file\"' EXIT", guidance_gate)
        self.assertIn("git cat-file -e \"$BASE_SHA:scripts/ci/validate_red_before_green.py\"", guidance_gate)
        self.assertIn("IFS= read -r BASE_SHA < \"$paths_file\"", guidance_gate)
        self.assertIn("git rev-parse \"$bootstrap^\" > \"$paths_file\"", guidance_gate)

    def test_the_element_set_is_derived_from_the_repository_not_hand_listed(self):
        """A hand list omits the next file, which is the defect being fixed.

        Proving the set is derived needs more than reading the config: the
        globs are applied to `git ls-files` output, so a file matching a glob
        is an element without anyone editing anything. The entrypoint itself
        must be an element, or the walk would start outside the set it judges.
        """
        report = harness_report(ROOT)
        config = load_config(ROOT)
        self.assertIn(config["entrypoint"], report["elements"])
        self.assertGreater(len(report["elements"]), 60)
        for element in report["elements"]:
            self.assertIn(element, tracked_files(ROOT))

    def test_the_walk_reports_an_unlinked_file_a_broken_link_and_a_reasonless_exemption(self):
        """Mutation coverage for the walk itself, against a synthetic tree.

        The four live assertions above pass vacuously once the harness is
        wired, so nothing in them proves the walk still detects anything --
        the unbound-check gap in `references/verification-gap-lens.md`. This
        builds a miniature harness in a temporary git repository and mutates
        it four ways, one per mutation #4489 named, calling the same
        `harness_report` the live checks call rather than a private copy.
        """
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            skills = root / ".agents/skills/act-as-mohab/references"
            skills.mkdir(parents=True)
            (root / "scripts/ci").mkdir(parents=True)
            (root / "tests/scripts").mkdir(parents=True)
            entrypoint = root / ".agents/skills/act-as-mohab/SKILL.md"
            entrypoint.write_text(
                "# Entry\n\n[roles](references/roles.md)\n\n"
                "Run `scripts/ci/tool.py` to watch.\n",
                encoding="utf-8",
            )
            (skills / "roles.md").write_text("# Roles\n", encoding="utf-8")
            (root / "scripts/ci/tool.py").write_text("x\n", encoding="utf-8")
            (root / "tests/scripts/test_agent_thing.py").write_text("x\n", encoding="utf-8")

            def write_budget(exemption_reason: object = "CI reaches these, not an agent.") -> None:
                exemption = {"path": "tests/scripts/test_agent_*.py"}
                if exemption_reason is not None:
                    exemption["reason"] = exemption_reason
                (root / "scripts/ci/agent_guidance_budget.json").write_text(
                    json.dumps(
                        {
                            "harness_reachability": {
                                "entrypoint": ".agents/skills/act-as-mohab/SKILL.md",
                                "deployable_root": ".agents/skills",
                                "element_globs": [
                                    ".agents/skills/**",
                                    "scripts/ci/*.py",
                                    "tests/scripts/test_agent_*.py",
                                ],
                                "exemptions": [exemption],
                            }
                        }
                    ),
                    encoding="utf-8",
                )

            write_budget()
            for command in (
                ["git", "init", "-q"],
                ["git", "add", "-A"],
            ):
                subprocess.run(  # nosec B603 B607 - fixed local git commands.
                    command, cwd=root, capture_output=True, text=True, check=True
                )
            baseline = harness_report(root)
            self.assertEqual(baseline["orphans"], [], "the fixture must start clean")
            self.assertEqual(baseline["broken_links"], [])
            self.assertEqual(baseline["exemption_problems"], [])

            # Mutation 1 -- a new reference nothing links to.
            (skills / "stray.md").write_text("# Stray\n", encoding="utf-8")
            subprocess.run(  # nosec B603 B607 - fixed local git command.
                ["git", "add", "-A"], cwd=root, capture_output=True, text=True, check=True
            )
            self.assertEqual(
                harness_report(root)["orphans"],
                [".agents/skills/act-as-mohab/references/stray.md (no markdown link reaches it)"],
            )
            (skills / "stray.md").unlink()

            # Mutation 2 -- a link deleted from the entrypoint.
            entrypoint.write_text("# Entry\n\nRun `scripts/ci/tool.py`.\n", encoding="utf-8")
            subprocess.run(  # nosec B603 B607 - fixed local git command.
                ["git", "add", "-A"], cwd=root, capture_output=True, text=True, check=True
            )
            self.assertEqual(
                harness_report(root)["orphans"],
                [".agents/skills/act-as-mohab/references/roles.md (no markdown link reaches it)"],
            )

            # Mutation 3 -- a link pointed at a path that does not exist.
            entrypoint.write_text(
                "# Entry\n\n[roles](references/roles.md)\n[gone](references/gone.md)\n\n"
                "Run `scripts/ci/tool.py`.\n",
                encoding="utf-8",
            )
            subprocess.run(  # nosec B603 B607 - fixed local git command.
                ["git", "add", "-A"], cwd=root, capture_output=True, text=True, check=True
            )
            mutated = harness_report(root)
            self.assertEqual(
                mutated["broken_links"],
                [".agents/skills/act-as-mohab/SKILL.md -> references/gone.md"],
            )
            self.assertEqual(mutated["orphans"], [])

            # Mutation 4 -- the reason removed from an exemption entry.
            write_budget(exemption_reason=None)
            subprocess.run(  # nosec B603 B607 - fixed local git command.
                ["git", "add", "-A"], cwd=root, capture_output=True, text=True, check=True
            )
            reasonless = harness_report(root)
            self.assertEqual(
                reasonless["exemption_problems"],
                ["exemptions[0] tests/scripts/test_agent_*.py states no reason"],
            )
            self.assertIn(
                "tests/scripts/test_agent_thing.py (no reachable file names it)",
                reasonless["orphans"],
                "a reasonless exemption must stop exempting, not just be reported",
            )

    def test_a_backtick_mention_does_not_make_an_in_tree_file_reachable(self):
        """#4489's "links, not mentions", pinned where links are actually possible.

        Inside the deployable tree a link always works, so a mention there is
        the shape being rejected: an agent cannot follow it, and the file's
        own links stay unwalked, so a mention would also hide everything
        downstream of it. Outside the tree a link is forbidden by the
        deployment contract, which is why the mention mechanism exists at all
        -- and only there.
        """
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            references = root / ".agents/skills/act-as-mohab/references"
            references.mkdir(parents=True)
            (root / "scripts/ci").mkdir(parents=True)
            (root / ".agents/skills/act-as-mohab/SKILL.md").write_text(
                "# Entry\n\nSee `.agents/skills/act-as-mohab/references/roles.md` "
                "and `scripts/ci/tool.py`.\n",
                encoding="utf-8",
            )
            (references / "roles.md").write_text("# Roles\n", encoding="utf-8")
            (root / "scripts/ci/tool.py").write_text("x\n", encoding="utf-8")
            (root / "scripts/ci/agent_guidance_budget.json").write_text(
                json.dumps(
                    {
                        "harness_reachability": {
                            "entrypoint": ".agents/skills/act-as-mohab/SKILL.md",
                            "deployable_root": ".agents/skills",
                            "element_globs": [".agents/skills/**", "scripts/ci/*.py"],
                            "exemptions": [],
                        }
                    }
                ),
                encoding="utf-8",
            )
            for command in (["git", "init", "-q"], ["git", "add", "-A"]):
                subprocess.run(  # nosec B603 B607 - fixed local git commands.
                    command, cwd=root, capture_output=True, text=True, check=True
                )
            self.assertEqual(
                harness_report(root)["orphans"],
                [
                    ".agents/skills/act-as-mohab/references/roles.md "
                    "(no markdown link reaches it)"
                ],
            )

    def test_a_named_path_that_moved_is_reported_rather_than_ignored(self):
        """The #4481 fail-open shape, which is this module's own risk.

        Renaming `scripts/ci/tool.py` removes it from the element set, so no
        orphan is produced -- the check would go quiet about the very file it
        was wired to reach. The stale-name scan is what makes the rename loud.
        """
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            (root / ".agents/skills/act-as-mohab").mkdir(parents=True)
            (root / "scripts/ci").mkdir(parents=True)
            (root / ".agents/skills/act-as-mohab/SKILL.md").write_text(
                "# Entry\n\nRun `scripts/ci/tool.py`.\n", encoding="utf-8"
            )
            (root / "scripts/ci/renamed_tool.py").write_text("x\n", encoding="utf-8")
            (root / "scripts/ci/agent_guidance_budget.json").write_text(
                json.dumps(
                    {
                        "harness_reachability": {
                            "entrypoint": ".agents/skills/act-as-mohab/SKILL.md",
                            "deployable_root": ".agents/skills",
                            "element_globs": [".agents/skills/**", "scripts/ci/tool.py"],
                            "exemptions": [],
                        }
                    }
                ),
                encoding="utf-8",
            )
            for command in (["git", "init", "-q"], ["git", "add", "-A"]):
                subprocess.run(  # nosec B603 B607 - fixed local git commands.
                    command, cwd=root, capture_output=True, text=True, check=True
                )
            report = harness_report(root)
            self.assertEqual(report["orphans"], [])
            self.assertEqual(
                report["stale_named_paths"],
                ["scripts/ci/tool.py (named in .agents/skills/act-as-mohab/SKILL.md) "
                 "matches no tracked path"],
            )

    def test_the_walk_places_link_targets_when_the_root_is_spelled_unresolved(self):
        """A checkout whose spelling differs from its resolved form must still work.

        `link_walk` resolved every link target and then compared it against an
        *unresolved* `root`. When the two spellings differ, `relative_to` raises
        and the target is filed as "escapes the repository" -- a link pointing
        squarely inside the tree, reported as leaving it, and its file reported
        as an orphan. It fails closed and loudly rather than silently, so
        nothing was hidden; what it produced was a confident wrong diagnosis.

        It survived review because the divergence needs an environment nobody
        local had. On the Windows CI runner `TEMP` is itself spelled in 8.3
        form (`C:\\Users\\RUNNER~1\\AppData\\Local\\Temp`), so `mkdtemp()`
        inherits that prefix and `resolve()` expands it. On a developer machine
        `TEMP` holds the long spelling, `mkdtemp()` returns a long path, and
        `resolve()` is a no-op -- the 8.3 alias exists in both places, and only
        the spelling handed to the test differs. That distinction matters: the
        trigger is how the root is *spelled*, not whether the volume generates
        short names, which is why disabling 8.3 generation would not have saved
        it and why a symlinked checkout, a junction and a `subst` drive break
        it identically. Verified both ways against a real
        `GetShortPathNameW` root: before the fix the walk called
        `references/roles.md` an escape, after it the report is empty.

        The divergence is injected lexically -- `<temp>/repo/../repo` names the
        same directory as `<temp>/repo` without spelling it the same way -- so
        this runs identically on every platform and needs no privilege, no
        symlink support, and no 8.3 names. `assertNotEqual` guards it: if a
        future platform normalised the detour away, the case would prove
        nothing, and it must fail rather than pass vacuously.
        """
        with tempfile.TemporaryDirectory() as temporary_directory:
            real = Path(temporary_directory) / "repo"
            references = real / ".agents/skills/act-as-mohab/references"
            references.mkdir(parents=True)
            (real / "scripts/ci").mkdir(parents=True)
            (real / ".agents/skills/act-as-mohab/SKILL.md").write_text(
                "# Entry\n\n[roles](references/roles.md)\n\nRun `scripts/ci/tool.py`.\n",
                encoding="utf-8",
            )
            (references / "roles.md").write_text("# Roles\n", encoding="utf-8")
            (real / "scripts/ci/tool.py").write_text("x\n", encoding="utf-8")
            (real / "scripts/ci/agent_guidance_budget.json").write_text(
                json.dumps(
                    {
                        "harness_reachability": {
                            "entrypoint": ".agents/skills/act-as-mohab/SKILL.md",
                            "deployable_root": ".agents/skills",
                            "element_globs": [".agents/skills/**", "scripts/ci/*.py"],
                            "exemptions": [],
                        }
                    }
                ),
                encoding="utf-8",
            )
            for command in (["git", "init", "-q"], ["git", "add", "-A"]):
                subprocess.run(  # nosec B603 B607 - fixed local git commands.
                    command, cwd=real, capture_output=True, text=True, check=True
                )

            detoured = real.parent / "repo" / ".." / "repo"
            self.assertNotEqual(
                detoured,
                detoured.resolve(),
                "the fixture must actually spell the root differently from its resolved form",
            )
            self.assertEqual(detoured.resolve(), real.resolve())

            report = harness_report(detoured)
            self.assertEqual(
                report["broken_links"],
                [],
                "a link inside the tree must not be reported as escaping it",
            )
            self.assertEqual(report["orphans"], [])

    def test_the_path_token_scan_ignores_tokens_that_name_no_harness_root(self):
        """A false positive here reddens an honest PR, and the cheap repair is to gut the scan.

        Guidance is full of path-shaped code spans that are not repository
        paths -- a git ref, a branch glob, a report artifact, a URL host. None
        of them starts with a harness root, which is why no ignore list is
        needed and why one must not creep in.
        """
        roots = {"AGENTS.md", ".agents", "scripts"}
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            (root / ".agents").mkdir()
            page = root / ".agents/page.md"
            page.write_text(
                "Branch from `origin/main` onto `ChaosEngine/*`, read\n"
                "`surefire-reports/TEST-*.xml`, and see `shafthq.github.io`.\n"
                "Then read `AGENTS.md` and run:\n\n"
                "```bash\npy -3 scripts/ci/thing.py --check\n```\n",
                encoding="utf-8",
            )
            found = path_tokens(root, {".agents/page.md"}, roots)
        self.assertEqual(sorted(found), ["AGENTS.md", "scripts/ci/thing.py"])

    def test_the_glob_matcher_stops_a_star_at_a_path_separator(self):
        """`fnmatch` would let one glob swallow a whole subtree.

        `.claude/skills/*` must not match a file two levels down, or an
        element nobody meant to cover is silently counted as covered -- and
        the same widening would make an exemption far broader than its reason.
        """
        self.assertTrue(glob_regex(".claude/skills/*").match(".claude/skills/a"))
        self.assertFalse(glob_regex(".claude/skills/*").match(".claude/skills/a/SKILL.md"))
        self.assertTrue(glob_regex(".agents/skills/**").match(".agents/skills/a/b/c.md"))
        self.assertTrue(glob_regex("tests/scripts/test_agent_*.py").match(
            "tests/scripts/test_agent_router_contract.py"
        ))
        self.assertFalse(glob_regex("tests/scripts/test_agent_*.py").match(
            "tests/scripts/test_guard_lifecycle.py"
        ))


class EntrypointDutyTest(unittest.TestCase):
    """The content half of #4485: duties the entrypoint must state, unqualified."""

    ENTRYPOINT = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
    PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
    PLANNING_PLAYBOOK = ROOT / "chaos-engine/references/work-github-planning.md"

    def test_the_github_playbook_links_its_planning_and_tracking_half(self):
        """#4504: split before a future edit turns the file cap into an emergency."""
        content = self.PLAYBOOK.read_text(encoding="utf-8")
        self.assertTrue(self.PLANNING_PLAYBOOK.is_file())
        self.assertIn("[planning and tracking](work-github-planning.md)", content)
        planning = self.PLANNING_PLAYBOOK.read_text(encoding="utf-8")
        for heading in (
            "## 0. Ground the scope before asking anything",
            "## 1. Ask once, at the start, then go unattended",
            "## 2. Branch and track",
            "## 3. Work items in dependency order, front-loading risk",
            "## 3b. Tracking issue + one-issue-per-subtask (mandatory default for new work)",
        ):
            with self.subTest(heading=heading):
                self.assertIn(heading, planning)
        for clause in (
            "current status page",
            "in the same session",
            "final summary comment",
            "Closes #N",
        ):
            with self.subTest(planning_clause=clause):
                self.assertIn(clause, planning)
        for clause in (
            "Before committing any subagent's work",
            "unlink it from the PR's Development sidebar",
            "Before reviewing or shipping any nontrivial diff",
            "deferred/out-of-scope/adjacent-finding/follow-up",
            "description that lists each sub-item and its commit",
        ):
            with self.subTest(delivery_clause=clause):
                self.assertIn(clause, content)
        for clause in (
            "separate branch+PR per item looped sequentially",
            "rename it to `Tracking: `",
            "say what you searched",
            "two options",
            "skill updates",
            "Update task status as work proceeds",
        ):
            with self.subTest(planning_clause=clause):
                self.assertIn(clause, planning)

    def test_the_orchestrator_owns_a_pr_from_armed_to_confirmed_merged(self):
        """#4486: opening a PR is where the harness used to stop caring.

        Every clause here is one the issue named, and each is pinned
        separately so a partial deletion cannot hide behind the survivors.
        The four terminal shapes are pinned individually because `stale` is
        the one that produces no event at all and therefore the one a manual
        watch drops.
        """
        section = section_body(
            self.ENTRYPOINT.read_text(encoding="utf-8"), "## Ownership and completion"
        )
        self.assertTrue(section, "the entrypoint must keep the Ownership and completion section")
        compact = re.sub(r"\s+", " ", section)
        for clause in (
            "auto-merge",
            "arm",
            "watch",
            "`scripts/ci/watch_pr_checks.py`",
            "merged",
            "red",
            "conflicting",
            "stale",
            "fix",
            "confirm",
            "compaction",
        ):
            with self.subTest(clause=clause):
                self.assertIn(clause, compact.lower() if clause.islower() else compact)

    def test_the_pr_duty_cannot_be_qualified_into_something_optional(self):
        """A presence pin catches deletion and renaming, never weakening (#4469).

        "Arm auto-merge where practical" leaves every word above in place. So
        the section that carries the duty is also checked for the hedges that
        turn a duty into a suggestion.
        """
        section = section_body(
            self.ENTRYPOINT.read_text(encoding="utf-8"), "## Ownership and completion"
        )
        hedge = OPTIONALITY_HEDGE.search(section)
        self.assertIsNone(
            hedge,
            f"the PR duty is qualified by {hedge.group(0)!r} and is no longer a duty"
            if hedge
            else "",
        )

    def test_both_workflows_are_named_and_linked_to_a_section_that_exists(self):
        """#4487: a link, and an anchor that dies when the heading is renamed.

        #4452 recorded pins that stayed green after the guarded section was
        renamed. Resolving the anchor against the target file's real headings
        is what closes that: renaming the section in the playbook fails here,
        naming which anchor broke.
        """
        content = self.ENTRYPOINT.read_text(encoding="utf-8")
        slugs = heading_slugs(self.PLAYBOOK)
        for workflow, anchor in (
            ("learned-lessons workflow", "learned-lessons-workflow"),
            ("PR-merger workflow", "pr-merger-workflow-arm-watch-fix-confirm"),
        ):
            with self.subTest(workflow=workflow):
                self.assertIn(workflow, content, f"the entrypoint must name the {workflow}")
                link = f"../../references/work-github-playbook.md#{anchor}"
                self.assertIn(link, content, f"the entrypoint must link {link}")
                self.assertIn(anchor, slugs, f"{anchor} names no heading in the playbook")

    def test_the_learning_loop_is_required_before_every_report_of_done(self):
        """#4487: the routing table is the classifier; running it is the duty."""
        section = section_body(
            self.ENTRYPOINT.read_text(encoding="utf-8"), "## Learning loop"
        )
        self.assertTrue(section, "the entrypoint must keep the Learning loop section")
        compact = re.sub(r"\s+", " ", section)
        self.assertIn("Before reporting done", compact)
        self.assertIn("Nothing durable is a valid result", compact)
        self.assertIsNone(OPTIONALITY_HEDGE.search(section))

    def test_the_hedge_pattern_detects_the_weakenings_it_claims_to(self):
        """The two live hedge assertions pass vacuously on a clean file.

        Gutting `OPTIONALITY_HEDGE` would leave them green, so each phrasing
        is exercised against the shipped pattern rather than a copy, and one
        benign sentence guards the other direction.
        """
        for weakened in (
            "Arm auto-merge where practical.",
            "Watch every PR when feasible.",
            "Arm auto-merge for significant PRs.",
            "Consider arming auto-merge after review.",
            "Arming is optional once CI is green.",
            "Watching is best-effort.",
            "Confirm the merge if you can.",
        ):
            with self.subTest(text=weakened):
                self.assertIsNotNone(OPTIONALITY_HEDGE.search(weakened))
        self.assertIsNone(
            OPTIONALITY_HEDGE.search(
                "Arm auto-merge once the review gate passes, then watch until merged."
            )
        )


if __name__ == "__main__":
    unittest.main()
