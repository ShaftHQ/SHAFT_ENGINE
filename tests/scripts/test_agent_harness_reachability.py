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
   globs over `git ls-files`, never a hand-written list, so a file added
   tomorrow is an element tomorrow -- a hand list silently omits the next file,
   which is exactly how the 38 accumulated.
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
  is the failure mode most likely to bite next.
- **Not run by `validate_agent_setup.py`.** This is a unittest module, run by
  PR Gate's agent-harness job. `py -3 scripts/ci/validate_agent_setup.py
  --skip-external` stays green with the harness fully orphaned.
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


ROOT = Path(__file__).resolve().parents[2]
BUDGET = ROOT / "scripts/ci/agent_guidance_budget.json"

# Inline `spans` and fenced blocks both carry instrument paths in this
# repository's house style -- README.md writes the validate command in a bash
# fence and the adapter set in inline spans -- so both are scanned. Prose is
# not: an unfenced path is a mention, and #4489 rejected mentions.
INLINE_SPAN = re.compile(r"`([^`\n]+)`")
FENCED_BLOCK = re.compile(r"(?ms)^```[^\n]*\n(.*?)^```")
MARKDOWN_LINK = re.compile(r"(?<!!)\[[^]]*\]\(([^)]+)\)")
URI_SCHEME = re.compile(r"^[a-z][a-z0-9+.-]*:", re.IGNORECASE)
# A token earns a look only if it ends in a path-ish character. Without the
# right anchor `AGENTS.md.` at the end of a sentence and `roles.md),` inside
# prose both arrive as unresolvable paths and redden an honest file.
PATH_TOKEN = re.compile(r"^[.A-Za-z0-9_][A-Za-z0-9_.*/-]*[A-Za-z0-9_*]$")
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


def glob_regex(pattern: str) -> re.Pattern[str]:
    """Compile one POSIX-style path glob, with `*` stopping at a separator.

    `fnmatch` is not usable here: its `*` crosses `/`, so `.claude/skills/*`
    would swallow `.claude/skills/act-as-mohab/SKILL.md` and an element two
    levels deeper than intended would count as covered.
    """
    parts: list[str] = []
    index = 0
    while index < len(pattern):
        if pattern.startswith("**/", index):
            parts.append("(?:[^/]+/)*")
            index += 3
        elif pattern.startswith("**", index):
            parts.append(".*")
            index += 2
        elif pattern[index] == "*":
            parts.append("[^/]*")
            index += 1
        elif pattern[index] == "?":
            parts.append("[^/]")
            index += 1
        else:
            parts.append(re.escape(pattern[index]))
            index += 1
    return re.compile("".join(parts) + r"\Z")


def tracked_files(root: Path) -> list[str]:
    """Return every committed and every untracked-but-unignored path.

    `--others --exclude-standard` is not decoration. With `git ls-files` alone
    the first mutation #4489 names -- add a file under `references/` that
    nothing links to -- passed green, because an unstaged file is not tracked
    and so was not an element. That is the whole failure being fixed, arriving
    one `git add` early: an author who runs the suite before staging is told
    the harness is complete. A file that is present and not ignored is part of
    the harness whether or not the index has caught up.
    """
    listed = subprocess.run(  # nosec B603 B607 - fixed read-only git command.
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    )
    return sorted({path for path in listed.stdout.split("\0") if path})


def load_config(root: Path) -> dict:
    """Read the shipped reachability contract."""
    budget = json.loads(
        (root / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
    )
    return budget["harness_reachability"]


def link_walk(root: Path, entrypoint: str) -> tuple[set[str], list[str]]:
    """Walk markdown links from the entrypoint, returning reached paths and broken links.

    Recursion stops at any non-markdown target: a linked `.LICENSE` or
    `.yaml` is reached, but has no links of its own to follow.
    """
    reached: set[str] = set()
    broken: list[str] = []
    queue = [entrypoint]
    while queue:
        current = queue.pop()
        if current in reached:
            continue
        reached.add(current)
        path = root / current
        if path.suffix != ".md" or not path.is_file():
            continue
        for raw in MARKDOWN_LINK.findall(path.read_text(encoding="utf-8")):
            target = raw.strip().strip("<>").split("#", 1)[0]
            if not target or URI_SCHEME.match(target):
                continue
            resolved = (path.parent / target).resolve()
            try:
                relative = resolved.relative_to(root).as_posix()
            except ValueError:
                broken.append(f"{current} -> {raw} (escapes the repository)")
                continue
            if not resolved.exists():
                broken.append(f"{current} -> {raw}")
                continue
            queue.append(relative)
    return reached, broken


def path_tokens(root: Path, reached: set[str], roots: set[str]) -> dict[str, str]:
    """Map each harness path token found in a reachable file to the file naming it.

    A token counts only inside an inline code span or a fenced block, and only
    when its first segment names a harness root -- which keeps `origin/main`,
    `ChaosEngine/*` and `surefire-reports/TEST-*.xml` out without an ignore
    list, because none of them is a harness root.
    """
    found: dict[str, str] = {}
    for relative in sorted(reached):
        path = root / relative
        if path.suffix != ".md" or not path.is_file():
            continue
        content = path.read_text(encoding="utf-8")
        spans = INLINE_SPAN.findall(FENCED_BLOCK.sub("", content))
        chunks = [*spans, *FENCED_BLOCK.findall(content)]
        for token in (word for chunk in chunks for word in chunk.split()):
            if PATH_TOKEN.match(token) and token.split("/", 1)[0] in roots:
                found.setdefault(token, relative)
    return found


def harness_report(root: Path) -> dict[str, list[str]]:
    """Return every reachability defect in the tree at `root`, each as one string."""
    config = load_config(root)
    entrypoint = config["entrypoint"]
    deployable = config["deployable_root"].rstrip("/") + "/"
    tracked = tracked_files(root)
    element_globs = [glob_regex(pattern) for pattern in config["element_globs"]]
    elements = [
        path for path in tracked if any(matcher.match(path) for matcher in element_globs)
    ]
    roots = {pattern.split("/", 1)[0] for pattern in config["element_globs"]}

    reached, broken = link_walk(root, entrypoint)
    tokens = path_tokens(root, reached, roots)
    token_matchers = {token: glob_regex(token) for token in tokens}

    # Directories count as tracked paths. `.claude/skills/*` and
    # `.github/instructions/*` name real directories and no file, so matching
    # against file paths alone would report the harness's own house style as
    # stale -- and the cheapest repair for that is to delete the scan.
    nodes = set(tracked)
    for path in tracked:
        parts = path.split("/")
        for depth in range(1, len(parts)):
            nodes.add("/".join(parts[:depth]))
    stale = sorted(
        f"{token} (named in {source}) matches no tracked path"
        for token, source in tokens.items()
        if not any(token_matchers[token].match(node) for node in nodes)
    )

    exemption_problems: list[str] = []
    exemption_matchers: list[re.Pattern[str]] = []
    for index, entry in enumerate(config.get("exemptions", [])):
        label = f"exemptions[{index}]"
        if not isinstance(entry, dict):
            exemption_problems.append(f"{label} is not an object")
            continue
        pattern = entry.get("path")
        reason = entry.get("reason")
        if not isinstance(pattern, str) or not pattern.strip():
            exemption_problems.append(f"{label} has no path")
            continue
        label = f"exemptions[{index}] {pattern}"
        if not isinstance(reason, str) or not reason.strip():
            exemption_problems.append(f"{label} states no reason")
            continue
        matcher = glob_regex(pattern)
        exemption_matchers.append(matcher)
        if not any(matcher.match(candidate) for candidate in elements):
            exemption_problems.append(f"{label} matches no tracked harness element")

    orphans: list[str] = []
    for element in elements:
        if any(matcher.match(element) for matcher in exemption_matchers):
            continue
        if element.startswith(deployable):
            if element not in reached:
                orphans.append(f"{element} (no markdown link reaches it)")
            continue
        if element in reached:
            continue
        if not any(
            token == element or token_matchers[token].match(element) for token in tokens
        ):
            orphans.append(f"{element} (no reachable file names it)")

    return {
        "orphans": sorted(orphans),
        "broken_links": sorted(broken),
        "stale_named_paths": stale,
        "exemption_problems": sorted(exemption_problems),
        "reached": sorted(reached),
        "elements": elements,
    }


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
        self.assertGreaterEqual(len(report["elements"]), 84)

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

    ENTRYPOINT = ROOT / ".agents/skills/act-as-mohab/SKILL.md"
    PLAYBOOK = ROOT / ".agents/skills/act-as-mohab/references/work-github-playbook.md"

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
                link = f"references/work-github-playbook.md#{anchor}"
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
