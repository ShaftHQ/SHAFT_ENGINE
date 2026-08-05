"""The harness reachability walk, shared by the pull request gate and the local one.

Extracted from `tests/scripts/test_agent_harness_reachability.py` for #4531
gap 2. The walk lived inside the test module, so
`scripts/ci/validate_agent_setup.py` -- the check `AGENTS.md` names, and the
one an agent on a machine with no CI actually runs -- could not call it
without a CI script importing a test. It therefore exited 0 on a fully
orphaned harness, and only a GitHub Actions job ever reported orphans. A
guarantee enforced only by CI is not a guarantee for a machine that never
opens a pull request, which is precisely what the owner requirement about
*any machine* rules out.

Only the walk moved. Every assertion, every pinned count and every mutation
fixture stays in the test module, which now imports these names. Duplicating
the walk was rejected on this repository's own evidence: #4481 recorded four
of five copies of a references glob failing open after the path moved, and a
second copy of a reachability check is the same bet.
"""

from __future__ import annotations

import json
import re
import subprocess  # nosec B404 - the walk runs one trusted, read-only git query.
from pathlib import Path


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

    `--cached` alone is wrong in the other direction too, and that half was
    missed on the first pass: after an unstaged rename the index still lists
    the old path, `--others` lists the new one, and both read as present, so a
    named instrument that moved reported clean until someone staged it. The
    same "one `git add` early" shape, mirrored. Existence on disk is therefore
    the final filter -- present and not ignored is the honest definition, and
    it is the same definition in both directions.
    """
    listed = subprocess.run(  # nosec B603 B607 - fixed read-only git command.
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    )
    return sorted(
        {
            path
            for path in listed.stdout.split("\0")
            if path and (root / path).exists()
        }
    )


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

    `root` is resolved here, at the one boundary where it enters the walk,
    because each link target is resolved below and `relative_to` compares
    spellings rather than locations. Comparing a resolved target against an
    unresolved root made every link inside a detoured checkout -- an 8.3 short
    name, a symlink, a junction, a `subst` drive -- read as escaping the
    repository. Normalise both sides once rather than per comparison.
    """
    root = root.resolve()
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
                # Recorded, never swallowed. A target that cannot be placed
                # inside the repository is a defect either way, so this arm
                # fails closed: `continue` drops it from the walk *and* the
                # report names it. With both sides now resolved, reaching here
                # means a genuine escape rather than a spelling mismatch.
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
    """Return every reachability defect in the tree at `root`, each as one string.

    Resolved once here so every helper below sees one spelling of the root.
    `link_walk` resolves defensively too, for a direct caller.
    """
    root = root.resolve()
    config = load_config(root)
    entrypoint = config["entrypoint"]
    deployable = config["deployable_root"].rstrip("/") + "/"
    tracked = tracked_files(root)
    element_globs = [glob_regex(pattern) for pattern in config["element_globs"]]
    roots = {pattern.split("/", 1)[0] for pattern in config["element_globs"]}

    reached, broken = link_walk(root, entrypoint)
    tokens = path_tokens(root, reached, roots)
    token_matchers = {token: glob_regex(token) for token in tokens}

    # The boundary derives from the guidance, not only from the config beside
    # it -- #4531 gap 1. `element_globs` alone decides what may be reported as
    # an orphan, so a harness surface under an uncovered path is *invisible*
    # rather than unreachable, and the count simply does not move. This module's
    # own docstring calls that the failure mode most likely to bite next.
    #
    # Naming is the case the repository can settle by itself. A reachable
    # guidance file that tells an agent to run an instrument has already
    # declared that instrument part of the harness, so the element set absorbs
    # it and no one has to remember to widen a glob. Measured before it was
    # written: thirteen instruments named across five playbooks -- among them
    # `scripts/ci/validate_shaft_mcp_transports.py` and
    # `scripts/ci/github-auth-env.sh` -- sat outside every glob.
    #
    # Absorbing rather than widening by directory is deliberate. Adding
    # `scripts/ci/*.py`, `tests/scripts/*.py` and `.github/workflows/*.yml`
    # instead was measured at 177 elements and 64 fresh orphans, nearly all of
    # them release and product CI that no agent guidance names. A boundary that
    # forces 64 exemptions to describe a 13-file gap teaches everyone that
    # exemptions are paperwork, which is how an exemption list stops meaning
    # anything.
    #
    # Wildcards are excluded for the same reason they buy no reachability: a
    # wildcard re-derives itself from the tree it is meant to be checking, so
    # letting one enrol elements would let a single `scripts/**` in prose annex
    # the repository.
    named_elements = {
        token
        for token in tokens
        if "*" not in token and token in set(tracked) and (root / token).is_file()
    }
    elements = [
        path
        for path in tracked
        if any(matcher.match(path) for matcher in element_globs) or path in named_elements
    ]

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

    # Which elements the config would have missed. Reported rather than
    # asserted on: it is the size of the gap the absorption rule closes, and it
    # legitimately moves whenever a playbook starts or stops naming a tool.
    absorbed_by_name = sorted(
        f"{path} (named in {tokens[path]})"
        for path in named_elements
        if not any(matcher.match(path) for matcher in element_globs)
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

    # A wildcard token cannot fail, so it cannot satisfy reachability. It
    # matches whatever exists, which means a new role adapter, a new harness
    # test module or a staged rename inside a directory it covers all stayed
    # green -- the token re-derives itself from the tree it is supposed to be
    # checking. 24 of the elements were held up this way. Exact tokens keep
    # their power because they name one path and die when it moves; wildcards
    # stay legal as prose and are still stale-checked above, they just buy
    # nothing here.
    exact_tokens = {token for token in tokens if "*" not in token}

    orphans: list[str] = []
    by_link: list[str] = []
    by_exact_token: list[str] = []
    wildcard_only: list[str] = []
    for element in elements:
        if any(matcher.match(element) for matcher in exemption_matchers):
            continue
        if element in reached:
            by_link.append(element)
            continue
        if element.startswith(deployable):
            orphans.append(f"{element} (no markdown link reaches it)")
            continue
        if element in exact_tokens:
            by_exact_token.append(element)
            continue
        if any(token_matchers[token].match(element) for token in tokens):
            wildcard_only.append(element)
            orphans.append(f"{element} (only a wildcard token names it)")
            continue
        orphans.append(f"{element} (no reachable file names it)")

    return {
        "orphans": sorted(orphans),
        "broken_links": sorted(broken),
        "stale_named_paths": stale,
        "absorbed_by_name": absorbed_by_name,
        "exemption_problems": sorted(exemption_problems),
        "reached": sorted(reached),
        "elements": elements,
        "by_link": sorted(by_link),
        "by_exact_token": sorted(by_exact_token),
        "wildcard_only": sorted(wildcard_only),
    }
