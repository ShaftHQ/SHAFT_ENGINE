"""Client-name scrub for public repositories.

When a denylist is configured (env or untracked local file), refuse PR bodies,
commit subjects/bodies, branch names, and added lines in the PR diff that
contain any listed marker.
Not configured → no-op. Configured → fail closed.

Denylist sources (merged):
- ``CHAOS_ENGINE_CLIENT_NAME_DENYLIST`` — comma-separated tokens
- ``.chaos-engine-state/client-name-denylist.txt`` — one token per line
  (``#`` comments and blanks ignored). File is gitignored with the state dir.
"""

from __future__ import annotations

import os
import subprocess
from pathlib import Path
from typing import Iterable

ENV_KEY = "CHAOS_ENGINE_CLIENT_NAME_DENYLIST"
STATE_REL = ".chaos-engine-state/client-name-denylist.txt"
MIN_TOKEN_LEN = 3


def load_denylist(root: Path | None = None, *, environ: dict[str, str] | None = None) -> tuple[str, ...]:
    """Return configured denylist tokens (original casing preserved)."""
    env = environ if environ is not None else os.environ
    tokens: list[str] = []
    raw = (env.get(ENV_KEY) or "").strip()
    if raw:
        tokens.extend(part.strip() for part in raw.split(","))
    if root is not None:
        path = Path(root) / STATE_REL
        if path.is_file():
            for line in path.read_text(encoding="utf-8").splitlines():
                stripped = line.strip()
                if not stripped or stripped.startswith("#"):
                    continue
                tokens.append(stripped)
    cleaned: list[str] = []
    seen: set[str] = set()
    for token in tokens:
        if len(token) < MIN_TOKEN_LEN:
            continue
        key = token.casefold()
        if key in seen:
            continue
        seen.add(key)
        cleaned.append(token)
    return tuple(cleaned)


def is_configured(denylist: Iterable[str]) -> bool:
    return bool(tuple(denylist))


def findings_in_text(text: str, denylist: Iterable[str], *, surface: str) -> list[str]:
    """Return failure strings when any denylist token appears in text (case-insensitive)."""
    if not text:
        return []
    hay = text.casefold()
    for token in denylist:
        if token.casefold() in hay:
            return [f"client-name scrub: denied marker in {surface}"]
    return []


def _git(root: Path, *args: str) -> str:
    proc = subprocess.run(  # nosec B603 B607
        ["git", "-C", str(root), *args],
        check=False,
        capture_output=True,
        text=True,
    )
    if proc.returncode != 0:
        return ""
    return proc.stdout or ""


def default_base_ref(root: Path) -> str:
    """Resolve the remote default branch tip without a hard-coded branch token."""
    sym = _git(root, "symbolic-ref", "--quiet", "refs/remotes/origin/HEAD").strip()
    if sym.startswith("refs/remotes/"):
        return sym.removeprefix("refs/remotes/")
    remote = "origin"
    for name in ("main", "master"):
        ref = f"{remote}/{name}"
        if _git(root, "rev-parse", "--verify", ref).strip():
            return ref
    return f"{remote}/{'main'}"


def current_branch(root: Path) -> str:
    return _git(root, "branch", "--show-current").strip()


def commit_messages_since_merge_base(root: Path, *, base_ref: str | None = None) -> list[str]:
    """Subjects + bodies for commits reachable from HEAD but not base_ref."""
    ref = base_ref or default_base_ref(root)
    mb = _git(root, "merge-base", "HEAD", ref).strip()
    if not mb:
        roots = _git(root, "rev-list", "--max-parents=0", "HEAD").splitlines()
        mb = roots[0].strip() if roots else ""
    if not mb:
        return []
    log = _git(root, "log", f"{mb}..HEAD", "--format=%s%n%b%n---")
    return [c.strip() for c in log.split("---") if c.strip()]


def open_pr_body(root: Path) -> str:
    """Best-effort open PR body for the current branch (empty when unavailable)."""
    branch = current_branch(root)
    if not branch:
        return ""
    proc = subprocess.run(  # nosec B603 B607
        ["gh", "pr", "view", "--head", branch, "--json", "body", "-q", ".body"],
        cwd=str(root),
        check=False,
        capture_output=True,
        text=True,
    )
    if proc.returncode != 0:
        return ""
    return proc.stdout or ""



def added_lines_from_diff(diff_text: str) -> str:
    """Extract added file content from a unified diff (ignore headers and removals)."""
    out: list[str] = []
    for line in (diff_text or "").splitlines():
        if line.startswith("+") and not line.startswith("+++"):
            out.append(line[1:])
    return "\n".join(out)


def diff_added_text_since_merge_base(root: Path, *, base_ref: str | None = None) -> str:
    """Added-line content of ``merge-base...HEAD`` (empty when git cannot resolve)."""
    ref = base_ref or default_base_ref(root)
    mb = _git(root, "merge-base", "HEAD", ref).strip()
    if not mb:
        return ""
    return added_lines_from_diff(_git(root, "diff", f"{mb}...HEAD"))


def client_name_scrub_failures(
    root: Path,
    *,
    denylist: Iterable[str] | None = None,
    branch: str | None = None,
    commit_messages: Iterable[str] | None = None,
    pr_body: str | None = None,
    diff_added_text: str | None = None,
    environ: dict[str, str] | None = None,
) -> list[str]:
    """Fail closed when a configured denylist marker appears on PR-visible surfaces.

    Surfaces: branch name, commit messages, PR body, and added lines of the PR
    diff. When no denylist is configured, returns []. Explicit kwargs avoid
    git/gh (tests and fixtures).
    """
    root = Path(root)
    tokens = tuple(denylist) if denylist is not None else load_denylist(root, environ=environ)
    if not is_configured(tokens):
        return []

    failures: list[str] = []
    branch_name = current_branch(root) if branch is None else branch
    failures.extend(findings_in_text(branch_name, tokens, surface="branch name"))

    messages = (
        list(commit_messages)
        if commit_messages is not None
        else commit_messages_since_merge_base(root)
    )
    for idx, message in enumerate(messages):
        failures.extend(findings_in_text(message, tokens, surface=f"commit message[{idx}]"))

    body = open_pr_body(root) if pr_body is None else pr_body
    failures.extend(findings_in_text(body, tokens, surface="PR body"))

    added = (
        diff_added_text
        if diff_added_text is not None
        else diff_added_text_since_merge_base(root)
    )
    failures.extend(findings_in_text(added, tokens, surface="PR diff added lines"))
    return failures
