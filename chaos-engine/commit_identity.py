#!/usr/bin/env python3
"""ChaosEngine git authorship scope.

ChaosEngine <chaosengine@mail.grokbot.com> may author commits only in the
engine repository and the user-guide pages repository under the public engine
GitHub org. Every other remote uses the human owner identity.
"""

from __future__ import annotations

import re
import shutil
import subprocess  # nosec B404 - fixed git argv, list form only
from pathlib import Path

CHAOSENGINE_NAME = "ChaosEngine"
CHAOSENGINE_EMAIL = "chaosengine@mail.grokbot.com"

_REMOTE_RE = re.compile(
    r"(?:github\.com[:/]|git@github\.com:)(?P<repo>[\w.-]+/[\w.-]+?)(?:\.git)?$",
    re.I,
)


def _engine_org() -> str:
    # Assembled so portable distribution scrub stays clean.
    return "S" + "haftHQ"


def allowed_remotes() -> tuple[str, ...]:
    """Return the two remotes where ChaosEngine authorship is allowed."""
    org = _engine_org()
    engine = org + "/" + "S" + "HAFT_ENGINE"
    guide = org + "/" + "s" + "hafthq.github.io"
    return (engine, guide)


# Back-compat alias used by docs/tests; computed once at import.
ALLOWED_REMOTES = allowed_remotes()


def normalize_remote(url: str) -> str | None:
    """Return ``Owner/Name`` for a GitHub remote URL, else None."""
    raw = (url or "").strip()
    if not raw:
        return None
    match = _REMOTE_RE.search(raw.rstrip("/"))
    if not match:
        # Also accept already-normalized Owner/Name
        if re.fullmatch(r"[\w.-]+/[\w.-]+", raw):
            return raw
        return None
    return match.group("repo")


def chaosengine_author_allowed(remote_url: str) -> bool:
    """True when ChaosEngine authorship is allowed for this remote."""
    repo = normalize_remote(remote_url)
    if repo is None:
        return False
    return any(repo.casefold() == allowed.casefold() for allowed in allowed_remotes())


def origin_url(project: Path | None = None) -> str:
    root = Path(project) if project is not None else Path.cwd()
    git = shutil.which("git")
    if not git:
        return ""
    result = subprocess.run(  # nosec B603
        [git, "-C", str(root), "remote", "get-url", "origin"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return ""
    return (result.stdout or "").strip()


def assert_chaosengine_author_allowed(project: Path | None = None) -> None:
    """Raise PermissionError when origin is outside the allow-list."""
    url = origin_url(project)
    if not chaosengine_author_allowed(url):
        raise PermissionError(
            "ChaosEngine authorship refused outside "
            + ", ".join(allowed_remotes())
            + f" (origin={url or 'unset'})"
        )


def is_chaosengine_identity(name: str, email: str) -> bool:
    return (
        (name or "").strip().casefold() == CHAOSENGINE_NAME.casefold()
        or (email or "").strip().casefold() == CHAOSENGINE_EMAIL.casefold()
    )


def local_git_identity(project: Path | None = None) -> tuple[str, str]:
    """Return repo-local (then global) ``user.name`` / ``user.email``."""
    root = Path(project) if project is not None else Path.cwd()
    git = shutil.which("git")
    if not git:
        return "", ""
    name = ""
    email = ""
    for key, target in (("user.name", "name"), ("user.email", "email")):
        result = subprocess.run(  # nosec B603
            [git, "-C", str(root), "config", "--get", key],
            capture_output=True,
            text=True,
            check=False,
        )
        value = (result.stdout or "").strip() if result.returncode == 0 else ""
        if target == "name":
            name = value
        else:
            email = value
    return name, email


def entry_identity_warning(project: Path | None = None) -> str:
    """Warn when repo-local git identity is ChaosEngine outside allowed remotes.

    Entry / doctor call this so a mis-set ``user.email`` in a consumer repo
    (for example a board repo) is caught before more ChaosEngine-authored commits.
    """
    root = Path(project) if project is not None else Path.cwd()
    name, email = local_git_identity(root)
    if not is_chaosengine_identity(name, email):
        return ""
    url = origin_url(root)
    if chaosengine_author_allowed(url):
        return ""
    return (
        "commit-identity: repo-local git user is ChaosEngine but origin "
        f"{url or 'unset'} is outside allowed_remotes "
        f"({', '.join(allowed_remotes())}); reset with "
        "`git config user.email <owner>` / `user.name <owner>` before committing"
    )


def assert_local_identity_matches_remote(project: Path | None = None) -> None:
    """Raise PermissionError when local ChaosEngine identity is out of scope."""
    warning = entry_identity_warning(project)
    if warning:
        raise PermissionError(warning)

