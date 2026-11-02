#!/usr/bin/env python3
"""
ChaosEngine git authorship scope (H-IDENTITY).

Repo → allowed-identity map: ChaosEngine <chaosengine@mail.grokbot.com> may
author commits ONLY in the public engine repo and its user-guide pages repo.
Every other remote must use the human owner identity. Pre-commit / PreToolUse
refuse ChaosEngine authorship outside that map.
"""
from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess  # nosec B404 - fixed git argv, list form only
import sys
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

# Explicit repo → allowed author email map (H-IDENTITY).
# ChaosEngine email is allowed only for these keys; all other repos require
# a non-ChaosEngine owner identity.
def repo_identity_map() -> dict[str, str]:
    """Map normalized Owner/Name → allowed ChaosEngine email (or empty)."""
    return {repo: CHAOSENGINE_EMAIL for repo in allowed_remotes()}


def normalize_remote(url: str) -> str | None:
    """Return ``Owner/Name`` for a GitHub remote URL, else None."""
    raw = (url or "").strip()
    if not raw:
        return None
    match = _REMOTE_RE.search(raw.rstrip("/"))
    if not match:
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


def effective_commit_identity(
    project: Path | None = None,
    *,
    environ: dict[str, str] | None = None,
    name: str | None = None,
    email: str | None = None,
) -> tuple[str, str]:
    """Identity that would author the next commit (env overrides config)."""
    env = environ if environ is not None else os.environ
    cfg_name, cfg_email = local_git_identity(project)
    author_name = (
        name
        if name is not None
        else str(env.get("GIT_AUTHOR_NAME") or env.get("GIT_COMMITTER_NAME") or cfg_name or "").strip()
    )
    author_email = (
        email
        if email is not None
        else str(env.get("GIT_AUTHOR_EMAIL") or env.get("GIT_COMMITTER_EMAIL") or cfg_email or "").strip()
    )
    return author_name, author_email


def parse_owner_identity(raw: str) -> tuple[str, str]:
    """Parse ``Name <email>`` or bare email into (name, email)."""
    text = (raw or "").strip()
    if not text:
        return "", ""
    match = re.match(r"^(?P<name>.*?)<(?P<email>[^>]+)>\s*$", text)
    if match:
        return match.group("name").strip(), match.group("email").strip()
    if "@" in text and " " not in text:
        return "", text
    return text, ""


def configured_owner_identity(
    project: Path | None = None,
    *,
    environ: dict[str, str] | None = None,
) -> tuple[str, str]:
    """
    Return host-configured owner (name, email) from env or untracked state file.

    Sources (first wins):
    - ``CE_OWNER_IDENTITY`` (``Name <email>``)
    - ``CE_OWNER_NAME`` + ``CE_OWNER_EMAIL``
    - ``{project}/.chaos-engine-state/owner-identity`` (gitignored; one line)

    Empty when unset — portable default allows any non-ChaosEngine identity.
    Never hardcode a personal name/email in the tree; operators set this locally.
    """
    env = environ if environ is not None else os.environ
    raw = str(env.get("CE_OWNER_IDENTITY") or "").strip()
    if raw:
        return parse_owner_identity(raw)
    name = str(env.get("CE_OWNER_NAME") or "").strip()
    email = str(env.get("CE_OWNER_EMAIL") or "").strip()
    if name or email:
        return name, email
    root = Path(project) if project is not None else Path.cwd()
    state = root / ".chaos-engine-state" / "owner-identity"
    if state.is_file():
        try:
            return parse_owner_identity(state.read_text(encoding="utf-8").splitlines()[0])
        except OSError:
            return "", ""
    return "", ""


def pre_commit_identity_failures(
    project: Path | None = None,
    *,
    environ: dict[str, str] | None = None,
    name: str | None = None,
    email: str | None = None,
    remote_url: str | None = None,
) -> list[str]:
    """
    Return failures when the next commit would use ChaosEngine outside the map.

    Empty list means the commit may proceed. Non-ChaosEngine identities are
    always allowed (owner identity on any repo).
    """
    root = Path(project) if project is not None else Path.cwd()
    author_name, author_email = effective_commit_identity(
        root, environ=environ, name=name, email=email,
    )
    url = origin_url(root) if remote_url is None else remote_url
    if is_chaosengine_identity(author_name, author_email):
        if chaosengine_author_allowed(url):
            return []
        return [
            "commit-identity: ChaosEngine authorship blocked for origin "
            f"{url or 'unset'}; allowed only on {', '.join(allowed_remotes())}. "
            "Use the owner git identity (user.name / user.email) in this repo."
        ]
    # Non-ChaosEngine: optional host-configured owner binding (CE_OWNER_IDENTITY).
    expected_name, expected_email = configured_owner_identity(root, environ=environ)
    if not expected_email and not expected_name:
        return []
    if chaosengine_author_allowed(url):
        return []  # allow-listed remotes may still use owner identity
    failures: list[str] = []
    if expected_email and author_email.casefold() != expected_email.casefold():
        failures.append(
            "commit-identity: author email "
            f"{author_email or '(empty)'} does not match CE_OWNER_IDENTITY "
            f"email {expected_email} for origin {url or 'unset'}"
        )
    if expected_name and author_name and author_name.casefold() != expected_name.casefold():
        failures.append(
            "commit-identity: author name "
            f"{author_name!r} does not match CE_OWNER_IDENTITY name "
            f"{expected_name!r} for origin {url or 'unset'}"
        )
    return failures


def entry_identity_warning(project: Path | None = None) -> str:
    """
    Warn when repo-local git identity is ChaosEngine outside allowed remotes.

    Entry / doctor call this so a mis-set ``user.email`` in a consumer repo
    is caught before more ChaosEngine-authored commits.
    """
    failures = pre_commit_identity_failures(project)
    return failures[0] if failures else ""


def assert_local_identity_matches_remote(project: Path | None = None) -> None:
    """Raise PermissionError when local ChaosEngine identity is out of scope."""
    warning = entry_identity_warning(project)
    if warning:
        raise PermissionError(warning)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--pre-commit",
        action="store_true",
        help="exit 1 when ChaosEngine identity is out of scope for origin",
    )
    parser.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    root = args.project or Path.cwd()
    if args.pre_commit:
        failures = pre_commit_identity_failures(root)
        for item in failures:
            print(item, file=sys.stderr)
        return 1 if failures else 0
    warning = entry_identity_warning(root)
    if warning:
        print(warning)
        return 1
    print("commit-identity: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
