"""Canonical repository and pull-request context resolution."""

from __future__ import annotations

import json
import re
import shutil
import subprocess  # nosec B404 - fixed read-only git/gh commands, never a shell.
from dataclasses import dataclass
from pathlib import Path


_REPOSITORY = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
_PR_URL = re.compile(
    r"^https://[^/]+/(?P<owner>[A-Za-z0-9_.-]+)/(?P<repo>[A-Za-z0-9_.-]+)/pull/(?P<number>[1-9]\d*)/?$"
)
_GIT_URL = re.compile(r"[:/]([^/:]+)/([^/]+?)(?:\.git)?/?$")


class RepositoryContextError(ValueError):
    """Raised when repository or pull-request context is invalid."""


@dataclass(frozen=True)
class RepositoryContext:
    """Stable repository context shared by scripts and portable clients."""

    repo: str
    root: Path
    pr_number: int | None


def parse_pr_reference(pr: int | str | None) -> tuple[str | None, int | None, bool]:
    """Return (URL repository, number, is bare numeric) for one PR selector."""
    if pr is None:
        return None, None, False
    if isinstance(pr, int) or (isinstance(pr, str) and pr.isdigit()):
        number = int(pr)
        if number < 1:
            raise RepositoryContextError("PR number must be positive")
        return None, number, True
    if not isinstance(pr, str):
        raise RepositoryContextError("PR must be a positive number or GitHub pull-request URL")
    match = _PR_URL.fullmatch(pr.strip())
    if not match:
        raise RepositoryContextError("PR must be a positive number or GitHub pull-request URL")
    return f"{match.group('owner')}/{match.group('repo')}", int(match.group("number")), False


def validate_repository(repository: str) -> str:
    """Validate and return an owner/repository slug."""
    repository = repository.strip()
    if not _REPOSITORY.fullmatch(repository):
        raise RepositoryContextError("repository must use owner/repo form")
    return repository


def parse_git_remote(url: str) -> str:
    """Extract owner/repository from an SSH or HTTPS git remote URL."""
    match = _GIT_URL.search(url.strip())
    if not match:
        raise RepositoryContextError(f"cannot parse owner/repo from git remote url: {url!r}")
    return validate_repository(f"{match.group(1)}/{match.group(2)}")


def infer_repository(
    root: Path,
    *,
    runner=None,
    executable_resolver=None,
) -> str:
    """Infer owner/repository with `gh repo view`, then the origin git remote."""
    runner = subprocess.run if runner is None else runner
    executable_resolver = shutil.which if executable_resolver is None else executable_resolver
    root = Path(root).resolve()
    gh_executable = executable_resolver("gh")
    if gh_executable is not None:
        try:
            result = runner(  # nosec B603
                [gh_executable, "repo", "view", "--json", "nameWithOwner"],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
        except (OSError, subprocess.SubprocessError):
            result = None
        if result is not None and result.returncode == 0:
            try:
                repository = json.loads(result.stdout)["nameWithOwner"]
                if isinstance(repository, str):
                    return validate_repository(repository)
            except (json.JSONDecodeError, KeyError, RepositoryContextError):
                # A malformed gh response is not authoritative; continue to
                # the documented git-origin fallback below.
                pass

    git_executable = executable_resolver("git")
    if git_executable is None:
        raise RepositoryContextError(
            "cannot resolve repository: `gh repo view` failed and git is not on PATH"
        )
    try:
        remote = runner(  # nosec B603
            [git_executable, "remote", "get-url", "origin"],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
    except (OSError, subprocess.SubprocessError) as error:
        raise RepositoryContextError(f"cannot resolve repository: {error}") from error
    if remote.returncode != 0:
        raise RepositoryContextError(
            "cannot resolve repository: `gh repo view` and `git remote get-url origin` both failed"
        )
    return parse_git_remote(remote.stdout)


def resolve_repository_context(
    *,
    explicit_repo: str | None,
    pr: int | str | None,
    explicit_root: Path | None,
    cwd: Path,
    runner=None,
    executable_resolver=None,
) -> RepositoryContext:
    """Resolve context with repo > PR URL > root > cwd precedence."""
    root = Path(explicit_root if explicit_root is not None else cwd).resolve()
    if not root.is_dir():
        raise RepositoryContextError(f"repository root must be an existing directory: {root}")
    url_repo, pr_number, _ = parse_pr_reference(pr)
    repository = explicit_repo or url_repo or infer_repository(
        root,
        runner=runner,
        executable_resolver=executable_resolver,
    )
    return RepositoryContext(validate_repository(repository), root, pr_number)
