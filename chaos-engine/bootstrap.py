#!/usr/bin/env python3
"""Resolve and install the latest portable ChaosEngine from a GitHub branch."""

from __future__ import annotations

import argparse
import email.utils
import hashlib
import json
import os
import re
import runpy
import sys
import tempfile
import time
import types
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path, PurePosixPath


REPOSITORY = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")
COMMIT = re.compile(r"[0-9a-f]{40}")
MAX_RESPONSE_BYTES = 10 * 1024 * 1024
MAX_SOURCE_BYTES = 10 * 1024 * 1024
MAX_FILE_BYTES = 2 * 1024 * 1024
MAX_FILES = 2000
MAX_READ_ATTEMPTS = 4
MAX_RETRY_AFTER_SECONDS = 60.0
RETRY_BASE_SECONDS = 1.0
TRANSIENT_HTTP_STATUS = frozenset({408, 425, 429, 500, 502, 503, 504})


def parse_retry_after(value: str) -> float | None:
    try:
        delay = float(value)
    except ValueError:
        try:
            parsed = email.utils.parsedate_to_datetime(value)
        except (TypeError, ValueError, OverflowError):
            return None
        if parsed is None or parsed.tzinfo is None:
            return None
        delay = max(0.0, parsed.timestamp() - time.time())
    if not 0 <= delay <= MAX_RETRY_AFTER_SECONDS:
        return None
    return delay


def request(url: str) -> urllib.request.Request:
    headers = {"Accept": "application/vnd.github+json", "User-Agent": "ChaosEngine-bootstrap"}
    token = os.environ.get("GITHUB_TOKEN")
    if token:
        headers["Authorization"] = f"Bearer {token}"
    return urllib.request.Request(url, headers=headers)


def valid_branch(branch: str) -> bool:
    parts = branch.split("/")
    return (
        re.fullmatch(r"[^\x00-\x20\x7f~^:?*\\\[\]]+", branch) is not None
        and not branch.startswith(("-", "/"))
        and not branch.endswith(("/", "."))
        and "//" not in branch
        and ".." not in branch
        and "@{" not in branch
        and branch != "HEAD"
        and all(part and not part.startswith(".") and not part.endswith(".lock") for part in parts)
    )


def retry_delay(error: BaseException, attempt: int) -> float | None:
    if isinstance(error, urllib.error.HTTPError):
        retry_after = error.headers.get("Retry-After") if error.headers is not None else None
        if error.code not in TRANSIENT_HTTP_STATUS and not (
            error.code == 403 and retry_after is not None
        ):
            return None
        if retry_after is not None:
            delay = parse_retry_after(retry_after)
            if delay is None:
                return None
            return delay
        if error.code == 429:
            return MAX_RETRY_AFTER_SECONDS
    elif not isinstance(error, (ConnectionError, TimeoutError, urllib.error.URLError)):
        return None
    return RETRY_BASE_SECONDS * (2**attempt)


def read_response(
    opener,
    url: str,
    *,
    limit: int = MAX_RESPONSE_BYTES,
    sleeper=None,
) -> bytes:
    sleeper = time.sleep if sleeper is None else sleeper
    for attempt in range(MAX_READ_ATTEMPTS):
        try:
            with opener(request(url), timeout=30) as response:
                value = response.read(limit + 1)
            break
        except (OSError, TimeoutError, urllib.error.URLError) as error:
            try:
                delay = retry_delay(error, attempt)
            finally:
                if isinstance(error, urllib.error.HTTPError):
                    error.close()
            if delay is None or attempt + 1 == MAX_READ_ATTEMPTS:
                raise RuntimeError(
                    "unable to resolve latest ChaosEngine from the configured upstream"
                ) from error
            sleeper(delay)
    if len(value) > limit:
        raise ValueError("ChaosEngine upstream response exceeds the download limit")
    return value


def resolve_latest(repository: str, branch: str | None, opener=urllib.request.urlopen) -> tuple[str, str]:
    components = repository.split("/")
    if (
        REPOSITORY.fullmatch(repository) is None
        or len(components) != 2
        or any(component in {".", ".."} for component in components)
    ):
        raise ValueError("repository must be an explicit GitHub owner/repository")
    if branch is None:
        repository_document = read_response(
            opener,
            f"https://api.github.com/repos/{repository}",
        )
        try:
            repository_value = json.loads(repository_document)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError("GitHub returned invalid repository metadata") from error
        branch = repository_value.get("default_branch") if isinstance(repository_value, dict) else None
        if not isinstance(branch, str):
            raise ValueError("GitHub returned invalid repository metadata")
    if not valid_branch(branch):
        raise ValueError("branch is invalid")
    encoded_branch = urllib.parse.quote(branch, safe="")
    document = read_response(
        opener,
        f"https://api.github.com/repos/{repository}/commits/{encoded_branch}",
    )
    try:
        value = json.loads(document)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("GitHub returned an invalid ChaosEngine revision") from error
    commit = value.get("sha") if isinstance(value, dict) else None
    if not isinstance(commit, str) or COMMIT.fullmatch(commit) is None:
        raise ValueError("GitHub returned an invalid ChaosEngine revision")
    return commit, branch


def download_source(
    repository: str,
    commit: str,
    destination: Path,
    *,
    opener=urllib.request.urlopen,
) -> Path:
    """Download only the bounded ChaosEngine subtree, never the whole repository."""
    encoded_repository = "/".join(
        urllib.parse.quote(part, safe="") for part in repository.split("/")
    )
    document = read_response(
        opener,
        f"https://api.github.com/repos/{encoded_repository}/git/trees/{commit}?recursive=1",
    )
    try:
        value = json.loads(document)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("GitHub returned an invalid ChaosEngine source tree") from error
    if not isinstance(value, dict) or value.get("truncated") is not False:
        raise ValueError("GitHub returned an incomplete ChaosEngine source tree")
    tree = value.get("tree")
    if not isinstance(tree, list):
        raise ValueError("GitHub returned an invalid ChaosEngine source tree")

    selected: list[tuple[PurePosixPath, int]] = []
    total = 0
    for entry in tree:
        if not isinstance(entry, dict) or not isinstance(entry.get("path"), str):
            raise ValueError("GitHub returned an invalid ChaosEngine source tree")
        path = PurePosixPath(entry["path"])
        if path.is_absolute() or ".." in path.parts or not path.parts:
            raise ValueError("ChaosEngine source tree contains an unsafe path")
        if path.parts[0] != "chaos-engine":
            continue
        if entry.get("type") == "tree":
            continue
        if entry.get("type") != "blob" or entry.get("mode") not in {"100644", "100755"}:
            raise ValueError("ChaosEngine source tree contains an unsupported entry")
        size = entry.get("size")
        if not isinstance(size, int) or size < 0 or size > MAX_FILE_BYTES:
            raise ValueError("ChaosEngine source file exceeds the download limit")
        relative = PurePosixPath(*path.parts[1:])
        if not relative.parts:
            raise ValueError("ChaosEngine source tree has an unexpected layout")
        if relative.parts[:2] == ("assets", "brand") or relative.as_posix() == "RESEARCH.md":
            continue
        selected.append((relative, size))
        total += size

    if not selected:
        raise ValueError("ChaosEngine source tree has an unexpected layout")
    if len(selected) > MAX_FILES:
        raise ValueError("ChaosEngine source tree contains too many files")
    if total > MAX_SOURCE_BYTES:
        raise ValueError("ChaosEngine source tree exceeds the download limit")

    source = destination / "chaos-engine"
    source.mkdir()
    for relative, expected_size in selected:
        encoded_path = "/".join(urllib.parse.quote(part, safe="") for part in relative.parts)
        content = read_response(
            opener,
            f"https://raw.githubusercontent.com/{encoded_repository}/{commit}/chaos-engine/{encoded_path}",
            limit=MAX_FILE_BYTES,
        )
        if len(content) != expected_size:
            raise ValueError("ChaosEngine source file does not match the resolved tree")
        target = source.joinpath(*relative.parts)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(content)
    if not (source / "skills/chaos-engine/SKILL.md").is_file():
        raise ValueError("ChaosEngine source tree is incomplete")
    return source


def load_installer(source: Path):
    path = source / "install.py"
    return types.SimpleNamespace(**runpy.run_path(str(path)))


def resolve_distribution(installer, project: Path, source: Path, requested: str | None) -> str:
    if isinstance(requested, str) and requested.strip():
        return requested.strip()
    detect = getattr(installer, "detect_distribution", None)
    if callable(detect):
        guessed = detect(project, source)
        if isinstance(guessed, str) and guessed.strip():
            return guessed.strip()
    return "portable"


def install_latest(
    project: Path,
    *,
    repository: str,
    branch: str | None = None,
    skip_tools: bool = False,
    distribution: str | None = None,
    opener=urllib.request.urlopen,
    provisioner=None,
) -> dict[str, object]:
    project = Path(project).resolve()
    if not project.is_dir():
        raise ValueError(f"project is not a directory: {project}")
    prior_install = (project / ".chaos-engine").exists()
    commit, resolved_branch = resolve_latest(repository, branch, opener=opener)
    with tempfile.TemporaryDirectory(prefix="chaos-engine-bootstrap-") as temporary:
        source = download_source(repository, commit, Path(temporary), opener=opener)
        installer = load_installer(source)
        distribution = resolve_distribution(installer, project, source, distribution)
        if distribution == "portable":
            provenance = {
                "kind": "git-digest",
                "repositorySha256": hashlib.sha256(repository.casefold().encode()).hexdigest(),
                "branchSha256": hashlib.sha256(resolved_branch.encode()).hexdigest(),
                "commit": commit,
            }
        else:
            provenance = {
                "kind": "git",
                "repository": repository,
                "branch": resolved_branch,
                "commit": commit,
            }
        if skip_tools:
            target = installer.install(
                project, source, commit, source_record=provenance, distribution=distribution
            )
        else:
            target = installer.install_with_dependencies(
                project,
                source,
                commit,
                provisioner=provisioner,
                source_record=provenance,
                distribution=distribution,
            )
    if skip_tools or provisioner is not None:
        return {"status": "installed", "root": str(target), "commit": commit}
    host_controller = installer.load_installed_controller(target, "hosts")
    try:
        doctor = installer.doctor_with_dependencies(project, verify_clients=False)
        if doctor.get("status") != "healthy":
            raise RuntimeError("ChaosEngine doctor did not report a healthy installation")
        clients = host_controller.activate_detected_plugins(project)
        doctor["clients"] = clients.get("clients", {})
    except BaseException:
        if prior_install:
            installer.rollback(project)
        else:
            installer.uninstall_with_dependencies(project)
        raise
    return {
        "status": "installed",
        "root": str(target),
        "commit": commit,
        "clients": clients,
        "doctor": doctor,
    }


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--project", type=Path, default=Path.cwd())
    result.add_argument("--repository", required=True)
    result.add_argument("--branch")
    result.add_argument("--distribution")
    result.add_argument("--skip-tools", action="store_true", help=argparse.SUPPRESS)
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        result = install_latest(
            args.project,
            repository=args.repository,
            branch=args.branch,
            skip_tools=args.skip_tools,
            distribution=args.distribution,
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
