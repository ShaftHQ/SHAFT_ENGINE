#!/usr/bin/env python3
"""Resolve and install the latest portable ChaosEngine from a GitHub branch."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import sys
import tempfile
import types
import urllib.error
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path, PurePosixPath


REPOSITORY = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")
COMMIT = re.compile(r"[0-9a-f]{40}")
MAX_ARCHIVE_BYTES = 50 * 1024 * 1024
MAX_FILES = 2000


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


def read_response(opener, url: str) -> bytes:
    try:
        with opener(request(url), timeout=30) as response:
            value = response.read(MAX_ARCHIVE_BYTES + 1)
    except (OSError, TimeoutError, urllib.error.URLError) as error:
        raise RuntimeError("unable to resolve latest ChaosEngine from the configured upstream") from error
    if len(value) > MAX_ARCHIVE_BYTES:
        raise ValueError("ChaosEngine archive exceeds the download limit")
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


def extract_source(archive: bytes, destination: Path) -> Path:
    archive_path = destination / "source.zip"
    archive_path.write_bytes(archive)
    source = destination / "chaos-engine"
    source.mkdir()
    try:
        with zipfile.ZipFile(archive_path) as package:
            infos = package.infolist()
            if len(infos) > MAX_FILES:
                raise ValueError("ChaosEngine archive contains too many files")
            if sum(info.file_size for info in infos) > MAX_ARCHIVE_BYTES:
                raise ValueError("ChaosEngine archive expands beyond the size limit")
            roots: set[str] = set()
            selected = []
            for info in infos:
                path = PurePosixPath(info.filename)
                if path.is_absolute() or ".." in path.parts or not path.parts:
                    raise ValueError("ChaosEngine archive contains an unsafe path")
                if (info.external_attr >> 16) & 0o170000 == 0o120000:
                    raise ValueError("ChaosEngine archive contains a link")
                if len(path.parts) >= 3 and path.parts[1] == "chaos-engine":
                    roots.add(path.parts[0])
                    selected.append((info, Path(*path.parts[2:])))
            if len(roots) != 1 or not selected:
                raise ValueError("ChaosEngine archive has an unexpected layout")
            for info, relative in selected:
                if not relative.parts:
                    continue
                target = source / relative
                if info.is_dir():
                    target.mkdir(parents=True, exist_ok=True)
                    continue
                target.parent.mkdir(parents=True, exist_ok=True)
                with package.open(info) as input_stream, target.open("xb") as output_stream:
                    shutil.copyfileobj(input_stream, output_stream)
    except zipfile.BadZipFile as error:
        raise ValueError("ChaosEngine archive is invalid") from error
    if not (source / "skills/chaos-engine/SKILL.md").is_file():
        raise ValueError("ChaosEngine archive is incomplete")
    return source


def load_installer(source: Path):
    path = source / "install.py"
    module = types.ModuleType("chaos_engine_installer")
    module.__file__ = str(path)
    exec(compile(path.read_bytes(), str(path), "exec"), module.__dict__)
    return module


def install_latest(
    project: Path,
    *,
    repository: str,
    branch: str | None = None,
    skip_tools: bool = False,
    opener=urllib.request.urlopen,
    provisioner=None,
) -> dict[str, str]:
    project = Path(project).resolve()
    if not project.is_dir():
        raise ValueError(f"project is not a directory: {project}")
    commit, resolved_branch = resolve_latest(repository, branch, opener=opener)
    encoded_repository = "/".join(urllib.parse.quote(part, safe="") for part in repository.split("/"))
    archive = read_response(
        opener,
        f"https://codeload.github.com/{encoded_repository}/zip/{commit}",
    )
    with tempfile.TemporaryDirectory(prefix="chaos-engine-bootstrap-") as temporary:
        source = extract_source(archive, Path(temporary))
        installer = load_installer(source)
        provenance = {
            "kind": "git",
            "repository": repository,
            "branch": resolved_branch,
            "commit": commit,
        }
        if skip_tools:
            target = installer.install(project, source, commit, source_record=provenance)
        else:
            target = installer.install_with_dependencies(
                project,
                source,
                commit,
                provisioner=provisioner,
                source_record=provenance,
            )
    return {"status": "installed", "root": str(target), "commit": commit}


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--project", type=Path, default=Path.cwd())
    result.add_argument("--repository", required=True)
    result.add_argument("--branch")
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
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
