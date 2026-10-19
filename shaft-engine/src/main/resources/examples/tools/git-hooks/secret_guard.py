#!/usr/bin/env python3
"""Git pre-commit and pre-push guard against generated reports and secrets (#7177).

It checks every blob that a commit stages or that a push sends:

- paths of generated reports and local secret files (Allure HTML, allure-results/,
  allure-report/, secrets.properties, browserstack.yml),
- literal values from the local secret files, raw and inside the base64 blobs of an
  Allure single-file report,
- JWT-like tokens,
- secret assignments such as ``accessKey = <value>``.

It never prints a value, and it refuses a direct push to main or master.
A false positive is fixed here or in ``.secret-guard-allow``, never with ``--no-verify``.

Usage::

    python3 secret_guard.py --staged                 # pre-commit
    python3 secret_guard.py --push < <pushed refs>    # pre-push (git passes the refs on stdin)
    python3 secret_guard.py --rev origin/main..HEAD   # audit a branch

``SECRET_GUARD_FILES`` lists the local secret files (comma separated, relative to the
repository root). The default is
``src/main/resources/properties/secrets.properties,browserstack.yml``.
``.secret-guard-allow`` at the repository root holds path globs, one per line, whose
files skip the token and assignment checks. Report paths and known values are always checked.
"""
from __future__ import annotations

import base64
import binascii
import fnmatch
import json
import os
import re
import shutil
import subprocess  # nosec B404 - runs git only, with a resolved path and no shell
import sys
from pathlib import Path
from typing import Iterable, Iterator

DEFAULT_SECRET_FILES = "src/main/resources/properties/secrets.properties,browserstack.yml"
ALLOW_FILE = ".secret-guard-allow"
PROTECTED_REFS = ("refs/heads/main", "refs/heads/master")
BLOCKED_PATH = re.compile(
    r"(?i)(allure[^/]*\.html$|(^|/)allure-(results|report)/|(^|/)secrets\.properties$|(^|/)browserstack\.yml$)"
)
TOKEN = re.compile(rb"eyJ[A-Za-z0-9_-]{10,}\.eyJ[A-Za-z0-9_-]{10,}")
ASSIGNMENT = re.compile(
    rb"(?i)(client_?secret|access_?key|password|api_?key|session_?id)[\"']?\s*[:=]\s*[\"']?"
    rb"([A-Za-z0-9._~+/=-]{16,})(?!\s*\()"
)
REPORT_BLOB = re.compile(rb'd\("[^"]+","([A-Za-z0-9+/=]{16,})"\)')
SECRET_LINE = re.compile(r"\s*[^#=:\s]+\s*[=:]\s*(\S{12,})")
MINIMUM_VALUE_LENGTH = 12
ZERO_SHA = re.compile(r"^0+$")


def _git_executable() -> str:
    found = shutil.which("git")
    if not found:
        raise RuntimeError("secret_guard: git is not on PATH")
    return found


def git(*args: str, data: bytes | None = None) -> bytes:
    """Run git and return its standard output."""
    return subprocess.run(  # nosec B603 - fixed git argv, no shell
        [_git_executable(), *args], input=data, capture_output=True, check=True
    ).stdout


def secret_values(root: Path, names: Iterable[str]) -> set[bytes]:
    """Read the literal secret values from the local secret files that exist."""
    values: set[bytes] = set()
    for name in names:
        path = root / name.strip()
        if not name.strip() or not path.is_file():
            continue
        text = path.read_text(encoding="utf-8", errors="ignore")
        if path.suffix == ".json":
            _json_values(json.loads(text or "null"), values)
            continue
        for line in text.splitlines():
            match = SECRET_LINE.match(line)
            if match:
                value = match.group(1).strip("\"'")
                if len(value) >= MINIMUM_VALUE_LENGTH:
                    values.add(value.encode())
    return values


def _json_values(node: object, values: set[bytes]) -> None:
    if isinstance(node, dict):
        for child in node.values():
            _json_values(child, values)
    elif isinstance(node, list):
        for child in node:
            _json_values(child, values)
    elif isinstance(node, str) and len(node) >= MINIMUM_VALUE_LENGTH:
        values.add(node.encode())


def allowed_globs(root: Path) -> list[str]:
    """Path globs whose files skip the token and assignment heuristics."""
    path = root / ALLOW_FILE
    if not path.is_file():
        return []
    lines = path.read_text(encoding="utf-8").splitlines()
    return [line.strip() for line in lines if line.strip() and not line.lstrip().startswith("#")]


def _decoded_blobs(data: bytes) -> list[bytes]:
    blobs = [data]
    for encoded in REPORT_BLOB.findall(data):
        try:
            blobs.append(base64.b64decode(encoded + b"=" * (-len(encoded) % 4)))
        except (binascii.Error, ValueError):
            continue
    return blobs


def findings(path: str, data: bytes, values: set[bytes], allow: list[str]) -> list[str]:
    """Name what is wrong with one blob, without its content."""
    found = ["generated report or secret file"] if BLOCKED_PATH.search(path) else []
    blobs = _decoded_blobs(data)
    if any(value in blob for blob in blobs for value in values):
        found.append("known secret value")
    if any(fnmatch.fnmatch(path, pattern) for pattern in allow):
        return found
    if any(TOKEN.search(blob) for blob in blobs):
        found.append("JWT-like token")
    if any(
        re.search(rb"\d", match.group(2)) and re.search(rb"[A-Za-z]", match.group(2))
        for match in ASSIGNMENT.finditer(data)
    ):
        found.append("secret assignment")
    return found


def staged() -> Iterator[tuple[str, bytes]]:
    """Blobs added or changed in the index."""
    names = git("diff", "--cached", "--name-only", "--diff-filter=ACMR", "-z").split(b"\0")
    for name in filter(None, names):
        path = name.decode()
        yield path, git("show", ":" + path)


def pushed_revisions(refs: str) -> list[str]:
    """Turn pre-push stdin into rev-list arguments; refuse a direct push to main."""
    args: list[str] = []
    for line in refs.splitlines():
        parts = line.split()
        if len(parts) != 4:
            continue
        _, local_sha, remote_ref, remote_sha = parts
        if ZERO_SHA.match(local_sha):
            continue
        if remote_ref in PROTECTED_REFS:
            raise PermissionError("secret_guard: no direct push to " + remote_ref)
        args.append(local_sha)
        args.extend(["^" + remote_sha] if not ZERO_SHA.match(remote_sha) else ["--not", "--remotes"])
    return args


def revisions(args: list[str]) -> Iterator[tuple[str, bytes]]:
    """Blobs reachable from the given rev-list arguments, each once."""
    if not args:
        return
    objects = []
    for line in git("rev-list", "--objects", *args).decode().splitlines():
        sha, _, path = line.partition(" ")
        if path:
            objects.append((sha, path))
    if not objects:
        return
    kinds = git("cat-file", "--batch-check=%(objectname) %(objecttype)",
                data="".join(sha + "\n" for sha, _ in objects).encode()).decode().split()
    blob_shas = {kinds[index] for index in range(0, len(kinds) - 1, 2) if kinds[index + 1] == "blob"}
    seen: set[str] = set()
    for sha, path in objects:
        if sha in blob_shas and sha not in seen:
            seen.add(sha)
            yield path, git("cat-file", "blob", sha)


def main(argv: list[str], stdin: str = "") -> int:
    """Run the guard; return the process exit code."""
    root = Path(git("rev-parse", "--show-toplevel").decode().strip())
    try:
        if argv[:1] == ["--push"]:
            items: Iterable[tuple[str, bytes]] = revisions(pushed_revisions(stdin))
        elif argv[:1] == ["--rev"]:
            items = revisions(argv[1:])
        else:
            items = staged()
    except PermissionError as refused:
        print(str(refused), file=sys.stderr)
        return 1
    values = secret_values(root, os.environ.get("SECRET_GUARD_FILES", DEFAULT_SECRET_FILES).split(","))
    allow = allowed_globs(root)
    blocked = [f"{path}: {', '.join(found)}" for path, data in items for found in [findings(path, data, values, allow)] if found]
    if blocked:
        print("secret_guard BLOCKED (values not shown):\n  " + "\n  ".join(blocked), file=sys.stderr)
        print("Fix the files, or a false positive in the guard or .secret-guard-allow. Never use --no-verify.",
              file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], sys.stdin.read() if sys.argv[1:2] == ["--push"] else ""))
