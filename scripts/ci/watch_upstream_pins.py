#!/usr/bin/env python3
"""Compare repository pins to the latest stable upstream.

Reads ``chaos-engine/dependencies.json`` and every ``chaos-engine/vendor/*/PIN.json``.
The machine receipt ``.chaos-engine-dependencies.json`` is not a pin.

Latest for MemPalace and Graphify is the newest non-yanked stable PyPI release.
Unreleased history (a newer default-branch commit, a changelog version that is
not published) is not latest. Latest for a vendored repository is the commit of
its newest GitHub release, or default-branch HEAD when that repository has no
stable release.

Report-only unless ``--write``. ``--write`` opens or updates one issue titled
``Upstream pin drift`` and does not merge or install anything.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess  # nosec B404 - fixed gh argument vector, never a shell.
import sys
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Mapping, Sequence

SUGGESTION_TITLE = "Upstream pin drift"
CAVEMAN_OVERLAY = "chaos-engine/vendor/caveman/INVENTORY.md"
WATCHED_PYPI = ("mempalace", "graphify")
_STABLE_VERSION = re.compile(r"^\d+(?:\.\d+)*$")
_USER_AGENT = "shaft-engine-upstream-watch"


class UpstreamError(RuntimeError):
    """Stable upstream could not be read. No report is produced."""


@dataclass(frozen=True)
class Pin:
    name: str
    kind: str
    current: str
    upstream_url: str
    repository: str = ""
    overlay: str = ""


@dataclass(frozen=True)
class Stable:
    """Published stable channel. Unreleased fields are not a target."""

    version: str = ""
    release_commit: str = ""
    head_commit: str = ""
    unreleased_version: str = ""


@dataclass(frozen=True)
class Verdict:
    name: str
    current: str
    latest: str
    upstream_url: str
    behind: bool
    overlay: str = ""


def _version_key(version: str) -> tuple[int, ...]:
    core = version.split("+", 1)[0]
    if not _STABLE_VERSION.fullmatch(core):
        raise ValueError(f"not a stable version: {version}")
    return tuple(int(part) for part in core.split("."))


def newest_stable_version(payload: Mapping[str, Any]) -> str:
    """Newest numbered release that still has a non-yanked file."""
    releases = payload.get("releases")
    if not isinstance(releases, dict):
        releases = {}
    stable: list[str] = []
    for version, files in releases.items():
        if not isinstance(version, str) or not _STABLE_VERSION.fullmatch(version):
            continue
        if not isinstance(files, list):
            continue
        if any(isinstance(item, dict) and not item.get("yanked") for item in files):
            stable.append(version)
    if stable:
        return max(stable, key=_version_key)
    info = payload.get("info") if isinstance(payload.get("info"), dict) else {}
    version = str(info.get("version") or "")
    if _STABLE_VERSION.fullmatch(version):
        return version
    raise UpstreamError("PyPI payload has no stable release")


def _stable_identity(pin: Pin, stable: Stable) -> str:
    """Newest published stable release. Unreleased history is not a target."""
    if pin.kind == "pypi":
        latest = stable.version
    elif stable.release_commit:
        latest = stable.release_commit
    else:
        latest = stable.head_commit
    if not latest:
        raise ValueError(f"{pin.name} has no stable upstream")
    return latest


def compare_pin(pin: Pin, stable: Stable) -> Verdict:
    latest = _stable_identity(pin, stable)
    if pin.kind == "pypi":
        behind = _version_key(pin.current) < _version_key(latest)
    elif pin.kind == "git":
        behind = pin.current != latest
    else:
        raise ValueError(f"unknown pin kind for {pin.name}")
    return Verdict(
        name=pin.name,
        current=pin.current,
        latest=latest,
        upstream_url=pin.upstream_url,
        behind=behind,
        overlay=pin.overlay,
    )


def load_pins(root: Path) -> tuple[Pin, ...]:
    """Repository pins only. Does not read ``.chaos-engine-dependencies.json``."""
    spec_path = root / "chaos-engine" / "dependencies.json"
    spec = json.loads(spec_path.read_text(encoding="utf-8"))
    tools = spec["tools"]
    dependencies = spec["dependencies"]
    pins: list[Pin] = []
    for name in WATCHED_PYPI:
        package = str(tools[name]["package"])
        if "==" not in package:
            raise ValueError(f"{name} package pin must be name==version")
        pins.append(
            Pin(
                name=name,
                kind="pypi",
                current=package.split("==", 1)[1],
                upstream_url=str(dependencies[name]["stableChannel"]),
            )
        )
    vendor_root = root / "chaos-engine" / "vendor"
    for pin_path in sorted(vendor_root.glob("*/PIN.json")):
        payload = json.loads(pin_path.read_text(encoding="utf-8"))
        repository = str(payload.get("repository") or "")
        commit = str(payload.get("commit") or "")
        if not repository or not commit:
            raise ValueError(f"{pin_path} is missing repository or commit")
        name = pin_path.parent.name
        pins.append(
            Pin(
                name=name,
                kind="git",
                current=commit,
                upstream_url=f"https://github.com/{repository}",
                repository=repository,
                overlay=CAVEMAN_OVERLAY if name == "caveman" else "",
            )
        )
    return tuple(pins)


def render_suggestion(verdicts: Sequence[Verdict]) -> str:
    drifts = [verdict for verdict in verdicts if verdict.behind]
    if not drifts:
        return ""
    lines = [
        "Repository pins are behind the latest published stable upstream.",
        "",
        "One open issue tracks this drift. A later run updates this issue instead of opening another.",
        "Close this issue once the pins match. A run that finds nothing behind opens nothing.",
        "This check does not merge, and it does not install or upgrade tools.",
        "",
    ]
    for verdict in drifts:
        lines.extend(
            (
                f"### {verdict.name}",
                f"- Current pin: `{verdict.current}`",
                f"- Latest stable: `{verdict.latest}`",
                f"- Upstream: {verdict.upstream_url}",
            )
        )
        if verdict.overlay:
            lines.append(
                "- Re-apply the ChaosEngine project-mode overlay in "
                f"`{verdict.overlay}` after the pin refresh. "
                "Do not drop that overlay when replacing vendored files."
            )
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def suggest(verdicts: Sequence[Verdict], store: Any) -> str:
    """Open or update one issue. Return ``created``, ``updated``, or ``none``."""
    if not any(verdict.behind for verdict in verdicts):
        return "none"
    body = render_suggestion(verdicts)
    existing = store.open_by_title(SUGGESTION_TITLE)
    if existing is None:
        store.create(SUGGESTION_TITLE, body)
        return "created"
    store.update(existing, body)
    return "updated"


def compare_and_suggest(
    pins: Sequence[Pin],
    upstream: Mapping[str, Stable],
    store: Any = None,
) -> tuple[tuple[Verdict, ...], str]:
    verdicts = tuple(compare_pin(pin, upstream[pin.name]) for pin in pins)
    if store is None:
        return verdicts, "report"
    return verdicts, suggest(verdicts, store)


def _request_json(url: str, *, github: bool) -> tuple[int, Any]:
    headers = {"User-Agent": _USER_AGENT, "Accept": "application/json"}
    if github:
        headers["Accept"] = "application/vnd.github+json"
        token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
        if token:
            headers["Authorization"] = f"Bearer {token}"
    request = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(request, timeout=30) as response:  # nosec B310 - https URLs from pins.
            return response.status, json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        detail = error.read().decode("utf-8", errors="replace")[:500]
        if error.code == 404:
            return 404, None
        raise UpstreamError(f"{url} returned HTTP {error.code}: {detail}") from error
    except urllib.error.URLError as error:
        raise UpstreamError(f"{url} is unreachable: {error.reason}") from error
    except json.JSONDecodeError as error:
        raise UpstreamError(f"{url} did not return JSON: {error}") from error


def fetch_pypi(url: str) -> Stable:
    status, payload = _request_json(url, github=False)
    if status != 200 or not isinstance(payload, dict):
        raise UpstreamError(f"{url} returned HTTP {status}")
    return Stable(version=newest_stable_version(payload))


def fetch_git(repository: str) -> Stable:
    base = f"https://api.github.com/repos/{repository}"
    status, release = _request_json(f"{base}/releases/latest", github=True)
    if status == 200 and isinstance(release, dict) and release.get("tag_name"):
        tag = str(release["tag_name"])
        quoted = urllib.parse.quote(tag, safe="")
        commit_status, commit = _request_json(f"{base}/commits/{quoted}", github=True)
        if commit_status != 200 or not isinstance(commit, dict) or not commit.get("sha"):
            raise UpstreamError(f"{repository} release {tag} has no commit")
        return Stable(release_commit=str(commit["sha"]))
    if status == 404:
        repo_status, repo = _request_json(base, github=True)
        if repo_status != 200 or not isinstance(repo, dict) or not repo.get("default_branch"):
            raise UpstreamError(f"{repository} has no stable release and no default branch")
        branch = str(repo["default_branch"])
        quoted = urllib.parse.quote(branch, safe="")
        head_status, head = _request_json(f"{base}/commits/{quoted}", github=True)
        if head_status != 200 or not isinstance(head, dict) or not head.get("sha"):
            raise UpstreamError(f"{repository} default branch {branch} has no commit")
        return Stable(head_commit=str(head["sha"]))
    raise UpstreamError(f"{repository} releases/latest returned HTTP {status}")


def fetch_stable(pin: Pin) -> Stable:
    if pin.kind == "pypi":
        return fetch_pypi(pin.upstream_url)
    if pin.kind == "git":
        return fetch_git(pin.repository)
    raise UpstreamError(f"unknown pin kind for {pin.name}")


class GhIssueStore:
    """One open issue identified by the stable title."""

    def __init__(self, repo: str) -> None:
        self.repo = repo
        self._gh = shutil.which("gh") or "gh"

    def _run(self, args: list[str]) -> str:
        completed = subprocess.run(  # nosec B603 - fixed gh argument vector, never a shell.
            args,
            check=True,
            capture_output=True,
            text=True,
        )
        return completed.stdout

    def open_by_title(self, title: str) -> int | None:
        raw = self._run(
            [
                self._gh,
                "issue",
                "list",
                "--repo",
                self.repo,
                "--state",
                "open",
                "--limit",
                "50",
                "--json",
                "number,title",
                "--search",
                f"{title} in:title",
            ]
        )
        matches = [item for item in json.loads(raw or "[]") if item.get("title") == title]
        if not matches:
            return None
        return min(int(item["number"]) for item in matches)

    def create(self, title: str, body: str) -> int:
        raw = self._run(
            [
                self._gh,
                "issue",
                "create",
                "--repo",
                self.repo,
                "--title",
                title,
                "--body",
                body,
            ]
        )
        return int(raw.strip().rstrip("/").split("/")[-1])

    def update(self, number: int, body: str) -> None:
        self._run(
            [
                self._gh,
                "issue",
                "edit",
                str(number),
                "--repo",
                self.repo,
                "--body",
                body,
            ]
        )


def format_report(verdicts: Sequence[Verdict]) -> str:
    rows = [
        {
            "behind": verdict.behind,
            "current": verdict.current,
            "latest": verdict.latest,
            "name": verdict.name,
            "upstream": verdict.upstream_url,
        }
        for verdict in verdicts
    ]
    return json.dumps(rows, indent=2, sort_keys=True) + "\n"


def main(
    argv: list[str] | None = None,
    fetch: Callable[[Pin], Stable] = fetch_stable,
    store_factory: Callable[[str], Any] = GhIssueStore,
) -> int:
    parser = argparse.ArgumentParser(description="Compare repository pins to the latest stable upstream.")
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument(
        "--write",
        action="store_true",
        help="Open or update one GitHub issue. The default is report-only.",
    )
    parser.add_argument("--repo", default=os.environ.get("GITHUB_REPOSITORY", "ShaftHQ/SHAFT_ENGINE"))
    args = parser.parse_args(argv)
    try:
        pins = load_pins(args.root)
        upstream = {pin.name: fetch(pin) for pin in pins}
        store = store_factory(args.repo) if args.write else None
        verdicts, _action = compare_and_suggest(pins, upstream, store)
    except (UpstreamError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1
    sys.stdout.write(format_report(verdicts))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
