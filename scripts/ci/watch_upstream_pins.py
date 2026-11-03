#!/usr/bin/env python3
"""Compare repository pins to the latest stable upstream.

Reads ``chaos-engine/dependencies.json`` and every ``chaos-engine/vendor/*/PIN.json``.
The machine receipt ``.chaos-engine-dependencies.json`` is not a pin.

Latest for a PyPI or npm package is the newest non-yanked stable release.
Latest for uv, Node, and Temurin is the newest stable version on that
dependency's stable channel. Latest for a commit pin is the commit of the
newest GitHub release, or default-branch HEAD when that repository has no
stable release. A pin that already equals default-branch HEAD is current
even when HEAD is ahead of the newest release (#7432). Unreleased history
is not latest. The Python generation pin
and the project-runtime series are not one bump and are not watched here.
deja stays on its pinned release tag.

Report-only unless ``--write``. ``--write`` opens or updates one issue titled
``Upstream pin drift`` and does not merge or install anything.
"""

from __future__ import annotations

import argparse
import copy
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


def _version_key(version: str) -> tuple[tuple[int, ...], int]:
    """Dotted version first, then a numeric ``+build``.

    The build stays out of the dotted tuple so ``25+36`` cannot sort ahead of
    ``25.0.5+8``, while ``25.0.4+7`` is still behind ``25.0.4+8``.
    """
    core, separator, build = version.partition("+")
    if not _STABLE_VERSION.fullmatch(core):
        raise ValueError(f"not a stable version: {version}")
    numbers = tuple(int(part) for part in core.split("."))
    build_number = 0
    if separator:
        token = build.split("-", 1)[0]
        if token.isdigit():
            build_number = int(token)
    return numbers, build_number


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
    if pin.kind in _VERSION_KINDS:
        latest = stable.version
    elif stable.release_commit:
        latest = stable.release_commit
    else:
        latest = stable.head_commit
    if not latest:
        raise ValueError(f"{pin.name} has no stable upstream")
    return latest


_SERIES_KINDS = frozenset({"series-node", "series-java", "series-python"})
_VERSION_KINDS = frozenset(
    {"pypi", "npm", "github-tag", "node-index", "adoptium", "maven-central", "python-org"}
) | _SERIES_KINDS
_COMMIT_KINDS = frozenset({"git", "release"})


def compare_pin(pin: Pin, stable: Stable) -> Verdict:
    latest = _stable_identity(pin, stable)
    if pin.kind in _SERIES_KINDS:
        # A series pin ("25", "3.13") is behind when the newest stable series is newer.
        latest = ".".join(latest.split("+", 1)[0].split(".")[: len(pin.current.split("."))])
        behind = _version_key(pin.current) < _version_key(latest)
    elif pin.kind in _VERSION_KINDS:
        behind = _version_key(pin.current) < _version_key(latest)
    elif pin.kind in _COMMIT_KINDS:
        # #7432: a pin equal to the default-branch tip is current even when that
        # tip is ahead of the newest release commit (release remains the
        # reported latest for older pins).
        behind = pin.current != latest
        if (
            behind
            and stable.head_commit
            and pin.current == stable.head_commit
        ):
            behind = False
            latest = stable.head_commit
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
    dependencies = spec["dependencies"]
    pins: list[Pin] = []
    if not isinstance(dependencies, dict):
        raise ValueError("dependencies.json has no dependencies object")
    for name, contract in dependencies.items():
        pin = _dependency_pin(spec, str(name), contract)
        if pin is not None:
            pins.append(pin)
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
    pins.extend(runtime_pins(spec))
    pins.extend(repository_runtime_pins(root))
    require_full_coverage(spec, pins)
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


UV_ASSETS = {
    "windows-x64": "uv-x86_64-pc-windows-msvc.zip",
    "windows-arm64": "uv-aarch64-pc-windows-msvc.zip",
    "linux-x64": "uv-x86_64-unknown-linux-gnu.tar.gz",
    "linux-arm64": "uv-aarch64-unknown-linux-gnu.tar.gz",
    "macos-x64": "uv-x86_64-apple-darwin.tar.gz",
    "macos-arm64": "uv-aarch64-apple-darwin.tar.gz",
}


def uv_release_from_github_payload(payload: Mapping[str, Any]) -> dict[str, Any]:
    """
    Build one uv pin release from a GitHub release document.

    Every OS asset must carry a sha256 digest. A missing digest refuses the
    bump instead of leaving that platform on the previous checksum.
    """
    tag = str(payload.get("tag_name") or "").lstrip("v")
    assets = payload.get("assets")
    if not _STABLE_VERSION.fullmatch(tag) or not isinstance(assets, list):
        raise UpstreamError("uv release payload is not a stable release")
    digests: dict[str, str] = {}
    for asset in assets:
        if not isinstance(asset, dict):
            continue
        digest = str(asset.get("digest") or "")
        if digest.startswith("sha256:"):
            digests[str(asset.get("name") or "")] = digest.split(":", 1)[1]
    artifacts: dict[str, dict[str, str]] = {}
    for platform, filename in UV_ASSETS.items():
        digest = digests.get(filename)
        if digest is None or re.fullmatch(r"[0-9a-f]{64}", digest) is None:
            raise UpstreamError(f"uv release is missing a sha256 digest for {filename}")
        artifacts[platform] = {"sha256": digest}
    return {"component": "uv", "version": tag, "artifacts": artifacts}


def fetch_uv_release() -> dict[str, Any]:
    status, payload = _request_json(
        "https://api.github.com/repos/astral-sh/uv/releases/latest", github=True
    )
    if status != 200 or not isinstance(payload, dict):
        raise UpstreamError(f"uv release lookup failed: HTTP {status}")
    return uv_release_from_github_payload(payload)


_RUNTIME_KEYS = {"java": "temurin", "maven-tools-mcp": "maven-tools-source"}
_SKIP_DEPENDENCIES = frozenset({"python", "deja"})


def _package_version(package: str) -> str | None:
    if "==" in package:
        return package.split("==", 1)[1]
    if "@" in package:
        return package.rsplit("@", 1)[1]
    return None


def _pin_current(spec: Mapping[str, Any], name: str) -> str | None:
    tools = spec.get("tools") if isinstance(spec.get("tools"), dict) else {}
    tool = tools.get(name) if isinstance(tools, dict) else None
    package = tool.get("package") if isinstance(tool, dict) else None
    if isinstance(package, str):
        version = _package_version(package)
        if version:
            return version
    runtimes = spec.get("runtimes") if isinstance(spec.get("runtimes"), dict) else {}
    runtime = runtimes.get(_RUNTIME_KEYS.get(name, name)) if isinstance(runtimes, dict) else None
    version = runtime.get("version") if isinstance(runtime, dict) else None
    if isinstance(version, str) and version:
        return version
    return None


def _github_repository(url: str) -> str:
    marker = "api.github.com/repos/"
    if marker not in url:
        return ""
    parts = url.split(marker, 1)[1].split("/")
    if len(parts) < 2:
        return ""
    return f"{parts[0]}/{parts[1]}"


def _channel_kind(url: str, current: str) -> str | None:
    if len(current) == 40 and all(character in "0123456789abcdef" for character in current):
        return "release"
    if "pypi.org" in url:
        return "pypi"
    if "registry.npmjs.org" in url:
        return "npm"
    if "nodejs.org" in url:
        return "node-index"
    if "adoptium.net" in url:
        return "adoptium"
    if "api.github.com" in url and "/releases/latest" in url:
        return "github-tag"
    return None


def _dependency_pin(spec: Mapping[str, Any], name: str, contract: object) -> Pin | None:
    """One watched pin, or None when this dependency is not a published version pin."""
    if name in _SKIP_DEPENDENCIES or not isinstance(contract, dict):
        return None
    url = str(contract.get("stableChannel") or "")
    if not url or "/releases/tags/" in url:
        return None
    current = _pin_current(spec, name)
    if not current:
        return None
    kind = _channel_kind(url, current)
    if kind is None:
        return None
    if kind in _VERSION_KINDS and _STABLE_VERSION.fullmatch(current.split("+", 1)[0]) is None:
        return None
    return Pin(
        name=name,
        kind=kind,
        current=current,
        upstream_url=url,
        repository=_github_repository(url) if kind == "release" else "",
    )


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


def _stable_version_payload(url: str, version: str) -> Stable:
    cleaned = version.strip().lstrip("v")
    if _STABLE_VERSION.fullmatch(cleaned.split("+", 1)[0]) is None:
        raise UpstreamError(f"{url} has no stable version")
    return Stable(version=cleaned)


def fetch_npm(url: str) -> Stable:
    status, payload = _request_json(url, github=False)
    if status != 200 or not isinstance(payload, dict):
        raise UpstreamError(f"{url} returned HTTP {status}")
    return _stable_version_payload(url, str(payload.get("version") or ""))


def fetch_github_tag(url: str) -> Stable:
    status, payload = _request_json(url, github=True)
    if status != 200 or not isinstance(payload, dict):
        raise UpstreamError(f"{url} returned HTTP {status}")
    return _stable_version_payload(url, str(payload.get("tag_name") or ""))


def newest_node_version(payload: object) -> str:
    """Newest Node LTS. A Current (non-LTS) line is not the installer pin."""
    if not isinstance(payload, list):
        raise UpstreamError("node index is not a list")
    versions = [
        str(item.get("version") or "").lstrip("v")
        for item in payload
        if isinstance(item, dict)
        and item.get("lts") not in (False, None, "")
        and _STABLE_VERSION.fullmatch(str(item.get("version") or "").lstrip("v"))
    ]
    if not versions:
        raise UpstreamError("node index has no stable LTS version")
    return max(versions, key=_version_key)


def fetch_node_index(url: str) -> Stable:
    status, payload = _request_json(url, github=False)
    if status != 200:
        raise UpstreamError(f"{url} returned HTTP {status}")
    try:
        version = newest_node_version(payload)
    except UpstreamError as error:
        raise UpstreamError(f"{url} has no stable Node version") from error
    return Stable(version=version)


def _adoptium_feature(url: str) -> int | None:
    """The feature line named in the channel, such as ``feature_version=25``."""
    raw = urllib.parse.parse_qs(urllib.parse.urlparse(url).query).get("feature_version", [""])[0]
    return int(raw) if str(raw).isdigit() else None


def _adoptium_version(item: Mapping[str, Any]) -> str | None:
    raw = str(item.get("openjdk_version") or item.get("semver") or "").strip().lstrip("v")
    if raw.endswith("-LTS"):
        raw = raw[: -len("-LTS")]
    if _STABLE_VERSION.fullmatch(raw.split("+", 1)[0]) is None:
        return None
    return raw


def newest_adoptium_version(payload: Mapping[str, Any], *, feature: int | None) -> str:
    """Newest GA on the requested feature line. A mixed page must not jump lines."""
    found: list[str] = []
    for item in payload.get("versions") or []:
        if not isinstance(item, dict):
            continue
        version = _adoptium_version(item)
        if version is None:
            continue
        if feature is not None and item.get("major") != feature:
            continue
        found.append(version)
    if not found:
        raise UpstreamError("adoptium payload has no stable Temurin version")
    return max(found, key=_version_key)


def fetch_adoptium(url: str) -> Stable:
    status, payload = _request_json(url, github=False)
    if status != 200 or not isinstance(payload, dict):
        raise UpstreamError(f"{url} returned HTTP {status}")
    try:
        version = newest_adoptium_version(payload, feature=_adoptium_feature(url))
    except UpstreamError as error:
        raise UpstreamError(f"{url} has no stable Temurin version") from error
    return Stable(version=version)


def fetch_stable(pin: Pin) -> Stable:
    if pin.kind == "pypi":
        return fetch_pypi(pin.upstream_url)
    if pin.kind == "npm":
        return fetch_npm(pin.upstream_url)
    if pin.kind == "github-tag":
        return fetch_github_tag(pin.upstream_url)
    if pin.kind == "node-index":
        return fetch_node_index(pin.upstream_url)
    if pin.kind == "adoptium":
        return fetch_adoptium(pin.upstream_url)
    if pin.kind in _COMMIT_KINDS:
        return fetch_git(pin.repository)
    if pin.kind in _EXTRA_FETCHERS:
        return _EXTRA_FETCHERS[pin.kind](pin.upstream_url)
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


def _replace_package_version(package: str, version: str) -> str:
    if "==" in package:
        name, _old = package.split("==", 1)
        return f"{name}=={version}"
    if package.startswith("@") and package.count("@") >= 2:
        name, _old = package.rsplit("@", 1)
        return f"{name}@{version}"
    if "@" in package:
        name, _old = package.rsplit("@", 1)
        return f"{name}@{version}"
    raise ValueError(f"package pin is not name==version or name@version: {package}")


def _linked_component(release: Mapping[str, Any]) -> tuple[str, str]:
    component = str(release.get("component") or "")
    version = str(release.get("version") or "")
    if not component or not _STABLE_VERSION.fullmatch(version):
        raise ValueError("stable release is missing a component or a stable version")
    if component == "python":
        raise ValueError("the Python generation pin and the project-runtime pin are not one bump")
    return component, version


def _linked_tool(spec: Mapping[str, Any], component: str) -> dict[str, Any]:
    tools = spec.get("tools")
    if not isinstance(tools, dict) or component not in tools:
        raise ValueError(f"{component} has no tool pin to move")
    tool = tools[component]
    if not isinstance(tool, dict) or not isinstance(tool.get("package"), str):
        raise ValueError(f"{component} tool pin has no package")
    return tool


def _linked_runtime(spec: Mapping[str, Any], component: str) -> tuple[dict[str, Any], dict[str, Any], str]:
    runtimes = spec.get("runtimes")
    if not isinstance(runtimes, dict) or component not in runtimes:
        raise ValueError(f"{component} has no runtime pin to move")
    runtime = runtimes[component]
    if not isinstance(runtime, dict):
        raise ValueError(f"{component} runtime pin is invalid")
    artifacts = runtime.get("artifacts")
    if not isinstance(artifacts, dict) or not artifacts:
        raise ValueError(f"{component} has no per-OS artifacts")
    old_version = str(runtime.get("version") or "")
    if not old_version:
        raise ValueError(f"{component} runtime pin has no version")
    return runtime, artifacts, old_version


def _same_artifact_set(component: str, artifacts: Mapping[str, Any], provided: object) -> Mapping[str, Any]:
    if not isinstance(provided, Mapping) or set(provided) != set(artifacts):
        raise ValueError(f"{component} stable release must name every OS artifact")
    return provided


def _refuse_downgrade(component: str, version: str, old_version: str) -> None:
    if _version_key(version) < _version_key(old_version):
        raise ValueError(f"{component} stable {version} is older than pin {old_version}")


def _rewrite_one_artifact(
    component: str,
    platform: str,
    row: object,
    old_version: str,
    version: str,
    incoming: object,
) -> str:
    if not isinstance(row, dict) or not isinstance(row.get("url"), str):
        raise ValueError(f"{component} artifact is invalid: {platform}")
    digest = incoming.get("sha256") if isinstance(incoming, Mapping) else None
    if not isinstance(digest, str) or re.fullmatch(r"[0-9a-f]{64}", digest) is None:
        raise ValueError(f"{component} artifact checksum is invalid: {platform}")
    if old_version not in str(row["url"]):
        raise ValueError(f"{component} artifact URL is not on {old_version}: {platform}")
    row["url"] = str(row["url"]).replace(old_version, version)
    row["sha256"] = digest
    if version not in row["url"]:
        raise ValueError(f"{component} artifact URL missed {version}: {platform}")
    return digest


def _old_digest(row: Mapping[str, Any]) -> str:
    return str(row.get("sha256") or "")


def _stamp_digest(wrappers: dict[str, str], old_digest: str, digest: str) -> None:
    if not old_digest:
        return
    for label, text in list(wrappers.items()):
        if old_digest in text:
            wrappers[label] = text.replace(old_digest, digest)


def _stamp_version(wrappers: dict[str, str], old_version: str, version: str) -> None:
    for label, text in list(wrappers.items()):
        if old_version not in text:
            raise ValueError(f"installer provisioning pin {label} does not contain {old_version}")
        wrappers[label] = text.replace(old_version, version)


def apply_linked_pin(
    spec: Mapping[str, Any],
    wrappers: Mapping[str, str],
    release: Mapping[str, Any],
) -> tuple[dict[str, Any], dict[str, str]]:
    """
    Move every consumer of one component pin onto ``release`` together.

    The generation Python series (``runtimes.python``) and the project-runtime
    Python series (``dependencies.python``) are not this pin and are left as
    they are. A release that omits an OS artifact is rejected so no platform
    keeps the previous checksum.
    """
    component, version = _linked_component(release)
    updated = copy.deepcopy(dict(spec))
    tool = _linked_tool(updated, component)
    runtime, artifacts, old_version = _linked_runtime(updated, component)
    provided = _same_artifact_set(component, artifacts, release.get("artifacts"))
    _refuse_downgrade(component, version, old_version)
    next_wrappers = dict(wrappers)
    if _version_key(version) == _version_key(old_version):
        return updated, next_wrappers
    tool["package"] = _replace_package_version(str(tool["package"]), version)
    runtime["version"] = version
    for platform, row in artifacts.items():
        previous = _old_digest(row)
        _rewrite_one_artifact(component, platform, row, old_version, version, provided[platform])
        _stamp_digest(next_wrappers, previous, str(row["sha256"]))
    _stamp_version(next_wrappers, old_version, version)
    return updated, next_wrappers


def unlinked_pin_error(
    spec: Mapping[str, Any],
    wrappers: Mapping[str, str],
    component: str,
) -> str | None:
    """Reject a bump that moved only one consumer of ``component``."""
    tools = spec.get("tools") if isinstance(spec.get("tools"), dict) else {}
    runtimes = spec.get("runtimes") if isinstance(spec.get("runtimes"), dict) else {}
    tool = tools.get(component) if isinstance(tools, dict) else None
    runtime = runtimes.get(component) if isinstance(runtimes, dict) else None
    package = tool.get("package") if isinstance(tool, dict) else None
    if not isinstance(package, str) or "==" not in package:
        return f"{component} tool pin is missing"
    tool_version = package.split("==", 1)[1]
    runtime_version = runtime.get("version") if isinstance(runtime, dict) else None
    if tool_version != runtime_version:
        return f"{component} tool pin {tool_version} != runtime pin {runtime_version}"
    artifacts = runtime.get("artifacts") if isinstance(runtime, dict) else None
    if not isinstance(artifacts, dict) or not artifacts:
        return f"{component} has no per-OS artifacts"
    for platform, row in artifacts.items():
        url = row.get("url") if isinstance(row, dict) else None
        digest = row.get("sha256") if isinstance(row, dict) else None
        if not isinstance(url, str) or runtime_version not in url:
            return f"{platform} artifact is not on {runtime_version}"
        if not isinstance(digest, str) or not any(digest in text for text in wrappers.values()):
            return f"{platform} checksum is absent from the installer provisioning pins"
    for label, text in wrappers.items():
        if not isinstance(text, str) or runtime_version not in text:
            return f"installer provisioning pin {label} is not {runtime_version}"
    generation = runtimes.get("python") if isinstance(runtimes, dict) else None
    project = spec.get("dependencies")
    project_python = project.get("python") if isinstance(project, dict) else None
    generation_pin = generation.get("version") if isinstance(generation, dict) else None
    project_pin = project_python.get("maximumSeries") if isinstance(project_python, dict) else None
    if isinstance(generation_pin, str) and isinstance(project_pin, str) and generation_pin == project_pin:
        return "generation Python pin collapsed into the project-runtime pin"
    return None


def _consumer_fingerprint(
    spec: Mapping[str, Any], wrappers: Mapping[str, str], component: str
) -> tuple[object, ...]:
    """Version and checksum identity. Formatting of the spec file is not part of it."""
    tools = spec.get("tools") if isinstance(spec.get("tools"), dict) else {}
    runtimes = spec.get("runtimes") if isinstance(spec.get("runtimes"), dict) else {}
    tool = tools.get(component) if isinstance(tools, dict) else None
    runtime = runtimes.get(component) if isinstance(runtimes, dict) else None
    package = tool.get("package") if isinstance(tool, dict) else None
    version = runtime.get("version") if isinstance(runtime, dict) else None
    artifacts = runtime.get("artifacts") if isinstance(runtime, dict) else None
    digests: list[tuple[str, object, object]] = []
    if isinstance(artifacts, dict):
        for platform in sorted(artifacts):
            row = artifacts[platform]
            if isinstance(row, dict):
                digests.append((platform, row.get("url"), row.get("sha256")))
    texts = tuple(sorted((label, text) for label, text in wrappers.items()))
    return package, version, tuple(digests), texts


def _changed_strings(before: Any, after: Any) -> list[tuple[str, str]]:
    if isinstance(before, str) and isinstance(after, str):
        return [] if before == after else [(before, after)]
    if isinstance(before, dict) and isinstance(after, dict) and before.keys() == after.keys():
        return [pair for key in before for pair in _changed_strings(before[key], after[key])]
    if isinstance(before, list) and isinstance(after, list) and len(before) == len(after):
        return [pair for old, new in zip(before, after) for pair in _changed_strings(old, new)]
    if before == after:
        return []
    raise ValueError("structural change")


def preserve_format(text: str, before: Any, after: Any) -> str:
    """#7205: edit only the changed pin strings, keeping the file's layout; else pretty-print."""
    try:
        rewritten = text
        for old, new in dict.fromkeys(_changed_strings(before, after)):
            quoted = json.dumps(old)
            if quoted not in rewritten:
                raise ValueError(old)
            rewritten = rewritten.replace(quoted, json.dumps(new))
        if json.loads(rewritten) == after:
            return rewritten
    except ValueError:
        # In-place rewrite impossible; fall back to a full reformat below.
        pass
    return json.dumps(after, indent=2) + "\n"


def apply_release_files(root: Path, release: Mapping[str, Any]) -> None:
    """Rewrite the linked pin files under ``root`` or reject a one-sided result."""
    spec_path = root / "chaos-engine" / "dependencies.json"
    spec = json.loads(spec_path.read_text(encoding="utf-8"))
    wrapper_paths = {
        "install.ps1": root / "chaos-engine" / "install.ps1",
        "install.sh": root / "chaos-engine" / "install.sh",
    }
    wrappers = {
        label: path.read_text(encoding="utf-8")
        for label, path in wrapper_paths.items()
    }
    component = str(release["component"])
    updated, rewritten = apply_linked_pin(spec, wrappers, release)
    error = unlinked_pin_error(updated, rewritten, component)
    if error:
        raise ValueError(error)
    if _consumer_fingerprint(spec, wrappers, component) == _consumer_fingerprint(
        updated, rewritten, component
    ):
        return
    spec_path.write_text(preserve_format(spec_path.read_text(encoding="utf-8"), spec, updated), encoding="utf-8")
    for label, path in wrapper_paths.items():
        path.write_text(rewritten[label], encoding="utf-8")


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


# ---------------------------------------------------------------------------
# Full coverage (#7738): every pin is watched against the latest stable
# upstream, or carries an explicit exemption reason. A new pin that is neither
# makes load_pins fail, so drift can never go silent.
# ---------------------------------------------------------------------------

NODE_INDEX_URL = "https://nodejs.org/dist/index.json"
ADOPTIUM_RELEASES_URL = "https://api.adoptium.net/v3/info/available_releases"
PYTHON_RELEASES_URL = "https://www.python.org/api/v2/downloads/release/?is_published=true"
MAVEN_METADATA_URL = (
    "https://repo.maven.apache.org/maven2/org/apache/maven/apache-maven/maven-metadata.xml"
)

# Pins that intentionally have no version to watch. The reason is required.
COVERAGE_EXEMPT: dict[str, str] = {
    "dependencies.context7": "no version pin; installs the registry latest tag",
    "dependencies.chrome-devtools-mcp": "no version pin; npx resolves the registry latest tag",
    "dependencies.deja": "watched through runtimes.deja",
    "dependencies.python": "watched through runtimes.python",
    "runtimes.maven-tools-source": "watched through dependencies.maven-tools-mcp",
    "runtimes.temurin": "watched through dependencies.java and java-feature",
}

_SETUP_KEYS = {"node-version": "node", "java-version": "java", "python-version": "python"}
_SETUP_LINE = re.compile(r"^\s*(node-version|java-version|python-version)\s*:\s*['\"]?([^'\"\s#]+)")
_POM_JDK = re.compile(r"<jdk\.version>\s*(\d+)\s*</jdk\.version>")
_SERIES_URL = {
    "node": ("series-node", NODE_INDEX_URL),
    "java": ("series-java", ADOPTIUM_RELEASES_URL),
    "python": ("series-python", PYTHON_RELEASES_URL),
}


def runtime_pins(spec: Mapping[str, Any]) -> list[Pin]:
    """Runtimes the dependency contract does not already watch."""
    runtimes = spec.get("runtimes") if isinstance(spec.get("runtimes"), dict) else {}
    pins: list[Pin] = []
    python = runtimes.get("python") if isinstance(runtimes, dict) else None
    if isinstance(python, dict) and _STABLE_VERSION.fullmatch(str(python.get("version") or "")):
        pins.append(Pin("python", "series-python", str(python["version"]), PYTHON_RELEASES_URL))
    maven = runtimes.get("maven") if isinstance(runtimes, dict) else None
    if isinstance(maven, dict) and maven.get("version"):
        pins.append(Pin("maven", "maven-central", str(maven["version"]), MAVEN_METADATA_URL))
    deja = runtimes.get("deja") if isinstance(runtimes, dict) else None
    if isinstance(deja, dict) and deja.get("version"):
        url = next(iter((deja.get("artifacts") or {}).values()), {}).get("url", "")
        match = re.match(r"https://github\.com/([^/]+/[^/]+)/releases/", str(url))
        if match:
            latest = f"https://api.github.com/repos/{match.group(1)}/releases/latest"
            pins.append(Pin("deja", "github-tag", str(deja["version"]), latest))
    temurin = runtimes.get("temurin") if isinstance(runtimes, dict) else None
    if isinstance(temurin, dict) and temurin.get("version"):
        feature = str(temurin["version"]).split(".", 1)[0].split("+", 1)[0]
        pins.append(Pin("java-feature", "series-java", feature, ADOPTIUM_RELEASES_URL))
    return pins


def repository_runtime_pins(root: Path) -> list[Pin]:
    """Literal runtime versions in CI workflows and the build, one pin per distinct value."""
    found: dict[tuple[str, str], set[str]] = {}
    workflows = root / ".github" / "workflows"
    for path in sorted(workflows.glob("*.y*ml")) if workflows.is_dir() else ():
        for line in path.read_text(encoding="utf-8").splitlines():
            match = _SETUP_LINE.match(line)
            if not match or not _STABLE_VERSION.fullmatch(match.group(2)):
                continue  # expressions and aliases such as lts/* track upstream already
            runtime = _SETUP_KEYS[match.group(1)]
            found.setdefault((runtime, match.group(2)), set()).add(path.relative_to(root).as_posix())
    pom = root / "pom.xml"
    if pom.is_file():
        for match in _POM_JDK.finditer(pom.read_text(encoding="utf-8")):
            found.setdefault(("java", match.group(1)), set()).add("pom.xml")
    pins: list[Pin] = []
    for (runtime, value), files in sorted(found.items()):
        kind, url = _SERIES_URL[runtime]
        name = f"ci-{runtime}@{value} ({', '.join(sorted(files))})"
        pins.append(Pin(name, kind, value, url))
    return pins


def require_full_coverage(spec: Mapping[str, Any], pins: Sequence[Pin]) -> None:
    watched = {pin.name for pin in pins}
    alias = {"temurin": "java", "maven-tools-source": "maven-tools-mcp"}
    missing: list[str] = []
    for section in ("dependencies", "runtimes"):
        entries = spec.get(section) if isinstance(spec.get(section), dict) else {}
        for name in entries:
            key = f"{section}.{name}"
            if key in COVERAGE_EXEMPT:
                continue
            if name in watched or alias.get(name) in watched:
                continue
            missing.append(key)
    if missing:
        raise ValueError(
            "unwatched pins (watch them or add a COVERAGE_EXEMPT reason): " + ", ".join(sorted(missing))
        )


def newest_python_version(payload: object) -> str:
    if not isinstance(payload, list):
        raise UpstreamError("python.org release list is not a list")
    versions = []
    for item in payload:
        if not isinstance(item, dict) or item.get("pre_release") or not item.get("is_published", True):
            continue
        raw = str(item.get("name") or "").removeprefix("Python ").strip()
        if raw.startswith("3.") and _STABLE_VERSION.fullmatch(raw):
            versions.append(raw)
    if not versions:
        raise UpstreamError("python.org has no stable release")
    return max(versions, key=_version_key)


def fetch_python_org(url: str) -> Stable:
    status, payload = _request_json(url, github=False)
    if status != 200:
        raise UpstreamError(f"{url} returned HTTP {status}")
    return Stable(version=newest_python_version(payload))


def fetch_newest_node(url: str) -> Stable:
    """Newest stable Node line, LTS or Current: a series pin targets the newest major."""
    status, payload = _request_json(url, github=False)
    if status != 200 or not isinstance(payload, list):
        raise UpstreamError(f"{url} returned HTTP {status}")
    versions = [
        str(item.get("version") or "").lstrip("v")
        for item in payload
        if isinstance(item, dict) and _STABLE_VERSION.fullmatch(str(item.get("version") or "").lstrip("v"))
    ]
    if not versions:
        raise UpstreamError(f"{url} has no stable Node version")
    return Stable(version=max(versions, key=_version_key))


def fetch_java_feature(url: str) -> Stable:
    status, payload = _request_json(url, github=False)
    feature = payload.get("most_recent_feature_release") if isinstance(payload, dict) else None
    if status != 200 or not isinstance(feature, int):
        raise UpstreamError(f"{url} returned HTTP {status}")
    return Stable(version=str(feature))


def newest_maven_version(xml: str) -> str:
    versions = [v for v in re.findall(r"<version>([^<]+)</version>", xml) if _STABLE_VERSION.fullmatch(v)]
    if not versions:
        raise UpstreamError("maven metadata has no stable version")
    return max(versions, key=_version_key)


def fetch_maven_central(url: str) -> Stable:
    request = urllib.request.Request(url, headers={"User-Agent": _USER_AGENT})
    try:
        with urllib.request.urlopen(request, timeout=30) as response:  # noqa: S310 - fixed https URL
            return Stable(version=newest_maven_version(response.read().decode("utf-8")))
    except OSError as error:
        raise UpstreamError(f"{url} could not be read: {error}") from error


_EXTRA_FETCHERS: dict[str, Callable[[str], Stable]] = {
    "python-org": fetch_python_org,
    "series-python": fetch_python_org,
    "series-node": fetch_newest_node,
    "series-java": fetch_java_feature,
    "maven-central": fetch_maven_central,
}


def main(
    argv: list[str] | None = None,
    fetch: Callable[[Pin], Stable] = fetch_stable,
    store_factory: Callable[[str], Any] = GhIssueStore,
    release_for: Callable[[str], Mapping[str, Any] | None] | None = None,
) -> int:
    parser = argparse.ArgumentParser(description="Compare repository pins to the latest stable upstream.")
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument(
        "--write",
        action="store_true",
        help="Open or update one GitHub issue. The default is report-only.",
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Move every linked consumer of a published stable release together.",
    )
    parser.add_argument("--repo", default=os.environ.get("GITHUB_REPOSITORY", "ShaftHQ/SHAFT_ENGINE"))
    args = parser.parse_args(argv)
    try:
        pins = load_pins(args.root)
        upstream = {pin.name: fetch(pin) for pin in pins}
        store = store_factory(args.repo) if args.write else None
        verdicts, _action = compare_and_suggest(pins, upstream, store)
        if args.apply:
            source = release_for or (lambda name: fetch_uv_release() if name == "uv" else None)
            for component in ("uv",):
                release = source(component)
                if release:
                    apply_release_files(args.root, release)
    except (UpstreamError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1
    sys.stdout.write(format_report(verdicts))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
