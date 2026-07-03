#!/usr/bin/env python3
"""Prepare deterministic SHAFT release version updates for a release PR."""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from urllib.parse import quote
from urllib.request import Request, urlopen

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
RELEASE_VERSION = re.compile(r"(?P<major>\d+)\.(?P<quarter>[1-4])\.(?P<date>\d{8})")
SHAFT_VERSION_PROPERTY = re.compile(r"(<shaft\.version>)[^<]+(</shaft\.version>)")
SEMVER = re.compile(r"^(\d+)\.(\d+)\.(\d+)$")
UNSTABLE_VERSION = re.compile(r"(?i)(alpha|beta|rc|cr|m[0-9]+|ea|preview|milestone|snapshot)")
ANDROID_COMMAND_LINE_TOOLS = re.compile(r"commandlinetools-linux-(\d+)_latest\.zip")
PLUGIN_VERSION = re.compile(r"^pluginVersion=.*$", re.MULTILINE)
NPM_INTERNAL_VERSIONS = {
    "allure3Version": ("allure", 3),
    "appiumServerVersion": ("appium", None),
    "appiumInspectorPluginVersion": ("appium-inspector-plugin", None),
    "appiumUiAutomator2DriverVersion": ("appium-uiautomator2-driver", None),
    "appiumXcuitestDriverVersion": ("appium-xcuitest-driver", None),
}
EXTRA_VERSION_REFERENCES = (
    Path("modular-era-feature-catalog.md"),
    Path("shaft-intellij/src/test/java/com/shaft/intellij/ui/ShaftPluginScreenshotRendererTest.java"),
)


def _text(element: ET.Element, path: str) -> str:
    return (element.findtext(path, default="", namespaces=NS) or "").strip()


def root_version(root: Path = ROOT) -> str:
    version = _text(ET.parse(root / "pom.xml").getroot(), "m:version")
    if not RELEASE_VERSION.fullmatch(version):
        raise ValueError(f"root version is not a dated SHAFT release: {version!r}")
    return version


def target_version(current_version: str, release_date: str) -> str:
    current = RELEASE_VERSION.fullmatch(current_version)
    if not current:
        raise ValueError(f"current version is not a dated SHAFT release: {current_version!r}")
    date = dt.date.fromisoformat(release_date)
    quarter = ((date.month - 1) // 3) + 1
    return f"{current.group('major')}.{quarter}.{date:%Y%m%d}"


def _version_key(version: str) -> tuple[int, int, int]:
    match = RELEASE_VERSION.fullmatch(version)
    if not match:
        raise ValueError(f"version is not a dated SHAFT release: {version!r}")
    return int(match.group("major")), int(match.group("quarter")), int(match.group("date"))


def _semver_key(version: str) -> tuple[int, int, int]:
    match = SEMVER.fullmatch(version)
    if not match:
        raise ValueError(f"version is not stable semver: {version!r}")
    return tuple(map(int, match.groups()))


def _is_stable_semver(version: object) -> bool:
    return isinstance(version, str) and SEMVER.fullmatch(version) is not None and not UNSTABLE_VERSION.search(version)


def _json_url(url: str) -> object:
    request = Request(url, headers={"User-Agent": "ShaftHQ/SHAFT_ENGINE release-pr"})
    with urlopen(request, timeout=30) as response:
        return json.loads(response.read().decode("utf-8"))


def latest_stable_npm_version(package: str, major: int | None = None) -> str:
    metadata = _json_url(f"https://registry.npmjs.org/{quote(package, safe='')}")
    if not isinstance(metadata, dict):
        raise ValueError(f"{package}: npm metadata was not an object")
    versions = {
        version
        for version in metadata.get("versions", {})
        if _is_stable_semver(version) and (major is None or _semver_key(version)[0] == major)
    }
    latest = metadata.get("dist-tags", {}).get("latest")
    if _is_stable_semver(latest) and (major is None or _semver_key(latest)[0] == major):
        versions.add(latest)
    if not versions:
        raise ValueError(f"{package}: no stable npm versions found")
    return max(versions, key=_semver_key)


def latest_node_lts_version() -> str:
    releases = _json_url("https://nodejs.org/dist/index.json")
    if not isinstance(releases, list):
        raise ValueError("Node.js release index was not a list")
    versions = {
        release.get("version", "").removeprefix("v")
        for release in releases
        if isinstance(release, dict) and release.get("lts")
    }
    stable_versions = {version for version in versions if _is_stable_semver(version)}
    if not stable_versions:
        raise ValueError("Node.js release index has no LTS semver versions")
    return max(stable_versions, key=_semver_key)


def _local_name(element: ET.Element) -> str:
    return element.tag.rsplit("}", 1)[-1]


def latest_android_command_line_tools_version() -> str:
    request = Request(
        "https://dl.google.com/android/repository/repository2-1.xml",
        headers={"User-Agent": "ShaftHQ/SHAFT_ENGINE release-pr"},
    )
    with urlopen(request, timeout=30) as response:
        repository = ET.fromstring(response.read())
    for package in repository.iter():
        if _local_name(package) != "remotePackage" or package.attrib.get("path") != "cmdline-tools;latest":
            continue
        for child in package.iter():
            if _local_name(child) == "url" and child.text:
                match = ANDROID_COMMAND_LINE_TOOLS.fullmatch(child.text.strip())
                if match:
                    return match.group(1)
    raise ValueError("Android command-line tools latest Linux archive was not found")


def latest_internal_versions() -> dict[str, str]:
    versions = {
        method: latest_stable_npm_version(package, major)
        for method, (package, major) in NPM_INTERNAL_VERSIONS.items()
    }
    versions["nodeLtsVersion"] = latest_node_lts_version()
    versions["androidCommandLineToolsVersion"] = latest_android_command_line_tools_version()
    return versions


def _internal_default_pattern(method: str) -> re.Pattern[str]:
    return re.compile(
        r'(@DefaultValue\(")[^"]+("\)\s+(?:String|int)\s+' + re.escape(method) + r"\(\);)"
    )


def _replace(path: Path, pattern: re.Pattern[str], replacement: str) -> bool:
    source = path.read_text(encoding="utf-8")
    updated, count = pattern.subn(replacement, source)
    if count == 0:
        raise ValueError(f"{path}: expected release version target was not found")
    if updated == source:
        return False
    path.write_text(updated, encoding="utf-8")
    return True


def _replace_literal(path: Path, old: str, new: str) -> bool:
    source = path.read_text(encoding="utf-8")
    updated = source.replace(old, new)
    if updated == source:
        return False
    path.write_text(updated, encoding="utf-8")
    return True


def _module_poms(root: Path) -> list[Path]:
    parent = ET.parse(root / "pom.xml").getroot()
    return [
        root / _text(module, ".") / "pom.xml"
        for module in parent.findall("m:modules/m:module", NS)
    ]


def _shaft_version_poms(root: Path) -> list[Path]:
    return sorted(
        path
        for path in root.rglob("pom.xml")
        if "target" not in path.parts
        and "<shaft.version>" in path.read_text(encoding="utf-8")
    )


def prepare_release(
    root: Path,
    release_date: str,
    internal_versions: dict[str, str] | None = None,
) -> tuple[str, str, list[Path]]:
    current_version = root_version(root)
    release_version = target_version(current_version, release_date)
    if _version_key(release_version) <= _version_key(current_version):
        raise ValueError(f"target version {release_version} must be newer than {current_version}")

    changed: list[Path] = []
    files = [
        root / "pom.xml",
        *_module_poms(root),
    ]
    for path in files:
        if _replace_literal(path, current_version, release_version):
            changed.append(path)

    for path in _shaft_version_poms(root):
        if _replace(path, SHAFT_VERSION_PROPERTY, rf"\g<1>{release_version}\g<2>"):
            changed.append(path)

    internal = root / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java"
    if _replace(internal, _internal_default_pattern("shaftEngineVersion"), rf"\g<1>{release_version}\g<2>"):
        changed.append(internal)
    for method, version in (internal_versions or latest_internal_versions()).items():
        if _replace(internal, _internal_default_pattern(method), rf"\g<1>{version}\g<2>"):
            changed.append(internal)

    intellij_properties = root / "shaft-intellij/gradle.properties"
    if _replace(intellij_properties, PLUGIN_VERSION, f"pluginVersion={release_version}"):
        changed.append(intellij_properties)

    for relative in EXTRA_VERSION_REFERENCES:
        path = root / relative
        if path.is_file() and _replace_literal(path, current_version, release_version):
            changed.append(path)

    return current_version, release_version, sorted(set(changed))


def _write_github_output(path: Path, old_version: str, release_version: str) -> None:
    with path.open("a", encoding="utf-8") as output:
        output.write(f"old_version={old_version}\n")
        output.write(f"release_version={release_version}\n")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--release-date", required=True, help="Release date in YYYY-MM-DD format")
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--github-output", type=Path)
    args = parser.parse_args()

    try:
        old_version, release_version, changed = prepare_release(args.root, args.release_date)
    except ValueError as error:
        print(error, file=sys.stderr)
        return 1

    if args.github_output:
        _write_github_output(args.github_output, old_version, release_version)

    print(f"old_version={old_version}")
    print(f"release_version={release_version}")
    for path in changed:
        print(f"updated={path.relative_to(args.root).as_posix()}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
