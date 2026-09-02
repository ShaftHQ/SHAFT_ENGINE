"""Print whether a revision range changes dependencies that need GitHub review."""

from __future__ import annotations

import re
import subprocess
import sys


DEPENDENCY_CONFIG = ".github/dependency-review-config.yml"
CI_PYTHON_REQUIREMENTS = "requirements-ci.txt"
DEPENDENCY_SUFFIXES = ("/pom.xml", ".pom.xml")
GRADLE_BUILD_SUFFIX = ".gradle.kts"
GRADLE_PROPERTIES_SUFFIX = "/gradle.properties"
PROPERTY_REF = re.compile(r"\$\{([A-Za-z0-9_.-]+)\}")


def git(*arguments: str) -> str:
    return subprocess.check_output(  # nosec B603 B607 - fixed read-only Git command.
        ["git", *arguments], text=True, encoding="utf-8"
    )


def content(revision: str, path: str) -> str | None:
    result = subprocess.run(  # nosec B603 B607 - fixed read-only Git command.
        ["git", "show", f"{revision}:{path}"], capture_output=True, text=True, encoding="utf-8"
    )
    return result.stdout if result.returncode == 0 else None


def maven_dependencies(source: str) -> tuple[str, ...]:
    return tuple(re.findall(r"<dependencies\b[^>]*>.*?</dependencies>", source, re.DOTALL))


def maven_properties(source: str) -> dict[str, str]:
    match = re.search(r"<properties\b[^>]*>(.*?)</properties>", source, re.DOTALL)
    if match is None:
        return {}
    return {
        name: value
        for name, value in re.findall(r"<([A-Za-z0-9_.-]+)>(.*?)</\1>", match.group(1), re.DOTALL)
        if name not in {"property", "properties"}
    }


def dependency_version_property_refs(source: str) -> frozenset[str]:
    """Return property names referenced from dependency or imported BOM versions."""
    refs: set[str] = set()
    for block_name in ("dependencyManagement", "dependencies"):
        for block in re.findall(rf"<{block_name}\b[^>]*>(.*?)</{block_name}>", source, re.DOTALL):
            for dependency in re.findall(r"<dependency\b[^>]*>.*?</dependency>", block, re.DOTALL):
                version = re.search(r"<version>(.*?)</version>", dependency, re.DOTALL)
                if version is None:
                    continue
                refs.update(PROPERTY_REF.findall(version.group(1)))
    return frozenset(refs)


def gradle_properties(source: str) -> dict[str, str]:
    return {
        key.strip(): value.strip()
        for line in source.splitlines()
        if "=" in line and not line.lstrip().startswith("#")
        for key, value in [line.split("=", 1)]
        if key.strip() != "pluginVersion"
    }


def needs_review(base: str, head: str) -> bool:
    changed = git("diff", "--name-only", base, head).splitlines()
    if DEPENDENCY_CONFIG in changed:
        return True
    for path in changed:
        if path == CI_PYTHON_REQUIREMENTS:
            return True
        before, after = content(base, path), content(head, path)
        if path == "pom.xml" or path.endswith(DEPENDENCY_SUFFIXES):
            if before is None or after is None:
                return True
            if maven_dependencies(before) != maven_dependencies(after):
                return True
            old_properties = maven_properties(before)
            new_properties = maven_properties(after)
            referenced = dependency_version_property_refs(before) | dependency_version_property_refs(
                after
            )
            for name in referenced:
                if old_properties.get(name) != new_properties.get(name):
                    return True
        elif path.endswith(GRADLE_BUILD_SUFFIX):
            return True
        elif path.endswith(GRADLE_PROPERTIES_SUFFIX):
            if before is None or after is None or gradle_properties(before) != gradle_properties(after):
                return True
    return False


def main(arguments: list[str]) -> int:
    if len(arguments) != 2:
        print("usage: dependency_review_changes.py <base> <head>", file=sys.stderr)
        return 2
    print(str(needs_review(*arguments)).lower())
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
