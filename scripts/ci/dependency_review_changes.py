"""Print whether a revision range changes dependencies that need GitHub review."""

from __future__ import annotations

import subprocess
import sys
import xml.etree.ElementTree as ET


DEPENDENCY_CONFIG = ".github/dependency-review-config.yml"
DEPENDENCY_SUFFIXES = ("/pom.xml", ".pom.xml")
GRADLE_BUILD_SUFFIX = ".gradle.kts"
GRADLE_PROPERTIES_SUFFIX = "/gradle.properties"


def git(*arguments: str) -> str:
    return subprocess.check_output(["git", *arguments], text=True, encoding="utf-8")


def content(revision: str, path: str) -> str | None:
    result = subprocess.run(
        ["git", "show", f"{revision}:{path}"], capture_output=True, text=True, encoding="utf-8"
    )
    return result.stdout if result.returncode == 0 else None


def local_name(tag: str) -> str:
    return tag.rsplit("}", 1)[-1]


def maven_dependencies(source: str) -> tuple[str, ...] | None:
    try:
        root = ET.fromstring(source)
    except ET.ParseError:
        return None
    return tuple(
        ET.tostring(element, encoding="unicode")
        for element in root.iter()
        if local_name(element.tag) == "dependencies"
    )


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
        before, after = content(base, path), content(head, path)
        if path == "pom.xml" or path.endswith(DEPENDENCY_SUFFIXES):
            if before is None or after is None:
                return True
            old_dependencies = maven_dependencies(before)
            new_dependencies = maven_dependencies(after)
            if old_dependencies is None or new_dependencies is None or old_dependencies != new_dependencies:
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
