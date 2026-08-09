"""Print whether a revision range changes dependencies that need GitHub review."""

from __future__ import annotations

import subprocess
import sys
import re


DEPENDENCY_CONFIG = ".github/dependency-review-config.yml"
CI_PYTHON_REQUIREMENTS = "requirements-ci.txt"
DEPENDENCY_SUFFIXES = ("/pom.xml", ".pom.xml")
GRADLE_BUILD_SUFFIX = ".gradle.kts"
GRADLE_PROPERTIES_SUFFIX = "/gradle.properties"


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
            old_dependencies = maven_dependencies(before)
            new_dependencies = maven_dependencies(after)
            if old_dependencies != new_dependencies:
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
