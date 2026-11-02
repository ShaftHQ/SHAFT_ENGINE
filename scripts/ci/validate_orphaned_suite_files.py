#!/usr/bin/env python3
"""Detect TestNG suite XML files under src/test/resources that nothing references.

Issue #4071 (following #4018, which deleted
shaft-engine/src/test/resources/TestSuites/unitTestSuite.xml): a TestNG suite
XML file that no pom, workflow, or script points at is invisible -- unlike a
failing test, nothing reports it, so it can sit there indefinitely looking
like coverage while actually running nothing. This guard makes that class of
defect loud: it finds every TestNG suite XML (root element ``<suite>``) under
any module's ``src/test/resources`` tree and fails when the file's own name
never appears in any ``pom.xml``, ``.github/workflows/*.yml``, or
``scripts/**`` file.

Deliberately narrow and distinct from ``scripts/ci/assert_tests_executed.py``
(#4079): that guard checks whether a *class a CI job already selected*
actually produced test output at runtime; this one checks whether a *suite
XML file on disk* is wired to anything at all, purely statically. Neither
subsumes the other -- a suite file can be referenced yet the class it names
can still execute zero tests (a different bug), and a class can run fine via
plain ``-Dtest`` selectors having nothing to do with any suite file.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SUITE_XML_GLOB = "**/src/test/resources/**/*.xml"
REFERENCE_GLOBS = (
    "**/pom.xml",
    ".github/workflows/*.yml",
    ".github/workflows/*.yaml",
    "scripts/**/*.py",
    "scripts/**/*.sh",
)
SUITE_ROOT_PATTERN = re.compile(r"<suite\b", re.IGNORECASE)


def issue(code: str, path: str, message: str) -> dict[str, str]:
    """Create a stable validation issue."""
    return {"code": code, "path": path, "message": message}


def _is_testng_suite(xml_path: Path) -> bool:
    """True when the file's root element looks like a TestNG <suite>."""
    try:
        text = xml_path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return False
    return bool(SUITE_ROOT_PATTERN.search(text))


def _referencing_corpus(root: Path) -> str:
    """Concatenated text of every pom, workflow, and script file that could reference a suite."""
    chunks: list[str] = []
    for pattern in REFERENCE_GLOBS:
        for path in sorted(root.glob(pattern)):
            if path.is_file():
                try:
                    chunks.append(path.read_text(encoding="utf-8"))
                except (OSError, UnicodeDecodeError):
                    continue
    return "\n".join(chunks)


def validate_repository(root: Path = ROOT) -> list[dict[str, str]]:
    """Flag every TestNG suite XML file that no pom, workflow, or script references."""
    errors: list[dict[str, str]] = []
    corpus = _referencing_corpus(root)
    for xml_path in sorted(root.glob(SUITE_XML_GLOB)):
        if not xml_path.is_file() or not _is_testng_suite(xml_path):
            continue
        relative = xml_path.relative_to(root).as_posix()
        if xml_path.name not in corpus:
            errors.append(
                issue(
                    "orphaned-suite-xml",
                    relative,
                    f"'{xml_path.name}' is not referenced by any pom.xml, .github/workflows/*.yml, "
                    "or scripts/** file -- it looks like TestNG suite coverage but nothing runs it",
                )
            )
    return errors


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    errors = validate_repository(args.root.resolve())
    if args.format == "json":
        print(json.dumps({"valid": not errors, "errors": errors}, indent=2))
    elif errors:
        for error in errors:
            print(f"{error['code']}: {error['path']}: {error['message']}", file=sys.stderr)
    else:
        print("All TestNG suite XML files are referenced by something.")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
