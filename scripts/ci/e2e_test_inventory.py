#!/usr/bin/env python3
"""Inventory Java test classes against GitHub E2E workflow ``-Dtest`` selectors.

This utility supports the coverage-improvement workflow by making it easy to
find test classes that are not selected by any regular E2E workflow. It parses
``src/test/java`` test classes and the Maven Surefire ``-Dtest=...`` selectors
embedded in workflow run commands, including simple workflow environment
variable substitutions such as ``${{ env.GLOBAL_TESTING_SCOPE }}`` and
``${GLOBAL_TESTING_SCOPE}``.
"""

import argparse
import fnmatch
import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

DEFAULT_WORKFLOWS = (
    Path(".github/workflows/e2eTests.yml"),
    Path(".github/workflows/e2eLocalTests.yml"),
    Path(".github/workflows/e2eLambdaTestTests.yml"),
)
TEST_CLASS_SUFFIXES = ("Test", "Tests", "UnitTest", "CoverageUnitTest")
DTEST_PATTERN = re.compile(r"-Dtest=([^\"\s]+|[^\"]*?)(?=\")")
ENV_PATTERN = re.compile(r"^\s*([A-Z][A-Z0-9_]*)\s*:\s*(.+?)\s*$")
GITHUB_ENV_REFERENCE_PATTERN = re.compile(r"\$\{\{\s*env\.([A-Z][A-Z0-9_]*)\s*}}")
SHELL_ENV_REFERENCE_PATTERN = re.compile(r"\$\{([A-Z][A-Z0-9_]*)}")
REGEX_SELECTOR_PATTERN = re.compile(r"%regex\[(.*)]")
ABSTRACT_CLASS_PATTERN = re.compile(r"\babstract\s+class\s+")


@dataclass(frozen=True)
class JavaTestClass:
    """A Java test class discovered under ``src/test/java``."""

    class_name: str
    qualified_name: str
    path: str


@dataclass(frozen=True)
class SurefireSelector:
    """One Maven Surefire ``-Dtest`` token from an E2E workflow."""

    source: str
    line: int
    raw: str
    negated: bool
    regex: bool
    pattern: str

    def matches(self, test_class: JavaTestClass) -> bool:
        """Return whether this selector token matches the provided Java class."""
        candidates = (test_class.class_name, test_class.qualified_name)
        if self.regex:
            try:
                compiled = re.compile(self.pattern)
            except re.error:
                return False
            return any(compiled.search(candidate) for candidate in candidates)

        pattern = self.pattern.replace("#*", "")
        return any(fnmatch.fnmatchcase(candidate, pattern) for candidate in candidates)


@dataclass(frozen=True)
class WorkflowExpression:
    """A complete ``-Dtest`` expression found in a workflow command."""

    source: str
    line: int
    value: str
    selectors: tuple[SurefireSelector, ...]

    def selects(self, test_class: JavaTestClass) -> bool:
        """Approximate Maven Surefire class selection for this expression."""
        positive_selectors = [selector for selector in self.selectors if not selector.negated]
        negative_selectors = [selector for selector in self.selectors if selector.negated]

        selected = True if not positive_selectors else any(selector.matches(test_class) for selector in positive_selectors)
        if not selected:
            return False
        return not any(selector.matches(test_class) for selector in negative_selectors)


def _qualified_name(test_root: Path, java_file: Path) -> str:
    relative = java_file.relative_to(test_root).with_suffix("")
    return ".".join(relative.parts)


def discover_test_classes(test_root: Path) -> list[JavaTestClass]:
    """Discover Java test classes below ``test_root`` using standard suffixes."""
    classes: list[JavaTestClass] = []
    if not test_root.exists():
        return classes

    for java_file in sorted(test_root.rglob("*.java")):
        class_name = java_file.stem
        if not class_name.endswith(TEST_CLASS_SUFFIXES):
            continue
        if ABSTRACT_CLASS_PATTERN.search(java_file.read_text(encoding="utf-8")):
            continue
        classes.append(
            JavaTestClass(
                class_name=class_name,
                qualified_name=_qualified_name(test_root, java_file),
                path=java_file.as_posix(),
            )
        )
    return classes


def _strip_yaml_value(value: str) -> str:
    value = value.strip()
    if value and value[0] in {'"', "'"} and value[-1:] == value[0]:
        return value[1:-1]
    return value


def parse_workflow_env(workflow: Path) -> dict[str, str]:
    """Parse simple scalar workflow environment variables from a YAML file."""
    env: dict[str, str] = {}
    if not workflow.exists():
        return env

    for line in workflow.read_text(encoding="utf-8").splitlines():
        match = ENV_PATTERN.match(line)
        if match:
            env[match.group(1)] = _strip_yaml_value(match.group(2))
    return env


def _expand_env_references(value: str, env: dict[str, str]) -> str:
    def replace_github(match: re.Match[str]) -> str:
        return env.get(match.group(1), match.group(0))

    def replace_shell(match: re.Match[str]) -> str:
        return env.get(match.group(1), match.group(0))

    value = GITHUB_ENV_REFERENCE_PATTERN.sub(replace_github, value)
    return SHELL_ENV_REFERENCE_PATTERN.sub(replace_shell, value)


def _split_selector_tokens(value: str) -> list[str]:
    return [token.strip() for token in value.split(",") if token.strip()]


def parse_selector_token(source: str, line: int, raw_token: str) -> SurefireSelector:
    """Parse one comma-separated Surefire selector token."""
    token = raw_token.strip()
    negated = token.startswith("!")
    if negated:
        token = token[1:].strip()

    regex_match = REGEX_SELECTOR_PATTERN.fullmatch(token)
    if regex_match:
        return SurefireSelector(source, line, raw_token.strip(), negated, True, regex_match.group(1))

    return SurefireSelector(source, line, raw_token.strip(), negated, False, token)


def parse_workflow_expressions(workflows: Iterable[Path]) -> list[WorkflowExpression]:
    """Extract all Maven ``-Dtest`` expressions from workflow run commands."""
    expressions: list[WorkflowExpression] = []
    for workflow in workflows:
        env = parse_workflow_env(workflow)
        if not workflow.exists():
            continue
        for line_number, line in enumerate(workflow.read_text(encoding="utf-8").splitlines(), start=1):
            for match in DTEST_PATTERN.finditer(line):
                value = _expand_env_references(match.group(1).strip(), env)
                selectors = tuple(parse_selector_token(workflow.as_posix(), line_number, token) for token in _split_selector_tokens(value))
                expressions.append(WorkflowExpression(workflow.as_posix(), line_number, value, selectors))
    return expressions


def classes_without_e2e_selection(test_classes: Iterable[JavaTestClass], expressions: Iterable[WorkflowExpression]) -> list[JavaTestClass]:
    """Return test classes that are not selected by any parsed workflow expression."""
    expressions = list(expressions)
    uncovered = []
    for test_class in test_classes:
        if not any(expression.selects(test_class) for expression in expressions):
            uncovered.append(test_class)
    return uncovered


def to_json(test_classes: list[JavaTestClass], expressions: list[WorkflowExpression], uncovered: list[JavaTestClass]) -> str:
    """Serialize the inventory result as JSON."""
    return json.dumps(
        {
            "testClassCount": len(test_classes),
            "workflowExpressionCount": len(expressions),
            "unselectedTestClassCount": len(uncovered),
            "unselectedTestClasses": [test_class.__dict__ for test_class in uncovered],
            "workflowExpressions": [
                {
                    "source": expression.source,
                    "line": expression.line,
                    "value": expression.value,
                    "selectors": [selector.__dict__ for selector in expression.selectors],
                }
                for expression in expressions
            ],
        },
        indent=2,
        sort_keys=True,
    )


def to_markdown(test_classes: list[JavaTestClass], expressions: list[WorkflowExpression], uncovered: list[JavaTestClass]) -> str:
    """Render the inventory result as a Markdown summary."""
    lines = [
        "# E2E Test Selection Inventory",
        "",
        f"* Discovered Java test classes: **{len(test_classes)}**",
        f"* Parsed workflow `-Dtest` expressions: **{len(expressions)}**",
        f"* Test classes not selected by any parsed E2E workflow expression: **{len(uncovered)}**",
        "",
    ]
    if uncovered:
        lines.extend(["| Test class | Path |", "|---|---|"])
        lines.extend(f"| `{test_class.qualified_name}` | `{test_class.path}` |" for test_class in uncovered)
    else:
        lines.append("Every discovered test class is selected by at least one parsed E2E workflow expression.")
    return "\n".join(lines)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Inventory Java test classes against E2E workflow -Dtest selectors.")
    parser.add_argument("--test-root", type=Path, default=Path("src/test/java"), help="Java test source root")
    parser.add_argument(
        "--workflow",
        action="append",
        type=Path,
        dest="workflows",
        help="Workflow YAML file to inspect; defaults to the three E2E workflows",
    )
    parser.add_argument("--format", choices=("markdown", "json"), default="markdown", help="Output format")
    parser.add_argument(
        "--fail-on-unselected",
        action="store_true",
        help="Exit with status 1 when any discovered test class is not selected by the parsed workflows",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    workflows = args.workflows or list(DEFAULT_WORKFLOWS)
    test_classes = discover_test_classes(args.test_root)
    expressions = parse_workflow_expressions(workflows)
    uncovered = classes_without_e2e_selection(test_classes, expressions)
    print(to_json(test_classes, expressions, uncovered) if args.format == "json" else to_markdown(test_classes, expressions, uncovered))
    return 1 if args.fail_on_unselected and uncovered else 0


if __name__ == "__main__":
    raise SystemExit(main())
