"""Reject a newly added unittest that already passes against its parent code."""

from __future__ import annotations

import argparse
import ast
import io
import json
import os
import re
import subprocess  # nosec B404 - fixed list-argument commands only.
import sys
import tempfile
import tarfile
from dataclasses import dataclass
from pathlib import Path


NO_RED_REASON_WORDS = 12
PARENT_TEST_TIMEOUT_SECONDS = 10
RESULT_SENTINEL = "RED_RESULT:"
RESULT_RUNNER = r"""
import io
import json
import os
import sys
import unittest

target = sys.argv[1]
suite = unittest.TestLoader().loadTestsFromName(target)
stream = io.StringIO()
result = unittest.TextTestRunner(stream=stream, verbosity=0).run(suite)

def rows(values):
    return [
        {"id": getattr(test, "id", lambda: str(test))(), "traceback": traceback}
        for test, traceback in values
    ]

payload = json.dumps({
    "testsRun": result.testsRun,
    "failures": rows(result.failures),
    "errors": rows(result.errors),
    "skipped": [
        {"id": getattr(test, "id", lambda: str(test))(), "reason": reason}
        for test, reason in result.skipped
    ],
    "expectedFailures": rows(result.expectedFailures),
    "unexpectedSuccesses": [
        getattr(test, "id", lambda: str(test))() for test in result.unexpectedSuccesses
    ],
}, sort_keys=True).encode("utf-8")
os.write(1, b"RED_RESULT:" + payload + b"\n")
os.close(1)
"""


@dataclass(frozen=True)
class ParentTestOutcome:
    """Structured result for one added test executed against parent code."""

    kind: str
    details: str = ""


def source_at(root: Path, revision: str, path: str) -> str:
    result = subprocess.run(
        ["git", "show", f"{revision}:{path}"],
        cwd=root,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        check=False,
    )  # nosec B603 B607 - fixed read-only git command.
    if result.returncode:
        raise ValueError(f"cannot read {path} at {revision}")
    return result.stdout


def test_methods(source: str) -> set[tuple[str, str]]:
    """Names of unittest test methods, including module-level test functions."""
    found: set[tuple[str, str]] = set()
    for node in ast.parse(source).body:
        if isinstance(node, ast.FunctionDef) and node.name.startswith("test"):
            found.add(("", node.name))
        elif isinstance(node, ast.ClassDef):
            for member in node.body:
                if isinstance(member, ast.FunctionDef) and member.name.startswith("test"):
                    found.add((node.name, member.name))
    return found


def no_red_reason(root: Path, revision: str) -> bool:
    message = subprocess.run(
        ["git", "show", "-s", "--format=%B", revision], cwd=root, capture_output=True, text=True,
        encoding="utf-8", errors="replace", check=False,
    ).stdout  # nosec B603 B607 - fixed read-only git command.
    trailers = subprocess.run(
        ["git", "interpret-trailers", "--parse"], input=message, capture_output=True, text=True,
        encoding="utf-8", errors="replace", check=False,
    ).stdout  # nosec B603 B607 - fixed read-only git command.
    match = re.search(r"(?im)^no-red:\s*(.+)$", trailers)
    return bool(match and len(match.group(1).split()) >= NO_RED_REASON_WORDS)


def module_name(path: str) -> str:
    return ".".join(Path(path).with_suffix("").parts)


def restore_parent_tree(root: Path, revision: str, overlay: Path) -> None:
    archive = subprocess.run(
        ["git", "archive", f"{revision}^"],
        cwd=root,
        capture_output=True,
        check=False,
    )  # nosec B603 B607 - fixed read-only git command.
    if archive.returncode:
        raise ValueError(f"cannot archive parent tree at {revision}^")
    with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as contents:
        contents.extractall(overlay, filter="data")


def run_parent_code_test(
    root: Path, revision: str, production_path: str, test_path: str, child_test: str,
    class_name: str, method_name: str,
) -> ParentTestOutcome:
    with tempfile.TemporaryDirectory() as temporary:
        overlay = Path(temporary)
        test = overlay / test_path
        restore_parent_tree(root, revision, overlay)
        test.parent.mkdir(parents=True, exist_ok=True)
        if not (overlay / production_path).is_file():
            raise ValueError(f"cannot read {production_path} at {revision}^")
        test.write_text(child_test, encoding="utf-8")
        target = ".".join(part for part in (module_name(test_path), class_name, method_name) if part)
        environment = os.environ | {
            "PYTHONPATH": os.pathsep.join(filter(None, (str(overlay), str(root), os.environ.get("PYTHONPATH"))))
        }
        try:
            completed = subprocess.run(
                [sys.executable, "-c", RESULT_RUNNER, target],
                cwd=overlay,
                env=environment,
                capture_output=True,
                text=True,
                timeout=PARENT_TEST_TIMEOUT_SECONDS,
                check=False,
            )  # nosec B603 - fixed Python executable and local runner.
        except subprocess.TimeoutExpired:
            return ParentTestOutcome("timeout")
        if completed.returncode != 0:
            return ParentTestOutcome("crash", f"exit {completed.returncode}")
        sentinel_lines = [
            line for line in completed.stdout.splitlines() if line.startswith(RESULT_SENTINEL)
        ]
        if len(sentinel_lines) != 1:
            if not sentinel_lines:
                return ParentTestOutcome("missing result")
            return ParentTestOutcome("invalid result", f"{len(sentinel_lines)} frames")
        try:
            payload = json.loads(sentinel_lines[-1].removeprefix(RESULT_SENTINEL))
        except (json.JSONDecodeError, AttributeError):
            return ParentTestOutcome("invalid result")
        required_lists = (
            "failures", "errors", "skipped", "expectedFailures", "unexpectedSuccesses"
        )
        if (
            not isinstance(payload, dict)
            or not isinstance(payload.get("testsRun"), int)
            or isinstance(payload.get("testsRun"), bool)
            or any(not isinstance(payload.get(name), list) for name in required_lists)
        ):
            return ParentTestOutcome("invalid result")
        failures = payload["failures"]
        errors = payload["errors"]
        traceback_rows = failures + errors + payload["expectedFailures"]
        if any(
            not isinstance(item, dict)
            or set(item) != {"id", "traceback"}
            or not all(isinstance(item[key], str) for key in ("id", "traceback"))
            for item in traceback_rows
        ):
            return ParentTestOutcome("invalid result")
        if any(
            not isinstance(item, dict)
            or set(item) != {"id", "reason"}
            or not all(isinstance(item[key], str) for key in ("id", "reason"))
            for item in payload["skipped"]
        ) or any(not isinstance(item, str) for item in payload["unexpectedSuccesses"]):
            return ParentTestOutcome("invalid result")
        if failures and errors:
            return ParentTestOutcome("mixed failure and error")
        if errors:
            tracebacks = "\n".join(
                str(item.get("traceback", "")) for item in errors if isinstance(item, dict)
            )
            if "ModuleNotFoundError" in tracebacks or "ImportError" in tracebacks:
                return ParentTestOutcome("import error")
            if "AttributeError" in tracebacks:
                return ParentTestOutcome("attribute error")
            if "setUp" in tracebacks:
                return ParentTestOutcome("setup error")
            return ParentTestOutcome("error")
        if payload["skipped"]:
            return ParentTestOutcome("skip")
        if payload["expectedFailures"]:
            return ParentTestOutcome("expected failure")
        if payload["unexpectedSuccesses"]:
            return ParentTestOutcome("unexpected success")
        if payload["testsRun"] == 0:
            return ParentTestOutcome("zero tests")
        if payload["testsRun"] != 1:
            return ParentTestOutcome("wrong test count", str(payload["testsRun"]))
        if len(failures) == 1:
            if failures[0]["id"] != target:
                return ParentTestOutcome("wrong target", failures[0]["id"])
            return ParentTestOutcome("assertion failure")
        if failures:
            return ParentTestOutcome("multiple failures", str(len(failures)))
        return ParentTestOutcome("pass")


def validate(root: Path, revision: str, production_path: str, test_path: str) -> list[str]:
    if no_red_reason(root, revision):
        return []
    try:
        parent_test = source_at(root, f"{revision}^", test_path)
    except ValueError:
        parent_test = ""
    child_test = source_at(root, revision, test_path)
    added = sorted(test_methods(child_test) - test_methods(parent_test))
    violations: list[str] = []
    for class_name, method_name in added:
        outcome = run_parent_code_test(
            root, revision, production_path, test_path, child_test, class_name, method_name
        )
        if outcome.kind != "assertion failure":
            prefix = f"{class_name}." if class_name else ""
            details = f" ({outcome.details})" if outcome.details else ""
            violations.append(
                f"{test_path}: {prefix}{method_name} produced {outcome.kind}{details} "
                f"against {production_path} at {revision}^; expected assertion failure"
            )
    return violations


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("revision")
    parser.add_argument("production_path")
    parser.add_argument("test_path")
    args = parser.parse_args()
    try:
        violations = validate(args.root.resolve(), args.revision, args.production_path, args.test_path)
    except ValueError as error:
        print(f"red-before-green unavailable: {error}", file=sys.stderr)
        return 2
    for violation in violations:
        print(f"red-before-green: {violation}", file=sys.stderr)
    return 1 if violations else 0


if __name__ == "__main__":
    raise SystemExit(main())
