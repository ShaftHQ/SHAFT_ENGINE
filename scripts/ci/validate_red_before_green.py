"""Reject a newly added unittest that already passes against its parent code."""

from __future__ import annotations

import argparse
import ast
import io
import os
import re
import subprocess
import sys
import tempfile
import tarfile
from pathlib import Path


NO_RED_REASON_WORDS = 12


def source_at(root: Path, revision: str, path: str) -> str:
    result = subprocess.run(
        ["git", "show", f"{revision}:{path}"],
        cwd=root,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        check=False,
    )
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
    ).stdout
    trailers = subprocess.run(
        ["git", "interpret-trailers", "--parse"], input=message, capture_output=True, text=True,
        encoding="utf-8", errors="replace", check=False,
    ).stdout
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
    )
    if archive.returncode:
        raise ValueError(f"cannot archive parent tree at {revision}^")
    with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as contents:
        contents.extractall(overlay, filter="data")


def run_parent_code_test(
    root: Path, revision: str, production_path: str, test_path: str, child_test: str,
    class_name: str, method_name: str,
) -> int:
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
        return subprocess.run(
            [sys.executable, "-m", "unittest", target], cwd=overlay, env=environment, check=False
        ).returncode


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
        if run_parent_code_test(
            root, revision, production_path, test_path, child_test, class_name, method_name
        ) == 0:
            prefix = f"{class_name}." if class_name else ""
            violations.append(
                f"{test_path}: {prefix}{method_name} passes against {production_path} at {revision}^"
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
