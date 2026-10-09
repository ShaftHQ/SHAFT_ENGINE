#!/usr/bin/env python3
"""Local mirror of the static-analysis findings that block merge after CI is green.

Codacy and GitHub code quality run only after a push, so each finding costs a
full CI cycle. This stdlib-only check flags the same classes on the Python lines
a branch changes, before the push:

- complexity: a function whose McCabe complexity exceeds the gate (Codacy MC0001)
- empty-except: an ``except`` that only passes, with no comment saying why
- unnecessary-lambda: ``lambda x: f(x)`` that should be ``f``
- f-string-without-placeholders: an f-string with nothing to format (F541)
- weak-assert: ``assertTrue``/``assertFalse`` wrapping a comparison or ``in``
  (prefer ``assertGreater``/``assertIn``/etc. for informative failures)

and, on the Java methods a branch changes:

- npath: a method whose PMD NPath exceeds the gate (Codacy NPathComplexity),
  estimated by ``scripts/ci/java_npath.py``

Usage: ``python3 scripts/ci/code_quality_preflight.py [--base REF] [paths...]``
"""

from __future__ import annotations

import argparse
import ast
import importlib.util
import re
import shutil
import subprocess  # nosec B404 - fixed git argv only.
import sys
from pathlib import Path

MAX_COMPLEXITY = 15
MAX_NPATH = 200
_SOURCES = (".py", ".java")
_HUNK = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@")
_BRANCHES = (ast.If, ast.For, ast.AsyncFor, ast.While, ast.ExceptHandler, ast.match_case)


def _git(root: Path, *args: str) -> str | None:
    git = shutil.which("git")
    if git is None:
        return None
    try:
        completed = subprocess.run(  # nosec B603 - absolute git from shutil.which, fixed argv.
            [git, *args], cwd=root, capture_output=True, text=True, check=False
        )
    except OSError:
        return None
    return completed.stdout if completed.returncode == 0 else None


def changed_python_lines(root: Path, base: str = "origin/main") -> dict[str, set[int]]:
    """Added or modified Python and Java lines versus the merge base, worktree included."""
    merge_base = (_git(root, "merge-base", base, "HEAD") or "").strip()
    diff = _git(root, "diff", "-U0", "--no-color", merge_base) if merge_base else None
    changed: dict[str, set[int]] = {}
    current: str | None = None
    for line in (diff or "").splitlines():
        if line.startswith("+++ "):
            name = line[4:].strip()
            current = name[2:] if name.startswith("b/") and name.endswith(_SOURCES) else None
            continue
        match = _HUNK.match(line)
        if current and match:
            start, count = int(match.group(1)), int(match.group(2) or "1")
            changed.setdefault(current, set()).update(range(start, start + count))
    for name in (_git(root, "ls-files", "--others", "--exclude-standard") or "").splitlines():
        if name.endswith(_SOURCES):
            changed[name] = {0}  # untracked: whole file is new
    return changed


def complexity(function: ast.AST) -> int:
    """McCabe complexity of one function body, not counting nested functions.

    Counts decision statements only (no boolean operators, conditional
    expressions or comprehensions), which reproduces Codacy's MC0001 numbers.
    """
    score = 1
    stack = list(ast.iter_child_nodes(function))
    while stack:
        node = stack.pop()
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda, ast.ClassDef)):
            continue
        if isinstance(node, _BRANCHES):
            score += 1
        if isinstance(node, (ast.For, ast.AsyncFor, ast.While, ast.Try)) and node.orelse:
            score += 1
        stack.extend(ast.iter_child_nodes(node))
    return score


def _touches(node: ast.AST, lines: set[int]) -> bool:
    if 0 in lines:
        return True
    end = getattr(node, "end_lineno", None) or node.lineno
    return any(node.lineno <= line <= end for line in lines)


def _is_unnecessary_lambda(node: ast.Lambda) -> bool:
    args = node.args
    if args.defaults or args.kw_defaults or args.vararg or args.kwarg or args.kwonlyargs or args.posonlyargs:
        return False
    call = node.body
    if not isinstance(call, ast.Call) or call.keywords:
        return False
    names = [arg.arg for arg in args.args]
    passed = [item.id if isinstance(item, ast.Name) else None for item in call.args]
    if names != passed:
        return False
    # The callee must not use the lambda's own parameters (e.g. lambda x: x.f(x)).
    return not any(isinstance(item, ast.Name) and item.id in names for item in ast.walk(call.func))


def _has_comment(source_lines: list[str], node: ast.ExceptHandler) -> bool:
    end = getattr(node, "end_lineno", None) or node.lineno
    return any("#" in source_lines[index - 1] for index in range(node.lineno, min(end, len(source_lines)) + 1))


_ASSERT_OPS = {
    (True, ast.Eq): "assertEqual",
    (True, ast.NotEq): "assertNotEqual",
    (True, ast.Lt): "assertLess",
    (True, ast.LtE): "assertLessEqual",
    (True, ast.Gt): "assertGreater",
    (True, ast.GtE): "assertGreaterEqual",
    (True, ast.In): "assertIn",
    (True, ast.NotIn): "assertNotIn",
    (True, ast.Is): "assertIs",
    (True, ast.IsNot): "assertIsNot",
    (False, ast.Eq): "assertNotEqual",
    (False, ast.NotEq): "assertEqual",
    (False, ast.Lt): "assertGreaterEqual",
    (False, ast.LtE): "assertGreater",
    (False, ast.Gt): "assertLessEqual",
    (False, ast.GtE): "assertLess",
    (False, ast.In): "assertNotIn",
    (False, ast.NotIn): "assertIn",
    (False, ast.Is): "assertIsNot",
    (False, ast.IsNot): "assertIs",
}


def _assert_method_name(func: ast.AST) -> str | None:
    if isinstance(func, ast.Attribute):
        return func.attr
    if isinstance(func, ast.Name):
        return func.id
    return None


def _weak_assert_suggestion(node: ast.Call) -> str | None:
    """Return the preferred assert* name when assertTrue/False wraps a Compare."""
    name = _assert_method_name(node.func)
    if name not in ("assertTrue", "assertFalse") or not node.args:
        return None
    first = node.args[0]
    if not isinstance(first, ast.Compare) or len(first.ops) != 1:
        return None
    return _ASSERT_OPS.get((name == "assertTrue", type(first.ops[0])))



def _java_npath():
    spec = importlib.util.spec_from_file_location("java_npath", Path(__file__).with_name("java_npath.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _npaths(source: str) -> list[tuple[str, int, int, int]]:
    try:
        return list(_java_npath().methods(source))
    except (ValueError, IndexError, RecursionError):
        return []  # unparseable Java: leave it to CI rather than block the push


def java_findings(path: Path, lines: set[int], display: str, baseline: str | None = None) -> list[str]:
    """Changed Java methods whose estimated NPath newly exceeds Codacy's PMD gate.

    With ``baseline`` (the file at the merge base), a method that was already over
    the gate is flagged only when its NPath grew, matching Codacy's new-issue rule.
    """
    try:
        source = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return []
    before = {name: npath for name, _first, _last, npath in _npaths(baseline or "")}
    return [
        f"{display}:{first} npath: `{name}` is {npath} (gate {MAX_NPATH}); "
        "extract helpers so sequential branches stop multiplying before pushing"
        for name, first, last, npath in _npaths(source)
        if npath > max(MAX_NPATH, before.get(name, 0))
        and (0 in lines or any(first <= line <= last for line in lines))
    ]


def file_findings(path: Path, lines: set[int], display: str) -> list[str]:
    if display.endswith(".java"):
        return java_findings(path, lines, display)
    try:
        source = path.read_text(encoding="utf-8")
        tree = ast.parse(source, filename=display)
    except (OSError, SyntaxError, UnicodeDecodeError, ValueError):
        return []
    source_lines = source.splitlines()
    format_specs = {id(node.format_spec) for node in ast.walk(tree)
                    if isinstance(node, ast.FormattedValue) and node.format_spec is not None}
    findings: list[str] = []
    for node in ast.walk(tree):
        if not hasattr(node, "lineno") or not _touches(node, lines):
            continue
        where = f"{display}:{node.lineno}"
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            score = complexity(node)
            if score > MAX_COMPLEXITY:
                findings.append(
                    f"{where} complexity: `{node.name}` is {score} (gate {MAX_COMPLEXITY}); "
                    "extract helpers or a dispatch table before pushing"
                )
        elif isinstance(node, ast.ExceptHandler):
            if all(isinstance(item, ast.Pass) for item in node.body) and not _has_comment(source_lines, node):
                findings.append(f"{where} empty-except: handle the error or add a comment saying why it is ignored")
        elif isinstance(node, ast.Lambda) and _is_unnecessary_lambda(node):
            findings.append(f"{where} unnecessary-lambda: pass the callee directly instead of wrapping it")
        elif (isinstance(node, ast.JoinedStr) and id(node) not in format_specs
              and not any(isinstance(item, ast.FormattedValue) for item in node.values)):
            findings.append(f"{where} f-string-without-placeholders: drop the f prefix")
        elif isinstance(node, ast.Call):
            preferred = _weak_assert_suggestion(node)
            if preferred is not None:
                used = _assert_method_name(node.func)
                findings.append(
                    f"{where} weak-assert: `{used}` wrapping a comparison or `in` "
                    f"cannot give an informative message; use `{preferred}` instead"
                )
    return findings


def preflight_findings(root: Path, changed: dict[str, set[int]] | None = None,
                       base: str = "origin/main") -> list[str]:
    diffed = changed is None
    changed = changed_python_lines(root, base) if diffed else changed
    merge_base = (_git(root, "merge-base", base, "HEAD") or "").strip() if diffed else ""
    findings: list[str] = []
    for name in sorted(changed):
        path = root / name
        if not path.is_file():
            continue
        if name.endswith(".java") and merge_base:
            baseline = _git(root, "show", f"{merge_base}:{name}")
            findings.extend(java_findings(path, changed[name], name, baseline))
        else:
            findings.extend(file_findings(path, changed[name], name))
    return findings


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--base", default="origin/main")
    parser.add_argument("paths", nargs="*", help="check these whole files instead of the branch diff")
    args = parser.parse_args(argv)
    root = args.root.resolve()
    changed = {path: {0} for path in args.paths} if args.paths else None
    findings = preflight_findings(root, changed, args.base)
    for finding in findings:
        print(f"code-quality preflight: {finding}", file=sys.stderr)
    if findings:
        print(f"code-quality preflight: {len(findings)} finding(s); fix them, then push. "
              "Do not push to let CI find them.", file=sys.stderr)
        return 1
    print("code-quality preflight: pass")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
