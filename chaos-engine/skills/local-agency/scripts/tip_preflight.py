#!/usr/bin/env python3
"""Tip-churn local preflight for ChaosEngine PRs (#6161: #6164 #6165 #6169).

Run before every push so Bandit B607, README inventory drift, and stale
Memory ``content_hash`` land in the same tip as the edit that caused them
instead of costing a PR Gate + fresh-installer matrix cycle:

    python3 chaos-engine/skills/local-agency/scripts/tip_preflight.py
    python3 chaos-engine/skills/local-agency/scripts/tip_preflight.py --rehash .memory/memory/<type>/<slug>.json

``classify_failure`` maps a CI summary line to a known fingerprint so
babysitters fetch a bounded excerpt instead of a full failed log (#6167).
Stdlib only; portable across every host.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import importlib.util
import json
import re
import shutil
import subprocess  # nosec B404 - list argv with an absolute git from shutil.which.
import sys
from pathlib import Path

SUBPROCESS_FUNCS = frozenset({"run", "call", "check_call", "check_output", "Popen"})

def _bare_argv0(node: ast.Call) -> str | None:
    """Find the bare argument for subprocess functions."""
    if not isinstance(node.func, ast.Attribute) or node.func.attr not in SUBPROCESS_FUNCS:
        return None
    if not isinstance(node.func.value, ast.Name) or node.func.value.id != "subprocess":
        return None
    first = node.args[0] if node.args else None
    if not isinstance(first, (ast.List, ast.Tuple)) or not first.elts:
        return None
    head = first.elts[0]
    if not (isinstance(head, ast.Constant) and isinstance(head.value, str)) or "/" in head.value or "\\" in head.value:
        return None
    return head.value

def _suppressed(lines: list[str], node: ast.Call) -> bool:
    """Check if the line is suppressed."""
    return any("nosec" in line and "B607" in line for line in lines[node.lineno - 1 : (node.end_lineno or node.lineno)])

def b607_findings(source: str, filename: str) -> list[str]:
    """Find B607 subprocess findings."""
    tree = ast.parse(source, filename=filename)
    lines = source.splitlines()
    pairs = []

    for node in ast.walk(tree):
        if isinstance(node, ast.Call):
            name = _bare_argv0(node)
            if name is None:
                continue
            if _suppressed(lines, node):
                continue
            pairs.append((node.lineno, f"{filename}:{node.lineno}: B607 partial executable path {name!r}; resolve once with shutil.which() and pass the absolute path"))

    return [message for _, message in sorted(pairs)]


KNOWN_FINGERPRINTS: tuple[tuple[str, re.Pattern[str]], ...] = (
    ("graphify-empty-output-after-version", re.compile(r"graphify(?:\.exe)?\b.*(?:no process output|graphify-empty-output-after-version)", re.IGNORECASE)),
    ("inventory-drift", re.compile(r"source-derived inventory drift: [\w-]+")),
    ("bandit-b607", re.compile(r"\bB607\b|Bandit_B607")),
    ("memory-content-hash", re.compile(r"memory-content-hash|ObjectContentHashMismatch|content_hash .* does not match")),
    ("codacy-action-required", re.compile(r"\bACTION_REQUIRED\b"))
)


def classify_failure(text: str) -> str | None:
    """Return the first known fingerprint found in a failure summary/log, else None."""
    for fingerprint, regex in KNOWN_FINGERPRINTS:
        if regex.search(text):
            return fingerprint
    return None


def memory_content_hash(sidecar: dict, body: str) -> str:
    """Memory CLI content_hash recipe (mirrors validate_agent_setup.memory_content_hash)."""
    payload = {key: sidecar[key] for key in sidecar if key != "content_hash"}
    canonical = json.dumps(payload, sort_keys=True, separators=(",", ":"), ensure_ascii=False)
    blob = canonical + "\n" + body.replace("\r\n", "\n")
    return "sha256:" + hashlib.sha256(blob.encode("utf-8")).hexdigest()


def _sidecar_rel(path: str) -> str | None:
    """Map a touched Memory path to its sidecar path."""
    path = path.replace("\\", "/")
    if not path.startswith(".memory/memory/") or not (path.endswith(".md") or path.endswith(".json")):
        return None
    if path.endswith(".md"):
        path = path[:-3] + ".json"
    return path if path.endswith(".json") else None


def memory_hash_failures(root: Path, paths: list[str]) -> list[str]:
    """Recompute content_hash for touched .memory/memory objects; report mismatches before push."""
    sidecars = {_sidecar_rel(p) for p in paths if _sidecar_rel(p) is not None}
    failures = []
    for rel in sorted(sidecars):
        sidecar_path = root / rel
        if not sidecar_path.is_file():
            continue
        sidecar = json.loads(sidecar_path.read_text(encoding="utf-8"))
        body_path = sidecar.get("body_path")
        if not isinstance(body_path, str) or not (root / ".memory" / body_path).is_file():
            failures.append(f"{rel}: body_path {body_path!r} is missing")
            continue
        body_file = root / ".memory" / body_path
        actual = memory_content_hash(sidecar, body_file.read_text(encoding="utf-8"))
        if sidecar.get("content_hash") != actual:
            failures.append(f"{rel}: memory-content-hash stale (recorded {sidecar.get('content_hash')!r}, recomputed {actual!r}); run tip_preflight.py --rehash {rel} or memory save --stdin")
    return failures


def rehash_memory_object(root: Path, rel: str) -> str:
    """Rewrite only the stored content_hash of one sidecar; return the new hash."""
    sidecar_path = root / rel
    text = sidecar_path.read_text(encoding="utf-8")
    sidecar = json.loads(text)
    body = (root / ".memory" / sidecar["body_path"]).read_text(encoding="utf-8")
    new_hash = memory_content_hash(sidecar, body)
    sidecar["content_hash"] = new_hash
    sidecar_path.write_text(json.dumps(sidecar, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    return new_hash


def default_base(root: Path) -> str:
    """Portable base ref: the remote default branch, else upstream, else HEAD."""
    git = shutil.which("git")
    if git is None:
        return "HEAD"
    for argv in [
        ["symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD"],
        ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"]
    ]:
        result = subprocess.run([git] + argv, cwd=root, capture_output=True, text=True, check=False)  # nosec B603 - absolute git from shutil.which, list argv
        if result.returncode == 0 and result.stdout.strip():
            return result.stdout.strip()
    return "HEAD"


def changed_paths(root: Path, base: str | None = None) -> list[str]:
    """Paths changed versus base plus staged and unstaged edits (order-preserving, unique)."""
    base = base or default_base(root)
    git = shutil.which("git")
    if git is None:
        return []
    paths: list[str] = []
    for args in (["diff", "--name-only", f"{base}...HEAD"], ["diff", "--name-only", "HEAD"], ["diff", "--name-only", "--cached"]):
        completed = subprocess.run([git, *args], cwd=root, capture_output=True, text=True, check=False)  # nosec B603 - absolute git from shutil.which, list argv
        if completed.returncode != 0:
            continue
        paths.extend([line.strip() for line in completed.stdout.splitlines() if line.strip()])
    return list(dict.fromkeys(paths))


def inventory_drift(root: Path, validator=None) -> list[str]:
    """Mirror Agent Guidance Gate README inventory validation before push."""
    if validator is None:
        script = root / "scripts/ci/validate_chaos_engine_readme.py"
        if not script.is_file():
            return []
        spec = importlib.util.spec_from_file_location("ce_tip_preflight_readme", script)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        validator = module.validate
    return [f"{error}; refresh with python3 scripts/ci/validate_chaos_engine_readme.py --write in the same commit" for error in validator(root)]


def b607_failures(root: Path, paths: list[str]) -> list[str]:
    """Bandit B607 local blocking preflight for changed Python files."""
    failures = []
    for path in paths:
        if path.endswith(".py"):
            file = root / path
            if file.is_file():
                failures.extend(b607_findings(file.read_text(encoding="utf-8"), path))
    return failures


def preflight_failures(root: Path, paths: list[str], *, inventory_validator=None) -> list[str]:
    """One tip-churn preflight: B607 + README inventory + Memory content_hash."""
    failures = b607_failures(root, paths) + memory_hash_failures(root, paths)
    if any(p.replace("\\", "/").startswith("chaos-engine/") and p.endswith(".py") for p in paths):
        failures += inventory_drift(root, inventory_validator)
    return failures


def main(argv: list[str] | None = None) -> int:
    """CLI: exit 1 with one line per failure; exit 0 prints 'tip preflight: pass'."""
    parser = argparse.ArgumentParser(description="Tip churn preflight checks")
    parser.add_argument("--root", type=Path, default=Path.cwd(), help="Root directory")
    parser.add_argument("--base", type=str, default=None, help="Base ref (default: remote default branch, else upstream)")
    parser.add_argument("--rehash", type=str, default=None, help="Rehash specific path")
    parser.add_argument("paths", nargs="*", help="Paths to check")
    args = parser.parse_args(argv)

    root = args.root.resolve()
    if args.rehash:
        print(f"rehashed {args.rehash}: {rehash_memory_object(root, args.rehash)}")
        return 0

    paths = args.paths or changed_paths(root, args.base)
    failures = preflight_failures(root, paths, inventory_validator=None)

    if failures:
        for failure in failures:
            print(f"tip preflight: {failure}", file=sys.stderr)
        return 1
    else:
        print("tip preflight: pass")
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
