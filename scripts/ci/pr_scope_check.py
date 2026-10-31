#!/usr/bin/env python3
"""
H-PR-SCOPE: pre-merge PR diff must match stated scope in the PR body / plan.

Declare scope as either:

  <!-- chaos-engine:scope chaos-engine/,scripts/ci/,tests/scripts/ -->

or a ``## Scope`` section with bullet path prefixes:

  ## Scope
  - chaos-engine/
  - scripts/ci/

Every changed path (git diff --name-only base...head) must match at least one
declared prefix. Missing scope declaration → FAIL (fail-closed for agentic PRs).
Paths matching always-allowed noise (VERSION alone is still in-scope if listed)
are still checked; empty diff → PASS.
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

SCOPE_COMMENT_RE = re.compile(
    r"<!--\s*chaos-engine:scope\s+([^>]+?)-->",
    re.I | re.S,
)
SCOPE_SECTION_RE = re.compile(
    r"(?ims)^##\s+Scope\s*\n(.*?)(?=^##\s|\Z)",
)


def parse_scope_prefixes(body: str) -> list[str]:
    """Return normalized path prefixes declared in the PR body."""
    text = body or ""
    found: list[str] = []
    for match in SCOPE_COMMENT_RE.finditer(text):
        raw = match.group(1)
        for part in re.split(r"[\s,]+", raw.strip()):
            part = part.strip().strip("`")
            if part:
                found.append(part)
    section = SCOPE_SECTION_RE.search(text)
    if section:
        for line in section.group(1).splitlines():
            stripped = line.strip()
            if stripped.startswith(("-", "*", "•")):
                item = stripped.lstrip("-*• ").strip().strip("`")
                # Drop trailing gloss after em-dash / colon.
                item = re.split(r"\s+[—\-–:]\s+", item, maxsplit=1)[0].strip()
                if item and not item.lower().startswith("see "):
                    found.append(item)
    # Dedupe preserving order.
    out: list[str] = []
    seen: set[str] = set()
    for item in found:
        key = item.casefold()
        if key not in seen:
            seen.add(key)
            out.append(item)
    return out


def path_in_scope(path: str, prefixes: list[str]) -> bool:
    normalized = path.replace("\\", "/").lstrip("./")
    for prefix in prefixes:
        pref = prefix.replace("\\", "/").lstrip("./")
        if pref.endswith("/"):
            if normalized == pref[:-1] or normalized.startswith(pref):
                return True
        elif normalized == pref or normalized.startswith(pref + "/"):
            return True
        # basename / glob-ish suffix match for exact files
        elif pref.startswith("*.") and normalized.endswith(pref[1:]):
            return True
    return False


def out_of_scope_paths(changed: list[str], prefixes: list[str]) -> list[str]:
    if not prefixes:
        return ["__missing_scope_declaration__"]
    return [path for path in changed if path.strip() and not path_in_scope(path, prefixes)]


def evaluate(body: str, changed: list[str]) -> dict:
    prefixes = parse_scope_prefixes(body)
    outsiders = out_of_scope_paths(changed, prefixes)
    if outsiders == ["__missing_scope_declaration__"]:
        return {
            "ok": False,
            "hits": ["missing-scope"],
            "prefixes": [],
            "out_of_scope": [],
            "error": "PR body/plan must declare ## Scope or <!-- chaos-engine:scope ... -->",
        }
    return {
        "ok": not outsiders,
        "hits": [f"out-of-scope:{p}" for p in outsiders],
        "prefixes": prefixes,
        "out_of_scope": outsiders,
        "error": None if not outsiders else "diff paths outside stated scope",
    }



def is_agentic_pr(pr: dict | None) -> bool:
    """True when PR body/labels mark the change agentic."""
    if not isinstance(pr, dict):
        return False
    body = str(pr.get("body") or "")
    if "chaos-engine:agentic" in body.casefold() or "<!-- chaos-engine:agentic" in body.casefold():
        return True
    labels = {
        str(item.get("name") or "").casefold()
        for item in (pr.get("labels") or [])
        if isinstance(item, dict)
    }
    return "agentic" in labels


def pr_scope_failures(
    body: str,
    changed: list[str],
    *,
    require_agentic: bool = False,
    pr: dict | None = None,
) -> list[str]:
    """Return human-readable failures for H-PR-SCOPE (empty = pass / soft-skip).

    When ``require_agentic`` is True and *pr* is not agentic, return [] (soft skip).
    """
    if require_agentic and pr is not None and not is_agentic_pr(pr):
        return []
    result = evaluate(body or "", list(changed or []))
    if result.get("ok"):
        return []
    hits = result.get("hits") or []
    error = result.get("error") or "pr scope check failed"
    return [f"pr-scope (H-PR-SCOPE): {error}"] + [f"pr-scope: {h}" for h in hits]

def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--body", type=Path, help="PR body markdown file")
    parser.add_argument("--body-text", default="", help="PR body as string")
    parser.add_argument(
        "--changed",
        type=Path,
        help="file with one changed path per line (git diff --name-only)",
    )
    parser.add_argument(
        "--path",
        action="append",
        default=[],
        dest="paths",
        help="changed path (repeatable); alternative to --changed",
    )
    args = parser.parse_args(argv)
    if args.body is not None:
        if not args.body.is_file():
            print("pr-scope-check: missing body file", file=sys.stderr)
            return 1
        body = args.body.read_text(encoding="utf-8")
    else:
        body = args.body_text
    changed: list[str] = list(args.paths)
    if args.changed is not None:
        if not args.changed.is_file():
            print("pr-scope-check: missing changed-paths file", file=sys.stderr)
            return 1
        changed.extend(
            line.strip()
            for line in args.changed.read_text(encoding="utf-8").splitlines()
            if line.strip()
        )
    result = evaluate(body, changed)
    if result["ok"]:
        print("pr-scope-check: PASS")
        return 0
    print(f"pr-scope-check: FAIL — {result.get('error')}", file=sys.stderr)
    for hit in result.get("hits") or []:
        print(hit, file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
