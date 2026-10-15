#!/usr/bin/env python3
"""Weekly quality trend for ``main`` (#6990): Codacy and coverage.

Reads the ``Codacy Static Code Analysis`` check run on the last N ``main``
commits (conclusion plus new-issue count from its title) and the Codecov
coverage total. Exits 2 on a regression, meaning any failing Codacy run in the
window or coverage below the floor, so the workflow can open or update one
tracker issue. Quality layers are never removed; this report keeps the
non-blocking Codacy reporter visible on ``main``.
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess  # nosec B404 - fixed gh argument vector, never a shell.
import sys
import urllib.request

CODACY = "Codacy Static Code Analysis"
NEW_ISSUES = re.compile(r"(\d+)\s+new issue", re.IGNORECASE)


def _gh(path: str) -> object:
    gh = shutil.which("gh")
    if gh is None:
        raise SystemExit("gh CLI not found")
    out = subprocess.run([gh, "api", path], check=True, capture_output=True, text=True).stdout  # nosec B603
    return json.loads(out)


def codacy_rows(runs: list[dict]) -> list[dict]:
    rows = []
    for run in runs:
        title = str((run.get("output") or {}).get("title") or "")
        match = NEW_ISSUES.search(title)
        rows.append({"conclusion": run.get("conclusion") or "pending",
                     "new_issues": int(match.group(1)) if match else 0})
    return rows


def summarize(rows: list[dict], coverage: float | None, floor: float) -> dict:
    failing = sum(1 for row in rows if row["conclusion"] == "failure")
    new_issues = sum(row["new_issues"] for row in rows)
    coverage_drop = coverage is not None and coverage < floor
    return {"commits": len(rows), "codacy_failures": failing, "codacy_new_issues": new_issues,
            "coverage": coverage, "coverage_floor": floor,
            "regression": failing > 0 or coverage_drop}


def render(summary: dict) -> str:
    status = "REGRESSION" if summary["regression"] else "OK"
    return (
        "## Quality trend (main)\n\n"
        "| Commits | Codacy failures | Codacy new issues | Coverage % | Floor % | Status |\n"
        "|---|---|---|---|---|---|\n"
        f"| {summary['commits']} | {summary['codacy_failures']} | {summary['codacy_new_issues']} "
        f"| {summary['coverage']} | {summary['coverage_floor']} | {status} |\n"
    )


def fetch(repo: str, limit: int) -> tuple[list[dict], float | None]:
    commits = _gh(f"repos/{repo}/commits?sha=main&per_page={limit}")
    runs = []
    for commit in commits:  # type: ignore[union-attr]
        data = _gh(f"repos/{repo}/commits/{commit['sha']}/check-runs?check_name={CODACY.replace(' ', '%20')}")
        found = data.get("check_runs") or []  # type: ignore[union-attr]
        if found:
            runs.append(found[0])
    owner, name = repo.split("/", 1)
    url = f"https://api.codecov.io/api/v2/github/{owner}/repos/{name}/"
    try:
        with urllib.request.urlopen(url, timeout=20) as response:  # nosec B310 - fixed https host
            coverage = (json.load(response).get("totals") or {}).get("coverage")
    except (OSError, ValueError):
        coverage = None
    return runs, float(coverage) if coverage is not None else None


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--limit", type=int, default=30)
    parser.add_argument("--coverage-floor", type=float, default=61.0)
    args = parser.parse_args(argv)
    runs, coverage = fetch(args.repo, args.limit)
    summary = summarize(codacy_rows(runs), coverage, args.coverage_floor)
    print(render(summary))
    print(json.dumps(summary))
    return 2 if summary["regression"] else 0


if __name__ == "__main__":
    sys.exit(main())
