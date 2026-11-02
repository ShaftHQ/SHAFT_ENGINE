#!/usr/bin/env python3
"""
Auto-close CE program epics when every GitHub sub-issue is closed (#5585).

Convention (see chaos-engine/references/work-github-planning.md):
- CE program epics use GitHub native sub-issues under the epic.
- An epic is eligible when its number is in KNOWN_EPIC_NUMBERS (currently #5569)
  or it carries the ``ce-program-epic`` label.
- Child delivery PRs use ``Fixes #<child>`` / ``Closes #<child>`` — never the epic.
- On ``issues: closed`` for a child, this script no-ops unless the parent epic is
  eligible and every tracked sub-issue is closed; then it closes the epic.

Pure decision helpers are network-free so unit tests can fixture GraphQL shapes.
Live I/O goes through injectable callables (default: ``gh`` CLI).
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess  # nosec B404 - fixed list-args gh invocations, never shell=True
import sys
from dataclasses import dataclass
from typing import Callable, Sequence

EPIC_LABEL = "ce-program-epic"
KNOWN_EPIC_NUMBERS = frozenset({5569})

PARENT_QUERY = """
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    issue(number: $number) {
      number
      title
      state
      labels(first: 50) { nodes { name } }
      parent {
        number
        title
        state
        labels(first: 50) { nodes { name } }
      }
    }
  }
}
""".strip()

SUB_ISSUES_QUERY = """
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    issue(number: $number) {
      number
      title
      state
      labels(first: 50) { nodes { name } }
      subIssues(first: 100) {
        nodes { number title state }
      }
    }
  }
}
""".strip()


@dataclass(frozen=True)
class IssueRef:
    """Minimal issue identity used by the autoclose decision."""

    number: int
    title: str
    state: str
    labels: tuple[str, ...] = ()

    @property
    def is_closed(self) -> bool:
        return self.state.upper() == "CLOSED"


@dataclass(frozen=True)
class Decision:
    """Outcome of evaluating whether an epic should close."""

    action: str
    epic: IssueRef | None
    open_children: tuple[IssueRef, ...]
    closed_children: tuple[IssueRef, ...]
    reason: str

    @property
    def should_close(self) -> bool:
        return self.action == "close_epic"


def issue_from_graphql_node(node: dict | None) -> IssueRef | None:
    """Parse one GraphQL issue node into an IssueRef."""
    if not isinstance(node, dict):
        return None
    number = node.get("number")
    title = node.get("title")
    state = node.get("state")
    if not isinstance(number, int) or number < 1:
        return None
    if not isinstance(title, str) or not isinstance(state, str):
        return None
    labels_payload = node.get("labels") or {}
    label_nodes = labels_payload.get("nodes") if isinstance(labels_payload, dict) else None
    labels: list[str] = []
    if isinstance(label_nodes, list):
        for item in label_nodes:
            if isinstance(item, dict) and isinstance(item.get("name"), str):
                labels.append(item["name"])
    return IssueRef(number=number, title=title, state=state, labels=tuple(labels))


def parse_sub_issues(nodes: object) -> tuple[IssueRef, ...]:
    """Parse ``subIssues.nodes`` into IssueRef tuples (skips malformed rows)."""
    if not isinstance(nodes, list):
        return ()
    children: list[IssueRef] = []
    for node in nodes:
        parsed = issue_from_graphql_node(node if isinstance(node, dict) else None)
        if parsed is not None:
            children.append(parsed)
    return tuple(children)


def is_eligible_epic(issue: IssueRef) -> bool:
    """True when the issue is the known CE epic or carries ``ce-program-epic``."""
    if issue.number in KNOWN_EPIC_NUMBERS:
        return True
    return EPIC_LABEL in issue.labels


def decide(*, parent: IssueRef | None, children: Sequence[IssueRef]) -> Decision:
    """
    Decide whether the parent epic should close given its sub-issues.

    Never force-closes while any tracked child remains open. Epics with zero
    sub-issues are a no-op (avoids closing a mislabeled issue with no children).
    """
    if parent is None:
        return Decision(
            action="noop_no_parent",
            epic=None,
            open_children=(),
            closed_children=(),
            reason="closed issue has no GitHub parent sub-issue link",
        )
    if not is_eligible_epic(parent):
        return Decision(
            action="noop_ineligible",
            epic=parent,
            open_children=(),
            closed_children=(),
            reason=(
                f"#{parent.number} is not an eligible CE program epic "
                f"(need number in {sorted(KNOWN_EPIC_NUMBERS)} or label '{EPIC_LABEL}')"
            ),
        )
    if parent.is_closed:
        return Decision(
            action="noop_already_closed",
            epic=parent,
            open_children=(),
            closed_children=tuple(children),
            reason=f"epic #{parent.number} is already closed",
        )
    if not children:
        return Decision(
            action="noop_no_children",
            epic=parent,
            open_children=(),
            closed_children=(),
            reason=f"epic #{parent.number} has no tracked GitHub sub-issues",
        )
    open_children = tuple(child for child in children if not child.is_closed)
    closed_children = tuple(child for child in children if child.is_closed)
    if open_children:
        open_list = ", ".join(f"#{child.number}" for child in open_children)
        return Decision(
            action="noop_open_children",
            epic=parent,
            open_children=open_children,
            closed_children=closed_children,
            reason=f"epic #{parent.number} still has open sub-issues: {open_list}",
        )
    return Decision(
        action="close_epic",
        epic=parent,
        open_children=(),
        closed_children=closed_children,
        reason=(
            f"all {len(closed_children)} sub-issues of epic #{parent.number} are closed"
        ),
    )


def close_comment(*, epic: IssueRef, children: Sequence[IssueRef], trigger: int) -> str:
    """Build the comment posted when the epic is auto-closed."""
    child_list = ", ".join(f"#{child.number}" for child in children) or "(none)"
    return (
        f"Auto-closed by `epic-autoclose` (#5585): all {len(children)} GitHub "
        f"sub-issues under this CE program epic are closed.\n\n"
        f"- Trigger: close of #{trigger}\n"
        f"- Children: {child_list}\n"
        f"- Convention: subtasks use GitHub sub-issues; delivery PRs use "
        f"`Fixes #<child>` (never the epic). See "
        f"`chaos-engine/references/work-github-planning.md`.\n"
    )


def _run_gh(args: list[str], *, runner: Callable[..., subprocess.CompletedProcess]) -> str:
    """Run ``gh`` with fixed list args and return stdout, or raise RuntimeError."""
    completed = runner(
        args,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    if completed.returncode:
        detail = (completed.stderr or completed.stdout or "").strip()
        raise RuntimeError(detail or f"gh exited {completed.returncode}")
    return completed.stdout


def graphql(
    query: str,
    variables: dict,
    *,
    runner: Callable[..., subprocess.CompletedProcess],
) -> dict:
    """Execute a GitHub GraphQL query via ``gh api graphql``."""
    args = ["gh", "api", "graphql", "-f", f"query={query}"]
    for key, value in variables.items():
        if isinstance(value, int):
            args.extend(["-F", f"{key}={value}"])
        else:
            args.extend(["-f", f"{key}={value}"])
    raw = _run_gh(args, runner=runner)
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError as error:
        raise RuntimeError(f"GraphQL returned invalid JSON: {error}") from error
    if not isinstance(payload, dict):
        raise RuntimeError("GraphQL returned a non-object payload")
    if payload.get("errors"):
        raise RuntimeError(json.dumps(payload["errors"]))
    return payload


def fetch_parent_context(
    owner: str,
    name: str,
    issue_number: int,
    *,
    runner: Callable[..., subprocess.CompletedProcess],
) -> tuple[IssueRef | None, IssueRef | None]:
    """Return (closed_issue, parent_epic_or_none) for the triggering issue."""
    payload = graphql(
        PARENT_QUERY,
        {"owner": owner, "name": name, "number": issue_number},
        runner=runner,
    )
    issue_node = (
        ((payload.get("data") or {}).get("repository") or {}).get("issue")
    )
    closed = issue_from_graphql_node(issue_node if isinstance(issue_node, dict) else None)
    parent_node = issue_node.get("parent") if isinstance(issue_node, dict) else None
    parent = issue_from_graphql_node(parent_node if isinstance(parent_node, dict) else None)
    return closed, parent


def fetch_epic_children(
    owner: str,
    name: str,
    epic_number: int,
    *,
    runner: Callable[..., subprocess.CompletedProcess],
) -> tuple[IssueRef, tuple[IssueRef, ...]]:
    """Return (epic, children) refreshed from GraphQL subIssues."""
    payload = graphql(
        SUB_ISSUES_QUERY,
        {"owner": owner, "name": name, "number": epic_number},
        runner=runner,
    )
    issue_node = (
        ((payload.get("data") or {}).get("repository") or {}).get("issue")
    )
    epic = issue_from_graphql_node(issue_node if isinstance(issue_node, dict) else None)
    if epic is None:
        raise RuntimeError(f"epic #{epic_number} not found in {owner}/{name}")
    nodes = ()
    if isinstance(issue_node, dict):
        sub = issue_node.get("subIssues") or {}
        if isinstance(sub, dict):
            nodes = parse_sub_issues(sub.get("nodes"))
    return epic, nodes


def close_issue(
    owner: str,
    name: str,
    number: int,
    comment: str,
    *,
    runner: Callable[..., subprocess.CompletedProcess],
) -> None:
    """Close an issue with a comment via ``gh issue close``."""
    _run_gh(
        [
            "gh",
            "issue",
            "close",
            str(number),
            "--repo",
            f"{owner}/{name}",
            "--comment",
            comment,
        ],
        runner=runner,
    )


def evaluate_closed_issue(
    owner: str,
    name: str,
    issue_number: int,
    *,
    runner: Callable[..., subprocess.CompletedProcess],
) -> Decision:
    """Fetch live parent/children and decide whether to close the epic."""
    _closed, parent = fetch_parent_context(owner, name, issue_number, runner=runner)
    if parent is None:
        return decide(parent=None, children=())
    epic, children = fetch_epic_children(owner, name, parent.number, runner=runner)
    return decide(parent=epic, children=children)


def split_repository(repository: str) -> tuple[str, str]:
    """Parse ``owner/name`` into a pair."""
    parts = repository.split("/")
    if len(parts) != 2 or not parts[0] or not parts[1]:
        raise ValueError(f"repository must be owner/name, got {repository!r}")
    return parts[0], parts[1]


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--repository",
        default=os.environ.get("GITHUB_REPOSITORY", "ShaftHQ/SHAFT_ENGINE"),
        help="owner/name repository slug (default: $GITHUB_REPOSITORY or ShaftHQ/SHAFT_ENGINE)",
    )
    parser.add_argument(
        "--issue",
        type=int,
        required=True,
        help="number of the issue that just closed (trigger)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="print the decision JSON and do not close anything",
    )
    parser.add_argument(
        "--decision-only",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    owner, name = split_repository(args.repository)
    runner = subprocess.run
    decision = evaluate_closed_issue(owner, name, args.issue, runner=runner)
    payload = {
        "action": decision.action,
        "reason": decision.reason,
        "epic": None
        if decision.epic is None
        else {
            "number": decision.epic.number,
            "title": decision.epic.title,
            "state": decision.epic.state,
            "labels": list(decision.epic.labels),
        },
        "open_children": [child.number for child in decision.open_children],
        "closed_children": [child.number for child in decision.closed_children],
        "trigger_issue": args.issue,
        "dry_run": bool(args.dry_run),
    }
    print(json.dumps(payload, indent=2, sort_keys=True))
    if not decision.should_close:
        return 0
    if decision.epic is None:
        raise RuntimeError("close_epic decision missing epic payload")
    comment = close_comment(
        epic=decision.epic,
        children=decision.closed_children,
        trigger=args.issue,
    )
    if args.dry_run:
        print(f"dry-run: would close #{decision.epic.number} with comment:\n{comment}")
        return 0
    close_issue(owner, name, decision.epic.number, comment, runner=runner)
    print(f"closed epic #{decision.epic.number}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (RuntimeError, ValueError, OSError) as error:
        print(f"epic-autoclose error: {error}", file=sys.stderr)
        raise SystemExit(1) from error
