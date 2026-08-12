"""Portable, client-neutral ChaosEngine repository operations."""

from __future__ import annotations

import argparse
import contextlib
import io
import json
import shutil
import subprocess  # nosec B404 - fixed read-only git/gh commands, never a shell.
import sys
from pathlib import Path

try:
    from scripts.agents.repository_context import (
        RepositoryContext,
        RepositoryContextError,
        resolve_repository_context,
    )
    from scripts.agents import watch_pr_checks
    from scripts.agents.planning_contract import validate_plan
    from scripts.agents.github_client import GitHubClient, GitHubUnavailable
    from scripts.agents.pr_audit import audit_snapshot, collect_pr_snapshot
    from scripts.agents.delivery_status import collect_delivery, evaluate_delivery, inspect_cleanup
    from scripts.agents.issue_filing import create_issue, prepare_issue_plan, receipt_digest, reconcile_labels, transition_issue, validate_issue_plan
except ModuleNotFoundError:
    from repository_context import RepositoryContext, RepositoryContextError, resolve_repository_context
    import watch_pr_checks
    from planning_contract import validate_plan
    from github_client import GitHubClient, GitHubUnavailable
    from pr_audit import audit_snapshot, collect_pr_snapshot
    from delivery_status import collect_delivery, evaluate_delivery, inspect_cleanup
    from issue_filing import create_issue, prepare_issue_plan, receipt_digest, reconcile_labels, transition_issue, validate_issue_plan


EXIT_ENVIRONMENT_ERROR = 3


def add_context_arguments(parser: argparse.ArgumentParser) -> None:
    """Add the shared repository-context flags to one command parser."""
    parser.add_argument("--repo", help="owner/repo slug")
    parser.add_argument("--pr", help="positive PR number or GitHub pull-request URL")
    parser.add_argument("--root", type=Path, help="repository root; defaults to cwd")


def context_from_arguments(arguments: argparse.Namespace) -> RepositoryContext:
    """Resolve one parsed command's repository context."""
    return resolve_repository_context(
        explicit_repo=arguments.repo,
        pr=arguments.pr,
        explicit_root=arguments.root,
        cwd=Path.cwd(),
    )


def context_payload(context: RepositoryContext) -> dict:
    """Return the stable JSON representation of repository context."""
    return {"repo": context.repo, "root": str(context.root), "pr": context.pr_number}


def checkpoint_status(
    context: RepositoryContext,
    *,
    runner=None,
    executable_resolver=None,
) -> dict:
    """Report current HEAD and any open pull request whose head is exact."""
    runner = subprocess.run if runner is None else runner
    executable_resolver = shutil.which if executable_resolver is None else executable_resolver
    git = executable_resolver("git")
    gh = executable_resolver("gh")
    if git is None or gh is None:
        raise RepositoryContextError("checkpoint status requires git and gh on PATH")
    try:
        head_result = runner(  # nosec B603
            [git, "rev-parse", "HEAD"], cwd=context.root, capture_output=True,
            text=True, check=False,
        )
        if head_result.returncode:
            raise RepositoryContextError(head_result.stderr.strip() or "cannot resolve current HEAD")
        head = head_result.stdout.strip()
        pull_requests = runner(  # nosec B603
            [
                gh,
                "api",
                f"repos/{context.repo}/commits/{head}/pulls",
                "--paginate",
                "--slurp",
            ],
            cwd=context.root, capture_output=True, text=True, check=False,
        )
    except (OSError, subprocess.SubprocessError) as error:
        raise RepositoryContextError(f"cannot query checkpoint status: {error}") from error
    if pull_requests.returncode:
        raise RepositoryContextError(
            pull_requests.stderr.strip() or "cannot query open pull requests"
        )
    try:
        pages = json.loads(pull_requests.stdout or "[]")
    except json.JSONDecodeError as error:
        raise RepositoryContextError(f"invalid pull-request response: {error}") from error
    if not isinstance(pages, list):
        raise RepositoryContextError("invalid pull-request response: expected page arrays")
    listed: list[dict] = []
    for page_number, page in enumerate(pages):
        if not isinstance(page, list):
            raise RepositoryContextError(
                f"invalid pull-request response: page {page_number} is not an array"
            )
        for item_number, item in enumerate(page):
            valid = (
                isinstance(item, dict)
                and isinstance(item.get("number"), int)
                and not isinstance(item.get("number"), bool)
                and isinstance(item.get("html_url"), str)
                and isinstance(item.get("state"), str)
                and isinstance(item.get("draft"), bool)
                and isinstance(item.get("head"), dict)
                and isinstance(item["head"].get("sha"), str)
            )
            if not valid:
                raise RepositoryContextError(
                    "invalid pull-request response: "
                    f"page {page_number} item {item_number} has an invalid shape"
                )
            listed.append(item)
    exact = next(
        (
            {
                "number": item.get("number"),
                "url": item.get("html_url"),
                "headRefOid": (item.get("head") or {}).get("sha"),
                "isDraft": bool(item.get("draft")),
            }
            for item in listed
            if item.get("state") == "open"
            and item["head"].get("sha") == head
        ),
        None,
    )
    return {**context_payload(context), "head": head, "pullRequest": exact}


def local_head(context: RepositoryContext) -> str:
    result = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=context.root, capture_output=True, text=True,
        timeout=10, check=False,
    )
    if result.returncode or not result.stdout.strip():
        raise RepositoryContextError(result.stderr.strip() or "cannot resolve local HEAD")
    return result.stdout.strip()


def build_parser() -> argparse.ArgumentParser:
    """Build the portable command parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    context = commands.add_parser("repository-context", help="resolve effective repository context")
    add_context_arguments(context)
    watch = commands.add_parser("watch-pr-checks", add_help=False, help="run the bounded PR watcher")
    watch.add_argument("arguments", nargs=argparse.REMAINDER)
    checkpoint = commands.add_parser("checkpoint-status", help="report HEAD and exact-head PR")
    add_context_arguments(checkpoint)
    plan = commands.add_parser("plan-validate", help="validate an evidence-backed plan")
    plan.add_argument("input", type=Path)
    audit = commands.add_parser("pr-audit", help="audit every PR feedback surface")
    add_context_arguments(audit)
    audit.add_argument("--dispositions", type=Path, required=True)
    audit.add_argument("--expected-head")
    audit.add_argument("--receipt-out", type=Path, required=True)
    delivery = commands.add_parser("delivery-status", help="verify owned PR merge and cleanup")
    delivery.add_argument("--manifest", type=Path, required=True)
    delivery.add_argument("--root", type=Path, default=Path.cwd())
    delivery.add_argument("--receipt-out", type=Path, required=True)
    issue_plan = commands.add_parser("issue-plan", help="validate and search a proposed issue")
    add_context_arguments(issue_plan)
    issue_plan.add_argument("--input", type=Path, required=True)
    issue_create = commands.add_parser("issue-create", help="create one confirmed validated issue")
    add_context_arguments(issue_create)
    issue_create.add_argument("--input", type=Path, required=True)
    issue_create.add_argument("--confirm-receipt-sha256", required=True)
    labels = commands.add_parser("issue-labels", help="dry-run or apply canonical label reconciliation")
    add_context_arguments(labels)
    labels.add_argument("--apply", action="store_true")
    transition = commands.add_parser("issue-transition", help="apply one validated lifecycle transition")
    add_context_arguments(transition)
    transition.add_argument("--issue", type=int, required=True)
    transition.add_argument("--input", type=Path, required=True)
    commands.add_parser("mcp", help="serve the read-only operations over MCP stdio")
    return parser


def _tool_schemas() -> list[dict]:
    context_properties = {
        "repo": {"type": "string"},
        "pr": {"type": ["integer", "string"]},
        "root": {"type": "string"},
    }
    return [
        {
            "name": "repository_context",
            "description": "Resolve the effective repository, root, and pull request.",
            "inputSchema": {"type": "object", "properties": context_properties, "additionalProperties": False},
        },
        {
            "name": "watch_pr_checks",
            "description": "Poll one pull request with bounded exit semantics.",
            "inputSchema": {"type": "object", "properties": context_properties, "additionalProperties": True},
        },
        {
            "name": "checkpoint_status",
            "description": "Report local HEAD and an exact-head open pull request.",
            "inputSchema": {"type": "object", "properties": context_properties, "additionalProperties": False},
        },
        {
            "name": "plan_validate",
            "description": "Validate a thorough evidence-backed implementation plan.",
            "inputSchema": {
                "type": "object",
                "properties": {"plan": {"type": "object"}},
                "required": ["plan"],
                "additionalProperties": False,
            },
        },
        {
            "name": "pr_audit",
            "description": "Return a paginated, head-bound pull-request feedback audit.",
            "inputSchema": {
                "type": "object",
                "properties": {
                    **context_properties,
                    "expectedHead": {"type": "string"},
                    "dispositions": {"type": "object"},
                },
                "required": ["pr", "dispositions"],
                "additionalProperties": False,
            },
        },
        {
            "name": "delivery_status",
            "description": "Verify all owned pull requests are live-merged and scoped cleanup is complete.",
            "inputSchema": {
                "type": "object", "properties": {
                    "manifest": {"type": "object"}, "root": {"type": "string"},
                }, "required": ["manifest"], "additionalProperties": False,
            },
        },
        {
            "name": "issue_plan",
            "description": "Validate an issue plan and search open and closed duplicates.",
            "inputSchema": {"type": "object", "properties": {
                **context_properties, "plan": {"type": "object"}, "taxonomy": {"type": "object"}
            }, "required": ["plan", "taxonomy"], "additionalProperties": False},
        },
    ]


def _namespace(arguments: dict) -> argparse.Namespace:
    return argparse.Namespace(
        repo=arguments.get("repo"),
        pr=arguments.get("pr"),
        root=Path(arguments["root"]) if arguments.get("root") else None,
    )


def _watch_arguments(arguments: dict) -> list[str]:
    result: list[str] = []
    for name in ("pr", "repo", "root", "max_polls", "interval"):
        if arguments.get(name) is not None:
            result.extend(("--" + name.replace("_", "-"), str(arguments[name])))
    if arguments.get("poll_once"):
        result.append("--poll-once")
    return result


def call_tool(name: str, arguments: dict) -> dict:
    """Call one MCP tool and return an MCP content result."""
    try:
        if name == "repository_context":
            payload = context_payload(context_from_arguments(_namespace(arguments)))
            return {"content": [{"type": "text", "text": json.dumps(payload, sort_keys=True)}]}
        if name == "checkpoint_status":
            payload = checkpoint_status(context_from_arguments(_namespace(arguments)))
            return {"content": [{"type": "text", "text": json.dumps(payload, sort_keys=True)}]}
        if name == "watch_pr_checks":
            stdout = io.StringIO()
            stderr = io.StringIO()
            with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
                exit_code = watch_pr_checks.main(_watch_arguments(arguments))
            payload = {"exitCode": exit_code, "stdout": stdout.getvalue(), "stderr": stderr.getvalue()}
            return {
                "content": [{"type": "text", "text": json.dumps(payload, sort_keys=True)}],
                "isError": exit_code != 0,
            }
        if name == "plan_validate":
            violations = validate_plan(arguments.get("plan"))
            payload = {"valid": not violations, "violations": violations}
            return {
                "content": [{"type": "text", "text": json.dumps(payload, sort_keys=True)}],
                "isError": bool(violations),
            }
        if name == "pr_audit":
            context = context_from_arguments(_namespace(arguments))
            if context.pr_number is None:
                raise RepositoryContextError("pr_audit requires an explicit pull request")
            snapshot = collect_pr_snapshot(
                GitHubClient(context.repo, root=context.root), context.pr_number
            )
            payload = audit_snapshot(
                snapshot, arguments.get("dispositions"),
                expected_head=arguments.get("expectedHead"),
            )
            return {
                "content": [{"type": "text", "text": json.dumps(payload, sort_keys=True)}],
                "isError": payload["decision"] != "allow",
            }
        if name == "delivery_status":
            manifest = arguments.get("manifest")
            if not isinstance(manifest, dict):
                raise RepositoryContextError("delivery manifest must be an object")
            context = context_from_arguments(_namespace({"root": arguments.get("root")}))
            payload = evaluate_delivery(
                manifest, collect_delivery(manifest, default_root=context.root), inspect_cleanup(manifest),
                execution_repository=context.repo, execution_head=local_head(context),
            )
            return {
                "content": [{"type": "text", "text": json.dumps(payload, sort_keys=True)}],
                "isError": payload["decision"] != "allow",
            }
        if name == "issue_plan":
            context = context_from_arguments(_namespace(arguments))
            receipt = prepare_issue_plan(arguments.get("plan"), arguments.get("taxonomy"), context.repo)
            receipt["sha256"] = receipt_digest(
                validate_issue_plan(receipt.get("normalizedPlan"), arguments.get("taxonomy"))
            )
            return {"content": [{"type": "text", "text": json.dumps(receipt, sort_keys=True)}], "isError": receipt["decision"] != "allow"}
        return {"content": [{"type": "text", "text": f"unknown tool: {name}"}], "isError": True}
    except (RepositoryContextError, GitHubUnavailable, ValueError) as error:
        return {"content": [{"type": "text", "text": str(error)}], "isError": True}


class _McpError(ValueError):
    """A JSON-RPC error with its standard wire code."""

    def __init__(
        self,
        code: int,
        message: str,
        *,
        request_id=None,
        notification: bool = False,
    ):
        super().__init__(message)
        self.code = code
        self.request_id = request_id
        self.notification = notification


def _parse_mcp_request(line: str) -> tuple[object, str, dict, bool]:
    """Parse and validate one JSON-RPC request envelope."""
    try:
        request = json.loads(line)
    except json.JSONDecodeError as error:
        raise _McpError(-32700, f"parse error: {error}") from error
    if not isinstance(request, dict):
        raise _McpError(-32600, "invalid request: expected a JSON object")
    request_id = request.get("id")
    method = request.get("method")
    if request.get("jsonrpc") != "2.0" or not isinstance(method, str):
        raise _McpError(-32600, "invalid request: jsonrpc must be 2.0 and method a string")
    if "id" in request and not (
        request_id is None
        or isinstance(request_id, str)
        or (isinstance(request_id, (int, float)) and not isinstance(request_id, bool))
    ):
        raise _McpError(-32600, "invalid request: id must be null, a string, or a number")
    notification = "id" not in request
    params = request.get("params", {})
    if params is None:
        params = {}
    if not isinstance(params, dict):
        raise _McpError(
            -32602,
            "invalid params: expected an object",
            request_id=request_id,
            notification=notification,
        )
    return request_id, method, params, notification


def _dispatch_mcp_request(method: str, params: dict, request_id) -> object:
    """Dispatch one validated JSON-RPC request."""
    if method == "initialize":
        return {
            "protocolVersion": params.get("protocolVersion", "2025-06-18"),
            "capabilities": {"tools": {}},
            "serverInfo": {"name": "chaosengine", "version": "1"},
        }
    if method == "tools/list":
        return {"tools": _tool_schemas()}
    if method == "tools/call":
        arguments = params.get("arguments", {})
        if arguments is None:
            arguments = {}
        if not isinstance(arguments, dict):
            raise _McpError(
                -32602,
                "invalid params: tool arguments must be an object",
                request_id=request_id,
            )
        return call_tool(str(params.get("name") or ""), arguments)
    if method == "shutdown":
        return None
    raise _McpError(-32601, f"method not found: {method}", request_id=request_id)


def serve_mcp(stdin=None, stdout=None) -> int:
    """Serve newline-delimited JSON-RPC without writing diagnostics to stdout."""
    stdin = sys.stdin if stdin is None else stdin
    stdout = sys.stdout if stdout is None else stdout
    for line in stdin:
        try:
            request_id, method, params, notification = _parse_mcp_request(line)
            if notification:
                continue
            result = _dispatch_mcp_request(method, params, request_id)
            response = {"jsonrpc": "2.0", "id": request_id, "result": result}
        except _McpError as error:
            if error.notification:
                continue
            response = {
                "jsonrpc": "2.0", "id": error.request_id,
                "error": {"code": error.code, "message": str(error)},
            }
        stdout.write(json.dumps(response, separators=(",", ":")) + "\n")
        stdout.flush()
    return 0


def main(argv: list[str] | None = None) -> int:
    """Run the portable CLI."""
    arguments = list(sys.argv[1:] if argv is None else argv)
    if arguments and arguments[0] == "watch-pr-checks":
        return watch_pr_checks.main(arguments[1:])
    if arguments and arguments[0] == "mcp":
        return serve_mcp()
    parsed = build_parser().parse_args(arguments)
    if parsed.command == "plan-validate":
        try:
            plan = json.loads(parsed.input.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            print(f"act-as-mohab: cannot read plan: {error}", file=sys.stderr)
            return 1
        violations = validate_plan(plan)
        print(json.dumps({"valid": not violations, "violations": violations}, sort_keys=True))
        return 1 if violations else 0
    if parsed.command == "pr-audit":
        try:
            context = context_from_arguments(parsed)
            if context.pr_number is None:
                raise RepositoryContextError("pr-audit requires --pr")
            dispositions = json.loads(parsed.dispositions.read_text(encoding="utf-8"))
            snapshot = collect_pr_snapshot(GitHubClient(context.repo, root=context.root), context.pr_number)
            payload = audit_snapshot(snapshot, dispositions, expected_head=parsed.expected_head)
        except (OSError, json.JSONDecodeError, RepositoryContextError, GitHubUnavailable, ValueError) as error:
            print(f"act-as-mohab: PR audit unavailable: {error}", file=sys.stderr)
            return EXIT_ENVIRONMENT_ERROR
        parsed.receipt_out.parent.mkdir(parents=True, exist_ok=True)
        parsed.receipt_out.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="utf-8")
        print(json.dumps(payload, sort_keys=True))
        return 0 if payload["decision"] == "allow" else 1
    if parsed.command == "delivery-status":
        try:
            manifest = json.loads(parsed.manifest.read_text(encoding="utf-8"))
            if not isinstance(manifest, dict):
                raise ValueError("delivery manifest must be an object")
            context = resolve_repository_context(explicit_root=parsed.root.resolve(), cwd=parsed.root.resolve())
            payload = evaluate_delivery(
                manifest, collect_delivery(manifest, default_root=context.root), inspect_cleanup(manifest),
                execution_repository=context.repo, execution_head=local_head(context),
            )
            parsed.receipt_out.parent.mkdir(parents=True, exist_ok=True)
            parsed.receipt_out.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="utf-8")
        except (OSError, json.JSONDecodeError, RepositoryContextError, GitHubUnavailable, ValueError) as error:
            print(f"act-as-mohab: delivery status unavailable: {error}", file=sys.stderr)
            return EXIT_ENVIRONMENT_ERROR
        print(json.dumps(payload, sort_keys=True))
        return 0 if payload["decision"] == "allow" else 1
    if parsed.command in {"issue-plan", "issue-create", "issue-labels", "issue-transition"}:
        try:
            context = context_from_arguments(parsed)
            taxonomy = json.loads((context.root / ".github/issue-taxonomy.json").read_text(encoding="utf-8"))
            plan = json.loads(parsed.input.read_text(encoding="utf-8")) if parsed.command != "issue-labels" else None
            if parsed.command == "issue-labels":
                payload = reconcile_labels(context.repo, taxonomy, apply=parsed.apply)
            elif parsed.command == "issue-transition":
                payload = transition_issue(context.repo, parsed.issue, plan, taxonomy)
            elif parsed.command == "issue-plan":
                payload = prepare_issue_plan(plan, taxonomy, context.repo)
                payload["sha256"] = receipt_digest(
                    validate_issue_plan(payload.get("normalizedPlan"), taxonomy)
                )
            else:
                payload = create_issue(
                    plan, taxonomy, context.repo, parsed.confirm_receipt_sha256
                )
        except (OSError, json.JSONDecodeError, RepositoryContextError, ValueError) as error:
            print(f"act-as-mohab: issue filing failed: {error}", file=sys.stderr)
            return EXIT_ENVIRONMENT_ERROR
        print(json.dumps(payload, sort_keys=True))
        return 0 if payload["decision"] == "allow" else 1
    try:
        context = context_from_arguments(parsed)
        payload = (
            context_payload(context)
            if parsed.command == "repository-context"
            else checkpoint_status(context)
        )
    except RepositoryContextError as error:
        print(f"act-as-mohab: {error}", file=sys.stderr)
        return EXIT_ENVIRONMENT_ERROR
    print(json.dumps(payload, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
