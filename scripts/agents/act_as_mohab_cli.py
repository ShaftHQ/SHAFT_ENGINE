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
except ModuleNotFoundError:
    from repository_context import RepositoryContext, RepositoryContextError, resolve_repository_context
    import watch_pr_checks


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
        return {"content": [{"type": "text", "text": f"unknown tool: {name}"}], "isError": True}
    except RepositoryContextError as error:
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
