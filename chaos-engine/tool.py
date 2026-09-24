#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import importlib.util
import os
import runpy
import shutil
import subprocess  # nosec B404 - executes only fixed owned tool names.
import sys
from pathlib import Path


TOOLS = {"uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"}
# Memory writes the shared origin/main store; advisory tools may still query when doctor-healthy.
MEMORY_ORIGIN_MAIN_TOOLS = frozenset({"memory", "memory-mcp"})
ADVISORY_ORIGIN_MAIN_TOOLS = frozenset({"mempalace", "mempalace-mcp", "graphify"})
ORIGIN_MAIN_SYNC_FIX_NEXT = "git fetch origin main && git merge --ff-only origin/main"
HELP_FLAGS = frozenset({"--help", "-h", "help"})
_CAPTURE_STORES = frozenset({"mempalace", "graphify"})


def tool_help_text() -> str:
    """One usage text for every host. There is no per-host help."""
    names = ", ".join(sorted(TOOLS | {"retrieve"}))
    return (
        "usage: tool.py <tool|retrieve> [args...]\n"
        "       tool.py --help\n"
        "\n"
        f"tools: {names}\n"
        "\n"
        "retrieve (MemPalace or Graphify checks justify later file reads):\n"
        "  tool.py retrieve [--store {memory,mempalace,graphify}] [--project PATH] [--dry-run] QUERY\n"
        "  tool.py mempalace search QUERY\n"
        "  tool.py graphify query QUERY\n"
        "  tool.py stores refresh [--if-stale]\n"
        "  tool.py stores install-schedule\n"
        "  tool.py stores status\n"
    )


def _record_store_output(installed_root: Path, store: str, text: str) -> None:
    path = installed_root / "hooks" / "retrieve_justification.py"
    if not path.is_file() or not text.strip():
        return
    spec = importlib.util.spec_from_file_location("chaos_engine_retrieve_justification", path)
    if spec is None or spec.loader is None:
        return
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.record_citations(
        shared_project_root(installed_root.resolve().parent),
        store,
        text,
    )


def _git_executable() -> str:
    """Resolve git once to an absolute path (Bandit B607 parity, #6165)."""
    git = shutil.which("git")
    if git is None:
        raise FileNotFoundError("git executable not found on PATH")
    return git


def shared_project_root(project: Path) -> Path:
    """Resolve the primary checkout that owns shared MemPalace / Memory / Graphify state."""
    if not (project / "tools/repository-map/resolve_mempalace.py").is_file():
        return project.resolve()
    completed = subprocess.run(  # nosec B603 - fixed Git query, no shell.
        [_git_executable(), "rev-parse", "--git-common-dir"],
        cwd=project,
        capture_output=True,
        text=True,
        check=True,
    )
    common = Path(completed.stdout.strip())
    if not common.is_absolute():
        common = (project / common).resolve()
    return common.parent.resolve()


def origin_main_revisions(root: Path) -> tuple[str, str]:
    """Return (HEAD, refs/remotes/origin/main) for the primary checkout."""
    revisions = subprocess.run(  # nosec B603 - fixed Git query, no shell.
        [_git_executable(), "rev-parse", "HEAD", "refs/remotes/origin/main"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.splitlines()
    if len(revisions) != 2:
        raise ValueError(
            "primary checkout HEAD and origin/main could not both be resolved "
            f"(not synchronized with origin/main). fix-next: {ORIGIN_MAIN_SYNC_FIX_NEXT}"
        )
    return revisions[0], revisions[1]


QUIET_ENVIRONMENT = {
    "TQDM_DISABLE": "1",
    "HF_HUB_DISABLE_PROGRESS_BARS": "1",
    "TRANSFORMERS_VERBOSITY": "error",
    "TOKENIZERS_PARALLELISM": "false",
}


def quiet_environment(environment: dict[str, str]) -> dict[str, str]:
    """#6179: retrieval output carries answers, not download progress bars."""
    return {**environment, **QUIET_ENVIRONMENT}


def desync_notice_applies(repo: Path) -> bool:
    """#6179: detached session worktrees never see the origin/main desync notice."""
    try:
        completed = subprocess.run(  # nosec B603 - fixed Git query, no shell.
            [_git_executable(), "symbolic-ref", "-q", "HEAD"],
            cwd=repo,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return True
    # `symbolic-ref -q` exits 1 only for a detached HEAD; other failures keep the notice.
    return completed.returncode != 1


def origin_main_desync_message(head: str, origin_main: str) -> str:
    """Name HEAD != origin/main and print the fast-forward fix-next (#5591)."""
    return (
        f"primary checkout HEAD ({head}) != origin/main ({origin_main}) "
        f"(not synchronized with origin/main). fix-next: {ORIGIN_MAIN_SYNC_FIX_NEXT}"
    )


def enforce_tool_origin_main_policy(project: Path, tool: str) -> None:
    """Hard-fail Memory tools when HEAD != origin/main; soft-warn advisory tools."""
    if not (project / "tools/repository-map/resolve_mempalace.py").is_file():
        return
    root = shared_project_root(project)
    head, origin_main = origin_main_revisions(root)
    if head == origin_main:
        return
    message = origin_main_desync_message(head, origin_main)
    retrieve = os.environ.get("CHAOS_ENGINE_RETRIEVE") == "1"
    if tool in MEMORY_ORIGIN_MAIN_TOOLS and not retrieve:
        raise ValueError(message)
    if (tool in ADVISORY_ORIGIN_MAIN_TOOLS or retrieve) and desync_notice_applies(project):
        print(f"warning: {message}", file=sys.stderr)


def load_host_controller(installed_root: Path):
    """Load the colocated state classifier without requiring a Python package."""
    path = installed_root / "hosts.py"
    if not path.is_file():
        raise ValueError("ChaosEngine host controller could not be loaded")
    return runpy.run_path(str(path), run_name="_chaos_engine_runtime_hosts")


def _load_stores(installed_root: Path):
    """Load the colocated store resolver, including a source tree beside tool.py."""
    candidates = (
        installed_root / "stores.py",
        Path(__file__).resolve().with_name("stores.py"),
    )
    for path in candidates:
        if path.is_file():
            return runpy.run_path(str(path), run_name="_chaos_engine_stores")
    raise ValueError("ChaosEngine store resolver could not be loaded")


def bind_store_invocation(tool: str, invocation: list[str], project: Path, installed_root: Path) -> list[str]:
    """Pin MemPalace and Graphify invocations to the shared repository stores."""
    if tool not in {"mempalace", "mempalace-mcp", "graphify"} or len(invocation) < 2:
        return invocation
    stores = _load_stores(installed_root)
    if tool in {"mempalace", "mempalace-mcp"}:
        try:
            palace = stores["resolve_palace"](project)
        except RuntimeError as error:
            raise ValueError(str(error)) from error
        return [invocation[0], *stores["inject_mempalace_arguments"](invocation[1:], palace)]
    graph_json = stores["resolve_graph_out"](project) / "graph.json"
    return [invocation[0], *stores["inject_graphify_arguments"](invocation[1:], graph_json)]


def stores_command(installed_root: Path, arguments: list[str]) -> int:
    """Refresh, describe, or schedule the shared MemPalace and Graphify stores."""
    stores = _load_stores(installed_root)
    project = installed_root.resolve().parent
    action = arguments[0] if arguments else ""
    if action in HELP_FLAGS or action == "":
        print(
            "usage: tool.py stores refresh [--if-stale]\n"
            "       tool.py stores status\n"
            "       tool.py stores install-schedule\n",
            end="",
        )
        return 0 if action in HELP_FLAGS else 2
    if action == "status":
        fresh, message = stores["graph_freshness"](project)
        print(message)
        return 0 if fresh else 1
    if action == "install-schedule":
        print(stores["install_schedule"](project))
        return 0
    if action == "refresh":
        return int(stores["refresh"](project, if_stale="--if-stale" in arguments[1:]))
    raise ValueError(f"unsupported stores command: {action}")


def mempalace_mcp_arguments(installed_root: Path, arguments: list[str]) -> list[str]:
    """Resolve the one owned palace and return its native MCP arguments."""
    project = installed_root.resolve().parent
    if arguments:
        raise ValueError(
            "MemPalace MCP does not accept host-supplied storage arguments"
        )
    try:
        palace = _load_stores(installed_root)["resolve_palace"](project)
    except RuntimeError as error:
        raise ValueError(str(error)) from error
    if not palace.is_absolute():
        raise ValueError("MemPalace MCP resolver returned a relative path")
    palace = palace.resolve()
    controller = load_host_controller(installed_root)
    state = controller["mempalace_directory_status"](palace)
    if state.get("status") != "healthy":
        raise ValueError(
            f"MemPalace runtime is {state.get('status', 'unknown')}: "
            f"{state.get('detail', 'operator action required')}"
        )
    return ["--palace", str(palace), "--backend", "sqlite_exact"]


def guard_mempalace_mcp(installed_root: Path, arguments: list[str]) -> None:
    """Compatibility wrapper for callers that only need validation."""
    mempalace_mcp_arguments(installed_root, arguments)


def resolve_command(
    installed_root: Path, tool: str, arguments: list[str] | None = None
) -> list[str]:
    if tool not in TOOLS:
        raise ValueError(f"unsupported ChaosEngine tool: {tool}")
    installed_project = installed_root.resolve().parent
    project = shared_project_root(installed_project)
    enforce_tool_origin_main_policy(installed_project, tool)
    path = installed_root / "dependencies.py"
    if not path.is_file():
        cli = "py -3" if os.name == "nt" else "python3"
        raise ValueError(
            "ChaosEngine dependency controller could not be loaded "
            f"(wiped or incomplete `.chaos-engine` runtime). fix-next: run the "
            f"ChaosEngine install one-liner from INSTALL.md, then "
            f"`{cli} .chaos-engine/install.py doctor --project .`"
        )
    controller = runpy.run_path(str(path), run_name="_chaos_engine_runtime_dependencies")
    return controller["active_dispatch"](project, tool, arguments or [])



def resolve_maven_tools_command(installed_root: Path) -> list[str]:
    """Resolve java + cached jar via hosts discover_maven_tools_runtime (#5782)."""
    controller = load_host_controller(installed_root)
    discover = controller.get("discover_maven_tools_runtime")
    if not callable(discover):
        raise ValueError("maven-tools-mcp runtime discovery is unavailable")
    runtime = discover()
    if runtime is None:
        raise ValueError(
            "maven-tools-mcp cache runtime is absent; install with "
            "`--with-maven-tools` or use docker mode"
        )
    java, jar = runtime
    return [str(java), "-jar", str(jar)]


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: tool.py <tool|retrieve> [args...]", file=sys.stderr)
        print("try: tool.py --help", file=sys.stderr)
        return 2
    if sys.argv[1] in HELP_FLAGS:
        print(tool_help_text(), end="")
        return 0
    try:
        installed_root = Path(__file__).resolve().parent
        tool = sys.argv[1]
        arguments = sys.argv[2:]
        if tool == "stores":
            return stores_command(installed_root, arguments)
        if tool == "retrieve":
            path = installed_root / "retrieve.py"
            if not path.is_file():
                print("retrieve.py missing from installed core", file=sys.stderr)
                return 1
            # Re-exec as retrieve CLI (argv[0] style via runpy is awkward; call main).
            spec_mod = runpy.run_path(str(path), run_name="_chaos_engine_retrieve")
            return int(spec_mod["main"](arguments))
        if tool == "mempalace-mcp":
            arguments = mempalace_mcp_arguments(installed_root, arguments)
        if tool == "maven-tools-mcp":
            command = resolve_maven_tools_command(installed_root)
            environment = quiet_environment(os.environ.copy())
            environment["PYTHONDONTWRITEBYTECODE"] = "1"
            return subprocess.call(  # nosec B603
                [*command, *arguments],
                env=environment,
                cwd=shared_project_root(installed_root.resolve().parent),
            )
        command = resolve_command(installed_root, tool, arguments)
        environment = quiet_environment(os.environ.copy())
        environment["PYTHONDONTWRITEBYTECODE"] = "1"
        invocation = (
            command
            if isinstance(command, list)
            else [str(command), *arguments]  # Compatibility for injected legacy tests.
        )
        project = shared_project_root(installed_root.resolve().parent)
        invocation = bind_store_invocation(tool, invocation, project, installed_root)
        if tool in _CAPTURE_STORES:
            completed = subprocess.run(  # nosec B603
                invocation,
                env=environment,
                cwd=project,
                capture_output=True,
                text=True,
                check=False,
            )
            sys.stdout.write(completed.stdout)
            sys.stderr.write(completed.stderr)
            if completed.returncode == 0:
                try:
                    _record_store_output(
                        installed_root,
                        tool,
                        completed.stdout + "\n" + completed.stderr,
                    )
                except (OSError, RuntimeError, ValueError):
                    # The store text already printed. A ledger write must not hide it.
                    pass
            return completed.returncode
        return subprocess.call(  # nosec B603
            invocation,
            env=environment,
            cwd=project,
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
