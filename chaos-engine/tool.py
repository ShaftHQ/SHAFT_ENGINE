#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import hashlib
import importlib.util
import os
import re
import runpy
import shutil
import subprocess  # nosec B404 - executes only fixed owned tool names.
import sys
from pathlib import Path


TOOLS = {"uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"}
# Memory writes the shared default-branch store; advisory tools may still query when doctor-healthy.
MEMORY_ORIGIN_MAIN_TOOLS = frozenset({"memory", "memory-mcp"})
ADVISORY_ORIGIN_MAIN_TOOLS = frozenset({"mempalace", "mempalace-mcp", "graphify"})
# #6216: the default branch is resolved from origin/HEAD, never hard-coded; these
# conventional names are only the fallback when origin/HEAD is unset.
DEFAULT_REMOTE = "origin"
DEFAULT_BRANCH_FALLBACKS = ("main", "master")
HELP_FLAGS = frozenset({"--help", "-h", "help"})
_CAPTURE_STORES = frozenset({"mempalace", "graphify"})


ENTRY_FILES = (
    "identity.md",
    "skills/chaos-engine/SKILL.md",
    "companions/caveman-ultra.md",
    "companions/ponytail-ultra.md",
)
_COMPANION_ENTRY = {
    "caveman": "companions/caveman-ultra.md",
    "ponytail": "companions/ponytail-ultra.md",
}


def stopped_companions(opt_out: str) -> frozenset[str]:
    """Ultra cards stay in the entry bundle until an explicit stop (#autoload).

    ``stop caveman``, ``stop ponytail``, and ``normal mode`` are the only offs.
    Vendor skill bodies are never part of this bundle.
    """
    folded = str(opt_out or "").casefold()
    if "normal mode" in folded:
        return frozenset(_COMPANION_ENTRY)
    stopped = set()
    if "stop caveman" in folded:
        stopped.add("caveman")
    if "stop ponytail" in folded:
        stopped.add("ponytail")
    return frozenset(stopped)


def entry_files(opt_out: str = "") -> tuple[str, ...]:
    stopped = stopped_companions(opt_out)
    return tuple(
        relative for relative in ENTRY_FILES
        if relative not in { _COMPANION_ENTRY[name] for name in stopped }
    )
ENTRY_RETRIEVE = (
    "Next: run `python3 .chaos-engine/tool.py retrieve --store graphify|mempalace "
    '"<q>"` before the first broad search, then record `retrieve: used` or '
    "`skipped(<reason>)`.\n"
)


def entry_bundle(installed_root: Path, opt_out: str = "") -> str:
    """#6377: one startup bundle for bots that auto-load nothing (Grok Bot, GPTs)."""
    parts = []
    for relative in entry_files(opt_out):
        path = installed_root / relative
        if path.is_file():
            parts.append(f"<!-- {relative} -->\n{path.read_text(encoding='utf-8').strip()}\n")
    parts.append(ENTRY_RETRIEVE)
    return "\n".join(parts)


def entry_output(installed_root: Path, full: bool = False, opt_out: str = "") -> str:
    """Installed re-runs print one line while the bundle is unchanged (token economy)."""
    bundle = entry_bundle(installed_root, opt_out)
    if installed_root.name != ".chaos-engine":
        return bundle
    digest = hashlib.sha256(bundle.encode("utf-8")).hexdigest()[:12]
    stamp = installed_root / "runtime" / "entry-stamp"
    try:
        unchanged = stamp.read_text(encoding="utf-8").strip() == digest
    except OSError:
        unchanged = False
    if unchanged and not full:
        return (
            f"ChaosEngine entry unchanged ({digest}); new conversation, context summary, or cards not in context: "
            "run `python3 .chaos-engine/tool.py entry --full`.\n"
        )
    try:
        stamp.parent.mkdir(parents=True, exist_ok=True)
        stamp.write_text(digest + "\n", encoding="utf-8")
    except OSError:
        pass  # read-only installs simply print the bundle every time
    return bundle


MAINTAIN_STASH = "chaos-engine-maintain"
MAINTAIN_RELOAD = (
    "Reload now: re-read `.chaos-engine/skills/chaos-engine/SKILL.md` and both companion "
    "cards (bots without hooks: `tool.py entry`, see references/bot-entry.md).\n"
)


def _maintain_git(project: Path, *args: str, runner=subprocess.run) -> subprocess.CompletedProcess:
    return runner(  # nosec B603 - fixed Git argv, no shell.
        [_git_executable(), *args], cwd=project, capture_output=True, text=True, check=False
    )


def maintain_sync(project: Path, *, runner=subprocess.run) -> tuple[bool, str]:
    """Fast-forward the default-branch checkout, keeping tracked edits (#6377)."""
    def git(*args: str) -> subprocess.CompletedProcess:
        return _maintain_git(project, *args, runner=runner)

    branch = default_branch_name(project)
    current = git("rev-parse", "--abbrev-ref", "HEAD").stdout.strip()
    if current != branch:
        return True, f"sync: skipped (on {current or 'detached HEAD'}, not {branch})"
    if git("fetch", DEFAULT_REMOTE, branch).returncode != 0:
        return False, f"sync: failed (git fetch {DEFAULT_REMOTE} {branch})"
    behind = git("rev-list", "--count", f"HEAD..{DEFAULT_REMOTE}/{branch}").stdout.strip()
    if behind in {"", "0"}:
        return True, "sync: current"
    dirty = bool(git("status", "--porcelain", "--untracked-files=no").stdout.strip())
    if dirty and git("stash", "push", "-m", MAINTAIN_STASH).returncode != 0:
        return False, "sync: failed (could not stash tracked edits)"
    merged = git("merge", "--ff-only", f"{DEFAULT_REMOTE}/{branch}").returncode == 0
    if dirty and git("stash", "pop").returncode != 0:
        return False, f"sync: conflict restoring edits; they stay in `git stash list` ({MAINTAIN_STASH})"
    if not merged:
        return False, f"sync: failed (not a fast-forward of {DEFAULT_REMOTE}/{branch})"
    return True, f"sync: fast-forwarded {behind} commit(s)" + (", edits kept" if dirty else "")


MAINTAIN_UNKNOWN_SOURCE = (
    "maintain: cannot recover the install source from its digest; rerun the install one-liner "
    "(or set CHAOS_ENGINE_REPOSITORY=owner/repository) (#6664)"
)


def _digest(value: str) -> str:
    return hashlib.sha256(value.encode()).hexdigest()


def _origin_repository(project: Path) -> str:
    """`owner/name` of the project's origin remote, or "" when it is not a GitHub remote."""
    try:
        url = _maintain_git(project, "remote", "get-url", DEFAULT_REMOTE).stdout.strip()
    except (OSError, ValueError):
        return ""
    match = re.search(r"github\.com[:/]([\w.-]+/[\w.-]+?)(?:\.git)?/?$", url)
    return match.group(1) if match else ""


def _match_digest(digest: object, candidates: tuple[str, ...], *, fold: bool = False) -> str:
    """First candidate whose sha256 equals the recorded digest, else ""."""
    for candidate in candidates:
        if candidate and _digest(candidate.casefold() if fold else candidate) == digest:
            return candidate
    return ""


def recorded_source(source: dict, project: Path) -> tuple[str, str]:
    """Plain `repository`/`branch` win; git-digest installs match candidates by sha256 (#6664)."""
    repository = str(source.get("repository") or "") or _match_digest(
        source.get("repositorySha256"),
        (os.environ.get("CHAOS_ENGINE_REPOSITORY", ""), _origin_repository(project)), fold=True)
    branch = str(source.get("branch") or "") or _match_digest(
        source.get("branchSha256"), (default_branch_name(project), *DEFAULT_BRANCH_FALLBACKS))
    return repository, branch


def maintain_commands(installed_root: Path, project: Path) -> list[list[str]]:
    """Reinstall from the recorded source, then doctor and a stale-store refresh.

    Raises ValueError when the source cannot be recovered; empty values are never passed on.
    """
    import json

    manifest = json.loads((installed_root / "manifest.json").read_text(encoding="utf-8"))
    repository, branch = recorded_source(manifest.get("source", {}), project)
    if not repository:
        raise ValueError(MAINTAIN_UNKNOWN_SOURCE)
    python = sys.executable
    reinstall = [python, str(installed_root / "bootstrap.py"), "--project", str(project), "--repository", repository]
    if branch:
        reinstall += ["--branch", branch]
    distribution = str(manifest.get("distribution", {}).get("id", ""))
    if distribution:
        reinstall += ["--distribution", distribution]
    return [
        reinstall,
        [python, str(installed_root / "install.py"), "doctor", "--project", str(project)],
        [python, str(installed_root / "tool.py"), "stores", "refresh", "--if-stale"],
    ]


def maintain(installed_root: Path, *, runner=subprocess.run) -> int:
    """`tool.py maintain`: sync, reinstall, doctor, refresh, reload (#6377)."""
    project = shared_project_root(installed_root.resolve().parent)
    ok, summary = maintain_sync(project, runner=runner)
    print(summary)
    if not ok:
        return 1
    try:
        commands = maintain_commands(installed_root, project)
    except ValueError as error:
        print(error, file=sys.stderr)
        return 1
    for command in commands:
        completed = runner(command, cwd=project, check=False)  # nosec B603 - fixed owned argv.
        if completed.returncode != 0:
            print(f"maintain: failed at `{' '.join(command[1:3])}`", file=sys.stderr)
            return completed.returncode or 1
    print(MAINTAIN_RELOAD, end="")
    return 0


def tool_help_text() -> str:
    """One usage text for every host. There is no per-host help."""
    names = ", ".join(sorted(TOOLS | {"retrieve", "entry", "maintain", "job"}))
    return (
        "usage: tool.py <tool|retrieve> [args...]\n"
        "       tool.py --help\n"
        "\n"
        f"tools: {names}\n"
        "\n"
        "entry (bots without hooks: print core card, companions, retrieve step):\n"
        "  tool.py entry [--full] [stop phrase]  (one line while unchanged; --full reprints)\n"
        "\n"
        "maintain (after each delivery: fast-forward, reinstall, doctor, refresh, reload):\n"
        "  tool.py maintain\n"
        "\n"
        "retrieve (MemPalace or Graphify checks justify later file reads):\n"
        "  tool.py retrieve [--store {memory,mempalace,graphify,deja}] [--project PATH] [--dry-run] QUERY\n"
        "  tool.py mempalace search QUERY\n"
        "  tool.py graphify query QUERY\n"
        "  tool.py stores refresh [--if-stale]\n"
        "  tool.py stores install-schedule\n"
        "  tool.py stores status\n"
        "\n"
        "job (long work that outlives the session; one worker per job; references/durable-jobs.md):\n"
        "  tool.py job start NAME [--heartbeat S] [--stale S] [--part-dir D]... -- CMD...\n"
        "  tool.py job status NAME [--json]  (exit 0 live/done, 3 stale, 4 failed, 5 stopped, 6 absent)\n"
        "  tool.py job wait NAME [--timeout S] [--tail N]  (block until not live; 7 = still live)\n"
        "  tool.py job resume NAME  (watchdog: no-op while live or done)\n"
        "  tool.py job stop NAME\n"
        "  tool.py job checkpoint NAME STEP [--check]\n"
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


def default_branch_name(root: Path) -> str:
    """Resolve the remote default branch from origin/HEAD, else a conventional fallback."""
    try:
        git = _git_executable()
        symbolic = subprocess.run(  # nosec B603 - fixed Git query, no shell.
            [git, "symbolic-ref", "--quiet", f"refs/remotes/{DEFAULT_REMOTE}/HEAD"],
            cwd=root, capture_output=True, text=True, check=False,
        ).stdout.strip()
        prefix = f"refs/remotes/{DEFAULT_REMOTE}/"
        if symbolic.startswith(prefix):
            return symbolic[len(prefix):]
        for name in DEFAULT_BRANCH_FALLBACKS:
            probe = subprocess.run(  # nosec B603 - fixed Git query, no shell.
                [git, "rev-parse", "--verify", "--quiet", f"{prefix}{name}"],
                cwd=root, capture_output=True, text=True, check=False,
            )
            if probe.returncode == 0:
                return name
    except (OSError, ValueError):
        # No git or no probe: fall back to the first documented default below.
        pass
    return DEFAULT_BRANCH_FALLBACKS[0]


def default_remote_ref(root: Path) -> str:
    return f"{DEFAULT_REMOTE}/{default_branch_name(root)}"


def sync_fix_next(root: Path) -> str:
    branch = default_branch_name(root)
    return (
        f"git fetch {DEFAULT_REMOTE} {branch} && "
        f"git merge --ff-only {DEFAULT_REMOTE}/{branch}"
    )


def origin_main_revisions(root: Path) -> tuple[str, str]:
    """Return (HEAD, remote default-branch tip) for the primary checkout."""
    revisions = subprocess.run(  # nosec B603 - fixed Git query, no shell.
        [_git_executable(), "rev-parse", "HEAD", f"refs/remotes/{default_remote_ref(root)}"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.splitlines()
    if len(revisions) != 2:
        ref = default_remote_ref(root)
        raise ValueError(
            f"primary checkout HEAD and {ref} could not both be resolved "
            f"(not synchronized with {ref}). fix-next: {sync_fix_next(root)}"
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
    """#6179: detached session worktrees never see the default-branch desync notice."""
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


def origin_main_desync_message(head: str, origin_main: str, root: Path | None = None) -> str:
    """Name HEAD != the default-branch tip and print the fast-forward fix-next (#5591)."""
    root = root or Path.cwd()
    ref = default_remote_ref(root)
    return (
        f"primary checkout HEAD ({head}) != {ref} ({origin_main}) "
        f"(not synchronized with {ref}). fix-next: {sync_fix_next(root)}"
    )


def enforce_tool_origin_main_policy(project: Path, tool: str) -> None:
    """Hard-fail Memory tools when HEAD != the default branch; soft-warn advisory tools."""
    if not (project / "tools/repository-map/resolve_mempalace.py").is_file():
        return
    root = shared_project_root(project)
    head, origin_main = origin_main_revisions(root)
    if head == origin_main:
        return
    message = origin_main_desync_message(head, origin_main, root)
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
        if tool == "entry":
            phrase = " ".join(arg for arg in arguments if arg != "--full")
            print(entry_output(installed_root, full="--full" in arguments, opt_out=phrase), end="")
            return 0
        if tool == "maintain":
            return maintain(installed_root)
        if tool in {"dod", "usage"}:
            script = "dod.py" if tool == "dod" else "session_token_usage.py"
            module = runpy.run_path(str(installed_root / script), run_name=f"_chaos_engine_{tool}")
            return int(module["main"](arguments if tool == "dod" else ["usage", *arguments]))
        if tool == "stores":
            return stores_command(installed_root, arguments)
        if tool == "job":
            # #6525: durable jobs outlive the agent session (lease, heartbeat, checkpoints).
            module = runpy.run_path(str(installed_root / "jobs.py"), run_name="_chaos_engine_job")
            return int(module["main"](arguments))
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
