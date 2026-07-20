#!/usr/bin/env python3
"""Local mirror of the checks that actually gate a merge in this repo."""
# Codacy runs both D212 and D213, so multi-line docstrings always flag
# one of them; detail therefore lives in this comment block.
#
#
# Issue #3302's retrospective found that PRs were passing local, ad-hoc QA
# and then breaking post-merge on exactly three CI gates. There is NO
# PR-blocking Maven test job in this repo. What actually blocks a merge is:
#
# 1. ``.github/workflows/security.yml``
#    - ``codeql`` job: GitHub CodeQL static analysis for Java, run against a
#      full multi-module build
#      (``mvn -pl shaft-engine,shaft-pilot-core,shaft-capture,shaft-doctor,
#      shaft-ai,shaft-heal,shaft-browserstack,shaft-video,shaft-visual,
#      shaft-sikulix,shaft-mcp -am clean package -DskipTests -Dgpg.skip``).
# 2. ``.github/workflows/pr-gate.yml`` (the required ``PR Gate Summary``
#    check, issue #3814): a single path-conditioned gate whose legs include
#    ``dependency-review`` (``actions/dependency-review-action`` against
#    ``.github/dependency-review-config.yml``, moved here out of
#    ``security.yml``) plus path-filtered docs-boundary, IntelliJ installer
#    verification, IntelliJ Gradle ``check buildPlugin verifyPlugin``,
#    shaft-cli build/test, SHAFT Capture browser E2E, and Template Coupling
#    legs. Out of scope for this script, which only mirrors the Maven
#    reactor gates; run Gradle directly for ``shaft-intellij`` changes.
# 3. The Maven Enforcer rules that fire on every ``mvn verify``/``package``/
#    ``install`` in the reactor:
#    - Root ``pom.xml`` ``<pluginManagement>`` (~line 555-605, execution id
#      ``enforce-toolchain``): ``requireJavaVersion``, ``requireMavenVersion``,
#      ``banDuplicatePomDependencyVersions``, ``dependencyConvergence``,
#      ``requireUpperBoundDeps``, ``bannedDependencies``.
#    - ``shaft-engine/pom.xml`` (~line 660-691, execution id
#      ``enforce-engine-dependency-boundary``): an additional
#      ``bannedDependencies`` rule that keeps the optional modules
#      (shaft-pilot-core, shaft-capture, shaft-doctor, shaft-ai, shaft-heal,
#      BrowserStack, desktop-video, visual-processing, SikuliX, etc.) out of
#      shaft-engine's own dependency graph.
#
# WHAT THIS SCRIPT CANNOT MIRROR LOCALLY:
#     Full CodeQL analysis requires the actual GitHub Action
#     (``github/codeql-action/init`` + ``analyze``) and its query packs; there
#     is no supported way to run equivalent CodeQL Java analysis from this
#     script. Do NOT claim CodeQL parity from a green run of this script --
#     it only proves the reactor compiles and the Enforcer rules
#     (dependencyConvergence, banned deps, toolchain) pass, which is the
#     other thing that silently broke post-merge in #3302. Likewise, the
#     ``dependency-review`` job diffs against GitHub's advisory database and
#     is not reproduced here.
#
# WHAT THIS SCRIPT DOES:
#     Runs a single scoped, test-free Maven reactor build:
#
#         mvn -pl <modules> -am -DskipTests -Dgpg.skip=true verify
#
#     ``-DskipTests`` skips Surefire/Failsafe entirely (no JVM fork storm,
#     compatible with the ``.claude/hooks/guard.py`` PreToolUse guard, which
#     explicitly allows this exact shape) while still running full compile
#     plus the Enforcer executions above, since Enforcer's default phase is
#     ``validate``/``verify`` regardless of ``-DskipTests``.
#
# Usage:
#     python3 scripts/ci/local_gate.py                      # auto-detect changed modules
#     python3 scripts/ci/local_gate.py --modules shaft-video # explicit scope
#     python3 scripts/ci/local_gate.py --dry-run             # print the mvn command only
#     python3 scripts/ci/local_gate.py --timeout 900         # bound the run
#
# Exit codes:
#     0   gate passed (or nothing relevant changed -- nothing to gate)
#     1   mvn verify failed (enforcer violation, compile error, etc.)
#     2   usage / environment error (e.g. mvn not on PATH, git failure)

from __future__ import annotations

import argparse
import shutil
# All subprocess use in this script is list-args (never shell=True) with
# executables resolved to absolute paths via shutil.which and arguments
# built from validated internal values only.
import subprocess  # nosec B404
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_TIMEOUT_SECONDS = 1800

# Lines worth surfacing first when a run fails: Enforcer violations and
# generic Maven ERROR lines are the two shapes #3302 broke on.
_FAILURE_MARKERS = ("ERROR", "[ERROR]", "Rule 0:", "enforce")
_TAIL_LINES = 60


def discover_module_dirs(root: Path) -> list[str]:
    """Return top-level directory names that contain a pom.xml."""
    modules = []
    for child in sorted(root.iterdir()):
        if child.is_dir() and (child / "pom.xml").is_file():
            modules.append(child.name)
    return modules


def run_git(args: list[str], root: Path) -> str:
    """Run one git command and return stripped stdout, raising on failure."""
    git_executable = shutil.which("git")
    if git_executable is None:
        raise RuntimeError("git is not on PATH")
    # List-args with an absolute executable and internally-built arguments;
    # nothing here is attacker-controlled input.
    completed = subprocess.run(  # nosec B603
        [git_executable, *args],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        raise RuntimeError(
            f"git {' '.join(args)} failed: {completed.stderr.strip() or completed.stdout.strip()}"
        )
    return completed.stdout.strip()


def changed_paths(root: Path) -> list[str]:
    """Return paths changed on this branch relative to origin/main."""
    # Prefers a three-dot diff against the merge base (origin/main...HEAD);
    # falls back to a plain two-dot diff against origin/main (matching the
    # semantics of `git diff --name-only origin/main`) if the merge base
    # cannot be resolved (e.g. shallow clone, detached history).
    try:
        run_git(["fetch", "--quiet", "origin", "main"], root)
    except RuntimeError:
        pass  # best effort; local origin/main ref may already be current

    try:
        output = run_git(
            ["diff", "--name-only", "origin/main...HEAD"], root
        )
        return [line for line in output.splitlines() if line]
    except RuntimeError:
        output = run_git(["diff", "--name-only", "origin/main"], root)
        return [line for line in output.splitlines() if line]


def map_paths_to_modules(paths: list[str], module_dirs: list[str]) -> tuple[set[str], bool]:
    """Map changed paths to top-level module directories."""
    # Returns (matched_modules, has_root_scope_change) where
    # has_root_scope_change is True if any changed path falls outside every
    # known module directory (root pom, scripts/, .github/, etc.) and
    # therefore requires the default reactor root scope.
    matched: set[str] = set()
    root_scope = False
    for path in paths:
        top = path.split("/", 1)[0]
        if top in module_dirs:
            matched.add(top)
        else:
            root_scope = True
    return matched, root_scope


def build_mvn_command(modules: list[str] | None) -> list[str]:
    """Build the mvn argument list for the merge-gate mirror."""
    command = ["mvn"]
    if modules:
        command += ["-pl", ",".join(modules)]
    command += ["-am", "-DskipTests", "-Dgpg.skip=true", "verify"]
    return command


def format_command_for_display(command: list[str]) -> str:
    """Render a command list as a copy-pasteable, PowerShell-safe string."""
    parts = []
    for part in command:
        if part.startswith("-D") and "=" in part:
            parts.append(f"'{part}'")
        else:
            parts.append(part)
    return " ".join(parts)


def surface_failure(stdout: str) -> str:
    """Extract enforcer/ERROR lines to the top, followed by the tail."""
    lines = stdout.splitlines()
    flagged = [line for line in lines if any(marker in line for marker in _FAILURE_MARKERS)]
    tail = lines[-_TAIL_LINES:]
    sections = []
    if flagged:
        sections.append("=== Flagged ERROR/enforcer lines ===")
        sections.append("\n".join(flagged))
    sections.append(f"=== Last {min(_TAIL_LINES, len(lines))} lines of output ===")
    sections.append("\n".join(tail))
    return "\n\n".join(sections)


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--modules",
        type=str,
        default=None,
        help="Comma-separated top-level module directories (e.g. shaft-video,shaft-doctor). "
        "If omitted, auto-detect from changed files vs origin/main.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the exact mvn command without executing it.",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=DEFAULT_TIMEOUT_SECONDS,
        help=f"Timeout in seconds for the mvn invocation (default: {DEFAULT_TIMEOUT_SECONDS}).",
    )
    parser.add_argument("--root", type=Path, default=ROOT)
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    root = args.root.resolve()

    modules: list[str] | None
    if args.modules:
        modules = [module.strip() for module in args.modules.split(",") if module.strip()]
    else:
        module_dirs = discover_module_dirs(root)
        try:
            paths = changed_paths(root)
        except RuntimeError as error:
            print(f"local_gate: git error: {error}", file=sys.stderr)
            return 2
        if not paths:
            print("local_gate: no changes detected vs origin/main; nothing to gate.")
            return 0
        matched, root_scope = map_paths_to_modules(paths, module_dirs)
        if root_scope:
            print(
                "local_gate: changes touch paths outside any module "
                "(root pom, scripts/, .github/, etc.); running default reactor root scope."
            )
            modules = None
        elif matched:
            modules = sorted(matched)
            print(f"local_gate: auto-detected changed modules: {', '.join(modules)}")
        else:
            print("local_gate: no module-relevant changes detected; nothing to gate.")
            return 0

    command = build_mvn_command(modules)

    if args.dry_run:
        print(format_command_for_display(command))
        return 0

    executable = shutil.which(command[0])
    if executable is None:
        print("local_gate: mvn is not on PATH", file=sys.stderr)
        return 2
    command[0] = executable

    print(f"local_gate: running {format_command_for_display(command)}")
    try:
        # List-args, absolute mvn path from shutil.which, no shell; the only
        # variable inputs are local module directory names.
        completed = subprocess.run(  # nosec B603
            command,
            cwd=root,
            capture_output=True,
            text=True,
            timeout=args.timeout,
            check=False,
        )
    except subprocess.TimeoutExpired:
        print(f"local_gate: mvn timed out after {args.timeout}s", file=sys.stderr)
        return 2

    if completed.returncode != 0:
        print(surface_failure(completed.stdout + "\n" + completed.stderr), file=sys.stderr)
        print(
            f"local_gate: mvn exited {completed.returncode} -- merge-gate mirror FAILED",
            file=sys.stderr,
        )
        return 1

    print("local_gate: PASSED (compile + enforcer convergence/banned-deps clean).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
