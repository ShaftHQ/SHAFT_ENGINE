"""
Install the secret guard as git pre-commit and pre-push hooks (#7177).

The hooks are written to ``<git common dir>/chaos-engine-hooks`` so they are never
tracked and serve every worktree of the clone. ``core.hooksPath`` points there.
A hooks setup that already existed is chained, not replaced:

- the previous ``core.hooksPath`` (or ``.git/hooks`` when it holds real hooks) is kept
  in ``chaosengine.chainedHooksPath``,
- ``pre-commit`` and ``pre-push`` run the guard first, then the chained hook,
- every other chained hook gets a pass-through shim.

Set ``CHAOS_ENGINE_SECRET_GUARD=0`` (or pass ``--no-secret-guard`` to install.py, which sets it) to skip.
"""
from __future__ import annotations

import os
import shutil
import stat
import subprocess  # nosec B404 - runs git only, with a resolved path and no shell
from pathlib import Path

HOOKS_DIRECTORY = "chaos-engine-hooks"
CHAIN_KEY = "chaosengine.chainedHooksPath"
GUARDED = ("pre-commit", "pre-push")
# #7323: only git's own hook names get shims; helper files in a chained dir (a copied
# secret_guard.py, __init__.py, README) must never overwrite what we install.
GIT_HOOK_NAMES = frozenset({
    "applypatch-msg", "pre-applypatch", "post-applypatch", "pre-commit", "pre-merge-commit",
    "prepare-commit-msg", "commit-msg", "post-commit", "pre-rebase", "post-checkout", "post-merge",
    "pre-push", "pre-receive", "update", "proc-receive", "post-receive", "post-update",
    "reference-transaction", "push-to-checkout", "pre-auto-gc", "post-rewrite", "sendemail-validate",
    "fsmonitor-watchman", "p4-changelist", "p4-prepare-changelist", "p4-post-changelist", "p4-pre-submit",
    "post-index-change",
})
_PREAMBLE = """#!/bin/sh
# ChaosEngine secret guard (#7177). Written by install.py; re-run the install to update it.
chained=$(git config --get {key})
case "$chained" in
  ""|/*|?:*) ;;
  *) chained="$(git rev-parse --show-toplevel)/$chained" ;;
esac
"""
_PRE_COMMIT = _PREAMBLE + """PY=$(command -v python3 || command -v python)
"$PY" "$(dirname "$0")/secret_guard.py" --staged || exit 1
if [ -n "$chained" ] && [ -x "$chained/pre-commit" ]; then
  exec "$chained/pre-commit" "$@"
fi
"""
_PRE_PUSH = _PREAMBLE + """PY=$(command -v python3 || command -v python)
refs=$(cat)
printf '%s\\n' "$refs" | "$PY" "$(dirname "$0")/secret_guard.py" --push || exit 1
if [ -n "$chained" ] && [ -x "$chained/pre-push" ]; then
  printf '%s\\n' "$refs" | "$chained/pre-push" "$@"
  exit $?
fi
"""
_SHIM = _PREAMBLE + """if [ -n "$chained" ] && [ -x "$chained/{name}" ]; then
  exec "$chained/{name}" "$@"
fi
"""


def _git(project: Path, *args: str, missing_ok: bool = False) -> str:
    """Run git in ``project``; with ``missing_ok`` any failure reads as empty output."""
    executable = shutil.which("git")
    if not executable:
        raise FileNotFoundError("git is not on PATH")
    completed = subprocess.run(  # nosec B603 - fixed git argv, no shell
        [executable, "-C", str(project), *args], capture_output=True, text=True, check=False
    )
    if completed.returncode not in (0, 1) and not missing_ok:
        raise subprocess.CalledProcessError(completed.returncode, args, completed.stderr)
    return completed.stdout.strip() if completed.returncode == 0 else ""


def _real_hooks(directory: Path) -> list[str]:
    if not directory.is_dir():
        return []
    return sorted(
        path.name for path in directory.iterdir()
        if path.is_file() and not path.name.endswith(".sample") and os.access(path, os.X_OK)
    )


def _write_executable(path: Path, text: str) -> None:
    path.write_text(text, encoding="utf-8", newline="\n")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


def hook_text(name: str) -> str:
    """Script text for one hook: the guard for pre-commit and pre-push, else a pass-through shim."""
    template = {"pre-commit": _PRE_COMMIT, "pre-push": _PRE_PUSH}.get(name, _SHIM)
    return template.replace("{key}", CHAIN_KEY).replace("{name}", name)


def _absolute(project: Path, value: str) -> Path:
    path = Path(value).expanduser()
    return (path if path.is_absolute() else project / path).resolve()


def _git_dir_marker(project: Path) -> Path | None:
    """Return the ``.git`` file or directory when present (possibly broken)."""
    marker = Path(project) / ".git"
    return marker if marker.exists() else None


def _git_objects_healthy(common_dir: Path) -> bool:
    """True when the common git dir has the usual objects/refs layout."""
    objects = common_dir / "objects"
    refs = common_dir / "refs"
    return objects.is_dir() and refs.is_dir()


def install_secret_guard(project: Path, guard_source: Path) -> dict[str, object]:
    """Wire the guard into the clone at ``project``; return what was done."""
    if os.environ.get("CHAOS_ENGINE_SECRET_GUARD", "1").strip().casefold() in {"0", "false", "no", "off"}:
        return {"status": "skipped", "reason": "CHAOS_ENGINE_SECRET_GUARD=0"}
    marker = _git_dir_marker(project)
    common = _git(project, "rev-parse", "--git-common-dir", missing_ok=True)
    if not common:
        if marker is not None:
            # .git exists but git cannot open the repository (box restart with
            # empty objects/refs, corrupt gitfile, …). Never silent-skip.
            return {
                "status": "failed",
                "reason": (
                    "broken git metadata (.git present but rev-parse failed); "
                    "repair the clone before install, or pass --no-secret-guard"
                ),
            }
        return {"status": "skipped", "reason": "not a git repository"}
    common_dir = Path(common) if Path(common).is_absolute() else (project / common)
    if not _git_objects_healthy(common_dir.resolve()):
        return {
            "status": "failed",
            "reason": (
                f"broken git metadata (missing objects/refs under {common_dir}); "
                "repair the clone before install, or pass --no-secret-guard"
            ),
        }
    top = _git(project, "rev-parse", "--show-toplevel", missing_ok=True)
    if not top or Path(top).resolve() != project.resolve():
        return {"status": "skipped", "reason": "not the root of its git repository"}
    common_dir = Path(common) if Path(common).is_absolute() else (project / common)
    target = (common_dir / HOOKS_DIRECTORY).resolve()
    current = _git(project, "config", "--get", "core.hooksPath")
    chained = _git(project, "config", "--get", CHAIN_KEY)
    if current and _absolute(project, current) != target:
        chained = current
    elif not current:
        default_hooks = common_dir / "hooks"
        chained = str(default_hooks.resolve()) if _real_hooks(default_hooks) else ""
    target.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(guard_source, target / "secret_guard.py")
    for name in GUARDED:
        _write_executable(target / name, hook_text(name))
    shims = [
        name for name in _real_hooks(_absolute(project, chained))
        if name in GIT_HOOK_NAMES and name not in GUARDED
    ] if chained else []
    for name in shims:
        _write_executable(target / name, hook_text(name))
    if chained:
        _git(project, "config", CHAIN_KEY, chained)
    elif _git(project, "config", "--get", CHAIN_KEY):
        _git(project, "config", "--unset", CHAIN_KEY)
    _git(project, "config", "core.hooksPath", str(target))
    status, reason = guard_health(project)
    if status != "pass":
        raise RuntimeError(f"installed secret guard is broken: {reason}")
    return {"status": "installed", "hooksPath": str(target), "chained": chained or None, "shims": shims}


def guard_health(project: Path) -> tuple[str, str]:
    """#7323: doctor row. ``fail`` when hooks point at a guard that does not compile (every commit fails)."""
    current = _git(project, "config", "--get", "core.hooksPath", missing_ok=True)
    if not current:
        return "pass", "not installed"
    target = _absolute(project, current)
    if target.name != HOOKS_DIRECTORY:
        return "pass", "not installed"
    guard = target / "secret_guard.py"
    try:
        compile(guard.read_text(encoding="utf-8"), str(guard), "exec")
    except (OSError, UnicodeDecodeError) as error:
        return "fail", f"{guard.name} unreadable: {type(error).__name__}"
    except SyntaxError as error:
        return "fail", f"{guard.name} line {error.lineno}: {error.msg}"
    canonical = Path(__file__).resolve().with_name("secret_guard.py")
    if canonical.is_file():
        try:
            installed = guard.read_bytes()
            expected = canonical.read_bytes()
        except OSError as error:
            return "fail", f"{guard.name} unreadable: {type(error).__name__}"
        if installed != expected:
            return "fail", "secret_guard.py differs from the canonical guard"
    return "pass", ""
