"""Detect and provision host prerequisites after a successful install (#7220).

Design decisions (settled in #7220's batch-3 PR):

* Elevation and consent: ``install`` mode runs only on an interactive, non-CI
  terminal. CI, no TTY, or ``CHAOS_ENGINE_PREREQUISITES=report`` only reports
  what is missing, with the official command, and never fails the install.
  ``off`` skips the step. System-wide steps that need admin (Chocolatey) are
  reported, not attempted, for non-admin users.
* Chocolatey overlap: winget is the one Windows package manager. Chocolatey is
  a fallback only when winget is still unavailable, so there is one update path.
* Package source per OS: Windows uses winget with vendor package ids. macOS
  uses Homebrew when present (no sudo). Linux is report-only: the vendor
  command is printed because every route needs sudo.
* Maven and Node.js have no vendor winget/brew-free route everywhere; Maven on
  Windows is report-only and points at ChaosEngine's own Maven runtime.
* "Latest" lookup: one HTTPS call per present tool, cached 24 h under
  ``.chaos-engine-state/prerequisites-latest.json``; offline means "present
  counts as current", never a failure. Report mode makes no network calls.
* Framework steps come from add-on data (``addons/*/post-install.json``): the
  framework's agentic installer, then an upgrade prompt only after a successful
  install, only when the project's framework version is older than the latest release (newer local dev versions count as
  current). Default No, 10 s timeout, skipped (No) without a TTY or in CI.
"""

from __future__ import annotations

import json
import os
import re
import shutil
import subprocess  # nosec B404 - fixed argument lists from the table below.
import sys
import time
import urllib.request
from pathlib import Path

CACHE_SECONDS = 24 * 3600
PROMPT_SECONDS = 10
RELEASES = "https://api.github.com/repos/{}/releases/latest"

# tool -> executable, GitHub repo for "latest" (None: no cheap lookup), per-OS install argv.
TOOLS: dict[str, dict[str, object]] = {
    "git": {"exe": "git", "repo": "git-for-windows/git",
            "winget": "Git.Git", "brew": "git", "linux": "sudo apt-get install -y git  # or dnf install git"},
    "mvn": {"exe": "mvn", "repo": None, "winget": None, "brew": "maven",
            "linux": "sudo apt-get install -y maven  # or use ChaosEngine's Maven runtime"},
    "gh": {"exe": "gh", "repo": "cli/cli", "winget": "GitHub.cli", "brew": "gh",
           "linux": "see https://github.com/cli/cli/blob/trunk/docs/install_linux.md"},
    "node": {"exe": "node", "repo": None, "winget": "OpenJS.NodeJS.LTS", "brew": "node",
             "linux": "see https://nodejs.org/en/download/package-manager"},
    "winget": {"exe": "winget", "repo": "microsoft/winget-cli", "winget": None, "brew": None, "linux": None},
    "pwsh": {"exe": "pwsh", "repo": "PowerShell/PowerShell", "winget": "Microsoft.PowerShell",
             "brew": None, "linux": None},
    "choco": {"exe": "choco", "repo": None, "winget": None, "brew": None, "linux": None},
}
WINGET_BOOTSTRAP = ["powershell", "-NoProfile", "-Command",
                    "Add-AppxPackage -RegisterByFamilyName -MainPackage Microsoft.DesktopAppInstaller_8wekyb3d8bbwe"]
CHOCO_BOOTSTRAP = ["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-Command",
                   "iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))"]


def in_ci(env: dict[str, str] | None = None) -> bool:
    env = os.environ if env is None else env
    return any(env.get(name) for name in ("CI", "GITHUB_ACTIONS", "TF_BUILD", "BUILDKITE", "JENKINS_URL"))


def interactive(env: dict[str, str] | None = None) -> bool:
    try:
        return sys.stdin.isatty() and sys.stdout.isatty() and not in_ci(env)
    except (AttributeError, ValueError):
        return False


def mode(env: dict[str, str] | None = None) -> str:
    env = os.environ if env is None else env
    chosen = env.get("CHAOS_ENGINE_PREREQUISITES", "").strip().lower()
    if chosen in {"off", "report"}:
        return chosen
    if chosen == "install":
        return "install" if not in_ci(env) else "report"
    return "install" if interactive(env) else "report"


def is_admin() -> bool:
    if os.name != "nt":
        return hasattr(os, "geteuid") and os.geteuid() == 0
    try:
        import ctypes
        return bool(ctypes.windll.shell32.IsUserAnAdmin())  # type: ignore[attr-defined]
    except (AttributeError, OSError):
        return False


def github_remote(project: Path) -> bool:
    config = project / ".git" / "config"
    try:
        return "github.com" in config.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return False


def required(project: Path, *, windows: bool) -> list[str]:
    """Tools this project needs, in provisioning order (winget first on Windows)."""
    tools: list[str] = []
    if windows:
        tools += ["winget", "pwsh"]
    if (project / ".git").exists():
        tools.append("git")
    if (project / "pom.xml").is_file():
        tools.append("mvn")
    if github_remote(project):
        tools.append("gh")
    tools.append("node")
    return tools


def parse_version(text: str) -> tuple[int, ...]:
    match = re.search(r"(\d+(?:\.\d+)+)", text or "")
    return tuple(int(part) for part in match.group(1).split(".")) if match else ()


def installed_version(exe: str, runner=subprocess.run) -> tuple[int, ...]:
    path = shutil.which(exe)
    if not path:
        return ()
    flag = "-v" if exe == "choco" else "--version"
    try:
        done = runner([path, flag], capture_output=True, text=True, timeout=20, check=False)  # nosec B603
    except (OSError, subprocess.SubprocessError):
        return ()
    return parse_version(done.stdout or done.stderr)


def latest_version(tool: str, cache_file: Path, fetch=None, now=None) -> tuple[int, ...]:
    """Cached latest stable version; () when unknown or offline."""
    repo = TOOLS[tool]["repo"]
    if not repo:
        return ()
    now = time.time() if now is None else now
    try:
        cache = json.loads(cache_file.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        cache = {}
    hit = cache.get(tool)
    if isinstance(hit, dict) and now - float(hit.get("at", 0)) < CACHE_SECONDS:
        return tuple(hit.get("version") or ())
    fetch = fetch or _fetch_json
    try:
        tag = str(fetch(RELEASES.format(repo)).get("tag_name", ""))
    except Exception:  # noqa: BLE001 - offline or rate limited degrades to "unknown".
        return ()
    version = parse_version(tag)
    cache[tool] = {"at": now, "version": list(version)}
    try:
        cache_file.parent.mkdir(parents=True, exist_ok=True)
        cache_file.write_text(json.dumps(cache, indent=2) + "\n", encoding="utf-8")
    except OSError:
        pass  # A read-only state dir only loses the cache; the lookup result still stands.
    return version


def _fetch_json(url: str) -> dict:
    request = urllib.request.Request(url, headers={"Accept": "application/vnd.github+json"})
    with urllib.request.urlopen(request, timeout=10) as response:  # nosec B310 - fixed https URL.
        return json.loads(response.read().decode("utf-8"))


def install_command(tool: str, *, windows: bool, have_winget: bool, have_brew: bool) -> list[str] | str | None:
    """argv to run, a str to report only, or None when there is no route."""
    spec = TOOLS[tool]
    if windows:
        if tool == "winget":
            return list(WINGET_BOOTSTRAP)
        if tool == "choco":
            return list(CHOCO_BOOTSTRAP)
        if spec["winget"] and have_winget:
            return ["winget", "install", "--id", str(spec["winget"]), "--exact", "--silent",
                    "--accept-package-agreements", "--accept-source-agreements", "--disable-interactivity"]
        if tool == "mvn":
            return "use ChaosEngine's Maven runtime (install.py provisions it) or https://maven.apache.org/install.html"
        return None
    if sys.platform == "darwin" and spec["brew"] and have_brew:
        return ["brew", "install", str(spec["brew"])]
    return str(spec["linux"]) if spec["linux"] else None


def plan(project: Path, *, windows: bool, current_mode: str, cache_file: Path,
         version_of=installed_version, latest_of=latest_version) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for tool in required(project, windows=windows):
        have = version_of(TOOLS[tool]["exe"])
        present = bool(have) or bool(shutil.which(str(TOOLS[tool]["exe"])))
        latest = latest_of(tool, cache_file) if (present and current_mode == "install") else ()
        if present and (not latest or not have or have >= latest):
            action = "skip"  # Already present and current (or latest unknown): no download.
        elif present:
            action = "upgrade"
        else:
            action = "install"
        rows.append({"tool": tool, "installed": ".".join(map(str, have)) or ("present" if present else ""),
                     "latest": ".".join(map(str, latest)), "action": action})
    return rows


def provision(project: Path, *, env: dict[str, str] | None = None, runner=subprocess.run,
              windows: bool | None = None, version_of=installed_version,
              latest_of=latest_version) -> dict[str, object]:
    current = mode(env)
    if current == "off":
        return {"mode": "off", "tools": []}
    windows = os.name == "nt" if windows is None else windows
    cache = project / ".chaos-engine-state" / "prerequisites-latest.json"
    rows = plan(project, windows=windows, current_mode=current, cache_file=cache,
                version_of=version_of, latest_of=latest_of)
    have_winget = any(r["tool"] == "winget" and r["action"] == "skip" for r in rows)
    have_brew = bool(shutil.which("brew"))
    for row in rows:
        if row["action"] == "skip":
            continue
        command = install_command(str(row["tool"]), windows=windows, have_winget=have_winget, have_brew=have_brew)
        if command is None or isinstance(command, str) or current != "install":
            row["status"] = "reported"
            row["command"] = command if isinstance(command, str) else (" ".join(command) if command else "")
            continue
        done = runner(command, check=False, timeout=900)  # nosec B603 - fixed argv.
        row["status"] = "installed" if getattr(done, "returncode", 1) == 0 else "failed"
        if row["tool"] == "winget" and row["status"] == "installed":
            have_winget = True
    if windows and not have_winget:
        # Chocolatey is the fallback package manager only when winget stayed unavailable.
        choco = {"tool": "choco", "installed": "", "latest": "", "action": "install"}
        if shutil.which("choco"):
            choco.update(action="skip", installed="present")
        elif current == "install" and is_admin():
            done = runner(list(CHOCO_BOOTSTRAP), check=False, timeout=900)  # nosec B603
            choco["status"] = "installed" if getattr(done, "returncode", 1) == 0 else "failed"
        else:
            choco.update(status="reported", command=" ".join(CHOCO_BOOTSTRAP) + "  # needs admin")
        rows.append(choco)
    return {"mode": current, "tools": rows}


def framework_hooks(project: Path) -> list[dict[str, object]]:
    """Post-install hooks shipped as data by selected add-ons (``addons/*/post-install.json``)."""
    hooks: list[dict[str, object]] = []
    for path in sorted((project / ".chaos-engine" / "addons").glob("*/post-install.json")):
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        if isinstance(data, dict) and data.get("schemaVersion") == 1:
            hooks.append(data)
    return hooks


def framework_version(project: Path, group_id: str) -> str | None:
    """Version of the first dependency or parent with ``group_id`` in pom.xml, if any."""
    try:
        pom = (project / "pom.xml").read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    for block in re.findall(r"<(?:dependency|parent)>(.*?)</(?:dependency|parent)>", pom, re.S):
        if f"<groupId>{group_id}</groupId>" in block:
            found = re.search(r"<version>\s*([^<$]+?)\s*</version>", block)
            if found:
                return found.group(1)
    return None


def is_outdated(local: str, latest: str) -> bool:
    """Newer or equal local (including dev builds) counts as current."""
    mine, theirs = parse_version(local), parse_version(latest)
    return bool(mine and theirs and mine < theirs)


def ask_yes_no(question: str, *, timeout: int = PROMPT_SECONDS, env: dict[str, str] | None = None,
               reader=None) -> bool:
    """Default No; times out to No; never asks without a TTY or in CI."""
    if reader is None:
        if not interactive(env):
            return False
        reader = _timed_input
    print(f"{question} [y/N] (default No in {timeout}s): ", end="", flush=True, file=sys.stderr)
    answer = reader(timeout)
    return (answer or "").strip().lower() in {"y", "yes"}


def _timed_input(timeout: int) -> str | None:
    import threading

    box: list[str] = []
    worker = threading.Thread(target=lambda: box.append(sys.stdin.readline()), daemon=True)
    worker.start()
    worker.join(timeout)
    return box[0] if box else None


def framework_steps(project: Path, hook: dict[str, object], *, install_ok: bool,
                    env: dict[str, str] | None = None, runner=subprocess.run, latest_tag=None,
                    ask=ask_yes_no) -> dict[str, object]:
    """Run an add-on's agentic installer, then offer its upgrader when the project is outdated."""
    name = str(hook.get("framework", "framework"))
    local = framework_version(project, str((hook.get("detect") or {}).get("pomGroupId", "")))
    if local is None:
        return {"framework": name, "detected": False}
    current = mode(env)
    shell = "nt" if os.name == "nt" else "posix"
    result: dict[str, object] = {"framework": name, "detected": True, "version": local}
    agentic = str((hook.get("agenticInstall") or {}).get(shell, ""))
    if agentic and current == "install":
        argv = (["powershell", "-NoProfile", "-Command", agentic] if shell == "nt" else ["sh", "-c", agentic])
        done = runner(argv, cwd=str(project), check=False, timeout=1800)  # nosec B603 - add-on data.
        result["agenticInstall"] = "installed" if getattr(done, "returncode", 1) == 0 else "failed"
    elif agentic:
        result["agenticInstall"] = "reported"
        result["agenticCommand"] = agentic
    if not install_ok or current != "install" or not hook.get("upgrader"):
        result["upgrade"] = "not-offered"
        return result
    repository = str(hook.get("latestReleaseRepository", ""))
    try:
        latest = (latest_tag or (lambda: str(_fetch_json(RELEASES.format(repository))["tag_name"])))()
    except Exception:  # noqa: BLE001 - offline: no prompt.
        result["upgrade"] = "latest-unknown"
        return result
    result["latest"] = latest
    if not is_outdated(local, latest):
        result["upgrade"] = "current"
        return result
    if not ask(f"{name} {local} is older than {latest}. Run the {name} upgrader?", env=env):
        result["upgrade"] = "declined"
        return result
    import tempfile

    with tempfile.TemporaryDirectory() as scratch:
        script = Path(scratch) / "upgrader.py"
        urllib.request.urlretrieve(str(hook["upgrader"]), script)  # nosec B310 - add-on https URL.
        done = runner([sys.executable, str(script), "--project", str(project)], check=False, timeout=3600)  # nosec B603
    result["upgrade"] = "upgraded" if getattr(done, "returncode", 1) == 0 else "failed"
    return result


def run(project: Path, *, install_ok: bool) -> dict[str, object]:
    """Entry point from install.py; never raises into the install transaction."""
    try:
        report = provision(project)
        if report["mode"] != "off":
            report["frameworks"] = [
                framework_steps(project, hook, install_ok=install_ok) for hook in framework_hooks(project)
            ]
        return report
    except Exception as error:  # noqa: BLE001 - prerequisites never fail an install.
        return {"mode": "error", "error": type(error).__name__}
