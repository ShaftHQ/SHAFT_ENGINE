#!/usr/bin/env python3
"""Resolve and install the latest portable ChaosEngine from a GitHub branch."""

from __future__ import annotations

import argparse
from collections import deque
from concurrent.futures import ThreadPoolExecutor, as_completed
from contextlib import contextmanager
import email.utils
import hashlib
import json
import os
import platform
import re
import shlex
import runpy
import shutil
import sys
import tempfile
import textwrap
import threading
import time
import traceback
import types
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path, PurePosixPath


REPOSITORY = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")
COMMIT = re.compile(r"[0-9a-f]{40}")
MAX_RESPONSE_BYTES = 10 * 1024 * 1024
MAX_SOURCE_BYTES = 10 * 1024 * 1024
MAX_FILE_BYTES = 2 * 1024 * 1024
MAX_FILES = 2000
DOWNLOAD_WORKERS = 8
MAX_READ_ATTEMPTS = 4
MAX_RETRY_AFTER_SECONDS = 60.0
RETRY_BASE_SECONDS = 1.0
TRANSIENT_HTTP_STATUS = frozenset({408, 425, 429, 500, 502, 503, 504})
CYBERNETIC_RED = "\x1b[38;2;255;59;77m"
ION_BLUE = "\x1b[38;2;47;125;255m"
OPTICAL_WHITE = "\x1b[38;2;242;247;255m"
BRAND_ASCII = (
    "  ,-----.          ---+       /",
    "  |                   |      /",
    "  |  *      /      ---+     /",
    "  |                   |    /",
    "  `-----'          ---+   /",
    "         ChaosEngine",
)
BRAND_UNICODE = (
    "  █▀▀▀▀▀▄           ───┐        ╱",
    "  █                    │       ╱",
    "  █   ◆      ╱      ───┤      ╱",
    "  █                    │     ╱",
    "  █▄▄▄▄▄▀           ───┘    ╱",
    "          ChaosEngine",
)
BRAND_NARROW = (
    "  /C|*|E/",
    "  ChaosEngine",
)
TRACE_LIMIT = 12 if (
    os.environ.get("CI")
    or os.environ.get("CHAOS_ENGINE_QUIET") == "1"
) else 40
STALL_SECONDS = 8.0
MAX_ISSUE_URL_CHARS = 7800
MAX_ISSUE_BODY_CHARS = 60000
HEAL_HANDOFF_RELATIVE = ".chaos-engine-state/heal-handoff.md"
ISSUE_FORM_FIELD_IDS = (
    "error_code",
    "cause",
    "failed_phase",
    "unhealthy",
    "platform",
    "os_name",
    "os_version",
    "architecture",
    "python_version",
    "machine",
    "doctor_details",
    "hosts_receipt",
    "core_dir",
    "install_py",
    "install_trace",
    "install_trace_snippet",
    "console_log",
    "doctor_json",
    "status_command",
    "doctor_command",
    "additional",
)
REQUIRED_ISSUE_FORM_FIELDS = (
    "error_code",
    "cause",
    "failed_phase",
    "unhealthy",
    "platform",
    "os_name",
    "os_version",
    "architecture",
    "python_version",
    "machine",
    "doctor_details",
    "hosts_receipt",
    "core_dir",
    "install_py",
    "install_trace",
    "status_command",
    "doctor_command",
)
OPTIONAL_ISSUE_FORM_FIELDS = (
    "install_trace_snippet",
    "console_log",
    "doctor_json",
    "additional",
)
ISSUE_FORM_LABELS = {
    "error_code": "Error code",
    "cause": "Cause",
    "failed_phase": "Failed phase",
    "unhealthy": "Unhealthy components",
    "platform": "Platform",
    "os_name": "OS name",
    "os_version": "OS version",
    "architecture": "Architecture",
    "python_version": "Python version",
    "machine": "Machine",
    "doctor_details": "Doctor details",
    "hosts_receipt": "Hosts receipt",
    "core_dir": "Core dir",
    "install_py": "install.py",
    "install_trace": "Install trace (repo-relative)",
    "install_trace_snippet": "Install trace snippet",
    "console_log": "Full console log",
    "doctor_json": "Full doctor JSON",
    "status_command": "Status command",
    "doctor_command": "Doctor command",
    "additional": "Additional context",
}


def install_trace_path(project: Path) -> Path:
    return Path(project) / ".chaos-engine-state/install-trace.json"


def write_install_trace(project: Path, result: dict[str, object], traces: list[tuple[float, str]]) -> Path:
    path = install_trace_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({"result": result, "trace": traces}, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return path


def runtime_environment() -> dict[str, str]:
    """Bounded OS/Python facts for installer failure reports (#5703)."""
    return {
        "os_name": platform.system()[:40],
        "os_version": platform.release()[:40],
        "architecture": platform.machine()[:40] or "unknown",
        "python_version": sys.version.split()[0][:32],
        "machine": platform.platform(terse=True)[:80],
    }


def doctor_failure_payload(error: BaseException) -> dict[str, object]:
    """Keep status/code/detail/fix-next only; never persist paths or secrets."""
    doctor = getattr(error, "doctor", None)
    if not isinstance(doctor, dict):
        return {}
    components: dict[str, object] = {}
    raw = doctor.get("components")
    if isinstance(raw, dict):
        for name, item in raw.items():
            if not isinstance(name, str) or not isinstance(item, dict):
                continue
            trimmed = {
                key: item[key]
                for key in ("status", "taskImpact", "detail", "code", "fixNext")
                if isinstance(item.get(key), str)
            }
            if trimmed:
                components[name] = trimmed
    commit = doctor.get("commit")
    return {
        "status": doctor.get("status") if isinstance(doctor.get("status"), str) else "unknown",
        "commit": commit if isinstance(commit, str) else None,
        "components": components,
    }


def write_failure_artifacts(
    project: Path,
    reporter: InstallReporter | None,
    error: BaseException,
) -> tuple[str, str]:
    """Write attachable console and doctor artifacts under .chaos-engine-state/."""
    state = Path(project) / ".chaos-engine-state"
    state.mkdir(parents=True, exist_ok=True)
    traces: list[str] = []
    if reporter is not None:
        traces = [f"[+{ended:.3f}] {message}" for ended, message in reporter.traces]
    (state / "install-console.log").write_text(
        ("\n".join(traces) + "\n") if traces else "no installer console traces\n",
        encoding="utf-8",
    )
    payload = doctor_failure_payload(error)
    doctor_rel = "not available"
    if payload:
        (state / "doctor-failure.json").write_text(
            json.dumps(payload, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        doctor_rel = ".chaos-engine-state/doctor-failure.json"
    return ".chaos-engine-state/install-console.log", doctor_rel


def redact_report_text(text: str) -> str:
    """Redact paths and secret assignments from multi-line installer reports."""
    text = re.sub(
        r"(?<!:)(?:[A-Za-z]:[\\/]|\\\\[^\s\\/]+[\\/]|/(?:(?:media|mnt|Volumes)(?:/\S+?)?/(?:Users|home)|home|Users|tmp|var|private)/)\S+",
        "[path]",
        text,
    )
    return re.sub(
        r"(?i)\b(token|secret|password|api_key)=\S+",
        lambda match: f"{match.group(1)}=<redacted>",
        text,
    )


def github_auth_token() -> str | None:
    for key in ("GH_TOKEN", "GITHUB_TOKEN"):
        value = os.environ.get(key)
        if isinstance(value, str) and value.strip():
            return value.strip()
    return None


def resolve_issue_token(explicit: str | None) -> str | None:
    """API filing is opt-in and never uses GitHub Actions GITHUB_TOKEN."""
    if explicit is not None:
        return explicit.strip() or None
    if os.environ.get("GITHUB_ACTIONS") == "true":
        return None
    if os.environ.get("CHAOS_ENGINE_FILE_ISSUE") != "1":
        return None
    return github_auth_token()


def upgrade_query_extras(error: BaseException) -> dict[str, str]:
    extras: dict[str, str] = {}
    commit = getattr(error, "observed_upgrade_commit", None)
    if isinstance(commit, str) and COMMIT.fullmatch(commit):
        extras["observed_commit"] = commit
    components = getattr(error, "observed_upgrade_components", ())
    labels: list[str] = []
    if isinstance(components, tuple):
        for component in components[:32]:
            if not isinstance(component, tuple) or len(component) != 2:
                continue
            name, status = component
            if (
                isinstance(name, str)
                and isinstance(status, str)
                and re.fullmatch(r"[a-z][a-z0-9-]{0,63}", name)
                and re.fullmatch(r"[a-z][a-z0-9-]{0,63}", status)
            ):
                labels.append(f"{name}:{status}")
    if labels:
        extras["candidate_components"] = ",".join(labels)
    details = getattr(error, "observed_upgrade_component_details", ())
    detail_labels: list[str] = []
    if isinstance(details, tuple):
        for component in details[:32]:
            if not isinstance(component, tuple) or len(component) != 2:
                continue
            name, detail = component
            if (
                isinstance(name, str)
                and isinstance(detail, str)
                and re.fullmatch(r"[a-z][a-z0-9-]{0,63}", name)
                and re.fullmatch(r"[a-z][a-z0-9-]{0,63}", detail)
            ):
                detail_labels.append(f"{name}:{detail}")
    if detail_labels:
        extras["candidate_component_details"] = ",".join(detail_labels)
    return extras


def encode_issue_form_url(
    repository: str,
    title: str,
    fields: dict[str, str],
    extra: dict[str, str] | None = None,
) -> str:
    """Pack required fields first; optional logs only while under the URL cap."""
    packed = {
        key: fields[key]
        for key in REQUIRED_ISSUE_FORM_FIELDS
        if isinstance(fields.get(key), str) and fields[key]
    }
    if extra:
        packed.update({key: value for key, value in extra.items() if value})
    optional = {
        key: fields[key]
        for key in OPTIONAL_ISSUE_FORM_FIELDS
        if isinstance(fields.get(key), str) and fields[key]
    }

    def render(current: dict[str, str]) -> str:
        query = {"template": "chaos-engine-installer.yml", "title": title, **current}
        return f"https://github.com/{repository}/issues/new?{urllib.parse.urlencode(query)}"

    url = render(packed)
    for key, value in optional.items():
        candidate = dict(packed)
        candidate[key] = value
        encoded = render(candidate)
        if len(encoded) <= MAX_ISSUE_URL_CHARS:
            packed = candidate
            url = encoded
            continue
        shrink = value
        while len(shrink) > 64:
            shrink = shrink[: len(shrink) // 2] + "\n…truncated…"
            candidate[key] = shrink
            encoded = render(candidate)
            if len(encoded) <= MAX_ISSUE_URL_CHARS:
                packed = candidate
                url = encoded
                break
    return url


def issue_form_markdown(fields: dict[str, str]) -> str:
    sections: list[str] = []
    for key in ISSUE_FORM_FIELD_IDS:
        value = fields.get(key, "")
        if not isinstance(value, str) or not value.strip():
            continue
        label = ISSUE_FORM_LABELS.get(key, key)
        fence = "json" if key == "doctor_json" else ""
        if key in {"console_log", "doctor_json", "install_trace_snippet"}:
            sections.append(f"### {label}\n\n```{fence}\n{value.rstrip()}\n```\n")
        else:
            sections.append(f"### {label}\n\n{value.strip()}\n")
    body = "\n".join(sections).strip() + "\n"
    if len(body) > MAX_ISSUE_BODY_CHARS:
        body = body[: MAX_ISSUE_BODY_CHARS - 20] + "\n…truncated…\n"
    return body


def create_installer_github_issue(
    repository: str,
    title: str,
    body: str,
    token: str,
    opener=urllib.request.urlopen,
) -> str | None:
    payload = json.dumps({"title": title, "body": body}).encode("utf-8")
    request_obj = urllib.request.Request(
        f"https://api.github.com/repos/{repository}/issues",
        data=payload,
        method="POST",
        headers={
            "Accept": "application/vnd.github+json",
            "User-Agent": "ChaosEngine-bootstrap",
            "Authorization": f"Bearer {token}",
            "Content-Type": "application/json",
        },
    )
    try:
        with opener(request_obj, timeout=30) as response:
            document = response.read(MAX_RESPONSE_BYTES)
    except (OSError, TimeoutError, urllib.error.URLError, urllib.error.HTTPError):
        return None
    try:
        value = json.loads(document.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError):
        return None
    html = value.get("html_url") if isinstance(value, dict) else None
    if isinstance(html, str) and html.startswith("https://github.com/"):
        return html
    return None


def core_install_py(project: Path) -> bool:
    return (Path(project) / ".chaos-engine" / "install.py").is_file()


def heal_handoff_prompt(doctor_command: str, issue_url: str) -> str:
    cli = "py -3" if os.name == "nt" else "python3"
    if doctor_command == "not available":
        return (
            "Load ChaosEngine if present. Restore the portable core with the documented "
            "ChaosEngine install one-liner, then read .chaos-engine-state/heal-handoff.md. "
            f"Comment investigation and outcome on {issue_url}."
        )
    return (
        "Load ChaosEngine in this project. Read .chaos-engine-state/heal-handoff.md "
        "and the local install-trace.json, install-console.log, and doctor-failure.json. "
        "Do not rerun the install one-liner unless the portable core is missing. "
        "Repair the named unhealthy components with "
        f"`{cli} .chaos-engine/install.py repair --project . --component <name>` "
        f"and `{doctor_command}` until required components are healthy. "
        f"Then comment investigation, commands, doctor excerpt, and outcome on {issue_url}."
    )


def write_heal_handoff(project: Path, fields: dict[str, str], issue_url: str) -> Path:
    target = Path(project) / HEAL_HANDOFF_RELATIVE
    target.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        "# Heal handoff",
        "",
        "The portable core is installed. One agent step remains.",
        "",
        f"Issue: {issue_url}",
        "",
        f"Error code: {fields.get('error_code', '')}",
        f"Failed phase: {fields.get('failed_phase', '')}",
        f"Unhealthy: {fields.get('unhealthy', '')}",
        f"Doctor details: {fields.get('doctor_details', '')}",
        f"Doctor: `{fields.get('doctor_command', '')}`",
        "",
        "Local artifacts:",
        "",
        "- `.chaos-engine-state/install-trace.json`",
        "- `.chaos-engine-state/install-console.log`",
        "- `.chaos-engine-state/doctor-failure.json`",
        "",
        "Do not rerun the install one-liner unless `.chaos-engine/install.py` is missing.",
        "",
    ]
    target.write_text("\n".join(lines), encoding="utf-8")
    return target


def brand_lines(*, width: int = 80, color: bool = False, unicode: bool = False) -> list[str]:
    if width < 28:
        templates = BRAND_NARROW
        glyph = "*"
    elif unicode and width >= 48:
        templates = BRAND_UNICODE
        glyph = "◆"
    else:
        templates = BRAND_ASCII
        glyph = "*"
    core = f"{CYBERNETIC_RED}{glyph}\x1b[0m" if color else glyph
    painted = []
    for template in templates:
        line = template.replace(glyph, core, 1) if glyph in template else template
        if color and "ChaosEngine" in line:
            line = line.replace(
                "ChaosEngine",
                f"{OPTICAL_WHITE}ChaosEngine\x1b[0m",
                1,
            )
        painted.append(line)
    return painted


def _component_blocks_health(value: object) -> bool:
    if not isinstance(value, dict):
        return False
    status = value.get("status")
    if status == "healthy":
        return False
    # Memory origin/main desync is advisory for required mcps during install verify (#5630).
    if status in {"compatible-legacy", "degraded", "sync-advisory"}:
        return False
    if value.get("taskImpact") == "optional" and status == "absent":
        return False
    return value.get("taskImpact") == "required"


def observed_blocking_components(components: object) -> tuple[tuple[str, str], ...]:
    """Keep only bounded, path-free status labels for a failed upgrade report."""
    if not isinstance(components, dict):
        return ()
    result: list[tuple[str, str]] = []
    for name, value in components.items():
        status = value.get("status") if isinstance(value, dict) else None
        if (
            not isinstance(name, str)
            or not isinstance(status, str)
            or re.fullmatch(r"[a-z][a-z0-9-]{0,63}", name) is None
            or re.fullmatch(r"[a-z][a-z0-9-]{0,63}", status) is None
            or not _component_blocks_health(value)
        ):
            continue
        result.append((name, status))
        if len(result) == 32:
            break
    return tuple(sorted(result))


def observed_blocking_component_details(components: object) -> tuple[tuple[str, str], ...]:
    """Keep only fixed outcome codes for required failed upgrade components."""
    if not isinstance(components, dict):
        return ()
    result: list[tuple[str, str]] = []
    for name, value in components.items():
        detail = value.get("detail") if isinstance(value, dict) else None
        if (
            not isinstance(name, str)
            or not isinstance(detail, str)
            or re.fullmatch(r"[a-z][a-z0-9-]{0,63}", name) is None
            or re.fullmatch(r"[a-z][a-z0-9-]{0,63}", detail) is None
            or not _component_blocks_health(value)
        ):
            continue
        result.append((name, detail))
        if len(result) == 32:
            break
    return tuple(sorted(result))


def _required_install_unhealthy(doctor: dict[str, object]) -> bool:
    components = doctor.get("components")
    if isinstance(components, dict) and any(
        _component_blocks_health(value) for value in components.values()
    ):
        return True
    for key in ("kernel", "hosts", "dependencies"):
        item = doctor.get(key)
        if isinstance(item, dict) and item.get("status") not in {None, "healthy", "absent"}:
            return True
    return False


def wants_maven_tools(project: Path, *, skip_tools: bool, requested: bool) -> bool:
    if skip_tools:
        return False
    return requested or (Path(project) / "pom.xml").is_file()


class InstallCancelled(RuntimeError):
    """Raised before an operation when interactive confirmation is declined."""


class InstallHealthError(RuntimeError):
    """Preserve bounded doctor context for recovery output."""

    def __init__(self, phase: str, doctor: dict[str, object]):
        """Capture the failed phase and names of unhealthy components."""
        components = doctor.get("components", {})
        unhealthy = tuple(
            name
            for name, value in components.items()
            if _component_blocks_health(value)
        ) if isinstance(components, dict) else ()
        cli = "py -3" if os.name == "nt" else "python3"
        if unhealthy:
            detail = ", ".join(unhealthy)
            super().__init__(
                f"ChaosEngine doctor did not report a healthy installation "
                f"(unhealthy: {detail}). Run: {cli} .chaos-engine/install.py doctor "
                f"--project . --json"
            )
        else:
            super().__init__(
                "ChaosEngine doctor did not report a healthy installation. "
                f"Run: {cli} .chaos-engine/install.py doctor --project . --json"
            )
        self.phase = phase
        self.unhealthy = unhealthy
        commit = doctor.get("commit")
        self.observed_commit = commit if isinstance(commit, str) and COMMIT.fullmatch(commit) else None
        self.observed_components = observed_blocking_components(components)
        self.observed_component_details = observed_blocking_component_details(components)
        self.doctor = doctor if isinstance(doctor, dict) else {}



def safe_command_trace(command: list[str] | tuple[str, ...]) -> str:
    """Format a command for installer traces without leaking secret-looking values."""
    redacted: list[str] = []
    secret = re.compile(
        r"(?i)(token|secret|password|passwd|api[_-]?key|authorization|bearer)\s*[=:]\s*\S+"
    )
    hexish = re.compile(r"^[0-9a-fA-F]{32,}$")
    for part in command:
        value = str(part)
        if secret.search(value):
            value = secret.sub(
                lambda match: match.group(0).split("=", 1)[0].split(":", 1)[0] + "=***",
                value,
            )
        elif hexish.fullmatch(value):
            value = "***"
        redacted.append(value)
    return " ".join(shlex.quote(item) for item in redacted)


class InstallReporter:
    """Dependency-free installer status renderer; UX always goes to stderr."""

    def __init__(self, *, stream=None, clock=time.monotonic):
        """Initialize reporting against the supplied output stream and clock."""
        self.stream = sys.stderr if stream is None else stream
        self.clock = clock
        self.started = clock()
        self.completed_operations: list[str] = []
        self.remaining_operations: tuple[str, ...] = ()
        self.current_operation: str | None = None
        self._in_flight: list[str] = []
        self._elapsed_as_current: dict[str, float] = {}
        self._completed_elapsed: dict[str, float] = {}
        self.history: list[tuple[float, str, str, float]] = []
        self.traces: list[tuple[float, str]] = []
        self.trace_count = 0
        self._current_started: float | None = None
        self.project_root: str | None = None
        self.trace_path: Path | None = None
        self.source_label: str | None = None
        self._download_total: int | None = None
        self._downloaded = 0
        self._download_samples = deque(maxlen=30)
        self.detail: str | None = None
        self._lock = threading.Lock()
        self._stop = threading.Event()
        self._thread: threading.Thread | None = None
        self._lines = 0
        self._tty = self._stderr_is_tty()
        self._color = self._tty and "NO_COLOR" not in os.environ
        self._unicode = self._encodable("✓◉·…█▀▄┬├┴╱◆")
        if self._tty and os.name == "nt" and not self._enable_windows_vt():
            self._color = False
        if os.environ.get("CHAOS_ENGINE_BRAND_SHOWN") != "1":
            for line in brand_lines(
                width=self._width(),
                color=self._color,
                unicode=self._tty and self._unicode,
            ):
                self.stream.write(line + "\n")
            self.stream.flush()

    def _stderr_is_tty(self) -> bool:
        if os.environ.get("TERM") == "dumb":
            return False
        isatty = getattr(self.stream, "isatty", None)
        if callable(isatty):
            return bool(isatty())
        try:
            return self.stream is sys.stderr and os.isatty(2)
        except (AttributeError, OSError, ValueError):
            return False

    def announce(self, project: Path, repository: str, branch: str) -> None:
        self.project_root = str(Path(project).resolve())
        self.trace_path = install_trace_path(Path(project).resolve())
        self.source_label = f"{repository}@{branch}"
        if self._tty:
            with self._lock:
                self._render_locked()
        else:
            self.stream.write(self._truncate(f"Install root: {self.project_root}") + "\n")
            self.stream.write(self._truncate(f"Source: {self.source_label}") + "\n")
            self.stream.flush()

    def trace(self, message: str) -> None:
        with self._lock:
            self.traces.append((self.clock() - self.started, message))
            self.trace_count += 1
            if self._tty:
                self._render_locked()
            else:
                self.stream.write(self._truncate(f"  {message}") + "\n")
                self.stream.flush()

    def _enable_windows_vt(self) -> bool:
        try:
            import ctypes
            handle = ctypes.windll.kernel32.GetStdHandle(-12)
            mode = ctypes.c_uint()
            return bool(
                ctypes.windll.kernel32.GetConsoleMode(handle, ctypes.byref(mode))
                and ctypes.windll.kernel32.SetConsoleMode(handle, mode.value | 0x0004)
            )
        except (AttributeError, OSError, ValueError):
            return False

    def _encodable(self, value: str) -> bool:
        try:
            value.encode(getattr(self.stream, "encoding", None) or "utf-8")
            return True
        except (LookupError, UnicodeEncodeError):
            return False

    def _width(self) -> int:
        return max(20, shutil.get_terminal_size(fallback=(80, 24)).columns)

    def _truncate(self, value: str) -> str:
        width = self._width()
        if len(value) <= width:
            return value
        suffix = "…" if self._unicode else "..."
        return value[: max(0, width - len(suffix))] + suffix

    def _wrap(self, value: str) -> list[str]:
        width = self._width()
        if len(value) <= width:
            return [value]
        indent = value[: len(value) - len(value.lstrip())]
        return textwrap.wrap(
            value,
            width=width,
            subsequent_indent=indent,
            break_long_words=True,
            break_on_hyphens=False,
        ) or [indent]

    def _paint(self, value: str, color: str) -> str:
        return f"\x1b[{color}m{value}\x1b[0m" if self._color else value

    def _duration(self, seconds: float) -> str:
        seconds = max(0, round(seconds))
        minutes, seconds = divmod(seconds, 60)
        return f"{minutes:02d}:{seconds:02d}"

    def _pause_current(self, now: float) -> None:
        if self.current_operation is None or self._current_started is None:
            return
        name = self.current_operation
        self._elapsed_as_current[name] = self._elapsed_as_current.get(name, 0.0) + max(
            0.0, now - self._current_started
        )
        self._current_started = None

    def start(
        self, operation: str, *, remaining: tuple[str, ...] | None = None,
        detail: str | None = None,
    ) -> None:
        with self._lock:
            now = self.clock()
            self._pause_current(now)
            if operation not in {
                "Download source",
                "Provision dependencies",
                "Install Maven Tools",
            }:
                self._download_total = None
                self._downloaded = 0
                self._download_samples.clear()
            if remaining is not None:
                kept = tuple(
                    item
                    for item in self._in_flight
                    if item not in remaining
                    and item != operation
                    and item not in self.completed_operations
                )
                self.remaining_operations = kept + tuple(
                    item for item in remaining if item != operation
                )
            self.current_operation = operation
            if operation not in self._in_flight:
                self._in_flight.append(operation)
            self._current_started = now
            self.detail = detail
            if self._tty:
                self._render_locked()
                if self._thread is None:
                    self._thread = threading.Thread(
                        target=self._ticker, name="chaos-engine-installer", daemon=True
                    )
                    self._thread.start()
            else:
                suffix = (
                    f" — {detail}"
                    if detail and self._unicode
                    else (f" - {detail}" if detail else "")
                )
                self.stream.write(self._truncate(f"START {operation}{suffix}") + "\n")
                self.stream.flush()

    def complete(self, operation: str, *, remaining: tuple[str, ...] = ()) -> None:
        with self._lock:
            now = self.clock()
            if self.current_operation == operation:
                self._pause_current(now)
                self.current_operation = None
            if operation in self._in_flight:
                self._in_flight.remove(operation)
            if operation not in self.completed_operations:
                self.completed_operations.append(operation)
            self._completed_elapsed[operation] = self._elapsed_as_current.get(operation, 0.0)
            duration = self._completed_elapsed[operation]
            self.history.append((now - self.started, "PASS", operation, duration))
            self.traces.append((now - self.started, f"PASS {operation} ({self._duration(duration)})"))
            self.trace_count += 1
            self.remaining_operations = remaining
            self.detail = None
            if self._tty:
                self._render_locked()
            else:
                self.stream.write(f"DONE  {operation}\n")
                self.stream.write(
                    f"[+{self._duration(now - self.started)}] PASS {operation} "
                    f"({self._duration(duration)})\n"
                )
                self.stream.flush()

    def begin_download(self, total: int | None, *, detail: str | None = None) -> None:
        with self._lock:
            now = self.clock()
            self._download_total = total if isinstance(total, int) and total > 0 else None
            self._downloaded = 0
            self._download_samples.clear()
            self._download_samples.append((now, 0))
            if detail:
                self.detail = detail
            if self._tty:
                self._render_locked()
            if self._thread is None:
                self._thread = threading.Thread(
                    target=self._ticker, name="chaos-engine-installer", daemon=True
                )
                self._thread.start()

    def downloaded(self, count: int) -> None:
        if count <= 0:
            return
        with self._lock:
            now = self.clock()
            self._downloaded += count
            self._download_samples.append((now, self._downloaded))
            while (
                len(self._download_samples) > 2
                and now - self._download_samples[0][0] > 8.0
            ):
                self._download_samples.popleft()
            if self._tty:
                self._render_locked()

    def _ticker(self) -> None:
        while not self._stop.wait(1.0):
            with self._lock:
                self._render_locked()

    def _transfer_stalled(self, now: float) -> bool:
        return bool(
            self._download_total is not None
            and self._download_samples
            and now - self._download_samples[-1][0] > STALL_SECONDS
        )

    def _download_rate(self) -> float | None:
        if len(self._download_samples) < 2:
            return None
        started, first = self._download_samples[0]
        ended, last = self._download_samples[-1]
        elapsed = ended - started
        transferred = last - first
        if elapsed < 1.0 or transferred <= 0 or self.clock() - ended > STALL_SECONDS:
            return None
        return transferred / elapsed

    @staticmethod
    def _size(value: float) -> str:
        units = ("B/s", "KiB/s", "MiB/s", "GiB/s")
        for unit in units[:-1]:
            if value < 1024:
                return f"{value:.0f} {unit}"
            value /= 1024
        return f"{value:.1f} {units[-1]}"

    def _render_locked(self) -> None:
        operations = list(
            dict.fromkeys(
                [
                    *self.completed_operations,
                    *self._in_flight,
                    *([self.current_operation] if self.current_operation else []),
                    *self.remaining_operations,
                ]
            )
        )
        now = self.clock()
        elapsed = max(0.0, now - self.started)
        check, active, empty = (("✓", "◉", " ") if self._unicode else ("x", "*", " "))
        lines = [""]
        if self.project_root:
            lines.append(self._truncate(f"  Install root: {self.project_root}"))
        if self.source_label:
            lines.append(self._truncate(f"  Source: {self.source_label}"))
        if self.project_root or self.source_label:
            lines.append("")
        for item in operations:
            if item in self.completed_operations:
                duration = self._duration(self._completed_elapsed.get(item, 0.0))
                lines.append(self._paint(self._truncate(f"  [{check}] {item}  {duration}"), "32"))
            elif item == self.current_operation or item in self._in_flight:
                lines.append(self._paint(self._truncate(f"  [{active}] {item}  running"), "36"))
            else:
                lines.append(self._truncate(f"  [{empty}] {item}"))
        separator = " · " if self._unicode else " | "
        metrics = [f"Elapsed {self._duration(elapsed)}"]
        rate = self._download_rate()
        if rate is not None:
            metrics.append(self._size(rate))
        if self._transfer_stalled(now):
            metrics.append("waiting for data")
        log = self.traces[-TRACE_LIMIT:] or [
            (ended, f"{result} {operation} ({self._duration(duration)})")
            for ended, result, operation, duration in self.history[-TRACE_LIMIT:]
        ]
        trace_path = self.trace_path or Path(".chaos-engine-state/install-trace.json")
        lines.extend(
            self._paint(line, "36")
            for line in self._wrap(
                f"  Trace (last {len(log)} of {self.trace_count}; full log: {trace_path.as_posix()})"
            )
        )
        for ended, message in log:
            lines.extend(self._wrap(f"  [+{self._duration(ended)}] {message}"))
        lines.append(self._paint("  Summary", "36"))
        lines.append(self._paint(self._truncate("  " + separator.join(metrics)), "36"))
        if self.detail:
            lines.append(self._paint(self._truncate(f"  {self.detail}"), "36"))
        if self._lines:
            self.stream.write(f"\x1b[{self._lines}F")
        rendered = "\n".join(line + "\x1b[K" for line in lines) + "\n"
        self.stream.write(rendered)
        self.stream.flush()
        self._lines = len(lines)

    def success(
        self,
        project: Path,
        doctor: dict[str, object],
        clients: dict[str, object],
        *,
        repository: str,
    ) -> None:
        commit = doctor.get("commit") if isinstance(doctor, dict) else None
        if not isinstance(commit, str) or len(commit) != 40:
            commit = None
        doctor_status = doctor.get("status") if isinstance(doctor, dict) else None
        if not isinstance(doctor_status, str) or not doctor_status:
            doctor_status = "unknown"
        components = doctor.get("components") if isinstance(doctor, dict) else None
        healthy = 0
        total = 0
        if isinstance(components, dict):
            for item in components.values():
                if not isinstance(item, dict):
                    continue
                total += 1
                if item.get("status") in {"healthy", "absent"}:
                    healthy += 1
        client_names = sorted(clients) if isinstance(clients, dict) else []
        self.close()
        self.stream.write(self._paint("  Summary", "36") + "\n")
        self.stream.write(
            "Installation Successful! You can now start a new agent session using Codex, Claude, Grok, Gemini, or Copilot. Just ask it to use chaos-engine and you should be good to go!\n"
        )
        if commit is not None:
            self.stream.write(f"Resolved commit: {commit}\n")
        if total:
            self.stream.write(
                f"Doctor: {doctor_status} ({healthy}/{total} components healthy)\n"
            )
        else:
            self.stream.write(f"Doctor: {doctor_status}\n")
        if client_names:
            self.stream.write(f"Clients: {', '.join(client_names)}\n")
        self.stream.write(format_first_session_brief(clients=clients if isinstance(clients, dict) else {}))
        handoff = project / ".chaos-engine-state" / "merge-handoff.md"
        if handoff.is_file() and not handoff.is_symlink():
            doctor = "py -3" if os.name == "nt" else "python3"
            doctor_command = f"{doctor} .chaos-engine/install.py doctor --project ."
            prompt = (
                "Merge ChaosEngine host configuration using "
                ".chaos-engine-state/merge-handoff.md. Follow "
                "chaos-engine/references/installer-program.md deterministic merge. "
                "Preserve every foreign handler and MCP server. Apply only the listed "
                f"owned blocks. Then run {doctor_command} and follow each fix-next."
            )
            self.stream.write(self._paint("  Merge handoff", "36") + "\n")
            self.stream.write(
                "Core is installed. Some host files were left unchanged. Details: "
                f"{handoff.as_posix()}\n"
            )
            self.stream.write(f"`{prompt}`\n")
        heal = project / HEAL_HANDOFF_RELATIVE
        if heal.is_file() and not heal.is_symlink():
            issue_url = "the GitHub issue linked in .chaos-engine-state/heal-handoff.md"
            try:
                for line in heal.read_text(encoding="utf-8").splitlines():
                    if line.startswith("Issue: "):
                        issue_url = line.split("Issue: ", 1)[1].strip()
                        break
            except OSError:
                pass
            doctor_cli = "py -3" if os.name == "nt" else "python3"
            doctor_command = f"{doctor_cli} .chaos-engine/install.py doctor --project ."
            prompt = heal_handoff_prompt(doctor_command, issue_url)
            self.stream.write(self._paint("  Heal handoff", "36") + "\n")
            self.stream.write(
                "Core is installed. One agent step remains. Details: "
                f"{HEAL_HANDOFF_RELATIVE}\n"
            )
            if "issues/new?" in issue_url:
                self.stream.write(
                    "Open this GitHub issue (required fields are filled), then paste the prompt:\n"
                )
            else:
                self.stream.write("GitHub issue:\n")
            self.stream.write(f"{issue_url}\n")
            self.stream.write(f"`{prompt}`\n")
        self.stream.write(
            format_host_onboarding_cards(
                detected=detect_install_hosts(),
                activated=clients if isinstance(clients, dict) else {},
            )
        )
        self.stream.write(f"{installer_user_guide_url(repository)}\n")
        self.stream.write(f"Full install trace: {install_trace_path(project).as_posix()}\n")
        self.stream.flush()

    def close(self) -> None:
        self._stop.set()
        thread = self._thread
        if (
            thread is not None
            and thread is not threading.current_thread()
            and thread.ident is not None
        ):
            thread.join(timeout=1.5)
        self._thread = None
        if self._tty and self._lines:
            self.stream.write("\n")
            self.stream.flush()
        self._lines = 0



def format_first_session_brief(*, clients: dict[str, object] | None = None) -> str:
    """Return the post-install first-session brief (landed / untracked / next 3)."""
    client_names = sorted(clients) if isinstance(clients, dict) else []
    if client_names:
        open_host = (
            "Open one activated host ("
            + ", ".join(client_names)
            + ") in this project."
        )
    else:
        open_host = (
            "Open any supported host in this project "
            "(Codex, Claude Code, Grok, Gemini, or GitHub Copilot)."
        )
    lines = [
        "First-session brief:",
        "  Landed: portable core (`.chaos-engine/`), lifecycle hooks, five host adapters,",
        "    Caveman + Ponytail companions, self-improve skill, Memory / MemPalace / Graphify store tooling.",
        "  Untracked: generated indexes, caches, receipts, and runtimes",
        "    (`.chaos-engine-runtime*`, dependency/host receipts, `graphify-out`, local tool caches).",
        "    Canonical adapters and config stay trackable.",
        "  Next:",
        f"    1. {open_host}",
        "    2. Ask the agent to load / use the `chaos-engine` skill.",
        "    3. Run a small sample task (for example: ask doctor status, or a one-file reversible edit).",
    ]
    return "\n".join(lines) + "\n"


def confirm_operation(operation: str, *, input_stream, output) -> None:
    output.write(f"Confirm {operation}? [y/N] ")
    output.flush()
    if input_stream.readline().strip().casefold() not in {"y", "yes"}:
        raise InstallCancelled(f"ChaosEngine installation cancelled before {operation}")


HOST_DETECT_COMMANDS = (
    ("claude", "claude", "Claude Code"),
    ("codex", "codex", "Codex"),
    ("grok", "grok", "Grok"),
    ("gemini", "gemini", "Gemini"),
    ("copilot", "gh", "GitHub Copilot"),
)

HOST_NEXT_ACTIONS = {
    "claude": "Open Claude Code in this project and ask it to use the chaos-engine skill.",
    "codex": "Start a Codex session in this project and ask it to use chaos-engine.",
    "grok": "Open Grok in this project and ask it to follow AGENTS.md / chaos-engine.",
    "gemini": "Open Gemini CLI in this project and ask it to use the chaos-engine skill.",
    "copilot": "Open this repo in an IDE with GitHub Copilot and ask Copilot to use chaos-engine.",
}


HOST_ONBOARDING_CARDS = {
    "claude": {
        "label": "Claude Code",
        "path": "marketplace/plugin",
        "how": (
            "Install registers a path-unique local marketplace and installs the "
            "`chaos-engine` (+ companions) plugins at project scope. Restart Claude "
            "Code, then ask it to use the chaos-engine skill."
        ),
        "gap": "Requires the `claude` CLI on PATH for automatic marketplace activation.",
    },
    "codex": {
        "label": "Codex",
        "path": "marketplace/plugin",
        "how": (
            "Install registers a path-unique local marketplace and installs the "
            "`chaos-engine` (+ companions) plugins. Restart Codex so it reloads the "
            "plugin cache, then ask it to use chaos-engine."
        ),
        "gap": "Requires the `codex` CLI on PATH for automatic marketplace activation.",
    },
    "grok": {
        "label": "Grok",
        "path": "file/hook injection",
        "how": (
            "Install writes AGENTS.md guidance and project hooks under `.grok/`. "
            "Open Grok in this project, run `grok inspect --json`, and if "
            "`projectTrusted` is false run `/hooks-trust`, then reload hooks."
        ),
        "gap": (
            "Hook trust is host-gated; doctor reports recovery-required until trusted "
            "hooks load."
        ),
    },
    "gemini": {
        "label": "Gemini",
        "path": "file/hook injection",
        "how": (
            "Install writes GEMINI.md / `.gemini/settings.json` and the Node "
            "`hooks/launch.js` launcher. Open Gemini CLI in this project and ask it "
            "to use the chaos-engine skill."
        ),
        "gap": (
            "Needs Node.js for the Gemini hook launcher; unsupported native events "
            "stay explicit capability gaps."
        ),
    },
    "copilot": {
        "label": "GitHub Copilot",
        "path": "file/hook injection",
        "how": (
            "Install writes `.github/copilot-instructions.md` and "
            "`.github/hooks/chaos-engine.json`. Open this repo in an IDE with "
            "GitHub Copilot (or Copilot cloud agent) and ask Copilot to use chaos-engine."
        ),
        "gap": (
            "Copilot is IDE/cloud hosted; CLI detection is soft (`gh` / `code` / `cursor`)."
        ),
    },
}


def format_host_onboarding_cards(
    *,
    detected: list[tuple[str, str, bool]] | None = None,
    activated: dict[str, object] | None = None,
) -> str:
    """Render five host onboarding cards with enablement path and explicit gaps."""
    detected_map = {
        host_id: found for host_id, _label, found in (detected or [])
    }
    activated_names = {
        str(name).casefold() for name in (activated or {})
    }
    lines = ["Host onboarding cards:"]
    for host_id, _command, _label in HOST_DETECT_COMMANDS:
        card = HOST_ONBOARDING_CARDS[host_id]
        markers: list[str] = []
        if detected_map.get(host_id):
            markers.append("detected")
        if host_id in activated_names or any(
            name == host_id or name.startswith(f"{host_id}-")
            for name in activated_names
        ):
            markers.append("activated")
        marker_text = f" [{', '.join(markers)}]" if markers else ""
        lines.append(f"  {card['label']}{marker_text} — {card['path']}")
        lines.append(f"    how: {card['how']}")
        lines.append(f"    gap: {card['gap']}")
    return "\n".join(lines) + "\n"



def detect_install_hosts(*, which=shutil.which) -> list[tuple[str, str, bool]]:
    """Return (id, label, detected) for the five supported hosts."""
    detected: list[tuple[str, str, bool]] = []
    for host_id, command, label in HOST_DETECT_COMMANDS:
        found = which(command) is not None
        if host_id == "copilot" and not found:
            # Copilot is IDE-hosted; treat a present `code`/`cursor` CLI as a soft signal.
            found = which("code") is not None or which("cursor") is not None
        detected.append((host_id, label, found))
    return detected


def run_first_run_wizard(
    *,
    project: Path,
    repository: str,
    with_maven_tools: bool,
    input_stream,
    output,
    which=shutil.which,
) -> None:
    """Guide a first-time interactive install before any network work."""
    hosts = detect_install_hosts(which=which)
    present = [label for _host_id, label, found in hosts if found]
    absent = [label for _host_id, label, found in hosts if not found]
    output.write("ChaosEngine first-run wizard\n")
    output.write(f"Project: {project}\n")
    output.write(f"Upstream: {repository}\n")
    output.write(
        "This install will add the portable ChaosEngine core, lifecycle hooks, "
        "Memory, MemPalace, Graphify CLI, five host adapters, and the Caveman + "
        "Ponytail companion skills (on by default; your off-switches still win).\n"
    )
    if with_maven_tools:
        output.write("Maven Tools MCP will also be installed for this project.\n")
    if present:
        output.write("Detected host CLIs: " + ", ".join(present) + "\n")
    else:
        output.write(
            "No host CLIs detected yet (Claude Code, Codex, Grok, Gemini, or IDE). "
            "Adapters still install for all five hosts.\n"
        )
    if absent:
        output.write("Not detected on PATH: " + ", ".join(absent) + "\n")
    output.write("Next after install:\n")
    for host_id, label, found in hosts:
        marker = "*" if found else "-"
        output.write(f"  {marker} {label}: {HOST_NEXT_ACTIONS[host_id]}\n")
    output.flush()
    confirm_operation("Install companions (Caveman + Ponytail) with the core", input_stream=input_stream, output=output)
    confirm_operation("Continue ChaosEngine install", input_stream=input_stream, output=output)


@contextmanager
def interactive_terminal():
    path = "CONIN$" if os.name == "nt" else os.path.join(os.sep, "dev", "tty")
    try:
        with open(path, "r", encoding="utf-8") as stream:  # noqa: PTH123 - controlling terminal path.
            yield stream
    except OSError as error:
        raise RuntimeError("interactive mode requires a usable controlling terminal") from error


def parse_retry_after(value: str) -> float | None:
    try:
        delay = float(value)
    except ValueError:
        try:
            parsed = email.utils.parsedate_to_datetime(value)
        except (TypeError, ValueError, OverflowError):
            return None
        if parsed is None or parsed.tzinfo is None:
            return None
        delay = max(0.0, parsed.timestamp() - time.time())
    if not 0 <= delay <= MAX_RETRY_AFTER_SECONDS:
        return None
    return delay


def request(url: str) -> urllib.request.Request:
    headers = {"Accept": "application/vnd.github+json", "User-Agent": "ChaosEngine-bootstrap"}
    token = os.environ.get("GITHUB_TOKEN")
    if token:
        headers["Authorization"] = f"Bearer {token}"
    return urllib.request.Request(url, headers=headers)


def valid_branch(branch: str) -> bool:
    parts = branch.split("/")
    return (
        re.fullmatch(r"[^\x00-\x20\x7f~^:?*\\\[\]]+", branch) is not None
        and not branch.startswith(("-", "/"))
        and not branch.endswith(("/", "."))
        and "//" not in branch
        and ".." not in branch
        and "@{" not in branch
        and branch != "HEAD"
        and all(part and not part.startswith(".") and not part.endswith(".lock") for part in parts)
    )


def retry_delay(error: BaseException, attempt: int) -> float | None:
    if isinstance(error, urllib.error.HTTPError):
        retry_after = error.headers.get("Retry-After") if error.headers is not None else None
        if error.code not in TRANSIENT_HTTP_STATUS and not (
            error.code == 403 and retry_after is not None
        ):
            return None
        if retry_after is not None:
            delay = parse_retry_after(retry_after)
            if delay is None:
                return None
            return delay
        if error.code == 429:
            return MAX_RETRY_AFTER_SECONDS
    elif not isinstance(error, (ConnectionError, TimeoutError, urllib.error.URLError)):
        return None
    return RETRY_BASE_SECONDS * (2**attempt)


def read_response(
    opener,
    url: str,
    *,
    limit: int = MAX_RESPONSE_BYTES,
    sleeper=None,
    progress=None,
) -> bytes:
    sleeper = time.sleep if sleeper is None else sleeper
    for attempt in range(MAX_READ_ATTEMPTS):
        try:
            with opener(request(url), timeout=30) as response:
                chunks = []
                total = 0
                while chunk := response.read(min(64 * 1024, limit + 1 - total)):
                    chunks.append(chunk)
                    total += len(chunk)
                    if progress is not None:
                        progress(len(chunk))
                    if total > limit:
                        break
                value = b"".join(chunks)
            break
        except (OSError, TimeoutError, urllib.error.URLError) as error:
            try:
                delay = retry_delay(error, attempt)
            finally:
                if isinstance(error, urllib.error.HTTPError):
                    error.close()
            if delay is None or attempt + 1 == MAX_READ_ATTEMPTS:
                raise RuntimeError(
                    "unable to resolve latest ChaosEngine from the configured upstream"
                ) from error
            sleeper(delay)
    if len(value) > limit:
        raise ValueError("ChaosEngine upstream response exceeds the download limit")
    return value


def resolve_latest(repository: str, branch: str | None, opener=urllib.request.urlopen) -> tuple[str, str]:
    components = repository.split("/")
    if (
        REPOSITORY.fullmatch(repository) is None
        or len(components) != 2
        or any(component in {".", ".."} for component in components)
    ):
        raise ValueError("repository must be an explicit GitHub owner/repository")
    if branch is None:
        repository_document = read_response(
            opener,
            f"https://api.github.com/repos/{repository}",
        )
        try:
            repository_value = json.loads(repository_document)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError("GitHub returned invalid repository metadata") from error
        branch = repository_value.get("default_branch") if isinstance(repository_value, dict) else None
        if not isinstance(branch, str):
            raise ValueError("GitHub returned invalid repository metadata")
    if not valid_branch(branch):
        raise ValueError("branch is invalid")
    if COMMIT.fullmatch(branch) is not None:
        return branch, branch
    encoded_branch = urllib.parse.quote(branch, safe="")
    document = read_response(
        opener,
        f"https://api.github.com/repos/{repository}/commits/{encoded_branch}",
    )
    try:
        value = json.loads(document)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("GitHub returned an invalid ChaosEngine revision") from error
    commit = value.get("sha") if isinstance(value, dict) else None
    if not isinstance(commit, str) or COMMIT.fullmatch(commit) is None:
        raise ValueError("GitHub returned an invalid ChaosEngine revision")
    return commit, branch


def download_source(
    repository: str,
    commit: str,
    destination: Path,
    *,
    opener=urllib.request.urlopen,
    reporter: InstallReporter | None = None,
) -> Path:
    """Download only the bounded ChaosEngine subtree, never the whole repository."""
    encoded_repository = "/".join(
        urllib.parse.quote(part, safe="") for part in repository.split("/")
    )
    document = read_response(
        opener,
        f"https://api.github.com/repos/{encoded_repository}/git/trees/{commit}?recursive=1",
    )
    try:
        value = json.loads(document)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("GitHub returned an invalid ChaosEngine source tree") from error
    if not isinstance(value, dict) or value.get("truncated") is not False:
        raise ValueError("GitHub returned an incomplete ChaosEngine source tree")
    tree = value.get("tree")
    if not isinstance(tree, list):
        raise ValueError("GitHub returned an invalid ChaosEngine source tree")

    selected: list[tuple[PurePosixPath, int]] = []
    total = 0
    for entry in tree:
        if not isinstance(entry, dict) or not isinstance(entry.get("path"), str):
            raise ValueError("GitHub returned an invalid ChaosEngine source tree")
        path = PurePosixPath(entry["path"])
        if path.is_absolute() or ".." in path.parts or not path.parts:
            raise ValueError("ChaosEngine source tree contains an unsafe path")
        if path.parts[0] != "chaos-engine":
            continue
        if entry.get("type") == "tree":
            continue
        if entry.get("type") != "blob" or entry.get("mode") not in {"100644", "100755"}:
            raise ValueError("ChaosEngine source tree contains an unsupported entry")
        size = entry.get("size")
        if not isinstance(size, int) or size < 0 or size > MAX_FILE_BYTES:
            raise ValueError("ChaosEngine source file exceeds the download limit")
        relative = PurePosixPath(*path.parts[1:])
        if not relative.parts:
            raise ValueError("ChaosEngine source tree has an unexpected layout")
        if relative.parts[:2] == ("assets", "brand") or relative.as_posix() in {
            "RESEARCH.md",
            "STANDALONE.md",
        }:
            continue
        selected.append((relative, size))
        total += size

    if not selected:
        raise ValueError("ChaosEngine source tree has an unexpected layout")
    if len(selected) > MAX_FILES:
        raise ValueError("ChaosEngine source tree contains too many files")
    if total > MAX_SOURCE_BYTES:
        raise ValueError("ChaosEngine source tree exceeds the download limit")
    if reporter is not None:
        reporter.begin_download(total, detail=f"{len(selected)} source files")
        reporter.trace(f"download {len(selected)} files ({total} bytes)")

    source = destination / "chaos-engine"
    source.mkdir()
    mkdir_lock = threading.Lock()

    def fetch_blob(item: tuple[PurePosixPath, int]) -> None:
        relative, expected_size = item
        encoded_path = "/".join(urllib.parse.quote(part, safe="") for part in relative.parts)
        content = read_response(
            opener,
            f"https://raw.githubusercontent.com/{encoded_repository}/{commit}/chaos-engine/{encoded_path}",
            limit=MAX_FILE_BYTES,
            progress=None if reporter is None else reporter.downloaded,
        )
        if len(content) != expected_size:
            raise ValueError("ChaosEngine source file does not match the resolved tree")
        target = source.joinpath(*relative.parts)
        with mkdir_lock:
            target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(content)

    workers = max(1, min(DOWNLOAD_WORKERS, len(selected)))
    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures = [pool.submit(fetch_blob, item) for item in selected]
        for future in as_completed(futures):
            future.result()
    if not (source / "skills/chaos-engine/SKILL.md").is_file():
        raise ValueError("ChaosEngine source tree is incomplete")
    return source


def load_installer(source: Path):
    path = source / "install.py"
    return types.SimpleNamespace(**runpy.run_path(str(path)))


def resolve_distribution(installer, project: Path, source: Path, requested: str | None) -> str:
    if isinstance(requested, str) and requested.strip():
        return requested.strip()
    detect = getattr(installer, "detect_distribution", None)
    if callable(detect):
        guessed = detect(project, source)
        if isinstance(guessed, str) and guessed.strip():
            return guessed.strip()
    return "portable"


def install_latest(
    project: Path,
    *,
    repository: str,
    branch: str | None = None,
    skip_tools: bool = False,
    with_maven_tools: bool = False,
    maven_tools_mode: str = "native",
    distribution: str | None = None,
    opener=urllib.request.urlopen,
    provisioner=None,
    interactive: bool = False,
    reporter: InstallReporter | None = None,
    terminal_factory=interactive_terminal,
    bundle_options: dict[str, bool] | None = None,
) -> dict[str, object]:
    if skip_tools and with_maven_tools:
        raise ValueError("--with-maven-tools cannot be combined with --skip-tools")
    project = Path(project).resolve()
    if not project.is_dir():
        raise ValueError(f"project is not a directory: {project}")
    with_maven_tools = wants_maven_tools(
        project, skip_tools=skip_tools, requested=with_maven_tools
    )
    reporter = reporter or InstallReporter()
    reporter.announce(project, repository, branch or "default")
    try:
        terminal_context = terminal_factory() if interactive else None
        if terminal_context is not None:
            terminal_input = terminal_context.__enter__()
        else:
            terminal_input = None
    except OSError as error:
        raise RuntimeError("interactive mode requires a usable controlling terminal") from error
    if terminal_input is not None:
        run_first_run_wizard(
            project=project,
            repository=repository,
            with_maven_tools=with_maven_tools,
            input_stream=terminal_input,
            output=reporter.stream,
        )
    def confirm(name: str) -> None:
        if terminal_input is not None:
            confirm_operation(name, input_stream=terminal_input, output=reporter.stream)
    operations = ["Resolve source", "Download source", "Install core"]
    if not skip_tools:
        operations.extend(("Provision dependencies", "Verify installation", "Activate clients"))
    if with_maven_tools:
        operations.insert(-2, "Install Maven Tools")
    remaining = lambda name: tuple(operations[operations.index(name) + 1 :])
    prior_install = (project / ".chaos-engine").exists()
    temporary = None
    try:
        confirm("Resolve source")
        reporter.start("Resolve source", remaining=remaining("Resolve source"))
        commit, resolved_branch = resolve_latest(repository, branch, opener=opener)
        reporter.complete("Resolve source", remaining=remaining("Resolve source"))
        temporary = tempfile.TemporaryDirectory(prefix="chaos-engine-bootstrap-")
        source_url = f"https://github.com/{repository}/tree/{commit}/chaos-engine"
        confirm("Download source")
        reporter.start("Download source", remaining=remaining("Download source"), detail=source_url)
        source = download_source(
            repository, commit, Path(temporary.name), opener=opener, reporter=reporter
        )
        reporter.complete("Download source", remaining=remaining("Download source"))
        installer = load_installer(source)
        distribution = resolve_distribution(installer, project, source, distribution)
        if distribution == "portable":
            provenance = {
                "kind": "git-digest",
                "repositorySha256": hashlib.sha256(repository.casefold().encode()).hexdigest(),
                "branchSha256": hashlib.sha256(resolved_branch.encode()).hexdigest(),
                "commit": commit,
            }
        else:
            provenance = {
                "kind": "git",
                "repository": repository,
                "branch": resolved_branch,
                "commit": commit,
            }
        confirm("Install core")
        reporter.start("Install core", remaining=remaining("Install core"))
        reporter.trace(f"install core commit={commit} distribution={distribution}")
        if skip_tools:
            target = installer.install(
                project, source, commit, source_record=provenance, distribution=distribution
            )
            reporter.complete("Install core", remaining=remaining("Install core"))
        else:
            # Core and provision are sequential: install_with_dependencies completes
            # "Install core" then starts "Provision dependencies" before deps work.
            confirm("Provision dependencies")
            if with_maven_tools:
                confirm("Install Maven Tools")
            target = installer.install_with_dependencies(
                project,
                source,
                commit,
                provisioner=provisioner,
                source_record=provenance,
                distribution=distribution,
                with_maven_tools=with_maven_tools,
                maven_tools_mode=maven_tools_mode,
                reporter=reporter,
                confirmer=confirm,
                bundle_options=bundle_options,
            )
            if with_maven_tools and "Install Maven Tools" in getattr(
                reporter, "_in_flight", ()
            ):
                reporter.complete(
                    "Install Maven Tools", remaining=remaining("Install Maven Tools")
                )
            if "Provision dependencies" in getattr(reporter, "_in_flight", ()) or (
                reporter.current_operation == "Provision dependencies"
            ):
                reporter.complete(
                    "Provision dependencies",
                    remaining=remaining("Provision dependencies"),
                )
            if "Install core" in getattr(reporter, "_in_flight", ()) or (
                reporter.current_operation == "Install core"
            ):
                reporter.complete("Install core", remaining=remaining("Install core"))
        temporary.cleanup()
    except BaseException:
        reporter.close()
        if temporary is not None:
            temporary.cleanup()
        if terminal_context is not None:
            terminal_context.__exit__(*sys.exc_info())
        raise
    if skip_tools or provisioner is not None:
        if terminal_context is not None:
            terminal_context.__exit__(None, None, None)
        reporter.close()
        return {"status": "installed", "root": str(target), "commit": commit}
    host_controller = installer.load_installed_controller(target, "hosts")
    try:
        reporter.start("Verify installation", remaining=remaining("Verify installation"))
        doctor = installer.doctor_with_dependencies(project, verify_clients=False)
        if _required_install_unhealthy(doctor):
            health_error = InstallHealthError("Verify installation", doctor)
            if not prior_install and core_install_py(project):
                reporter.complete(
                    "Verify installation", remaining=remaining("Verify installation")
                )
                clients = {"clients": {}}
                try:
                    confirm("Activate clients")
                    reporter.start(
                        "Activate clients", remaining=remaining("Activate clients")
                    )
                    if interactive:
                        clients = host_controller.activate_detected_plugins(
                            project, confirmer=confirm
                        )
                    else:
                        clients = host_controller.activate_detected_plugins(project)
                    reporter.complete("Activate clients", remaining=())
                except Exception:
                    clients = {"clients": {}}
                prefix = installer_cli_prefix(project) or "python3 .chaos-engine/install.py"
                fields = installer_issue_fields(
                    "CE-INSTALL-FAILED",
                    health_error,
                    reporter,
                    project,
                    f"{prefix} status --project . --json",
                    f"{prefix} doctor --project . --json",
                )
                issue_url = publish_installer_issue(
                    repository, "CE-INSTALL-FAILED", fields
                )
                write_heal_handoff(project, fields, issue_url)
                doctor["clients"] = clients.get("clients", {})
                if terminal_context is not None:
                    terminal_context.__exit__(None, None, None)
                reporter.success(
                    project, doctor, doctor["clients"], repository=repository
                )
                reporter.close()
                return {
                    "status": "heal-handoff",
                    "root": str(target),
                    "commit": commit,
                    "clients": clients,
                    "doctor": doctor,
                    "issueUrl": issue_url,
                }
            raise health_error
        confirm("Activate clients")
        reporter.start("Activate clients", remaining=remaining("Activate clients"))
        if interactive:
            clients = host_controller.activate_detected_plugins(project, confirmer=confirm)
        else:
            clients = host_controller.activate_detected_plugins(project)
        reporter.complete("Activate clients", remaining=())
        doctor["clients"] = clients.get("clients", {})
    except BaseException as error:
        reporter.close()
        if isinstance(error, InstallHealthError) and prior_install:
            error.observed_upgrade_commit = error.observed_commit
            error.observed_upgrade_components = error.observed_components
            error.observed_upgrade_component_details = error.observed_component_details
        if not isinstance(error, (KeyboardInterrupt, InstallCancelled)):
            if prior_install and (project / ".chaos-engine.backup").exists():
                installer.rollback(project)
        if terminal_context is not None:
            terminal_context.__exit__(*sys.exc_info())
        raise
    if terminal_context is not None:
        terminal_context.__exit__(None, None, None)
    reporter.success(project, doctor, doctor["clients"], repository=repository)
    reporter.close()
    return {
        "status": "installed",
        "root": str(target),
        "commit": commit,
        "clients": clients,
        "doctor": doctor,
    }


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--project", type=Path, default=Path.cwd())
    result.add_argument("--repository", required=True)
    result.add_argument("--branch")
    result.add_argument("--distribution")
    result.add_argument("--skip-tools", action="store_true", help=argparse.SUPPRESS)
    result.add_argument("--with-maven-tools", action="store_true")
    result.add_argument(
        "--maven-tools-mode", choices=("native", "docker"), default="native"
    )
    for bundle_name in (
        "memory",
        "mempalace",
        "graphify",
        "ponytail",
        "caveman",
    ):
        result.add_argument(
            f"--without-{bundle_name}",
            action="store_true",
            help=f"Disable default-on {bundle_name} (Memory/MemPalace/Graphify/Ponytail/Caveman).",
        )
    result.add_argument("--interactive", action="store_true")
    return result


def installer_help_url(repository: str) -> str:
    owner = repository.partition("/")[0].casefold()
    return f"https://{owner}.github.io/docs/agentic/chaos-engine#installer-errors"


def installer_user_guide_url(repository: str) -> str:
    owner = repository.partition("/")[0].casefold()
    return f"https://{owner}.github.io/docs/agentic/chaos-engine"


def installer_cli_prefix(project: Path | None = None) -> str | None:
    root = Path(project) if project is not None else Path.cwd()
    cli = root / ".chaos-engine" / "install.py"
    if not cli.is_file():
        return None
    command = "py -3" if os.name == "nt" else "python3"
    return f"{command} .chaos-engine/install.py"


def classify_install_error(error: BaseException) -> str:
    if isinstance(error, (KeyboardInterrupt, InstallCancelled)):
        return "CE-INSTALL-CANCELLED"
    detail = str(error)
    if "Claude marketplace collision" in detail or "Claude plugin collision" in detail:
        return "CE-CLAUDE-MARKETPLACE-CONFLICT"
    if "interactive mode requires" in detail:
        return "CE-INTERACTIVE-TERMINAL"
    if "checksum" in detail:
        return "CE-INSTALL-CHECKSUM"
    if "unsupported platform" in detail:
        return "CE-INSTALL-UNSUPPORTED-PLATFORM"
    if "entrypoint probe failed" in detail:
        return "CE-INSTALL-PROBE-FAILED"
    return "CE-INSTALL-FAILED"


def one_line_cause(error: BaseException) -> str:
    text = str(error).strip() or error.__class__.__name__
    if isinstance(error, FileNotFoundError) or getattr(error, "winerror", None) == 2:
        missing = None
        if getattr(error, "filename", None):
            missing = Path(str(error.filename)).name
        elif error.args:
            missing = str(error.args[-1])
        if missing and "dependency launcher not found" not in text:
            text = (
                f"dependency launcher not found: {missing} "
                f"(WinError 2 / file not found). fix-next: install `{missing}` on PATH "
                f"then rerun the ChaosEngine install one-liner"
            )
    text = " ".join(text.split())
    # Non-HTML [path] so GitHub issue forms cannot strip the marker and leave a
    # mount prefix such as /media/.../OS after redacting /Users/...
    text = re.sub(
        r"(?<!:)(?:[A-Za-z]:[\\/]|\\\\[^\s\\/]+[\\/]|/(?:(?:media|mnt|Volumes)(?:/\S+?)?/(?:Users|home)|home|Users|tmp|var|private)/)\S+",
        "[path]",
        text,
    )
    return re.sub(
        r"(?i)\b(token|secret|password|api_key)=\S+",
        lambda match: f"{match.group(1)}=<redacted>",
        text,
    )


def installer_issue_fields(
    code: str,
    error: BaseException,
    reporter: InstallReporter | None,
    project: Path | None,
    status_command: str,
    doctor_command: str,
) -> dict[str, str]:
    """Build every installer issue-form field from local, redacted evidence."""
    runtime = runtime_environment()
    payload = doctor_failure_payload(error)
    doctor_details = "not reported"
    if payload.get("components"):
        labels: list[str] = []
        components = payload["components"]
        if isinstance(components, dict):
            for name, item in components.items():
                if not isinstance(item, dict):
                    continue
                mark = item.get("code") or item.get("detail") or item.get("status")
                if isinstance(mark, str):
                    labels.append(f"{name}:{mark}")
        if labels:
            doctor_details = ",".join(labels)[:240]
    hosts_receipt = "unknown"
    core_dir = "unknown"
    install_py = "unknown"
    install_trace = "not available"
    install_trace_snippet = ""
    console_log = ""
    doctor_json = ""
    if project is not None:
        try:
            root = Path(project).resolve()
            hosts_receipt = (
                "present" if (root / ".chaos-engine-hosts.json").is_file() else "absent"
            )
            core_dir = "present" if (root / ".chaos-engine").is_dir() else "absent"
            install_py = (
                "present" if (root / ".chaos-engine" / "install.py").is_file() else "absent"
            )
            trace = install_trace_path(root)
            if trace.is_file():
                install_trace = ".chaos-engine-state/install-trace.json"
                raw = redact_report_text(trace.read_text(encoding="utf-8"))
                lines = [line.strip() for line in raw.splitlines() if line.strip()]
                install_trace_snippet = " | ".join(lines[-6:])[:400]
            console_rel, doctor_rel = write_failure_artifacts(root, reporter, error)
            console_path = root / console_rel
            if console_path.is_file():
                console_log = redact_report_text(console_path.read_text(encoding="utf-8"))
            if doctor_rel != "not available":
                doctor_path = root / doctor_rel
                if doctor_path.is_file():
                    doctor_json = redact_report_text(doctor_path.read_text(encoding="utf-8"))
        except OSError:
            pass
    return {
        "error_code": code,
        "cause": one_line_cause(error)[:240],
        "failed_phase": getattr(error, "phase", None)
        or (reporter.current_operation if reporter else "unknown")
        or "unknown",
        "unhealthy": ", ".join(getattr(error, "unhealthy", ())) or "not reported",
        "platform": sys.platform,
        "os_name": runtime["os_name"] or "unknown",
        "os_version": runtime["os_version"] or "unknown",
        "architecture": runtime["architecture"] or "unknown",
        "python_version": runtime["python_version"] or "unknown",
        "machine": runtime["machine"] or "unknown",
        "doctor_details": doctor_details or "not reported",
        "hosts_receipt": hosts_receipt,
        "core_dir": core_dir,
        "install_py": install_py,
        "install_trace": install_trace,
        "install_trace_snippet": install_trace_snippet,
        "console_log": console_log,
        "doctor_json": doctor_json,
        "status_command": status_command,
        "doctor_command": doctor_command,
        "additional": "Auto-filled by the ChaosEngine installer.",
    }


def publish_installer_issue(
    repository: str,
    code: str,
    fields: dict[str, str],
    *,
    opener=urllib.request.urlopen,
    token: str | None = None,
    extra: dict[str, str] | None = None,
) -> str:
    title = f"[ChaosEngine installer] {code}"
    resolved = resolve_issue_token(token)
    if resolved:
        created = create_installer_github_issue(
            repository, title, issue_form_markdown(fields), resolved, opener=opener
        )
        if created:
            return created
    return encode_issue_form_url(repository, title, fields, extra=extra)


def emit_install_failure(
    code: str,
    error: BaseException,
    repository: str,
    reporter: InstallReporter | None = None,
    project: Path | None = None,
    opener=urllib.request.urlopen,
    token: str | None = None,
) -> str | None:
    print(file=sys.stderr)
    if code == "CE-INSTALL-CANCELLED":
        print(f"{code}: installation interrupted", file=sys.stderr)
        print("Last verified generation was kept.", file=sys.stderr)
        print("Rerun the same install command to continue.", file=sys.stderr)
    else:
        print(f"{code}: {one_line_cause(error)}", file=sys.stderr)
        cause = one_line_cause(error).casefold()
        if isinstance(error, InstallHealthError) or "doctor did not report" in cause:
            print(
                "Next fix: paste the heal prompt below into any supported host in this folder.",
                file=sys.stderr,
            )
        elif "checksum" in cause:
            print(
                "Next fix: check network/proxy interference, then rerun the same install one-liner.",
                file=sys.stderr,
            )
        elif "timed out" in cause or "temporary failure" in cause or "network" in cause:
            print(
                "Next fix: restore network connectivity, then rerun the same install one-liner.",
                file=sys.stderr,
            )
        elif "python" in cause and ("not found" in cause or "required" in cause):
            print(
                "Next fix: install Python 3 (or leave it absent so the wrapper bootstraps uv), "
                "then rerun the same install one-liner.",
                file=sys.stderr,
            )
        elif "core is missing" in cause or "ce_core_missing" in cause:
            print(
                "Next fix: rerun the same install one-liner so ChaosEngine can restore "
                ".chaos-engine under the existing host receipt (or uninstall, then install).",
                file=sys.stderr,
            )
    print(file=sys.stderr)
    if project is not None:
        try:
            root = Path(project).resolve()
            receipt = root / ".chaos-engine-hosts.json"
            core = root / ".chaos-engine"
            install_py = core / "install.py"
            print(
                "Filesystem: "
                f"hosts_receipt={'present' if receipt.is_file() else 'absent'}; "
                f"core_dir={'present' if core.is_dir() else 'absent'}; "
                f"install_py={'present' if install_py.is_file() else 'absent'}",
                file=sys.stderr,
            )
            trace = root / ".chaos-engine-state" / "install-trace.json"
            if trace.is_file():
                print(
                    "Install trace: .chaos-engine-state/install-trace.json",
                    file=sys.stderr,
                )
                print(
                    "Attach .chaos-engine-state/install-trace.json to the GitHub issue.",
                    file=sys.stderr,
                )
            print(
                "Attach .chaos-engine-state/install-console.log and "
                ".chaos-engine-state/doctor-failure.json when present.",
                file=sys.stderr,
            )
        except OSError:
            # Best-effort diagnostics only; path resolution/stat failures must not hide the install error.
            pass
    print(f"Help: {installer_help_url(repository)}", file=sys.stderr)
    prefix = installer_cli_prefix(project)
    status_command = f"{prefix} status --project . --json" if prefix else None
    doctor_command = f"{prefix} doctor --project . --json" if prefix else None
    if prefix:
        print(f"Status: {status_command}", file=sys.stderr)
        print(f"Doctor: {doctor_command}", file=sys.stderr)
    else:
        print("Installer CLI is not on disk.", file=sys.stderr)
        print("Rerun the same install command to continue.", file=sys.stderr)
        status_command = "not available"
        doctor_command = "not available"
    if code != "CE-INSTALL-CANCELLED":
        fields = installer_issue_fields(
            code,
            error,
            reporter,
            project,
            status_command or "not available",
            doctor_command or "not available",
        )
        issue_url = publish_installer_issue(
            repository,
            code,
            fields,
            opener=opener,
            token=token,
            extra=upgrade_query_extras(error),
        )
        prompt = heal_handoff_prompt(fields["doctor_command"], issue_url)
        if "issues/new?" in issue_url:
            print(
                "Next step: click this link to open a GitHub issue with this report:",
                file=sys.stderr,
            )
        else:
            print("Next step: review the filed GitHub issue:", file=sys.stderr)
        print(issue_url, file=sys.stderr)
        print("Give this prompt to your agent in this folder:", file=sys.stderr)
        print(f"`{prompt}`", file=sys.stderr)
        if os.environ.get("CHAOS_ENGINE_DEBUG") == "1":
            traceback.print_exc()
        return issue_url
    if os.environ.get("CHAOS_ENGINE_DEBUG") == "1":
        traceback.print_exc()
    return None


def main() -> int:
    reporter = InstallReporter()
    args = parser().parse_args()
    try:
        bundle_options = {
            name: not getattr(args, f"without_{name}", False)
            for name in (
                "memory",
                "mempalace",
                "graphify",
                "ponytail",
                "caveman",
            )
        }
        result = install_latest(
            args.project,
            repository=args.repository,
            branch=args.branch,
            skip_tools=args.skip_tools,
            with_maven_tools=args.with_maven_tools,
            maven_tools_mode=args.maven_tools_mode,
            distribution=args.distribution,
            interactive=args.interactive,
            reporter=reporter,
            bundle_options=bundle_options,
        )
        write_install_trace(Path(args.project).resolve(), result, reporter.traces)
    except BaseException as error:
        if isinstance(error, SystemExit):
            raise
        reporter.close()
        code = classify_install_error(error)
        write_install_trace(
            Path(args.project).resolve(),
            {"status": "failed", "error": code},
            reporter.traces,
        )
        emit_install_failure(
            code,
            error,
            args.repository,
            reporter=reporter,
            project=Path(args.project).resolve(),
        )
        return 1
    reporter.close()
    if not getattr(sys.stdout, "isatty", lambda: False)():
        print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
