#!/usr/bin/env python3
"""Resolve and install the latest portable ChaosEngine from a GitHub branch."""

from __future__ import annotations

import argparse
from collections import deque
from contextlib import contextmanager
import email.utils
import hashlib
import json
import math
import os
import re
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
TRACE_LIMIT = 12
STALL_SECONDS = 8.0


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
    if value.get("taskImpact") == "optional" and status == "absent":
        return False
    return value.get("taskImpact") == "required"


def _required_install_unhealthy(
    doctor: dict[str, object], *, include_hosts: bool = True
) -> bool:
    components = doctor.get("components")
    if isinstance(components, dict) and any(
        _component_blocks_health(value) for value in components.values()
    ):
        return True
    keys = ("kernel", "hosts", "dependencies") if include_hosts else ("kernel", "dependencies")
    for key in keys:
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
        super().__init__("ChaosEngine doctor did not report a healthy installation")
        self.phase = phase
        components = doctor.get("components", {})
        self.unhealthy = tuple(
            name
            for name, value in components.items()
            if _component_blocks_health(value)
        ) if isinstance(components, dict) else ()


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
        self._displayed_eta: float | None = None
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

    def _height(self) -> int:
        return max(1, shutil.get_terminal_size(fallback=(80, 24)).lines)

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

    def _eta_duration(self, seconds: float) -> str:
        return self._duration(max(1, math.ceil(seconds)))

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
                if self._tty:
                    self._render_locked()
                else:
                    self._render_non_tty_heartbeat_locked()

    def _render_non_tty_heartbeat_locked(self) -> None:
        now = self.clock()
        if not self._transfer_stalled(now):
            return
        remaining = self._remaining(now)
        metrics = ([f"remaining {remaining}"] if remaining is not None else []) + [
            "waiting for data"
        ]
        self.stream.write(self._truncate("  " + " | ".join(metrics)) + "\n")
        self.stream.flush()

    def _remaining(self, now: float) -> str | None:
        rate = self._download_rate()
        stage_estimate = self._stage_estimate()
        pending_count = sum(
            operation not in self._in_flight for operation in self.remaining_operations
        )
        if self._download_total is not None and rate is not None:
            candidate = max(0, self._download_total - self._downloaded) / rate
            candidate += pending_count * stage_estimate
        elif self.current_operation is not None and stage_estimate > 0:
            current_elapsed = self._elapsed_as_current.get(self.current_operation, 0.0)
            if self._current_started is not None:
                current_elapsed += max(0.0, now - self._current_started)
            candidate = max(0.0, stage_estimate - current_elapsed)
            candidate += pending_count * stage_estimate
        elif self._transfer_stalled(now) and self._displayed_eta is not None:
            return self._eta_duration(self._displayed_eta)
        else:
            return None
        self._displayed_eta = candidate if self._displayed_eta is None else min(
            self._displayed_eta, candidate
        )
        return self._eta_duration(self._displayed_eta)

    def _stage_estimate(self) -> float:
        durations = [value for value in self._completed_elapsed.values() if value > 0]
        return sum(durations) / len(durations) if durations else 0.0

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
        remaining = self._remaining(now)
        if remaining is not None:
            metrics.append(f"remaining {remaining}")
        if self._transfer_stalled(now):
            metrics.append("waiting for data")
        log = self.traces[-TRACE_LIMIT:] or [
            (ended, f"{result} {operation} ({self._duration(duration)})")
            for ended, result, operation, duration in self.history[-TRACE_LIMIT:]
        ]
        trace_path = self.trace_path or Path(".chaos-engine-state/install-trace.json")
        trace_heading = [
            self._paint(line, "36")
            for line in self._wrap(
                f"  Trace (last {len(log)} of {self.trace_count}; full log: {trace_path.as_posix()})"
            )
        ]
        trace_lines: list[str] = []
        for ended, message in log:
            trace_lines.extend(self._wrap(f"  [+{self._duration(ended)}] {message}"))
        summary_lines = [
            self._paint("  Summary", "36"),
            self._paint(self._truncate("  " + separator.join(metrics)), "36"),
        ]
        if self.detail:
            summary_lines.append(self._paint(self._truncate(f"  {self.detail}"), "36"))
        height = self._height()
        trace_budget = max(0, height - len(lines) - len(trace_heading) - len(summary_lines))
        visible_trace = trace_lines[-trace_budget:] if trace_budget else []
        lines.extend([*trace_heading, *visible_trace, *summary_lines])
        if len(lines) > height:
            lines = lines[-height:]
        if self._lines:
            self.stream.write(f"\x1b[{min(self._lines, height)}F")
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
        del doctor, clients
        self.close()
        self.stream.write(self._paint("  Summary", "36") + "\n")
        self.stream.write(
            "Installation Successful! You can now start a new agent session using Codex, Claude, Grok, Gemini, or Copilot. Just ask it to use chaos-engine and you should be good to go!\n"
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


def confirm_operation(operation: str, *, input_stream, output) -> None:
    output.write(f"Confirm {operation}? [y/N] ")
    output.flush()
    if input_stream.readline().strip().casefold() not in {"y", "yes"}:
        raise InstallCancelled(f"ChaosEngine installation cancelled before {operation}")


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
    for relative, expected_size in selected:
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
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(content)
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
                "upstreamRepository": repository,
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
        if skip_tools:
            target = installer.install(
                project, source, commit, source_record=provenance, distribution=distribution
            )
        else:
            confirm("Provision dependencies")
            reporter.start("Provision dependencies", remaining=remaining("Provision dependencies"))
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
            )
            if with_maven_tools:
                reporter.complete(
                    "Install Maven Tools", remaining=remaining("Install Maven Tools")
                )
            reporter.complete("Provision dependencies", remaining=remaining("Provision dependencies"))
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
        if _required_install_unhealthy(doctor, include_hosts=False):
            raise InstallHealthError("Verify installation", doctor)
        reporter.complete("Verify installation", remaining=remaining("Verify installation"))
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
    text = " ".join(text.split())
    text = re.sub(
        r"(?<!:)(?:[A-Za-z]:[\\/]|/(?:home|Users|tmp|var|private)/)\S+",
        "<path>",
        text,
    )
    return re.sub(
        r"(?i)\b(token|secret|password|api_key)=\S+",
        lambda match: f"{match.group(1)}=<redacted>",
        text,
    )


def emit_install_failure(
    code: str,
    error: BaseException,
    repository: str,
    reporter: InstallReporter | None = None,
    project: Path | None = None,
) -> None:
    print(file=sys.stderr)
    if code == "CE-INSTALL-CANCELLED":
        print(f"{code}: installation interrupted", file=sys.stderr)
        print("Last verified generation was kept.", file=sys.stderr)
        print("Rerun the same install command to continue.", file=sys.stderr)
    else:
        print(f"{code}: {one_line_cause(error)}", file=sys.stderr)
    print(file=sys.stderr)
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
        status_command = "not available; .chaos-engine/install.py is not on disk"
        doctor_command = status_command
    if code != "CE-INSTALL-CANCELLED":
        body = "\n".join(
            (
                f"Error code: {code}",
                f"Cause: {one_line_cause(error)[:240]}",
                f"Failed phase: {getattr(error, 'phase', None) or (reporter.current_operation if reporter else 'unknown')}",
                "Unhealthy components: "
                + (", ".join(getattr(error, "unhealthy", ())) or "not reported"),
                "Current action: "
                + ((reporter.current_operation if reporter else None) or "none"),
                "History: "
                + (
                    "; ".join(
                        f"{result} {operation} ({reporter._duration(duration)})"
                        for _, result, operation, duration in reporter.history[-5:]
                    )
                    if reporter and reporter.history
                    else "none"
                ),
                f"Platform: {sys.platform}",
                f"Status command: {status_command}",
                f"Doctor command: {doctor_command}",
            )
        )
        query = urllib.parse.urlencode(
            {
                "template": "chaos-engine-installer.yml",
                "title": f"[ChaosEngine installer] {code}",
                "error_code": code,
                "cause": one_line_cause(error)[:240],
                "failed_phase": getattr(error, "phase", None)
                or (reporter.current_operation if reporter else "unknown"),
                "unhealthy": ", ".join(getattr(error, "unhealthy", ())) or "not reported",
                "platform": sys.platform,
                "status_command": status_command or "",
                "doctor_command": doctor_command or "",
            }
        )
        print(
            "Next step: click this link to open a GitHub issue with this report:",
            file=sys.stderr,
        )
        print(f"https://github.com/{repository}/issues/new?{query}", file=sys.stderr)
    if os.environ.get("CHAOS_ENGINE_DEBUG") == "1":
        traceback.print_exc()


def main() -> int:
    reporter = InstallReporter()
    args = parser().parse_args()
    try:
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
