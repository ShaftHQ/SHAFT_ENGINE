#!/usr/bin/env python3
"""
Delivery cleanup helpers for cleanup-at-close (issue 7755).

After a PR merges (or a task closes), bots must:
1. Sweep the worker ownership ledger (registered worktrees/clones/venvs/…).
2. Remove own linked worktrees whose PR is MERGED.
3. Stop leftover headless Chrome/Chromium and Allure report servers.
4. Fail the close gate if any of the above remain.

Never touches Chrome profile dirs (delete-guard never-touch). Process
matching is cmdline-based and user-scoped; report-only helpers never kill.
"""
from __future__ import annotations

import os
import re
import signal
from collections.abc import Callable, Sequence
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in __import__("sys").path:
    __import__("sys").path.insert(0, str(ROOT))

from scripts.ci import worker_ownership  # noqa: E402

# Headless browsers / Allure local servers left after test runs.
_HEADLESS_CHROME_RE = re.compile(
    r"(?i)(chrome|chromium|google-chrome)(\.exe)?\b.*--headless"
)
_ALLURE_SERVE_RE = re.compile(r"(?i)\ballure\b.*\bserve\b")
# Never match profile-bearing chrome without --headless.
_PROFILE_HINT_RE = re.compile(r"(?i)chrome-profile|user-data-dir")


def _read_cmdline(pid: int, *, read_text: Callable[[Path], str] | None = None) -> str:
    reader = read_text or (lambda p: p.read_text(encoding="utf-8", errors="replace"))
    raw = reader(Path(f"/proc/{pid}/cmdline"))
    return raw.replace("\x00", " ").strip()


def list_leftover_report_servers(
    *,
    proc_pids: Sequence[int] | None = None,
    read_cmdline: Callable[[int], str] | None = None,
    uid: int | None = None,
) -> list[dict]:
    """
    Return leftover headless Chrome/Chromium or Allure serve processes.

    ``proc_pids`` / ``read_cmdline`` are injectable for unit tests. When omitted,
    scans ``/proc`` for the current uid.
    """
    reader = read_cmdline or _read_cmdline
    if proc_pids is None:
        proc_root = Path("/proc")
        want_uid = os.getuid() if uid is None else uid
        found: list[int] = []
        for entry in proc_root.iterdir():
            if not entry.name.isdigit():
                continue
            try:
                if entry.stat().st_uid != want_uid:
                    continue
            except OSError:
                continue
            found.append(int(entry.name))
        proc_pids = found
    hits: list[dict] = []
    for pid in proc_pids:
        try:
            cmdline = reader(int(pid))
        except (OSError, ValueError):
            continue
        if not cmdline:
            continue
        kind = None
        if _ALLURE_SERVE_RE.search(cmdline):
            kind = "allure-serve"
        elif _HEADLESS_CHROME_RE.search(cmdline):
            if _PROFILE_HINT_RE.search(cmdline) and "--headless" not in cmdline.casefold():
                continue
            kind = "headless-chrome"
        if kind:
            hits.append({"pid": int(pid), "kind": kind, "cmdline": cmdline[:300]})
    return hits


def leftover_report_server_blockers(
    servers: Sequence[dict] | None = None,
) -> list[str]:
    rows = list(servers) if servers is not None else list_leftover_report_servers()
    if not rows:
        return []
    return [f"report-servers-remain:{len(rows)}"]


def stop_leftover_report_servers(
    servers: Sequence[dict] | None = None,
    *,
    owned_path_substrings: Sequence[str] | None = None,
    kill: Callable[[int, int], None] | None = None,
    sig: int = signal.SIGTERM,
) -> list[dict]:
    """
    SIGTERM leftover report servers. Returns the rows signalled.

    Injectable ``kill`` for tests. Never targets non-headless profile browsers.

    When ``owned_path_substrings`` is provided (ownership-ledger paths), only
    processes whose cmdline contains one of those substrings are signalled —
    so a shared-box bot cannot SIGTERM another bot's report server.
    When omitted, this helper is report-only (returns [] and signals nothing).
    """
    rows = list(servers) if servers is not None else list_leftover_report_servers()
    if owned_path_substrings is None:
        return []
    needles = [str(s).strip() for s in owned_path_substrings if str(s).strip()]
    if not needles:
        return []
    sender = kill or os.kill
    signalled: list[dict] = []
    for row in rows:
        cmdline = str(row.get("cmdline") or "")
        if not any(n in cmdline for n in needles):
            continue
        pid = int(row["pid"])
        try:
            sender(pid, sig)
        except OSError:
            continue
        signalled.append(row)
    return signalled


def _publish_media_blockers(
    *,
    live_publish: bool,
    coach_accept: dict | None,
    publish_package: dict | None,
    media_delivery: dict | None,
    dirty_unpushed_worktrees: Sequence[str] | None,
    leftover_browsers: Sequence[str] | None,
    allure_serve: bool,
) -> list[str]:
    """Close blockers added by the 2026-11-06 publish, media, and leftover gates."""
    blockers: list[str] = []
    if live_publish:
        accepted = isinstance(coach_accept, dict) and (
            coach_accept.get("accepted") is True or coach_accept.get("coach_gate_pass") is True
        )
        if not accepted:
            blockers.append("live-publish-without-coach-accept")
        elif not coach_accept.get("facts_checked"):
            blockers.append("coach-accept-missing-facts")
    if isinstance(publish_package, dict):
        if not publish_package.get("render_390"):
            blockers.append("missing-390px-render")
        if not publish_package.get("render_desktop"):
            blockers.append("missing-desktop-render")
        if not publish_package.get("contrast_note"):
            blockers.append("missing-contrast-note")
    if isinstance(media_delivery, dict):
        start = media_delivery.get("human_listen_start") or media_delivery.get("human_look_start")
        end = media_delivery.get("human_listen_end") or media_delivery.get("human_look_end")
        if not start or not end:
            blockers.append("media-missing-human-listen")
    dirty = [str(p) for p in (dirty_unpushed_worktrees or []) if str(p).strip()]
    if dirty:
        blockers.append(f"dirty-unpushed-worktrees:{len(dirty)}")
    browsers = [str(p) for p in (leftover_browsers or []) if str(p).strip()]
    if browsers:
        blockers.append(f"leftover-browsers:{len(browsers)}")
    if allure_serve:
        blockers.append("allure-serve-running")
    return blockers


def delivery_close_blockers(
    *,
    git_status_clean: bool,
    ownership_manifest: Path | str | None = None,
    leftover_merged_worktrees: Sequence[str] | None = None,
    leftover_report_servers: Sequence[dict] | str | None = None,
    session_logs_cleared: bool = True,
    live_publish: bool = False,
    coach_accept: dict | None = None,
    publish_package: dict | None = None,
    media_delivery: dict | None = None,
    dirty_unpushed_worktrees: Sequence[str] | None = None,
    leftover_browsers: Sequence[str] | None = None,
    allure_serve: bool = False,
) -> list[str]:
    """
    Compose close-gate blockers for a delivered task (issue 7755).
    """
    from scripts.ci.box_disk_guard import item_close_blockers

    created: list[str] = []
    accounted = True
    artifact_paths: list[str] = []
    if ownership_manifest is not None:
        remaining = worker_ownership.remaining_artifacts(ownership_manifest)
        artifact_paths = [str(e.get("path")) for e in remaining]
        created = worker_ownership.listed_paths(ownership_manifest)
        accounted = not remaining
    server_labels: list[str] = []
    if leftover_report_servers is None:
        server_labels = [
            f"{r.get('kind')}:{r.get('pid')}" for r in list_leftover_report_servers()
        ]
    elif isinstance(leftover_report_servers, list):
        for item in leftover_report_servers:
            if isinstance(item, dict):
                server_labels.append(f"{item.get('kind')}:{item.get('pid')}")
            else:
                server_labels.append(str(item))
    blockers = item_close_blockers(
        git_status_clean=git_status_clean,
        created_files=created,
        created_files_accounted=accounted,
        session_logs_cleared=session_logs_cleared,
        leftover_merged_worktrees=leftover_merged_worktrees,
        leftover_registered_artifacts=artifact_paths,
        leftover_report_servers=server_labels,
    )
    blockers.extend(
        _publish_media_blockers(
            live_publish=live_publish,
            coach_accept=coach_accept,
            publish_package=publish_package,
            media_delivery=media_delivery,
            dirty_unpushed_worktrees=dirty_unpushed_worktrees,
            leftover_browsers=leftover_browsers,
            allure_serve=allure_serve,
        )
    )
    return blockers


if __name__ == "__main__":
    raise SystemExit("import delivery_cleanup; do not run as a script")
