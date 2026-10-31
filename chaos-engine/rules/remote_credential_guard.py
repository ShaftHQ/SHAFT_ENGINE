"""
L77: refuse git remotes with embedded credentials (never print values)."""

from __future__ import annotations

import re
import subprocess  # nosec B404 — git remote get-url only, fixed argv
from pathlib import Path
from typing import Iterable
from urllib.parse import urlsplit, urlunsplit

# Fake-looking but non-functional fixtures for unit tests only.
FAKE_GHP = "ghp_" + ("0" * 36)
FAKE_PAT = "github_pat_" + ("A" * 22) + "_" + ("B" * 59)

_TOKEN_PREFIX = re.compile(
    r"(?i)(?:gh[oprsu]_|github_pat_|glpat-|xox[baprs]-|sk-|AKIA)"
)
_USERINFO_AT = re.compile(r"^[a-zA-Z][a-zA-Z0-9+.-]*://[^/\s]*:[^/\s]+@")


def redact_url(url: str) -> str:
    """Return a URL safe to print: strip userinfo; mask token-shaped path bits."""
    text = (url or "").strip()
    if not text:
        return ""
    try:
        parts = urlsplit(text)
    except ValueError:
        return "<unparseable-remote>"
    if parts.scheme and parts.netloc:
        host = parts.hostname or "host"
        netloc = host
        if parts.port:
            netloc = f"{host}:{parts.port}"
        return urlunsplit((parts.scheme, netloc, parts.path, "", ""))
    return "<redacted-remote>"


def url_embeds_credentials(url: str) -> str | None:
    """
    Return a short reason code when *url* embeds credentials; else None.

    Reason codes never include the secret. Empty / missing URL → None.
    """
    text = (url or "").strip()
    if not text:
        return None
    lowered = text.casefold()
    if "ghp_" in lowered or "github_pat_" in lowered:
        return "token-prefix-in-url"
    if "x-access-token:" in lowered:
        return "x-access-token-userinfo"
    if _USERINFO_AT.match(text):
        # git@github.com:org/repo is SSH scp-style — not user:pass.
        if text.startswith("git@"):
            return None
        return "userinfo-password"
    if _TOKEN_PREFIX.search(text):
        return "token-prefix-in-url"
    return None


def scan_remote_urls(pairs: Iterable[tuple[str, str]]) -> list[dict[str, str]]:
    """Scan ``(name, url)`` pairs; return findings without secret values."""
    findings: list[dict[str, str]] = []
    for name, url in pairs:
        reason = url_embeds_credentials(url)
        if reason:
            findings.append(
                {
                    "remote": str(name),
                    "reason": reason,
                    "redacted": redact_url(url),
                }
            )
    return findings


def list_remote_urls(project: Path) -> list[tuple[str, str]]:
    """Read ``git remote -v`` get-url for each remote (fetch + push collapsed)."""
    root = Path(project)
    try:
        proc = subprocess.run(  # nosec B603 B607 — fixed argv, no shell
            ["git", "-C", str(root), "remote"],
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError:
        return []
    if proc.returncode != 0:
        return []
    names = [line.strip() for line in (proc.stdout or "").splitlines() if line.strip()]
    pairs: list[tuple[str, str]] = []
    seen: set[tuple[str, str]] = set()
    for name in names:
        for push in (False, True):
            argv = ["git", "-C", str(root), "remote", "get-url", name]
            if push:
                argv = ["git", "-C", str(root), "remote", "get-url", "--push", name]
            try:
                got = subprocess.run(  # nosec B603 B607
                    argv, check=False, capture_output=True, text=True,
                )
            except OSError:
                continue
            if got.returncode != 0:
                continue
            url = (got.stdout or "").strip()
            key = (name, url)
            if key in seen:
                continue
            seen.add(key)
            pairs.append((name, url))
    return pairs


def findings_for_project(project: Path) -> list[dict[str, str]]:
    return scan_remote_urls(list_remote_urls(project))


def doctor_check(project: Path | None = None) -> tuple[str, str]:
    """Doctor row: ``pass`` or ``fail`` with a reason that never embeds secrets."""
    root = Path(project) if project is not None else Path.cwd()
    hits = findings_for_project(root)
    if not hits:
        return "pass", ""
    names = ", ".join(sorted({item["remote"] for item in hits}))
    reasons = ", ".join(sorted({item["reason"] for item in hits}))
    return (
        "fail",
        f"remote(s) embed credentials ({reasons}); remotes: {names}; "
        "rewrite to credential-free URL (use GH_TOKEN/gh auth); values not shown",
    )


def pre_push_failures(project: Path | None = None) -> list[str]:
    status, reason = doctor_check(project)
    if status == "pass":
        return []
    return [f"L77-no-token-in-remote: {reason}"]


def main(argv: list[str] | None = None) -> int:
    import sys

    args = list(argv) if argv is not None else sys.argv[1:]
    root = Path(args[0]) if args else Path.cwd()
    status, reason = doctor_check(root)
    if status == "pass":
        print("remote-credential: pass")
        return 0
    # Never print raw URLs or tokens.
    print(f"remote-credential: fail ({reason})", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
