"""Structural check for a retrospective ``## Enhancements`` block.

Reads that block only. It does not classify the rest of the chat.
"""

from __future__ import annotations

import re

_HEADING = re.compile(r"(?im)^##[ \t]+enhancements[ \t]*$")
_NEXT = re.compile(r"(?m)^##[ \t]+\S")
_BULLET = re.compile(r"^(?:[-*+]|\d+[.)])\s+(.*\S)\s*$")
_MARK = re.compile(r"^(shipped|tracked|rejected)\s*:\s*(.*?)\s*$", re.IGNORECASE)
_URL = re.compile(
    r"https://github\.com/[^\s)]+/(?:pull|issues|commit)/[^\s)]+",
    re.IGNORECASE,
)
_BANNED_EXACT = frozenset({"later"})
_BANNED_PARTS = ("out of scope", "separate decision")


def _block(note: str) -> str | None:
    match = _HEADING.search(note or "")
    if match is None:
        return None
    rest = (note or "")[match.end() :]
    nxt = _NEXT.search(rest)
    return rest[: nxt.start()] if nxt else rest


def _normalize(reason: str) -> str:
    text = reason.casefold().replace("-", " ")
    text = re.sub(r"[^a-z0-9 ]+", " ", text)
    return " ".join(text.split())


def _rows(block: str) -> tuple[list[str], list[tuple[str | None, str]]]:
    errors: list[str] = []
    rows: list[tuple[str | None, str]] = []
    for raw in block.splitlines():
        bullet = _BULLET.match(raw.strip())
        if bullet is None:
            continue
        body = bullet.group(1).strip()
        mark = _MARK.match(body)
        if mark is None:
            errors.append(f"unmarked enhancement: {body}")
            rows.append((None, body))
            continue
        kind = mark.group(1).lower()
        rest = mark.group(2).strip()
        if kind in {"shipped", "tracked"}:
            if _URL.search(rest) is None:
                errors.append(f"{kind} enhancement lacks a GitHub URL: {body}")
            rows.append((kind, rest))
            continue
        reason = _normalize(rest)
        if not reason:
            errors.append("rejected enhancement lacks a reason")
        elif reason in _BANNED_EXACT or any(part in reason for part in _BANNED_PARTS):
            errors.append(f"rejected enhancement reason is not a disposition: {rest}")
        rows.append(("rejected", rest))
    return errors, rows


def enhancement_errors(note: str) -> list[str]:
    block = _block(note)
    if block is None:
        return ["missing ## Enhancements block"]
    errors, rows = _rows(block)
    nothing = "nothing actionable" in (note or "").casefold()
    tracked = any(kind == "tracked" for kind, _rest in rows)
    unmarked = any(kind is None for kind, _rest in rows)
    if nothing and (tracked or unmarked):
        errors.append(
            "nothing actionable is invalid while a tracked or unmarked enhancement remains"
        )
    return errors


def tracked_urls(note: str) -> list[str]:
    block = _block(note)
    if block is None:
        return []
    _errors, rows = _rows(block)
    urls: list[str] = []
    for kind, rest in rows:
        if kind != "tracked":
            continue
        urls.extend(_URL.findall(rest))
    return list(dict.fromkeys(urls))
