#!/usr/bin/env python3
"""Owned MCP uniqueness — no duplicate GitHub (or other) server pairs."""

from __future__ import annotations

from collections.abc import Iterable

ALIAS_GROUPS: tuple[frozenset[str], ...] = (
    frozenset({"github", "github-gh", "github_gh"}),
)


def duplicate_groups(server_ids: Iterable[str]) -> list[tuple[str, ...]]:
    names = {str(item).strip().casefold() for item in server_ids if str(item).strip()}
    found: list[tuple[str, ...]] = []
    for group in ALIAS_GROUPS:
        hit = sorted(names & group)
        if len(hit) > 1:
            found.append(tuple(hit))
    counts: dict[str, int] = {}
    for item in server_ids:
        key = str(item).strip().casefold()
        if not key:
            continue
        counts[key] = counts.get(key, 0) + 1
    for key, count in sorted(counts.items()):
        if count > 1:
            found.append((key, key))
    return found


def uniqueness_error(server_ids: Iterable[str]) -> str | None:
    groups = duplicate_groups(server_ids)
    if not groups:
        return None
    rendered = ", ".join("+".join(group) for group in groups)
    return (
        "Duplicate MCP servers: "
        f"{rendered}. Keep one GitHub MCP. Repair: disable extras in host MCP config."
    )
