"""Memory full-dump: multi-query recall beyond top-50 with count check."""

from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass, field
from typing import Any, Iterable, Sequence

DEFAULT_TOP_N = 50
_SECRETISH = re.compile(
    r"(?i)(?:gh[oprsu]_|github_pat_|sk-|api[_-]?key|password|secret|token)"
    r"[A-Za-z0-9_:=./+\-]{8,}"
)


@dataclass
class QueryBatch:
    query: str
    hits: list[str]
    top_n: int = DEFAULT_TOP_N

    @property
    def hit_cap(self) -> bool:
        return len(self.hits) >= self.top_n


@dataclass
class DumpReport:
    exported_unique: int
    total: int | None
    cap_incomplete: bool
    queries: int
    count_ok: bool
    notes: list[str] = field(default_factory=list)


def fingerprint(text: str) -> str:
    return hashlib.sha256((text or "").encode("utf-8")).hexdigest()


def redact(text: str) -> str:
    """Strip credential-shaped spans; keep surrounding prose."""
    return _SECRETISH.sub("[REDACTED]", text or "")


def merge_batches(
    batches: Sequence[QueryBatch],
    *,
    transcript_extras: Iterable[str] = (),
    host_total: int | None = None,
) -> tuple[list[str], DumpReport]:
    """Dedup + count-check. Returns (unique_texts, report)."""
    ordered: list[str] = []
    seen: set[str] = set()
    cap_incomplete = False
    for batch in batches:
        if batch.hit_cap:
            cap_incomplete = True
        for hit in batch.hits:
            clean = redact(hit).strip()
            if not clean:
                continue
            fp = fingerprint(clean)
            if fp in seen:
                continue
            seen.add(fp)
            ordered.append(clean)
    for extra in transcript_extras:
        clean = redact(extra).strip()
        if not clean:
            continue
        fp = fingerprint(clean)
        if fp in seen:
            continue
        seen.add(fp)
        ordered.append(clean)
        # Transcript fallback used → still note if recall was capped.
    exported = len(ordered)
    notes: list[str] = []
    if host_total is None:
        count_ok = not cap_incomplete
        if cap_incomplete:
            notes.append(
                "host total unknown; at least one query hit top-N cap — "
                "re-run with more seeds or ReadTranscript fallback"
            )
        else:
            notes.append("host total unknown; no query hit top-N cap")
    else:
        count_ok = exported == host_total and not (
            cap_incomplete and exported < host_total
        )
        if exported != host_total:
            notes.append(
                f"count mismatch: exported={exported} total={host_total}"
            )
        if cap_incomplete and exported < host_total:
            notes.append("cap incomplete: need more queries or transcript fallback")
    report = DumpReport(
        exported_unique=exported,
        total=host_total,
        cap_incomplete=cap_incomplete,
        queries=len(batches),
        count_ok=count_ok,
        notes=notes,
    )
    return ordered, report


def report_to_dict(report: DumpReport) -> dict[str, Any]:
    return {
        "exported_unique": report.exported_unique,
        "total": report.total,
        "cap_incomplete": report.cap_incomplete,
        "queries": report.queries,
        "count_ok": report.count_ok,
        "notes": list(report.notes),
    }
