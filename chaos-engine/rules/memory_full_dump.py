"""Memory full-dump: multi-query recall beyond top-50 with shard count check.

CE 2026.10.31.8 field-report fixes (Carl 82/209, Cortex 123/126, Cleo 66−1):
- expected total = the bot's OWN shard only (exclude shared/other bots)
- dedupe by fact text + date + type
- host-truncated long facts: match by prefix / normalized id, flag truncated,
  recover full text via ReadTranscript (required step, not optional)
- receipt lists uncaptured ids
- CLI merges RecallMemory JSONL batches without hand-merging
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, Iterable, Sequence

DEFAULT_TOP_N = 50
_SECRETISH = re.compile(
    r"(?i)(?:gh[oprsu]_|github_pat_|sk-|api[_-]?key|password|secret|token)"
    r"[A-Za-z0-9_:=./+\-]{8,}"
)
# Host UIs often truncate mid-sentence with an ellipsis or hard cut.
_TRUNCATION_MARKERS = ("…", "...", "\u2026")
TRANSCRIPT_FALLBACK_REQUIRED = (
    "ReadTranscript fallback is required for truncated or missing shard facts; "
    "it is not optional"
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
class MemoryFact:
    """One memory row. ``bot`` is the owning shard (empty = shared)."""

    text: str
    date: str = ""
    type: str = ""
    fact_id: str = ""
    bot: str = ""
    truncated: bool = False
    source: str = "recall"  # recall | transcript

    def dedupe_key(self) -> str:
        return fingerprint(
            f"{normalize_text(self.text)}|{self.date.strip()}|{self.type.strip().casefold()}"
        )


@dataclass
class DumpReport:
    exported_unique: int
    total: int | None
    cap_incomplete: bool
    queries: int
    count_ok: bool
    notes: list[str] = field(default_factory=list)
    uncaptured_ids: list[str] = field(default_factory=list)
    truncated_recovered: int = 0
    shard: str = ""
    transcript_fallback_required: bool = False


def fingerprint(text: str) -> str:
    return hashlib.sha256((text or "").encode("utf-8")).hexdigest()


def redact(text: str) -> str:
    """Strip credential-shaped spans; keep surrounding prose."""
    return _SECRETISH.sub("[REDACTED]", text or "")


def normalize_text(text: str) -> str:
    """Collapse whitespace for stable dedupe / prefix matching."""
    return re.sub(r"\s+", " ", (text or "").strip())


def normalize_id(fact_id: str) -> str:
    """Host ids may arrive truncated or with ellipsis; compare on the stable prefix."""
    value = (fact_id or "").strip()
    for marker in _TRUNCATION_MARKERS:
        if marker in value:
            value = value.split(marker, 1)[0]
    return value.strip().rstrip("-_")


def looks_truncated(text: str, *, min_chars: int = 80) -> bool:
    clean = (text or "").rstrip()
    if not clean:
        return False
    if any(clean.endswith(m) for m in _TRUNCATION_MARKERS):
        return True
    # Hard cut with no terminal punctuation after a long span.
    if len(clean) >= min_chars and clean[-1] not in ".!?…\"'”’)":
        return True
    return False


def is_prefix_of(partial: str, full: str) -> bool:
    a = normalize_text(partial).rstrip(".…")
    b = normalize_text(full)
    return bool(a) and b.startswith(a) and len(b) > len(a)


def fact_from_hit(hit: Any, *, default_bot: str = "") -> MemoryFact:
    """Accept a plain string or a RecallMemory-shaped dict."""
    if isinstance(hit, MemoryFact):
        return hit
    if isinstance(hit, str):
        raw = hit.strip()
        if raw.startswith("{") and raw.endswith("}"):
            try:
                parsed = json.loads(raw)
            except json.JSONDecodeError:
                parsed = None
            if isinstance(parsed, dict):
                return fact_from_hit(parsed, default_bot=default_bot)
        text = redact(raw).strip()
        return MemoryFact(
            text=text,
            bot=default_bot,
            truncated=looks_truncated(text),
        )
    if not isinstance(hit, dict):
        text = redact(str(hit)).strip()
        return MemoryFact(text=text, bot=default_bot, truncated=looks_truncated(text))
    text = redact(str(hit.get("text") or hit.get("fact") or hit.get("content") or "")).strip()
    fact_id = str(hit.get("id") or hit.get("fact_id") or hit.get("memory_id") or "")
    # Explicit empty bot/shard/owner means shared — do NOT inherit default_bot.
    if "bot" in hit or "shard" in hit or "owner" in hit:
        bot = str(
            hit["bot"] if "bot" in hit else hit["shard"] if "shard" in hit else hit["owner"]
            or ""
        )
    else:
        bot = default_bot
    truncated = bool(hit.get("truncated")) or looks_truncated(text)
    return MemoryFact(
        text=text,
        date=str(hit.get("date") or hit.get("created") or hit.get("ts") or ""),
        type=str(hit.get("type") or hit.get("kind") or hit.get("category") or ""),
        fact_id=fact_id,
        bot=bot,
        truncated=truncated,
        source=str(hit.get("source") or "recall"),
    )


def own_shard_only(facts: Iterable[MemoryFact], bot: str) -> list[MemoryFact]:
    """Keep only facts owned by ``bot``. Shared (empty bot) and other shards drop."""
    want = (bot or "").strip().casefold()
    if not want:
        raise ValueError("bot/shard id is required for own-shard filtering")
    return [f for f in facts if (f.bot or "").strip().casefold() == want]


def shard_expected_total(
    host_total: int | None = None,
    *,
    shard_total: int | None = None,
    shared_total: int | None = None,
    other_bots_total: int | None = None,
    require_shard: bool = False,
) -> int | None:
    """Expected dump size.

    When ``require_shard`` (bot filter on): prefer ``shard_total``; else derive
    from host_total minus shared/other. Never treat the cross-bot host pool as
    the expected count for a bot dump (Carl 82/209, Cortex 123/126).

    When ``require_shard`` is false (legacy / team-wide dump): ``host_total``
    alone is accepted as the expected count.
    """
    if shard_total is not None:
        return int(shard_total)
    if host_total is None:
        return None
    if not require_shard and shared_total is None and other_bots_total is None:
        return int(host_total)
    subtract = 0
    if shared_total is not None:
        subtract += int(shared_total)
    if other_bots_total is not None:
        subtract += int(other_bots_total)
    if require_shard and shared_total is None and other_bots_total is None:
        return None
    return max(0, int(host_total) - subtract)


TranscriptLookup = Callable[[MemoryFact], str | None]



def _collect_raw_facts(
    batches: Sequence[QueryBatch],
    structured_hits: Iterable[Any],
    *,
    bot: str,
) -> tuple[list[MemoryFact], bool]:
    """Flatten batch + structured hits; report whether any query hit the top-N cap."""
    raw: list[MemoryFact] = []
    cap_incomplete = False
    for batch in batches:
        if batch.hit_cap:
            cap_incomplete = True
        for hit in batch.hits:
            raw.append(fact_from_hit(hit, default_bot=bot))
    for hit in structured_hits:
        raw.append(fact_from_hit(hit, default_bot=bot))
    return raw, cap_incomplete


def _recover_truncated(
    fact: MemoryFact,
    transcript_lookup: TranscriptLookup | None,
) -> tuple[MemoryFact | None, bool, str | None]:
    """Return (recovered_or_none, recovered_flag, uncaptured_id)."""
    if not fact.truncated:
        return fact, False, None
    recovered = transcript_lookup(fact) if transcript_lookup is not None else None
    if recovered and (
        is_prefix_of(fact.text, recovered)
        or normalize_text(recovered) == normalize_text(fact.text)
    ):
        return (
            MemoryFact(
                text=redact(recovered).strip(),
                date=fact.date,
                type=fact.type,
                fact_id=fact.fact_id,
                bot=fact.bot,
                truncated=False,
                source="transcript",
            ),
            True,
            None,
        )
    return None, False, fact.fact_id or fingerprint(fact.text)[:12]


def _merge_fact_list(
    facts: list[MemoryFact],
    transcript_extras: Iterable[str],
    *,
    bot: str,
    transcript_lookup: TranscriptLookup | None,
) -> tuple[list[str], dict[str, MemoryFact], int, list[str], list[str], bool]:
    """Dedupe facts; apply transcript recovery. Returns texts, by_id, recovered, uncaptured, notes, need_tx."""
    ordered: list[str] = []
    seen: set[str] = set()
    by_id: dict[str, MemoryFact] = {}
    truncated_recovered = 0
    uncaptured: list[str] = []
    notes: list[str] = []
    need_transcript = False

    def ingest(fact: MemoryFact) -> None:
        nonlocal truncated_recovered, need_transcript
        if not fact.text and not fact.truncated:
            return
        working = fact
        if working.truncated:
            need_transcript = True
            recovered, ok, uid = _recover_truncated(working, transcript_lookup)
            if ok and recovered is not None:
                working = recovered
                truncated_recovered += 1
            else:
                uncaptured.append(uid or fingerprint(working.text)[:12])
                notes.append(
                    f"truncated fact not recovered via ReadTranscript: {uid}"
                )
                return
        if not working.text:
            return
        key = working.dedupe_key()
        if key in seen:
            return
        seen.add(key)
        ordered.append(working.text)
        nid = normalize_id(working.fact_id)
        if nid:
            by_id[nid] = working

    for fact in facts:
        ingest(fact)
    for extra in transcript_extras:
        ingest(fact_from_hit(extra, default_bot=bot))
    return ordered, by_id, truncated_recovered, uncaptured, notes, need_transcript


def _apply_known_ids(
    known_shard_ids: Iterable[str],
    *,
    bot: str,
    by_id: dict[str, MemoryFact],
    ordered: list[str],
    transcript_lookup: TranscriptLookup | None,
    uncaptured: list[str],
    notes: list[str],
) -> bool:
    """Fill missing known ids via transcript; extend uncaptured/notes. Returns need_transcript."""
    need_transcript = False
    seen_keys = {fingerprint(normalize_text(t) + "||") for t in ordered}  # unused; re-ingest via merge

    def already(nid: str) -> bool:
        if nid in by_id:
            return True
        return any(fid.startswith(nid) or nid.startswith(fid) for fid in by_id)

    for raw_id in known_shard_ids:
        nid = normalize_id(raw_id)
        if not nid or already(nid):
            continue
        need_transcript = True
        if transcript_lookup is not None:
            probe = MemoryFact(text="", fact_id=raw_id, bot=bot, truncated=True)
            recovered = transcript_lookup(probe)
            if recovered:
                fact = MemoryFact(
                    text=redact(recovered).strip(),
                    fact_id=raw_id,
                    bot=bot,
                    truncated=False,
                    source="transcript",
                )
                if fact.text:
                    key = fact.dedupe_key()
                    # append if new
                    if fact.text not in ordered:
                        ordered.append(fact.text)
                    by_id[normalize_id(raw_id) or nid] = fact
                    if already(nid):
                        continue
        uncaptured.append(raw_id)
        notes.append(f"uncaptured shard id after transcript fallback: {raw_id}")
    return need_transcript


def merge_batches(
    batches: Sequence[QueryBatch],
    *,
    transcript_extras: Iterable[str] = (),
    host_total: int | None = None,
    bot: str = "",
    shard_total: int | None = None,
    shared_total: int | None = None,
    other_bots_total: int | None = None,
    structured_hits: Iterable[Any] = (),
    transcript_lookup: TranscriptLookup | None = None,
    known_shard_ids: Iterable[str] = (),
) -> tuple[list[str], DumpReport]:
    """Dedup + shard count-check. Returns (unique_texts, report).

    When ``bot`` is set, only that shard's facts count toward exported_unique
    and expected total. Truncated facts MUST go through ``transcript_lookup``
    (ReadTranscript); missing recovery → uncaptured_ids and count_ok=false.
    """
    raw_facts, cap_incomplete = _collect_raw_facts(
        batches, structured_hits, bot=bot
    )
    notes: list[str] = []
    if bot:
        facts = own_shard_only(raw_facts, bot)
        dropped = len(raw_facts) - len(facts)
        if dropped:
            notes.append(
                f"excluded {dropped} non-shard hit(s) (shared/other bots); "
                "expected total is own shard only"
            )
    else:
        facts = list(raw_facts)

    ordered, by_id, truncated_recovered, uncaptured, merge_notes, need_transcript = (
        _merge_fact_list(
            facts,
            transcript_extras,
            bot=bot,
            transcript_lookup=transcript_lookup,
        )
    )
    notes.extend(merge_notes)
    if _apply_known_ids(
        known_shard_ids,
        bot=bot,
        by_id=by_id,
        ordered=ordered,
        transcript_lookup=transcript_lookup,
        uncaptured=uncaptured,
        notes=notes,
    ):
        need_transcript = True

    expected = shard_expected_total(
        host_total,
        shard_total=shard_total,
        shared_total=shared_total,
        other_bots_total=other_bots_total,
        require_shard=bool(bot),
    )
    if bot and expected is None and host_total is not None and shard_total is None:
        notes.append(
            "host_total spans shared/other bots; pass shard_total "
            "(own shard only) — count_ok stays false until then"
        )

    exported = len(ordered)
    if uncaptured:
        need_transcript = True
        notes.append(TRANSCRIPT_FALLBACK_REQUIRED)

    if expected is None:
        count_ok = False
        if bot:
            notes.append(
                "shard total unknown; count_ok requires exported==shard_total "
                "(own shard only; pass shard_total)"
            )
        else:
            notes.append(
                "host total unknown; count_ok requires exported==total "
                "(pass host_total from the memory host)"
            )
        if cap_incomplete:
            notes.append(
                "at least one query hit top-N cap — "
                "re-run with more seeds or ReadTranscript fallback"
            )
    else:
        count_ok = (
            exported == expected
            and not uncaptured
            and not (cap_incomplete and exported < expected)
        )
        if exported != expected:
            notes.append(f"count mismatch: exported={exported} shard_total={expected}")
        if cap_incomplete and exported < expected:
            notes.append("cap incomplete: need more queries or transcript fallback")
        if uncaptured:
            count_ok = False

    report = DumpReport(
        exported_unique=exported,
        total=expected,
        cap_incomplete=cap_incomplete,
        queries=len(batches),
        count_ok=count_ok,
        notes=notes,
        uncaptured_ids=list(dict.fromkeys(uncaptured)),
        truncated_recovered=truncated_recovered,
        shard=bot,
        transcript_fallback_required=need_transcript,
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
        "uncaptured_ids": list(report.uncaptured_ids),
        "truncated_recovered": report.truncated_recovered,
        "shard": report.shard,
        "transcript_fallback_required": report.transcript_fallback_required,
    }


def load_batches_jsonl(path: Path) -> list[QueryBatch]:
    """Load RecallMemory JSONL batches: one JSON object per line.

    Accepted shapes per line:
    - ``{"query": "...", "hits": ["...", ...], "top_n": 50}``
    - ``{"query": "...", "hits": [{"text": "...", "bot": "...", ...}, ...]}``
    - a bare list of hit strings/dicts (query defaults to ``batch-<n>``)
    """
    batches: list[QueryBatch] = []
    for index, line in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
        raw = line.strip()
        if not raw or raw.startswith("#"):
            continue
        payload = json.loads(raw)
        if isinstance(payload, list):
            batches.append(
                QueryBatch(query=f"batch-{index}", hits=list(payload), top_n=DEFAULT_TOP_N)
            )
            continue
        if not isinstance(payload, dict):
            raise ValueError(f"batches.jsonl line {index}: expected object or array")
        hits = payload.get("hits") or payload.get("results") or payload.get("memories") or []
        if not isinstance(hits, list):
            raise ValueError(f"batches.jsonl line {index}: hits must be a list")
        # Keep structured dicts as-is; merge_batches accepts them via fact_from_hit
        # when passed through structured_hits. For QueryBatch.hits we store a
        # JSON-serialized form only when needed — prefer structured path.
        top_n = int(payload.get("top_n") or payload.get("topN") or DEFAULT_TOP_N)
        query = str(payload.get("query") or payload.get("q") or f"batch-{index}")
        # Store hits as strings OR leave markers; merge will also get structured.
        # Dict hits travel via structured_hits only (preserve bot/date/type/id).
        string_hits = [hit for hit in hits if isinstance(hit, str)]
        batches.append(QueryBatch(query=query, hits=string_hits, top_n=top_n))
    return batches


def _structured_from_batches_jsonl(path: Path) -> list[Any]:
    """Re-read JSONL and collect dict hits so bot/date/type/id survive."""
    structured: list[Any] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        raw = line.strip()
        if not raw or raw.startswith("#"):
            continue
        payload = json.loads(raw)
        hits = payload if isinstance(payload, list) else (
            (payload.get("hits") or payload.get("results") or payload.get("memories") or [])
            if isinstance(payload, dict)
            else []
        )
        for hit in hits:
            if isinstance(hit, dict):
                structured.append(hit)
    return structured


def write_dump_markdown(texts: Sequence[str], out: Path, *, shard: str = "") -> None:
    header = "# Memory full dump"
    if shard:
        header += f" — shard `{shard}`"
    body = "\n\n".join(f"- {t}" for t in texts)
    out.write_text(header + "\n\n" + body + ("\n" if body else ""), encoding="utf-8")


def run_cli(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="memory_full_dump.py",
        description=(
            "Merge RecallMemory JSONL batches into one dump + receipt. "
            "Expected total is the bot's own shard only."
        ),
    )
    parser.add_argument(
        "--batches",
        type=Path,
        required=True,
        help="JSONL file of RecallMemory query batches",
    )
    parser.add_argument(
        "--expected",
        type=int,
        required=True,
        help="Own-shard expected total (NOT the cross-bot host pool)",
    )
    parser.add_argument("--out", type=Path, required=True, help="Markdown dump path")
    parser.add_argument("--receipt", type=Path, required=True, help="JSON receipt path")
    parser.add_argument("--bot", default="", help="Shard / bot id (own-shard filter)")
    parser.add_argument(
        "--known-ids",
        type=Path,
        help="Optional text file of known shard fact ids (one per line)",
    )
    args = parser.parse_args(list(argv) if argv is not None else None)

    batches = load_batches_jsonl(args.batches)
    structured = _structured_from_batches_jsonl(args.batches)
    known_ids: list[str] = []
    if args.known_ids is not None:
        known_ids = [
            line.strip()
            for line in args.known_ids.read_text(encoding="utf-8").splitlines()
            if line.strip() and not line.strip().startswith("#")
        ]

    # CLI cannot call ReadTranscript itself; flag required + list uncaptured.
    def _no_live_transcript(_fact: MemoryFact) -> str | None:
        return None

    texts, report = merge_batches(
        batches,
        bot=args.bot,
        shard_total=args.expected,
        structured_hits=structured,
        known_shard_ids=known_ids,
        transcript_lookup=_no_live_transcript if known_ids else None,
    )
    # When truncated facts exist without recovery, notes already require transcript.
    write_dump_markdown(texts, args.out, shard=args.bot)
    args.receipt.write_text(
        json.dumps(report_to_dict(report), indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    return 0 if report.count_ok else 2


def main(argv: Sequence[str] | None = None) -> None:
    raise SystemExit(run_cli(argv))


if __name__ == "__main__":
    main()
