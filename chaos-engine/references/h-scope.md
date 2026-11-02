# Scope discipline (H-SCOPE)

## Rule

**H-SCOPE** covers Carl's scope-discipline brief — **not** PR diff vs `## Scope`
(that is [H-PR-SCOPE](pr-scope-check.md)).

1. **Spoken-lock titles keep scope words (L41).** Ear-locked titles must retain
   the speaker's scope noun phrase. Do not collapse "penetration testing" →
   "testing". Gate: `title_scope_gate.py` + design-skills lesson L41 `spoken-lock-scope-words` (load from the design-skills add-on index).
2. **One task of a kind.** At most one In progress item per bot/stream —
   [sequential-wip.md](sequential-wip.md).
3. **Exclusive file/path claims.** Two active workers must not share a writable
   path glob — [parallel-streams.md](parallel-streams.md) ownership +
   `scripts/ci/parallel_stream_conflict.py` (`refuse_dispatch_on_ownership_overlap`).

## Why

Near-title similarity alone let a retitle drop the scope noun (Q25 / penetration
testing class). Parallel WIP without path claims multiplies conflicts and missed
retros. Naming the PR-diff gate "H-SCOPE" hid these checks from workers looking
up Carl's brief.

## Check

- [ ] Spoken-lock: `title_scope_gate.py check --title T --srt answer.srt` PASS
- [ ] Board shows ≤1 In progress per bot ([sequential-wip](sequential-wip.md))
- [ ] Before dispatch: `parallel_stream_conflict.py` reports no writable overlap
- [ ] Reviewer brief lists L41 / title_scope_gate under spoken-lock (not under H-PR-SCOPE)

## See also

- [pr-scope-check.md](pr-scope-check.md) (H-PR-SCOPE)
- design L41 in the video checklist spoken-lock bullet
