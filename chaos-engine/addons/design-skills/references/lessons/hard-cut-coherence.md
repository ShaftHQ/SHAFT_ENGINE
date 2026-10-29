---
name: hard-cut-coherence
description: Use when joining or trimming talk clips; each join must be one thought, prefer contiguous alternate over a cut that drops setup.
---

# L19 Hard-cut coherence

## Use when

Hard-cutting privacy/price spans, shortening a Q&A answer, or choosing
between mute, cut, and a shorter contiguous window.

## Rules

1. **Each join is one thought.** The clause before the cut and the clause
   after must form a coherent sentence the ear accepts. Mid-thought and
   dangling connectors fail.
2. **ASR across the join, eye-checked.** Read word timestamps across the
   cut; confirm the kept opener still has its setup noun/claim. Gate
   helpers (`antecedent`, title polarity) support — they do not replace
   the ear/eye check.
3. **Prefer contiguous alternate over a cut that drops setup.** If
   removing a muted middle also removes the premise the closer needs,
   publish a shorter contiguous window instead of a join that skips
   setup (example pattern: keep claim-only beat; HOLD the free-tier /
   price tail).
4. **Sentence boundaries from word timestamps.** Land `start`/`end` on
   proven word ends, not exclusive ends of multi-thought ASR events.

## Verify

- [ ] PLAN/BRIEF notes the contiguous alternate when a mute/cut was
      rejected for incoherence; word-ts proof attached for contested cuts.
- [ ] Reviewer ear-check across each hard join; antecedent / polarity
      gates green on the kept window.

## Why

A surgically muted middle can leave a closer that assumes a setup the
viewer never heard. Contiguous shorter windows beat clever joins.

## Sources

Field lesson from a content QA recut batch: hard mute/cut through a
setup beat was rejected as cut-incoherent; contiguous alternate won.
