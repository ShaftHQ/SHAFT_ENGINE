---
name: spoken-lock-scope-words
description: Use when an ear-locked title must keep the speaker's scope noun phrase, not a shorter hypernym.
---

# L41 Spoken-lock titles keep scope words

## Use when

Titling a cut that is ear-locked to a spoken answer frame (Q&A, long-form chapter).

## Rules

1. Keep the **scope noun phrase** the speaker used (or clearly meant). Do not collapse to a shorter hypernym.
2. Examples of keep → do-not-collapse: penetration testing → testing; test design → testing; agentic testing → testing; software engineering → engineering.
3. Near-title similarity gates are **not** enough; scope is a separate check.
4. Gate: [`title_scope_gate.py`](../../scripts/title_scope_gate.py) `check --title T --srt answer.srt` — every scope noun from the title must appear in the answer SRT topic text.

## Verify

- [ ] Title quotes the scope phrase next to the transcript/ASR proof (or ear-lock note).
- [ ] `title_scope_gate` PASS on final title + answer SRT.
- [ ] Indep review re-checks scope, not only similarity.

## Sources

- Indexed via [`index-extension.md`](index-extension.md) (avoids primary-index / craft_qc collision with the sibling video sign-off lesson).

- Field reject where a retitle dropped the scope noun while similarity still passed.
