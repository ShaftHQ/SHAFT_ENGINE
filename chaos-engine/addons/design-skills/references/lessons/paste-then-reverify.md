---
name: paste-then-reverify
description: Use after every GUI paste into a hosted CMS Code block; paste from the build artifact then immediately run the livecms matrix.
---

# L31 Paste from artifact, then re-verify

## Use when

Automating or hand-pasting into a hosted builder Code block. Reinforces
[Squarespace 7.1](../site-builders/squarespace-7-1.md) paste-source rule
and [L28](live-verify-cms-embed.md).

## Rules

1. **Build artifact only.** Paste the full build-output file (select all /
   paste). Never paste from rendered DOM.
2. **Re-verify immediately.** After Save, run [L28](live-verify-cms-embed.md)
   livecms on the public URL.
3. **No nested containers.** Nested `.sqs-code-container` (or builder
   equivalent) count must stay **0**.
4. **Toast is untrusted.** Editor success toasts are not evidence of Done.

## Verify

- [ ] Post-paste `design_qc.py livecms` green.
- [ ] Nested code-container count is 0 on the public page/source check.

## Why

GUI paste slips (partial replace, nested wrappers, wrong block) are
common. Only artifact paste plus livecms catches them.

## Sources

Field lesson from a hosted-CMS embed redesign: republish waves needed
artifact paste plus immediate live re-verify to stop nested wrappers.
