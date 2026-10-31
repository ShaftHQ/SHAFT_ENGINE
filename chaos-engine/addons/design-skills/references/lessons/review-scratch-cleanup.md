---
name: review-scratch-cleanup
description: >
  L78 REVIEW-SCRATCH-CLEANUP (CE 2026.10.31.8). See core references for the enforceable gate.
---
Reviewer copies via `git archive` to `/tmp` or detached at a pushed sha so the
delete guard allows removal without an owner code.
