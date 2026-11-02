---
name: mega-pr-throughput-20261102c
description: Use when PR last-commit→merge median breaches 15 minutes, Codacy review threads block enqueue after green CI, or Java assertTrue(length()>0) findings appear only post-push.
---

# Mega-PR throughput — merge latency (#7736, 2026-11-02c)

Companion to [mega-pr-throughput-20261102.md](mega-pr-throughput-20261102.md)
and [mega-pr-throughput-20261102b.md](mega-pr-throughput-20261102b.md).

## Symptom

Weekly `pr-time-to-merge` reported median last-commit→merge **16.9 min**
(target 15) with **2.25** average PR Gate runs per non-bot PR (#7736).

## Root causes addressed here

1. **Post-green enqueue blocks** on unresolved code-quality review threads
   (seen on #7679 / #7731). Fix: resolve or reply+resolve before
   `enqueuePullRequest`; gate with `scripts/ci/unresolved_review_threads.py`.
2. **Re-push loops** from Java `Assert.assertTrue(x.length() > 0)` findings
   that only appear after Codacy runs. Fix: extend
   `scripts/ci/code_quality_preflight.py` with `java-length-gt-zero`.

## Rules

1. Run unresolved-thread check before merge-queue enqueue.
2. Keep first-push-green: preflight must catch the Java length/size pattern.
3. Do not raise the 15-minute target or delete the weekly alert.

## Verify

- [ ] `unresolved_review_threads` fixture test red on open / green on resolved.
- [ ] Preflight flags `assertTrue(s.length() > 0)` and accepts `assertFalse(s.isEmpty())`.
- [ ] ci-cd skill names both tools.
