---
name: fix-bug-class
description: Use when a reporter names one broken path; fix the owned-path class with transactional preflight and a full version-hop matrix test, not a one-file patch.
---

# Fix the bug class, not the reporter path

## Rule

1. When an upgrade/install/doctor failure names one file (VERSION, budget.json,
   a host toml), derive the **owned-path set** the installer already knows
   (`receipt_owned_paths`, host adapters, overlay).
2. Repair that class transactionally (preflight → write → VERSION last →
   post-verify doctor). Do not ship a third one-off path copy.
3. Add or extend a **version-hop matrix** test that walks every supported hop
   with linked worktrees and asserts `doctor_ok` after each hop — not only the
   reporter's single path.

## Why

The same upgrade bug was patched file-by-file three times (VERSION, budget,
codex toml) before the class fix landed. Matrix coverage catches the next
sibling path before a consumer does.

## Check

`tests/scripts/test_ce_bugfix_*` / `test_ce_retro_*` matrix hops include the
new VERSION and fail if any hop leaves `doctor_ok` false or half-applied hosts.

## Verdict

A bug-fixing delivery records the symptom, the suspected cause, the
remediation, the files, the tests, and the risks. The verification verdict is
`verified`, `partial`, or `failed`. Do not write `verified` unless the original
reproduction was exercised. `failed` returns to assessment with the new
evidence. Assess and verify stay in this reference. Fix the owned-path class
still applies.
