---
name: mega-pr-throughput-20261102
description: Use when a mega PR's local gates or full unittest discover burn wall clock, when workers get interrupted mid-suite, or before the first push of a large branch.
---

# Mega-PR throughput (2026-11-02)

## Use when

Opening or babysitting a mega PR (many closes, overlay churn, full
`tests/scripts` discover), or when a long-running worker on this box stops
without a clear error around the 35–50 minute mark.

Note: `chaos-engine/references/lessons/` (requested) is rejected by portable
forbidden-token / machine-path / lean-lint / harness-reachability gates for
new reference cards. This lesson ships beside the optimized CI scripts.

## Root causes measured on PR #7731 (Africa/Cairo)

| Gate / symptom | Before | Dominant cost | After (this PR) |
| --- | --- | ---: | --- |
| `validate_pr_closing_keywords.py --body` (39 Closes) | ~67 s | 39 serial `gh issue view` (~1.5 s each) | ~1.6 s GraphQL batch; skip network when no closing keyword; REST fallback parallel + 10 s timeout |
| `overlay_pre_push.py` | ~70–80 s | SkillSpector ~64 s serial; then cheap_ci ~3 s; repeated `gh pr view` | Collectors parallel; SkillSpector fingerprint cache (~5.5 s repeat); one cached `gh pr view` (20 s timeout); `--all-failures` prints every failure + timings |
| `code_quality_preflight.py` | ~0.3 s (41 files) | already cheap; serial per-file | per-file thread pool when ≥4 files |
| `protected-installer-acceptance` Download source | 80–139 s × many bootstrap installs (CI); 37.7 s local | test opener re-read ~666 `chaos-engine/` files on **every** blob URL (O(n²) disk) | cache `source_payload`/`pack_payload`/tree once per opener; local install_latest 37.7 s → 0.4 s; full bootstrap module 58 tests / 12.7 s |
| Full `unittest discover -s tests/scripts` | ~38–40 min | >35 min session cap; often two discovers at once | single-instance `full_scripts_unittest_lock`; checkpoint/resume before 35 min |

### Worker interruptions (11:2x–12:57 Cairo window and earlier today)

**Proven**

1. **Session cap vs suite length.** bot-entry Session cap enforces wrap-up at
   35 minutes (box hard-stop ~50). Full discovers completed in 38.3–40.5 min
   (mega-C stream-c-ut-final 09:32→10:10, mega-A unittest-full-final
   09:44→10:24, mega-C stream-c-ut-final2 10:15→10:53, mega-cleanup
   final-ut.log 10:22→11:02 Africa/Cairo). A worker that stays in one turn
   for the whole suite is interrupted by the cap, not by the tests finishing.
2. **Parallel full discovers overlapped.** mega-A and mega-C overlapped ~26 min;
   mega-C final2 and mega-cleanup final-ut overlapped ~31 min. Terminal 505615
   even waited on peers "so we are not OOM-killed".
3. **Memory pressure without a logged OOM kill.** At investigation time
   (13:47 Africa/Cairo): Mem 15 Gi, Swap 8 Gi with only ~52 Mi SwapFree
   (~3.0 Gi swap used). `dmesg`, `journalctl -k`, and `journalctl --user` for
   2026-11-02 showed **no** OOM killer / "Killed process" lines; `last -x`
   showed **no** reboot. So kernel OOM as the interrupt cause is **unproven**.

**Unproven:** host/app restart or OOM-kill of the agent process (no kern/user
journal evidence on this box image).

## Rules

1. **Run the full overlay pre-push before the first push.** Path-gated CI
   contracts (harness-map, README inventory, code-quality, cheap AG tests)
   must not be discovered only after a red CI round.
2. **One pre-push, all failures.** Use
   `python3 scripts/ci/overlay_pre_push.py --all-failures` so every collector
   reports in one pass with timings — do not fix-and-rerun in waves.
3. **Commit small and often.** Worker interruptions (session cap) discard
   uncommitted work; WIP commits on the mega branch lose nothing.
4. **Run local gates in parallel** (default in overlay pre-push). Do **not**
   run two full `unittest discover -s tests/scripts` at once.
5. **Never run the full suite in the foreground.** Full
   `unittest discover -s tests/scripts` takes ~38–40 min; a worker session is
   capped at 35 min. Prefer targeted modules plus `harness_pr_gate`'s selection.
   If a full discover is required, start it detached (`nohup`/`setsid`, log to a
   file) and check the log on resume — never block a turn on the whole suite.
6. **Only sanctioned full-discover path.** Use
   `python3 scripts/ci/run_full_scripts_unittest.py` (optionally with extra
   unittest args). It is the only sanctioned way to run the full
   `tests/scripts` discover: it acquires `full_scripts_unittest_lock` so a
   second suite cannot overlap. Do not call `python3 -m unittest discover -s
   tests/scripts` directly. Before minute 35, `tool.py checkpoint … --next`
   and end the turn; resume with `tool.py resume`.

## Verify

- [ ] `validate_pr_closing_keywords.py --body` with many Closes finishes in a few seconds (GraphQL), and with no Closes does not call `gh`.
- [ ] `overlay_pre_push.py --all-failures` prints `timing …` lines and a single failure count.
- [ ] Second pre-push with unchanged skill dirs skips SkillSpector scan (cache hit).
- [ ] Second `full_scripts_unittest_lock(..., blocking=False)` raises `BlockingIOError` while the first is held.
- [ ] No second full discover starts while another holds the lock.
- [ ] Full discover goes only through `scripts/ci/run_full_scripts_unittest.py` (lock + documented entrypoint).
- [ ] SkillSpector pre-push cache fingerprint is content sha256 (not mtime).
- [ ] `validate_pr_closing_keywords` GraphQL passes owner/name as variables.

## Sources

- Box evidence 2026-11-02 Africa/Cairo: mega-cleanup final-ut / stream-c-ut-final*
  logs; terminals 505611, 505613, 505615, 505616, 505622, 505632;
  `free` /proc/swaps; empty OOM journals.
- Profiling on `ChaosEngine/mega-perf` against PR #7731 body (39 closing keywords).
