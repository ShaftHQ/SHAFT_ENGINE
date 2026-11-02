# Mobile-retro harness lessons

Retro gaps from a mobile-automation run. Enforce via
[`mobile_retro.py`](../mobile_retro.py), tracked `scripts/ci/no_push_guard.py`,
and live runtime `scripts/ci/mobile_retro_runtime.py` (repo scripts; not shipped in the portable core).
On-demand only (see harness-map); not always-loaded.

1. **Freeze green results.** After a green run, copy the results directory to a
   freeze path. Live: `assemble_shard_blob` post-run, `hooks/guard`
   `_mobile_retro_post_run` on PostToolUse after tests, `rerun-failures.sh` and
   flutter e2e when green.
2. **Backend gate → Blocked.** Classify **Blocked** early so the rebuild loop
   stops. Live: `rerun-failures.sh` calls `classify-before-rebuild` before
   attempt > 1.
3. **Time-cap the primary path.** Cap a failing primary, kill a hung primary,
   switch to fallback. Live: flutter e2e device-fallback; workers are
   module-level picklable (POSIX fork / Windows spawn).
4. **Tracked NO_PUSH guard.** Pre-push fails if `scripts/ci/no_push_guard.py`
   is missing from `git ls-files` on a checkout that ships this helper.
5. **Pin evidence to build + branch.** Persist on green freeze; fail-closed
   reject on mismatch (`require_evidence_pin` /
   `overlay_pre_push.evidence_pin_failures` non-zero / non-empty).
6. **NamedBlock by gap id.** Live reporter: `extract_allure_failures` →
   `report_named_blocks_from_failures`.

Do not weaken session-cap or delete-guard rules to clear these.
