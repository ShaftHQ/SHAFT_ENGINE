# Mobile-retro harness lessons

Retro gaps from a mobile-automation run. Enforce via
[`mobile_retro.py`](../mobile_retro.py), the tracked
`scripts/ci/no_push_guard.py`, and the live CLI
`scripts/ci/mobile_retro_live.py`. On-demand only (see harness-map); not
always-loaded.

1. **Freeze green results.** After a green run, copy the results directory to a
   freeze path (`post_run_freeze` / `post-run-freeze`). A later red run must not
   overwrite that snapshot.
2. **Backend gate → Blocked.** When the signature is server-side (API ignores
   app version code, feature flag, and similar), classify **Blocked** early so
   the rebuild loop stops (`rebuild_loop_classify` / `rebuild-classify`).
3. **Time-cap the primary path.** When an authorized fallback exists, cap the
   failing primary (for example a cloud-device class-not-found), **kill** a hung
   primary, and switch to the fallback (`device_fallback_run` /
   `device-fallback`). POSIX uses fork; Windows uses spawn (picklable
   callables).
4. **Tracked NO_PUSH guard.** The WIP / push-once guard must be a tracked file
   (`scripts/ci/no_push_guard.py`). Pre-push fails if it is missing from
   `git ls-files` on a checkout that ships this helper.
5. **Pin evidence to build + branch.** Persist build id and branch with green
   evidence (`persist_evidence_pin`); reject evidence from a different build or
   branch (`enforce_evidence_pin`).
6. **NamedBlock by gap id.** Two scenarios that map to one gap id produce one
   NamedBlock (`live_report_named_blocks` / `report-blocks`), not two.

Library helpers cover freeze / classify / pin / NamedBlock / time-cap. Wired
live paths: tracked NO_PUSH on overlay pre-push, plus the five executor entry
points above via `mobile_retro_live.py`. Do not weaken session-cap or
delete-guard rules to clear these.
