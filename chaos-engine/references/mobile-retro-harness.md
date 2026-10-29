# Mobile-retro harness lessons

Retro gaps from a mobile-automation run. Enforce via
[`mobile_retro.py`](../mobile_retro.py) and the tracked
`scripts/ci/no_push_guard.py`. On-demand only (see harness-map); not
always-loaded.

1. **Freeze green results.** After a green run, copy the results directory to a
   freeze path. A later red run must not overwrite that snapshot.
2. **Backend gate → Blocked.** When the signature is server-side (API ignores
   app version code, feature flag, and similar), classify **Blocked** early so
   the loop stops rebuilding the app.
3. **Time-cap the primary path.** When an authorized fallback exists, cap the
   failing primary (for example a cloud-device class-not-found), **kill** a hung
   primary, and switch to the fallback.
4. **Tracked NO_PUSH guard.** The WIP / push-once guard must be a tracked file
   (`scripts/ci/no_push_guard.py`). Pre-push fails if it is missing from
   `git ls-files` on a checkout that ships this helper.
5. **Pin evidence to build + branch.** Record build id and branch with green
   evidence; reject evidence from a different build or branch.
6. **NamedBlock by gap id.** Two scenarios that map to one gap id produce one
   NamedBlock, not two.

Library helpers cover freeze / classify / pin / NamedBlock / time-cap. The
wired gate in this PR is the tracked NO_PUSH check on overlay pre-push. Do not
weaken session-cap or delete-guard rules to clear these.
