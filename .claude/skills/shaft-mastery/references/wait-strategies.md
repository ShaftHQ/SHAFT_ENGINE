# Flaky-Test Stabilization & Wait Strategies

## Doctrine
A flaky test is a bug with a probability attached — stabilize by removing
the race, never by widening a sleep or adding blind retries. Fixed sleeps
are always wrong: too short on slow CI, wasted time everywhere else.
Condition-polling (SHAFT SmartWait / explicit waits) with a budget is the
only correct wait primitive; wait on the ACTUAL readiness condition
(element interactable, request completed, `sys.boot_completed`), not a
proxy (time, spinner gone).

## Root-cause taxonomy (check in this order)
1. **Initialization-order races** — injection vs subframe load, listener vs
   first event. CI is slower; order flips there first.
2. **Shared mutable state** — static fields, shared sessionStorage across
   same-origin frames, shared temp dirs/ports across parallel tests.
3. **External world drift** — target site changed, emulated-device list
   changed (Chrome 143 removed "Pixel 5"), provider incident. Check pass
   history + live target before blaming code.
4. **Environment divergence** — headless vs headed, display-less Linux,
   Windows non-atomic file replace under AV contention, forked JVM missing
   a property.
5. **Genuine timing assumptions** — animations, debounce, eventual
   consistency; make the code observable instead of guessing.

## Make it deterministic, then fix it
The flake must fail on demand before you trust any fix. Techniques that
worked in SHAFT:
- Delay a fixture resource to force the slow ordering
  (`?frameDelay=900` iframe trick, PR #3432).
- Pin the fork/parallelism config down to one method
  (`-Dtest=Class#method`) to isolate shared-state suspects.
- Run the suspicious pair/order explicitly; TestNG order is stable given
  the same XML — reproduce the exact order CI used.
- Add temporary observation points at the boundary you suspect, not
  shotgun logging everywhere.

## Retries
Retry analyzers are quarantine, not cure: acceptable to keep a release
train moving while the root cause is tracked in an issue, never as the
closing state. Allure shows retries per historyId — a rising retry count
is a regression signal even while the suite stays "green".
