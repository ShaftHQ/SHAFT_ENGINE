# Mobile retest-hygiene playbook

## A. Map / confirm tap succeeds but UI unchanged (FR-1)

**Symptom:** `touch().tap` / click logs success; screenshots stay on the same screen; Confirm / submit never appears; `enabled`/`visible` may still read true; `hittable` may be **absent** from page source.

**Triage order:**

1. Rule out bad locator (wrong id, wrong ScrollView, off-screen).
2. Capture page source + screenshots before/after tap; note whether `hittable` is present.
3. Try a settle wait / single-tap-then-wait (re-taps can cancel in-flight map navigation).
4. If the control still has no effect after proven taps, **classify as likely app defect** on the product tracker; keep a SHAFT observability footnote only (#7706).
5. Do not burn the full suite on every map-dependent case after dual evidence — park siblings and continue non-map coverage.

Closed #6908 covers explicit `hittable=false` only. This playbook is **consumer triage**, not the engine fix.

## B. Picker destination probe (FR-2)

When option rows expose **no human label text** (name/label/value and nested StaticText empty):

1. Do not invent Continue-button text heuristics alone.
2. Tap candidate rows and **probe the destination screen** to classify which row is which.
3. If every row leads to the same unexpected destination, file an **app defect** (missing option / wrong list).

On iOS, row identity for scroll detection must use `name`, not Android `content-desc` (XCUITest rejects `content-desc`).

## C. Mixed bytecode (FR-3)

If a Maven Surefire process **started before** a fix commit, its classpath still runs the old classes.

**Rule:** After any page-object / helper commit, stop or finish in-flight surefire processes, then start a **new** JVM for the retest. Record in the progress journal which SHA the retest ran against.

## D. Progress journal (FR-4)

Durable journal outside chat:

- Per-method PASS/FAIL/SKIP with timestamps (Africa/Cairo labeled).
- Engine / CLI / ChaosEngine versions.
- In-flight PIDs and which SHA they compile.
- Parked cases and why (e.g. shared map wall).

On resume: re-read the journal and live branch/PR state before redo.

## E. iOS SwiftUI swipe fallback lifecycle (FR-5)

`swipeElementIntoView` may not move a SwiftUI vertical list. Workaround: tallest `XCUIElementTypeScrollView` + `mobile: dragFromToForDuration`. Tracked/closed as #7680; fix in PR #7681. Keep the consumer drag fallback until that release is on the pom; then drop it.

## Valid examples

- When a retest map points at a stale journey id, rebuild the map from the live journey SoT before picking the next case, instead of hand-editing the picker.
- After a bytecode/journal mismatch on retest, clear the journal entry for that journey and re-record evidence instead of forcing the old pin.

## Boundary

- English-only; say "a client mobile project"; no secrets.
