# Locator Design & Self-Healing

## Locator ladder (stop at the first stable rung)
test id > role + accessible name > stable attributes in a stable container >
text (assertions only; i18n-brittle) > structural CSS/XPath (last resort;
never index- or style-class-based). SHAFT's `By`-based APIs and
aria-snapshot assertions (`matchesAriaSnapshot`, partial-subset semantics)
reward the role+name rung heavily.

## Recorder truth rules (SHAFT-specific, hard-won)
- Recorded locators must map 1:1 to real user actions; suppress
  browser-synthesized phantom steps (subframe storage restores, PR #3432;
  click-tracking state, PR #3427 — never reintroduce).
- Codegen validates by compile-and-replay (`GeneratedTestValidator`) — a
  generated locator is unproven until replay passes headlessly.

## Self-healing (shaft-heal / doctor)
- Healing proposes a NEW locator from a failure snapshot
  (`doctor_propose_healed_locator`); score candidates by multi-signal
  similarity (attributes, text, neighbors, position) and NEVER auto-accept
  silently — healed locators are review-gated changes, else suites drift to
  asserting the wrong elements.
- Confidence scoring must penalize uncited/unevidenced output symmetrically
  — SHAFT's `DoctorConfidenceScorer` once scored zero-citation recommended
  actions 85/100 because only hypotheses had a penalty branch (PR #3344).
  When extending scorers, check every factor has both directions.
- Persisting healing history: append/merge, never rewrite the store (a
  data-wiping rewrite bug shipped once, issue #3315).
- `healer_run_failed_test` can burn multiple real Maven builds — precheck
  that Allure output exists before invoking heavy loops.

## Debugging a "broken" locator
Order: is the element in a different frame/context → is there a wait/race →
did the DOM actually change (diff live DOM vs expectation) → only then
rewrite the locator. Most "locator rot" reports are frame or timing bugs.
