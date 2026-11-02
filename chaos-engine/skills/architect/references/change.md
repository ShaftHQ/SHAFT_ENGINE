# Change safely

## 16 Test strategy

- Mix by risk, not by fixed ratio: many fast unit tests on logic, contract
  tests at service/API boundaries, few end-to-end on critical journeys.
- Test behaviour via public interfaces, not internals; one reason to fail.
- Framework-specific test authoring: route to the project test skill pack (identity.md), not here.

## 17 Refactoring and legacy

- Refactor in its own commit/step, before or after the behaviour change,
  only in code you touch. Behaviour stays identical; tests stay green.
- No tests? Write characterization tests pinning current output first.
- Find a seam (parameter, interface, wrapper) to inject tests or new code.
- Large replacement: strangler fig. Route a slice to new code, verify,
  repeat; delete old path at the end.
- High-complexity hot spot: [complexity gate](../../../references/complexity-gate.md).

## 18 Debugging

- No fix without root cause. Reproduce, read the full error, check recent
  changes, compare with a working case.
- One hypothesis at a time; smallest test that confirms or kills it.
- Fix the cause, add the regression test that failed before.
- 3 failed fixes: stop; question the design and write what you learned.

## 19 Review

Order: design fit, correctness and edge cases, tests prove the change,
complexity, naming, docs. Severity: blocker / should-fix / nit. Flag only
gaps against requirements or correctness; nits never block. Self-review the
diff before asking. Rounds follow core
[check and deliver](../../chaos-engine/SKILL.md#check-and-deliver).

## 20 Tech debt

- Name the type: deliberate or inadvertent, prudent or reckless (Fowler).
- Price it: interest (what it slows or risks) vs cost to fix.
- In scope and cheap: fix now. Otherwise file an issue with interest, cost,
  trigger to pay. Never leave it only in a comment or chat.
