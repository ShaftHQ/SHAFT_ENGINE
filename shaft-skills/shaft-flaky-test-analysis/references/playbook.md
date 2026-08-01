# Flaky-test-analysis playbook

## Ten practices

1. Confirm inconsistent outcomes for the same test identity, inputs, build, and intended environment before calling it flaky. [`ALLURE-STABILITY`]
2. Gather attempt-level history, status transitions, retry counts, durations, failures, traces, and environment metadata. [`ALLURE-STABILITY`, `SHAFT-REPORTING`]
3. Separate product nondeterminism, test nondeterminism, environment instability, and expected external variance. [`ALLURE-STABILITY`, `GOOGLE-SRE-TESTING`]
4. Repeat a bounded scope while controlling one suspected variable at a time and recording every attempt. [`GOOGLE-SRE-TESTING`]
5. Inspect synchronization, shared mutable state, order dependence, clocks, randomness, data collisions, networks, resources, browser or device state, and cleanup. [`SELENIUM-PRACTICES`, `GOOGLE-SRE-TESTING`]
6. Replace arbitrary sleeps and blanket retries with observable conditions, isolation, deterministic data, or dependency controls. [`SELENIUM-PRACTICES`, `ALLURE-STABILITY`]
7. Treat a retrying pass as instability evidence, not proof that the original failure disappeared. [`ALLURE-STABILITY`]
8. Quarantine only through an explicit policy with owner, reason, impact, expiry, and retained visibility. [`ALLURE-STABILITY`, `ISTQB-TM`]
9. Validate the repair with enough repeated clean runs across the conditions that previously varied. [`GOOGLE-SRE-TESTING`]
10. Track flake rate, affected platforms, retry cost, recurrence, and confidence until the issue is resolved. [`ALLURE-STABILITY`, `ISTQB-TM`]

## Examples

- Isolate an order-dependent test that leaves a static account or database record for the next test.
- Replace a sleep-based click with a state-based readiness condition after timing traces show a race.
- Correlate intermittent mobile failures with one device image, orientation transition, or Appium driver version.

## Boundary case

- A test that fails consistently under equivalent conditions is not flaky; route it to `shaft-failure-analysis` instead of hiding it behind retries.

## Output

Return flake evidence, controlled variables, suspected and eliminated causes,
repair or quarantine controls, repeated-run proof, rate, and residual risk.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
