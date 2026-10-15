# Test-completion playbook

## Ten practices

1. Evaluate every planned exit criterion with its measured value, its source, and pass/fail. Do not substitute a summary judgment. [`ISTQB-CTFL`, `ISTQB-TM`]
2. Reconcile the final counts (passed, failed, broken, skipped, blocked, not run) against the approved scope before declaring completion. [`ISTQB-CTFL`, `ALLURE-RESULTS`]
3. List open defects, deferred tests, waived criteria, and residual product risk, each with an owner and a decision. [`ISTQB-TM`]
4. Archive the reports, evidence, configuration, data versions, and environment identity needed to reproduce the verdict. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
5. Classify testware as keep, refactor, retire, or promote to regression, and record why. [`ISTQB-CTFL`]
6. Remove or quarantine obsolete, duplicate, or permanently skipped automated tests through reviewed changes, without silent deletion. [`ISTQB-TM`]
7. Hand over maintained suites with their run commands, prerequisites, known flakes, and owners. [`ISTQB-TM`]
8. Capture lessons learned that would change the next plan, estimate, or design. Leave out narrative that changes nothing. [`ISTQB-TM`]
9. Release test environments, devices, accounts, and data, following retention and privacy rules. [`ISTQB-CTFL`]
10. Publish one timestamped completion record that links each claim to its source. [`ISTQB-TM`, `SHAFT-REPORTING`]

## Examples

- Close a release regression cycle: exit criteria met except one waived criterion, with its owner and expiry.
- Retire a feature's end-to-end tests after the feature is removed, keeping the API contract tests.
- Hand over a migrated suite to the owning team with run commands, flake list, and archived baseline reports.

## Boundary case

- If the result files, plan baseline, or exit criteria are missing, report the cycle as not closable and name the missing input. Never declare completion from memory.

## Output

Return an exit-criteria table, reconciled counts, residual risks and waivers,
testware decisions, archive links, handover notes, lessons, and a completion status.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
