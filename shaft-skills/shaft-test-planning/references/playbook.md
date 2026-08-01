# Test-planning playbook

## Ten practices

1. Define objectives, in-scope and out-of-scope product areas, test items, and required decisions. [`ISTQB-CTFL`, `ISTQB-TM`]
2. Fit the plan to the delivery lifecycle, change size, architecture, regulations, and team context. [`ISTQB-CTFL`, `ISTQB-TM`]
3. Prioritize product and project risks by likelihood, impact, detectability, and time sensitivity. [`ISTQB-TM`]
4. Select suitable test levels, types, techniques, automation depth, and regression scope for each risk. [`ISTQB-CTFL`, `SELENIUM-PRACTICES`]
5. Set measurable entry, exit, suspension, resumption, and completion criteria. [`ISTQB-CTFL`, `ISTQB-TM`]
6. Estimate effort, duration, people, skills, environments, devices, tools, and contingency from explicit assumptions. [`ISTQB-TM`]
7. Plan test data, environments, dependencies, access, observability, and evidence retention before execution. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
8. Sequence activities around builds, integrations, migrations, freezes, and external dependencies. [`ISTQB-TM`]
9. Define progress, coverage, quality, defect, flake, risk, and completion measures plus their audiences and cadence. [`ISTQB-TM`, `ALLURE-STABILITY`]
10. Review the plan with stakeholders and adapt it when risks, scope, evidence, or constraints change. [`ISTQB-TM`]

## Valid examples

- Plan smoke, critical-journey, and regression coverage for a web checkout release with a two-day test window.
- Plan contract, migration, rollback, and data-integrity tests for a versioned API change.
- Plan Android and iOS coverage across supported OS versions, real devices, emulators, and accessibility checks.

## Boundary case

- If execution is demanded before scope, build, environment, or acceptance criteria are known, provide a minimal risk-based preflight and mark the plan blocked; do not represent assumptions as approval.

## Output

Return plan scope, risk-to-coverage mapping, activities, dependencies, estimates,
criteria, measures, owners, and unresolved decisions.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
