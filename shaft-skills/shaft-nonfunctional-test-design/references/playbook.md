# Nonfunctional-test-design playbook

## Ten practices

1. Convert each quality risk into a measurable objective, workload or fault, threshold, observation window, and decision rule. [`ISTQB-CTFL`, `ISTQB-TM`]
2. Prioritize performance, security, reliability, recovery, compatibility, scalability, usability, portability, and maintainability coverage by impact and likelihood. [`ISTQB-TM`, `OWASP-WSTG`]
3. Model realistic user journeys, concurrency, arrival rates, data volumes, distributions, platforms, and dependency behavior. [`GOOGLE-SRE-TESTING`]
4. Use a controlled production-representative environment and document calibration limits and intentional differences. [`GOOGLE-SRE-TESTING`]
5. Establish a repeatable baseline before comparing a change, capacity limit, degradation, or service-level target. [`GOOGLE-SRE-TESTING`, `GOOGLE-SRE-MONITORING`]
6. Design security checks from exposed assets, trust boundaries, identities, data flows, threats, and current OWASP guidance. [`OWASP-WSTG`]
7. Separate load, stress, spike, endurance, volume, scalability, and resource-efficiency questions instead of using one undifferentiated performance run. [`GOOGLE-SRE-TESTING`]
8. Exercise dependency loss, timeout, saturation, partial failure, restart, rollback, backup, restore, and disaster-recovery paths safely. [`GOOGLE-SRE-TESTING`]
9. Observe latency distributions, throughput, errors, saturation, resources, recovery time, data integrity, and user-visible symptoms during every run. [`GOOGLE-SRE-MONITORING`]
10. Repeat enough to quantify variance, disclose limitations, preserve evidence, and report thresholds and residual risk without overclaiming. [`GOOGLE-SRE-TESTING`, `ISTQB-TM`]

## Examples

- Design an API load test for peak checkout traffic using percentile latency, error rate, throughput, and saturation thresholds.
- Design a recovery test that interrupts one dependency, verifies graceful degradation, restores it, and measures recovery and data integrity.
- Design a compatibility matrix across supported browsers, devices, operating systems, locales, viewports, and network conditions.

## Boundary case

- Do not run stress, security, destructive recovery, or production probes without explicit scope, safety controls, rollback, monitoring, and authority; return a safe design instead.

## Output

Return quality risks, measurable objectives, model, environment, scenarios,
observability, thresholds, safety controls, repeat strategy, and limitations.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
