# Copilot Memory Ledger

Purpose: keep high-signal, reusable learnings from implementation sessions in one canonical place.

## Entry Template
- Date:
- Area: (e.g., Allure, Properties, CI workflow, WebDriver lifecycle)
- Trigger: (bug/regression/request)
- Lesson:
- Evidence: (file path, test class, workflow, issue/PR)
- Action taken: (instruction update, test added, code guardrail)

## Entries
- Date: 2026-05-07
- Area: Encoding / Unicode
- Trigger: Unicode text rendered incorrectly when tests were run by automated agents on different hosts.
- Lesson: Enforce UTF-8 at all runtime layers (Maven JVM, Surefire forked JVMs, and CI job environment) instead of relying on host defaults.
- Evidence: `pom.xml`, `.mvn/jvm.config`, `.github/actions/setup-test-env/action.yml`, `src/test/java/testPackage/properties/Log4jTests.java`
- Action taken: Added explicit UTF-8 JVM flags and coverage assertions to prevent regressions.

