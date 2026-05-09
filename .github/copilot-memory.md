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

- Date: 2026-05-08
- Area: Agent knowledge migration / instruction hygiene
- Trigger: Request to scan repository skills, tools, instructions, and memory for a smooth AI-agent migration without knowledge loss.
- Lesson: Use `docs/AGENT_KNOWLEDGE_MIGRATION.md` for future migration tasks: create or link a ticket, branch before editing, progressively scan instruction/memory/skill files, favor executable configuration over stale prose, update the memory ledger, commit, and open a PR; if GitHub publication access is missing, explicitly report the limitation and provide a PR handoff.
- Evidence: `docs/AGENT_KNOWLEDGE_MIGRATION.md`, `docs/tickets/2026-05-08-agent-knowledge-migration.md`, `.github/copilot-instructions.md`, `CLAUDE.md`
- Action taken: Added a migration runbook, local ticket, global instruction reference, Claude guidance, and this memory entry.

- Date: 2026-05-09
- Area: WebDriver lifecycle / coverage tests
- Trigger: Grid E2E failures in `ElementActionsCoverageUnitTest.shouldCoverBasicFluentAndActionWrappers`.
- Lesson: Coverage-only unit tests must not call GUI facade default constructors such as `ElementActions::new`; those constructors create real browser sessions through the driver factory and can produce Selenium Grid failures even when the test otherwise uses mocks. Prefer explicit mock-driver constructors for unit coverage and keep all driver-factory/session paths in focused integration tests.
- Evidence: `src/test/java/testPackage/unitTests/ElementActionsCoverageUnitTest.java`
- Action taken: Removed the default-constructor coverage invocation so the test remains fully mock-backed and session-isolated.
