# AGENTS.md

## Purpose
This file gives durable, repository-specific guidance for Codex and other coding agents working in SHAFT_ENGINE. Follow it for files in this repository root unless a more local `AGENTS.md` is added later.

## Repo Overview
SHAFT_ENGINE is a Maven-published Java test automation framework with a fluent API for web, mobile, API, CLI, database, visual, accessibility, and performance testing.

Important directories:
- `shaft-engine/src/main/java/com/shaft/`: framework source. Key packages include `driver` (`SHAFT` facade), `gui`, `api`, `cli`, `db`, `validation`, `properties`, `listeners`, and `tools`.
- `shaft-engine/src/test/java/`: TestNG/JUnit/Cucumber framework validation tests and examples.
- `shaft-engine/src/test/resources/`: Cucumber features, test suites, and test data files.
- `shaft-engine/src/main/resources/`: default configuration, examples, Docker/Kubernetes assets, Allure resources, images, and runtime support assets.
- `shaft-engine/pom.xml`: published engine JAR build, dependencies, resources, tests, and module-specific plugin executions.
- `docs/`: human documentation and architecture/runbook notes.
- `.github/workflows/`: CI for E2E tests, local tests, cloud tests, CodeQL, link checks, Maven Central deployment, JavaDocs, and dependency/version sync.
- `.github/instructions/`: path-scoped Copilot rules for Java source and tests; consult these before touching matching files.
- `.github/skills/` and `.github/copilot-memory.md`: prior agent workflows and reusable lessons.

## Tech Stack
- Java library packaged as a JAR with Maven.
- Executable config currently requires JDK `25.x` and Maven `3.9.0+`; trust `pom.xml`, `.java-version`, `.sdkmanrc`, and `.mvn/jvm.config` over stale prose.
- Major libraries/tools: Selenium WebDriver, Appium, REST Assured, TestNG, JUnit, Cucumber, Allure, JaCoCo, Lombok, Docker Compose for Selenium Grid/Postgres-backed E2E workflows, BrowserStack, and LambdaTest.
- CI quality gates include CodeQL, Codacy/Codecov integrations, dependency review, link checking, and Maven workflows.

## Setup
1. Use JDK 25. `.java-version` says `25`; `.sdkmanrc` says `java=25.0.1-tem`.
2. Use Maven 3.9.0 or newer.
3. Compile/package without tests first: `mvn clean install -DskipTests -Dgpg.skip`.
4. Do not install extra packages for discovery unless necessary. Maven will resolve project dependencies.
5. For browser/mobile/cloud E2E work, verify local browsers, Docker, Appium assets, BrowserStack/LambdaTest credentials, or Selenium Grid endpoints before assuming tests can run locally.

## Common Commands

| Task | Command | Notes |
| --- | --- | --- |
| Install dependencies / compile package | `mvn clean install -DskipTests -Dgpg.skip` | Mandatory pre-commit compile for code changes in existing agent guidance. Skips tests and GPG signing. |
| Run all tests | `mvn test` | Can be slow/environment-dependent; Surefire is configured with `testFailureIgnore=true`. Treat Allure results as the pass/fail source of truth. |
| Run targeted tests | `mvn -pl shaft-engine -am test -Dtest=TestClassName` | Preferred first validation for focused changes. Comma-separated and `%regex[...]` patterns are used in CI. |
| Run targeted tests with properties | `mvn -e -pl shaft-engine -am test "-Dtest=TestClassName" "-DtargetBrowserName=chrome"` | Use quoted `-D...` args when values include regex, commas, or shell-sensitive characters. |
| Generate JavaDocs | `mvn -pl shaft-engine javadoc:javadoc` | Use when public API JavaDocs are changed. |
| Publish JavaDocs | `mvn -pl shaft-engine resources:resources javadoc:javadoc scm-publish:publish-scm` | Listed in `pom.xml`; do not run unless explicitly requested. |
| Build/deploy to Maven Central | `mvn clean deploy -DskipTests` | Release-only; requires credentials/signing and should not be run during normal agent work. |
| Start Selenium Grid | `docker compose -f shaft-engine/src/main/resources/docker-compose/selenium4.yml up --scale chrome=1 --scale edge=0 --scale firefox=0 -d` | CI scales nodes higher. Requires Docker and cleanup after use. |
| Format | Verify before relying on this | No explicit formatter command was found. Preserve existing style. |
| Lint/static analysis | Verify before relying on this | No local lint command was found; CodeQL/Codacy run in CI. Use compile/tests/JavaDocs locally. |
| Typecheck | `mvn clean install -DskipTests -Dgpg.skip` | Java typechecking happens during Maven compilation. |
| Local app/service run | Not applicable | This repo is a library/framework, not a standalone web app. |

## Testing Guidance
- Primary test locations are `shaft-engine/src/test/java/` and `shaft-engine/src/test/resources/`.
- Test frameworks: TestNG is primary; JUnit and Cucumber are also supported via Maven/Surefire profiles and listeners.
- Prefer narrow validation first: `mvn -pl shaft-engine -am test -Dtest=AffectedTestClass`.
- CI uses many property-driven E2E runs, including local browsers, Docker Selenium Grid, BrowserStack, LambdaTest, mobile/Appium, API, DB, Cucumber, and Flutter-related tests. Do not assume those all run on a bare machine.
- Allure result JSON/report output is the authoritative pass/fail source for SHAFT test runs. Surefire output is useful diagnostics, but do not use it as the final oracle when it disagrees with Allure.
- Before analyzing Allure statuses, first count the executed tests/result JSON files. If the count is zero or unexpectedly low, treat the report as empty/invalid and fix rerun/report generation before drawing conclusions.
- For GitHub Actions E2E investigations, use `docs/CI_FAILURE_INVESTIGATION.md` to pull run/job metadata, download job logs, and parse self-contained `*_Allure.html` artifacts directly from embedded `data/test-results/*.json` payloads instead of opening large reports manually.
- Surefire is configured to continue after failures so JaCoCo/report generation can complete. Use `target/surefire-reports` as supporting evidence only after validating the Allure run is populated.
- Test data belongs under `shaft-engine/src/test/resources/testDataFiles/`; avoid hardcoding data in tests when existing guidance requires JSON test data.
- For TestNG browser tests, existing instructions require fresh driver setup per test and `@AfterMethod(alwaysRun = true)` cleanup with `driver.quit()`.
- For parallel tests, isolate state with `ThreadLocal` and avoid sharing driver instances across methods.
- Under TestNG `setParallel=METHODS`, ordinary instance fields are shared by concurrently running methods in the same test class. Store per-method temp directories, files, mocks, and setup state in `ThreadLocal` or keep the class `@Test(singleThreaded = true)` when it must mutate static/shared state; otherwise one method's `@AfterMethod` can delete or reset another method's resources.
- `@Test(singleThreaded = true)` serializes methods inside one TestNG class only; it does not isolate static SHAFT property setters from other classes in a parallel suite. Tests that assert behavior depending on global properties such as `executionAddress` must set those prerequisites inside the test (and rely on `@AfterMethod(alwaysRun = true)` cleanup) to avoid E2E flakes from cross-class property mutation.
- Tests or setup code that clean Allure results during parallel TestNG execution should preserve the live `allure-results` root directory and delete only its contents. Replacing the root can race with Allure's own file writer on Windows and surface as `FileAlreadyExistsException`/BROKEN results.
- When a final Allure summary is green but job logs show retry attempts, inspect the non-passed `*-result.json` attempts too. Retried `skipped` results with file/image IO messages often indicate a real parallel-isolation flake that may later become a final BROKEN/FAILED test in CI.

## Code Style and Conventions
- Public framework classes and public methods must have JavaDoc, including relevant `@param`, `@return`, and `@throws` tags.
- Keep public APIs backward-compatible. Do not remove or rename public methods without a deprecation cycle; prefer overloads for changed signatures.
- Public API facades use nested `public static` classes, especially around `SHAFT.java`.
- Utility classes should have a private constructor that throws `IllegalStateException("Utility class")`.
- Prefer specific exceptions, meaningful logging via SHAFT logging utilities, and no silent exception swallowing.
- Do not call `System.out.println` in framework code.
- Do not hardcode configurable values such as versions, URLs, timeouts, thresholds, or behavior switches. Add `@Key`/`@DefaultValue` entries in the appropriate `shaft-engine/src/main/java/com/shaft/properties/internal/` interface and access them through `SHAFT.Properties...`.
- Do not call `System.getProperty()` directly in framework code; use the SHAFT properties layer.
- Use `ThreadLocalPropertiesManager` for per-test/per-thread property overrides and clear thread-local properties at lifecycle boundaries with `Properties.clearForCurrentThread()`.
- Test assertions should use SHAFT fluent assertion wrappers where applicable, not raw TestNG/JUnit assertions.
- Prefer the `Locator` builder for expressive locators; simple stable `By.id`, `By.cssSelector`, or `By.xpath` are acceptable where appropriate.
- Keep WebDriver capabilities W3C-compliant: do not add legacy non-namespaced top-level capabilities such as `enableVideo`; use namespaced vendor/Grid capabilities (for example `selenoid:options`, `moon:options`, `se:recordVideo`) and merge existing maps instead of overwriting them.

## Architecture Notes
- `com.shaft.driver.SHAFT` is the main user-facing entry point that namespaces GUI, API, CLI, DB, TestData, Validations, Properties, and Report functionality.
- GUI functionality wraps Selenium/Appium and is split across browser, element, touch, alert, locator, image, and video helpers.
- API functionality is centered around REST Assured wrappers (`RestActions`, `RequestBuilder`, filters).
- Configuration is centralized in `com.shaft.properties.internal`; distinguish engine-wide flags from per-thread overrides.
- Reporting integrates with Allure and SHAFT report/log attachment helpers.
- The framework is intended to be batteries-included: when adding external tool dependencies, use resolution patterns such as PATH, package manager, and self-download into the Maven repository rather than requiring users to install binaries manually.

## GitHub API and CLI Authentication
- Before using GitHub APIs, Actions artifacts/logs, or `gh`, normalize the token environment with `source scripts/ci/github-auth-env.sh`. This maps `GITHUB_TOKEN` to `GH_TOKEN` and `GH_TOKEN` to `GITHUB_TOKEN` without printing secret values.
- If `gh` is missing and GitHub access is needed, provision the official GitHub CLI apt repository in the current Ubuntu/Debian environment before retrying:
  ```bash
  type -p curl >/dev/null || (sudo apt update && sudo apt install curl -y)
  curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
  sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
  echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
  sudo apt update
  sudo apt install gh -y
  gh --version
  ```
- After `gh` is installed, run `GH_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}" gh auth status -h github.com` to verify authentication without exposing the token. GitHub CLI automation requires a `GH_TOKEN` or `GITHUB_TOKEN` secret with the scopes needed for the requested operation, such as `repo` and `workflow` for repository and workflow actions.
- Prefer authenticated `gh` commands for GitHub Actions artifacts, logs, issues, and PRs; anonymous REST calls can list some public metadata but cannot reliably download artifacts, read logs, or create issues.
- When the user asks to create, draft, edit, or optimize a ticket/issue, create or update a GitHub Issue directly with `gh issue create`/`gh issue edit` or the authenticated GitHub API. Do not add a repository Markdown ticket file for that request unless the user explicitly asks for a checked-in document. If GitHub authentication, repository remotes, or publication access are unavailable, say so clearly and provide the intended issue title/body as a handoff instead of committing it as `docs/tickets/*.md`.

## Agent Workflow
- Start by reading this file plus any scoped instructions relevant to the files you will touch, especially `.github/instructions/framework-source.instructions.md` for `shaft-engine/src/main/java/**/*.java` and `.github/instructions/java-tests.instructions.md` for `shaft-engine/src/test/java/**/*.java`.
- When the user asks you to start work on a task, mentions `@codex` with `start` or `init`, or otherwise uses those keywords to begin implementation, follow the full start/init protocol below before coding. For `@codex init` comment triggers, begin with a planning phase and publish that plan back to the originating conversation before implementation.
- When the user asks you to work on an issue, create a dedicated branch from that issue and ensure the branch is linked to the issue correctly. Open a draft pull request early with a generic implementation-plan description, continue working locally on that branch, and push changes to the remote branch once you reach a good implementation milestone. When the work is complete, push the latest code, mark the pull request ready for review, and notify the user that the pull request is ready for review.
- Follow existing PDCA-style guidance: plan, make the smallest focused change, validate, then refactor only as needed.
- For bug fixes, reproduce the issue and add/verify a failing test before changing framework code whenever practical.
- Run the narrowest useful test first, then broader validation when the change warrants it.
- Update tests and docs when behavior changes.
- Avoid broad refactors, dependency churn, release/version changes, generated artifact edits, or workflow rewrites unless explicitly requested.
- Report commands run and results. Call out unvalidated assumptions and environment limitations.
- For agent-instruction or memory migrations, see `docs/AGENT_KNOWLEDGE_MIGRATION.md` and keep durable guidance in long-lived instruction files rather than task reports.
- A weekly/manual GitHub Actions workflow (`Refresh Agent Instructions`) uses `openai/codex-action@v1` to propose agent-guidance refreshes by pull request only; it requires the `OPENAI_API_KEY` Actions secret.

### Start/Init Task Protocol
Use this protocol whenever a maintainer tags `@codex` to start work, says `start`, says `init`, or otherwise asks the agent to begin a task. If a direct higher-priority instruction or environment limitation prevents a step, state the limitation explicitly and perform the nearest safe equivalent.

1. **Plan first for `@codex init` comments.** When tagged in an issue, pull request, discussion, or other conversation comment that says `init`, start with a planning phase before any implementation. Inspect the task context, produce a concrete implementation plan, and publish that plan as a real comment in the same GitHub conversation that mentioned you by using the GitHub UI, `gh`, or the authenticated GitHub API; do not leave the plan only in a local final response, PR body draft, or `make_pr` metadata because automation cannot execute text that is not posted back to the originating thread. The planning comment must include the exact phrase `@codex start implementing this plan.` so a follow-up automation trigger can begin execution. To avoid endless loops, plan only once per conversation after the first `@codex init` mention; before planning, check whether Codex has already been mentioned or has already posted a planning comment in that conversation, and do not replan unless a maintainer explicitly asks for a new or revised plan. Treat the `@codex start implementing this plan.` phrase in your own planning comment as the handoff to implementation, not as a new request to plan again.
2. **Assign ownership.** Assign the task to `codex` where GitHub supports that identity. If `codex` cannot be assigned, assign the task to the requesting maintainer instead and explain why in the PR or status update. Do not assign the task to unrelated maintainers such as `MohabMohie` unless explicitly requested.
3. **Link traceability before work.** Create a dedicated branch that is linked to the GitHub Issue or task. Prefer branch names that include the issue number or closing keyword relationship where the platform supports it, so merging the branch or PR can automatically close the task. If there is no issue number, no authenticated GitHub access, or the trigger is only a PR comment, document that no auto-closing link could be created.
4. **Open a draft PR early.** Create a draft PR from the new branch before substantial implementation. The PR body must identify Codex as the implementation agent and include an implementation plan written for a lower-intelligence AI agent: detailed steps, explicit files to inspect or edit, command examples, acceptance criteria, validation commands, rollback/failure handling, and assumptions.
5. **Use iterative PDCA execution.** Follow at least three development iterations for non-trivial work: iteration 1 makes the smallest working change, iteration 2 refactors toward minimal impact and industry best practices, and iteration 3 optimizes readability, maintainability, validation coverage, and documentation. Each iteration must include Plan, Do, Check, and Act activities.
6. **Commit at checkpoints.** Commit code to the task branch at every useful checkpoint after validation passes for that checkpoint. Keep commits focused and avoid mixing unrelated changes.
7. **Validate before claiming done.** Run the narrowest relevant checks first, then broader checks when warranted. For SHAFT test runs, confirm Allure results are populated before using them as the pass/fail oracle.
8. **Publish final status.** After implementation is done, push the final branch state, update the PR description or add a progress comment summarizing completed iterations, checks run, files changed, and open risks, then notify the requester that the PR is ready for review.


## Release Preparation Notes
- Release versions use `{major}.{quarter}.{YYYYMMDD}`; for example, a Q2 release generated on May 13, 2026 is `10.2.20260513`.
- A release PR title should include the release name/version and clearly say that the PR generates or prepares a new release.
- Before opening a release PR, inspect recent merged release PRs for changed files and PR-body conventions, then update these locations together: root `pom.xml`, every reactor child parent version, `shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java`, all consumer fixture `<shaft.version>` values, and all seven sample/demo project `pom.xml` files under `shaft-engine/src/main/resources/examples/`.
- In `Internal.java`, verify `allure3Version` against the latest stable `allure` npm package and `nodeLtsVersion` against the latest Node.js LTS patch; record when a value is already current.
- Validate release-only changes with dependency-update checks and `mvn clean install -DskipTests -Dgpg.skip`; merging the PR to `main` starts the Maven Central release pipeline, so do not run deploy/release commands locally.

## Safety and Constraints
- Do not expose secrets or copy values from `.env`, credential files, BrowserStack/LambdaTest variables, Maven Central credentials, GPG keys, or CI secrets.
- Do not run deployment/release commands, `scm-publish`, history rewrites, destructive cleanup, or credentialed cloud workflows unless explicitly requested.
- Treat `target/`, generated reports, downloaded binaries, and build artifacts as generated outputs; do not commit them unless maintainers explicitly request it.
- Be careful with release `pom.xml` version changes: release PRs must update the root project version, every reactor child parent version, consumer fixtures, `Internal.java` `shaftEngineVersion`, `allure3Version`, and `nodeLtsVersion`, and every sample/demo `<shaft.version>` under `shaft-engine/src/main/resources/examples/**/pom.xml` in the same branch before opening the PR. Do not rely on the post-release sample-sync workflow to fix release PR drift.
- Binary mobile test assets (`*.apk`, `*.ipa`) have special JaCoCo exclusions; do not move or reclassify them casually.
- Docker Compose E2E services should be stopped/cleaned up after local use.

## Prior Agent Lessons
- A previous migration consolidated durable agent knowledge into `.github/copilot-instructions.md`, `.github/copilot-memory.md`, `.github/instructions/`, `.github/skills/`, and `docs/AGENT_KNOWLEDGE_MIGRATION.md`; consult those instead of rediscovering all conventions from scratch.
- Executable configuration wins over stale prose when versions or commands disagree.
- Path-scoped instructions must stay scoped; do not flatten source-only or test-only rules into global policy unless they are truly repository-wide.
- Documentation-only changes can use lightweight validation, but code changes are expected to compile and run affected tests before commit.
- Java 25 plus Mockito inline mocking can fail on some TestNG interfaces with optional dependency types. Prefer extracting list/value-based helper logic for unit coverage over mocking or proxying `org.testng.ISuite`.
- If GitHub publication access is unavailable, say so clearly and provide a PR handoff rather than implying a visible GitHub PR exists.
- Agent-created branches/PRs must identify the agent author. Prefer branch names starting with `codex/` and PR titles starting with `codex:` for Codex-authored work. If a human maintainer token creates the PR, state in the PR body that Codex made the changes and that the token owner did not author them manually.
- The local `make_pr` tool only records PR metadata for the agent harness; it does **not** publish a visible GitHub pull request or upload the branch. For GitHub-backed tasks, after committing changes, push the `codex/` branch to `origin`, create or update the real GitHub PR with `gh pr create`/`gh pr edit`, include `Closes #<issue>` or an equivalent issue link when an issue exists, and verify with `git ls-remote --heads origin <branch>` plus `gh pr view --json number,url,headRefName,baseRefName,closingIssuesReferences` before telling the user the PR is open.
- Edge Grid run `27004138692` exposed a TestNG METHOD-parallel race where `ImageProcessingActionsUnitTest` stored its per-method temp directory in an instance field. The final signature was `javax.imageio.IIOException: Can't create an ImageInputStream!`, but job logs showed another method's cleanup deleting `target/temp/imageProcessingUnitTests/.../*.png`. For similar image/file failures, inspect adjacent setup/cleanup lines and convert per-method state to `ThreadLocal` before changing image comparison logic.

## Open Questions / Verify Before Relying
- No explicit local formatter command was found.
- No explicit local lint command was found beyond compile/tests/JavaDocs and CI CodeQL/Codacy.
- Some prose says Java 21, but current executable config enforces JDK 25; verify current `pom.xml` before changing toolchains.
- Exact credentials/environment variables for BrowserStack, LambdaTest, Maven Central, and release publishing are CI/maintainer concerns; do not infer or expose them.
