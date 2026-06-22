# Contributing To SHAFT Engine

This repository contains the Java 25 Maven reactor for SHAFT Engine. The public
user guide lives in
[ShaftHQ/shafthq.github.io](https://github.com/ShaftHQ/shafthq.github.io).

## 1. Check Out The Code

Fork the repository if you are not a maintainer. Then clone your fork:

```bash
git clone https://github.com/<your-user>/SHAFT_ENGINE.git
cd SHAFT_ENGINE
git remote add upstream https://github.com/ShaftHQ/SHAFT_ENGINE.git
```

Maintainers can clone the canonical repository directly:

```bash
git clone https://github.com/ShaftHQ/SHAFT_ENGINE.git
cd SHAFT_ENGINE
```

Start every change from current `main`:

```bash
git fetch --prune origin
git switch main
git pull --ff-only origin main
git switch -c <short-topic-branch>
```

If you work from a fork, replace the pull command with:

```bash
git fetch --prune upstream
git switch main
git merge --ff-only upstream/main
git push origin main
git switch -c <short-topic-branch>
```

## 2. Install The Toolchain

Required:

- JDK 25.x only.
- Maven 3.9.0 or newer.
- Git.

The repository includes `.java-version` and `.sdkmanrc` for version managers.
With SDKMAN:

```bash
sdk env install
sdk env
```

Verify the active tools before building:

```bash
java -version
mvn -version
```

Expected: Java reports version `25.x`, and Maven reports `3.9.0` or newer.

## 3. Build Locally

PowerShell users should keep Maven `-D` arguments quoted as shown.

```bash
mvn clean install "-DskipTests" "-Dgpg.skip"
```

This compiles the reactor and installs local artifacts without running the test
suite.

## 4. Make The Change

- Keep the scope tight and follow existing package and module boundaries.
- Preserve public API compatibility. Deprecate before removing or renaming a
  public API.
- Add JavaDocs for every new public class or public method.
- Use Log4j or SHAFT `ReportManager`; do not use `System.out.println` or
  `System.err.println` in production code.
- Use SHAFT fluent assertions in tests where applicable.
- Do not commit generated reports, binaries, secrets, `target/`, or local
  credential files.

The Docusaurus user guide is the canonical location for product, usage,
architecture, migration, and maintainer documentation. Do not add public guides
or module READMEs to this repository.

## 5. Validate By Risk

For documentation or agent-guidance changes:

```bash
py -3 scripts/ci/validate_agent_setup.py
py -3 scripts/ci/validate_documentation_boundaries.py
git diff --check
```

For localized code changes, run the affected test first:

```bash
mvn -pl shaft-engine -am test "-Dtest=TestClassName"
```

Then run one compile/package pass appropriate to the change. For broad API,
concurrency, build, or release changes, run targeted tests and the full
compile/package command before opening a pull request.

When public JavaDocs change, also run:

```bash
mvn -pl shaft-engine javadoc:javadoc
```

For visual behavior changes, include image or browser evidence. Run external or
credentialed cloud suites only when the required infrastructure is available and
the result is necessary.

## 6. Update Documentation When Needed

If the change affects public behavior, update the user guide in
[ShaftHQ/shafthq.github.io](https://github.com/ShaftHQ/shafthq.github.io) in
the same delivery.

Before the engine PR is ready:

1. Open the documentation PR.
2. Verify the documentation deploy preview and affected canonical routes.
3. Link the documentation PR from the engine PR.
4. If documentation is not required, state the concrete reason in the engine PR.

## 7. Open The Pull Request

Push your branch:

```bash
git push -u origin <short-topic-branch>
```

Open a pull request against `ShaftHQ/SHAFT_ENGINE:main`.

The PR is ready for review when it includes:

- A clear problem statement and solution summary.
- Focused tests or evidence for the changed path.
- Validation commands and results copied into the PR description.
- JavaDocs for new public APIs.
- Compatibility preserved, or deprecations added before removals.
- A linked user-guide PR for user-facing behavior changes, or a clear reason no
  docs change is needed.
- No generated reports, binaries, secrets, `target/`, or credentialed cloud
  output.

Reviewers should be able to check out the branch, run the listed commands, and
see the same result.
