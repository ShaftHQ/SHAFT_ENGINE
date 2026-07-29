# Maven Release Engineering

## SHAFT publishing shape
Multi-module reactor: core `shaft-engine`, optional `shaft-*` feature
modules, BOM and compatibility/publication modules. Consumers pin one
version; the BOM must stay internally consistent. Release flow runs GPG
signing — locally it fails without `-Dgpg.skip=true`; the proven local
install for live validation is
`mvn -pl shaft-engine,shaft-capture,shaft-mcp install -DskipTests -Dgpg.skip=true`.

## Dependency convergence
- `maven-enforcer-plugin` lives in root pluginManagement, activated in
  shaft-engine (banned-deps + convergence) — it is the cheap local mirror of
  merge-time CI breakage: run a targeted `mvn -pl <module> verify -DskipTests`
  before pushing dependency changes.
- Transitives can be internally inconsistent: `jython-slim:2.7.4` declares
  jffi 1.3.13 direct AND jffi-native 1.4.0 — converge both classifiers in
  `dependencyManagement`, not just the main artifact (PR #3344).
- Excluding a transitive (e.g. `org.openpnp:opencv` from sikulixapi) without
  re-declaring it directly compiles fine and dies at runtime with
  `NoClassDefFoundError` — check what sibling modules with the same need
  declare (shaft-sikulix vs shaft-visual, PR #3408).

## Resources & packaging traps
- An explicit `<resources><includes>` allowlist silently drops every file
  not listed — new runtime resources (JS payloads like `axe.min.js`) never
  reach `target/classes` or the jar while unit tests that read from source
  tree still pass (PR #3383). Verify packaging with `unzip -l` on the jar.
- Version shadowing: locally-installed `.m2` jars mask Central artifacts of
  the same version; a fix that "works locally" may be running a patched
  local build. Check jar timestamps/hashes when validation seems too easy.

## Verdict rules
- `workflow_run` conclusion `success` only means no job FAILED — job-level
  `if:` skips count as success. A "green" release workflow can have
  delivered nothing (repo gotcha; also: releases created with the default
  GITHUB_TOKEN emit NO release event — dependent workflows never fire).
- Release-candidate pipelines fail fast and mask later-stage bugs; a new
  failure after a fixed one is usually a genuinely new issue further along,
  not a regression of the fix.
