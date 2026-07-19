# IntelliJ Platform Plugin Development (shaft-intellij)

## Build reality
Gradle 9+ with JDK 21 as the build runtime (not Gradle 8.x, not JDK 25;
source/target compatibility is 17). UI evidence comes from
`ShaftPluginScreenshotRendererTest -Dshaft.intellij.screenshotDir=...` —
visible UI changes must regenerate screenshots for the PR.

`shaft-intellij/settings.gradle.kts` enforces the JDK-25-crashes-the-daemon
constraint directly (issue #3784): it fails fast with an actionable message
if the Gradle Daemon JVM is newer than JDK 21, instead of letting JDK 25
crash the daemon (`EXCEPTION_ACCESS_VIOLATION` in G1 GC on Windows). A hard
Gradle Daemon JVM criteria/toolchain pin to exactly JDK 21 was considered
and rejected: that mechanism takes precedence over `JAVA_HOME` and
`-Dorg.gradle.java.home` (Gradle docs), which would have forced CI's
`intellij-plugin.yml` `build-and-verify` job — JDK 17 only, via a
system-installed `gradle`, not the wrapper — onto a JDK 21 it never
installs. The guard leaves JDK 17-21 alone and only rejects newer,
unverified JDKs.

## Threading & UI rules
- EDT-only for Swing mutation; use `invokeLater`/`Application.invokeLater`
  from background threads, and never block the EDT on MCP/process I/O —
  every shaft-mcp call from the plugin belongs on a pooled thread with an
  EDT callback.
- `StatusText`/`getEmptyText()` placeholder lines clip at a fixed internal
  width (~42 chars) regardless of component width and never wrap — don't
  fight it with shorter fragments; paint placeholders manually
  (`paintComponent` + FontMetrics word-wrap) and expose text via a client
  property for tests (SHAFT's `PlaceholderTextArea`).
- Long-lived background pollers (heartbeats spawning a fresh MCP process
  every 30s per panel) are a leak pattern — lifecycle-scope them.
- Capture `this::method` as a field when the same reference must later be
  removed (e.g. `addStateChangeListener`/`removeStateChangeListener` pairs in
  `addNotify()`/`removeNotify()`) — re-evaluating a method-reference
  expression is not guaranteed to produce an identity-equal object, so a
  fresh one at the removal call site silently fails to remove anything
  (issue #3621).

## Trust & failure surfacing (hard-won)
- An MCP tool result with `isError: true` inside a successful JSON-RPC
  envelope IS a failure — surface the tool's own text; masking it as
  "Finished/OK" hides every downstream failure mode (PR #3396's
  highest-blast-radius fix).
- Gate SHAFT-specific actions on real project detection (pom/gradle contains
  shaft coordinates; fail-closed, cached) so the plugin is inert in ordinary
  projects.
- Setup-step badges derive ONLY from real verification checks, never from
  "the user clicked the button" (PR #3427). No phantom recorder steps:
  suppress browser-synthesized events that don't map to a user action.
- Long-running work (recordings) must never belong to an ephemeral agent
  turn/process — start it in a service whose lifetime the tool window owns
  (PR #3431).

## Testing
Plugin tests run headless via the IntelliJ test framework; Swing panels are
unit-testable if logic is separated from EDT wiring. Playwright/webapp tools
cannot see Swing — evidence is the screenshot renderer, not a browser.
