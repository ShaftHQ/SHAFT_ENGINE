# IntelliJ Platform Plugin Development (shaft-intellij)

## Build reality
Use the checked-in Gradle 9.3+ wrapper with JDK 25 as the build runtime;
source/target compatibility remains 17 for IntelliJ Platform compatibility.
UI evidence comes from
`ShaftPluginScreenshotRendererTest -Dshaft.intellij.screenshotDir=...` —
visible UI changes must regenerate screenshots for the PR.

Issue #3784 recorded one Windows OpenJDK 25.0.1 G1 crash and introduced a
temporary JDK 21 ceiling. That build already used Gradle 9.3, so the wrapper
version does not explain the crash. Issue #4779 raised the ceiling to JDK 25
only after
repeated fresh Windows JDK 25 builds passed and the supported Gradle/JDK
combination was reconfirmed; newer daemon JDKs remain rejected. CI must invoke
`shaft-intellij/gradlew` so its runtime support stays coupled to the checked-in
wrapper instead of a separately pinned system Gradle.

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

Any test that installs a real platform L&F (`UIManager.setLookAndFeel`) must
register `LookAndFeelIsolationExtension` (`shaft-intellij/src/test/java/.../ui/`,
issue #3782) so it snapshots/restores the `LookAndFeel` instance, `JBColor`
dark flag, and declared `UIManager` keys on the EDT — `UIManager.put()`
overrides persist across L&F switches by Swing design, and IntelliJ's
`DefaultTreeUI` only becomes the active `TreeUI` delegate once a platform
L&F has been installed at least once in the JVM, a latch that restoring the
L&F instance does **not** undo. Confirmed live (issue #3786): running the
*whole* module's `gradlew test` together with
`-Dshaft.intellij.screenshotDir=...` still breaks `ShaftTestsPanelTest`'s
tree-selection tests via that sticky latch even with the extension in
place — always scope screenshot regeneration with
`--tests ShaftPluginScreenshotRendererTest`, never a bare full-suite run.
