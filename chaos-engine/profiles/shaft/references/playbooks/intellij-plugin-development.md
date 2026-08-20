# IntelliJ Plugin Development

Use for `shaft-intellij` plugin, tool-window, Gradle or JDK, or Marketplace work.

Load the [IntelliJ plugin mastery chapter](../shaft-mastery/intellij-plugin.md)
instead of copying it. Visible UI also follows
[SHAFT UI design](shaft-ui-design.md).

Cite current JetBrains sources, not memory:

- Plugin SDK: https://plugins.jetbrains.com/docs/intellij/developing-plugins.html
- Marketplace approval: https://plugins.jetbrains.com/docs/marketplace/jetbrains-marketplace-approval-guidelines.html
- Marketplace security: https://plugins.jetbrains.com/docs/marketplace/understanding-plugin-security.html

## Workflow

1. Load mastery before editing plugin source, tests, Gradle, or Swing UI.
2. Keep the plugin a light wrapper. The same generate-from-scenario path must
   work from the plugin and from a CLI. Pass the user text to the selected
   client; do not drive `capture_start`, replay, or generate from the plugin.
3. Marketplace setup is copy the installer command and pre-type the IDE
   terminal only. Never execute the installer from the plugin with
   ProcessBuilder, Runtime.exec, or any equivalent.
4. Visible UI changes regenerate screenshots with
   `ShaftPluginScreenshotRendererTest` only, passing
   `-Dshaft.intellij.screenshotDir=...` and
   `-Dallure.automaticallyOpen=false`. Never a bare full-suite `gradlew test`.
5. Run the smallest focused plugin tests for the edited behavior. Headless only.

## Output

List plugin files touched, checks run, screenshot paths for visible UI, and
confirm plugin source has no installer-exec path.
