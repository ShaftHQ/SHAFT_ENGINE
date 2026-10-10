package com.shaft.intellij.ui.firstrun;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.AssistantAgentRoute;
import com.shaft.intellij.settings.ShaftFirstRunNoticeActivity;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.text.JTextComponent;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FirstRunWizardTest {
    private static final String STDIO_COMMAND = "java -jar shaft-mcp.jar stdio";
    @Test
    void stepOneShowsDetectedFactsAndDefaultsTheInstallerTarget() throws Exception {
        Path root = Files.createTempDirectory("shaft-wizard-facts");
        Files.writeString(root.resolve("pom.xml"), "<project>io.github.shafthq shaft-engine</project>");
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        FirstRunWizardPanel wizard = wizard(root, settings, () -> { }, (client, runtime) -> ShaftMcpToolResult.failure("not yet"),
                new PrerequisitePlan.Snapshot(true, "3.9.6", true));

        String facts = text(wizard, "Project facts");
        assertEquals(1, wizard.step());
        assertTrue(facts.contains("SHAFT project"), facts);
        assertTrue(facts.contains("Maven"), facts);
        assertTrue(facts.contains("JDK:"), facts);
        assertTrue(facts.contains("Operating system:"), facts);
        assertTrue(text(wizard, "SHAFT setup stepper").contains("Step 1 of 5"), text(wizard, "SHAFT setup stepper"));
        assertEquals("INTELLIJ_PLUGIN", wizard.installerTarget());
        assertFalse(wizard.verified());
        assertFalse(wizard.userChecked());
    }

    @Test
    void continueStaysDisabledUntilToolsExistOrTheUserContinuesAnyway() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        FirstRunWizardPanel wizard = wizard(null, settings, () -> { }, (client, runtime) -> ShaftMcpToolResult.failure("no"),
                new PrerequisitePlan.Snapshot(true, "3.6.1", true));
        wizard.primary().doClick();
        JButton primary = wizard.primary();
        JButton anyway = button(wizard, "Continue anyway");

        assertEquals(2, wizard.step());
        assertFalse(primary.isEnabled());
        assertTrue(anyway.isVisible());
        assertTrue(text(wizard, "Missing tool Maven").contains("Maven"));
        anyway.doClick();
        assertTrue(primary.isEnabled());
        primary.doClick();
        assertEquals(3, wizard.step());
    }

    @Test
    void presentToolsCollapseToOneReadyLine() {
        FirstRunWizardPanel wizard = readyWizard(new ShaftSettingsState.Settings(), () -> { });
        wizard.primary().doClick();

        assertEquals("Ready: JDK, Maven, Git", text(wizard, "Ready tools"));
        assertFalse(button(wizard, "Continue anyway").isVisible());
        assertTrue(wizard.primary().isEnabled());
    }

    @Test
    void verifiedRequiresAnExplicitPassingCheckAndCopyDoesNotRunTheInstaller() {
        AtomicReference<String> copied = new AtomicReference<>("");
        AtomicReference<String> typed = new AtomicReference<>("");
        AtomicInteger probes = new AtomicInteger();
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        FirstRunWizardPanel wizard = wizard(null, settings, () -> { },
                (client, runtime) -> {
                    probes.incrementAndGet();
                    return ShaftMcpToolResult.success("ok");
                },
                new PrerequisitePlan.Snapshot(true, "3.9.8", true));
        wizard.setCopySink(copied::set);
        wizard.setTerminalOpener((tab, command) -> {
            typed.set(command);
            return true;
        });
        wizard.setStdioCommandSource(() -> STDIO_COMMAND);
        advanceToInstall(wizard);

        assertEquals("Waiting", text(wizard, "Install check state"));
        assertFalse(wizard.verified());
        assertEquals(0, probes.get());
        button(wizard, "Copy").doClick();
        assertTrue(copied.get().contains("install-shaft-agentic-tools"), copied.get());
        assertTrue(copied.get().contains("intellij-plugin"), copied.get());
        assertEquals(copied.get(), typed.get());
        assertFalse(wizard.verified());
        assertEquals(0, probes.get());
        wizard.primary().doClick();
        assertTrue(wizard.verified());
        assertTrue(wizard.userChecked());
        assertEquals("Verified", text(wizard, "Install check state"));
        assertEquals(1, probes.get());
        assertFalse(settings.mcpSetupComplete);
        assertFalse(settings.firstRunWizardCompleted);
    }

    @Test
    void failedCheckNamesACauseAndOneRecoveryWithoutARawExitCode() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "keep-me";
        FirstRunWizardPanel wizard = wizard(null, settings, () -> { },
                (client, runtime) -> ShaftMcpToolResult.failure("exit code 17"),
                new PrerequisitePlan.Snapshot(true, "3.9.8", true));
        wizard.setStdioCommandSource(() -> STDIO_COMMAND);
        advanceToInstall(wizard);
        wizard.primary().doClick();

        String cause = text(wizard, "Failure cause");
        assertEquals("Needs attention", text(wizard, "Install check state"));
        assertFalse(cause.toLowerCase().contains("exit"));
        assertFalse(wizard.verified());
        assertEquals("Retry", wizard.primary().getText());
        assertEquals(1, visibleDefaultButtons(wizard));
        assertEquals("keep-me", settings.mcpCommand);
        assertFalse(settings.mcpSetupComplete);
    }

    @Test
    void blankStdioCommandDoesNotVerifyEvenWhenTheProbeSucceeds() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "";
        FirstRunWizardPanel wizard = wizard(null, settings, () -> { },
                (client, runtime) -> ShaftMcpToolResult.success("ok"),
                new PrerequisitePlan.Snapshot(true, "3.9.8", true));
        wizard.setStdioCommandSource(() -> "");
        advanceToInstall(wizard);
        wizard.primary().doClick();

        String cause = text(wizard, "Failure cause");
        assertFalse(wizard.verified());
        assertFalse(settings.mcpSetupComplete);
        assertFalse(settings.firstRunWizardCompleted);
        assertEquals("", settings.mcpCommand);
        assertEquals("Needs attention", text(wizard, "Install check state"));
        assertFalse(cause.toLowerCase().contains("exit"));
    }

    @Test
    void passingCheckStaysOnTheWizardUntilStepFivePersistsReadiness() {
        AtomicInteger finished = new AtomicInteger();
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "stale";
        FirstRunWizardPanel wizard = readyWizard(settings, finished::incrementAndGet);
        wizard.setStdioCommandSource(() -> STDIO_COMMAND);
        advanceToInstall(wizard);
        wizard.primary().doClick();

        assertTrue(wizard.verified());
        assertEquals(4, wizard.step());
        assertFalse(settings.mcpSetupComplete);
        assertFalse(settings.firstRunWizardCompleted);
        assertEquals("stale", settings.mcpCommand);
        wizard.primary().doClick();
        assertEquals(5, wizard.step());
        assertFalse(settings.mcpSetupComplete);
        assertFalse(settings.firstRunWizardCompleted);
        wizard.primary().doClick();
        assertTrue(settings.mcpSetupComplete);
        assertTrue(settings.agentLaneReady);
        assertTrue(settings.firstRunWizardCompleted);
        assertEquals(STDIO_COMMAND, settings.mcpCommand);
        assertEquals(1, finished.get());
    }

    @Test
    void skipLeavesSetupIncompleteAndDoesNotWipeChatOrCredentials() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantFamily = "CODEX";
        settings.mcpCommand = "keep-me";
        AtomicInteger saves = new AtomicInteger();
        AtomicInteger sessions = new AtomicInteger();
        FirstRunWizardPanel wizard = wizard(null, settings, () -> sessions.incrementAndGet(),
                (client, runtime) -> ShaftMcpToolResult.failure("no"),
                new PrerequisitePlan.Snapshot(false, "", false));
        wizard.setSecretStore((name, value) -> saves.incrementAndGet());

        button(wizard, "Skip").doClick();

        assertFalse(settings.firstRunWizardCompleted);
        assertEquals("keep-me", settings.mcpCommand);
        assertEquals("CODEX", settings.assistantFamily);
        assertEquals(0, saves.get());
        assertEquals(0, sessions.get());
        assertTrue(text(wizard, "Skip notice").contains("unavailable"));
    }

    @Test
    void apiKeyAppearsOnlyForTheAgentThatNeedsOne() {
        FirstRunWizardPanel wizard = readyWizard(new ShaftSettingsState.Settings(), () -> { });
        advanceToAgent(wizard);
        assertNotNull(wizard.selectedAgent());
        assertFalse(field(wizard, "API key").isVisible());
        button(wizard, "Use a different agent").doClick();
        wizard.selectAgent(AssistantAgentRoute.GEMINI_INTELLIJ);
        assertTrue(field(wizard, "API key").isVisible());
        wizard.selectAgent(AssistantAgentRoute.CODEX_CLI);
        assertFalse(field(wizard, "API key").isVisible());
    }

    @Test
    void openAssistantOrRecordMarksTheWizardComplete() {
        AtomicInteger finished = new AtomicInteger();
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "stale";
        FirstRunWizardPanel wizard = readyWizard(settings, finished::incrementAndGet);
        wizard.setStdioCommandSource(() -> STDIO_COMMAND);
        advanceToSuccess(wizard);
        assertEquals(0, settings.uxLiteResponses);
        wizard.primary().doClick();
        assertTrue(settings.firstRunWizardCompleted);
        assertTrue(settings.mcpSetupComplete);
        assertTrue(settings.agentLaneReady);
        assertEquals(STDIO_COMMAND, settings.mcpCommand);
        assertEquals(1, finished.get());

        ShaftSettingsState.Settings again = new ShaftSettingsState.Settings();
        again.mcpCommand = "stale";
        AtomicInteger recorded = new AtomicInteger();
        FirstRunWizardPanel record = readyWizard(again, recorded::incrementAndGet);
        record.setStdioCommandSource(() -> STDIO_COMMAND);
        advanceToSuccess(record);
        button(record, "Record a sample flow").doClick();
        assertTrue(again.firstRunWizardCompleted);
        assertTrue(again.mcpSetupComplete);
        assertTrue(again.agentLaneReady);
        assertEquals(STDIO_COMMAND, again.mcpCommand);
        assertEquals(1, recorded.get());
    }

    @Test
    void headingAndPrimaryFitTheNarrowToolWindow() {
        FirstRunWizardPanel wizard = readyWizard(new ShaftSettingsState.Settings(), () -> { });
        wizard.setSize(new Dimension(360, 700));
        wizard.doLayout();
        layout(wizard);
        JComponent heading = named(wizard, "SHAFT setup step heading", JComponent.class);
        JButton primary = wizard.primary();
        assertNotNull(heading);
        assertTrue(heading.getWidth() > 0 && primary.getWidth() > 0);
        assertTrue(heading.getX() + heading.getWidth() <= wizard.getWidth());
        assertTrue(primary.getX() + primary.getWidth() <= wizard.getWidth());
    }

    @Test
    void startupNoticeSourceDoesNotBuildTheWizardOrADialog() throws Exception {
        String activity = Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/settings/ShaftFirstRunNoticeActivity.java"));
        String plugin = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));
        assertTrue(activity.contains("executeOnPooledThread"));
        assertFalse(activity.contains("DialogWrapper"));
        assertFalse(activity.contains("FirstRunWizardPanel"));
        assertTrue(plugin.contains(
                "com.shaft.intellij.settings.ShaftFirstRunNoticeActivity"));
        ShaftSettingsState.Settings incomplete = new ShaftSettingsState.Settings();
        AtomicInteger posted = new AtomicInteger();
        ShaftFirstRunNoticeActivity.maybeNotify(incomplete, () -> posted.incrementAndGet());
        assertEquals(1, posted.get());
        incomplete.firstRunWizardCompleted = true;
        ShaftFirstRunNoticeActivity.maybeNotify(incomplete, () -> posted.incrementAndGet());
        assertEquals(1, posted.get());
    }

    private static void advanceToInstall(FirstRunWizardPanel wizard) {
        wizard.primary().doClick();
        if (!wizard.primary().isEnabled()) {
            button(wizard, "Continue anyway").doClick();
        }
        wizard.primary().doClick();
        wizard.primary().doClick();
        assertEquals(4, wizard.step());
    }

    private static void advanceToAgent(FirstRunWizardPanel wizard) {
        wizard.primary().doClick();
        if (!wizard.primary().isEnabled()) {
            button(wizard, "Continue anyway").doClick();
        }
        wizard.primary().doClick();
        assertEquals(3, wizard.step());
    }

    private static void advanceToSuccess(FirstRunWizardPanel wizard) {
        advanceToInstall(wizard);
        wizard.setStdioCommandSource(() -> STDIO_COMMAND);
        wizard.primary().doClick();
        assertTrue(wizard.verified());
        wizard.primary().doClick();
        assertEquals(5, wizard.step());
    }

    private static FirstRunWizardPanel readyWizard(ShaftSettingsState.Settings settings, Runnable onComplete) {
        return wizard(null, settings, onComplete, (client, runtime) -> ShaftMcpToolResult.success("ok"),
                new PrerequisitePlan.Snapshot(true, "3.9.8", true));
    }

    private static FirstRunWizardPanel wizard(Path root, ShaftSettingsState.Settings settings, Runnable onComplete,
                                              FirstRunWizardPanel.InstallProbe probe, PrerequisitePlan.Snapshot snapshot) {
        return new FirstRunWizardPanel(project(root), settings, onComplete, probe, () -> snapshot);
    }

    private static Project project(Path root) {
        String base = root == null ? "" : root.toString();
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "toString" -> "wizard-test";
                    case "getBasePath" -> base;
                    case "getName" -> "wizard-test";
                    case "isDisposed" -> false;
                    default -> defaultValue(method.getReturnType());
                });
    }

    private static Object defaultValue(Class<?> type) {
        if (type == boolean.class) {
            return false;
        }
        if (type == int.class) {
            return 0;
        }
        if (type == long.class) {
            return 0L;
        }
        return null;
    }

    private static int visibleDefaultButtons(Component root) {
        List<AbstractButton> found = new ArrayList<>();
        walkDefaults(root, root, found);
        return found.size();
    }

    private static void walkDefaults(Component component, Component root, List<AbstractButton> found) {
        if (component instanceof JButton button && button.isDefaultCapable() && shown(button, root)) {
            found.add(button);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                walkDefaults(child, root, found);
            }
        }
    }

    private static boolean shown(Component component, Component root) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
            if (current == root) {
                return true;
            }
        }
        return false;
    }

    private static void layout(Component component) {
        component.doLayout();
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                layout(child);
            }
        }
    }

    private static boolean contains(Component component, String expected) {
        if (component instanceof JLabel label && label.getText() != null && label.getText().contains(expected)) {
            return true;
        }
        if (component instanceof AbstractButton button && button.getText() != null && button.getText().contains(expected)) {
            return true;
        }
        if (component.getAccessibleContext() != null && expected.equals(component.getAccessibleContext().getAccessibleName())) {
            return true;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                if (contains(child, expected)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static String text(Component root, String name) {
        JLabel label = named(root, name, JLabel.class);
        return label == null ? "" : label.getText();
    }

    private static JButton button(Component root, String name) {
        return named(root, name, JButton.class);
    }

    private static JTextComponent field(Component root, String name) {
        return named(root, name, JTextComponent.class);
    }

    private static <T extends Component> T named(Component component, String name, Class<T> type) {
        if (type.isInstance(component) && component.getAccessibleContext() != null
                && name.equals(component.getAccessibleContext().getAccessibleName())) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = named(child, name, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }
}
