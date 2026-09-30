package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.awt.Container;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentFirstUxTest {
    private static final List<String> REMOVED = List.of(
            "Analysis & Design", "Reporting & Analytics", "Visual Baselines", "Locator playground");

    @Test
    void agentIsTheDefaultAndRemovedSurfacesAreNotSelectable() throws Exception {
        ShaftToolWindowPanel panel = readyPanel();
        assertEquals(List.of("Agent", "Workflow", "Execution log"), labels(panel));
        assertEquals(ShaftToolWindowPanel.SURFACE_AGENT, panel.selectedStageLabel());
        String tree = treeText(panel);
        for (String removed : REMOVED) {
            assertFalse(tree.contains(removed), removed);
        }
        String plugin = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));
        String settings = Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/settings/ShaftSettingsConfigurable.java"));
        String notifications = Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/notifications/FailedRunDoctorNotifier.java"));
        for (String removed : REMOVED) {
            assertFalse(plugin.contains(removed), removed);
            assertFalse(settings.contains(removed), removed);
            assertFalse(notifications.contains(removed), removed);
        }
    }

    @Test
    void setupNamesTheGapAndTheReadyState() {
        assertEquals("Install the agent runtime, then press Check.",
                SetupReadiness.message(false, true, true));
        assertEquals("Agent ready.", SetupReadiness.message(true, true, true));
        Project project = fakeProject();
        ShaftSettingsState.Settings missing = new ShaftSettingsState.Settings();
        ShaftMcpSetupPanel gap = new ShaftMcpSetupPanel(project, missing, () -> { }, (c, r) -> null);
        assertTrue(text(gap, "SHAFT MCP setup next step").contains("press Check"));
        ShaftSettingsState.Settings ready = new ShaftSettingsState.Settings();
        ready.mcpSetupComplete = true;
        ready.mcpCommand = "shaft-mcp";
        ShaftMcpSetupPanel done = new ShaftMcpSetupPanel(project, ready, () -> { }, (c, r) -> null);
        assertEquals("Agent ready.", text(done, "SHAFT MCP setup next step"));
    }

    @Test
    void assistantPromptQuestionAndApprovalStaySingle() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, new ShaftSettingsState.Settings());
        panel.submitReviewedPrompt("Check the login flow");
        assertTrue(panel.transcriptMarkdown().contains("Check the login flow"));
        panel.offerQuestion(List.of("Yes", "No"));
        button(panel, "Suggested answer: Yes").doClick();
        assertEquals("Yes", panel.promptText());
        SwingUtilities.invokeAndWait(() -> panel.offerApproval("capture_start"));
        assertEquals(1, count(panel, "Deny for capture_start"));
        SwingUtilities.invokeAndWait(button(panel, "Deny for capture_start")::doClick);
        assertTrue(panel.transcriptMarkdown().contains("Denied"));
    }

    @Test
    void commandLandsInTheConsoleAndLogsDistinguishFailure() {
        ShaftToolWindowPanel panel = readyPanel();
        String command = "mvn -q -Dtest=AppTest test";
        panel.placeCommand(command);
        assertEquals(command, panel.placedCommand());
        assertTrue(panel.executionLogPanel().text().contains(command));
        panel.showLogChunk("BUILD SUCCESS", false);
        assertEquals("Succeeded", panel.executionLogPanel().state());
        panel.showLogChunk("BUILD FAILURE", true);
        assertEquals("Failed", panel.executionLogPanel().state());
        assertTrue(panel.executionLogPanel().text().contains("BUILD FAILURE"));
    }

    private static List<String> labels(ShaftToolWindowPanel panel) {
        List<String> labels = new ArrayList<>();
        var selector = panel.workflowSelector();
        for (int index = 0; index < selector.getItemCount(); index++) {
            labels.add(selector.getItemAt(index).label());
        }
        return labels;
    }

    private static ShaftToolWindowPanel readyPanel() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        return new ShaftToolWindowPanel(null, settings, (c, r) -> null,
                ShaftAssistantChatState.getInstance(null));
    }

    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "toString" -> "agent-first";
                    case "getBasePath" -> "";
                    case "getName" -> "agent-first";
                    default -> method.getReturnType() == boolean.class ? false
                            : method.getReturnType() == int.class ? 0
                            : method.getReturnType() == long.class ? 0L : null;
                });
    }

    private static String text(Component root, String name) {
        JLabel label = find(root, name, JLabel.class);
        return label == null ? "" : label.getText();
    }

    private static JButton button(Component root, String name) {
        return find(root, name, JButton.class);
    }

    private static int count(Component root, String name) {
        List<JButton> found = new ArrayList<>();
        walk(root, name, found);
        return found.size();
    }

    private static void walk(Component component, String name, List<JButton> found) {
        if (component instanceof JButton && component.getAccessibleContext() != null
                && name.equals(component.getAccessibleContext().getAccessibleName())) {
            found.add((JButton) component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                walk(child, name, found);
            }
        }
    }

    private static <T extends Component> T find(Component component, String name, Class<T> type) {
        if (type.isInstance(component) && component.getAccessibleContext() != null
                && name.equals(component.getAccessibleContext().getAccessibleName())) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = find(child, name, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static String treeText(Component component) {
        StringBuilder text = new StringBuilder();
        append(component, text);
        return text.toString();
    }

    private static void append(Component component, StringBuilder text) {
        if (component instanceof JLabel label) {
            text.append(label.getText()).append(' ');
        }
        if (component != null && component.getAccessibleContext() != null
                && component.getAccessibleContext().getAccessibleName() != null) {
            text.append(component.getAccessibleContext().getAccessibleName()).append(' ');
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                append(child, text);
            }
        }
    }
}
