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
            "Analysis & Design", "Reporting & Analytics", "Visual Baselines");

    @Test
    void agentIsTheDefaultAndRemovedSurfacesAreNotSelectable() throws Exception {
        ShaftToolWindowPanel panel = readyPanel();
        assertEquals(ShaftToolWindowPanel.SURFACE_AGENT, panel.selectedStageLabel());
        String tree = treeText(panel);
        for (String removed : REMOVED) {
            assertFalse(tree.contains(removed), removed);
        }
        String plugin = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));
        for (String removed : REMOVED) {
            assertFalse(plugin.contains(removed), removed);
        }
        assertTrue(plugin.contains("Open Claude Code Terminal"));
    }

    @Test
    void setupNamesTheGapAndTheReadyState() {
        assertEquals("Install the agent runtime, then press Check.",
                SetupReadiness.message(false, true, true));
        assertEquals("Agent ready.", SetupReadiness.message(true, true, true));
        Project project = fakeProject();
        ShaftSettingsState.Settings missing = new ShaftSettingsState.Settings();
        ShaftMcpSetupPanel gap = new ShaftMcpSetupPanel(project, missing, () -> { }, (client, runtime) -> null);
        assertTrue(labelText(gap, "SHAFT MCP setup next step").contains("press Check"));
        ShaftSettingsState.Settings ready = new ShaftSettingsState.Settings();
        ready.mcpSetupComplete = true;
        ready.mcpCommand = "shaft-mcp";
        ShaftMcpSetupPanel done = new ShaftMcpSetupPanel(project, ready, () -> { }, (client, runtime) -> null);
        assertEquals("Agent ready.", labelText(done, "SHAFT MCP setup next step"));
    }

    @Test
    void assistantPromptQuestionAndApprovalStaySingle() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, new ShaftSettingsState.Settings());
        panel.submitReviewedPrompt("Check the login flow");
        assertTrue(panel.transcriptMarkdown().contains("Check the login flow"));
        panel.offerQuestion(List.of("Yes", "No"));
        findButton(panel, "Suggested answer: Yes").doClick();
        assertEquals("Yes", panel.promptText());
        SwingUtilities.invokeAndWait(() -> panel.offerApproval("capture_start"));
        JButton deny = findButton(panel, "Deny for capture_start");
        assertEquals(1, countButtons(panel, "Deny for capture_start"));
        SwingUtilities.invokeAndWait(deny::doClick);
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

    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "toString" -> "agent-first-test";
                    case "getBasePath" -> "";
                    case "getName" -> "agent-first";
                    default -> defaultValue(method.getReturnType());
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (returnType == boolean.class) {
            return false;
        }
        if (returnType == int.class) {
            return 0;
        }
        if (returnType == long.class) {
            return 0L;
        }
        return null;
    }

    private static ShaftToolWindowPanel readyPanel() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        return new ShaftToolWindowPanel(null, settings, (client, runtime) -> null,
                ShaftAssistantChatState.getInstance(null));
    }

    private static String labelText(Component root, String accessibleName) {
        JLabel label = find(root, accessibleName, JLabel.class);
        return label == null ? "" : label.getText();
    }

    private static JButton findButton(Component root, String accessibleName) {
        return find(root, accessibleName, JButton.class);
    }

    private static int countButtons(Component root, String accessibleName) {
        List<JButton> found = new ArrayList<>();
        collect(root, accessibleName, found);
        return found.size();
    }

    private static void collect(Component component, String accessibleName, List<JButton> found) {
        if (component instanceof JButton button
                && component.getAccessibleContext() != null
                && accessibleName.equals(component.getAccessibleContext().getAccessibleName())) {
            found.add(button);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collect(child, accessibleName, found);
            }
        }
    }

    private static <T extends Component> T find(Component component, String accessibleName, Class<T> type) {
        if (type.isInstance(component)
                && component.getAccessibleContext() != null
                && accessibleName.equals(component.getAccessibleContext().getAccessibleName())) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = find(child, accessibleName, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static String treeText(Component component) {
        StringBuilder text = new StringBuilder();
        appendTree(component, text);
        return text.toString();
    }

    private static void appendTree(Component component, StringBuilder text) {
        if (component == null) {
            return;
        }
        if (component instanceof JLabel label) {
            text.append(label.getText()).append(' ');
        }
        if (component.getAccessibleContext() != null
                && component.getAccessibleContext().getAccessibleName() != null) {
            text.append(component.getAccessibleContext().getAccessibleName()).append(' ');
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                appendTree(child, text);
            }
        }
    }
}
