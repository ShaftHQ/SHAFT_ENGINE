package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the "act, don't silently no-op" half of issue #3552: in default mode
 * (advancedUiEnabled=false) the SHAFT tool window still has exactly one workflow view
 * (Assistant), and {@link ShaftToolWindowPanel#prefillAssistantPrompt(String)} must fill its
 * composer and select that tab -- never auto-send, mirroring the existing empty-state chip norm
 * ({@code ShaftAssistantPanel#emptyStateChip}).
 */
class ShaftAssistantPromptRoutingTest {
    @Test
    void prefillPromptFillsTheComposerAndFocusesItWithoutSending() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        panel.prefillPrompt("Diagnose my last failed test run");

        assertEquals("Diagnose my last failed test run", panel.promptText());
        assertTrue(panel.transcriptMarkdown().isBlank(),
                "prefillPrompt must never auto-send: the transcript stays empty until the user "
                        + "reviews and sends it themselves.");
    }

    @Test
    void prefillPromptOnBlankTextIsANoOp() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.prefillPrompt("existing draft");

        panel.prefillPrompt("");
        panel.prefillPrompt(null);

        assertEquals("existing draft", panel.promptText());
    }

    @Test
    void toolWindowPrefillAssistantPromptRoutesThroughTheAssistantPanelInDefaultMode() {
        // connectedMcpSettings() leaves advancedUiEnabled at its default (false), matching the
        // majority of users this gate-audit targets.
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        toolWindow.prefillAssistantPrompt("Heal my last failed test run");

        ShaftAssistantPanel assistantPanel = toolWindow.assistantPanel();
        assertNotNull(assistantPanel, "showMainView() must always build and retain the Assistant panel");
        assertEquals("Heal my last failed test run", assistantPanel.promptText());
        assertEquals("Assistant", selectedWorkflowLabel(toolWindow));
    }

    @Test
    void toolWindowPrefillAssistantPromptIsANoOpBeforeMcpIsConfigured() {
        // Before setup completes, showSetupView() is active and no ShaftAssistantPanel exists yet;
        // this must not throw -- the setup panel itself already explains what to do next.
        ShaftSettingsState.Settings unconfigured = new ShaftSettingsState.Settings();
        unconfigured.mcpCommand = "";
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), unconfigured);

        toolWindow.prefillAssistantPrompt("Diagnose my last failed test run");

        assertNull(toolWindow.assistantPanel());
    }

    private static String selectedWorkflowLabel(ShaftToolWindowPanel toolWindow) {
        Object selected = toolWindow.workflowSelector().getSelectedItem();
        return selected instanceof ShaftToolWindowPanel.WorkflowView view ? view.label() : "";
    }

    private static ShaftSettingsState.Settings connectedMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
    }

    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return "";
                        case "getName":
                            return "shaft-assistant-prompt-routing-test-project";
                        case "getService":
                            return null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive()) {
            return null;
        }
        return returnType == boolean.class ? false : 0;
    }
}
