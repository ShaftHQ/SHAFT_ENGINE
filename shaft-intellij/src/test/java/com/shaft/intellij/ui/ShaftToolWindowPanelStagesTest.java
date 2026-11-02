package com.shaft.intellij.ui;

import com.shaft.intellij.java.JavaTargetContext;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JComboBox;
import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #5942 / #5957: three stages with live record as the Automation canvas default.
 */
class ShaftToolWindowPanelStagesTest {
    @Test
    void defaultUiExposesThreeStagesAndARecorderWithoutExpertMode() {
        ShaftToolWindowPanel panel = newPanel(false);
        assertEquals(List.of(
                ShaftToolWindowPanel.SURFACE_AGENT,
                ShaftToolWindowPanel.SURFACE_WORKFLOW,
                ShaftToolWindowPanel.SURFACE_LOG), stageLabels(panel));
        assertTrue(panel.workflowSelector().isVisible());
        assertNotNull(panel.assistantPanel());
        assertEquals(ShaftToolWindowPanel.SURFACE_AGENT, panel.selectedStageLabel());
    }

    @Test
    void expertModeAddsMoreNotAFourthProductStageName() {
        ShaftToolWindowPanel panel = newPanel(true);
        assertEquals(List.of(
                ShaftToolWindowPanel.SURFACE_AGENT,
                ShaftToolWindowPanel.SURFACE_WORKFLOW,
                ShaftToolWindowPanel.SURFACE_LOG), stageLabels(panel));
        assertFalse(stageLabels(panel).contains("More"));
    }

    @Test
    void liveRecordIsAutomationDefaultWithoutExpertMode() {
        ShaftToolWindowPanel panel = newPanel(false);
        assertNotNull(panel.guidedWorkflowPanel());
        panel.workflowSelector().setSelectedIndex(1);
        assertEquals(ShaftToolWindowPanel.SURFACE_WORKFLOW, panel.selectedStageLabel());
        assertEquals(ShaftToolWindowPanel.SURFACE_WORKFLOW, panel.selectedSurfaceLabel());
        assertNotNull(findButton(panel.guidedWorkflowPanel(), "Start recording"));
        assertNotNull(findButton(panel.guidedWorkflowPanel(), "Pause recording"));
        assertNotNull(findButton(panel.guidedWorkflowPanel(), "Stop recording"));
        assertNotNull(findButton(panel.guidedWorkflowPanel(), "Clear recording"));
    }

    @Test
    void recordAtCaretSelectsAutomationRecorderWithoutExpertMode() {
        ShaftToolWindowPanel panel = newPanel(false);
        panel.startRecordingAtTarget(new JavaTargetContext(
                "src/test/java/LoginTest.java", "tests", "LoginTest", "logsIn"));
        assertEquals(ShaftToolWindowPanel.SURFACE_WORKFLOW, panel.selectedStageLabel());
    }


    @Test
    void reportingOverviewIsDefaultCanvasWithoutFivePrimaryTabs() {
        ShaftToolWindowPanel panel = newPanel(false);
        panel.workflowSelector().setSelectedIndex(2);
        assertEquals(ShaftToolWindowPanel.SURFACE_LOG, panel.selectedStageLabel());
        assertNotNull(panel.executionLogPanel());
    }

    private static ShaftToolWindowPanel newPanel(boolean expert) {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        settings.advancedUiEnabled = expert;
        return new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));
    }

    private static List<String> stageLabels(ShaftToolWindowPanel panel) {
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = panel.workflowSelector();
        List<String> labels = new ArrayList<>();
        for (int index = 0; index < selector.getItemCount(); index++) {
            labels.add(selector.getItemAt(index).label());
        }
        return labels;
    }

    private static JButton findButton(Component component, String accessibleName) {
        if (component instanceof JButton button
                && accessibleName.equals(button.getAccessibleContext().getAccessibleName())) {
            return button;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JButton found = findButton(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }
}
