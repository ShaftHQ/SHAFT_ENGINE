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
                ShaftToolWindowPanel.STAGE_DESIGN,
                ShaftToolWindowPanel.STAGE_AUTOMATION,
                ShaftToolWindowPanel.STAGE_REPORTING), stageLabels(panel));
        assertTrue(panel.workflowSelector().isVisible());
        assertNotNull(panel.recorderPanel());
        assertNotNull(panel.assistantPanel());
        assertNotNull(panel.designStagePanel());
        assertNotNull(panel.designStagePanel().storyArea());
        assertNotNull(panel.designStagePanel().ingestButton());
        assertNotNull(panel.designStagePanel().analyzeButton());
        assertNotNull(panel.designStagePanel().gherkinButton());
        assertFalse(panel.designStagePanel().gherkinButton().isEnabled());
        assertEquals(ShaftToolWindowPanel.STAGE_DESIGN, panel.selectedStageLabel());
    }

    @Test
    void expertModeAddsMoreNotAFourthProductStageName() {
        ShaftToolWindowPanel panel = newPanel(true);
        assertEquals(List.of(
                ShaftToolWindowPanel.STAGE_DESIGN,
                ShaftToolWindowPanel.STAGE_AUTOMATION,
                ShaftToolWindowPanel.STAGE_REPORTING,
                "More"), stageLabels(panel));
    }

    @Test
    void liveRecordIsAutomationDefaultWithoutExpertMode() {
        ShaftToolWindowPanel panel = newPanel(false);
        assertNotNull(panel.automationStagePanel());
        assertNotNull(panel.guidedWorkflowPanel());
        assertEquals(AutomationStagePanel.ACCESSIBLE_NAME,
                panel.automationStagePanel().getAccessibleContext().getAccessibleName());
        panel.workflowSelector().setSelectedIndex(1);
        assertEquals(ShaftToolWindowPanel.STAGE_AUTOMATION, panel.selectedStageLabel());
        assertEquals(AutomationStagePanel.LIVE_RECORD_TAB, panel.selectedSurfaceLabel());
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
        assertEquals(ShaftToolWindowPanel.STAGE_AUTOMATION, panel.selectedStageLabel());
        assertEquals(AutomationStagePanel.LIVE_RECORD_TAB, panel.selectedSurfaceLabel());
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
