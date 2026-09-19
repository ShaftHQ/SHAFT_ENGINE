package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JComponent;
import java.awt.Component;
import java.awt.Container;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AutomationStagePanelTest {
    @Test
    void liveRecordExposesStartPauseStopClearWithoutProject() {
        AutomationStagePanel panel = newPanel();
        assertEquals(AutomationStagePanel.ACCESSIBLE_NAME,
                panel.getAccessibleContext().getAccessibleName());
        assertEquals(AutomationStagePanel.LIVE_RECORD_TAB, panel.surfaces().getTitleAt(0));
        GuidedWorkflowPanel guided = panel.guidedWorkflowPanel();
        assertNotNull(findButton(guided, "Start recording"));
        assertNotNull(findButton(guided, "Pause recording"));
        assertNotNull(findButton(guided, "Stop recording"));
        assertNotNull(findButton(guided, "Clear recording"));
        assertNotNull(guided.targetUrlField());
        assertNotNull(guided.intentField());
    }

    @Test
    void readyPackPrefillsUrlAndIntent() {
        AutomationStagePanel panel = newPanel();
        panel.applyReadyPackPrefill("https://shop.example/checkout", "valid payment places the order");
        assertEquals("https://shop.example/checkout", panel.guidedWorkflowPanel().targetUrlField().getText());
        assertEquals("valid payment places the order", panel.guidedWorkflowPanel().intentField().getText());
    }

    @Test
    void handoffJsonPrefillsAutomation() {
        AutomationStagePanel panel = newPanel();
        panel.applyHandoffPrefillJson("""
                {
                  "status": "ready",
                  "optionalUrl": "https://shop.example/checkout",
                  "automationPrefill": {
                    "url": "https://shop.example/checkout",
                    "intent": "valid payment places the order"
                  }
                }
                """);
        assertEquals("https://shop.example/checkout", panel.guidedWorkflowPanel().targetUrlField().getText());
        assertTrue(panel.guidedWorkflowPanel().intentField().getText().contains("valid payment"));
    }

    @Test
    void pauseTogglesWithoutCrashing() {
        AutomationStagePanel panel = newPanel();
        GuidedWorkflowPanel guided = panel.guidedWorkflowPanel();
        assertFalse(guided.recordingPausedForTests());
        findButton(guided, "Pause recording").doClick();
        assertTrue(guided.recordingPausedForTests());
        findButton(guided, "Pause recording").doClick();
        assertFalse(guided.recordingPausedForTests());
    }


    @Test
    void locatorPickerTabIsAvailableOnAutomationCanvas() {
        AutomationStagePanel panel = newPanel();
        assertEquals(AutomationStagePanel.LOCATOR_PICKER_TAB, panel.surfaces().getTitleAt(1));
        assertEquals(LocatorPlaygroundPanel.ACCESSIBLE_NAME,
                panel.locatorPlaygroundPanel().getAccessibleContext().getAccessibleName());
        panel.showLocatorPicker();
        assertEquals(1, panel.surfaces().getSelectedIndex());
    }

    private static AutomationStagePanel newPanel() {
        return new AutomationStagePanel(null, (tool, args) -> {
        }, new ShaftSettingsState.Settings());
    }

    private static JButton findButton(Component component, String accessibleName) {
        if (component instanceof JButton button
                && accessibleName.equals(accessibleName(button))) {
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

    private static String accessibleName(JComponent component) {
        String name = component.getAccessibleContext().getAccessibleName();
        return name == null ? "" : name;
    }
}
