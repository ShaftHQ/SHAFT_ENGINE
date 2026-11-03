package com.shaft.intellij.testrunner;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftRunConfigurationOverridesPanelTest {
    @Test
    void roundTripsStateAndTogglesFields() {
        ShaftRunConfigurationOverridesPanel panel = new ShaftRunConfigurationOverridesPanel();
        ShaftRunConfigurationOverrides empty = panel.getState();
        assertFalse(empty.isEnabled());
        assertEquals("", empty.getBrowser());

        ShaftRunConfigurationOverrides enabled = new ShaftRunConfigurationOverrides();
        enabled.setEnabled(true);
        enabled.setBrowser("firefox");
        enabled.setHeadless(true);
        enabled.setExtraVmArgs("-Dfoo=bar");
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.WEB);
        target.setExecution(ShaftTargetSelection.Execution.LOCAL);
        enabled.setTarget(target);
        panel.setState(enabled);

        ShaftRunConfigurationOverrides roundTrip = panel.getState();
        assertTrue(roundTrip.isEnabled());
        assertEquals("firefox", roundTrip.getBrowser());
        assertTrue(roundTrip.isHeadless());
        assertEquals("-Dfoo=bar", roundTrip.getExtraVmArgs());
        assertEquals(ShaftTargetSelection.Platform.WEB, roundTrip.getTarget().getPlatform());

        panel.setState(null);
        ShaftRunConfigurationOverrides cleared = panel.getState();
        assertFalse(cleared.isEnabled());
        assertEquals("", cleared.getBrowser());
        assertEquals(ShaftRunConfigurationOverridesPanel.INHERIT_BROWSER,
                ShaftRunOverridesLabels.PROJECT_DEFAULT);
    }
}
