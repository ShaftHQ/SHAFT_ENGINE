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

    @Test
    void roundTripsMobileTarget() {
        ShaftRunConfigurationOverridesPanel panel = new ShaftRunConfigurationOverridesPanel();
        ShaftRunConfigurationOverrides enabled = new ShaftRunConfigurationOverrides();
        enabled.setEnabled(true);
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.ANDROID);
        target.setExecution(ShaftTargetSelection.Execution.BROWSERSTACK);
        target.setDeviceOs("ANDROID");
        target.setAppAlias("app-1");
        target.setRetryCount(1);
        enabled.setTarget(target);
        panel.setState(enabled);
        ShaftRunConfigurationOverrides roundTrip = panel.getState();
        assertEquals(ShaftTargetSelection.Platform.ANDROID, roundTrip.getTarget().getPlatform());
        assertEquals(ShaftTargetSelection.Execution.BROWSERSTACK, roundTrip.getTarget().getExecution());
        assertEquals("ANDROID", roundTrip.getTarget().getDeviceOs());
        assertEquals("app-1", roundTrip.getTarget().getAppAlias());
        assertEquals(1, roundTrip.getTarget().getRetryCount());
    }


    @Test
    void coversBrowserVariantsAndDisabled() {
        ShaftRunConfigurationOverridesPanel panel = new ShaftRunConfigurationOverridesPanel();
        for (String browser : new String[] {"chrome", "firefox", "edge", "safari", ""}) {
            ShaftRunConfigurationOverrides o = new ShaftRunConfigurationOverrides();
            o.setEnabled(true);
            o.setBrowser(browser);
            o.setHeadless(false);
            o.setExtraVmArgs("");
            panel.setState(o);
            assertEquals(browser, panel.getState().getBrowser());
        }
        ShaftRunConfigurationOverrides off = new ShaftRunConfigurationOverrides();
        off.setEnabled(false);
        off.setBrowser("chrome");
        panel.setState(off);
        assertFalse(panel.getState().isEnabled());
    }
}
