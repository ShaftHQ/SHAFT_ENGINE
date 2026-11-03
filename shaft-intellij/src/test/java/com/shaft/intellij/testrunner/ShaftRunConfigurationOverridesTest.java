package com.shaft.intellij.testrunner;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftRunConfigurationOverridesTest {
    @Test
    void targetDefaultsWhenNull() {
        ShaftRunConfigurationOverrides overrides = new ShaftRunConfigurationOverrides();
        overrides.setTarget(null);
        assertNotNull(overrides.getTarget());
        overrides.setEnabled(true);
        overrides.setBrowser("chrome");
        overrides.setHeadless(true);
        overrides.setExtraVmArgs("-Dx=1");
        ShaftTargetSelection t = new ShaftTargetSelection();
        t.setPlatform(ShaftTargetSelection.Platform.ANDROID);
        overrides.setTarget(t);
        assertEquals(ShaftTargetSelection.Platform.ANDROID, overrides.getTarget().getPlatform());
        assertTrue(overrides.isEnabled());
        assertTrue(overrides.isHeadless());
        assertEquals("chrome", overrides.getBrowser());
        assertEquals("-Dx=1", overrides.getExtraVmArgs());
        assertFalse(new ShaftRunConfigurationOverrides().isEnabled());
    }
}
