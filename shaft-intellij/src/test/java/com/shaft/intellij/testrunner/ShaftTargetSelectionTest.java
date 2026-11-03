package com.shaft.intellij.testrunner;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftTargetSelectionTest {
    @Test
    void blocksCloudMobileWithoutAppAlias() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.ANDROID);
        target.setExecution(ShaftTargetSelection.Execution.BROWSERSTACK);
        assertFalse(target.isValid());
        target.setAppAlias("checkout-debug");
        assertTrue(target.isValid());
        assertTrue(target.toVmArgs().stream().anyMatch(a -> a.contains("mobile_app=checkout-debug")));
    }

    @Test
    void blocksWebWithMobileDevice() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.WEB);
        target.setDeviceOs("ANDROID");
        assertFalse(target.isValid());
    }

    @Test
    void localAndroidRequiresDevice() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.ANDROID);
        target.setExecution(ShaftTargetSelection.Execution.LOCAL);
        assertFalse(target.isValid());
        target.setDeviceOs("ANDROID");
        assertTrue(target.isValid());
    }
}
