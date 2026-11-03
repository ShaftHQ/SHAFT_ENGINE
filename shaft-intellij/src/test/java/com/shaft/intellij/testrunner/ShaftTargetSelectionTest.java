package com.shaft.intellij.testrunner;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
        assertTrue(target.validationErrors().get(0).contains("mobile device"));
    }

    @Test
    void blocksWebWithAppAlias() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.WEB);
        target.setAppAlias("not-for-web");
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
        List<String> args = target.toVmArgs();
        assertTrue(args.contains("-DtargetOperatingSystem=ANDROID"));
        assertTrue(args.contains("-Dmobile_automationName=UIAutomator2"));
        assertTrue(args.contains("-DexecutionAddress=localhost:4723"));
    }

    @Test
    void localIosVmArgs() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.IOS);
        target.setExecution(ShaftTargetSelection.Execution.LOCAL);
        target.setDeviceOs("iOS");
        assertTrue(target.isValid());
        List<String> args = target.toVmArgs();
        assertTrue(args.contains("-Dmobile_automationName=XCUITest"));
        assertTrue(args.stream().anyMatch(a -> a.contains("executionAddress=localhost:4723")));
    }

    @Test
    void webLocalAndCloudExecutionArgs() {
        ShaftTargetSelection web = new ShaftTargetSelection();
        web.setPlatform(ShaftTargetSelection.Platform.WEB);
        web.setExecution(ShaftTargetSelection.Execution.LOCAL);
        assertTrue(web.isValid());
        assertTrue(web.toVmArgs().contains("-DexecutionAddress=local"));

        ShaftTargetSelection cloud = new ShaftTargetSelection();
        cloud.setPlatform(ShaftTargetSelection.Platform.WEB);
        cloud.setExecution(ShaftTargetSelection.Execution.LAMBDATEST);
        cloud.setDeviceOs("Windows 11");
        cloud.setRetryCount(2);
        assertTrue(cloud.isValid());
        List<String> args = cloud.toVmArgs();
        assertTrue(args.contains("-DexecutionAddress=lambdatest"));
        assertTrue(args.contains("-DretryCount=2"));
        assertTrue(args.stream().anyMatch(a -> a.contains("Windows 11")));
    }

    @Test
    void sauceLabsMobileNeedsApp() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.IOS);
        target.setExecution(ShaftTargetSelection.Execution.SAUCELABS);
        assertFalse(target.isValid());
        target.setAppAlias("ios-app");
        assertTrue(target.isValid());
    }

    @Test
    void nullSafeSettersAndLabels() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setDeviceOs(null);
        target.setAppAlias(null);
        target.setRetryCount(-3);
        assertEquals("", target.getDeviceOs());
        assertEquals("", target.getAppAlias());
        assertEquals(0, target.getRetryCount());
        assertEquals("Web", ShaftTargetSelection.Platform.WEB.label());
        assertEquals("BrowserStack", ShaftTargetSelection.Execution.BROWSERSTACK.label());
        assertEquals(ShaftTargetSelection.INHERIT, ShaftTargetSelection.INHERIT);
    }


    @Test
    void toStringUsesLabel() {
        assertEquals(ShaftTargetSelection.Platform.WEB.label(), ShaftTargetSelection.Platform.WEB.toString());
        assertEquals(ShaftTargetSelection.Execution.LOCAL.label(), ShaftTargetSelection.Execution.LOCAL.toString());
    }
}
