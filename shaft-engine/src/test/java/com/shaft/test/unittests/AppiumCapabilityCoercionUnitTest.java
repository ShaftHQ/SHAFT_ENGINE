package com.shaft.test.unittests;

import com.shaft.driver.internal.DriverFactory.OptionsManager;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Regression for #7143: string-typed Appium capabilities must stay JSON strings even when numeric.
 */
public class AppiumCapabilityCoercionUnitTest {

    @Test
    public void numericPlatformVersionStaysAString() {
        Assert.assertEquals(OptionsManager.coerceAppiumCapabilityValue("appium:platformVersion", "16"), "16");
    }

    @Test
    public void integerLookingPlatformVersionIsAJsonStringInTheSessionPayload() {
        String json = OptionsManager.appiumSessionCapabilityJson("appium:platformVersion", "16").replaceAll("\\s+", "");
        Assert.assertTrue(json.contains("\"appium:platformVersion\":\"16\""), json);
        String timeout = OptionsManager.appiumSessionCapabilityJson("appium:newCommandTimeout", "600").replaceAll("\\s+", "");
        Assert.assertTrue(timeout.contains("\"appium:newCommandTimeout\":600"), timeout);
    }

    @Test
    public void numericUdidStaysAString() {
        Assert.assertEquals(OptionsManager.coerceAppiumCapabilityValue("appium:udid", " 12345 "), "12345");
    }

    @Test
    public void numericNonStringCapabilityBecomesInteger() {
        Assert.assertEquals(OptionsManager.coerceAppiumCapabilityValue("appium:newCommandTimeout", "600"), 600);
    }

    @Test
    public void booleanCapabilityBecomesBoolean() {
        Assert.assertEquals(OptionsManager.coerceAppiumCapabilityValue("appium:noReset", "TRUE"), Boolean.TRUE);
    }

    @Test
    public void overflowingNumberStaysAString() {
        Assert.assertEquals(OptionsManager.coerceAppiumCapabilityValue("appium:someLimit", "99999999999"), "99999999999");
    }

    @Test
    public void textCapabilityIsUnchanged() {
        Assert.assertEquals(OptionsManager.coerceAppiumCapabilityValue("appium:automationName", "UiAutomator2"), "UiAutomator2");
    }
}
