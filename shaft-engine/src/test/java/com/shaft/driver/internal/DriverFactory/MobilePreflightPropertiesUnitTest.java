package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class MobilePreflightPropertiesUnitTest {
    @AfterMethod(alwaysRun = true)
    public void restore() {
        SHAFT.Properties.mobile.set().preflight(true).preflightResolveHosts("").verifyRunningLocale(true);
    }

    @Test
    public void preflightPropertiesHaveSafeDefaultsAndCanBeOverridden() {
        Assert.assertTrue(SHAFT.Properties.mobile.preflight());
        Assert.assertEquals(SHAFT.Properties.mobile.preflightResolveHosts(), "");
        Assert.assertTrue(SHAFT.Properties.mobile.verifyRunningLocale());
        SHAFT.Properties.mobile.set().preflight(false).preflightResolveHosts("api.example.com").verifyRunningLocale(false);
        Assert.assertFalse(SHAFT.Properties.mobile.preflight());
        Assert.assertEquals(SHAFT.Properties.mobile.preflightResolveHosts(), "api.example.com");
        Assert.assertFalse(SHAFT.Properties.mobile.verifyRunningLocale());
    }
}
