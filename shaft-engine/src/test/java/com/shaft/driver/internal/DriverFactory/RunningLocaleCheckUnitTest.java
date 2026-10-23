package com.shaft.driver.internal.DriverFactory;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SessionNotCreatedException;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class RunningLocaleCheckUnitTest {
    @Test
    public void halfSwitchedLocaleIsReported() {
        String reason = RunningLocaleCheck.mismatch("en", "US", "ar_SA");
        Assert.assertNotNull(reason);
        Assert.assertTrue(reason.contains("runs locale ar_SA") && reason.contains("asked for en_US"), reason);
    }

    @Test
    public void matchingOrUnknownLocalePasses() {
        Assert.assertNull(RunningLocaleCheck.mismatch("en", "US", "en_US"));
        Assert.assertNull(RunningLocaleCheck.mismatch("ar", "", "ar_EG"));
        Assert.assertNull(RunningLocaleCheck.mismatch("en", "US", "en-US"));
        Assert.assertNull(RunningLocaleCheck.mismatch("en", "US", null));
    }

    @Test
    public void verifyFailsFastOnMismatchAndSkipsWithoutLanguage() {
        JavascriptExecutor driver = mock(JavascriptExecutor.class);
        when(driver.executeScript(eq("mobile: deviceInfo"), any())).thenReturn(Map.of("locale", "ar_SA"));
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> RunningLocaleCheck.verify(driver, "en", "US"));
        Assert.assertTrue(e.getMessage().startsWith("Preflight: the Android device runs locale ar_SA"), e.getMessage());
        RunningLocaleCheck.verify(driver, "", "");
    }

    @Test
    public void driverWithoutDeviceInfoIsSkipped() {
        JavascriptExecutor driver = mock(JavascriptExecutor.class);
        when(driver.executeScript(eq("mobile: deviceInfo"), any())).thenThrow(new UnsupportedOperationException("no"));
        RunningLocaleCheck.verify(driver, "en", "US");
    }
}
