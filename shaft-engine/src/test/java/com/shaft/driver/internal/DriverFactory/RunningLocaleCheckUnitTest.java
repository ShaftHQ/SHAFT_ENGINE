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
        String reason = RunningLocaleCheck.mismatch("en", "US", "ar_SA").orElseThrow();
        Assert.assertTrue(reason.contains("runs locale ar_SA") && reason.contains("asked for en_US"), reason);
    }

    @Test
    public void matchingOrUnknownLocalePasses() {
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "US", "en_US").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("ar", "", "ar_EG").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "US", "en-US").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "US", "").isEmpty());
    }

    @Test
    public void verifyFailsFastOnMismatchAndSkipsWithoutLanguage() {
        JavascriptExecutor driver = mock(JavascriptExecutor.class);
        when(driver.executeScript(eq("mobile: deviceInfo"), any())).thenReturn(Map.of("locale", "ar_SA"));
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> RunningLocaleCheck.verify(driver, "en", "US"));
        Assert.assertTrue(String.valueOf(e.getMessage()).contains("Preflight: the Android device runs locale ar_SA"), e.getMessage());
        RunningLocaleCheck.verify(driver, "", "");
    }

    @Test
    public void unknownRunningLocaleWarnsInsteadOfFailing() {
        JavascriptExecutor driver = mock(JavascriptExecutor.class);
        when(driver.executeScript(eq("mobile: deviceInfo"), any())).thenReturn(Map.of("model", "iPhone"));
        RunningLocaleCheck.verify(driver, "en", "US");
        when(driver.executeScript(eq("mobile: deviceInfo"), any())).thenReturn("not a map");
        RunningLocaleCheck.verify(driver, "en", "US");
    }

    @Test
    public void driverWithoutDeviceInfoIsSkipped() {
        JavascriptExecutor driver = mock(JavascriptExecutor.class);
        when(driver.executeScript(eq("mobile: deviceInfo"), any())).thenThrow(new UnsupportedOperationException("no"));
        RunningLocaleCheck.verify(driver, "en", "US");
    }

    @Test
    public void mismatchComparesLanguageAndRegionPrecisely() {
        RunningLocaleCheck.verify(mock(JavascriptExecutor.class), null, "US");
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "US", "").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "US", "null").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("EN", "us", "en-US").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "", "en_GB").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "US", "en").isEmpty());
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "US", "en_GB").orElseThrow().contains("asked for en_US"));
        Assert.assertTrue(RunningLocaleCheck.mismatch("en", "", "ar").orElseThrow().contains("asked for en."));
        JavascriptExecutor match = mock(JavascriptExecutor.class);
        when(match.executeScript(eq("mobile: deviceInfo"), any())).thenReturn(Map.of("locale", "en_US"));
        RunningLocaleCheck.verify(match, "en", "US");
    }
}
