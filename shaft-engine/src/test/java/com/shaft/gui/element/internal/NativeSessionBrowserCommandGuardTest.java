package com.shaft.gui.element.internal;

import com.shaft.driver.SHAFT;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

/**
 * Issues #6827, #6828, #6829, #6831: failure-time collectors must not send browser-only WebDriver
 * commands (Get Title, Get URL, tag-name iframe probes, DOM scripts, CSS reads) to native app sessions.
 */
public class NativeSessionBrowserCommandGuardTest {
    private String originalPlatform;

    @BeforeMethod
    public void useNativeAndroid() {
        originalPlatform = SHAFT.Properties.platform.targetPlatform();
        SHAFT.Properties.platform.set().targetPlatform("Android");
        SHAFT.Properties.mobile.set().browserName("");
    }

    @AfterMethod(alwaysRun = true)
    public void restorePlatform() {
        SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
    }

    private static WebDriver scriptableDriver() {
        return mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
    }

    @Test(description = "failure evidence for a native session sends no title, iframe or DOM-script command")
    public void failureEvidenceSkipsBrowserOnlyCommands() {
        WebDriver driver = scriptableDriver();

        String json = FailureEvidence.render(By.id("submit"), driver, List.of(), new RuntimeException("boom"),
                Map.of("matchCount", 0), true);

        Assert.assertTrue(json.contains("matchCount"), json);
        verify(driver, never()).getTitle();
        verify(driver, never()).findElements(any(By.class));
        verify(driver, never()).switchTo();
        verify((JavascriptExecutor) driver, never()).executeScript(anyString(), any(Object[].class));
    }

    @Test(description = "actionability diagnostics for a native element read only native state")
    public void actionabilityDiagnosticsSkipWebOnlyReads() {
        WebDriver driver = scriptableDriver();
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.getRect()).thenReturn(new Rectangle(0, 0, 10, 10));

        Map<String, Object> diagnostics = ElementActionabilityDiagnostics.collect(By.id("submit"), driver,
                List.of(element), new RuntimeException("boom"));

        Assert.assertEquals(diagnostics.get("displayed"), true);
        verify(driver, never()).getCurrentUrl();
        verify(element, never()).getCssValue(anyString());
        verify(element, never()).getDomAttribute(anyString());
        verify((JavascriptExecutor) driver, never()).executeScript(anyString(), any(Object[].class));
        Mockito.verifyNoMoreInteractions(driver);
    }
}
