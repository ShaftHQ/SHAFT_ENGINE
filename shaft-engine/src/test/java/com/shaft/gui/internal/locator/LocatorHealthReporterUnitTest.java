package com.shaft.gui.internal.locator;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class LocatorHealthReporterUnitTest {
    @BeforeMethod
    public void setup() {
        Properties.clearForCurrentThread();
        LocatorHealthReporter.reset();
        SHAFT.Properties.reporting.set()
                .locatorHealthReportEnabled(true)
                .slowLocatorThresholdMillis(100)
                .failOnLocatorHealthWarnings(false);
        SHAFT.Properties.paths.set().executionSummaryReport("target/locator-health-reporter/");
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        LocatorHealthReporter.reset();
        Properties.clearForCurrentThread();
    }

    @Test(description = "Locator health reporter aggregates lookup timing and warning metrics by locator")
    public void locatorHealthReporterAggregatesLookupMetricsByLocator() {
        LocatorHealthReporter.recordLookup(By.id("login"), 50, 1, 1, false, 0);
        LocatorHealthReporter.recordLookup(By.id("login"), 150, 3, 2, false, 1);
        LocatorHealthReporter.recordLookup(By.cssSelector(".missing"), 200, 4, 0, true, 0);

        String json = LocatorHealthReporter.buildSummaryJson();

        Assert.assertTrue(json.contains("\"locator\" : \"By.id: login\""));
        Assert.assertTrue(json.contains("\"lookupCount\" : 2"));
        Assert.assertTrue(json.contains("\"averageLookupMillis\" : 100.0"));
        Assert.assertTrue(json.contains("\"p95LookupMillis\" : 150"));
        Assert.assertTrue(json.contains("\"pollingAttempts\" : 4"));
        Assert.assertTrue(json.contains("\"multipleElementMatches\" : 1"));
        Assert.assertTrue(json.contains("\"staleElementRetries\" : 1"));
        Assert.assertTrue(json.contains("\"slowLookups\" : 1"));
        Assert.assertTrue(json.contains("\"timeoutCount\" : 1"));
    }

    @Test(description = "Element presence wait records locator health lookup metrics")
    public void elementPresenceWaitRecordsLocatorHealthMetrics() {
        SHAFT.Properties.reporting.set().captureElementName(false);
        By locator = By.id("ready");
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.when(driver.findElements(locator)).thenReturn(java.util.List.of(element));
        Mockito.when(element.getRect()).thenReturn(new Rectangle(1, 2, 3, 4));
        Mockito.when(element.getDomProperty("outerHTML")).thenReturn("<button id=\"ready\"></button>");
        Mockito.when(element.getDomProperty("innerHTML")).thenReturn("");

        java.util.List<Object> result = new ElementActionsHelper(true)
                .waitForElementPresence(driver, locator, false);

        Assert.assertEquals(result.getFirst(), 1);
        String json = LocatorHealthReporter.buildSummaryJson();
        Assert.assertTrue(json.contains("\"locator\" : \"By.id: ready\""));
        Assert.assertTrue(json.contains("\"lookupCount\" : 1"));
        Assert.assertTrue(json.contains("\"pollingAttempts\" : 1"));
    }

    @Test(description = "Locator health reporter tracks healing attempts, accepted recoveries, and rejected recoveries")
    public void locatorHealthReporterTracksHealingMetrics() {
        LocatorHealthReporter.recordHealingAttempt(By.cssSelector(".submit"), true);
        LocatorHealthReporter.recordHealingAttempt(By.cssSelector(".submit"), false);

        String json = LocatorHealthReporter.buildSummaryJson();

        Assert.assertTrue(json.contains("\"healingAttempts\" : 2"));
        Assert.assertTrue(json.contains("\"acceptedHealings\" : 1"));
        Assert.assertTrue(json.contains("\"rejectedHealings\" : 1"));
    }

    @Test(description = "Locator health reporter can fail the run when warnings are configured as fatal")
    public void locatorHealthReporterReturnsFailureWhenWarningsAreFatal() {
        SHAFT.Properties.reporting.set().failOnLocatorHealthWarnings(true);

        LocatorHealthReporter.recordLookup(By.id("slow"), 150, 2, 1, false, 0);

        AssertionError failure = LocatorHealthReporter.reportAndGetFailure();

        Assert.assertNotNull(failure);
        Assert.assertTrue(failure.getMessage().contains("Locator health warnings"));
    }
}
