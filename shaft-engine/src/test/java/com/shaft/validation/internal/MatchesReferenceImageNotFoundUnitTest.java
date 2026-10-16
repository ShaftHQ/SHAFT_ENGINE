package com.shaft.validation.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/** #7052: matchesReferenceImage() reports a missing element instead of "expected false, but found false". */
public class MatchesReferenceImageNotFoundUnitTest {
    private static final By LOCATOR = By.id("missing");

    @Test
    public void reportsTheElementNotFoundCauseWhenTheElementNeverAppeared() {
        Object actual = ValidationsHelper.describeMatchesActual(false, false,
                List.of(new NoSuchElementException("no such element: Unable to locate element")), LOCATOR);
        Assert.assertTrue(actual instanceof String, "a missing element must be reported as a cause, not a boolean");
        String message = (String) actual;
        Assert.assertTrue(message.contains("Element not found"), message);
        Assert.assertTrue(message.contains(LOCATOR.toString()), message);
        Assert.assertTrue(message.contains("NoSuchElementException"), message);
        Assert.assertTrue(message.contains("Unable to locate element"), message);
    }

    @Test
    public void keepsThePassingResultUnchanged() {
        Assert.assertEquals(ValidationsHelper.describeMatchesActual(true, true, List.of(), LOCATOR), Boolean.TRUE);
        // a stale lookup failure from an earlier retry never overrides a passing comparison
        Assert.assertEquals(ValidationsHelper.describeMatchesActual(true, true,
                List.of(new NoSuchElementException("earlier retry")), LOCATOR), Boolean.TRUE);
    }

    @Test
    public void keepsTheImageMismatchResultWhenTheElementWasFound() {
        Assert.assertEquals(ValidationsHelper.describeMatchesActual(false, false, List.of(), LOCATOR), Boolean.FALSE);
    }

    private static WebDriver driverThrowing(RuntimeException failure) {
        WebDriver driver = mock(WebDriver.class);
        when(driver.findElement(LOCATOR)).thenThrow(failure);
        return driver;
    }

    @Test
    public void captureRecordsAndRethrowsAMissingElement() {
        List<RuntimeException> failure = new ArrayList<>();
        NoSuchElementException missing = new NoSuchElementException("gone");
        Assert.assertThrows(NoSuchElementException.class,
                () -> ValidationsHelper.captureElementScreenshot(driverThrowing(missing), LOCATOR, failure));
        Assert.assertEquals(failure, List.of(missing));
    }

    @Test
    public void captureTreatsZeroWidthAsNotFound() {
        List<RuntimeException> failure = new ArrayList<>();
        Assert.assertThrows(NoSuchElementException.class, () -> ValidationsHelper.captureElementScreenshot(
                driverThrowing(new WebDriverException("Cannot take screenshot with 0 width.")), LOCATOR, failure));
        Assert.assertTrue(failure.getFirst() instanceof NoSuchElementException);
    }

    @Test
    public void captureRetriesOnFirefoxTransientErrorAndRethrowsOthers() {
        List<RuntimeException> failure = new ArrayList<>();
        Assert.assertEquals(ValidationsHelper.captureElementScreenshot(
                driverThrowing(new WebDriverException("NS_ERROR_FAILURE")), LOCATOR, failure).length, 0);
        Assert.assertThrows(WebDriverException.class, () -> ValidationsHelper.captureElementScreenshot(
                driverThrowing(new WebDriverException("session deleted")), LOCATOR, failure));
        Assert.assertTrue(failure.isEmpty());
    }

    @Test
    public void captureClearsAnEarlierFailureOnceTheElementIsFound() {
        WebElement element = mock(WebElement.class);
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[]{7});
        WebDriver driver = mock(WebDriver.class);
        when(driver.findElement(LOCATOR)).thenReturn(element);
        List<RuntimeException> failure = new ArrayList<>(List.of(new NoSuchElementException("earlier")));
        Assert.assertEquals(ValidationsHelper.captureElementScreenshot(driver, LOCATOR, failure), new byte[]{7});
        Assert.assertTrue(failure.isEmpty());
    }

    @Test
    public void logsReferenceImageStateWithoutFailing() {
        ValidationsHelper.logReferenceImageState(null);
        ValidationsHelper.logReferenceImageState(new byte[0]);
        ValidationsHelper.logReferenceImageState(new byte[]{1});
    }
}
