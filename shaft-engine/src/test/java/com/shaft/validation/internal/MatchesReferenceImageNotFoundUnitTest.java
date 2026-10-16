package com.shaft.validation.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.concurrent.atomic.AtomicReference;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/** #7052: matchesReferenceImage() reports a missing element instead of "expected false, but found false". */
public class MatchesReferenceImageNotFoundUnitTest {
    private static final By LOCATOR = By.id("missing");

    @Test
    public void reportsTheElementNotFoundCauseWhenTheElementNeverAppeared() {
        Object actual = ValidationsHelper.describeMatchesActual(false, false,
                new NoSuchElementException("no such element: Unable to locate element"), LOCATOR);
        Assert.assertTrue(actual instanceof String, "a missing element must be reported as a cause, not a boolean");
        String message = (String) actual;
        Assert.assertTrue(message.contains("Element not found"), message);
        Assert.assertTrue(message.contains(LOCATOR.toString()), message);
        Assert.assertTrue(message.contains("NoSuchElementException"), message);
        Assert.assertTrue(message.contains("Unable to locate element"), message);
    }

    @Test
    public void keepsThePassingResultUnchanged() {
        Assert.assertEquals(ValidationsHelper.describeMatchesActual(true, true, null, LOCATOR), Boolean.TRUE);
        // a stale lookup failure from an earlier retry never overrides a passing comparison
        Assert.assertEquals(ValidationsHelper.describeMatchesActual(true, true,
                new NoSuchElementException("earlier retry"), LOCATOR), Boolean.TRUE);
    }

    @Test
    public void keepsTheImageMismatchResultWhenTheElementWasFound() {
        Assert.assertEquals(ValidationsHelper.describeMatchesActual(false, false, null, LOCATOR), Boolean.FALSE);
    }

    private static WebDriver driverThrowing(RuntimeException failure) {
        WebDriver driver = mock(WebDriver.class);
        when(driver.findElement(LOCATOR)).thenThrow(failure);
        return driver;
    }

    @Test
    public void captureRecordsAndRethrowsAMissingElement() {
        AtomicReference<RuntimeException> failure = new AtomicReference<>();
        NoSuchElementException missing = new NoSuchElementException("gone");
        Assert.assertThrows(NoSuchElementException.class,
                () -> ValidationsHelper.captureElementScreenshot(driverThrowing(missing), LOCATOR, failure));
        Assert.assertSame(failure.get(), missing);
    }

    @Test
    public void captureTreatsZeroWidthAsNotFound() {
        AtomicReference<RuntimeException> failure = new AtomicReference<>();
        Assert.assertThrows(NoSuchElementException.class, () -> ValidationsHelper.captureElementScreenshot(
                driverThrowing(new WebDriverException("Cannot take screenshot with 0 width.")), LOCATOR, failure));
        Assert.assertTrue(failure.get() instanceof NoSuchElementException);
    }

    @Test
    public void captureRetriesOnFirefoxTransientErrorAndRethrowsOthers() {
        AtomicReference<RuntimeException> failure = new AtomicReference<>();
        Assert.assertNull(ValidationsHelper.captureElementScreenshot(
                driverThrowing(new WebDriverException("NS_ERROR_FAILURE")), LOCATOR, failure));
        Assert.assertThrows(WebDriverException.class, () -> ValidationsHelper.captureElementScreenshot(
                driverThrowing(new WebDriverException("session deleted")), LOCATOR, failure));
        Assert.assertNull(failure.get());
    }

    @Test
    public void captureClearsAnEarlierFailureOnceTheElementIsFound() {
        WebElement element = mock(WebElement.class);
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[]{7});
        WebDriver driver = mock(WebDriver.class);
        when(driver.findElement(LOCATOR)).thenReturn(element);
        AtomicReference<RuntimeException> failure = new AtomicReference<>(new NoSuchElementException("earlier"));
        Assert.assertEquals(ValidationsHelper.captureElementScreenshot(driver, LOCATOR, failure), new byte[]{7});
        Assert.assertNull(failure.get());
    }

    @Test
    public void logsReferenceImageStateWithoutFailing() {
        ValidationsHelper.logReferenceImageState(null);
        ValidationsHelper.logReferenceImageState(new byte[0]);
        ValidationsHelper.logReferenceImageState(new byte[]{1});
    }
}
