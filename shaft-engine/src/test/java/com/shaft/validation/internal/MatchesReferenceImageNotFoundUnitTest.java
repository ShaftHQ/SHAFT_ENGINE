package com.shaft.validation.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.testng.Assert;
import org.testng.annotations.Test;

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
}
