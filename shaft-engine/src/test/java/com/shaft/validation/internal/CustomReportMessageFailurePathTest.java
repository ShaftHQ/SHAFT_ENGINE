package com.shaft.validation.internal;

import com.shaft.validation.Validations;
import com.shaft.validation.ValidationEnums;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * The custom report message is part of the failure string the hard assert throws
 * and the soft assert accumulates. The call goes through {@link Validations} and the
 * static report path used by Playwright.
 */
public class CustomReportMessageFailurePathTest {
    private static final String MARKER = "order total diverged";

    @Test
    public void hardAssertFailureIncludesCustomMessageAndExpectedActual() {
        AssertionError failure = Assert.expectThrows(AssertionError.class, () ->
                Validations.assertThat().object("shipped").isEqualTo("quoted")
                        .withCustomReportMessage(MARKER).perform());

        String message = failure.getMessage();
        Assert.assertTrue(message.contains(MARKER), message);
        Assert.assertTrue(message.contains("expected"), message);
        Assert.assertTrue(message.contains("quoted"), message);
        Assert.assertTrue(message.contains("shipped"), message);
    }

    @Test
    public void softAssertFailureAccumulatesTheSameCustomMessage() {
        try {
            Validations.verifyThat().object("shipped").isEqualTo("quoted")
                    .withCustomReportMessage(MARKER).perform();
            String summary = ValidationsHelper.verificationSummaryText();
            Assert.assertNotNull(summary);
            Assert.assertTrue(summary.contains(MARKER), summary);
            Assert.assertTrue(summary.contains("expected"), summary);
            Assert.assertTrue(summary.contains("quoted"), summary);
            Assert.assertTrue(summary.contains("shipped"), summary);
        } finally {
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    @Test
    public void passingValidationWithACustomMessageDoesNotThrow() {
        Validations.assertThat().object("quoted").isEqualTo("quoted")
                .withCustomReportMessage(MARKER).perform();
    }

    @Test
    public void blankCustomMessageAddsNoEmptyPrefix() {
        AssertionError failure = Assert.expectThrows(AssertionError.class, () ->
                Validations.assertThat().object("shipped").isEqualTo("quoted")
                        .withCustomReportMessage("   ").perform());
        String message = failure.getMessage();
        Assert.assertFalse(message.startsWith("—"), message);
        Assert.assertFalse(message.startsWith(" —"), message);
        Assert.assertTrue(message.contains("expected"), message);
        Assert.assertTrue(message.contains("quoted"), message);
    }

    @Test
    public void forceFailDoesNotPrefixTheCustomMessageTwice() {
        AssertionError failure = Assert.expectThrows(AssertionError.class, () ->
                Validations.assertThat().forceFail().withCustomReportMessage(MARKER).perform());
        String message = failure.getMessage();
        Assert.assertTrue(message.contains(MARKER), message);
        Assert.assertFalse(message.contains(MARKER + " — "), message);
        int first = message.indexOf(MARKER);
        Assert.assertEquals(message.indexOf(MARKER, first + MARKER.length()), -1, message);
    }

    @Test
    public void unchainedHardAssertStillThrowsWithoutTheCustomMessage() {
        AssertionError failure = Assert.expectThrows(AssertionError.class, () ->
                Validations.assertThat().object("shipped").isEqualTo("quoted"));
        String message = failure.getMessage();
        Assert.assertTrue(message.contains("quoted"), message);
        Assert.assertTrue(message.contains("shipped"), message);
        Assert.assertFalse(message.contains(MARKER), message);
    }

    @Test
    public void staticReportPathUsedByPlaywrightIncludesTheCustomMessage() {
        AssertionError failure = Assert.expectThrows(AssertionError.class, () ->
                ValidationsHelper.reportValidationState(
                        ValidationEnums.ValidationCategory.HARD_ASSERT,
                        false,
                        "quoted",
                        "shipped",
                        null,
                        System.currentTimeMillis(),
                        false,
                        MARKER));
        String message = failure.getMessage();
        Assert.assertTrue(message.contains(MARKER), message);
        Assert.assertTrue(message.contains("quoted"), message);
        Assert.assertTrue(message.contains("shipped"), message);
    }
}
