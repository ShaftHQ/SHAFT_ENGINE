package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;

/**
 * Coverage-focused unit tests for {@link VisualValidationsBuilder} wiring from both the element-level
 * and page-level {@code matchesScreenshot()} entry points. These tests exercise builder configuration
 * paths without requiring a live browser session; end-to-end baseline lifecycle behavior is covered by
 * {@code ImageProcessingActionsScreenshotBaselineUnitTest}.
 */
public class VisualValidationsBuilderUnitTest {
    private static final By SAMPLE_LOCATOR = By.id("sample");

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "element-level matchesScreenshot() should return an unexecuted builder seeded with the element target")
    public void elementMatchesScreenshotShouldSeedBuilderWithoutExecuting() {
        WebDriverElementValidationsBuilder elementBuilder = new WebDriverElementValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, null, SAMPLE_LOCATOR, new StringBuilder("the element "));

        VisualValidationsBuilder visualBuilder = elementBuilder.matchesScreenshot();

        Assert.assertFalse(visualBuilder.pageLevel);
        Assert.assertEquals(visualBuilder.locator, SAMPLE_LOCATOR);
        Assert.assertEquals(visualBuilder.validationCategory, ValidationEnums.ValidationCategory.HARD_ASSERT);
        Assert.assertTrue(elementBuilder.reportMessageBuilder.toString().contains("matches the visual regression baseline screenshot."));
    }

    @Test(description = "page-level matchesScreenshot() should seed a page-level builder with a null locator")
    public void browserMatchesScreenshotShouldSeedPageLevelBuilder() {
        WebDriverBrowserValidationsBuilder browserBuilder = new WebDriverBrowserValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, null, new StringBuilder("the browser "));

        VisualValidationsBuilder visualBuilder = browserBuilder.matchesScreenshot();

        Assert.assertTrue(visualBuilder.pageLevel);
        Assert.assertNull(visualBuilder.locator);
        Assert.assertTrue(browserBuilder.reportMessageBuilder.toString().contains("page matches the visual regression baseline screenshot."));
    }

    @Test(description = "maxDiffPixels/maxDiffPixelRatio/mask options should populate builder fields without executing")
    public void optionsShouldPopulateFieldsWithoutExecuting() {
        WebDriverElementValidationsBuilder elementBuilder = new WebDriverElementValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, null, SAMPLE_LOCATOR, new StringBuilder());
        By mask = By.id("mask");

        VisualValidationsBuilder visualBuilder = elementBuilder.matchesScreenshot()
                .maxDiffPixels(100)
                .maxDiffPixelRatio(0.05)
                .mask(mask);

        Assert.assertEquals(visualBuilder.maxDiffPixels, Integer.valueOf(100));
        Assert.assertEquals(visualBuilder.maxDiffPixelRatio, Double.valueOf(0.05));
        Assert.assertEquals(visualBuilder.maskLocators, List.of(mask));
    }

    @Test(description = "mask() should accumulate locators across multiple chained calls")
    public void maskShouldAccumulateLocatorsAcrossChainedCalls() {
        WebDriverElementValidationsBuilder elementBuilder = new WebDriverElementValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, null, SAMPLE_LOCATOR, new StringBuilder());
        By firstMask = By.id("mask-1");
        By secondMask = By.id("mask-2");

        VisualValidationsBuilder visualBuilder = elementBuilder.matchesScreenshot()
                .mask(firstMask)
                .mask(secondMask);

        Assert.assertEquals(visualBuilder.maskLocators, List.of(firstMask, secondMask));
    }
}
