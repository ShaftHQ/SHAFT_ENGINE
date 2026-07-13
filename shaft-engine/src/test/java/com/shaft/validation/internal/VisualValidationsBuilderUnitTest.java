package com.shaft.validation.internal;

import com.shaft.gui.playwright.validation.PlaywrightVisualComparisonOptions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.VisualComparisonOptions;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

/**
 * Coverage-focused unit tests for the {@code matchesScreenshot(...)} option plumbing. Since
 * {@code matchesScreenshot(...)} now executes immediately (like every other assertion, needing no
 * {@code perform()}), these tests exercise the public {@link VisualComparisonOptions} builder and the
 * internal {@link VisualValidationsBuilder#applyOptions(VisualComparisonOptions)} seeding without a live
 * browser session; end-to-end baseline lifecycle behavior is covered by
 * {@code ImageProcessingActionsScreenshotBaselineUnitTest}.
 */
public class VisualValidationsBuilderUnitTest {
    private static final By SAMPLE_LOCATOR = By.id("sample");

    @Test(description = "VisualComparisonOptions should accumulate diff budgets and masks fluently")
    public void optionsShouldAccumulateFields() {
        By firstMask = By.id("mask-1");
        By secondMask = By.id("mask-2");

        VisualComparisonOptions options = VisualComparisonOptions.create()
                .maxDiffPixels(100)
                .maxDiffPixelRatio(0.05)
                .mask(firstMask, secondMask);

        Assert.assertEquals(options.getMaxDiffPixels(), Integer.valueOf(100));
        Assert.assertEquals(options.getMaxDiffPixelRatio(), Double.valueOf(0.05));
        Assert.assertEquals(options.getMaskLocators(), List.of(firstMask, secondMask));
    }

    @Test(description = "unset VisualComparisonOptions fields should stay null so builder defaults apply")
    public void unsetOptionsShouldStayNull() {
        VisualComparisonOptions options = VisualComparisonOptions.create();

        Assert.assertNull(options.getMaxDiffPixels());
        Assert.assertNull(options.getMaxDiffPixelRatio());
        Assert.assertTrue(options.getMaskLocators().isEmpty());
    }

    @Test(description = "applyOptions should copy diff budgets and masks onto the internal builder")
    public void applyOptionsShouldSeedBuilderFields() {
        By mask = By.id("mask");
        VisualValidationsBuilder builder = new VisualValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, null, SAMPLE_LOCATOR, false, new StringBuilder());

        builder.applyOptions(VisualComparisonOptions.create()
                .maxDiffPixels(42)
                .maxDiffPixelRatio(0.02)
                .mask(mask));

        Assert.assertEquals(builder.maxDiffPixels, Integer.valueOf(42));
        Assert.assertEquals(builder.maxDiffPixelRatio, Double.valueOf(0.02));
        Assert.assertEquals(builder.maskLocators, List.of(mask));
    }

    @Test(description = "applyOptions(null) should leave the builder at its defaults")
    public void applyOptionsNullShouldLeaveDefaults() {
        VisualValidationsBuilder builder = new VisualValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, null, SAMPLE_LOCATOR, false, new StringBuilder());

        builder.applyOptions(null);

        Assert.assertNull(builder.maxDiffPixels);
        Assert.assertNull(builder.maxDiffPixelRatio);
        Assert.assertTrue(builder.maskLocators.isEmpty());
    }

    @Test(description = "PlaywrightVisualComparisonOptions should keep fluent chaining typed and inherit By masks")
    public void playwrightOptionsShouldStayTypedAndInheritByMasks() {
        By byMask = By.id("by-mask");

        PlaywrightVisualComparisonOptions options = PlaywrightVisualComparisonOptions.create()
                .maxDiffPixels(7)
                .maxDiffPixelRatio(0.03)
                .mask(byMask);

        Assert.assertEquals(options.getMaxDiffPixels(), Integer.valueOf(7));
        Assert.assertEquals(options.getMaxDiffPixelRatio(), Double.valueOf(0.03));
        Assert.assertEquals(options.getMaskLocators(), List.of(byMask));
        Assert.assertTrue(options.getPlaywrightMaskLocators().isEmpty());
    }
}
