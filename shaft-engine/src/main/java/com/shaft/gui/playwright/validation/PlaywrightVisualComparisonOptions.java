package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.validation.VisualComparisonOptions;
import org.openqa.selenium.By;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Playwright variant of {@link VisualComparisonOptions} that adds a Playwright {@link Locator}-based
 * {@code mask(...)} overload, for use with the Playwright engine's
 * {@code assertThat(...).matchesScreenshot(options)} assertion.
 *
 * <pre>{@code
 * driver.element().assertThat(locator)
 *       .matchesScreenshot(PlaywrightVisualComparisonOptions.create()
 *               .maxDiffPixelRatio(0.01)
 *               .mask(page.locator("#timestamp")));
 * }</pre>
 */
public class PlaywrightVisualComparisonOptions extends VisualComparisonOptions {
    private final List<Locator> playwrightMaskLocators = new ArrayList<>();

    /**
     * @return a new, empty Playwright options object to configure and pass to {@code matchesScreenshot(...)}
     */
    public static PlaywrightVisualComparisonOptions create() {
        return new PlaywrightVisualComparisonOptions();
    }

    @Override
    public PlaywrightVisualComparisonOptions maxDiffPixels(int maxDiffPixels) {
        super.maxDiffPixels(maxDiffPixels);
        return this;
    }

    @Override
    public PlaywrightVisualComparisonOptions maxDiffPixelRatio(double maxDiffPixelRatio) {
        super.maxDiffPixelRatio(maxDiffPixelRatio);
        return this;
    }

    @Override
    public PlaywrightVisualComparisonOptions mask(By... masks) {
        super.mask(masks);
        return this;
    }

    /**
     * Excludes the region(s) covered by the given Playwright locators from the pixel comparison.
     *
     * @param masks Playwright locators of elements whose bounding boxes should be excluded from the diff
     * @return this options object, to continue chaining
     */
    public PlaywrightVisualComparisonOptions mask(Locator... masks) {
        this.playwrightMaskLocators.addAll(Arrays.asList(masks));
        return this;
    }

    /**
     * @return the (possibly empty) list of Playwright locators whose regions are excluded from the diff
     */
    public List<Locator> getPlaywrightMaskLocators() {
        return Collections.unmodifiableList(playwrightMaskLocators);
    }
}
