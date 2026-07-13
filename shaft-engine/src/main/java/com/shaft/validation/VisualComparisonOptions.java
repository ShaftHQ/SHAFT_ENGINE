package com.shaft.validation;

import org.openqa.selenium.By;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Options for the {@code matchesScreenshot(...)} visual-regression assertion, mirroring Playwright's
 * {@code toHaveScreenshot()} options.
 *
 * <p>Build with {@link #create()} and the fluent setters, then pass the result to
 * {@code assertThat(...).matchesScreenshot(options)}. Like every other SHAFT assertion,
 * {@code matchesScreenshot(...)} executes immediately &mdash; no trailing {@code perform()} is
 * required.</p>
 *
 * <pre>{@code
 * driver.element().assertThat(By.id("logo"))
 *       .matchesScreenshot(VisualComparisonOptions.create()
 *               .maxDiffPixelRatio(0.01)
 *               .mask(By.id("timestamp")));
 * }</pre>
 */
public class VisualComparisonOptions {
    private final List<By> maskLocators = new ArrayList<>();
    private Integer maxDiffPixels;
    private Double maxDiffPixelRatio;

    /**
     * @return a new, empty options object to configure and pass to {@code matchesScreenshot(...)}
     */
    public static VisualComparisonOptions create() {
        return new VisualComparisonOptions();
    }

    /**
     * Caps the number of differing pixels allowed for the comparison to still pass.
     *
     * @param maxDiffPixels maximum allowed differing pixel count
     * @return this options object, to continue chaining
     */
    public VisualComparisonOptions maxDiffPixels(int maxDiffPixels) {
        this.maxDiffPixels = maxDiffPixels;
        return this;
    }

    /**
     * Caps the ratio of differing pixels (0.0-1.0) allowed for the comparison to still pass.
     *
     * @param maxDiffPixelRatio maximum allowed differing pixel ratio
     * @return this options object, to continue chaining
     */
    public VisualComparisonOptions maxDiffPixelRatio(double maxDiffPixelRatio) {
        this.maxDiffPixelRatio = maxDiffPixelRatio;
        return this;
    }

    /**
     * Excludes the region(s) covered by the given locators from the pixel comparison.
     *
     * @param masks locators of elements whose bounding boxes should be excluded from the diff
     * @return this options object, to continue chaining
     */
    public VisualComparisonOptions mask(By... masks) {
        this.maskLocators.addAll(Arrays.asList(masks));
        return this;
    }

    /**
     * @return the configured maximum differing pixel count, or {@code null} if unset
     */
    public Integer getMaxDiffPixels() {
        return maxDiffPixels;
    }

    /**
     * @return the configured maximum differing pixel ratio, or {@code null} if unset
     */
    public Double getMaxDiffPixelRatio() {
        return maxDiffPixelRatio;
    }

    /**
     * @return the (possibly empty) list of locators whose regions are excluded from the diff
     */
    public List<By> getMaskLocators() {
        return Collections.unmodifiableList(maskLocators);
    }
}
