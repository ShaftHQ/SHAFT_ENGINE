package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Fluent options builder for the {@code matchesScreenshot()} visual-regression assertion.
 *
 * <p>Unlike most SHAFT validation builders (which execute immediately on their terminal call),
 * this builder gathers optional diff-budget/mask options before {@link #perform()} triggers the
 * comparison, mirroring Playwright's {@code toHaveScreenshot()} options.</p>
 */
@SuppressWarnings("unused")
public class VisualValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final WebDriver driver;
    protected final By locator;
    protected final boolean pageLevel;
    protected final StringBuilder reportMessageBuilder;
    protected final ValidationEnums.ValidationType validationType = ValidationEnums.ValidationType.POSITIVE;
    protected final List<By> maskLocators = new ArrayList<>();
    protected Integer maxDiffPixels;
    protected Double maxDiffPixelRatio;

    public VisualValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, By locator,
                                    boolean pageLevel, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;
        this.locator = locator;
        this.pageLevel = pageLevel;
        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Caps the number of differing pixels allowed for the comparison to still pass.
     *
     * @param maxDiffPixels maximum allowed differing pixel count
     * @return this builder, to continue chaining options
     */
    public VisualValidationsBuilder maxDiffPixels(int maxDiffPixels) {
        this.maxDiffPixels = maxDiffPixels;
        return this;
    }

    /**
     * Caps the ratio of differing pixels (0.0-1.0) allowed for the comparison to still pass.
     *
     * @param maxDiffPixelRatio maximum allowed differing pixel ratio
     * @return this builder, to continue chaining options
     */
    public VisualValidationsBuilder maxDiffPixelRatio(double maxDiffPixelRatio) {
        this.maxDiffPixelRatio = maxDiffPixelRatio;
        return this;
    }

    /**
     * Excludes the region(s) covered by the given locators from the pixel comparison.
     *
     * @param masks locators of elements whose bounding boxes should be excluded from the diff
     * @return this builder, to continue chaining options
     */
    public VisualValidationsBuilder mask(By... masks) {
        this.maskLocators.addAll(Arrays.asList(masks));
        return this;
    }

    /**
     * Executes the visual-regression comparison (or saves a new baseline on first run / when
     * {@code -Dshaft.updateSnapshots=true} is set).
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor perform() {
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }
}
