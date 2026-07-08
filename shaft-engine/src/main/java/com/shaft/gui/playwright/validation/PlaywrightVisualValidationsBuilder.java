package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.ValidationsExecutor;
import com.shaft.validation.internal.VisualValidationsBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Playwright variant of {@link VisualValidationsBuilder}: adds a Playwright {@link Locator}-based
 * {@code mask(...)} overload and routes {@link #perform()} through {@link PlaywrightValidationsExecutor}
 * instead of the Selenium-oriented base executor.
 */
public final class PlaywrightVisualValidationsBuilder extends VisualValidationsBuilder {
    private final PlaywrightSession session;
    private final Locator playwrightLocator;
    private final String playwrightLocatorDescription;
    private final List<Locator> playwrightMaskLocators = new ArrayList<>();

    PlaywrightVisualValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, PlaywrightSession session,
                                       Locator playwrightLocator, String playwrightLocatorDescription, boolean pageLevel,
                                       StringBuilder reportMessageBuilder) {
        super(validationCategory, null, null, pageLevel, reportMessageBuilder);
        this.session = session;
        this.playwrightLocator = playwrightLocator;
        this.playwrightLocatorDescription = playwrightLocatorDescription;
    }

    @Override
    public PlaywrightVisualValidationsBuilder maxDiffPixels(int maxDiffPixels) {
        super.maxDiffPixels(maxDiffPixels);
        return this;
    }

    @Override
    public PlaywrightVisualValidationsBuilder maxDiffPixelRatio(double maxDiffPixelRatio) {
        super.maxDiffPixelRatio(maxDiffPixelRatio);
        return this;
    }

    /**
     * Excludes the region(s) covered by the given Playwright locators from the pixel comparison.
     *
     * @param masks Playwright locators of elements whose bounding boxes should be excluded from the diff
     * @return this builder, to continue chaining options
     */
    public PlaywrightVisualValidationsBuilder mask(Locator... masks) {
        playwrightMaskLocators.addAll(Arrays.asList(masks));
        return this;
    }

    @Override
    public ValidationsExecutor perform() {
        var executor = new PlaywrightValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    PlaywrightSession session() {
        return session;
    }

    Locator playwrightLocator() {
        return playwrightLocator;
    }

    String playwrightLocatorDescription() {
        return playwrightLocatorDescription;
    }

    List<Locator> playwrightMaskLocators() {
        return playwrightMaskLocators;
    }

    boolean pageLevel() {
        return pageLevel;
    }

    Integer maxDiffPixelsValue() {
        return maxDiffPixels;
    }

    Double maxDiffPixelRatioValue() {
        return maxDiffPixelRatio;
    }

    ValidationEnums.ValidationCategory validationCategory() {
        return validationCategory;
    }

    StringBuilder reportMessageBuilder() {
        return reportMessageBuilder;
    }
}
