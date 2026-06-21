package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;

final class PlaywrightNativeValidationsBuilder extends NativeValidationsBuilder {
    private final PlaywrightSession session;
    private final Locator playwrightLocator;
    private final String playwrightElementAttribute;
    private final String playwrightElementCssProperty;
    private final String playwrightBrowserAttribute;

    PlaywrightNativeValidationsBuilder(ValidationEnums.ValidationCategory validationCategory,
                                       PlaywrightSession session,
                                       Locator playwrightLocator,
                                       String validationMethod,
                                       String elementAttribute,
                                       String elementCssProperty,
                                       String browserAttribute,
                                       StringBuilder reportMessageBuilder) {
        super(new SeedBuilder(validationCategory, validationMethod, reportMessageBuilder));
        this.session = session;
        this.playwrightLocator = playwrightLocator;
        this.playwrightElementAttribute = elementAttribute;
        this.playwrightElementCssProperty = elementCssProperty;
        this.playwrightBrowserAttribute = browserAttribute;
        this.elementAttribute = elementAttribute;
        this.elementCssProperty = elementCssProperty;
        this.browserAttribute = browserAttribute;
    }

    @Override
    protected ValidationsExecutor createExecutor() {
        return new PlaywrightValidationsExecutor(this);
    }

    PlaywrightValidationsExecutor createExecutor(ValidationEnums.ValidationType validationType,
                                                 ValidationEnums.ValidationComparisonType validationComparisonType,
                                                 Object expectedValue) {
        this.validationType = validationType;
        this.validationComparisonType = validationComparisonType;
        this.expectedValue = expectedValue;
        return new PlaywrightValidationsExecutor(this);
    }

    PlaywrightSession session() {
        return session;
    }

    Locator playwrightLocator() {
        return playwrightLocator;
    }

    String playwrightElementAttribute() {
        return playwrightElementAttribute;
    }

    String playwrightElementCssProperty() {
        return playwrightElementCssProperty;
    }

    String playwrightBrowserAttribute() {
        return playwrightBrowserAttribute;
    }

    ValidationEnums.ValidationCategory validationCategory() {
        return validationCategory;
    }

    ValidationEnums.ValidationType validationType() {
        return validationType;
    }

    String validationMethod() {
        return validationMethod;
    }

    ValidationEnums.ValidationComparisonType validationComparisonType() {
        return validationComparisonType;
    }

    Object expectedValue() {
        return expectedValue;
    }

    StringBuilder reportMessageBuilder() {
        return reportMessageBuilder;
    }

    private static final class SeedBuilder extends ValidationsBuilder {
        SeedBuilder(ValidationEnums.ValidationCategory validationCategory,
                    String validationMethod,
                    StringBuilder reportMessageBuilder) {
            super(validationCategory);
            this.validationMethod = validationMethod;
            this.reportMessageBuilder.append(reportMessageBuilder);
        }
    }
}
