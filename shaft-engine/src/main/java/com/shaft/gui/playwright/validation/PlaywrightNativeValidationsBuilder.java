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
    private final String playwrightLocatorDescription;
    private final String playwrightElementAttribute;
    private final String playwrightElementCssProperty;
    private final String playwrightBrowserAttribute;
    private ValidationEnums.VisualValidationEngine visualValidationEngine;
    private String ariaSnapshotFileName;

    PlaywrightNativeValidationsBuilder(ValidationEnums.ValidationCategory validationCategory,
                                       PlaywrightSession session,
                                       Locator playwrightLocator,
                                       String playwrightLocatorDescription,
                                       String validationMethod,
                                       String elementAttribute,
                                       String elementCssProperty,
                                       String browserAttribute,
                                       StringBuilder reportMessageBuilder) {
        super(new SeedBuilder(validationCategory, validationMethod, reportMessageBuilder));
        this.session = session;
        this.playwrightLocator = playwrightLocator;
        this.playwrightLocatorDescription = playwrightLocatorDescription;
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

    PlaywrightValidationsExecutor createVisualExecutor(ValidationEnums.ValidationType validationType,
                                                       ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationType = validationType;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.expectedValue = validationType.getValue();
        this.visualValidationEngine = visualValidationEngine;
        return new PlaywrightValidationsExecutor(this);
    }

    PlaywrightValidationsExecutor createAriaSnapshotExecutor(String snapshotFileName) {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.expectedValue = true;
        this.ariaSnapshotFileName = snapshotFileName;
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

    String playwrightLocatorDescription() {
        return playwrightLocatorDescription;
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

    ValidationEnums.VisualValidationEngine visualValidationEngine() {
        return visualValidationEngine;
    }

    String ariaSnapshotFileName() {
        return ariaSnapshotFileName;
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
