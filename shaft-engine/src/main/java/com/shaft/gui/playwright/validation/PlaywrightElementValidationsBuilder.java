package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.gui.driver.ElementAssertions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;

public class PlaywrightElementValidationsBuilder implements ElementAssertions {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final PlaywrightSession session;
    private final Locator locator;
    private final String locatorDescription;
    private final StringBuilder reportMessageBuilder = new StringBuilder("the element ");

    public PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory validationCategory,
                                               PlaywrightSession session,
                                               Locator locator) {
        this(validationCategory, session, locator, String.valueOf(locator));
    }

    public PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory validationCategory,
                                               PlaywrightSession session,
                                               Locator locator,
                                               String locatorDescription) {
        this.validationCategory = validationCategory;
        this.session = session;
        this.locator = locator;
        this.locatorDescription = locatorDescription;
    }

    @Override
    public ValidationsExecutor exists() {
        reportMessageBuilder.append("exists.");
        var executor = executor("elementExists", ValidationEnums.ValidationType.POSITIVE);
        executor.internalPerform();
        return executor;
    }

    @Override
    public ValidationsExecutor doesNotExist() {
        reportMessageBuilder.append("does not exist.");
        var executor = executor("elementExists", ValidationEnums.ValidationType.NEGATIVE);
        executor.internalPerform();
        return executor;
    }

    @Override
    public ValidationsExecutor matchesReferenceImage() {
        return visualValidation(ValidationEnums.ValidationType.POSITIVE, ValidationEnums.VisualValidationEngine.EXACT_OPENCV);
    }

    @Override
    public ValidationsExecutor matchesReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        return visualValidation(ValidationEnums.ValidationType.POSITIVE, visualValidationEngine);
    }

    @Override
    public ValidationsExecutor doesNotMatchReferenceImage() {
        return visualValidation(ValidationEnums.ValidationType.NEGATIVE, ValidationEnums.VisualValidationEngine.EXACT_OPENCV);
    }

    @Override
    public ValidationsExecutor doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        return visualValidation(ValidationEnums.ValidationType.NEGATIVE, visualValidationEngine);
    }

    @Override
    public NativeValidationsBuilder attribute(String attribute) {
        reportMessageBuilder.append("Attribute \"").append(attribute).append("\" ");
        return builder("elementAttributeEquals", attribute, null);
    }

    @Override
    public NativeValidationsBuilder domAttribute(String domAttribute) {
        reportMessageBuilder.append("DOM attribute \"").append(domAttribute).append("\" ");
        return builder("elementDomAttributeEquals", domAttribute, null);
    }

    @Override
    public NativeValidationsBuilder domProperty(String domProperty) {
        reportMessageBuilder.append("DOM property \"").append(domProperty).append("\" ");
        return builder("elementDomPropertyEquals", domProperty, null);
    }

    @Override
    public NativeValidationsBuilder property(String domProperty) {
        reportMessageBuilder.append("DOM property \"").append(domProperty).append("\" ");
        return builder("elementPropertyEquals", domProperty, null);
    }

    @Override
    public ValidationsExecutor isSelected() {
        reportMessageBuilder.append("is selected; selected property ");
        return expectedState("elementSelected", ValidationEnums.ValidationType.POSITIVE);
    }

    @Override
    public ValidationsExecutor isChecked() {
        reportMessageBuilder.append("is checked; checked state ");
        return expectedState("elementChecked", ValidationEnums.ValidationType.POSITIVE);
    }

    @Override
    public ValidationsExecutor isVisible() {
        reportMessageBuilder.append("is visible; visible state ");
        return expectedState("elementVisible", ValidationEnums.ValidationType.POSITIVE);
    }

    @Override
    public ValidationsExecutor isEnabled() {
        reportMessageBuilder.append("is enabled; enabled state ");
        return expectedState("elementEnabled", ValidationEnums.ValidationType.POSITIVE);
    }

    @Override
    public ValidationsExecutor isNotSelected() {
        reportMessageBuilder.append("is not selected; selected property ");
        return expectedState("elementSelected", ValidationEnums.ValidationType.NEGATIVE);
    }

    @Override
    public ValidationsExecutor isNotChecked() {
        reportMessageBuilder.append("is not checked; checked state ");
        return expectedState("elementChecked", ValidationEnums.ValidationType.NEGATIVE);
    }

    @Override
    public ValidationsExecutor isHidden() {
        reportMessageBuilder.append("is hidden; visible state ");
        return expectedState("elementVisible", ValidationEnums.ValidationType.NEGATIVE);
    }

    @Override
    public ValidationsExecutor isDisabled() {
        reportMessageBuilder.append("is disabled; enabled state ");
        return expectedState("elementEnabled", ValidationEnums.ValidationType.NEGATIVE);
    }

    @Override
    public NativeValidationsBuilder text() {
        reportMessageBuilder.append("text ");
        return builder("elementDomAttributeEquals", "text", null);
    }

    @Override
    public NativeValidationsBuilder textTrimmed() {
        reportMessageBuilder.append("text trimmed ");
        return builder("elementDomAttributeEquals", "textTrimmed", null);
    }

    @Override
    public NativeValidationsBuilder cssProperty(String elementCssProperty) {
        reportMessageBuilder.append("CSS property \"").append(elementCssProperty).append("\" ");
        return builder("elementCssPropertyEquals", null, elementCssProperty);
    }

    private PlaywrightNativeValidationsBuilder builder(String validationMethod, String elementAttribute, String elementCssProperty) {
        return new PlaywrightNativeValidationsBuilder(validationCategory, session, locator, locatorDescription, validationMethod,
                elementAttribute, elementCssProperty, null, reportMessageBuilder);
    }

    private ValidationsExecutor visualValidation(ValidationEnums.ValidationType validationType,
                                                 ValidationEnums.VisualValidationEngine visualValidationEngine) {
        reportMessageBuilder.append(validationType.getValue() ? "matches" : "does not match")
                .append(" the reference image \"").append(visualValidationEngine).append("\".");
        var executor = builder("elementMatches", null, null)
                .createVisualExecutor(validationType, visualValidationEngine);
        executor.internalPerform();
        return executor;
    }

    private ValidationsExecutor expectedState(String validationMethod, ValidationEnums.ValidationType validationType) {
        reportMessageBuilder.append("is ").append(validationType.getValue() ? "TRUE." : "FALSE.");
        var executor = executor(validationMethod, validationType);
        executor.internalPerform();
        return executor;
    }

    private PlaywrightValidationsExecutor executor(String validationMethod, ValidationEnums.ValidationType validationType) {
        return builder(validationMethod, null, null)
                .createExecutor(validationType, ValidationEnums.ValidationComparisonType.EQUALS, validationType.getValue());
    }
}
