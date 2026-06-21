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
    private final StringBuilder reportMessageBuilder = new StringBuilder("the element ");

    public PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory validationCategory,
                                               PlaywrightSession session,
                                               Locator locator) {
        this.validationCategory = validationCategory;
        this.session = session;
        this.locator = locator;
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
        return unsupportedVisualValidation();
    }

    @Override
    public ValidationsExecutor matchesReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        return unsupportedVisualValidation();
    }

    @Override
    public ValidationsExecutor doesNotMatchReferenceImage() {
        return unsupportedVisualValidation();
    }

    @Override
    public ValidationsExecutor doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        return unsupportedVisualValidation();
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

    private ValidationsExecutor unsupportedVisualValidation() {
        return builder("playwrightUnsupportedVisualValidation", null, null)
                .isEqualTo("Playwright visual reference validation is implemented");
    }

    private PlaywrightNativeValidationsBuilder builder(String validationMethod, String elementAttribute, String elementCssProperty) {
        return new PlaywrightNativeValidationsBuilder(validationCategory, session, locator, validationMethod,
                elementAttribute, elementCssProperty, null, reportMessageBuilder);
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
