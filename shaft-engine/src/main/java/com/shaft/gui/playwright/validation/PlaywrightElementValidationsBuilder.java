package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.gui.driver.ElementAssertions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;

public class PlaywrightElementValidationsBuilder implements ElementAssertions {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final Locator locator;

    public PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, Locator locator) {
        this.validationCategory = validationCategory;
        this.locator = locator;
    }

    @Override
    public ValidationsExecutor exists() {
        return object(locator.count() > 0).isTrue();
    }

    @Override
    public ValidationsExecutor doesNotExist() {
        return object(locator.count() == 0).isTrue();
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
        return object(locator.getAttribute(attribute));
    }

    @Override
    public NativeValidationsBuilder domAttribute(String domAttribute) {
        if ("text".equalsIgnoreCase(domAttribute)) {
            return object(locator.textContent());
        }
        return object(locator.getAttribute(domAttribute));
    }

    @Override
    public NativeValidationsBuilder domProperty(String domProperty) {
        return object(locator.evaluate("(element, property) => element[property]", domProperty));
    }

    @Override
    public NativeValidationsBuilder property(String domProperty) {
        return domProperty(domProperty);
    }

    @Override
    public ValidationsExecutor isSelected() {
        return object(locator.evaluate("element => !!element.selected")).isTrue();
    }

    @Override
    public ValidationsExecutor isChecked() {
        return object(locator.isChecked()).isTrue();
    }

    @Override
    public ValidationsExecutor isVisible() {
        return object(locator.isVisible()).isTrue();
    }

    @Override
    public ValidationsExecutor isEnabled() {
        return object(locator.isEnabled()).isTrue();
    }

    @Override
    public ValidationsExecutor isNotSelected() {
        return object(locator.evaluate("element => !element.selected")).isTrue();
    }

    @Override
    public ValidationsExecutor isNotChecked() {
        return object(!locator.isChecked()).isTrue();
    }

    @Override
    public ValidationsExecutor isHidden() {
        return object(!locator.isVisible()).isTrue();
    }

    @Override
    public ValidationsExecutor isDisabled() {
        return object(!locator.isEnabled()).isTrue();
    }

    @Override
    public NativeValidationsBuilder text() {
        return object(locator.textContent());
    }

    @Override
    public NativeValidationsBuilder textTrimmed() {
        String text = locator.textContent();
        return object(text == null ? null : text.trim());
    }

    @Override
    public NativeValidationsBuilder cssProperty(String elementCssProperty) {
        return object(locator.evaluate("(element, property) => getComputedStyle(element).getPropertyValue(property)",
                elementCssProperty));
    }

    private NativeValidationsBuilder object(Object actual) {
        return validationCategory == ValidationEnums.ValidationCategory.HARD_ASSERT
                ? Validations.assertThat().object(actual)
                : Validations.verifyThat().object(actual);
    }

    private ValidationsExecutor unsupportedVisualValidation() {
        return object("Playwright visual reference validation is not implemented yet")
                .isEqualTo("Playwright visual reference validation is implemented");
    }
}
