package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class NativeValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    WebDriver driver;
    By locator;

    ValidationEnums.ValidationType validationType;
    String validationMethod;

    String elementAttribute;
    String elementCssProperty;
    String browserAttribute;

    ValidationEnums.ValidationComparisonType validationComparisonType;
    Object expectedValue;
    Object actualValue;

    Object response;
    String jsonPath;

    public NativeValidationsBuilder(WebDriverElementValidationsBuilder webDriverElementValidationsBuilder) {
        this.validationCategory = webDriverElementValidationsBuilder.validationCategory;
        this.driver = webDriverElementValidationsBuilder.driver;
        this.locator = webDriverElementValidationsBuilder.locator;
        this.validationMethod = webDriverElementValidationsBuilder.validationMethod;
        this.elementAttribute = webDriverElementValidationsBuilder.elementAttribute;
        this.elementCssProperty = webDriverElementValidationsBuilder.elementCssProperty;
    }

    public NativeValidationsBuilder(WebDriverBrowserValidationsBuilder webDriverBrowserValidationsBuilder) {
        this.validationCategory = webDriverBrowserValidationsBuilder.validationCategory;
        this.driver = webDriverBrowserValidationsBuilder.driver;
        this.validationMethod = webDriverBrowserValidationsBuilder.validationMethod;
        this.browserAttribute = webDriverBrowserValidationsBuilder.browserAttribute;
    }

    public NativeValidationsBuilder(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationMethod = validationsBuilder.validationMethod;
        this.actualValue = validationsBuilder.actualValue;
    }

    public NativeValidationsBuilder(RestValidationsBuilder restValidationsBuilder) {
        this.validationCategory = restValidationsBuilder.validationCategory;
        this.validationMethod = restValidationsBuilder.validationMethod;
        this.jsonPath = restValidationsBuilder.jsonPath;
        this.response = restValidationsBuilder.response;
    }

    public ValidationsExecutor isEqualTo(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotEqual(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor contains(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotContain(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor matchesRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotMatchRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor equalsIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotEqualIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isNotNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isTrue() {
        this.expectedValue = true;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isFalse() {
        this.expectedValue = false;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }
}
