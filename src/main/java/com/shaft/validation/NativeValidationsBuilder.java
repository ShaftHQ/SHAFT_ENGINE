package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class NativeValidationsBuilder {
    protected ValidationEnums.ValidationCategory validationCategory;
    protected WebDriver driver;
    protected By locator;

    protected ValidationEnums.ValidationType validationType;
    protected String validationMethod;

    protected String elementAttribute;
    protected String elementCssProperty;
    protected String browserAttribute;

    protected ValidationEnums.ValidationComparisonType validationComparisonType;
    protected Object expectedValue;
    protected Object actualValue;

    protected Object response;
    protected String jsonPath;

    protected String folderRelativePath;
    protected String fileName;

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

    public NativeValidationsBuilder(FileValidationsBuilder fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;
    }

    /**
     * Use this to check that the actual object is equal to the expected value
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isEqualTo(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object is not equal to the expected value
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqual(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object contains the expected value
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor contains(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object does not contain the expected value
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotContain(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object matches the expected regular expression
     * @param expectedValue the test data / expected regular expression for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor matchesRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object does not match the expected regular expression
     * @param expectedValue the test data / expected regular expression for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotMatchRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object is equal to the expected value (ignoring case sensitivity)
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor equalsIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object is not equal to the expected value (ignoring case sensitivity)
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqualIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object is null
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object is not null
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isNotNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object is true
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isTrue() {
        this.expectedValue = true;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual object is false
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isFalse() {
        this.expectedValue = false;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }
}
