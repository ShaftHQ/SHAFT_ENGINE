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

    protected StringBuilder reportMessageBuilder;

    public NativeValidationsBuilder(WebDriverElementValidationsBuilder webDriverElementValidationsBuilder) {
        this.validationCategory = webDriverElementValidationsBuilder.validationCategory;
        this.driver = webDriverElementValidationsBuilder.driver;
        this.locator = webDriverElementValidationsBuilder.locator;
        this.validationMethod = webDriverElementValidationsBuilder.validationMethod;
        this.elementAttribute = webDriverElementValidationsBuilder.elementAttribute;
        this.elementCssProperty = webDriverElementValidationsBuilder.elementCssProperty;

        this.reportMessageBuilder = webDriverElementValidationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(WebDriverBrowserValidationsBuilder webDriverBrowserValidationsBuilder) {
        this.validationCategory = webDriverBrowserValidationsBuilder.validationCategory;
        this.driver = webDriverBrowserValidationsBuilder.driver;
        this.validationMethod = webDriverBrowserValidationsBuilder.validationMethod;
        this.browserAttribute = webDriverBrowserValidationsBuilder.browserAttribute;

        this.reportMessageBuilder = webDriverBrowserValidationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationMethod = validationsBuilder.validationMethod;
        this.actualValue = validationsBuilder.actualValue;

        this.reportMessageBuilder = validationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(RestValidationsBuilder restValidationsBuilder) {
        this.validationCategory = restValidationsBuilder.validationCategory;
        this.validationMethod = restValidationsBuilder.validationMethod;
        this.jsonPath = restValidationsBuilder.jsonPath;
        this.response = restValidationsBuilder.response;

        this.reportMessageBuilder = restValidationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(FileValidationsBuilder fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;

        this.reportMessageBuilder = fileValidationsBuilder.reportMessageBuilder;
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
        reportMessageBuilder.append("is equal to [").append(expectedValue).append("].");
        return new ValidationsExecutor(this);
    }

    /**
     * Overrides the default object method equals and is the same as calling isEqualTo(expectedValue).perform();
     * @param expectedValue the test data / expected value for the object under test
     * @return boolean value true if passed and throws AssertionError if failed (return value can be safely ignored)
     */
    @Override
    public boolean equals(Object expectedValue){
        isEqualTo(expectedValue).perform();
        return true;
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
        reportMessageBuilder.append("does not equal [").append(expectedValue).append("].");
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
        reportMessageBuilder.append("contains [").append(expectedValue).append("].");
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
        reportMessageBuilder.append("does not contain [").append(expectedValue).append("].");
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
        reportMessageBuilder.append("matches this regular expression [").append(expectedValue).append("].");
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
        reportMessageBuilder.append("does not match this regular expression [").append(expectedValue).append("].");
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
        reportMessageBuilder.append("equals [").append(expectedValue).append("], ignoring case sensitivity.");
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
        reportMessageBuilder.append("does not equal [").append(expectedValue).append("], ignoring case sensitivity.");
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
        reportMessageBuilder.append("is NULL.");
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
        reportMessageBuilder.append("is not NULL.");
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
        reportMessageBuilder.append("is TRUE.");
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
        reportMessageBuilder.append("is FALSE.");
        return new ValidationsExecutor(this);
    }
}
