package com.shaft.validation.internal.builder;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.executor.GenericExecutor;
import com.shaft.validation.internal.executor.ValidationsExecutor;
import com.shaft.validation.internal.executor.WebExecutor;
import lombok.Getter;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

@Getter
public class Native {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected WebDriver driver;
    protected By locator;

    protected ValidationEnums.ValidationType validationType;
    protected final String validationMethod;

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

    protected final StringBuilder reportMessageBuilder;

    public Native(WebElement webElementValidationsBuilder) {
        this.validationCategory = webElementValidationsBuilder.validationCategory;
        this.driver = webElementValidationsBuilder.driver;
        this.locator = webElementValidationsBuilder.locator;
        this.validationMethod = webElementValidationsBuilder.validationMethod;
        this.elementAttribute = webElementValidationsBuilder.elementAttribute;
        this.elementCssProperty = webElementValidationsBuilder.elementCssProperty;

        this.reportMessageBuilder = webElementValidationsBuilder.reportMessageBuilder;
    }

    public Native(WebBrowser webBrowserValidationsBuilder) {
        this.validationCategory = webBrowserValidationsBuilder.validationCategory;
        this.driver = webBrowserValidationsBuilder.driver;
        this.validationMethod = webBrowserValidationsBuilder.validationMethod;
        this.browserAttribute = webBrowserValidationsBuilder.browserAttribute;

        this.reportMessageBuilder = webBrowserValidationsBuilder.reportMessageBuilder;
    }

    public Native(Standalone validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationMethod = validationsBuilder.validationMethod;
        this.actualValue = validationsBuilder.actualValue;

        this.reportMessageBuilder = validationsBuilder.reportMessageBuilder;
    }

    public Native(API restValidationsBuilder) {
        this.validationCategory = restValidationsBuilder.validationCategory;
        this.validationMethod = restValidationsBuilder.validationMethod;
        this.jsonPath = restValidationsBuilder.jsonPath;
        this.response = restValidationsBuilder.response;

        this.reportMessageBuilder = restValidationsBuilder.reportMessageBuilder;
    }

    public Native(File fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;

        this.reportMessageBuilder = fileValidationsBuilder.reportMessageBuilder;
    }

    /**
     * Use this to check that the actual object is equal to the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isEqualTo(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is equal to \"").append(expectedValue).append("\".");
        return getExecutor();
    }

    /**
     * Overrides the default object method equals and is the same as calling isEqualTo(expectedValue).perform();
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return boolean value true if passed and throws AssertionError if failed (return value can be safely ignored)
     */
    @SuppressWarnings("EqualsWhichDoesntCheckParameterClass")
    @Override
    public boolean equals(Object expectedValue) {
        isEqualTo(expectedValue).perform();
        return true;
    }

    /**
     * Use this to check that the actual object is not equal to the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqual(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not equal \"").append(expectedValue).append("\".");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object contains the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor contains(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("contains \"").append(expectedValue).append("\".");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object does not contain the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotContain(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not contain \"").append(expectedValue).append("\".");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object matches the expected regular expression
     *
     * @param expectedValue the test data / expected regular expression for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor matchesRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("matches this regular expression \"").append(expectedValue).append("\".");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object does not match the expected regular expression
     *
     * @param expectedValue the test data / expected regular expression for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotMatchRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not match this regular expression \"").append(expectedValue).append("\".");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object is equal to the expected value (ignoring case sensitivity)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor equalsIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("equals \"").append(expectedValue).append("\", ignoring case sensitivity.");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object is not equal to the expected value (ignoring case sensitivity)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqualIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not equal \"").append(expectedValue).append("\", ignoring case sensitivity.");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object is null
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is NULL.");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object is not null
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isNotNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("is not NULL.");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object is true
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isTrue() {
        this.expectedValue = true;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is TRUE.");
        return getExecutor();
    }

    /**
     * Use this to check that the actual object is false
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isFalse() {
        this.expectedValue = false;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is FALSE.");
        return getExecutor();
    }

    private ValidationsExecutor getExecutor() {
        if (driver != null) {
            return new WebExecutor(this);
        } else {
            return new GenericExecutor(this);
        }
    }
}
