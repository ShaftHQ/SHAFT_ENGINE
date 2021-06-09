package com.shaft.validation;

import com.shaft.api.RestActions;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class ValidationsAttributesBuilder {
    ValidationEnums.ValidationCategory validationCategory = ValidationEnums.ValidationCategory.HARD_ASSERT;
    ValidationEnums.ValidationComparisonType validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
    ValidationEnums.ValidationType validationType = ValidationEnums.ValidationType.POSITIVE;
    String customLogMessage = "";

    String validationMethod = "";
    Object expectedValue = null;
    Object actualValue = null;

    WebDriver driver;
    By locator;
    String elementAttribute;
    String elementCssProperty;
    String browserAttribute;

    ValidationEnums.NumbersComparativeRelation numbersComparativeRelation;

    String folderRelativePath;
    String fileName;

    boolean condition;

    ValidationEnums.VisualValidationEngine visualValidationEngine;

    Object response;
    String fileAbsolutePath;
    String jsonPath;

    ValidationsAttributesBuilder(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationMethod = validationsBuilder.validationMethod;
        this.expectedValue = validationsBuilder.expectedValue;
        this.actualValue = validationsBuilder.actualValue;
        this.folderRelativePath = validationsBuilder.folderRelativePath;
        this.fileName = validationsBuilder.fileName;
        this.condition = validationsBuilder.condition;
    }

    ValidationsAttributesBuilder(WebElementValidationsBuilder webElementValidationsBuilder) {
        this.validationCategory = webElementValidationsBuilder.validationsBuilder.validationCategory;
        this.validationMethod = webElementValidationsBuilder.validationsBuilder.validationMethod;
        this.expectedValue = webElementValidationsBuilder.validationsBuilder.expectedValue;
        this.actualValue = webElementValidationsBuilder.validationsBuilder.actualValue;
        this.driver = webElementValidationsBuilder.driver;
        this.locator = webElementValidationsBuilder.locator;
        this.elementAttribute = webElementValidationsBuilder.elementAttribute;
        this.elementCssProperty = webElementValidationsBuilder.elementCssProperty;
        this.visualValidationEngine = webElementValidationsBuilder.visualValidationEngine;

    }

    ValidationsAttributesBuilder(WebBrowserValidationsBuilder webBrowserValidationsBuilder) {
        this.validationCategory = webBrowserValidationsBuilder.validationsBuilder.validationCategory;
        this.validationMethod = webBrowserValidationsBuilder.validationsBuilder.validationMethod;
        this.expectedValue = webBrowserValidationsBuilder.validationsBuilder.expectedValue;
        this.actualValue = webBrowserValidationsBuilder.validationsBuilder.actualValue;
        this.driver = webBrowserValidationsBuilder.driver;
        this.browserAttribute = webBrowserValidationsBuilder.browserAttribute;
    }

    ValidationsAttributesBuilder(JsonValidationsBuilder jsonValidationsBuilder) {
        this.validationCategory = jsonValidationsBuilder.validationsBuilder.validationCategory;
        this.validationMethod = jsonValidationsBuilder.validationsBuilder.validationMethod;
        this.expectedValue = jsonValidationsBuilder.validationsBuilder.expectedValue;
        this.actualValue = jsonValidationsBuilder.validationsBuilder.actualValue;
        this.response = jsonValidationsBuilder.response;
        this.fileAbsolutePath = jsonValidationsBuilder.fileAbsolutePath;
        this.jsonPath = jsonValidationsBuilder.jsonPath;
    }


    public ValidationsAttributesBuilder withCustomLogMessage(String customLogMessage) {
        this.customLogMessage = customLogMessage;
        return this;
    }

    public ValidationsAttributesBuilder withContainsComparison() {
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        return this;
    }

    public ValidationsAttributesBuilder withRegexMatchingComparison() {
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        return this;
    }

    public ValidationsAttributesBuilder withCaseInsensitiveComparison() {
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        return this;
    }

    public ValidationsAttributesBuilder withNumbersComparisonRelation(ValidationEnums.NumbersComparativeRelation numbersComparativeRelation) {
        this.numbersComparativeRelation = numbersComparativeRelation;
        return this;
    }

    public ValidationsAttributesBuilder negatively() {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return this;
    }

    public void perform() {
        switch (validationMethod) {
            case "forceFail":
                ValidationHelper.validateFail(validationCategory, customLogMessage);
                break;
            case "objectsAreEqual":
                ValidationHelper.validateEquals(validationCategory, expectedValue, actualValue, validationComparisonType, validationType, customLogMessage);
                break;
            case "objectIsNull":
                ValidationHelper.validateNull(validationCategory, actualValue, validationType, customLogMessage);
                break;
            case "elementExists":
                ValidationHelper.validateElementExists(validationCategory, driver, locator, validationType, customLogMessage);
                break;
            case "elementAttributeEquals":
                ValidationHelper.validateElementAttribute(validationCategory, driver, locator, elementAttribute, String.valueOf(expectedValue),
                        validationComparisonType, validationType, customLogMessage);
                break;
            case "elementCssPropertyEquals":
                ValidationHelper.validateElementCSSProperty(validationCategory, driver, locator, elementCssProperty, String.valueOf(expectedValue),
                        validationComparisonType, validationType, customLogMessage);
                break;
            case "browserAttributeEquals":
                ValidationHelper.validateBrowserAttribute(validationCategory, driver, browserAttribute, String.valueOf(expectedValue), validationComparisonType,
                        validationType, customLogMessage);
                break;
            case "comparativeRelationBetweenNumbers":
                ValidationHelper.validateComparativeRelation(validationCategory, (Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType, customLogMessage);
                break;
            case "fileExists":
                ValidationHelper.validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customLogMessage);
                break;
            case "conditionIsTrue":
                ValidationHelper.validateTrue(validationCategory, condition, validationType, customLogMessage);
                break;
            case "responseEqualsFileContent":
                ValidationHelper.validateJSONFileContent(validationCategory, (Response) response, fileAbsolutePath, RestActions.ComparisonType.valueOf(validationComparisonType.name()), "", validationType, customLogMessage);
                break;
            case "jsonPathValueEquals":
                ValidationHelper.validateEquals(validationCategory, expectedValue,
                        RestActions.getResponseJSONValue(response, jsonPath), validationComparisonType,
                        validationType, customLogMessage);
                break;
            case "elementMatches":
                ValidationHelper.validateElementMatches(validationCategory, driver, locator, visualValidationEngine, validationType, customLogMessage);
                break;
            default:
                break;
        }
    }
}
