package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class ValidationsExecutor {
    ValidationEnums.ValidationCategory validationCategory;
    WebDriver driver;
    By locator;
    ValidationEnums.ValidationType validationType;
    String validationMethod;
    String customReportMessage = "";

    ValidationEnums.VisualValidationEngine visualValidationEngine;

    String elementAttribute;
    String elementCssProperty;
    String browserAttribute;

    ValidationEnums.ValidationComparisonType validationComparisonType;
    Object expectedValue;

    boolean condition;
    Object actualValue;

    public ValidationsExecutor(WebDriverElementValidationsBuilder webDriverElementValidationsBuilder) {
        this.validationCategory = webDriverElementValidationsBuilder.validationCategory;
        this.driver = webDriverElementValidationsBuilder.driver;
        this.locator = webDriverElementValidationsBuilder.locator;
        this.validationType = webDriverElementValidationsBuilder.validationType;
        this.validationMethod = webDriverElementValidationsBuilder.validationMethod;
        this.visualValidationEngine = webDriverElementValidationsBuilder.visualValidationEngine;
    }

    public ValidationsExecutor(ValidationsComparisonTypeManager validationsComparisonTypeManager) {
        this.validationCategory = validationsComparisonTypeManager.validationCategory;
        this.driver = validationsComparisonTypeManager.driver;
        this.locator = validationsComparisonTypeManager.locator;
        this.validationType = validationsComparisonTypeManager.validationType;
        this.validationMethod = validationsComparisonTypeManager.validationMethod;

        this.elementAttribute = validationsComparisonTypeManager.elementAttribute;
        this.validationComparisonType = validationsComparisonTypeManager.validationComparisonType;
        this.expectedValue = validationsComparisonTypeManager.expectedValue;
        this.actualValue = validationsComparisonTypeManager.actualValue;
        this.elementCssProperty = validationsComparisonTypeManager.elementCssProperty;
        this.browserAttribute = validationsComparisonTypeManager.browserAttribute;
    }

    public ValidationsExecutor(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationType = validationsBuilder.validationType;
        this.validationMethod = validationsBuilder.validationMethod;

        this.condition = validationsBuilder.condition;
        this.actualValue = validationsBuilder.actualValue;
    }

    public ValidationsExecutor withCustomReportMessage(String customReportMessage) {
        this.customReportMessage = customReportMessage;
        return this;
    }

    public void perform() {
        switch (validationMethod) {
            case "forceFail":
                ValidationHelper.validateFail(validationCategory, customReportMessage);
                break;
            case "objectsAreEqual":
                ValidationHelper.validateEquals(validationCategory, expectedValue, actualValue, validationComparisonType, validationType, customReportMessage);
                break;
            case "objectIsNull":
                ValidationHelper.validateNull(validationCategory, actualValue, validationType, customReportMessage);
                break;
            case "conditionIsTrue":
                ValidationHelper.validateTrue(validationCategory, condition, validationType, customReportMessage);
                break;
            case "elementExists":
                ValidationHelper.validateElementExists(validationCategory, driver, locator, validationType, customReportMessage);
                break;
            case "elementMatches":
                ValidationHelper.validateElementMatches(validationCategory, driver, locator, visualValidationEngine, validationType, customReportMessage);
                break;
            case "elementAttributeEquals":
                ValidationHelper.validateElementAttribute(validationCategory, driver, locator, elementAttribute, String.valueOf(expectedValue),
                        validationComparisonType, validationType, customReportMessage);
                break;
            case "elementCssPropertyEquals":
                ValidationHelper.validateElementCSSProperty(validationCategory, driver, locator, elementCssProperty, String.valueOf(expectedValue),
                        validationComparisonType, validationType, customReportMessage);
                break;
            case "browserAttributeEquals":
                ValidationHelper.validateBrowserAttribute(validationCategory, driver, browserAttribute, String.valueOf(expectedValue), validationComparisonType,
                        validationType, customReportMessage);
                break;
            case "comparativeRelationBetweenNumbers":
//                ValidationHelper.validateComparativeRelation(validationCategory, (Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType, customReportMessage);
                break;
            case "fileExists":
//                ValidationHelper.validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
                break;
            case "responseEqualsFileContent":
//                ValidationHelper.validateJSONFileContent(validationCategory, (Response) response, fileAbsolutePath, RestActions.ComparisonType.valueOf(validationComparisonType.name()), "", validationType, customReportMessage);
                break;
            case "jsonPathValueEquals":
//                ValidationHelper.validateEquals(validationCategory, expectedValue,
//                        RestActions.getResponseJSONValue(response, jsonPath), validationComparisonType,
//                        validationType, customReportMessage);
                break;
            default:
                break;
        }
    }
}
