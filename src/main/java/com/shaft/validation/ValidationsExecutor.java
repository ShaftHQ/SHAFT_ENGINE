package com.shaft.validation;

import com.shaft.api.RestActions;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.io.File;

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

    ValidationEnums.NumbersComparativeRelation numbersComparativeRelation;

    Object response;
    String fileAbsolutePath;
    RestActions.ComparisonType restComparisonType;
    File fileRelativePathObject;
    String jsonPath;

    String folderRelativePath;
    String fileName;

    public ValidationsExecutor(WebDriverElementValidationsBuilder webDriverElementValidationsBuilder) {
        this.validationCategory = webDriverElementValidationsBuilder.validationCategory;
        this.driver = webDriverElementValidationsBuilder.driver;
        this.locator = webDriverElementValidationsBuilder.locator;
        this.validationType = webDriverElementValidationsBuilder.validationType;
        this.validationMethod = webDriverElementValidationsBuilder.validationMethod;
        this.visualValidationEngine = webDriverElementValidationsBuilder.visualValidationEngine;
    }

    public ValidationsExecutor(NativeValidationsBuilder nativeValidationsBuilder) {
        this.validationCategory = nativeValidationsBuilder.validationCategory;
        this.driver = nativeValidationsBuilder.driver;
        this.locator = nativeValidationsBuilder.locator;
        this.validationType = nativeValidationsBuilder.validationType;
        this.validationMethod = nativeValidationsBuilder.validationMethod;

        this.elementAttribute = nativeValidationsBuilder.elementAttribute;
        this.validationComparisonType = nativeValidationsBuilder.validationComparisonType;
        this.expectedValue = nativeValidationsBuilder.expectedValue;
        this.actualValue = nativeValidationsBuilder.actualValue;
        this.elementCssProperty = nativeValidationsBuilder.elementCssProperty;
        this.browserAttribute = nativeValidationsBuilder.browserAttribute;

        this.response = nativeValidationsBuilder.response;
        this.jsonPath = nativeValidationsBuilder.jsonPath;
    }

    public ValidationsExecutor(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationType = validationsBuilder.validationType;
        this.validationMethod = validationsBuilder.validationMethod;

        this.condition = validationsBuilder.condition;
        this.actualValue = validationsBuilder.actualValue;
    }

    public ValidationsExecutor(NumberValidationsBuilder numberValidationsBuilder) {
        this.validationCategory = numberValidationsBuilder.validationCategory;
        this.validationType = numberValidationsBuilder.validationType;
        this.validationMethod = numberValidationsBuilder.validationMethod;

        this.expectedValue = numberValidationsBuilder.expectedValue;
        this.actualValue = numberValidationsBuilder.actualValue;

        this.numbersComparativeRelation = numberValidationsBuilder.numbersComparativeRelation;
    }

    public ValidationsExecutor(RestValidationsBuilder restValidationsBuilder) {

        this.validationCategory = restValidationsBuilder.validationCategory;
        this.validationMethod = restValidationsBuilder.validationMethod;
        this.validationType = restValidationsBuilder.validationType;
        this.response = restValidationsBuilder.response;
        this.fileAbsolutePath = restValidationsBuilder.fileAbsolutePath;
        this.fileRelativePathObject = restValidationsBuilder.fileRelativePathObject;

        this.restComparisonType = restValidationsBuilder.restComparisonType;

    }

    public ValidationsExecutor(FileValidationsBuilder fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.validationType = fileValidationsBuilder.validationType;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;
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
                ValidationHelper.validateComparativeRelation(validationCategory, (Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType, customReportMessage);
                break;
            case "fileExists":
                ValidationHelper.validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
                break;
            case "responseEqualsFileContent":
                ValidationHelper.validateJSONFileContent(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
                break;
            case "jsonPathValueEquals":
                ValidationHelper.validateEquals(validationCategory, expectedValue,
                        RestActions.getResponseJSONValue(response, jsonPath), validationComparisonType,
                        validationType, customReportMessage);
                break;
            case "checkResponseSchema":
                ValidationHelper.validateResponseFileSchema(validationCategory, (Response) response, fileRelativePathObject, restComparisonType, "", validationType, customReportMessage);
                break;
            default:
                break;
        }
    }
}
