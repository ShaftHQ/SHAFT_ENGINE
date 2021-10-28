package com.shaft.validation;

import com.shaft.api.RestActions;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;


public class ValidationsExecutor {
    private final ValidationEnums.ValidationCategory validationCategory;
    private WebDriver driver;
    private By locator;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    private String customReportMessage = "";

    private ValidationEnums.VisualValidationEngine visualValidationEngine;

    private String elementAttribute;
    private String elementCssProperty;
    private String browserAttribute;

    private ValidationEnums.ValidationComparisonType validationComparisonType;
    private Object expectedValue;

    private boolean condition;
    private Object actualValue;

    private ValidationEnums.NumbersComparativeRelation numbersComparativeRelation;

    private Object response;
    private String fileAbsolutePath;
    private RestActions.ComparisonType restComparisonType;

    private String jsonPath;
    private String folderRelativePath;
    private String fileName;

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
        this.restComparisonType = restValidationsBuilder.restComparisonType;

    }

    public ValidationsExecutor(FileValidationsBuilder fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.validationType = fileValidationsBuilder.validationType;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;
    }

    /**
     * Set a customized business-readable message that will appear in the execution report instead of the technical log message which will be nested under it
     * @param customReportMessage the message that you would like to describe this validation in the execution report
     * @return the current ValidationsExecutor object so that you can call the perform() method and execute this validation
     */
    public ValidationsExecutor withCustomReportMessage(String customReportMessage) {
        this.customReportMessage = customReportMessage;
        return this;
    }

    /**
     * Execute this validation
     */
    public void perform() {
        switch (validationMethod) {
            case "forceFail" -> ValidationHelper.validateFail(validationCategory, customReportMessage);
            case "objectsAreEqual" -> ValidationHelper.validateEquals(validationCategory, expectedValue, actualValue, validationComparisonType, validationType, customReportMessage);
            case "objectIsNull" -> ValidationHelper.validateNull(validationCategory, actualValue, validationType, customReportMessage);
            case "conditionIsTrue" -> ValidationHelper.validateTrue(validationCategory, condition, validationType, customReportMessage);
            case "elementExists" -> ValidationHelper.validateElementExists(validationCategory, driver, locator, validationType, customReportMessage);
            case "elementMatches" -> ValidationHelper.validateElementMatches(validationCategory, driver, locator, visualValidationEngine, validationType, customReportMessage);
            case "elementAttributeEquals" -> ValidationHelper.validateElementAttribute(validationCategory, driver, locator, elementAttribute, String.valueOf(expectedValue),
                    validationComparisonType, validationType, customReportMessage);
            case "elementCssPropertyEquals" -> ValidationHelper.validateElementCSSProperty(validationCategory, driver, locator, elementCssProperty, String.valueOf(expectedValue),
                    validationComparisonType, validationType, customReportMessage);
            case "browserAttributeEquals" -> ValidationHelper.validateBrowserAttribute(validationCategory, driver, browserAttribute, String.valueOf(expectedValue), validationComparisonType,
                    validationType, customReportMessage);
            case "comparativeRelationBetweenNumbers" -> ValidationHelper.validateComparativeRelation(validationCategory, (Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType, customReportMessage);
            case "fileExists" -> ValidationHelper.validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
            case "responseEqualsFileContent" -> ValidationHelper.validateJSONFileContent(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "jsonPathValueEquals" -> ValidationHelper.validateEquals(validationCategory, expectedValue,
                    RestActions.getResponseJSONValue(response, jsonPath), validationComparisonType,
                    validationType, customReportMessage);
            case "checkResponseSchema" -> ValidationHelper.validateResponseFileSchema(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            default -> {
            }
        }
    }
}
