package io.github.shafthq.shaft.validations;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.tools.io.PdfFileManager;
import com.shaft.validation.ValidationEnums;
import io.github.shafthq.shaft.gui.browser.JavaScriptWaitManager;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.Objects;


public class ValidationsExecutor {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    protected final StringBuilder reportMessageBuilder;
    private WebDriver driver;
    private By locator;
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

        this.reportMessageBuilder = webDriverElementValidationsBuilder.reportMessageBuilder;
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

        this.folderRelativePath = nativeValidationsBuilder.folderRelativePath;
        this.fileName = nativeValidationsBuilder.fileName;

        this.reportMessageBuilder = nativeValidationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationType = validationsBuilder.validationType;
        this.validationMethod = validationsBuilder.validationMethod;

        this.condition = validationsBuilder.condition;
        this.actualValue = validationsBuilder.actualValue;

        this.reportMessageBuilder = validationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(NumberValidationsBuilder numberValidationsBuilder) {
        this.validationCategory = numberValidationsBuilder.validationCategory;
        this.validationType = numberValidationsBuilder.validationType;
        this.validationMethod = numberValidationsBuilder.validationMethod;

        this.expectedValue = numberValidationsBuilder.expectedValue;
        this.actualValue = numberValidationsBuilder.actualValue;

        this.numbersComparativeRelation = numberValidationsBuilder.numbersComparativeRelation;

        this.response = numberValidationsBuilder.response;
        this.jsonPath = numberValidationsBuilder.jsonPath;

        this.reportMessageBuilder = numberValidationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(RestValidationsBuilder restValidationsBuilder) {

        this.validationCategory = restValidationsBuilder.validationCategory;
        this.validationMethod = restValidationsBuilder.validationMethod;
        this.validationType = restValidationsBuilder.validationType;
        this.response = restValidationsBuilder.response;
        this.fileAbsolutePath = restValidationsBuilder.fileAbsolutePath;
        this.restComparisonType = restValidationsBuilder.restComparisonType;

        this.reportMessageBuilder = restValidationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(FileValidationsBuilder fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.validationType = fileValidationsBuilder.validationType;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;

        this.reportMessageBuilder = fileValidationsBuilder.reportMessageBuilder;
    }

    /**
     * Set a customized business-readable message that will appear in the execution report instead of the technical log message which will be nested under it
     *
     * @param customReportMessage the message that you would like to describe this validation in the execution report
     * @return the current ValidationsExecutor object so that you can call the "perform()" method and execute this validation
     */
    public ValidationsExecutor withCustomReportMessage(String customReportMessage) {
        this.customReportMessage = customReportMessage;
        return this;
    }

    /**
     * Execute this validation
     */
    public void perform() {
        JavaScriptWaitManager.waitForLazyLoading();
        if ("".equals(customReportMessage)) {
            customReportMessage = reportMessageBuilder.toString();
        }
        switch (validationMethod) {
            case "forceFail" -> ValidationsHelper.validateFail(validationCategory, customReportMessage);
            case "objectsAreEqual" -> ValidationsHelper.validateEquals(validationCategory, expectedValue, actualValue, validationComparisonType, validationType, customReportMessage);
            case "objectIsNull" -> ValidationsHelper.validateNull(validationCategory, actualValue, validationType, customReportMessage);
            case "conditionIsTrue" -> ValidationsHelper.validateTrue(validationCategory, condition, validationType, customReportMessage);
            case "elementExists" -> ValidationsHelper.validateElementExists(validationCategory, driver, locator, validationType, customReportMessage);
            case "elementMatches" -> ValidationsHelper.validateElementMatches(validationCategory, driver, locator, visualValidationEngine, validationType, customReportMessage);
            case "elementAttributeEquals" -> ValidationsHelper.validateElementAttribute(validationCategory, driver, locator, elementAttribute, String.valueOf(expectedValue),
                    validationComparisonType, validationType, customReportMessage);
            case "elementCssPropertyEquals" -> ValidationsHelper.validateElementCSSProperty(validationCategory, driver, locator, elementCssProperty, String.valueOf(expectedValue),
                    validationComparisonType, validationType, customReportMessage);
            case "browserAttributeEquals" -> ValidationsHelper.validateBrowserAttribute(validationCategory, driver, browserAttribute, String.valueOf(expectedValue), validationComparisonType,
                    validationType, customReportMessage);
            case "comparativeRelationBetweenNumbers" -> ValidationsHelper.validateComparativeRelation(validationCategory, (Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType, customReportMessage);
            case "fileExists" -> ValidationsHelper.validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
            case "responseEqualsFileContent" -> ValidationsHelper.validateJSONFileContent(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "jsonPathValueEquals" -> ValidationsHelper.validateEquals(validationCategory, expectedValue,
                    RestActions.getResponseJSONValue(response, jsonPath), validationComparisonType,
                    validationType, customReportMessage);
            case "jsonPathValueAsListEquals" -> {
                for (Object value : Objects.requireNonNull(RestActions.getResponseJSONValueAsList((Response) response, jsonPath))) {
                    ValidationsHelper.validateEquals(validationCategory, expectedValue, value.toString(), validationComparisonType, validationType, customReportMessage);
                }
            }
            case "responseBody" -> ValidationsHelper.validateEquals(validationCategory, expectedValue,
                    RestActions.getResponseBody((Response) response), validationComparisonType,
                    validationType, customReportMessage);
            case "responseTime" -> ValidationsHelper.validateComparativeRelation(validationCategory, (Number) expectedValue,
                    RestActions.getResponseTime((Response) response), numbersComparativeRelation, validationType, customReportMessage);
            case "checkResponseSchema" -> ValidationsHelper.validateResponseFileSchema(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "fileContent" -> {
                String fileContent;
                if (fileName.contains(".pdf")) {
                    fileContent = PdfFileManager.readFileContent(folderRelativePath + fileName);
                } else {
                    fileContent = FileActions.getInstance().readFile(folderRelativePath, fileName);
                }
                ValidationsHelper.validateEquals(validationCategory, expectedValue, fileContent, validationComparisonType, validationType, customReportMessage);
            }
            case "fileChecksum" -> {
                var fileChecksum = FileActions.getInstance().getFileChecksum(new TerminalActions(), folderRelativePath, fileName);
                ValidationsHelper.validateEquals(validationCategory, expectedValue, fileChecksum, validationComparisonType, validationType, customReportMessage);
            }
            default -> {
            }
        }
    }
}
