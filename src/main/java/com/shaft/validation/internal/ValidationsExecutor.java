package com.shaft.validation.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.tools.io.PdfFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ProgressBarLogger;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Step;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.Objects;

public class ValidationsExecutor {
    protected final StringBuilder reportMessageBuilder;
    private final ValidationEnums.ValidationCategory validationCategory;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private String validationCategoryString;
    private final ThreadLocal<WebDriver> driver = new ThreadLocal<>();
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
    private final ThreadLocal<Object> response = new ThreadLocal<>();
    private String fileAbsolutePath;
    private RestActions.ComparisonType restComparisonType;
    private String jsonPath;
    private String folderRelativePath;
    private String fileName;

    public ValidationsExecutor(WebDriverElementValidationsBuilder webDriverElementValidationsBuilder) {
        this.validationCategory = webDriverElementValidationsBuilder.validationCategory;
        this.driver.set(webDriverElementValidationsBuilder.driver);
        this.locator = webDriverElementValidationsBuilder.locator;
        this.validationType = webDriverElementValidationsBuilder.validationType;
        this.validationMethod = webDriverElementValidationsBuilder.validationMethod;
        this.visualValidationEngine = webDriverElementValidationsBuilder.visualValidationEngine;
        this.reportMessageBuilder = webDriverElementValidationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(NativeValidationsBuilder nativeValidationsBuilder) {
        this.validationCategory = nativeValidationsBuilder.validationCategory;
        this.driver.set(nativeValidationsBuilder.driver);
        this.locator = nativeValidationsBuilder.locator;
        this.validationType = nativeValidationsBuilder.validationType;
        this.validationMethod = nativeValidationsBuilder.validationMethod;
        this.elementAttribute = nativeValidationsBuilder.elementAttribute;
        this.validationComparisonType = nativeValidationsBuilder.validationComparisonType;
        this.expectedValue = nativeValidationsBuilder.expectedValue;
        this.actualValue = nativeValidationsBuilder.actualValue;
        this.elementCssProperty = nativeValidationsBuilder.elementCssProperty;
        this.browserAttribute = nativeValidationsBuilder.browserAttribute;

        this.response.set(nativeValidationsBuilder.response);
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

        this.response.set(numberValidationsBuilder.response);
        this.jsonPath = numberValidationsBuilder.jsonPath;

        this.reportMessageBuilder = numberValidationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(RestValidationsBuilder restValidationsBuilder) {

        this.validationCategory = restValidationsBuilder.validationCategory;
        this.validationMethod = restValidationsBuilder.validationMethod;
        this.validationType = restValidationsBuilder.validationType;
        this.response.set(restValidationsBuilder.response);
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
        ReportManager.log(customReportMessage);
    }

    protected void internalPerform() {
        JavaScriptWaitManager.waitForLazyLoading(driver.get());
        boolean clearCustomReportMessage = false;
        if (customReportMessage.isBlank()) {
            customReportMessage = reportMessageBuilder.toString();
            clearCustomReportMessage = true;
        }
        this.validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert" : "Verify";
        ReportManager.logDiscrete(this.validationCategoryString + " that " + this.customReportMessage);
        try (ProgressBarLogger pblogger = new ProgressBarLogger(this.validationCategoryString.equals("Assert") ? "Asserting..." : "Verifying...")) {
            // perform validation
            performValidation();
        }
        if (Boolean.TRUE.equals(clearCustomReportMessage))
            customReportMessage = "";
        driver.remove();
        response.remove();
    }

    @Step(" {this.validationCategoryString} that {this.customReportMessage}")
    private void performValidation() {
        switch (validationMethod) {
            case "forceFail" -> new ValidationsHelper().validateFail(validationCategory, customReportMessage);
            case "objectsAreEqual" ->
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, actualValue, validationComparisonType, validationType);
            case "conditionIsTrue" ->
                    new ValidationsHelper().validateTrue(validationCategory, condition, validationType, customReportMessage);
            case "elementExists" ->
                    new ValidationsHelper2(validationCategory).validateElementExists(driver.get(), locator, validationType);
            case "elementMatches" ->
                    new ValidationsHelper().validateElementMatches(validationCategory, driver.get(), locator, visualValidationEngine, validationType, customReportMessage);
            case "elementAttributeEquals" ->
                    new ValidationsHelper2(validationCategory).validateElementAttribute(driver.get(), locator, elementAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "elementCssPropertyEquals" ->
                    new ValidationsHelper2(validationCategory).validateElementCSSProperty(driver.get(), locator, elementCssProperty, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "browserAttributeEquals" ->
                    new ValidationsHelper2(validationCategory).validateBrowserAttribute(driver.get(), browserAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "comparativeRelationBetweenNumbers" ->
                    new ValidationsHelper2(validationCategory).validateNumber((Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType);
            case "fileExists" ->
                    new ValidationsHelper().validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
            case "responseEqualsFileContent" ->
                    new ValidationsHelper().validateJSONFileContent(validationCategory, (Response) response.get(), fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "jsonPathValueEquals" ->
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, RestActions.getResponseJSONValue(response.get(), jsonPath), validationComparisonType, validationType);
            case "jsonPathValueAsListEquals" -> {
                for (Object value : Objects.requireNonNull(RestActions.getResponseJSONValueAsList((Response) response.get(), jsonPath))) {
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, value.toString(), validationComparisonType, validationType);
                }
            }
            case "responseBody" ->
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, RestActions.getResponseBody((Response) response.get()), validationComparisonType, validationType);
            case "responseTime" ->
                    new ValidationsHelper2(validationCategory).validateNumber((Number) expectedValue, RestActions.getResponseTime((Response) response.get()), numbersComparativeRelation, validationType);
            case "checkResponseSchema" ->
                    new ValidationsHelper().validateResponseFileSchema(validationCategory, (Response) response.get(), fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "fileContent" -> {
                String fileContent;
                if (fileName.contains(".pdf")) {
                    fileContent = PdfFileManager.readFileContent(folderRelativePath + fileName);
                } else {
                    fileContent = FileActions.getInstance(true).readFile(folderRelativePath, fileName);
                }
                new ValidationsHelper2(validationCategory).validateEquals(expectedValue, fileContent, validationComparisonType, validationType);
            }
            case "fileChecksum" -> {
                var fileChecksum = FileActions.getInstance(true).getFileChecksum(new TerminalActions(), folderRelativePath, fileName);
                new ValidationsHelper2(validationCategory).validateEquals(expectedValue, fileChecksum, validationComparisonType, validationType);
            }
            default -> {
            }
        }
    }
}
