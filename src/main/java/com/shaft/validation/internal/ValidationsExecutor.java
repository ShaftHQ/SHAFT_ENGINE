package com.shaft.validation.internal;

import com.google.common.util.concurrent.AtomicDouble;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.tools.internal.support.CreateVirtualThread;
import com.shaft.tools.io.PdfFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Step;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import static com.shaft.tools.internal.support.JavaHelper.printProgressBar;
import static com.shaft.tools.io.internal.ReportManagerHelper.*;


public class ValidationsExecutor {
    protected final StringBuilder reportMessageBuilder;
    private final ValidationEnums.ValidationCategory validationCategory;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private String validationCategoryString;
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
    /**
     * The following variables are used
     * for printing the progress bar
     * **/
/*    int timeoutVal = (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
    CreateVirtualThread progressBarThread;
    Runnable task = () -> {
        try {
            initializeProgressBarConfig();
            printProgressBar(timeoutVal);
            returnToDefaultConfig();
            printNewlineAfterProgressBar();
        } catch (InterruptedException e) {
            returnToDefaultConfig();
            printNewlineAfterProgressBar();
            throw new RuntimeException(e);
        }
    };*/
    /******************************************************************/
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
        ReportManager.log(customReportMessage);
    }

    protected void internalPerform() {
        JavaScriptWaitManager.waitForLazyLoading(driver);
        boolean clearCustomReportMessage = false;
        if (customReportMessage.isBlank()) {
            customReportMessage = reportMessageBuilder.toString();
            clearCustomReportMessage = true;
        }
        this.validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert" : "Verify";
        performValidation();
        if (Boolean.TRUE.equals(clearCustomReportMessage))
            customReportMessage = "";
    }

    @Step(" {this.validationCategoryString} that {this.customReportMessage}")
    private void performValidation() {
        /**start the progress bar thread**/
//            progressBarThread = new CreateVirtualThread();
//            progressBarThread.runThreadWithTask(task);
        /************************************/
        switch (validationMethod) {
            case "forceFail" -> new ValidationsHelper().validateFail(validationCategory, customReportMessage);
            case "objectsAreEqual" ->
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, actualValue, validationComparisonType, validationType);
            case "conditionIsTrue" ->
                    new ValidationsHelper().validateTrue(validationCategory, condition, validationType, customReportMessage);
            case "elementExists" ->
                    new ValidationsHelper2(validationCategory).validateElementExists(driver, locator, validationType);
            case "elementMatches" ->
                    new ValidationsHelper().validateElementMatches(validationCategory, driver, locator, visualValidationEngine, validationType, customReportMessage);
            case "elementAttributeEquals" ->
                    new ValidationsHelper2(validationCategory).validateElementAttribute(driver, locator, elementAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "elementCssPropertyEquals" ->
                    new ValidationsHelper2(validationCategory).validateElementCSSProperty(driver, locator, elementCssProperty, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "browserAttributeEquals" ->
                    new ValidationsHelper2(validationCategory).validateBrowserAttribute(driver, browserAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "comparativeRelationBetweenNumbers" ->
                    new ValidationsHelper2(validationCategory).validateNumber((Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType);
            case "fileExists" ->
                    new ValidationsHelper().validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
            case "responseEqualsFileContent" ->
                    new ValidationsHelper().validateJSONFileContent(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "jsonPathValueEquals" ->
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, RestActions.getResponseJSONValue(response, jsonPath), validationComparisonType, validationType);
            case "jsonPathValueAsListEquals" -> {
                for (Object value : Objects.requireNonNull(RestActions.getResponseJSONValueAsList((Response) response, jsonPath))) {
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, value.toString(), validationComparisonType, validationType);
                }
            }
            case "responseBody" ->
                    new ValidationsHelper2(validationCategory).validateEquals(expectedValue, RestActions.getResponseBody((Response) response), validationComparisonType, validationType);
            case "responseTime" ->
                    new ValidationsHelper2(validationCategory).validateNumber((Number) expectedValue, RestActions.getResponseTime((Response) response), numbersComparativeRelation, validationType);
            case "checkResponseSchema" ->
                    new ValidationsHelper().validateResponseFileSchema(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
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
        // this is used to stop the progress bar thread in case the assertion succeeded
//        progressBarThread.stopThreadNow();
    }
}
