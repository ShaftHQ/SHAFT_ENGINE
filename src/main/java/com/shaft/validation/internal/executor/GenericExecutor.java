package com.shaft.validation.internal.executor;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.PdfFileManager;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.builder.Number;
import com.shaft.validation.internal.builder.*;
import io.qameta.allure.Step;
import io.restassured.response.Response;

import java.util.Objects;


public class GenericExecutor implements ValidationsExecutor {
    protected final StringBuilder reportMessageBuilder;
    private final ValidationEnums.ValidationCategory validationCategory;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    private String validationCategoryString;
    private String validationMethodString;
    private String customReportMessage = "";
    private ValidationEnums.ValidationComparisonType validationComparisonType;
    private Object expectedValue;
    private Object actualValue;
    private ValidationEnums.NumbersComparativeRelation numbersComparativeRelation;
    private Object response;
    private String fileAbsolutePath;
    private RestActions.ComparisonType restComparisonType;
    private String jsonPath;
    private String folderRelativePath;
    private String fileName;

    public GenericExecutor(Native nativeValidationsBuilder) {
        this.validationCategory = nativeValidationsBuilder.getValidationCategory();
        this.validationType = nativeValidationsBuilder.getValidationType();
        this.validationMethod = nativeValidationsBuilder.getValidationMethod();

        this.validationComparisonType = nativeValidationsBuilder.getValidationComparisonType();
        this.expectedValue = nativeValidationsBuilder.getExpectedValue();
        this.actualValue = nativeValidationsBuilder.getActualValue();

        this.response = nativeValidationsBuilder.getResponse();
        this.jsonPath = nativeValidationsBuilder.getJsonPath();

        this.folderRelativePath = nativeValidationsBuilder.getFolderRelativePath();
        this.fileName = nativeValidationsBuilder.getFileName();

        this.reportMessageBuilder = nativeValidationsBuilder.getReportMessageBuilder();
    }

    public GenericExecutor(Standalone validationsBuilder) {
        this.validationCategory = validationsBuilder.getValidationCategory();
        this.validationType = validationsBuilder.getValidationType();
        this.validationMethod = validationsBuilder.getValidationMethod();

        this.actualValue = validationsBuilder.getActualValue();

        this.reportMessageBuilder = validationsBuilder.getReportMessageBuilder();
    }

    public GenericExecutor(Number numberValidationsBuilder) {
        this.validationCategory = numberValidationsBuilder.getValidationCategory();
        this.validationType = numberValidationsBuilder.getValidationType();
        this.validationMethod = numberValidationsBuilder.getValidationMethod();

        this.expectedValue = numberValidationsBuilder.getExpectedValue();
        this.actualValue = numberValidationsBuilder.getActualValue();

        this.numbersComparativeRelation = numberValidationsBuilder.getNumbersComparativeRelation();

        this.response = numberValidationsBuilder.getResponse();
        this.jsonPath = numberValidationsBuilder.getJsonPath();

        this.reportMessageBuilder = numberValidationsBuilder.getReportMessageBuilder();
    }

    public GenericExecutor(API restValidationsBuilder) {

        this.validationCategory = restValidationsBuilder.getValidationCategory();
        this.validationType = restValidationsBuilder.getValidationType();
        this.validationMethod = restValidationsBuilder.getValidationMethod();

        this.response = restValidationsBuilder.getResponse();
        this.fileAbsolutePath = restValidationsBuilder.getFileAbsolutePath();
        this.restComparisonType = restValidationsBuilder.getRestComparisonType();

        this.reportMessageBuilder = restValidationsBuilder.getReportMessageBuilder();
    }

    public GenericExecutor(File fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.getValidationCategory();
        this.validationType = fileValidationsBuilder.getValidationType();
        this.validationMethod = fileValidationsBuilder.getValidationMethod();

        this.folderRelativePath = fileValidationsBuilder.getFolderRelativePath();
        this.fileName = fileValidationsBuilder.getFileName();

        this.reportMessageBuilder = fileValidationsBuilder.getReportMessageBuilder();
    }

    /**
     * Set a customized business-readable message that will appear in the execution report instead of the technical log message which will be nested under it
     *
     * @param customReportMessage the message that you would like to describe this validation in the execution report
     * @return the current ValidationsExecutor object so that you can call the "perform()" method and execute this validation
     */
    public GenericExecutor withCustomReportMessage(String customReportMessage) {
        this.customReportMessage = customReportMessage;
        return this;
    }

    /**
     * Execute this validation
     */
    public void perform() {
        if (customReportMessage.isBlank()) {
            customReportMessage = reportMessageBuilder.toString();
        }
        validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert that" : "Verify that";
        validationMethodString = JavaHelper.convertToSentenceCase(validationMethod).toLowerCase();
        performValidation();
    }

    @Step(" {this.validationCategoryString} {this.validationMethodString}")
    private ValidationsBuilder performValidation() {
        switch (validationMethod) {
            case "forceFail" -> Helper.validateFail(validationCategory, customReportMessage);
            case "objectsAreEqual" ->
                    Helper.validateEquals(validationCategory, expectedValue, actualValue, validationComparisonType, validationType, customReportMessage);
            case "objectIsNull" ->
                    Helper.validateNull(validationCategory, actualValue, validationType, customReportMessage);
            case "comparativeRelationBetweenNumbers" -> {
                Helper.validateComparativeRelation(validationCategory, (java.lang.Number) expectedValue, (java.lang.Number) actualValue, numbersComparativeRelation, validationType, customReportMessage);
                return new Standalone(validationCategory).number((java.lang.Number) actualValue);
            }
            case "fileExists" -> {
                Helper.validateFileExists(validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
                return new Standalone(validationCategory).file(folderRelativePath, fileName);
            }
            case "responseEqualsFileContent" -> {
                Helper.validateJSONFileContent(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
                return new Standalone(validationCategory).response(response);
            }
            case "jsonPathValueEquals" -> {
                Helper.validateEquals(validationCategory, expectedValue, RestActions.getResponseJSONValue(response, jsonPath), validationComparisonType, validationType, customReportMessage);
                return new Standalone(validationCategory).response(response);
            }
            case "jsonPathValueAsListEquals" -> {
                for (Object value : Objects.requireNonNull(RestActions.getResponseJSONValueAsList((Response) response, jsonPath))) {
                    Helper.validateEquals(validationCategory, expectedValue, value.toString(), validationComparisonType, validationType, customReportMessage);
                    return new Standalone(validationCategory).response(response);
                }
            }
            case "responseBody" -> {
                Helper.validateEquals(validationCategory, expectedValue, RestActions.getResponseBody((Response) response), validationComparisonType, validationType, customReportMessage);
                return new Standalone(validationCategory).response(response);
            }
            case "responseTime" -> {
                Helper.validateComparativeRelation(validationCategory, (java.lang.Number) expectedValue, RestActions.getResponseTime((Response) response), numbersComparativeRelation, validationType, customReportMessage);
                return new Standalone(validationCategory).response(response);
            }
            case "checkResponseSchema" -> {
                Helper.validateResponseFileSchema(validationCategory, (Response) response, fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
                return new Standalone(validationCategory).response(response);
            }
            case "fileContent" -> {
                String fileContent;
                if (fileName.contains(".pdf")) {
                    fileContent = PdfFileManager.readFileContent(folderRelativePath + fileName);
                } else {
                    fileContent = FileActions.getInstance().readFile(folderRelativePath, fileName);
                }
                Helper.validateEquals(validationCategory, expectedValue, fileContent, validationComparisonType, validationType, customReportMessage);
                return new Standalone(validationCategory).file(folderRelativePath, fileName);
            }
            case "fileChecksum" -> {
                var fileChecksum = FileActions.getInstance().getFileChecksum(new TerminalActions(), folderRelativePath, fileName);
                Helper.validateEquals(validationCategory, expectedValue, fileChecksum, validationComparisonType, validationType, customReportMessage);
                return new Standalone(validationCategory).file(folderRelativePath, fileName);
            }
        }
        return new Standalone(validationCategory);
    }
}
