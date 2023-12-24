package com.shaft.validation.internal.builder;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.executor.GenericExecutor;
import lombok.Getter;


@SuppressWarnings("unused")
@Getter
public class API implements ValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected final Object response;
    protected String fileAbsolutePath;
    protected RestActions.ComparisonType restComparisonType;
    protected String jsonPath;

    protected final StringBuilder reportMessageBuilder;

    public API(ValidationEnums.ValidationCategory validationCategory, Object response, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.response = response;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check if the content of the provided actual response object is equal to the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor isEqualToFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is equal to the contents of this file \"").append(fileRelativePath).append("\".");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object is equal to the expected file content (Ignoring Order)
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor isEqualToFileContentIgnoringOrder(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS_IGNORING_ORDER;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is equal to the contents of this file \"").append(fileRelativePath).append("\" (Ignoring Ordering).");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object is not equal to the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor doesNotEqualFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("is not equal to the contents of this file \"").append(fileRelativePath).append("\".");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object is not equal to the expected file content (Ignoring Order)
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor doesNotEqualFileContentIgnoringOrder(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS_IGNORING_ORDER;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("is not equal to the contents of this file \"").append(fileRelativePath).append("\" (Ignoring Ordering).");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object contains the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor containsFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("contains the contents of this file \"").append(fileRelativePath).append("\".");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object does not contain the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor doesNotContainFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not contain the contents of this file \"").append(fileRelativePath).append("\".");
        return new GenericExecutor(this);
    }

    /**
     * Use this to extract a certain value from the provided actual response object and check against it
     *
     * @param jsonPath JSONPath of the target value; the JSONPath expression that will be evaluated in order to extract the desired value [without the trailing $.]
     *                 , please refer to these urls for examples:
     *                 <a href="https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html">SmartBear.com/jsonpath</a>
     *                 <a href="http://jsonpath.com/">jsonpath.com/</a>
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public Native extractedJsonValue(String jsonPath) {
        this.validationMethod = "jsonPathValueEquals";
        this.jsonPath = jsonPath;
        reportMessageBuilder.append("extracted value from the JSON path \"").append(jsonPath).append("\" ");
        return new Native(this);
    }

    /**
     * Use this to extract a certain value from the provided actual response object as list and check every item against it
     *
     * @param jsonPath JSONPath of the target value; the JSONPath expression that will be evaluated in order to extract the desired value [without the trailing $.]
     *                 , please refer to these urls for examples:
     *                 <a href="https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html">SmartBear.com/jsonpath</a>
     *                 <a href="http://jsonpath.com/">jsonpath.com/</a>
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public Native extractedJsonValueAsList(String jsonPath) {
        this.validationMethod = "jsonPathValueAsListEquals";
        this.jsonPath = jsonPath;
        reportMessageBuilder.append("extracted value from the JSON path \"").append(jsonPath).append("\" ");
        return new Native(this);
    }

    public Native body() {
        this.validationMethod = "responseBody";
        reportMessageBuilder.append("Body ");
        return new JSON(this);
    }

    public Number time() {
        this.validationMethod = "responseTime";
        reportMessageBuilder.append("Time ");
        return new Number(this);
    }

    /**
     * Use this to check if the content of the provided actual response object matches the schema for the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor matchesSchema(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "checkResponseSchema";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("schema matches that in this file \"").append(fileRelativePath).append("\".");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object matches the schema for the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor doesNotMatchSchema(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "checkResponseSchema";
        this.fileAbsolutePath = FileActions.getInstance().getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("schema does not match that in this file \"").append(fileRelativePath).append("\".");
        return new GenericExecutor(this);
    }
}
