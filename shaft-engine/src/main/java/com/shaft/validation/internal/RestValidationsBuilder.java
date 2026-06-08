package com.shaft.validation.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.validation.ValidationEnums;


@SuppressWarnings("unused")
public class RestValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final Object response;
    protected final StringBuilder reportMessageBuilder;
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected String fileAbsolutePath;
    protected RestActions.ComparisonType restComparisonType;
    protected String jsonPath;

    public RestValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, Object response, StringBuilder reportMessageBuilder) {
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
    public ValidationsExecutor isEqualToFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is equal to the contents of this file \"").append(fileRelativePath).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check if the content of the provided actual response object is equal to the expected file content (Ignoring Order)
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isEqualToFileContentIgnoringOrder(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS_IGNORING_ORDER;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is equal to the contents of this file \"").append(fileRelativePath).append("\" (Ignoring Ordering).");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check if the content of the provided actual response object is not equal to the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqualFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("is not equal to the contents of this file \"").append(fileRelativePath).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check if the content of the provided actual response object is not equal to the expected file content (Ignoring Order)
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqualFileContentIgnoringOrder(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS_IGNORING_ORDER;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("is not equal to the contents of this file \"").append(fileRelativePath).append("\" (Ignoring Ordering).");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check if the content of the provided actual response object contains the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor containsFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("contains the contents of this file \"").append(fileRelativePath).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check if the content of the provided actual response object does not contain the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotContainFileContent(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not contain the contents of this file \"").append(fileRelativePath).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
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
    public NativeValidationsBuilder extractedJsonValue(String jsonPath) {
        this.validationMethod = "jsonPathValueEquals";
        this.jsonPath = jsonPath;
        reportMessageBuilder.append("extracted value from the JSON path \"").append(jsonPath).append("\" ");
        return new NativeValidationsBuilder(this);
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
    public NativeValidationsBuilder extractedJsonValueAsList(String jsonPath) {
        this.validationMethod = "jsonPathValueAsListEquals";
        this.jsonPath = jsonPath;
        reportMessageBuilder.append("extracted value from the JSON path \"").append(jsonPath).append("\" ");
        return new NativeValidationsBuilder(this);
    }

    public NativeValidationsBuilder body() {
        this.validationMethod = "responseBody";
        reportMessageBuilder.append("Body ");
        return new JSONValidationsBuilder(this);
    }

    public NumberValidationsBuilder time() {
        this.validationMethod = "responseTime";
        reportMessageBuilder.append("Time ");
        return new NumberValidationsBuilder(this);
    }

    /**
     * Use this to check if the content of the provided actual response object matches the schema for the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor matchesSchema(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "checkResponseSchema";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("schema matches that in this file \"").append(fileRelativePath).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check if the content of the provided actual response object matches the schema for the expected file content
     *
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotMatchSchema(String fileRelativePath) {
        fileRelativePath = JavaHelper.appendTestDataToRelativePath(fileRelativePath);
        this.validationMethod = "checkResponseSchema";
        this.fileAbsolutePath = FileActions.getInstance(true).getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("schema does not match that in this file \"").append(fileRelativePath).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }
}
