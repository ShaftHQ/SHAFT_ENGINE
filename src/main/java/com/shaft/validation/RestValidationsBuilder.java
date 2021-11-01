package com.shaft.validation;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;


public class RestValidationsBuilder {
    protected ValidationEnums.ValidationCategory validationCategory;
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected Object response;
    protected String fileAbsolutePath;
    protected RestActions.ComparisonType restComparisonType;
    protected String jsonPath;

    public RestValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, Object response) {
        this.validationCategory = validationCategory;
        this.response = response;
    }

    /**
     * Use this to check if the content of the provided actual response object is equal to the expected file content
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isEqualToFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object is not equal to the expected file content
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqualFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object contains the expected file content
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor containsFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check if the content of the provided actual response object does not contain the expected file content
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotContainFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to extract a certain value from the provided actual response object and check against it
     * @param jsonPath JSONPath of the target value; the JSONPath expression that will be evaluated in order to extract the desired value [without the trailing $.]
     *                 , please refer to these urls for examples:
     *                         https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html
     *                         http://jsonpath.com/
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder extractedJsonValue(String jsonPath) {
        this.validationMethod = "jsonPathValueEquals";
        this.jsonPath = jsonPath;
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check if the content of the provided actual response object matches the schema for the expected file content
     * @param fileRelativePath relative path to the target expected response file
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor checkResponseSchema(String fileRelativePath) {
        this.validationMethod = "checkResponseSchema";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }
}
