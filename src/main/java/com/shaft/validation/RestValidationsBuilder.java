package com.shaft.validation;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;

public class RestValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    String validationMethod;
    ValidationEnums.ValidationType validationType;
    Object response;
    String fileAbsolutePath;
    RestActions.ComparisonType restComparisonType;
    String jsonPath;

    public RestValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, Object response) {
        this.validationCategory = validationCategory;
        this.response = response;
    }

    public ValidationsExecutor isEqualToFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotEqualFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor containsFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotContainFileContent(String fileRelativePath) {
        this.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        this.restComparisonType = RestActions.ComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public NativeValidationsBuilder extractedJsonValue(String jsonPath) {
        this.validationMethod = "jsonPathValueEquals";
        this.jsonPath = jsonPath;
        return new NativeValidationsBuilder(this);
    }
}
