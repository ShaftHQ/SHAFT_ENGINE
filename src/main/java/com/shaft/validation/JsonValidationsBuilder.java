package com.shaft.validation;

import com.shaft.cli.FileActions;

public class JsonValidationsBuilder {
    ValidationsBuilder validationsBuilder;
    Object response;
    String fileAbsolutePath;
    String jsonPath;

    public JsonValidationsBuilder(ValidationsBuilder validationsBuilder, Object response) {
        this.validationsBuilder = validationsBuilder;
        this.response = response;
    }

    public ValidationsAttributesBuilder responseEqualsFileContent(String fileRelativePath) {
        this.validationsBuilder.validationMethod = "responseEqualsFileContent";
        this.fileAbsolutePath = FileActions.getAbsolutePath(fileRelativePath);
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder jsonPathValueEquals(String jsonPath, String expectedValue) {
        this.validationsBuilder.validationMethod = "jsonPathValueEquals";
        this.validationsBuilder.expectedValue = expectedValue;
        this.jsonPath = jsonPath;
        return new ValidationsAttributesBuilder(this);
    }

}
