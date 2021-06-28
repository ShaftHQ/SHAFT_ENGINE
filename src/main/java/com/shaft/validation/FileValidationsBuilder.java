package com.shaft.validation;

public class FileValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    String validationMethod;
    ValidationEnums.ValidationType validationType;
    String folderRelativePath;
    String fileName;

    public FileValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, String folderRelativePath, String fileName) {
        this.validationCategory = validationCategory;
        this.folderRelativePath = folderRelativePath;
        this.fileName = fileName;
    }

    public ValidationsExecutor exists() {
        this.validationMethod = "fileExists";
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotExist() {
        this.validationMethod = "fileExists";
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }
}
