package com.shaft.validation;

public class FileValidationsBuilder {
    protected ValidationEnums.ValidationCategory validationCategory;
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected String folderRelativePath;
    protected String fileName;

    public FileValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, String folderRelativePath, String fileName) {
        this.validationCategory = validationCategory;
        this.folderRelativePath = folderRelativePath;
        this.fileName = fileName;
    }

    /**
     * Use this to check if a certain file exists
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor exists() {
        this.validationMethod = "fileExists";
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check if a certain file does not exist
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotExist() {
        this.validationMethod = "fileExists";
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }
}
