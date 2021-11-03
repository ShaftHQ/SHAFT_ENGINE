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

    /**
     * Use this to calculate and check a certain file checksum to confirm if it has the exact same content or not
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder checksum() {
        this.validationMethod = "fileChecksum";
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to attempt to read and validate a certain file content (works for PDF and TEXT files)
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder content(){
        this.validationMethod = "fileContent";
        return new NativeValidationsBuilder(this);
    }
}
