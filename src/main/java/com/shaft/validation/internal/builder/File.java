package com.shaft.validation.internal.builder;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.executor.GenericExecutor;
import lombok.Getter;

@Getter
public class File implements ValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected final String folderRelativePath;
    protected final String fileName;

    protected final StringBuilder reportMessageBuilder;

    public File(ValidationEnums.ValidationCategory validationCategory, String folderRelativePath, String fileName, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.folderRelativePath = folderRelativePath;
        this.fileName = fileName;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check if a certain file exists
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor exists() {
        this.validationMethod = "fileExists";
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("exists.");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check if a certain file does not exist
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor doesNotExist() {
        this.validationMethod = "fileExists";
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not exist.");
        return new GenericExecutor(this);
    }

    /**
     * Use this to calculate and check a certain file checksum to confirm if it has the exact same content or not
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @SuppressWarnings("unused")
    public Native checksum() {
        this.validationMethod = "fileChecksum";
        reportMessageBuilder.append("checksum ");
        return new Native(this);
    }

    /**
     * Use this to attempt to read and validate a certain file content (works for PDF and TEXT files)
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public Native content() {
        this.validationMethod = "fileContent";
        reportMessageBuilder.append("content ");
        return new Native(this);
    }
}
