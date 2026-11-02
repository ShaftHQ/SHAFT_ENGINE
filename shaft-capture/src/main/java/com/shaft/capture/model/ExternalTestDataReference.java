package com.shaft.capture.model;

/**
 * Reference to external test data without retaining the original value.
 *
 * @param id stable data identifier
 * @param logicalName reviewer-friendly field name
 * @param source external data source
 * @param relativePath optional sanitized data-file path
 * @param jsonPointer optional JSON pointer within the data file
 * @param classification privacy classification
 */
public record ExternalTestDataReference(
        String id,
        String logicalName,
        DataSource source,
        String relativePath,
        String jsonPointer,
        DataClassification classification) {
    /**
     * External data sources supported by generated tests.
     */
    public enum DataSource {
        JSON,
        ENVIRONMENT,
        SECRET_PROVIDER,
        FILE_FIXTURE
    }

    /**
     * Deterministic privacy classifications.
     */
    public enum DataClassification {
        ORDINARY,
        SENSITIVE,
        SECRET,
        UPLOAD
    }

    /**
     * Creates an immutable external test-data reference.
     */
    public ExternalTestDataReference {
        id = ModelSupport.requireText(id, "Data reference ID");
        logicalName = ModelSupport.requireText(logicalName, "Logical data name");
        source = source == null ? DataSource.JSON : source;
        relativePath = ModelSupport.relativePath(relativePath, "Data path");
        jsonPointer = ModelSupport.text(jsonPointer);
        classification = classification == null ? DataClassification.ORDINARY : classification;
        if (source == DataSource.JSON && (relativePath.isBlank() || jsonPointer.isBlank())) {
            throw new IllegalArgumentException("JSON data references require a relative path and JSON pointer.");
        }
    }
}
