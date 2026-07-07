package com.shaft.capture.generate.api;

/**
 * How thoroughly generated code validates each response.
 */
public enum ApiValidationDepth {
    /**
     * Only the recorded HTTP status code is asserted (via {@code setTargetStatusCode}).
     */
    STATUS,
    /**
     * {@link #STATUS} plus an assertion on every response header classified
     * {@link LeafClassification#STABLE} (sensitive headers are never asserted; volatile ones
     * would flake on replay, so both are skipped).
     */
    STATUS_HEADERS,
    /**
     * {@link #STATUS} plus a {@code matchesSchema} assertion against a schema inferred by
     * {@link JsonSchemaInferencer} from the recorded response body.
     */
    SCHEMA,
    /**
     * {@link #STATUS} plus a full-body {@code isEqualToFileContentIgnoringOrder} assertion
     * against a normalized golden file (volatile/correlated leaves replaced with a stable
     * placeholder, sensitive leaves masked, so the assertion doesn't flake on values that
     * legitimately change every run).
     */
    FULL_BODY
}
