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
    FULL_BODY,
    /**
     * {@link #STATUS} plus a targeted business-value assertion on every response leaf classified
     * {@link LeafClassification#STABLE}, asserting the recorded value at its JSON path via
     * {@code getResponseJSONValue}. Volatile, correlated, and sensitive leaves are skipped so the
     * generated test pins only the business-meaningful fields that a human would check, without
     * flaking on generated IDs/timestamps or leaking secrets. This is the assertion model the
     * business-logic recorder (#3499) uses for chained domain scenarios.
     */
    BUSINESS
}
