package com.shaft.capture.generate.api;

/**
 * Classification of one scalar JSON leaf discovered in a recorded API response body.
 */
public enum LeafClassification {
    /**
     * A value expected to be identical on every replay (e.g. a fixed status message).
     */
    STABLE,
    /**
     * A value expected to change on every replay (e.g. a generated ID or timestamp) that is not
     * observed being reused by a later request in the same session.
     */
    VOLATILE,
    /**
     * A value that must never be asserted or persisted in generated source/test-data (e.g. an
     * authorization token or password).
     */
    SENSITIVE,
    /**
     * A {@link #VOLATILE} value that {@code TransactionCorrelator} found reused by a later
     * request in the same session, and which codegen therefore chains through a variable instead
     * of asserting or replaying as a literal.
     */
    CORRELATED
}
