package com.shaft.capture.generate.api;

/**
 * One place a correlated (previously VOLATILE) response value was found reused by a later
 * transaction's outbound request.
 *
 * @param transactionId the later transaction whose request reuses the value
 * @param location where in that request the value was found
 * @param detail header name when {@code location} is {@link Location#REQUEST_HEADER}; empty otherwise
 */
public record CorrelationUse(String transactionId, Location location, String detail) {
    public CorrelationUse {
        transactionId = transactionId == null ? "" : transactionId;
        location = location == null ? Location.REQUEST_BODY : location;
        detail = detail == null ? "" : detail;
    }

    /**
     * Where a correlated value was found reused in a later request.
     */
    public enum Location {
        URL,
        REQUEST_HEADER,
        REQUEST_BODY
    }
}
