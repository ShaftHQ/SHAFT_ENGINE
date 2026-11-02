package com.shaft.capture.runtime;

import java.util.List;
import java.util.Map;

/**
 * Safe summary of a captured network transaction for API capture tools.
 *
 * @param transactionId unique transaction identifier
 * @param method HTTP method (GET, POST, etc.)
 * @param url sanitized URL without sensitive query parameters or headers
 * @param statusCode HTTP response status code
 * @param resourceKind resource type (document, xhr, fetch, etc.)
 * @param timingMillis request/response timing in milliseconds
 * @param bodyRefMetadata safe body reference metadata (not the actual body)
 * @param correlatedUiSequence related UI events in the capture sequence
 */
public record NetworkTransaction(
        String transactionId,
        String method,
        String url,
        int statusCode,
        String resourceKind,
        long timingMillis,
        Map<String, Object> bodyRefMetadata,
        List<Integer> correlatedUiSequence) {
    /**
     * Creates an immutable network transaction summary.
     */
    public NetworkTransaction {
        transactionId = transactionId == null ? "" : transactionId;
        method = method == null ? "" : method;
        url = url == null ? "" : url;
        resourceKind = resourceKind == null ? "" : resourceKind;
        bodyRefMetadata = bodyRefMetadata == null ? Map.of() : Map.copyOf(bodyRefMetadata);
        correlatedUiSequence = correlatedUiSequence == null ? List.of() : List.copyOf(correlatedUiSequence);
        if (statusCode < 0) {
            throw new IllegalArgumentException("HTTP status code cannot be negative.");
        }
        if (timingMillis < 0) {
            throw new IllegalArgumentException("Timing cannot be negative.");
        }
    }
}
