package com.shaft.capture.model.network;

import java.util.Map;

/**
 * Sanitized inbound HTTP response evidence. No raw or decoded body content is
 * retained; only a safe {@link BodyRef} reference is kept.
 *
 * @param statusCode HTTP response status code
 * @param headers sanitized, deterministically ordered response headers
 * @param body optional safe reference to the response body
 */
public record HttpResponseRecord(
        int statusCode,
        Map<String, String> headers,
        BodyRef body) {
    /**
     * Creates an immutable HTTP response record.
     */
    public HttpResponseRecord {
        if (statusCode < 100 || statusCode > 599) {
            throw new IllegalArgumentException("HTTP response status code must be a valid HTTP status.");
        }
        headers = HttpRequestRecord.normalizeHeaders(headers);
    }
}
