package com.shaft.capture.model.network;

import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

/**
 * Sanitized outbound HTTP request evidence. No raw or decoded body content is
 * retained; only a safe {@link BodyRef} reference is kept.
 *
 * @param method HTTP method, normalized to upper case
 * @param url sanitized request URL
 * @param headers sanitized, deterministically ordered request headers
 * @param body optional safe reference to the request body
 */
public record HttpRequestRecord(
        String method,
        String url,
        Map<String, String> headers,
        BodyRef body) {
    /**
     * Creates an immutable HTTP request record.
     */
    public HttpRequestRecord {
        method = NetworkModelSupport.requireText(method, "HTTP request method").toUpperCase(java.util.Locale.ROOT);
        url = NetworkModelSupport.requireText(url, "HTTP request URL");
        headers = normalizeHeaders(headers);
    }

    static Map<String, String> normalizeHeaders(Map<String, String> values) {
        if (values == null || values.isEmpty()) {
            return Map.of();
        }
        Map<String, String> copy = new TreeMap<>();
        values.forEach((key, value) -> copy.put(
                NetworkModelSupport.text(key).toLowerCase(java.util.Locale.ROOT),
                Objects.requireNonNullElse(value, "")));
        return Map.copyOf(copy);
    }
}
