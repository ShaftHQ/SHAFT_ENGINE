package com.shaft.mcp;

/**
 * One observed network transaction summary returned by {@code browser_network_requests}, without
 * headers or bodies.
 *
 * @param id             1-based transaction id, usable with {@code browser_network_request}
 * @param method         HTTP method
 * @param url            request URL
 * @param status         HTTP response status code, or {@code 0} when the exchange failed
 * @param mimeType       response {@code Content-Type} without parameters, blank when absent
 * @param durationMs     exchange duration in milliseconds
 * @param requestSizeBytes  request body size in bytes
 * @param responseSizeBytes response body size in bytes
 * @param timestamp      epoch millis when the exchange finished
 * @param failureReason  safe failure reason, blank on success
 */
public record McpNetworkTransaction(
        int id,
        String method,
        String url,
        int status,
        String mimeType,
        long durationMs,
        long requestSizeBytes,
        long responseSizeBytes,
        long timestamp,
        String failureReason) {
}
