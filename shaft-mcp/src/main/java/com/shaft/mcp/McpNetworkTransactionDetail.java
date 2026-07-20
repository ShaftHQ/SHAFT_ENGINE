package com.shaft.mcp;

import java.util.Map;

/**
 * One observed network transaction's detail returned by {@code browser_network_requests} when called
 * with an {@code id}, including headers and a truncated, redacted response body preview as recorded
 * by trace capture.
 *
 * <p>Full, untruncated request/response bodies are not available from this tool; they require a
 * {@code capture_start} session (optionally with its nested {@code codegenOptions}) started with
 * {@code saveHarContent=full}.
 *
 * @param id                1-based transaction id
 * @param method            HTTP method
 * @param url               request URL
 * @param status            HTTP response status code, or {@code 0} when the exchange failed
 * @param mimeType          response {@code Content-Type} without parameters, blank when absent
 * @param durationMs        exchange duration in milliseconds
 * @param requestSizeBytes  request body size in bytes
 * @param responseSizeBytes response body size in bytes
 * @param timestamp         epoch millis when the exchange finished
 * @param failureReason     safe failure reason, blank on success
 * @param requestHeaders    sanitized request headers
 * @param responseHeaders   sanitized response headers
 * @param bodyPreview       truncated, redacted response body preview
 */
public record McpNetworkTransactionDetail(
        int id,
        String method,
        String url,
        int status,
        String mimeType,
        long durationMs,
        long requestSizeBytes,
        long responseSizeBytes,
        long timestamp,
        String failureReason,
        Map<String, String> requestHeaders,
        Map<String, String> responseHeaders,
        String bodyPreview) {
}
