package com.shaft.capture.generate.api;

import java.util.List;
import java.util.Map;

/**
 * One recorded API transaction with resolved (not just referenced) request/response body text,
 * ready for classification, correlation, and rendering.
 *
 * @param transactionId stable transaction identifier from the recording
 * @param method HTTP method
 * @param url request URL
 * @param origin {@code scheme://host[:port]} of the request URL, used to group transactions by
 *               origin during rendering
 * @param requestHeaders sanitized request headers
 * @param requestBody resolved request body text (empty if none/binary/oversized/unavailable)
 * @param statusCode response status code (0 if there was no response, e.g. a failed transaction)
 * @param responseHeaders sanitized response headers
 * @param responseBody resolved response body text (empty if none/binary/oversized/unavailable)
 * @param responseLeaves classified leaves of the response body (see {@link ResponseNormalizer})
 */
public record ApiTransaction(
        String transactionId,
        String method,
        String url,
        String origin,
        Map<String, String> requestHeaders,
        String requestBody,
        int statusCode,
        Map<String, String> responseHeaders,
        String responseBody,
        List<ResponseLeaf> responseLeaves) {
    public ApiTransaction {
        transactionId = transactionId == null ? "" : transactionId;
        method = method == null ? "" : method.toUpperCase(java.util.Locale.ROOT);
        url = url == null ? "" : url;
        origin = origin == null ? "" : origin;
        requestHeaders = requestHeaders == null ? Map.of() : Map.copyOf(requestHeaders);
        requestBody = requestBody == null ? "" : requestBody;
        responseHeaders = responseHeaders == null ? Map.of() : Map.copyOf(responseHeaders);
        responseBody = responseBody == null ? "" : responseBody;
        responseLeaves = responseLeaves == null ? List.of() : List.copyOf(responseLeaves);
    }
}
