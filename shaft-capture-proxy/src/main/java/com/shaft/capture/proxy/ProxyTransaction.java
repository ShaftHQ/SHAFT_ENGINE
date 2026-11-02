package com.shaft.capture.proxy;

import java.util.Map;

/**
 * One raw HTTP(S) transaction captured by {@link ApiCaptureProxyServer}, before any redaction,
 * classification, or session-model wrapping. A higher-level controller is responsible for turning
 * this into a first-class {@code CaptureEvent.NetworkEvent} the same way
 * {@code com.shaft.capture.network.CaptureNetworkRecorder}'s raw callbacks are wrapped by
 * {@code ManagedCaptureRecorder} -- this module has no dependency on session/store types, only on
 * shaft-capture's model, which it does not construct directly.
 *
 * @param method HTTP method
 * @param url reconstructed absolute request URL
 * @param requestHeaders raw request headers, in wire order
 * @param requestBody raw request body bytes (empty if none)
 * @param statusCode response status code (0 if no response was received, e.g. a failed transaction)
 * @param responseHeaders raw response headers, in wire order (empty if no response)
 * @param responseBody raw response body bytes (empty if none)
 */
public record ProxyTransaction(
        String method,
        String url,
        Map<String, String> requestHeaders,
        byte[] requestBody,
        int statusCode,
        Map<String, String> responseHeaders,
        byte[] responseBody) {
}
