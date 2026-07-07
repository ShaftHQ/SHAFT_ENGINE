package com.shaft.mcp;

import java.util.List;

/**
 * Safe status of a mobile API capture session -- no bodies, no raw headers, nothing sensitive.
 *
 * @param active whether a capture session is currently running
 * @param sessionId active (or last) session identifier
 * @param proxyPort loopback port the MITM proxy is listening on (0 if not active)
 * @param caCertificatePem the per-installation CA certificate, PEM-encoded, for the caller to
 *                         surface as a device trust-installation artifact (public data only --
 *                         never the private key)
 * @param transactionCount number of transactions captured so far
 * @param warnings safe warnings (pairing limitations, pinned hosts, transactions that could not
 *                 be recorded)
 */
public record MobileApiCaptureStatus(
        boolean active,
        String sessionId,
        int proxyPort,
        String caCertificatePem,
        long transactionCount,
        List<String> warnings) {
    public MobileApiCaptureStatus {
        sessionId = sessionId == null ? "" : sessionId;
        caCertificatePem = caCertificatePem == null ? "" : caCertificatePem;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
