package com.shaft.mcp;

import com.shaft.capture.model.CaptureReadiness;

import java.util.List;

/**
 * Safe status of a mobile API capture session -- no bodies, no raw headers, nothing sensitive.
 *
 * <p>Shares the recorder readiness vocabulary (#3499): {@code readiness} reuses the frozen web
 * {@link CaptureReadiness.State} (Ready/Risky/Blocked) so the mobile API capture speaks the same
 * language as the web/mobile/Playwright recorders. The atomic unit here is a <em>transaction</em>
 * (an HTTP request/response), not a GUI "step".
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
 * @param outputPath the persisted capture-session JSON path, always visible so the caller can
 *                   generate an API test from it after stopping (empty before a session starts)
 * @param readiness Ready/Risky/Blocked verdict rolled up from warnings; never {@code null}
 */
public record MobileApiCaptureStatus(
        boolean active,
        String sessionId,
        int proxyPort,
        String caCertificatePem,
        long transactionCount,
        List<String> warnings,
        String outputPath,
        CaptureReadiness.State readiness) {
    public MobileApiCaptureStatus {
        sessionId = sessionId == null ? "" : sessionId;
        caCertificatePem = caCertificatePem == null ? "" : caCertificatePem;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        outputPath = outputPath == null ? "" : outputPath;
        readiness = readiness == null ? CaptureReadiness.State.READY : readiness;
    }

    /**
     * Back-compatible constructor matching the original pre-#3499 shape; defaults the session path
     * to empty and readiness to {@link CaptureReadiness.State#READY}.
     */
    public MobileApiCaptureStatus(
            boolean active,
            String sessionId,
            int proxyPort,
            String caCertificatePem,
            long transactionCount,
            List<String> warnings) {
        this(active, sessionId, proxyPort, caCertificatePem, transactionCount, warnings, "",
                CaptureReadiness.State.READY);
    }
}
