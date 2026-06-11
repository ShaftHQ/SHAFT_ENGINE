package com.shaft.pilot.audit;

import com.shaft.pilot.ai.AiResponseStatus;

import java.time.Duration;
import java.time.Instant;

/**
 * Safe audit metadata that excludes prompts, evidence, responses, and credentials.
 *
 * @param timestamp event time
 * @param requestId request correlation identifier
 * @param purpose request purpose
 * @param provider provider identifier
 * @param model model identifier
 * @param redactionSummary safe redaction counts
 * @param duration elapsed duration
 * @param status normalized outcome
 */
public record AiAuditEvent(
        Instant timestamp,
        String requestId,
        String purpose,
        String provider,
        String model,
        String redactionSummary,
        Duration duration,
        AiResponseStatus status) {
    /**
     * Normalizes user-controlled metadata before it reaches logs.
     */
    public AiAuditEvent {
        requestId = safe(requestId, 128);
        purpose = safe(purpose, 128);
        provider = safe(provider, 64);
        model = safe(model, 128);
        redactionSummary = safe(redactionSummary, 256);
    }

    private static String safe(String value, int limit) {
        String normalized = value == null ? "" : value.replaceAll("\\p{Cntrl}", " ");
        return normalized.length() <= limit ? normalized : normalized.substring(0, limit);
    }
}
