package com.shaft.doctor;

import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.ApprovalPolicy;

import java.math.BigDecimal;
import java.time.Duration;

/**
 * Explicit policy for one optional provider-assisted Doctor analysis.
 *
 * @param enabled whether provider analysis is requested
 * @param approvalPolicy explicit processing-location and evidence-category approval
 * @param timeout provider request timeout
 * @param budget provider request budget
 * @param maxEvidenceItems maximum minimized evidence items sent to the provider
 * @param maxEvidenceBytes maximum UTF-8 bytes across submitted evidence
 * @param maxResponseBytes maximum accepted structured provider response bytes
 * @param cacheEnabled whether safe structured successful advisories may be cached
 */
public record DoctorAiAnalysisRequest(
        boolean enabled,
        ApprovalPolicy approvalPolicy,
        Duration timeout,
        AiBudget budget,
        int maxEvidenceItems,
        long maxEvidenceBytes,
        long maxResponseBytes,
        boolean cacheEnabled) {
    /**
     * Default maximum number of minimized evidence items.
     */
    public static final int DEFAULT_MAX_EVIDENCE_ITEMS = 12;
    /**
     * Default maximum submitted evidence size.
     */
    public static final long DEFAULT_MAX_EVIDENCE_BYTES = 131_072;
    /**
     * Default maximum structured provider response size.
     */
    public static final long DEFAULT_MAX_RESPONSE_BYTES = 65_536;

    /**
     * Creates a validated provider-analysis policy.
     */
    public DoctorAiAnalysisRequest {
        approvalPolicy = approvalPolicy == null ? ApprovalPolicy.denyAll() : approvalPolicy;
        timeout = timeout == null ? Duration.ofSeconds(30) : timeout;
        budget = budget == null ? new AiBudget(8_000, 1_500, BigDecimal.ZERO) : budget;
        if (timeout.isZero() || timeout.isNegative()) {
            throw new IllegalArgumentException("Doctor AI timeout must be positive.");
        }
        if (maxEvidenceItems <= 0 || maxEvidenceBytes <= 0 || maxResponseBytes <= 0) {
            throw new IllegalArgumentException("Doctor AI evidence and response limits must be positive.");
        }
    }

    /**
     * Creates conservative enabled defaults.
     *
     * @param approvalPolicy explicit provider approval
     * @return enabled provider-analysis request
     */
    public static DoctorAiAnalysisRequest defaults(ApprovalPolicy approvalPolicy) {
        return new DoctorAiAnalysisRequest(
                true,
                approvalPolicy,
                Duration.ofSeconds(30),
                new AiBudget(8_000, 1_500, BigDecimal.ZERO),
                DEFAULT_MAX_EVIDENCE_ITEMS,
                DEFAULT_MAX_EVIDENCE_BYTES,
                DEFAULT_MAX_RESPONSE_BYTES,
                false);
    }

    /**
     * Creates a disabled request that preserves deterministic output.
     *
     * @return disabled request
     */
    public static DoctorAiAnalysisRequest disabled() {
        return new DoctorAiAnalysisRequest(
                false,
                ApprovalPolicy.denyAll(),
                Duration.ofSeconds(30),
                new AiBudget(0, 0, BigDecimal.ZERO),
                DEFAULT_MAX_EVIDENCE_ITEMS,
                DEFAULT_MAX_EVIDENCE_BYTES,
                DEFAULT_MAX_RESPONSE_BYTES,
                false);
    }
}
