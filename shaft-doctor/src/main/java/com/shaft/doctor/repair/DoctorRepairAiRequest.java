package com.shaft.doctor.repair;

import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.ApprovalPolicy;

import java.math.BigDecimal;
import java.time.Duration;

/**
 * Explicit policy for optional provider-generated structured repair patches.
 *
 * @param enabled whether provider patch generation is requested
 * @param approvalPolicy processing-location and evidence-category approval
 * @param timeout provider timeout
 * @param budget provider budget
 * @param maxSourceFiles maximum approved source files submitted
 * @param maxSourceBytes maximum aggregate submitted source bytes
 * @param maxResponseBytes maximum structured response bytes
 */
public record DoctorRepairAiRequest(
        boolean enabled,
        ApprovalPolicy approvalPolicy,
        Duration timeout,
        AiBudget budget,
        int maxSourceFiles,
        long maxSourceBytes,
        long maxResponseBytes) {
    /**
     * Creates a validated provider patch policy.
     */
    public DoctorRepairAiRequest {
        approvalPolicy = approvalPolicy == null ? ApprovalPolicy.denyAll() : approvalPolicy;
        timeout = timeout == null ? Duration.ofSeconds(45) : timeout;
        budget = budget == null ? new AiBudget(12_000, 4_000, BigDecimal.ZERO) : budget;
        if (timeout.isZero() || timeout.isNegative()
                || maxSourceFiles <= 0 || maxSourceBytes <= 0 || maxResponseBytes <= 0) {
            throw new IllegalArgumentException("Doctor repair AI limits must be positive.");
        }
    }

    /**
     * Creates conservative enabled defaults.
     *
     * @param approvalPolicy explicit provider approval
     * @return enabled request
     */
    public static DoctorRepairAiRequest defaults(ApprovalPolicy approvalPolicy) {
        return new DoctorRepairAiRequest(
                true,
                approvalPolicy,
                Duration.ofSeconds(45),
                new AiBudget(12_000, 4_000, BigDecimal.ZERO),
                12,
                262_144,
                524_288);
    }
}
