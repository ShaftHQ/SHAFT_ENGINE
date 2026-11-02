package com.shaft.doctor.repair;

import java.nio.file.Path;

/**
 * Separate explicit approval for publishing a reviewed Doctor proposal.
 *
 * @param manifestPath persisted proposal manifest
 * @param approved must be true for any GitHub write
 * @param approvalToken exact token returned with the proposal
 * @param overrideFailedValidation explicit failed-validation override
 * @param overrideRationale required rationale for an override
 * @param title draft pull-request title
 */
public record DoctorRepairPublicationRequest(
        Path manifestPath,
        boolean approved,
        String approvalToken,
        boolean overrideFailedValidation,
        String overrideRationale,
        String title) {
    /**
     * Creates a validated publication request.
     */
    public DoctorRepairPublicationRequest {
        if (manifestPath == null) {
            throw new IllegalArgumentException("Proposal manifest path is required.");
        }
        approvalToken = approvalToken == null ? "" : approvalToken.trim();
        overrideRationale = overrideRationale == null ? "" : overrideRationale.trim();
        title = title == null ? "" : title.trim();
    }
}
