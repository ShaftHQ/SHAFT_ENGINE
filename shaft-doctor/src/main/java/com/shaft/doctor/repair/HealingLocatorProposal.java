package com.shaft.doctor.repair;

import java.util.List;

/**
 * Proposal-only bridge from a verified SHAFT Heal report to the isolated
 * SHAFT Doctor repair workflow.
 *
 * @param schemaVersion proposal schema version
 * @param proposalId proposal identifier
 * @param healingAttemptId source healing attempt
 * @param sourcePath repository-relative source path
 * @param sourceLine mapped source line
 * @param originalExpression exact original Java locator expression
 * @param proposedExpression proposed replacement Java locator expression
 * @param confidence verified healing confidence
 * @param evidence bounded evidence references
 * @param sourceSha256 source checksum used for stale-source detection
 * @param proposedSourceSha256 complete proposed source checksum
 * @param patch structured Doctor patch for an isolated proposal
 * @param approvalToken review token
 * @param manifestPath persisted proposal manifest
 */
public record HealingLocatorProposal(
        String schemaVersion,
        String proposalId,
        String healingAttemptId,
        String sourcePath,
        int sourceLine,
        String originalExpression,
        String proposedExpression,
        double confidence,
        List<String> evidence,
        String sourceSha256,
        String proposedSourceSha256,
        DoctorRepairRequest.FilePatch patch,
        String approvalToken,
        String manifestPath) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable proposal.
     */
    public HealingLocatorProposal {
        evidence = evidence == null ? List.of() : List.copyOf(evidence);
    }
}
