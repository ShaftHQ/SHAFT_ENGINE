package com.shaft.doctor.repair;

import java.util.List;

/**
 * Reviewable Doctor repair proposal created in an isolated Git worktree.
 *
 * @param schemaVersion proposal schema version
 * @param proposalId stable proposal identifier
 * @param status proposal lifecycle status
 * @param repositoryRoot approved repository root
 * @param baseSha exact approved base commit
 * @param branchName dedicated Doctor branch
 * @param worktreePath isolated worktree path
 * @param issueReference linked issue or session
 * @param diagnosisPath diagnosis source
 * @param evidenceBundlePath optional evidence source
 * @param patches guarded patch manifest
 * @param validationPlan approved validation commands
 * @param validationResults command and Allure results
 * @param unifiedDiff complete review diff
 * @param risk residual risk statement
 * @param rollback rollback guidance
 * @param approvalToken token required by a separate publication action
 * @param manifestPath persisted proposal manifest
 * @param publication published draft PR details, when present
 */
public record RepairProposal(
        String schemaVersion,
        String proposalId,
        Status status,
        String repositoryRoot,
        String baseSha,
        String branchName,
        String worktreePath,
        String issueReference,
        String diagnosisPath,
        String evidenceBundlePath,
        List<PatchManifestEntry> patches,
        List<List<String>> validationPlan,
        List<ValidationResult> validationResults,
        String unifiedDiff,
        String risk,
        String rollback,
        String approvalToken,
        String manifestPath,
        Publication publication) {
    /**
     * Current proposal schema.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Proposal lifecycle.
     */
    public enum Status {
        PROPOSED,
        PUBLISHED,
        CANCELLED
    }

    /**
     * Guarded file change metadata.
     *
     * @param path repository-relative path
     * @param operation applied operation
     * @param contentSha256 resulting content checksum
     * @param sizeBytes resulting content size
     * @param rationale change rationale
     * @param evidenceIds supporting evidence references
     */
    public record PatchManifestEntry(
            String path,
            DoctorRepairRequest.FilePatch.Operation operation,
            String contentSha256,
            long sizeBytes,
            String rationale,
            List<String> evidenceIds) {
    }

    /**
     * Validation result with bounded diagnostics and populated Allure evidence.
     *
     * @param command exact argument vector
     * @param exitCode process exit code
     * @param timedOut whether the command exceeded its deadline
     * @param passed trusted validation verdict
     * @param allureResultCount populated Allure result count
     * @param diagnostics bounded redacted process diagnostics
     */
    public record ValidationResult(
            List<String> command,
            int exitCode,
            boolean timedOut,
            boolean passed,
            int allureResultCount,
            List<String> diagnostics) {
    }

    /**
     * Published draft pull-request details.
     *
     * @param url draft pull-request URL
     * @param commitSha published repair commit
     * @param validationOverride whether failed validation was explicitly overridden
     * @param overrideRationale recorded override rationale
     */
    public record Publication(
            String url,
            String commitSha,
            boolean validationOverride,
            String overrideRationale) {
    }

    /**
     * Returns whether every requested validation passed.
     *
     * @return true only when at least one validation ran and all passed
     */
    public boolean validationPassed() {
        return !validationResults.isEmpty() && validationResults.stream().allMatch(ValidationResult::passed);
    }
}
