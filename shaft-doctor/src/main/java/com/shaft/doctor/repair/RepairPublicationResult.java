package com.shaft.doctor.repair;

/**
 * Safe result from an approval-gated draft publication.
 *
 * @param proposalId proposal identifier
 * @param branchName published branch
 * @param commitSha published commit
 * @param pullRequestUrl draft pull-request URL
 * @param reusedExisting whether an existing draft PR was reused
 */
public record RepairPublicationResult(
        String proposalId,
        String branchName,
        String commitSha,
        String pullRequestUrl,
        boolean reusedExisting) {
}
