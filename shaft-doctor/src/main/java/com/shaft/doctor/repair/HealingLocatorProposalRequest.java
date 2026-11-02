package com.shaft.doctor.repair;

import java.nio.file.Path;

/**
 * Explicit request to derive a reviewed locator patch proposal from a verified
 * SHAFT Heal report.
 *
 * @param repositoryRoot approved repository root
 * @param healingReportPath verified SHAFT Heal report JSON
 * @param sourcePath repository-relative Java source file
 * @param sourcePatchConsent explicit source-patch proposal consent
 * @param outputDirectory proposal artifact directory
 */
public record HealingLocatorProposalRequest(
        Path repositoryRoot,
        Path healingReportPath,
        String sourcePath,
        boolean sourcePatchConsent,
        Path outputDirectory) {
    /**
     * Creates a validated request.
     */
    public HealingLocatorProposalRequest {
        if (repositoryRoot == null || healingReportPath == null || outputDirectory == null) {
            throw new IllegalArgumentException(
                    "Repository, healing report, and output directory are required.");
        }
        sourcePath = sourcePath == null ? "" : sourcePath.replace('\\', '/').trim();
        if (sourcePath.isBlank() || sourcePath.startsWith("/") || sourcePath.contains("..")) {
            throw new IllegalArgumentException("A repository-relative source path is required.");
        }
    }
}
