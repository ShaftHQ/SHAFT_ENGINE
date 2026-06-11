package com.shaft.capture.generate;

import java.nio.file.Path;

/**
 * Generated artifact paths and their deterministic report.
 *
 * @param sourcePath generated Java source
 * @param testDataPath generated JSON test data
 * @param reportPath generation report
 * @param enrichmentPreviewPath optional enrichment preview
 * @param report generation report
 */
public record CaptureGenerationResult(
        Path sourcePath,
        Path testDataPath,
        Path reportPath,
        Path enrichmentPreviewPath,
        CaptureGenerationReport report) {
    /**
     * Returns whether generation and all requested validations succeeded.
     *
     * @return true for a successful report
     */
    public boolean successful() {
        return report != null && report.status() == CaptureGenerationReport.Status.SUCCESS;
    }
}
