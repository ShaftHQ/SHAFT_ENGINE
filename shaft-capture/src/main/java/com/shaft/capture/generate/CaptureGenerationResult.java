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

    /**
     * Returns the deterministic review path beside the generation report.
     *
     * @return review artifact path
     */
    public Path reviewPath() {
        return reportPath == null || reportPath.getParent() == null
                ? Path.of("capture-review.json")
                : reportPath.getParent().resolve("capture-review.json");
    }

    /**
     * Returns the local Capture workbench HTML path beside the review artifact.
     *
     * @return review UI artifact path
     */
    public Path reviewUiPath() {
        return reviewPath().resolveSibling("capture-workbench.html");
    }
}
