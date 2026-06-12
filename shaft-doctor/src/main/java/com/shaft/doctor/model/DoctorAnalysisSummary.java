package com.shaft.doctor.model;

/**
 * Safe analysis summary for CLI or MCP responses without retained evidence content.
 *
 * @param bundleId evidence bundle identifier
 * @param evidenceItemCount retained evidence item count
 * @param omittedItemCount evidence items omitted by policy or limits
 * @param diagnosis deterministic diagnosis
 * @param bundlePath written bundle path
 * @param jsonReportPath written JSON report path
 * @param markdownReportPath written Markdown report path
 */
public record DoctorAnalysisSummary(
        String bundleId,
        int evidenceItemCount,
        int omittedItemCount,
        Diagnosis diagnosis,
        String bundlePath,
        String jsonReportPath,
        String markdownReportPath) {
    /**
     * Creates a safe summary from a complete local result.
     *
     * @param result complete local result
     * @return content-free summary
     */
    public static DoctorAnalysisSummary from(DoctorAnalysisResult result) {
        if (result == null) {
            throw new IllegalArgumentException("Doctor analysis result is required.");
        }
        return new DoctorAnalysisSummary(
                result.bundle().bundleId(),
                result.bundle().evidence().size(),
                result.bundle().redaction().omittedItems(),
                result.diagnosis(),
                result.bundlePath(),
                result.jsonReportPath(),
                result.markdownReportPath());
    }

    /**
     * Creates a validated summary.
     */
    public DoctorAnalysisSummary {
        bundleId = DoctorModelSupport.requireText(bundleId, "Bundle ID");
        if (evidenceItemCount < 0 || omittedItemCount < 0) {
            throw new IllegalArgumentException("Doctor evidence counts cannot be negative.");
        }
        if (diagnosis == null) {
            throw new IllegalArgumentException("Doctor diagnosis is required.");
        }
        bundlePath = DoctorModelSupport.text(bundlePath);
        jsonReportPath = DoctorModelSupport.text(jsonReportPath);
        markdownReportPath = DoctorModelSupport.text(markdownReportPath);
    }
}
