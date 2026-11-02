package com.shaft.doctor.model;

/**
 * Analyzer outputs and diagnosis.
 *
 * @param bundle evidence bundle
 * @param diagnosis deterministic diagnosis
 * @param bundlePath written bundle JSON path
 * @param jsonReportPath written machine-readable report path
 * @param markdownReportPath written Markdown report path
 */
public record DoctorAnalysisResult(
        EvidenceBundle bundle,
        Diagnosis diagnosis,
        String bundlePath,
        String jsonReportPath,
        String markdownReportPath) {
    /**
     * Creates a complete analysis result.
     */
    public DoctorAnalysisResult {
        if (bundle == null || diagnosis == null) {
            throw new IllegalArgumentException("Bundle and diagnosis are required.");
        }
        bundlePath = DoctorModelSupport.text(bundlePath);
        jsonReportPath = DoctorModelSupport.text(jsonReportPath);
        markdownReportPath = DoctorModelSupport.text(markdownReportPath);
    }
}
