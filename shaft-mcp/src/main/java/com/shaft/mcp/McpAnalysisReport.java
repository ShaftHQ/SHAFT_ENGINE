package com.shaft.mcp;

import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;

import java.util.List;

/**
 * Local-stdio MCP report for failed Allure analysis and remediation snippets.
 *
 * @param schemaVersion report schema version
 * @param status report status
 * @param bundleId evidence bundle identifier
 * @param primaryCause deterministic primary cause
 * @param confidence deterministic confidence
 * @param summary concise deterministic summary
 * @param diagnosis full deterministic diagnosis
 * @param actions recorded remediation actions
 * @param codeBlocks copy-paste code blocks
 * @param providerFallback optional AI fallback status
 * @param bundlePath written evidence bundle path
 * @param jsonReportPath written JSON report path
 * @param markdownReportPath written Markdown report path
 * @param warnings safe warnings
 */
public record McpAnalysisReport(
        String schemaVersion,
        Status status,
        String bundleId,
        CauseCategory primaryCause,
        Confidence confidence,
        String summary,
        Diagnosis diagnosis,
        List<McpActionRecord> actions,
        List<McpCodeBlock> codeBlocks,
        McpProviderFallback providerFallback,
        String bundlePath,
        String jsonReportPath,
        String markdownReportPath,
        List<String> warnings) {
    /**
     * Current report schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Report status.
     */
    public enum Status {
        DETERMINISTIC,
        WITH_PROVIDER_ADVISORY
    }

    /**
     * Creates an immutable analysis report.
     */
    public McpAnalysisReport {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        status = status == null ? Status.DETERMINISTIC : status;
        bundleId = bundleId == null ? "" : bundleId.trim();
        primaryCause = primaryCause == null ? CauseCategory.UNKNOWN : primaryCause;
        confidence = confidence == null ? Confidence.UNKNOWN : confidence;
        summary = summary == null ? "" : summary.trim();
        actions = actions == null ? List.of() : List.copyOf(actions);
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        providerFallback = providerFallback == null ? McpProviderFallback.disabled() : providerFallback;
        bundlePath = bundlePath == null ? "" : bundlePath.trim();
        jsonReportPath = jsonReportPath == null ? "" : jsonReportPath.trim();
        markdownReportPath = markdownReportPath == null ? "" : markdownReportPath.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
