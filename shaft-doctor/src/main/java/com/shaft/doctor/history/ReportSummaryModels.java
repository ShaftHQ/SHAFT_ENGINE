package com.shaft.doctor.history;

import java.util.List;
import java.util.Map;

/**
 * Reconciled Reporting summaries for IDE / MCP {@code report_summary} / CLI {@code shaft report summary}
 * (issue #5976 / S3-10).
 *
 * <p>Shapes follow {@code shaft-execution-reporting} (engineer) and {@code shaft-stakeholder-reporting}
 * (stakeholder) playbooks. Counts only — never secrets, tokens, cookies, or raw log bodies.
 */
public final class ReportSummaryModels {
    public static final String SCHEMA_VERSION = "1.0";

    private ReportSummaryModels() {
    }

    /**
     * Dual audience summary package for the unified Reporting canvas.
     *
     * @param schemaVersion response schema
     * @param empty whether no Allure results were found to reconcile
     * @param emptyMessage empty-state guidance (includes {@code generate_test_report} CTA when empty)
     * @param allureResultsPath resolved results directory inspected (may be blank)
     * @param reportPath resolved Allure HTML path when present (may be blank)
     * @param counts reconciled status totals (no secrets)
     * @param flakeRetryHiddenCount rows tagged retry-hidden
     * @param flakeTransitionsCount rows tagged transitions
     * @param healRecoveredCount RECOVERED heal reports
     * @param healAmbiguousCount AMBIGUOUS heal reports
     * @param healNoCandidatesCount NO_CANDIDATES heal reports
     * @param engineerSummary playbook-shaped engineer text
     * @param stakeholderSummary playbook-shaped stakeholder text
     * @param warnings non-fatal notes
     */
    public record SummaryView(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            String allureResultsPath,
            String reportPath,
            Map<String, Integer> counts,
            int flakeRetryHiddenCount,
            int flakeTransitionsCount,
            int healRecoveredCount,
            int healAmbiguousCount,
            int healNoCandidatesCount,
            String engineerSummary,
            String stakeholderSummary,
            List<String> warnings) {
        public SummaryView {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? SCHEMA_VERSION : schemaVersion.trim();
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            allureResultsPath = allureResultsPath == null ? "" : allureResultsPath;
            reportPath = reportPath == null ? "" : reportPath;
            counts = Map.copyOf(counts == null ? Map.of() : counts);
            flakeRetryHiddenCount = Math.max(0, flakeRetryHiddenCount);
            flakeTransitionsCount = Math.max(0, flakeTransitionsCount);
            healRecoveredCount = Math.max(0, healRecoveredCount);
            healAmbiguousCount = Math.max(0, healAmbiguousCount);
            healNoCandidatesCount = Math.max(0, healNoCandidatesCount);
            engineerSummary = engineerSummary == null ? "" : engineerSummary;
            stakeholderSummary = stakeholderSummary == null ? "" : stakeholderSummary;
            warnings = List.copyOf(warnings == null ? List.of() : warnings);
        }
    }

    /**
     * Open-Allure result for MCP {@code report_open} / CLI {@code shaft report open}.
     *
     * @param schemaVersion response schema
     * @param empty whether no Allure HTML report exists yet
     * @param emptyMessage empty-state text with {@code generate_test_report} CTA when empty
     * @param reportPath absolute or workspace-relative HTML path when found
     * @param resultsPath related allure-results path when known
     * @param opened whether the host attempted to open the report in a browser
     * @param ctaTool {@code generate_test_report} when empty, else blank
     * @param warnings non-fatal notes
     */
    public record OpenView(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            String reportPath,
            String resultsPath,
            boolean opened,
            String ctaTool,
            List<String> warnings) {
        public OpenView {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? SCHEMA_VERSION : schemaVersion.trim();
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            reportPath = reportPath == null ? "" : reportPath;
            resultsPath = resultsPath == null ? "" : resultsPath;
            ctaTool = ctaTool == null ? "" : ctaTool;
            warnings = List.copyOf(warnings == null ? List.of() : warnings);
        }
    }
}
