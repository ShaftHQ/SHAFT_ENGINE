package com.shaft.doctor.history;

import java.util.List;

/**
 * Heal insights for the Reporting canvas (S3-06 / issue #5972).
 *
 * <p>Counts {@code HealingDecision} statuses from persisted SHAFT Heal reports. Locator patch
 * proposals are review-gated: only {@code RECOVERED} with a passing action outcome may offer a
 * reviewable diff. {@code AMBIGUOUS} never gets an apply-to-source primary action.
 * {@code NO_CANDIDATES} is shown, never silent. Failed replay never persists a proposal.
 * Policy: {@code locator-healing.md} and issue #5454 remain controlling — never auto-land.
 */
public final class HealInsightModels {
    /** Shared schema version for heal-insights responses. */
    public static final String SCHEMA_VERSION = "1.0";

    private HealInsightModels() {
    }

    /**
     * Primary review-gate action exposed to IDE / MCP clients.
     */
    public enum PrimaryAction {
        /** RECOVERED + passing replay — offer a reviewable locator patch (never auto-write). */
        REVIEW_DIFF,
        /** Visible but not eligible for source-patch propose (AMBIGUOUS, failed replay, etc.). */
        NONE
    }

    /**
     * Aggregated heal insights table for IDE / MCP {@code report_heal} / CLI {@code shaft report heal}.
     *
     * @param schemaVersion response schema
     * @param empty whether no heal reports were found
     * @param emptyMessage user-facing empty-state text when {@code empty} is true
     * @param reportsPath resolved heal-reports directory inspected (may be blank)
     * @param proposalsPath resolved proposal manifests directory inspected (may be blank)
     * @param totalReports number of HealingReport JSON files successfully read
     * @param statusCounts counts by HealingDecision status (stable enum order)
     * @param insights per-report insight rows (newest-ish first by attemptId when sortable)
     * @param warnings non-fatal notes
     */
    public record HealInsightsTable(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            String reportsPath,
            String proposalsPath,
            int totalReports,
            List<StatusCount> statusCounts,
            List<HealInsight> insights,
            List<String> warnings) {
        public HealInsightsTable {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? SCHEMA_VERSION : schemaVersion.trim();
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            reportsPath = reportsPath == null ? "" : reportsPath;
            proposalsPath = proposalsPath == null ? "" : proposalsPath;
            totalReports = Math.max(0, totalReports);
            statusCounts = List.copyOf(statusCounts == null ? List.of() : statusCounts);
            insights = List.copyOf(insights == null ? List.of() : insights);
            warnings = List.copyOf(warnings == null ? List.of() : warnings);
        }
    }

    /**
     * One HealingDecision status bucket.
     *
     * @param status decision status name (e.g. RECOVERED, AMBIGUOUS, NO_CANDIDATES)
     * @param count number of reports with this status
     */
    public record StatusCount(String status, int count) {
        public StatusCount {
            status = status == null ? "" : status;
            count = Math.max(0, count);
        }
    }

    /**
     * One heal insight derived from a HealingReport JSON file.
     *
     * @param attemptId heal attempt id
     * @param reportPath absolute or workspace path to the report JSON
     * @param status HealingDecision status
     * @param originalLocator original failed locator
     * @param proposedLocator selected candidate locator when present
     * @param confidence decision confidence 0..1
     * @param actionOutcome action metadata outcome (PASSED / FAILED / PENDING / …)
     * @param postActionVerification post-action verification token
     * @param reason decision reason
     * @param canProposeSourcePatch whether {@code doctor_propose_healed_locator} is allowed (FR-002)
     * @param primaryAction review-gate primary action (SC-001 / SC-002)
     * @param primaryActionLabel human label for the primary action (blank when NONE)
     * @param proposalManifestPath existing proposal manifest path when already persisted
     * @param sourcePatchProposed whether the report recorded a prior proposal emission
     */
    public record HealInsight(
            String attemptId,
            String reportPath,
            String status,
            String originalLocator,
            String proposedLocator,
            double confidence,
            String actionOutcome,
            String postActionVerification,
            String reason,
            boolean canProposeSourcePatch,
            PrimaryAction primaryAction,
            String primaryActionLabel,
            String proposalManifestPath,
            boolean sourcePatchProposed) {
        public HealInsight {
            attemptId = attemptId == null ? "" : attemptId;
            reportPath = reportPath == null ? "" : reportPath;
            status = status == null ? "" : status;
            originalLocator = originalLocator == null ? "" : originalLocator;
            proposedLocator = proposedLocator == null ? "" : proposedLocator;
            confidence = Math.max(0, Math.min(1, confidence));
            actionOutcome = actionOutcome == null ? "" : actionOutcome;
            postActionVerification = postActionVerification == null ? "" : postActionVerification;
            reason = reason == null ? "" : reason;
            primaryAction = primaryAction == null ? PrimaryAction.NONE : primaryAction;
            primaryActionLabel = primaryActionLabel == null ? "" : primaryActionLabel;
            proposalManifestPath = proposalManifestPath == null ? "" : proposalManifestPath;
        }
    }
}
