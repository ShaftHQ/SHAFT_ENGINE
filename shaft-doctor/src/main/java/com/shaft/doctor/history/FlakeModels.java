package com.shaft.doctor.history;

import java.util.List;

/**
 * Dual flake definitions for the Reporting canvas (S3-02 / issue #5968).
 *
 * <p>Retry-hidden (intra-run) and cross-launch transition counts are never collapsed into one
 * score. Unknown history is an explicit assessment, not a fabricated 0% rate. CI commit metadata
 * is optional for same-SHA scoring.
 */
public final class FlakeModels {
    private FlakeModels() {
    }

    /**
     * Flake table for IDE / MCP {@code report_flake} / CLI {@code shaft report flake}.
     *
     * @param schemaVersion response schema
     * @param empty whether no rows could be produced (no history and no retries)
     * @param emptyMessage user-facing empty-state text when {@code empty} is true
     * @param windowSize max launches considered for transition counting (TestOps-style, default 10)
     * @param transitionThreshold min transitions to tag {@code transitions} (default 3)
     * @param rows per-historyId dual-definition rows
     * @param warnings non-fatal notes
     */
    public record FlakeTable(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            int windowSize,
            int transitionThreshold,
            List<FlakeRow> rows,
            List<String> warnings) {
        public FlakeTable {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            windowSize = Math.max(1, windowSize);
            transitionThreshold = Math.max(1, transitionThreshold);
            rows = List.copyOf(rows == null ? List.of() : rows);
            warnings = List.copyOf(warnings == null ? List.of() : warnings);
        }
    }

    /**
     * One test's dual flake assessment. Display both {@code retryHidden} and
     * {@code transitionAssessment}/{@code transitionCount} — never a single combined score alone.
     *
     * @param historyId stable Allure history id
     * @param name display name
     * @param fullName full name when known
     * @param retryHidden true when an intra-run fail/broken attempt was hidden by a final pass
     * @param retryHiddenTag {@code retry-hidden} when applicable, else blank
     * @param transitionAssessment {@code unknown}, {@code always-failing}, {@code always-passing},
     *                             {@code transitions}, or {@code below-threshold}
     * @param transitionCount consecutive pass↔fail flips in the window; {@code null} when unknown
     * @param launchCount launches considered in the window
     * @param sameShaAvailable whether at least one launch carried CI commit metadata
     * @param sameShaTransitionCount transitions within the newest SHA cohort; {@code null} when N/A
     * @param tags distinct tags such as {@code retry-hidden} and/or {@code transitions} (never a
     *             combined flakiness percentage)
     */
    public record FlakeRow(
            String historyId,
            String name,
            String fullName,
            boolean retryHidden,
            String retryHiddenTag,
            String transitionAssessment,
            Integer transitionCount,
            int launchCount,
            boolean sameShaAvailable,
            Integer sameShaTransitionCount,
            List<String> tags) {
        public FlakeRow {
            historyId = historyId == null ? "" : historyId;
            name = name == null ? "" : name;
            fullName = fullName == null ? "" : fullName;
            retryHiddenTag = retryHiddenTag == null ? "" : retryHiddenTag;
            transitionAssessment = transitionAssessment == null || transitionAssessment.isBlank()
                    ? "unknown"
                    : transitionAssessment.trim();
            launchCount = Math.max(0, launchCount);
            tags = List.copyOf(tags == null ? List.of() : tags);
        }
    }
}
