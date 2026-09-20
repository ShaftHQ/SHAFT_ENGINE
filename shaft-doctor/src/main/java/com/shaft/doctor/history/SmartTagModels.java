package com.shaft.doctor.history;

import java.util.List;

/**
 * BrowserStack/Allure-style smart tags for Reporting and SHAFT Tests (S3-09 / issue #5975).
 *
 * <p>Tags: New, Always-failing, Flaky, Regressed, Fixed. Duration-anomaly is optional when
 * history carries timings. Unknown/insufficient history never invents Flaky (FR-003).
 */
public final class SmartTagModels {
    /** Shared schema version for smart-tag table responses. */
    public static final String SCHEMA_VERSION = "1.0";

    public static final String TAG_NEW = "New";
    public static final String TAG_ALWAYS_FAILING = "Always-failing";
    public static final String TAG_FLAKY = "Flaky";
    public static final String TAG_REGRESSED = "Regressed";
    public static final String TAG_FIXED = "Fixed";
    public static final String TAG_DURATION_ANOMALY = "Duration-anomaly";

    private SmartTagModels() {
    }

    /**
     * Smart-tag table for IDE / MCP {@code report_smart_tags} / CLI {@code shaft report tags}.
     *
     * @param schemaVersion response schema
     * @param empty whether no rows could be produced
     * @param emptyMessage user-facing empty-state when {@code empty}
     * @param windowSize max newest launches considered
     * @param flakyTransitionThreshold min flips to tag Flaky
     * @param rows per-historyId smart-tag rows
     * @param warnings non-fatal notes
     */
    public record SmartTagTable(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            int windowSize,
            int flakyTransitionThreshold,
            List<SmartTagRow> rows,
            List<String> warnings) {
        public SmartTagTable {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                    ? SCHEMA_VERSION
                    : schemaVersion.trim();
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            windowSize = Math.max(1, windowSize);
            flakyTransitionThreshold = Math.max(1, flakyTransitionThreshold);
            rows = List.copyOf(rows == null ? List.of() : rows);
            warnings = List.copyOf(warnings == null ? List.of() : warnings);
        }
    }

    /**
     * One test's smart tags derived from cross-run history.
     *
     * @param historyId stable Allure history id
     * @param name display name
     * @param fullName full name when known
     * @param primaryTag highest-priority tag, or blank when none apply
     * @param tags all applicable tags (never invents Flaky from insufficient history)
     * @param launchCount launches considered in the window
     * @param transitionCount pass↔fail flips; {@code null} when unknown/insufficient
     * @param durationAnomaly whether the newest duration is anomalous (only when timings exist)
     * @param newestStatus newest launch status
     * @param previousStatus previous launch status when known, else blank
     */
    public record SmartTagRow(
            String historyId,
            String name,
            String fullName,
            String primaryTag,
            List<String> tags,
            int launchCount,
            Integer transitionCount,
            boolean durationAnomaly,
            String newestStatus,
            String previousStatus) {
        public SmartTagRow {
            historyId = historyId == null ? "" : historyId;
            name = name == null ? "" : name;
            fullName = fullName == null ? "" : fullName;
            primaryTag = primaryTag == null ? "" : primaryTag.trim();
            tags = List.copyOf(tags == null ? List.of() : tags);
            launchCount = Math.max(0, launchCount);
            newestStatus = newestStatus == null ? "" : newestStatus;
            previousStatus = previousStatus == null ? "" : previousStatus;
        }
    }
}
