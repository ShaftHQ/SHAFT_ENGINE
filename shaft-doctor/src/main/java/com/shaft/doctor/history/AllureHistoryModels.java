package com.shaft.doctor.history;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable view models for cross-run Allure history (S3-01 / issue #5967).
 *
 * <p>{@code HISTORY} rows come from Allure 3 {@code history.jsonl} (one launch per line).
 * {@code RETRY} rows come from multiple {@code *-result.json} files that share a
 * {@code historyId} inside a single {@code allure-results} tree. Doctor JSON is joined by
 * {@code historyId} only and never rewrites Allure artifacts.
 */
public final class AllureHistoryModels {
    private AllureHistoryModels() {
    }

    /**
     * Full ingest result for the Reporting canvas / MCP {@code report_history} tool.
     *
     * @param schemaVersion response schema
     * @param empty whether no history launches were found (missing or empty history.jsonl)
     * @param emptyMessage user-facing empty-state text when {@code empty} is true
     * @param historyPath resolved history.jsonl path that was read (may be missing)
     * @param doctorReportPath resolved Doctor JSON path that was read (may be blank)
     * @param allureResultsPath resolved allure-results path inspected for retries (may be blank)
     * @param allureResultsRootReplaced always {@code false}; ingest never replaces the results root
     * @param accumulateHistoryHonored whether ingest treated history as append-only Allure history
     * @param limitPerHistoryId max launches retained per historyId
     * @param launchCount number of history.jsonl launch lines ingested
     * @param tests per-historyId series (cross-run history)
     * @param retries intra-launch retries keyed by historyId
     * @param warnings non-fatal parse/join warnings
     */
    public record HistoryView(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            String historyPath,
            String doctorReportPath,
            String allureResultsPath,
            boolean allureResultsRootReplaced,
            boolean accumulateHistoryHonored,
            int limitPerHistoryId,
            int launchCount,
            List<TestHistorySeries> tests,
            List<RetryGroup> retries,
            List<String> warnings) {
        public HistoryView {
            schemaVersion = blankToDefault(schemaVersion, "1.0");
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            historyPath = historyPath == null ? "" : historyPath;
            doctorReportPath = doctorReportPath == null ? "" : doctorReportPath;
            allureResultsPath = allureResultsPath == null ? "" : allureResultsPath;
            tests = List.copyOf(tests == null ? List.of() : tests);
            retries = List.copyOf(retries == null ? List.of() : retries);
            warnings = List.copyOf(warnings == null ? List.of() : warnings);
            allureResultsRootReplaced = false;
        }
    }

    /**
     * Cross-run history for one {@code historyId}.
     *
     * @param historyId stable Allure history id
     * @param name display name from the newest launch
     * @param fullName full name from the newest launch
     * @param doctorCause optional Doctor primary cause joined by historyId
     * @param doctorSummary optional Doctor summary joined by historyId
     * @param launches newest-first launches (already truncated to the limit)
     */
    public record TestHistorySeries(
            String historyId,
            String name,
            String fullName,
            String doctorCause,
            String doctorSummary,
            List<LaunchStatus> launches) {
        public TestHistorySeries {
            historyId = historyId == null ? "" : historyId;
            name = name == null ? "" : name;
            fullName = fullName == null ? "" : fullName;
            doctorCause = doctorCause == null ? "" : doctorCause;
            doctorSummary = doctorSummary == null ? "" : doctorSummary;
            launches = List.copyOf(launches == null ? List.of() : launches);
        }
    }

    /**
     * One cross-run launch observation for a historyId.
     *
     * @param launchUuid history.jsonl launch uuid
     * @param launchName report/launch name
     * @param timestamp epoch millis from the history line
     * @param status Allure status
     * @param statusDetails optional status details message
     * @param durationMs duration when known
     * @param kind always {@code HISTORY} for cross-run rows
     * @param commitSha optional CI/git commit when present in history metadata (never required)
     */
    public record LaunchStatus(
            String launchUuid,
            String launchName,
            long timestamp,
            String status,
            String statusDetails,
            long durationMs,
            String kind,
            String commitSha) {
        public LaunchStatus {
            launchUuid = launchUuid == null ? "" : launchUuid;
            launchName = launchName == null ? "" : launchName;
            status = status == null ? "" : status;
            statusDetails = statusDetails == null ? "" : statusDetails;
            kind = blankToDefault(kind, "HISTORY");
            commitSha = commitSha == null ? "" : commitSha.trim();
        }

        /** Back-compat constructor without CI metadata (S3-01 callers). */
        public LaunchStatus(
                String launchUuid,
                String launchName,
                long timestamp,
                String status,
                String statusDetails,
                long durationMs,
                String kind) {
            this(launchUuid, launchName, timestamp, status, statusDetails, durationMs, kind, "");
        }
    }

    /**
     * Intra-launch retries for one historyId inside a single allure-results tree.
     *
     * @param historyId stable Allure history id
     * @param name display name
     * @param attempts newest-first retry attempts ({@code RETRY} kind)
     */
    public record RetryGroup(String historyId, String name, List<RetryAttempt> attempts) {
        public RetryGroup {
            historyId = historyId == null ? "" : historyId;
            name = name == null ? "" : name;
            attempts = List.copyOf(attempts == null ? List.of() : attempts);
        }
    }

    /**
     * One intra-launch retry attempt.
     *
     * @param resultUuid Allure result uuid
     * @param status status
     * @param start epoch millis
     * @param stop epoch millis
     * @param kind always {@code RETRY}
     */
    public record RetryAttempt(String resultUuid, String status, long start, long stop, String kind) {
        public RetryAttempt {
            resultUuid = resultUuid == null ? "" : resultUuid;
            status = status == null ? "" : status;
            kind = blankToDefault(kind, "RETRY");
        }
    }

    /**
     * Mutable builder used while merging append-only history lines and Doctor joins.
     */
    static final class SeriesBuilder {
        private final String historyId;
        private String name = "";
        private String fullName = "";
        private String doctorCause = "";
        private String doctorSummary = "";
        private final List<LaunchStatus> launches = new ArrayList<>();

        SeriesBuilder(String historyId) {
            this.historyId = Objects.requireNonNull(historyId, "historyId");
        }

        void addLaunch(LaunchStatus launch) {
            launches.add(launch);
        }

        void noteNames(String nextName, String nextFullName) {
            if (nextName != null && !nextName.isBlank()) {
                name = nextName.trim();
            }
            if (nextFullName != null && !nextFullName.isBlank()) {
                fullName = nextFullName.trim();
            }
        }

        void joinDoctor(String cause, String summary) {
            if (doctorCause.isBlank() && cause != null && !cause.isBlank()) {
                doctorCause = cause.trim();
            }
            if (doctorSummary.isBlank() && summary != null && !summary.isBlank()) {
                doctorSummary = summary.trim();
            }
        }

        TestHistorySeries build(int limit) {
            Map<String, LaunchStatus> dedup = new LinkedHashMap<>();
            for (LaunchStatus launch : launches) {
                String key = launch.launchUuid().isBlank()
                        ? launch.timestamp() + ":" + launch.status()
                        : launch.launchUuid();
                dedup.put(key, launch);
            }
            List<LaunchStatus> ordered = new ArrayList<>(dedup.values());
            ordered.sort(Comparator.comparingLong(LaunchStatus::timestamp).reversed());
            if (limit > 0 && ordered.size() > limit) {
                ordered = new ArrayList<>(ordered.subList(0, limit));
            }
            if (!ordered.isEmpty()) {
                // Prefer newest launch names when the builder saw older lines last.
            }
            return new TestHistorySeries(historyId, name, fullName, doctorCause, doctorSummary, ordered);
        }
    }

    private static String blankToDefault(String value, String fallback) {
        return value == null || value.isBlank() ? fallback : value.trim();
    }
}
