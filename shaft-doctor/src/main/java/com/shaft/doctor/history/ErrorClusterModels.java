package com.shaft.doctor.history;

import java.util.List;

/**
 * Unique-error clusters keyed by Doctor historical-signature keys (S3-03 / issue #5969).
 *
 * <p>No cloud ML: clustering is deterministic fingerprint / normalized-signature grouping.
 * Each cluster lists impacted tests (error → tests). Empty results yield an empty-state, not an
 * error crash.
 */
public final class ErrorClusterModels {
    /** Shared schema version for cluster table responses. */
    public static final String SCHEMA_VERSION = "1.0";

    private ErrorClusterModels() {
    }

    /**
     * Cluster table for IDE / MCP {@code report_clusters} / CLI {@code shaft report clusters}.
     *
     * @param schemaVersion response schema
     * @param empty whether no clusters could be produced (no failed results / blank signatures)
     * @param emptyMessage user-facing empty-state text when {@code empty} is true
     * @param allureResultsPath resolved allure-results path inspected (may be blank)
     * @param doctorReportPath resolved Doctor JSON path inspected (may be blank)
     * @param clusterCount number of unique-signature clusters
     * @param impactedTestCount total impacted failed/broken tests across clusters
     * @param clusters signature → impacted tests, largest first
     * @param warnings non-fatal notes
     */
    public record ClusterTable(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            String allureResultsPath,
            String doctorReportPath,
            int clusterCount,
            int impactedTestCount,
            List<ErrorCluster> clusters,
            List<String> warnings) {
        public ClusterTable {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? SCHEMA_VERSION : schemaVersion.trim();
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            allureResultsPath = allureResultsPath == null ? "" : allureResultsPath;
            doctorReportPath = doctorReportPath == null ? "" : doctorReportPath;
            clusterCount = Math.max(0, clusterCount);
            impactedTestCount = Math.max(0, impactedTestCount);
            clusters = List.copyOf(clusters == null ? List.of() : clusters);
            warnings = List.copyOf(warnings == null ? List.of() : warnings);
        }
    }

    /**
     * One unique-error cluster: Doctor signature key plus the tests that share it.
     *
     * @param signatureKey Doctor historical-signature / {@code clusterFingerprint} key
     * @param displayError representative failure message (or signature key when message blank)
     * @param impactedCount number of impacted tests in this cluster
     * @param impactedTests tests sharing this signature (stable order by historyId then name)
     */
    public record ErrorCluster(
            String signatureKey,
            String displayError,
            int impactedCount,
            List<ImpactedTest> impactedTests) {
        public ErrorCluster {
            signatureKey = signatureKey == null ? "" : signatureKey;
            displayError = displayError == null || displayError.isBlank() ? signatureKey : displayError;
            impactedCount = Math.max(0, impactedCount);
            impactedTests = List.copyOf(impactedTests == null ? List.of() : impactedTests);
            if (impactedCount == 0) {
                impactedCount = impactedTests.size();
            }
        }
    }

    /**
     * One failed/broken test belonging to a signature cluster.
     *
     * @param historyId Allure history id when known
     * @param name display name
     * @param fullName full name when known
     * @param uuid Allure result uuid when known
     * @param status failed or broken
     * @param failureMessage shortened statusDetails message
     */
    public record ImpactedTest(
            String historyId,
            String name,
            String fullName,
            String uuid,
            String status,
            String failureMessage) {
        public ImpactedTest {
            historyId = historyId == null ? "" : historyId;
            name = name == null ? "" : name;
            fullName = fullName == null ? "" : fullName;
            uuid = uuid == null ? "" : uuid;
            status = status == null ? "" : status;
            failureMessage = failureMessage == null ? "" : failureMessage;
        }
    }
}
