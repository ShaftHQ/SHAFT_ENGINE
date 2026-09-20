package com.shaft.doctor.history;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Builds reconciled engineer/stakeholder Reporting summaries (issue #5976 / S3-10).
 *
 * <p>Counts Allure {@code *-result.json} finals, then reuses {@link DualFlakeComputer} and
 * {@link HealInsightsAggregator} for stability / heal tallies. Playbook shapes come from
 * {@code shaft-execution-reporting} and {@code shaft-stakeholder-reporting}. Never embeds secrets.
 */
public final class ReportSummaryComputer {
    private static final ObjectMapper JSON = JsonMapper.builder().build();
    private static final String EMPTY_MESSAGE =
            "No Allure results found. Run tests, then call generate_test_report (or Open Allure "
                    + "after generation) so the Reporting canvas can reconcile counts.";

    private ReportSummaryComputer() {
    }

    /**
     * Computes dual-audience summaries from optional local artifact paths.
     *
     * @param allureResultsRoot allure-results directory (may be missing)
     * @param reportHtml optional Allure HTML path (may be missing)
     * @param historyJsonl optional history.jsonl for flake
     * @param doctorJson optional Doctor JSON (path recorded only; contents never copied)
     * @param healReportsDir optional heal reports directory
     * @param healProposalsDir optional heal proposals directory
     * @return summary view with playbook-shaped text
     */
    public static ReportSummaryModels.SummaryView compute(
            Path allureResultsRoot,
            Path reportHtml,
            Path historyJsonl,
            Path doctorJson,
            Path healReportsDir,
            Path healProposalsDir) {
        List<String> warnings = new ArrayList<>();
        Map<String, Integer> counts = countFinalStatuses(allureResultsRoot, warnings);
        boolean empty = total(counts) == 0;
        if (empty) {
            return new ReportSummaryModels.SummaryView(
                    ReportSummaryModels.SCHEMA_VERSION,
                    true,
                    EMPTY_MESSAGE,
                    pathString(allureResultsRoot),
                    pathString(reportHtml),
                    zeroCounts(),
                    0,
                    0,
                    0,
                    0,
                    0,
                    EMPTY_MESSAGE,
                    EMPTY_MESSAGE,
                    List.copyOf(warnings));
        }

        FlakeModels.FlakeTable flake = flakeTable(historyJsonl, doctorJson, allureResultsRoot, warnings);
        int retryHidden = 0;
        int transitions = 0;
        if (flake != null && !flake.empty()) {
            for (FlakeModels.FlakeRow row : flake.rows()) {
                if (row.retryHidden()) {
                    retryHidden++;
                }
                if (row.tags().stream().anyMatch(tag -> "transitions".equalsIgnoreCase(tag))
                        || "transitions".equalsIgnoreCase(row.transitionAssessment())) {
                    transitions++;
                }
            }
        }

        HealInsightModels.HealInsightsTable heal =
                HealInsightsAggregator.aggregate(healReportsDir, healProposalsDir);
        int recovered = statusCount(heal, "RECOVERED");
        int ambiguous = statusCount(heal, "AMBIGUOUS");
        int noCandidates = statusCount(heal, "NO_CANDIDATES");

        return new ReportSummaryModels.SummaryView(
                ReportSummaryModels.SCHEMA_VERSION,
                false,
                "",
                pathString(allureResultsRoot),
                pathString(reportHtml),
                counts,
                retryHidden,
                transitions,
                recovered,
                ambiguous,
                noCandidates,
                engineerText(
                        counts,
                        pathString(allureResultsRoot),
                        pathString(reportHtml),
                        pathString(doctorJson),
                        retryHidden,
                        transitions,
                        heal.totalReports(),
                        recovered,
                        ambiguous,
                        noCandidates),
                stakeholderText(counts, retryHidden, transitions, recovered),
                List.copyOf(warnings));
    }

    /**
     * Resolves an open-Allure view without generating HTML (CTA only when missing).
     *
     * @param reportHtml candidate HTML path (may be missing)
     * @param allureResultsRoot related results directory (may be missing)
     * @param opened whether the caller opened the report in a browser
     * @return open view
     */
    public static ReportSummaryModels.OpenView openView(
            Path reportHtml, Path allureResultsRoot, boolean opened) {
        if (reportHtml != null && Files.isRegularFile(reportHtml)) {
            return new ReportSummaryModels.OpenView(
                    ReportSummaryModels.SCHEMA_VERSION,
                    false,
                    "",
                    pathString(reportHtml),
                    pathString(allureResultsRoot),
                    opened,
                    "",
                    List.of());
        }
        return new ReportSummaryModels.OpenView(
                ReportSummaryModels.SCHEMA_VERSION,
                true,
                EMPTY_MESSAGE,
                "",
                pathString(allureResultsRoot),
                false,
                "generate_test_report",
                List.of());
    }

    static Map<String, Integer> countFinalStatuses(Path allureResultsRoot, List<String> warnings) {
        Map<String, Integer> totals = zeroCounts();
        if (allureResultsRoot == null || !Files.isDirectory(allureResultsRoot)) {
            return totals;
        }
        Map<String, StatusAttempt> newest = new LinkedHashMap<>();
        try (Stream<Path> walk = Files.list(allureResultsRoot)) {
            List<Path> files = walk
                    .filter(path -> path.getFileName().toString().toLowerCase(Locale.ROOT)
                            .endsWith("-result.json"))
                    .sorted()
                    .toList();
            for (Path file : files) {
                try {
                    JsonNode result = JSON.readTree(Files.readString(file, StandardCharsets.UTF_8));
                    String historyId = text(result, "historyId");
                    String uuid = text(result, "uuid", "id");
                    String key = !historyId.isBlank() ? historyId : uuid;
                    if (key.isBlank()) {
                        key = file.getFileName().toString();
                    }
                    String status = normalizeStatus(text(result, "status"));
                    long stop = result.path("stop").asLong(result.path("start").asLong(0L));
                    StatusAttempt prior = newest.get(key);
                    if (prior == null || stop >= prior.stop()) {
                        newest.put(key, new StatusAttempt(status, stop));
                    }
                } catch (IOException | RuntimeException exception) {
                    warnings.add("Skipped " + file.getFileName() + ": " + exception.getMessage());
                }
            }
        } catch (IOException exception) {
            warnings.add("Unable to list allure-results: " + exception.getMessage());
            return totals;
        }
        for (StatusAttempt attempt : newest.values()) {
            totals.merge(attempt.status(), 1, Integer::sum);
            totals.merge("selected", 1, Integer::sum);
            totals.merge("started", 1, Integer::sum);
            totals.merge("completed", 1, Integer::sum);
        }
        return totals;
    }

    private static FlakeModels.FlakeTable flakeTable(
            Path historyJsonl, Path doctorJson, Path allureResultsRoot, List<String> warnings) {
        try {
            AllureHistoryModels.HistoryView history =
                    AllureHistoryIngestor.ingest(historyJsonl, doctorJson, allureResultsRoot, 10);
            return DualFlakeComputer.compute(history);
        } catch (RuntimeException exception) {
            warnings.add("Flake tally skipped: " + exception.getMessage());
            return null;
        }
    }

    private static int statusCount(HealInsightModels.HealInsightsTable table, String status) {
        if (table == null || table.empty()) {
            return 0;
        }
        return table.statusCounts().stream()
                .filter(row -> status.equalsIgnoreCase(row.status()))
                .mapToInt(HealInsightModels.StatusCount::count)
                .findFirst()
                .orElse(0);
    }

    private static Map<String, Integer> zeroCounts() {
        Map<String, Integer> totals = new LinkedHashMap<>();
        totals.put("selected", 0);
        totals.put("started", 0);
        totals.put("completed", 0);
        totals.put("passed", 0);
        totals.put("failed", 0);
        totals.put("broken", 0);
        totals.put("skipped", 0);
        totals.put("unknown", 0);
        return totals;
    }

    private static String engineerText(
            Map<String, Integer> counts,
            String resultsPath,
            String reportPath,
            String doctorPath,
            int retryHidden,
            int transitions,
            int healTotal,
            int recovered,
            int ambiguous,
            int noCandidates) {
        return """
                Audience: Engineering
                Period: latest local Allure results
                Objective: Reconcile run outcomes for triage (shaft-execution-reporting)
                Provenance: results=%s; report=%s; doctor=%s
                Reconciled: selected=%d started=%d completed=%d passed=%d failed=%d broken=%d skipped=%d unknown=%d
                Coverage: local workspace scope (not a release gate)
                Clusters: see Reporting Clusters surface (Doctor signatures)
                Defects: confirm labels on Doctor card — counts only in this summary
                Stability: flake retry-hidden=%d transitions=%d
                Heal: reports=%d recovered=%d ambiguous=%d no_candidates=%d
                Gate: local-only; no release recommendation from engineer summary
                Risks/next: Open Allure; review Doctor for failed/broken; copy stakeholder summary if needed
                """.formatted(
                blankDash(resultsPath),
                blankDash(reportPath),
                blankDash(doctorPath),
                counts.getOrDefault("selected", 0),
                counts.getOrDefault("started", 0),
                counts.getOrDefault("completed", 0),
                counts.getOrDefault("passed", 0),
                counts.getOrDefault("failed", 0),
                counts.getOrDefault("broken", 0),
                counts.getOrDefault("skipped", 0),
                counts.getOrDefault("unknown", 0),
                retryHidden,
                transitions,
                healTotal,
                recovered,
                ambiguous,
                noCandidates);
    }

    private static String stakeholderText(
            Map<String, Integer> counts, int retryHidden, int transitions, int recovered) {
        int passed = counts.getOrDefault("passed", 0);
        int failed = counts.getOrDefault("failed", 0);
        int broken = counts.getOrDefault("broken", 0);
        int skipped = counts.getOrDefault("skipped", 0);
        int blockers = failed + broken;
        String decision;
        if (blockers == 0 && passed > 0) {
            decision = "Go (local verification only — not a release sign-off)";
        } else if (blockers > 0 && passed > 0) {
            decision = "Conditional — investigate unresolved failures before release decisions";
        } else if (blockers > 0) {
            decision = "No-go until failures are understood";
        } else {
            decision = "No recommendation — evidence incomplete";
        }
        return """
                Decision: %s
                Scope: Latest local automated run in this workspace
                Verified outcomes: passed=%d failed=%d broken=%d skipped=%d
                Business risks: %d unresolved failing/broken test(s); flake signals retry-hidden=%d transitions=%d
                Trend: heal recovered=%d (review-gated; never auto-landed)
                Recommendation: %s
                Conditions: Treat this as a local health check; require engineering appendix for release
                Owners: Engineering owns failed/broken triage; stakeholders track decision only
                Technical appendix: engineer summary / Allure / Doctor / Heal panels
                """.formatted(
                decision,
                passed,
                failed,
                broken,
                skipped,
                blockers,
                retryHidden,
                transitions,
                recovered,
                decision);
    }

    private static String normalizeStatus(String status) {
        return switch (status == null ? "" : status.toLowerCase(Locale.ROOT)) {
            case "passed", "pass" -> "passed";
            case "failed", "fail" -> "failed";
            case "broken" -> "broken";
            case "skipped", "skip", "canceled", "cancelled" -> "skipped";
            default -> "unknown";
        };
    }

    private static int total(Map<String, Integer> counts) {
        return counts.getOrDefault("passed", 0)
                + counts.getOrDefault("failed", 0)
                + counts.getOrDefault("broken", 0)
                + counts.getOrDefault("skipped", 0)
                + counts.getOrDefault("unknown", 0);
    }

    private static String pathString(Path path) {
        return path == null ? "" : path.toAbsolutePath().normalize().toString().replace('\\', '/');
    }

    private static String blankDash(String value) {
        return value == null || value.isBlank() ? "—" : value;
    }

    private static String text(JsonNode node, String... fields) {
        if (node == null || node.isNull()) {
            return "";
        }
        for (String field : fields) {
            JsonNode value = node.get(field);
            if (value != null && !value.isNull() && !value.asText("").isBlank()) {
                return value.asText().trim();
            }
        }
        return "";
    }

    private record StatusAttempt(String status, long stop) {
    }
}
