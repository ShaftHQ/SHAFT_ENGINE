package com.shaft.doctor.history;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Aggregates persisted SHAFT Heal reports into Reporting-canvas heal insights (issue #5972 / S3-06).
 *
 * <p>FR-001: dashboard counts by {@code HealingDecision} status.
 * FR-002: {@code canProposeSourcePatch} only when RECOVERED + passing action (persist-on-pass).
 * FR-003: never auto-writes locators — clients must call {@code doctor_propose_healed_locator}
 * for a reviewable diff, then a separate explicit Doctor repair publish.
 * SC-001: AMBIGUOUS primary action is {@link HealInsightModels.PrimaryAction#NONE}.
 * SC-002: RECOVERED + pass offers {@link HealInsightModels.PrimaryAction#REVIEW_DIFF}.
 * Edge: NO_CANDIDATES is counted and listed; failed replay never sets canProposeSourcePatch.
 */
public final class HealInsightsAggregator {
    private static final ObjectMapper JSON = JsonMapper.builder().build();
    private static final String EMPTY_MESSAGE =
            "No heal insights yet. Run tests with SHAFT Heal enabled so reports land under "
                    + "target/shaft-heal/reports, or point reportsPath at a directory of "
                    + "HealingReport JSON. Reuse healer_run_failed_test for a guarded passing "
                    + "replay before proposing a locator patch.";

    /** Stable display order matching HealingDecision.Status declaration order. */
    private static final List<String> STATUS_ORDER = List.of(
            "RECOVERED",
            "NO_HISTORY",
            "NO_CANDIDATES",
            "BELOW_THRESHOLD",
            "AMBIGUOUS",
            "REJECTED_PRECONDITION",
            "PROVIDER_FALLBACK");

    private HealInsightsAggregator() {
    }

    /**
     * Aggregates heal reports under the default {@code target/shaft-heal/reports} layout.
     *
     * @param reportsDirectory directory of {@code <attemptId>.json} HealingReport files (may be missing)
     * @return heal insights table
     */
    public static HealInsightModels.HealInsightsTable aggregate(Path reportsDirectory) {
        return aggregate(reportsDirectory, null);
    }

    /**
     * Aggregates heal reports and optionally attaches existing proposal manifests by attempt id.
     *
     * @param reportsDirectory directory of HealingReport JSON files (may be missing → empty-state)
     * @param proposalsDirectory optional directory of healing-locator-proposal-*.json manifests
     * @return heal insights table
     */
    public static HealInsightModels.HealInsightsTable aggregate(
            Path reportsDirectory, Path proposalsDirectory) {
        List<String> warnings = new ArrayList<>();
        String reportsPath = pathString(reportsDirectory);
        String proposalsPath = pathString(proposalsDirectory);

        Map<String, String> proposalsByAttempt = indexProposals(proposalsDirectory, warnings);

        if (reportsDirectory == null || !Files.isDirectory(reportsDirectory)) {
            if (reportsDirectory != null) {
                warnings.add("heal reports path is not a directory: " + reportsPath);
            }
            return emptyTable(reportsPath, proposalsPath, warnings);
        }

        List<Path> files = listReportFiles(reportsDirectory, warnings);
        if (files.isEmpty()) {
            return emptyTable(reportsPath, proposalsPath, warnings);
        }

        Map<String, Integer> counts = new LinkedHashMap<>();
        for (String status : STATUS_ORDER) {
            counts.put(status, 0);
        }
        List<HealInsightModels.HealInsight> insights = new ArrayList<>();
        int read = 0;
        for (Path file : files) {
            try {
                JsonNode report = JSON.readTree(Files.readString(file, StandardCharsets.UTF_8));
                HealInsightModels.HealInsight insight = toInsight(report, file, proposalsByAttempt);
                if (insight.attemptId().isBlank() && insight.status().isBlank()) {
                    warnings.add("Skipped unreadable heal report (missing attemptId/status): " + file);
                    continue;
                }
                bump(counts, insight.status());
                insights.add(insight);
                read++;
            } catch (IOException | RuntimeException exception) {
                warnings.add("Failed to parse heal report: " + file.getFileName()
                        + " (" + shortMessage(exception) + ")");
            }
        }

        if (read == 0) {
            return emptyTable(reportsPath, proposalsPath, warnings);
        }

        insights.sort(Comparator
                .comparing(HealInsightModels.HealInsight::canProposeSourcePatch).reversed()
                .thenComparing(HealInsightModels.HealInsight::status)
                .thenComparing(HealInsightModels.HealInsight::attemptId));

        List<HealInsightModels.StatusCount> statusCounts = new ArrayList<>();
        for (Map.Entry<String, Integer> entry : counts.entrySet()) {
            if (entry.getValue() > 0 || STATUS_ORDER.contains(entry.getKey())) {
                statusCounts.add(new HealInsightModels.StatusCount(entry.getKey(), entry.getValue()));
            }
        }

        return new HealInsightModels.HealInsightsTable(
                HealInsightModels.SCHEMA_VERSION,
                false,
                "",
                reportsPath,
                proposalsPath,
                read,
                statusCounts,
                insights,
                List.copyOf(warnings));
    }

    /**
     * Persist-on-pass review gate mirroring {@code HealingLocatorProposalService#validateReport}.
     *
     * @param status decision status
     * @param selectedCandidateId selected candidate id
     * @param actionOutcome action outcome
     * @param postActionVerification post-action verification
     * @return {@code true} only when a source-patch proposal may be persisted for review
     */
    public static boolean mayPersistProposal(
            String status,
            String selectedCandidateId,
            String actionOutcome,
            String postActionVerification) {
        if (!"RECOVERED".equals(normalize(status))) {
            return false;
        }
        if (selectedCandidateId == null || selectedCandidateId.isBlank()) {
            return false;
        }
        if (!"PASSED".equals(normalize(actionOutcome))) {
            return false;
        }
        String verification = normalize(postActionVerification);
        return !"FAILED".equals(verification)
                && !"ELEMENT_NOT_INTERACTABLE".equals(verification)
                && !"ELEMENT_STALE".equals(verification);
    }

    private static HealInsightModels.HealInsight toInsight(
            JsonNode report, Path file, Map<String, String> proposalsByAttempt) {
        String attemptId = text(report, "attemptId");
        JsonNode decision = report.path("decision");
        String status = text(decision, "status");
        String selectedId = text(decision, "selectedCandidateId");
        double confidence = decision.path("confidence").asDouble(0);
        String reason = text(decision, "reason");
        boolean sourcePatchProposed = decision.path("sourcePatchProposed").asBoolean(false);
        JsonNode action = report.path("action");
        String outcome = text(action, "outcome");
        String verification = text(action, "postActionVerification");
        String original = text(report, "originalLocator");
        String proposed = selectedLocator(report, selectedId);

        boolean canPropose = mayPersistProposal(status, selectedId, outcome, verification);
        HealInsightModels.PrimaryAction primary = canPropose
                ? HealInsightModels.PrimaryAction.REVIEW_DIFF
                : HealInsightModels.PrimaryAction.NONE;
        // SC-001: AMBIGUOUS explicitly has no apply-to-source primary action.
        if ("AMBIGUOUS".equals(normalize(status))) {
            primary = HealInsightModels.PrimaryAction.NONE;
            canPropose = false;
        }
        String label = primary == HealInsightModels.PrimaryAction.REVIEW_DIFF
                ? "Review patch"
                : "";
        String proposalPath = proposalsByAttempt.getOrDefault(attemptId, "");

        return new HealInsightModels.HealInsight(
                attemptId,
                file.toAbsolutePath().normalize().toString(),
                status.isBlank() ? "UNKNOWN" : status,
                original,
                proposed,
                confidence,
                outcome.isBlank() ? "PENDING" : outcome,
                verification,
                reason,
                canPropose,
                primary,
                label,
                proposalPath,
                sourcePatchProposed);
    }

    private static String selectedLocator(JsonNode report, String selectedId) {
        if (selectedId == null || selectedId.isBlank()) {
            return "";
        }
        for (JsonNode candidate : report.path("candidates")) {
            if (selectedId.equals(text(candidate, "candidateId"))) {
                return text(candidate, "proposedLocator");
            }
        }
        return "";
    }

    private static Map<String, String> indexProposals(Path proposalsDirectory, List<String> warnings) {
        Map<String, String> byAttempt = new LinkedHashMap<>();
        if (proposalsDirectory == null || !Files.isDirectory(proposalsDirectory)) {
            return byAttempt;
        }
        try (Stream<Path> stream = Files.list(proposalsDirectory)) {
            stream.filter(Files::isRegularFile)
                    .filter(path -> {
                        String name = path.getFileName().toString();
                        return name.startsWith("healing-locator-proposal-") && name.endsWith(".json");
                    })
                    .sorted()
                    .forEach(path -> {
                        try {
                            JsonNode node = JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
                            String attemptId = text(node, "healingAttemptId");
                            if (!attemptId.isBlank()) {
                                byAttempt.putIfAbsent(attemptId, path.toAbsolutePath().normalize().toString());
                            }
                        } catch (IOException | RuntimeException exception) {
                            warnings.add("Failed to index proposal manifest: " + path.getFileName());
                        }
                    });
        } catch (IOException exception) {
            warnings.add("Could not list proposals directory: " + shortMessage(exception));
        }
        return byAttempt;
    }

    private static List<Path> listReportFiles(Path reportsDirectory, List<String> warnings) {
        try (Stream<Path> stream = Files.list(reportsDirectory)) {
            return stream.filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".json"))
                    .sorted()
                    .toList();
        } catch (IOException exception) {
            warnings.add("Could not list heal reports: " + shortMessage(exception));
            return List.of();
        }
    }

    private static HealInsightModels.HealInsightsTable emptyTable(
            String reportsPath, String proposalsPath, List<String> warnings) {
        List<HealInsightModels.StatusCount> zeros = new ArrayList<>();
        for (String status : STATUS_ORDER) {
            zeros.add(new HealInsightModels.StatusCount(status, 0));
        }
        return new HealInsightModels.HealInsightsTable(
                HealInsightModels.SCHEMA_VERSION,
                true,
                EMPTY_MESSAGE,
                reportsPath,
                proposalsPath,
                0,
                zeros,
                List.of(),
                List.copyOf(warnings));
    }

    private static void bump(Map<String, Integer> counts, String status) {
        String key = status == null || status.isBlank() ? "UNKNOWN" : status;
        counts.merge(key, 1, Integer::sum);
    }

    private static String text(JsonNode node, String field) {
        JsonNode value = node.path(field);
        return value.isMissingNode() || value.isNull() ? "" : value.asText("");
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().toUpperCase(Locale.ROOT);
    }

    private static String pathString(Path path) {
        return path == null ? "" : path.toAbsolutePath().normalize().toString();
    }

    private static String shortMessage(Throwable exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? exception.getClass().getSimpleName() : message;
    }
}
