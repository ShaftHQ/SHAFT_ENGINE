package com.shaft.doctor.history;

import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HealInsightsAggregatorTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void emptyDirectoryIsEmptyState(@TempDir Path root) throws Exception {
        Path reports = root.resolve("reports");
        Files.createDirectories(reports);
        HealInsightModels.HealInsightsTable table = HealInsightsAggregator.aggregate(reports);
        assertTrue(table.empty());
        assertTrue(table.emptyMessage().contains("No heal insights"));
        assertEquals(0, table.totalReports());
        assertTrue(table.statusCounts().stream().anyMatch(c -> "NO_CANDIDATES".equals(c.status())));
    }

    @Test
    void countsRecoveredAmbiguousAndNoCandidates(@TempDir Path root) throws Exception {
        Path reports = root.resolve("reports");
        Files.createDirectories(reports);
        writeReport(reports, "a1", "RECOVERED", "cand-1", "PASSED", "OK", true);
        writeReport(reports, "a2", "AMBIGUOUS", "", "FAILED", "FAILED", false);
        writeReport(reports, "a3", "NO_CANDIDATES", "", "PENDING", "UNVERIFIABLE", false);

        HealInsightModels.HealInsightsTable table = HealInsightsAggregator.aggregate(reports);
        assertFalse(table.empty());
        assertEquals(3, table.totalReports());
        assertEquals(1, countOf(table, "RECOVERED"));
        assertEquals(1, countOf(table, "AMBIGUOUS"));
        assertEquals(1, countOf(table, "NO_CANDIDATES"));
        assertEquals(3, table.insights().size());
    }

    @Test
    void recoveredWithPassOffersReviewDiff(@TempDir Path root) throws Exception {
        Path reports = root.resolve("reports");
        Files.createDirectories(reports);
        writeReport(reports, "ok-1", "RECOVERED", "cand-1", "PASSED", "OK", true);

        HealInsightModels.HealInsight insight = HealInsightsAggregator.aggregate(reports).insights().getFirst();
        assertTrue(insight.canProposeSourcePatch());
        assertEquals(HealInsightModels.PrimaryAction.REVIEW_DIFF, insight.primaryAction());
        assertEquals("Review patch", insight.primaryActionLabel());
        assertEquals("By.id: new-login", insight.proposedLocator());
    }

    @Test
    void ambiguousHasNoApplyToSourcePrimaryAction(@TempDir Path root) throws Exception {
        Path reports = root.resolve("reports");
        Files.createDirectories(reports);
        writeReport(reports, "amb-1", "AMBIGUOUS", "cand-1", "PASSED", "OK", true);

        HealInsightModels.HealInsight insight = HealInsightsAggregator.aggregate(reports).insights().getFirst();
        assertEquals("AMBIGUOUS", insight.status());
        assertFalse(insight.canProposeSourcePatch());
        assertEquals(HealInsightModels.PrimaryAction.NONE, insight.primaryAction());
        assertTrue(insight.primaryActionLabel().isBlank());
    }

    @Test
    void failedReplayNeverPersists(@TempDir Path root) throws Exception {
        Path reports = root.resolve("reports");
        Files.createDirectories(reports);
        writeReport(reports, "fail-1", "RECOVERED", "cand-1", "FAILED", "FAILED", true);

        HealInsightModels.HealInsight insight = HealInsightsAggregator.aggregate(reports).insights().getFirst();
        assertFalse(insight.canProposeSourcePatch());
        assertEquals(HealInsightModels.PrimaryAction.NONE, insight.primaryAction());
        assertFalse(HealInsightsAggregator.mayPersistProposal(
                "RECOVERED", "cand-1", "FAILED", "FAILED"));
    }

    @Test
    void noCandidatesIsShownNotSilent(@TempDir Path root) throws Exception {
        Path reports = root.resolve("reports");
        Files.createDirectories(reports);
        writeReport(reports, "nc-1", "NO_CANDIDATES", "", "PENDING", "UNVERIFIABLE", false);

        HealInsightModels.HealInsightsTable table = HealInsightsAggregator.aggregate(reports);
        assertEquals(1, countOf(table, "NO_CANDIDATES"));
        assertEquals("NO_CANDIDATES", table.insights().getFirst().status());
        assertEquals(HealInsightModels.PrimaryAction.NONE, table.insights().getFirst().primaryAction());
    }

    @Test
    void indexesExistingProposalManifest(@TempDir Path root) throws Exception {
        Path reports = root.resolve("reports");
        Path proposals = root.resolve("proposals");
        Files.createDirectories(reports);
        Files.createDirectories(proposals);
        writeReport(reports, "ok-2", "RECOVERED", "cand-1", "PASSED", "OK", true);
        ObjectNode proposal = JSON.createObjectNode();
        proposal.put("healingAttemptId", "ok-2");
        proposal.put("proposalId", "heal-ok-2-deadbeef");
        Path manifest = proposals.resolve("healing-locator-proposal-heal-ok-2-deadbeef.json");
        Files.writeString(manifest, proposal.toString(), StandardCharsets.UTF_8);

        HealInsightModels.HealInsight insight =
                HealInsightsAggregator.aggregate(reports, proposals).insights().getFirst();
        assertTrue(insight.proposalManifestPath().endsWith(manifest.getFileName().toString()));
    }

    private static int countOf(HealInsightModels.HealInsightsTable table, String status) {
        return table.statusCounts().stream()
                .filter(count -> status.equals(count.status()))
                .mapToInt(HealInsightModels.StatusCount::count)
                .findFirst()
                .orElse(0);
    }

    private static void writeReport(
            Path reports,
            String attemptId,
            String status,
            String selectedId,
            String outcome,
            String verification,
            boolean withCandidate) throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "2.0");
        root.put("attemptId", attemptId);
        root.put("originalLocator", "By.id: old-login");
        ObjectNode decision = root.putObject("decision");
        decision.put("status", status);
        decision.put("selectedCandidateId", selectedId);
        decision.put("confidence", 0.91);
        decision.put("reason", "test");
        decision.put("actionOnly", true);
        decision.put("sourcePatchProposed", false);
        ObjectNode action = root.putObject("action");
        action.put("name", "click");
        action.put("recoveryUsed", true);
        action.put("outcome", outcome);
        action.put("postActionVerification", verification);
        action.put("failure", "");
        ArrayNode candidates = root.putArray("candidates");
        if (withCandidate) {
            ObjectNode candidate = candidates.addObject();
            candidate.put("candidateId", selectedId.isBlank() ? "cand-1" : selectedId);
            candidate.put("proposedLocator", "By.id: new-login");
            candidate.put("unique", true);
            candidate.put("contextMatched", true);
            candidate.put("interactable", true);
            candidate.putArray("evidence").add("id");
        }
        Files.writeString(reports.resolve(attemptId + ".json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root) + "\n",
                StandardCharsets.UTF_8);
    }
}
