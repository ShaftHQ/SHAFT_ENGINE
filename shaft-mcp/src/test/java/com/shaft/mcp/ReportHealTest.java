package com.shaft.mcp;

import com.shaft.doctor.history.HealInsightModels;
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

class ReportHealTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void reportHealSurfacesRecoveredAndAmbiguous(@TempDir Path workspace) throws Exception {
        Path reports = workspace.resolve("target/shaft-heal/reports");
        Files.createDirectories(reports);
        writeReport(reports, "ok-1", "RECOVERED", "cand-1", "PASSED");
        writeReport(reports, "amb-1", "AMBIGUOUS", "", "FAILED");
        writeReport(reports, "nc-1", "NO_CANDIDATES", "", "PENDING");

        TraceService service = new TraceService(McpWorkspacePolicy.of(workspace), new McpDoctorRemediationService());
        HealInsightModels.HealInsightsTable table = service.reportHeal(
                "target/shaft-heal/reports", null);

        assertFalse(table.empty());
        assertEquals(3, table.totalReports());
        assertTrue(table.insights().stream().anyMatch(i ->
                "RECOVERED".equals(i.status()) && i.canProposeSourcePatch()
                        && i.primaryAction() == HealInsightModels.PrimaryAction.REVIEW_DIFF));
        assertTrue(table.insights().stream().anyMatch(i ->
                "AMBIGUOUS".equals(i.status()) && !i.canProposeSourcePatch()
                        && i.primaryAction() == HealInsightModels.PrimaryAction.NONE));
        assertTrue(table.insights().stream().anyMatch(i -> "NO_CANDIDATES".equals(i.status())));
    }

    private static void writeReport(
            Path reports, String attemptId, String status, String selectedId, String outcome)
            throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "2.0");
        root.put("attemptId", attemptId);
        root.put("originalLocator", "By.id: old-login");
        ObjectNode decision = root.putObject("decision");
        decision.put("status", status);
        decision.put("selectedCandidateId", selectedId);
        decision.put("confidence", 0.9);
        decision.put("reason", "test");
        decision.put("actionOnly", true);
        decision.put("sourcePatchProposed", false);
        ObjectNode action = root.putObject("action");
        action.put("name", "click");
        action.put("recoveryUsed", true);
        action.put("outcome", outcome);
        action.put("postActionVerification", "PASSED".equals(outcome) ? "OK" : "FAILED");
        action.put("failure", "");
        ArrayNode candidates = root.putArray("candidates");
        if (!selectedId.isBlank()) {
            ObjectNode candidate = candidates.addObject();
            candidate.put("candidateId", selectedId);
            candidate.put("proposedLocator", "By.id: new-login");
            candidate.put("unique", true);
            candidate.put("contextMatched", true);
            candidate.putArray("evidence").add("id");
        }
        Files.writeString(reports.resolve(attemptId + ".json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root) + "\n",
                StandardCharsets.UTF_8);
    }
}
