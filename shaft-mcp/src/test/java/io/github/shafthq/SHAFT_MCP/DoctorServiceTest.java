package io.github.shafthq.SHAFT_MCP;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.doctor.model.CauseCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorServiceTest {
    @Test
    void toolAnalyzesAllowlistedEvidenceOffline(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("allure-results"));
        Path result = input.resolve("mcp-result.json");
        Files.writeString(result, new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "mcp",
                "historyId", "mcp",
                "name", "mcp",
                "fullName", "example.Mcp.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var analysis = new DoctorService().analyze(
                List.of(input.toString()),
                List.of(),
                List.of(temp.toString()),
                temp.resolve("doctor-output").toString(),
                false,
                false,
                1);

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, analysis.diagnosis().primaryCause());
        assertEquals(2, analysis.evidenceItemCount());
        assertTrue(Files.isRegularFile(Path.of(analysis.markdownReportPath())));
    }
}
