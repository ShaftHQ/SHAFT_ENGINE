package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

/** Keeps the Playwright parity matrix honest: every claim has evidence or a tracked issue. */
public class TraceViewerParityMatrixTest {
    private static final Path MATRIX = Path.of("src/test/resources/trace-viewer/playwright-parity-matrix.json");
    private static final Path VIEWER_RESOURCES = Path.of("src/main/resources/META-INF/shaft/trace-viewer");
    private static final Set<String> STATUSES = Set.of("present", "partial", "missing");

    @Test
    public void everyParityRowShouldHaveEvidenceOrATrackedIssue() throws Exception {
        JsonNode matrix = new ObjectMapper().readTree(Files.readString(MATRIX));
        StringBuilder viewerSource = new StringBuilder();
        try (var files = Files.list(VIEWER_RESOURCES)) {
            for (Path file : files.sorted().toList()) {
                viewerSource.append(Files.readString(file)).append('\n');
            }
        }
        String viewer = viewerSource.toString();
        Set<String> ids = new HashSet<>();
        Assert.assertTrue(matrix.path("rows").size() >= 15, "The matrix must cover the Playwright feature set.");
        for (JsonNode row : matrix.path("rows")) {
            String id = row.path("id").asText();
            Assert.assertTrue(ids.add(id), "Duplicate parity row: " + id);
            String status = row.path("status").asText();
            Assert.assertTrue(STATUSES.contains(status), id + " has an unknown status: " + status);
            if (!"missing".equals(status)) {
                String evidence = row.path("evidence").asText();
                Assert.assertFalse(evidence.isBlank(), id + " claims support without evidence.");
                Assert.assertTrue(viewer.contains(evidence), id + " evidence not found in the viewer: " + evidence);
            }
            if (!"present".equals(status)) {
                Assert.assertTrue(row.path("issue").asInt() > 0, id + " is not present and must link an issue.");
                Assert.assertFalse(row.path("gap").asText().isBlank(), id + " must describe its gap.");
            }
        }
    }
}
