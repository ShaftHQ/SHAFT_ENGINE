package com.shaft.doctor.collect;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;
import tools.jackson.databind.json.JsonMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies the SHAFT single-file Allure HTML report parser against a small synthetic fixture that
 * mirrors the empirically observed embedded-data format (real single-file reports live under an
 * {@code allure-report} directory as {@code *AllureReport.html} and were used to confirm this shape).
 */
class AllureHtmlReportParserTest {
    private static final JsonMapper MAPPER = JsonMapper.builder().build();

    @Test
    void extractsEveryEmbeddedTestResultFromASingleFileReport(@TempDir Path temp) throws IOException {
        Path report = temp.resolve("2026-01-01_00-00-00-000_AllureReport.html");
        Files.writeString(report, syntheticReportHtml(), StandardCharsets.UTF_8);

        AllureHtmlReportParser.ParseResult result = new AllureHtmlReportParser().parse(report);

        assertTrue(result.reportDetected());
        assertEquals(0, result.malformedEntryCount());
        assertEquals(3, result.testResults().size());

        List<String> statuses = result.testResults().stream()
                .map(node -> node.path("status").asText())
                .sorted()
                .toList();
        assertEquals(List.of("broken", "failed", "passed"), statuses);

        JsonNode failed = result.testResults().stream()
                .filter(node -> "failed".equals(node.path("status").asText()))
                .findFirst().orElseThrow();
        assertEquals("shouldFailOnAssertion", failed.path("name").asText());
        assertEquals("expected [true] but found [false]", failed.path("error").path("message").asText());
        assertFalse(failed.path("error").path("trace").asText().isBlank());
        assertEquals("hist-failed", failed.path("historyId").asText());
    }

    @Test
    void nonAllureHtmlIsNotDetectedAsAReport(@TempDir Path temp) throws IOException {
        Path page = temp.resolve("page-source.html");
        Files.writeString(page, "<html><body>Just a captured page.</body></html>", StandardCharsets.UTF_8);

        AllureHtmlReportParser.ParseResult result = new AllureHtmlReportParser().parse(page);

        assertFalse(result.reportDetected());
        assertTrue(result.testResults().isEmpty());
        assertEquals(0, result.malformedEntryCount());
    }

    @Test
    void malformedEmbeddedEntriesAreSkippedWithoutThrowing(@TempDir Path temp) throws IOException {
        String html = "<script>window.allureReportData = {};</script>"
                + "<script>Promise.allSettled([d(\"data/test-results/broken.json\",\"not-valid-base64!!\")]);</script>";
        Path report = temp.resolve("AllureReport.html");
        Files.writeString(report, html, StandardCharsets.UTF_8);

        AllureHtmlReportParser.ParseResult result = new AllureHtmlReportParser().parse(report);

        assertTrue(result.reportDetected());
        assertTrue(result.testResults().isEmpty());
        assertEquals(1, result.malformedEntryCount());
    }

    @Test
    void missingFileIsHandledGracefully() {
        AllureHtmlReportParser.ParseResult result =
                new AllureHtmlReportParser().parse(Path.of("does-not-exist-anywhere.html"));

        assertFalse(result.reportDetected());
        assertTrue(result.testResults().isEmpty());
    }

    @Test
    void entryNameMatchingPrefixButNotJsonSuffixIsSkipped(@TempDir Path temp) throws IOException {
        // Covers the "!name.endsWith(\".json\")" side of the compound guard: a name that starts
        // with the test-result prefix but does not end in ".json" must be skipped like any other
        // non-test-result entry, never attempted for decoding.
        String html = "<script>window.allureReportData = {};</script>"
                + "<script>Promise.allSettled(["
                + "d(\"data/test-results/notes.txt\",\"" + Base64.getEncoder().encodeToString(
                        "{\"status\":\"passed\"}".getBytes(StandardCharsets.UTF_8)) + "\")"
                + "]);</script>";
        Path report = temp.resolve("AllureReport.html");
        Files.writeString(report, html, StandardCharsets.UTF_8);

        AllureHtmlReportParser.ParseResult result = new AllureHtmlReportParser().parse(report);

        assertTrue(result.reportDetected());
        assertTrue(result.testResults().isEmpty());
        assertEquals(0, result.malformedEntryCount(),
                "An entry outside the test-results/*.json shape is skipped, not counted as malformed");
    }

    @Test
    void decodedNonObjectJsonIsTreatedAsMalformed(@TempDir Path temp) throws IOException {
        // Covers the "node.isObject()" false side of the decode() ternary: valid base64 that
        // decodes into valid JSON which is not an object (a bare array here) must be rejected.
        String arrayPayload = Base64.getEncoder().encodeToString("[1,2,3]".getBytes(StandardCharsets.UTF_8));
        String html = "<script>window.allureReportData = {};</script>"
                + "<script>Promise.allSettled([d(\"data/test-results/array.json\",\"" + arrayPayload + "\")]);"
                + "</script>";
        Path report = temp.resolve("AllureReport.html");
        Files.writeString(report, html, StandardCharsets.UTF_8);

        AllureHtmlReportParser.ParseResult result = new AllureHtmlReportParser().parse(report);

        assertTrue(result.reportDetected());
        assertTrue(result.testResults().isEmpty());
        assertEquals(1, result.malformedEntryCount());
    }

    @Test
    void decodedBlankPayloadYieldingNullNodeIsTreatedAsMalformed(@TempDir Path temp) throws IOException {
        // Covers the "node != null" false side of the decode() ternary: valid, empty base64
        // decodes into an empty byte array, which Jackson's readTree treats as no content (null).
        String emptyPayload = Base64.getEncoder().encodeToString(new byte[0]);
        String html = "<script>window.allureReportData = {};</script>"
                + "<script>Promise.allSettled([d(\"data/test-results/empty.json\",\"" + emptyPayload + "\")]);"
                + "</script>";
        Path report = temp.resolve("AllureReport.html");
        Files.writeString(report, html, StandardCharsets.UTF_8);

        AllureHtmlReportParser.ParseResult result = new AllureHtmlReportParser().parse(report);

        assertTrue(result.reportDetected());
        assertTrue(result.testResults().isEmpty());
        assertEquals(1, result.malformedEntryCount());
    }

    private static String syntheticReportHtml() {
        String failed = encode(testResult("aaa1", "shouldFailOnAssertion", "failed", "hist-failed",
                "expected [true] but found [false]",
                "java.lang.AssertionError: expected [true] but found [false]\n\tat com.example.Suite"
                        + ".shouldFailOnAssertion(Suite.java:42)"));
        String passed = encode(testResult("bbb2", "shouldPass", "passed", "hist-passed", null, null));
        String broken = encode(testResult("ccc3", "shouldBreak", "broken", "hist-broken",
                "NullPointerException: Cannot invoke method on null object",
                "java.lang.NullPointerException: Cannot invoke method on null object\n\tat com.example.Suite"
                        + ".shouldBreak(Suite.java:88)"));
        return """
                <html><head></head><body>
                <script>
                  window.allureReportDataReady = false;
                  window.allureReportData = window.allureReportData || {};
                  function d(name, value){
                    return new Promise(function (resolve) {
                      window.allureReportData[name] = value;
                      return resolve(true);
                    });
                  }
                </script>
                <script defer>
                  Promise.allSettled([
                    d("widgets/statistic.json","eyJ0b3RhbCI6M30="),
                    d("data/test-results/aaa1.json","%s"),
                    d("data/test-results/bbb2.json","%s"),
                    d("data/test-results/ccc3.json","%s")
                  ]);
                </script>
                </body></html>
                """.formatted(failed, passed, broken);
    }

    private static ObjectNode testResult(
            String id, String name, String status, String historyId, String errorMessage, String errorTrace) {
        ObjectNode node = MAPPER.createObjectNode();
        node.put("id", id);
        node.put("name", name);
        node.put("fullName", "com.example.Suite." + name);
        node.put("status", status);
        node.put("start", 1_000);
        node.put("stop", 2_000);
        node.put("historyId", historyId);
        node.put("flaky", false);
        node.put("isRetry", false);
        node.put("retriesCount", 0);
        var labels = node.putArray("labels");
        labels.addObject().put("name", "testClass").put("value", "com.example.Suite");
        labels.addObject().put("name", "testMethod").put("value", name);
        if (errorMessage != null) {
            var error = node.putObject("error");
            error.put("message", errorMessage);
            error.put("trace", errorTrace);
        }
        node.putArray("steps");
        node.putArray("attachments");
        node.putArray("parameters");
        node.putArray("retries");
        return node;
    }

    private static String encode(ObjectNode node) {
        return Base64.getEncoder().encodeToString(MAPPER.writeValueAsString(node).getBytes(StandardCharsets.UTF_8));
    }
}
