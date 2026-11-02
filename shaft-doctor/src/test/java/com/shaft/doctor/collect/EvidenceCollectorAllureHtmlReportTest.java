package com.shaft.doctor.collect;

import com.shaft.doctor.DoctorAnalyzer;
import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;
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
 * Proves a SHAFT single-file Allure HTML report ({@code *AllureReport.html}) is collected into the
 * same ALLURE_RESULT evidence shape as individual {@code *-result.json} files, so the deterministic
 * rule engine can diagnose a run from the report alone.
 */
class EvidenceCollectorAllureHtmlReportTest {
    private static final JsonMapper MAPPER = JsonMapper.builder().build();

    @Test
    void htmlReportYieldsAllureResultEvidenceForEveryEmbeddedTest(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("allure-report"));
        Files.writeString(inputDir.resolve("2026-01-01_00-00-00-000_AllureReport.html"),
                syntheticReportHtml(), StandardCharsets.UTF_8);
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        EvidenceCollector collector = new EvidenceCollector();
        // includePageSnapshots is deliberately false: an Allure report is not a page snapshot and
        // must not be gated by that flag.
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                false,
                false,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);

        EvidenceBundle bundle = collector.collect(request);

        List<EvidenceItem> allure = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .filter(item -> !"true".equals(item.attributes().get("invalid")))
                .toList();
        assertEquals(3, allure.size(), "One ALLURE_RESULT item per embedded test result: " + allure);
        assertTrue(bundle.evidence().stream().noneMatch(item -> item.category() == EvidenceCategory.PAGE_SNAPSHOT),
                "An Allure HTML report must never be routed to PAGE_SNAPSHOT");

        EvidenceItem failed = allure.stream()
                .filter(item -> "failed".equals(item.attributes().get("status")))
                .findFirst().orElseThrow();
        assertEquals("hist-failed", failed.attributes().get("historyId"));
        assertEquals("shouldFailOnAssertion", failed.attributes().get("testMethod"));
        assertEquals("expected [true] but found [false]", failed.attributes().get("failureMessage"));
        assertFalse(failed.attributes().get("signature").isBlank());
        assertEquals("allure-result-json", failed.provenance().adapter());

        EvidenceItem broken = allure.stream()
                .filter(item -> "broken".equals(item.attributes().get("status")))
                .findFirst().orElseThrow();
        assertEquals("hist-broken", broken.attributes().get("historyId"));

        boolean exceptionChainPresent = bundle.evidence().stream()
                .anyMatch(item -> item.category() == EvidenceCategory.EXCEPTION_CHAIN
                        && item.content() != null
                        && item.content().contains("AssertionError"));
        assertTrue(exceptionChainPresent, "Failure trace must also surface as an EXCEPTION_CHAIN item");
    }

    @Test
    void deterministicAnalysisDiagnosesAFailureFromTheHtmlReportAlone(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("allure-report"));
        Files.writeString(inputDir.resolve("AllureReport.html"), syntheticReportHtml(), StandardCharsets.UTF_8);

        DoctorAnalysisResult result = new DoctorAnalyzer().analyze(new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                temp.resolve("doctor-output"),
                false,
                false,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true));

        assertEquals(CauseCategory.TEST, result.diagnosis().primaryCause());
        assertFalse(result.diagnosis().findings().isEmpty());
    }

    @Test
    void nonAllureHtmlPageSnapshotStillHonorsIncludePageSnapshotsFlag(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("page-source.html"),
                "<html><body>Captured page, not an Allure report.</body></html>", StandardCharsets.UTF_8);
        EvidenceCollector collector = new EvidenceCollector();
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                Files.createDirectories(temp.resolve("output")),
                false,
                false,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);

        EvidenceBundle bundle = collector.collect(request);

        assertTrue(bundle.evidence().isEmpty(),
                "A plain page snapshot must still be omitted when includePageSnapshots is false");
    }

    @Test
    void htmlReportWithNoDecodableTestResultsYieldsAnInvalidWarningItem(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("allure-report"));
        String html = "<script>window.allureReportData = {};</script>"
                + "<script>Promise.allSettled(["
                + "d(\"data/test-results/broken1.json\",\"not-valid-base64!!\"),"
                + "d(\"data/test-results/broken2.json\",\"also-not-valid!!\")"
                + "]);</script>";
        Files.writeString(inputDir.resolve("AllureReport.html"), html, StandardCharsets.UTF_8);
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        EvidenceCollector collector = new EvidenceCollector();
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                false,
                false,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);

        EvidenceBundle bundle = collector.collect(request);

        List<EvidenceItem> allure = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .toList();
        assertEquals(1, allure.size(), "A report with no decodable results yields exactly one warning item");
        EvidenceItem warning = allure.getFirst();
        assertEquals("true", warning.attributes().get("invalid"));
        assertTrue(warning.content().contains("2 embedded test result entries could not be decoded"),
                warning.content());
    }

    @Test
    void htmlReportWithNoEmbeddedTestResultsAtAllYieldsAGenericWarning(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("allure-report"));
        // The embedded-data marker is present, but no d("data/test-results/*.json", ...) calls exist
        // at all, so malformedEntryCount is 0 and the generic (not "could not be decoded") message applies.
        String html = "<script>window.allureReportData = {};</script>"
                + "<script>Promise.allSettled([d(\"widgets/statistic.json\",\"eyJ0b3RhbCI6MH0=\")]);</script>";
        Files.writeString(inputDir.resolve("AllureReport.html"), html, StandardCharsets.UTF_8);
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        EvidenceCollector collector = new EvidenceCollector();
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                false,
                false,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);

        EvidenceBundle bundle = collector.collect(request);

        List<EvidenceItem> allure = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .toList();
        assertEquals(1, allure.size());
        assertTrue(allure.getFirst().content().contains("contains no embedded test results"),
                allure.getFirst().content());
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
