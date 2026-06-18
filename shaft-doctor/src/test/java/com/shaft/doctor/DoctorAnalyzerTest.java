package com.shaft.doctor;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.doctor.cli.DoctorCli;
import com.shaft.doctor.ai.DoctorAiAnalysisService;
import com.shaft.doctor.analysis.DeterministicRuleEngine;
import com.shaft.doctor.collect.EvidenceCollector;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.DoctorAdvisory;
import com.shaft.doctor.model.DoctorAiAnalysisResult;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.report.DoctorReportWriter;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.EnumSet;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assumptions.abort;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorAnalyzerTest {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    @ParameterizedTest(name = "{0}")
    @MethodSource("classificationCases")
    void seededFixturesProduceStableCitedClassifications(
            String name,
            String message,
            CauseCategory expected,
            @TempDir Path temp) throws IOException {
        Path input = Files.createDirectories(temp.resolve("input"));
        writeResult(input.resolve(name + "-result.json"), "failed", message, "trace", name, 1);

        DoctorAnalysisResult result = analyze(temp, input, "out-" + name, 1, false, false, List.of());

        assertEquals(expected, result.diagnosis().primaryCause());
        assertFalse(result.diagnosis().findings().isEmpty());
        if (expected == CauseCategory.UNKNOWN) {
            assertFalse(result.diagnosis().missingEvidence().isEmpty());
        } else {
            assertTrue(result.diagnosis().findings().stream()
                    .filter(finding -> finding.kind().name().equals("INFERENCE"))
                    .anyMatch(finding -> !finding.evidenceIds().isEmpty()));
        }
    }

    @Test
    void emptyMalformedAndUnexpectedlySmallRunsAreNotSuccessful(@TempDir Path temp) throws IOException {
        Path empty = Files.createDirectories(temp.resolve("empty"));
        DoctorAnalysisResult emptyResult = analyze(temp, empty, "empty-out", 1, false, false, List.of());
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, emptyResult.diagnosis().primaryCause());
        assertTrue(emptyResult.diagnosis().summary().contains("incomplete"));

        Path malformed = Files.createDirectories(temp.resolve("malformed"));
        Files.writeString(malformed.resolve("broken-result.json"), "{\"status\":", StandardCharsets.UTF_8);
        DoctorAnalysisResult malformedResult =
                analyze(temp, malformed, "malformed-out", 1, false, false, List.of());
        assertEquals("1", malformedResult.bundle().metadata().get("invalidAllureResultCount"));
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, malformedResult.diagnosis().primaryCause());

        Path small = Files.createDirectories(temp.resolve("small"));
        writeResult(small.resolve("only-result.json"), "passed", "", "", "small", 1);
        DoctorAnalysisResult smallResult = analyze(temp, small, "small-out", 2, false, false, List.of());
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, smallResult.diagnosis().primaryCause());
        assertTrue(smallResult.diagnosis().rationale().contains("completeness"));
    }

    @Test
    void retriesAndHistoricalSignaturesRemainVisible(@TempDir Path temp) throws IOException {
        Path first = Files.createDirectories(temp.resolve("first"));
        writeResult(first.resolve("retry-1-result.json"), "failed",
                "NoSuchElementException: unable to locate element", "trace 123", "retry-history", 1);
        writeResult(first.resolve("retry-2-result.json"), "passed",
                "", "", "retry-history", 2);
        DoctorAnalysisResult firstResult = analyze(temp, first, "first-out", 1, false, false, List.of());

        assertEquals(CauseCategory.LOCATOR, firstResult.diagnosis().primaryCause());
        assertTrue(firstResult.diagnosis().findings().stream()
                .anyMatch(finding -> finding.ruleId().equals("retry-correlation")));
        var firstTriage = MAPPER.readTree(Path.of(firstResult.jsonReportPath()).getParent()
                .resolve("doctor-triage.json").toFile());
        assertEquals(1, firstTriage.path("hiddenRetryFailures").asInt());
        assertEquals(1, firstTriage.path("failingAttempts").asInt());
        assertTrue(Files.isRegularFile(Path.of(firstResult.jsonReportPath()).getParent()
                .resolve("execution-intelligence.md")));

        Path second = Files.createDirectories(temp.resolve("second"));
        writeResult(second.resolve("repeat-result.json"), "failed",
                "NoSuchElementException: unable to locate element", "trace 999", "other-history", 3);
        DoctorAnalysisResult repeated = analyze(temp, second, "second-out", 1, false, false,
                List.of(Path.of(firstResult.bundlePath())));

        assertTrue(repeated.diagnosis().findings().stream()
                .anyMatch(finding -> finding.ruleId().equals("historical-signature-correlation")));
        var repeatedIntelligence = MAPPER.readTree(Path.of(repeated.jsonReportPath()).getParent()
                .resolve("execution-intelligence.json").toFile());
        assertEquals(1, repeatedIntelligence.path("recurringFailures").asInt());
        assertTrue(repeatedIntelligence.path("summary").asText().contains("recurring"));
    }

    @Test
    void redactsSecretsPathsAndOptInPageEvidence(@TempDir Path temp) throws IOException {
        Path input = Files.createDirectories(temp.resolve("privacy-input"));
        String canary = "DO-NOT-RETAIN-SECRET";
        writeResult(input.resolve("secret-result.json"), "failed",
                "NoSuchElementException token=" + canary, "Authorization: Bearer " + canary,
                "privacy", 1);
        var resultTree = (com.fasterxml.jackson.databind.node.ObjectNode) MAPPER.readTree(
                input.resolve("secret-result.json").toFile());
        resultTree.set("parameters", MAPPER.valueToTree(List.of(
                Map.of("name", "password", "value", canary))));
        Files.writeString(input.resolve("secret-result.json"),
                MAPPER.writeValueAsString(resultTree), StandardCharsets.UTF_8);
        Files.writeString(input.resolve("custom.properties"),
                "username=user\npassword=" + canary + "\n", StandardCharsets.UTF_8);
        Files.writeString(input.resolve("page-source.html"),
                "<html><input type=\"password\" value=\"" + canary + "\"></html>",
                StandardCharsets.UTF_8);
        Files.write(input.resolve("screen.png"), new byte[]{1, 2, 3, 4});

        DoctorAnalysisResult defaultResult =
                analyze(temp, input, "privacy-default", 1, false, false, List.of());
        assertFalse(defaultResult.bundle().evidence().stream()
                .anyMatch(item -> item.category() == EvidenceCategory.SCREENSHOT
                        || item.category() == EvidenceCategory.PAGE_SNAPSHOT));
        assertOutputDoesNotContain(defaultResult, canary, temp.toString());

        DoctorAnalysisResult approved =
                analyze(temp, input, "privacy-approved", 1, true, true, List.of());
        assertTrue(approved.bundle().evidence().stream()
                .anyMatch(item -> item.category() == EvidenceCategory.SCREENSHOT));
        assertTrue(approved.bundle().evidence().stream()
                .anyMatch(item -> item.category() == EvidenceCategory.PAGE_SNAPSHOT));
        assertOutputDoesNotContain(approved, canary, temp.toString());
        Path retainedScreenshot = approved.bundle().evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.SCREENSHOT)
                .findFirst().orElseThrow()
                .relativePath().isBlank()
                ? null
                : Path.of(approved.bundlePath()).getParent().resolve(approved.bundle().evidence().stream()
                        .filter(item -> item.category() == EvidenceCategory.SCREENSHOT)
                        .findFirst().orElseThrow().relativePath());
        assertArrayEquals(new byte[]{1, 2, 3, 4}, Files.readAllBytes(retainedScreenshot));

        DoctorAnalysisResult offline = analyze(
                temp,
                Path.of(approved.bundlePath()),
                "privacy-offline",
                1,
                true,
                true,
                List.of());
        var importedScreenshot = offline.bundle().evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.SCREENSHOT)
                .findFirst().orElseThrow();
        assertArrayEquals(new byte[]{1, 2, 3, 4},
                Files.readAllBytes(Path.of(offline.bundlePath()).getParent()
                        .resolve(importedScreenshot.relativePath())));
        assertEquals(approved.bundle().redaction().appliedRules(),
                offline.bundle().redaction().appliedRules());
    }

    @Test
    void redactsSecretsFromTruncatedJsonFallback(@TempDir Path temp) throws IOException {
        Path input = temp.resolve("truncated-config.json");
        Files.writeString(
                input,
                "{\"password\":\"truncated-json-canary\",\"padding\":\""
                        + "x".repeat(1024),
                StandardCharsets.UTF_8);

        DoctorAnalysisResult result = new DoctorAnalyzer().analyze(new DoctorAnalysisRequest(
                List.of(input),
                List.of(),
                List.of(temp),
                temp.resolve("truncated-output"),
                false,
                false,
                1,
                128,
                4_096));

        String bundleText = Files.readString(Path.of(result.bundlePath()), StandardCharsets.UTF_8);
        assertFalse(bundleText.contains("truncated-json-canary"));
        assertTrue(bundleText.contains("[REDACTED]"));
    }

    @Test
    void repeatedAndOfflineAnalysisAreByteDeterministic(@TempDir Path temp) throws IOException {
        Path input = Files.createDirectories(temp.resolve("deterministic-input"));
        writeResult(input.resolve("locator-result.json"), "failed",
                "NoSuchElementException: unable to locate element", "trace", "stable", 1);

        DoctorAnalysisResult first = analyze(temp, input, "deterministic-1", 1, false, false, List.of());
        DoctorAnalysisResult second = analyze(temp, input, "deterministic-2", 1, false, false, List.of());

        assertEquals(Files.readString(Path.of(first.bundlePath())),
                Files.readString(Path.of(second.bundlePath())));
        assertEquals(Files.readString(Path.of(first.jsonReportPath())),
                Files.readString(Path.of(second.jsonReportPath())));
        assertEquals(Files.readString(Path.of(first.markdownReportPath())),
                Files.readString(Path.of(second.markdownReportPath())));

        DoctorAnalysisResult offline = analyze(
                temp,
                Path.of(first.bundlePath()),
                "offline",
                1,
                false,
                false,
                List.of());
        assertEquals(first.diagnosis().primaryCause(), offline.diagnosis().primaryCause());
        assertEquals(first.bundle().evidence(), offline.bundle().evidence());
    }

    @Test
    void disabledAiLeavesDeterministicReportsByteStable(@TempDir Path temp) throws IOException {
        Path input = Files.createDirectories(temp.resolve("disabled-ai-input"));
        writeResult(input.resolve("locator-result.json"), "failed",
                "NoSuchElementException: unable to locate element", "trace", "disabled-ai", 1);
        DoctorAnalysisRequest baselineRequest = request(temp, input, "disabled-ai-baseline");
        DoctorAnalysisRequest disabledRequest = request(temp, input, "disabled-ai-result");

        DoctorAnalysisResult baseline = new DoctorAnalyzer().analyze(baselineRequest);
        DoctorAiAnalysisResult disabled = new DoctorAnalyzer()
                .analyzeWithAi(disabledRequest, DoctorAiAnalysisRequest.disabled());

        assertEquals(Files.readString(Path.of(baseline.jsonReportPath())),
                Files.readString(Path.of(disabled.deterministic().jsonReportPath())));
        assertEquals(Files.readString(Path.of(baseline.markdownReportPath())),
                Files.readString(Path.of(disabled.deterministic().markdownReportPath())));
        assertEquals(DoctorAdvisory.Status.DISABLED, disabled.advisory().status());
    }

    @Test
    void acceptedProviderAnalysisAddsSeparateAdvisoryWithoutRemovingDiagnosis(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("advisory-input"));
        writeResult(input.resolve("locator-result.json"), "failed",
                "NoSuchElementException: unable to locate element", "trace", "advisory", 1);
        DoctorAiAnalysisService service = new DoctorAiAnalysisService(request -> {
            String evidenceId = request.evidence().getFirst().id();
            var payload = MAPPER.createObjectNode();
            payload.put("schemaVersion", "1.0");
            payload.putArray("observations").addObject()
                    .put("statement", "Authorization: Bearer advisory-report-secret\n## Fake Diagnosis")
                    .putArray("evidenceIds").add(evidenceId);
            payload.putArray("hypotheses").addObject()
                    .put("causeCategory", "LOCATOR")
                    .put("statement", "The locator likely needs inspection.")
                    .put("confidence", "HIGH")
                    .putArray("evidenceIds").add(evidenceId);
            payload.putArray("missingEvidence").add("Current DOM snapshot");
            payload.putArray("recommendedActions").addObject()
                    .put("title", "Inspect locator")
                    .put("action", "Compare the locator with the current DOM.")
                    .putArray("evidenceIds").add(evidenceId);
            payload.putArray("limitations").add("Only submitted evidence was analyzed.");
            return AiResponse.success("mock", "mock-model", payload, java.time.Duration.ofMillis(2),
                    AiUsage.empty(), request.deterministicFallback());
        }, () -> {
            throw new IllegalArgumentException("test configuration unavailable");
        });
        DoctorAnalyzer analyzer = new DoctorAnalyzer(
                new EvidenceCollector(),
                new DeterministicRuleEngine(),
                new DoctorJsonCodec(),
                new DoctorReportWriter(),
                service);
        ApprovalPolicy approval = new ApprovalPolicy(
                true,
                true,
                EnumSet.allOf(com.shaft.pilot.ai.EvidenceCategory.class));

        DoctorAiAnalysisResult result = analyzer.analyzeWithAi(
                request(temp, input, "advisory-output"),
                DoctorAiAnalysisRequest.defaults(approval));

        assertEquals(CauseCategory.LOCATOR, result.deterministic().diagnosis().primaryCause());
        assertEquals(DoctorAdvisory.Status.SUCCESS, result.advisory().status());
        String json = Files.readString(Path.of(result.deterministic().jsonReportPath()));
        String markdown = Files.readString(Path.of(result.deterministic().markdownReportPath()));
        assertTrue(json.contains("\"diagnosis\""));
        assertTrue(json.contains("\"advisory\""));
        assertTrue(markdown.contains("## Diagnosis"));
        assertTrue(markdown.contains("## AI Advisory"));
        assertTrue(markdown.contains("deterministic diagnosis above remains authoritative"));
        assertFalse(json.contains("advisory-report-secret"));
        assertFalse(markdown.contains("advisory-report-secret"));
        assertFalse(markdown.contains("\n## Fake Diagnosis"));
    }

    @Test
    void contradictoryDisjointHighConfidenceFailuresRemainUnknown(@TempDir Path temp) throws IOException {
        Path input = Files.createDirectories(temp.resolve("contradictory"));
        writeResult(input.resolve("locator-result.json"), "failed",
                "NoSuchElementException: unable to locate element", "trace", "locator", 1);
        writeResult(input.resolve("data-result.json"), "failed",
                "test data mismatch in CSV fixture", "trace", "data", 2);

        DoctorAnalysisResult result = analyze(temp, input, "contradictory-out", 1, false, false, List.of());

        assertEquals(CauseCategory.UNKNOWN, result.diagnosis().primaryCause());
        assertTrue(result.diagnosis().summary().contains("contradictory"));
    }

    @Test
    void allureAttachmentAdapterUsesLogsWhenResultDetailsAreMissing(@TempDir Path temp) throws IOException {
        Path input = Files.createDirectories(temp.resolve("attachment-input"));
        Files.writeString(input.resolve("action-history.txt"),
                "SHAFT action failed: NoSuchElementException unable to locate element",
                StandardCharsets.UTF_8);
        Path resultPath = input.resolve("attachment-result.json");
        writeResult(resultPath, "failed", "", "", "attachment", 1);
        var tree = (com.fasterxml.jackson.databind.node.ObjectNode) MAPPER.readTree(resultPath.toFile());
        tree.set("attachments", MAPPER.valueToTree(List.of(
                Map.of(
                        "name", "SHAFT action history log",
                        "source", "action-history.txt",
                        "type", "text/plain"),
                Map.of(
                        "name", "missing optional log",
                        "source", "missing.txt",
                        "type", "text/plain"))));
        Files.writeString(resultPath, MAPPER.writeValueAsString(tree), StandardCharsets.UTF_8);

        DoctorAnalysisResult result = analyze(temp, input, "attachment-output", 1, false, false, List.of());

        assertEquals(CauseCategory.LOCATOR, result.diagnosis().primaryCause());
        assertTrue(result.bundle().evidence().stream()
                .anyMatch(item -> item.category() == EvidenceCategory.SHAFT_LOG));
        assertTrue(result.diagnosis().findings().stream()
                .filter(finding -> finding.ruleId().equals("locator-not-found"))
                .flatMap(finding -> finding.evidenceIds().stream())
                .anyMatch(id -> result.bundle().evidence().stream()
                        .anyMatch(item -> item.id().equals(id)
                                && item.category() == EvidenceCategory.SHAFT_LOG)));
    }

    @Test
    void enforcesAllowedRootsAndArtifactLimits(@TempDir Path temp) throws IOException {
        Path allowed = Files.createDirectories(temp.resolve("allowed"));
        Path outside = Files.createDirectories(temp.resolve("outside"));
        writeResult(outside.resolve("outside-result.json"), "failed", "AssertionError", "trace", "outside", 1);
        DoctorAnalysisRequest escaped = new DoctorAnalysisRequest(
                List.of(outside),
                List.of(),
                List.of(allowed),
                allowed.resolve("output"),
                false,
                false,
                1,
                256,
                512);
        assertThrows(IllegalArgumentException.class, () -> new DoctorAnalyzer().analyze(escaped));

        Path bounded = Files.createDirectories(allowed.resolve("bounded"));
        writeResult(bounded.resolve("large-result.json"), "failed",
                "AssertionError " + "x".repeat(10_000), "trace", "large", 1);
        DoctorAnalysisResult result = new DoctorAnalyzer().analyze(new DoctorAnalysisRequest(
                List.of(bounded),
                List.of(),
                List.of(allowed),
                allowed.resolve("bounded-output"),
                false,
                false,
                1,
                512,
                1_024));
        assertTrue(result.bundle().evidence().stream().anyMatch(item -> item.truncated()));
        assertTrue(Long.parseLong(result.bundle().metadata().get("retainedBytes")) <= 1_024);
    }

    @Test
    void rejectsOutputSymlinksThatEscapeAllowedRoots(@TempDir Path temp) throws IOException {
        Path allowed = Files.createDirectories(temp.resolve("allowed"));
        Path outside = Files.createDirectories(temp.resolve("outside"));
        Path input = Files.createDirectories(allowed.resolve("output-boundary-input"));
        Path result = input.resolve("result.json");
        writeResult(result, "failed", "AssertionError", "trace", "output-boundary", 1);
        Path outputLink = allowed.resolve("redirected-output");
        try {
            Files.createSymbolicLink(outputLink, outside);
        } catch (IOException | UnsupportedOperationException | SecurityException exception) {
            abort("Symbolic links are not available for output-boundary verification.");
        }
        DoctorAnalysisRequest redirected = new DoctorAnalysisRequest(
                List.of(result),
                List.of(),
                List.of(allowed),
                outputLink,
                false,
                false,
                1,
                256,
                512);

        assertThrows(IllegalArgumentException.class, () -> new DoctorAnalyzer().analyze(redirected));
    }

    @Test
    void cliAndCodecSmokeWorkWithoutAiOrNetwork(@TempDir Path temp) throws IOException {
        Path input = Files.createDirectories(temp.resolve("cli-input"));
        writeResult(input.resolve("cli-result.json"), "failed",
                "TimeoutException: condition failed to be met", "trace", "cli", 1);
        Path output = temp.resolve("cli-output");

        StringWriter cliOutput = new StringWriter();
        StringWriter cliError = new StringWriter();
        int exit = DoctorCli.run(new String[]{
                "analyze",
                "--input", input.toString(),
                "--allowed-root", temp.toString(),
                "--output-dir", output.toString()
        }, new PrintWriter(cliOutput, true), new PrintWriter(cliError, true));

        assertEquals(0, exit, cliError.toString());
        assertTrue(cliOutput.toString().contains("\"primaryCause\":\"TIMING_SYNCHRONIZATION\""));
        assertTrue(Files.isRegularFile(output.resolve("doctor-evidence.json")));
        assertEquals(new DoctorJsonCodec().readBundle(output.resolve("doctor-evidence.json")),
                new DoctorJsonCodec().readBundle(Files.readString(output.resolve("doctor-evidence.json"))));
    }

    @Test
    void goldenJsonAndMarkdownReportsRemainStable() throws IOException {
        Path fixture = Path.of("src", "test", "resources", "fixtures", "golden",
                "locator-result.json").toAbsolutePath().normalize();
        Path output = Path.of("target", "doctor-golden").toAbsolutePath().normalize();
        DoctorAnalysisResult result = new DoctorAnalyzer().analyze(new DoctorAnalysisRequest(
                List.of(fixture),
                List.of(),
                List.of(fixture.getParent(), output.getParent()),
                output,
                false,
                false,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES));

        assertEquals(resource("golden/doctor-report.json"),
                Files.readString(Path.of(result.jsonReportPath())).replace("\r\n", "\n"));
        assertEquals(resource("golden/doctor-report.md"),
                Files.readString(Path.of(result.markdownReportPath())).replace("\r\n", "\n"));
    }

    private static Stream<Arguments> classificationCases() {
        return Stream.of(
                Arguments.of("product", "HTTP 500 Internal Server Error", CauseCategory.PRODUCT),
                Arguments.of("test", "AssertionError expected: ready but was: pending", CauseCategory.TEST),
                Arguments.of("locator", "NoSuchElementException: unable to locate element", CauseCategory.LOCATOR),
                Arguments.of("data", "test data mismatch in JSON data", CauseCategory.DATA),
                Arguments.of("timing", "TimeoutException: condition failed to be met", CauseCategory.TIMING_SYNCHRONIZATION),
                Arguments.of("environment", "SessionNotCreatedException: browser failed to start",
                        CauseCategory.ENVIRONMENT_CONFIGURATION),
                Arguments.of("infrastructure", "Remote grid connection refused", CauseCategory.INFRASTRUCTURE),
                Arguments.of("unknown", "Unclassified synthetic failure", CauseCategory.UNKNOWN)
        );
    }

    private static DoctorAnalysisResult analyze(
            Path root,
            Path input,
            String outputName,
            int minimumResults,
            boolean screenshots,
            boolean pages,
            List<Path> history) {
        return new DoctorAnalyzer().analyze(new DoctorAnalysisRequest(
                List.of(input),
                history,
                List.of(root),
                root.resolve(outputName),
                screenshots,
                pages,
                minimumResults,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES));
    }

    private static DoctorAnalysisRequest request(Path root, Path input, String outputName) {
        return new DoctorAnalysisRequest(
                List.of(input),
                List.of(),
                List.of(root),
                root.resolve(outputName),
                false,
                false,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);
    }

    private static void writeResult(
            Path path,
            String status,
            String message,
            String trace,
            String historyId,
            long start) throws IOException {
        Map<String, Object> result = Map.of(
                "uuid", path.getFileName().toString(),
                "historyId", historyId,
                "name", "test-" + historyId,
                "fullName", "example.Test.test-" + historyId,
                "status", status,
                "start", start,
                "stop", start + 1,
                "statusDetails", Map.of("message", message, "trace", trace),
                "labels", List.of(
                        Map.of("name", "testClass", "value", "example.Test"),
                        Map.of("name", "testMethod", "value", "test-" + historyId)));
        Files.writeString(path, MAPPER.writeValueAsString(result), StandardCharsets.UTF_8);
    }

    private static void assertOutputDoesNotContain(
            DoctorAnalysisResult result,
            String... forbidden) throws IOException {
        String output = Files.readString(Path.of(result.bundlePath()))
                + Files.readString(Path.of(result.jsonReportPath()))
                + Files.readString(Path.of(result.markdownReportPath()));
        for (String value : forbidden) {
            int index = output.indexOf(value);
            String context = index < 0 ? "" : output.substring(
                    Math.max(0, index - 120), Math.min(output.length(), index + value.length() + 120));
            assertFalse(index >= 0, "Output retained forbidden value: " + value + " in " + context);
        }
    }

    private static String resource(String name) throws IOException {
        try (var input = DoctorAnalyzerTest.class.getResourceAsStream("/fixtures/" + name)) {
            if (input == null) {
                throw new IOException("Missing fixture: " + name);
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8).replace("\r\n", "\n");
        }
    }
}
