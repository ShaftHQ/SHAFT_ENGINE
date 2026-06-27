package com.shaft.capture.generate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureGeneratorTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @TempDir
    Path temp;

    @Test
    void representativeSessionGeneratesDeterministicCompilableSourceAndExternalData() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");

        CaptureGenerationResult first = new CaptureGenerator().generate(request(session, temp.resolve("first")));
        CaptureGenerationResult second = new CaptureGenerator().generate(request(session, temp.resolve("second")));

        assertTrue(first.successful(), first.report().unsupportedEvents().toString());
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                first.report().compilation().status());
        assertTrue(Files.isRegularFile(first.reviewPath()));
        assertTrue(Files.isRegularFile(first.reviewUiPath()));
        var review = JSON.readTree(first.reviewPath().toFile());
        assertEquals("1.0", review.path("schemaVersion").asText());
        assertEquals(first.report().sessionId(), review.path("sessionId").asText());
        assertTrue(review.path("readinessScore").asInt() >= 0);
        List<String> blockers = new java.util.ArrayList<>();
        review.path("blockers").forEach(blocker -> blockers.add(blocker.asText()));
        assertTrue(blockers.stream()
                .anyMatch(blocker -> blocker.contains("data.password")
                        && blocker.contains("environment variable")));
        assertTrue(blockers.stream()
                .anyMatch(blocker -> blocker.contains("upload.avatar")
                        && blocker.contains("fixture")));
        assertEquals(Files.readString(first.sourcePath()), Files.readString(second.sourcePath()));
        assertEquals(Files.readString(first.testDataPath()), Files.readString(second.testDataPath()));
        assertEquals(first.report(), second.report());

        String source = Files.readString(first.sourcePath());
        String data = Files.readString(first.testDataPath());
        String golden = Files.readString(Path.of(
                "src/test/resources/fixtures/golden-generated-session-1.java"));
        assertEquals(normalizeLineEndings(golden), normalizeLineEndings(source));
        assertTrue(source.contains("@AfterMethod(alwaysRun = true)"));
        assertTrue(source.contains("driver.quit();"));
        assertTrue(source.contains("SHAFT.GUI.Locator.inputField(\"Username\")"));
        assertTrue(source.contains("driver.assertThat().element(USERNAME_INPUT_LOCATOR).text()"));
        assertFalse(source.contains("alice"));
        assertTrue(data.contains("\"username\" : \"alice\""));
        assertFalse(data.toLowerCase().contains("password"));
        String workbench = Files.readString(first.reviewUiPath());
        assertTrue(workbench.contains("Build record command"));
        assertTrue(workbench.contains("Playwright Codegen Feature Map"));
        assertTrue(workbench.contains("capture checkpoint"));
        assertTrue(workbench.contains("--shaft-primary"));
        assertTrue(workbench.contains("status-chip"));

    }

    @Test
    void recorderOverlayResourceUsesShaftUiTheme() throws Exception {
        try (InputStream stream = CaptureGeneratorTest.class
                .getResourceAsStream("/browser/shaft-capture-recorder.js")) {
            assertTrue(stream != null, "Recorder resource should be available on the test classpath.");
            String recorder = new String(stream.readAllBytes(), StandardCharsets.UTF_8);

            assertTrue(recorder.contains("--shaft-primary"));
            assertTrue(recorder.contains("status-chip"));
            assertTrue(recorder.contains("shaft-capture-assert"));
            assertTrue(recorder.contains("shaft-capture-pick"));
            assertTrue(recorder.contains("locator_preference"));
            assertTrue(recorder.contains("shaft-capture-readiness"));
            assertTrue(recorder.contains("readinessState"));
            assertTrue(recorder.contains("viewBox=\"0 0 24 24\""));
            assertTrue(recorder.contains("aria-label=\"Toggle assertion mode\""));
            assertTrue(recorder.contains("aria-label=\"Toggle locator picker\""));
            assertTrue(recorder.contains("kind: \"verification\""));
            assertTrue(recorder.contains("overflow-x: hidden"));
        }
    }

    @Test
    void unsupportedStepFailsWithEventIdAndRemediation() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureEvent.ClickEvent click = (CaptureEvent.ClickEvent) base.events().get(1);
        List<CaptureEvent> events = new java.util.ArrayList<>(base.events());
        events.set(1, new CaptureEvent.ClickEvent(
                click.context(), click.target(), CaptureEvent.MouseButton.SECONDARY, 1));
        CaptureSession unsupported = new CaptureSession(
                base.schemaVersion(), base.sessionId(), base.status(), base.startedAt(), base.endedAt(),
                base.browser(), events, base.checkpoints(), base.dataReferences(),
                base.redactionSummary(), base.extensions());
        Path session = session(unsupported);
        writeCaptureData("alice");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("unsupported")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("event-2") && message.contains("primary-button")));
        assertFalse(Files.exists(result.sourcePath()));
    }

    @Test
    void unsupportedAssertionCheckpointKeepsRecorderDescriptionInReport() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession unsupported = new CaptureSession(
                base.schemaVersion(),
                "unsupported-assertion-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(base.events().getFirst()),
                List.of(new Checkpoint(
                        "unsupported-assertion",
                        1,
                        CaptureFixtures.STARTED.plusSeconds(1),
                        Checkpoint.CheckpointKind.ASSERTION,
                        "Unsupported assertion type: CSS matches")),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path session = session(unsupported);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("unsupported-assertion")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("Unsupported assertion type: CSS matches")));
    }

    @Test
    void playwrightBackendGeneratesCompilableShaftPlaywrightSource() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession simple = new CaptureSession(
                base.schemaVersion(),
                "playwright-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                base.startedAt().plusSeconds(5),
                base.browser(),
                base.events().subList(0, 4),
                List.of(),
                List.of(CaptureFixtures.ordinary()),
                base.redactionSummary(),
                base.extensions());
        Path session = session(simple);
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator().generate(
                request(session, temp.resolve("playwright")),
                CaptureGenerator.CodegenBackend.PLAYWRIGHT);

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                result.report().compilation().status());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("private SHAFT.GUI.Playwright driver;"));
        assertTrue(source.contains("driver = new SHAFT.GUI.Playwright();"));
        assertTrue(source.contains("driver.element().click(USERNAME_INPUT_LOCATOR);"));
        assertFalse(source.contains("DriverFactory"));
        assertFalse(source.contains("ExpectedConditions"));
    }

    @Test
    void fallbackLocatorReplayOptionGeneratesSharedHelperAndRankedCandidates() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("fallback-replay"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll(), true));

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("private static final By[] USERNAME_INPUT_LOCATOR_FALLBACKS"));
        assertTrue(source.contains("USERNAME_INPUT_LOCATOR,"));
        assertTrue(source.contains("By.cssSelector(\"form input:nth-child(1)\")"));
        assertTrue(source.contains("captureReplayLocator(\"username-input\", USERNAME_INPUT_LOCATOR"));
        assertTrue(source.contains("SHAFT.Report.log(\"Capture fallback locator used for \" + logicalElementId"));
        assertTrue(source.contains(", true, \"input\", \"Username\")"));
        assertTrue(source.contains("matchesCaptureTarget(candidate, expectedTagName, expectedAccessibleName"));
    }

    @Test
    void deterministicReviewFlagsGeneratedCodeRisks() throws Exception {
        ExternalTestDataReference card = new ExternalTestDataReference(
                "data.card",
                "card number",
                ExternalTestDataReference.DataSource.JSON,
                "capture-data.json",
                "/values/data.card",
                ExternalTestDataReference.DataClassification.SENSITIVE);
        ElementSnapshot cardInput = new ElementSnapshot(
                "card-input",
                "input",
                "textbox",
                "Card number",
                "Card number",
                Map.of("name", "card"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.XPATH,
                        "/html/body/div[3]/form/input[1]", 1, true, false,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.POSITIONAL))),
                true,
                true,
                false);
        ElementSnapshot payButton = new ElementSnapshot(
                "pay-button",
                "button",
                "button",
                "Pay now",
                "",
                Map.of("type", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.XPATH,
                        "/html/body/div[3]/form/button[2]", 1, true, false,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.POSITIONAL))),
                true,
                true,
                false);
        CaptureSession reviewSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "review-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://shop.example/checkout"),
                        new CaptureEvent.TypeEvent(CaptureFixtures.context(2), cardInput, card),
                        new CaptureEvent.WaitEvent(CaptureFixtures.context(3),
                                CaptureEvent.WaitCondition.FIXED_DURATION, Duration.ofSeconds(2), null, null),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(4), payButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(card),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(reviewSession);
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        root.putObject("values").put("data.card", "test-card-value");
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("review")));

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        var review = JSON.readTree(result.reviewPath().toFile());
        List<String> categories = new java.util.ArrayList<>();
        review.path("findings").forEach(finding -> categories.add(finding.path("category").asText()));
        assertTrue(categories.contains("LOCATOR"), review.toString());
        assertTrue(categories.contains("ASSERTION"), review.toString());
        assertTrue(categories.contains("WAIT"), review.toString());
        assertTrue(categories.contains("TEST_DATA"), review.toString());
        assertTrue(result.report().warnings().stream().anyMatch(warning -> warning.contains("review/LOCATOR")));
        assertEquals(CaptureReadiness.State.RISKY, result.report().readiness());
        assertTrue(result.report().readinessWarnings().stream()
                .anyMatch(warning -> warning.contains("positional XPATH")));
    }

    @Test
    void deterministicReviewMapsReplayFailureTraceAndNetworkDependency() throws Exception {
        Path session = session(new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "trace-review-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://shop.example/checkout"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of()));
        GeneratedTestValidator validator = new GeneratedTestValidator() {
            @Override
            public CaptureGenerationReport.Validation compile(Path source, Path classesDirectory) {
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                        List.of(),
                        0);
            }

            @Override
            public CaptureGenerationReport.Validation replay(
                    String fullyQualifiedClassName,
                    Path classesDirectory,
                    Path resourcesDirectory,
                    Path workDirectory,
                    Duration timeout) {
                Path trace = workDirectory.resolve("target/shaft-traces/trace-review/shaft-trace.json");
                try {
                    Files.createDirectories(trace.getParent());
                    Files.writeString(trace, """
                            {
                              "schemaVersion": "1.0",
                              "source": {
                                "file": "src/test/java/generated/capture/TraceReviewTest.java",
                                "line": "31",
                                "snippet": "driver.element().click(USERNAME_INPUT_LOCATOR);"
                              },
                              "actions": [
                                {"id": "action-2", "category": "element", "name": "CLICK", "status": "failed",
                                 "locator": "By.xpath: /html/body/div[3]/form/button[2]",
                                 "url": "https://shop.example/checkout", "message": "Click failed",
                                 "exception": {"type": "org.openqa.selenium.NoSuchElementException", "message": "missing"},
                                 "attachments": [], "metadata": {}}
                              ],
                              "network": [
                                {"url": "https://shop.example/api/pay", "status": 500, "method": "POST"}
                              ]
                            }
                            """, StandardCharsets.UTF_8);
                } catch (java.io.IOException exception) {
                    throw new IllegalStateException(exception);
                }
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        List.of("Replay produced 1 non-passing Allure result file(s)."),
                        1);
            }
        };

        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), validator, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        session, temp.resolve("trace-review"), "generated.capture", "TraceReviewTest", false,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertFalse(result.successful());
        var review = JSON.readTree(result.reviewPath().toFile());
        List<String> categories = new java.util.ArrayList<>();
        review.path("findings").forEach(finding -> categories.add(finding.path("category").asText()));
        assertTrue(categories.contains("REPLAY_TRACE"), review.toString());
        assertTrue(categories.contains("NETWORK_DEPENDENCY"), review.toString());
        assertTrue(result.report().warnings().stream()
                .anyMatch(warning -> warning.contains("trace action action-2")), result.report().warnings().toString());
        assertTrue(result.report().warnings().stream()
                .anyMatch(warning -> warning.contains("#3065")), result.report().warnings().toString());
    }

    @Test
    void secretCanaryInExternalDataIsRejectedWithoutLeakingIntoReport() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        String canary = "sk-secret-canary-123456789";
        writeCaptureData(canary);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("secret")));

        assertFalse(result.successful());
        String report = Files.readString(result.reportPath());
        assertFalse(report.contains(canary));
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.startsWith("privacy:")));
    }

    @Test
    void approvedEnrichmentIsAppliedThenRecompiled() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");
        ObjectNode payload = JSON.createObjectNode();
        payload.put("className", "EnrichedJourneyTest");
        payload.put("methodName", "completeCheckout");
        payload.putObject("elementNames").put("username-input", "USERNAME_FIELD");
        payload.putArray("assertions").addObject()
                .put("eventSequence", 2)
                .put("verification", "ELEMENT_VISIBLE")
                .put("negated", false);
        CaptureEnrichmentService enrichment = new CaptureEnrichmentService(request ->
                AiResponse.success("mock", "mock-model", payload, Duration.ZERO,
                        AiUsage.empty(), request.deterministicFallback()));
        CaptureGenerator generator = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), new GeneratedTestValidator(), enrichment);
        Path preview = temp.resolve("preview.json");

        CaptureGenerationResult previewResult = generator.generate(new CaptureGenerationRequest(
                session, temp.resolve("preview-output"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.PREVIEW, preview, false,
                new ApprovalPolicy(true, true, java.util.Set.of(com.shaft.pilot.ai.EvidenceCategory.TEXT))));
        assertTrue(previewResult.successful());
        assertTrue(Files.readString(preview).contains("EnrichedJourneyTest"));

        CaptureGenerationResult applied = generator.generate(new CaptureGenerationRequest(
                session, temp.resolve("applied-output"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.APPLY, preview, true,
                ApprovalPolicy.denyAll()));

        assertTrue(applied.successful(), applied.report().unsupportedEvents().toString());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.APPLIED,
                applied.report().enrichment().status());
        String source = Files.readString(applied.sourcePath());
        assertTrue(source.contains("public class EnrichedJourneyTest"));
        assertTrue(source.contains("public void completeCheckout()"));
        assertTrue(source.contains("private static final By USERNAME_FIELD"));
        assertTrue(source.contains("driver.assertThat().element(USERNAME_FIELD).isVisible().perform();"));
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                applied.report().compilation().status());
    }

    @Test
    void staleOrInvalidEnrichmentPreviewIsRejected() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");
        Path preview = temp.resolve("invalid-preview.json");
        Files.writeString(preview, """
                {
                  "schemaVersion": "1.0",
                  "deterministicFingerprint": "stale",
                  "provider": "mock",
                  "proposal": {
                    "className": "not-valid!",
                    "methodName": "",
                    "elementNames": {},
                    "assertions": []
                  },
                  "diff": []
                }
                """, StandardCharsets.UTF_8);

        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("invalid-output"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.APPLY, preview, true,
                ApprovalPolicy.denyAll()));

        assertFalse(result.successful());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.REJECTED,
                result.report().enrichment().status());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("does not match")));
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("not a Java identifier")));
    }

    @Test
    void invalidProviderProposalIsRejectedBeforePreviewPersistence() {
        ObjectNode payload = JSON.createObjectNode();
        payload.put("className", "not-valid!");
        payload.put("methodName", "");
        payload.putObject("elementNames");
        payload.putArray("assertions");
        CaptureEnrichmentService enrichment = new CaptureEnrichmentService(request ->
                AiResponse.success("mock", "mock-model", payload, Duration.ZERO,
                        AiUsage.empty(), request.deterministicFallback()));

        assertThrows(IllegalStateException.class, () -> enrichment.preview(
                CaptureFixtures.representativeSession(),
                "1234567890abcdef",
                "Session1Test",
                "replaySession1",
                Map.of("username-input", "USERNAME_INPUT_LOCATOR"),
                new ApprovalPolicy(true, true,
                        java.util.Set.of(com.shaft.pilot.ai.EvidenceCategory.TEXT))));
    }

    private CaptureGenerationRequest request(Path session, Path output) {
        return new CaptureGenerationRequest(
                session, output, "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll());
    }

    private Path session(CaptureSession captureSession) {
        Path path = temp.resolve("capture.json");
        new CaptureJsonCodec().write(path, captureSession);
        return path;
    }

    private static String normalizeLineEndings(String value) {
        return value.replace("\r\n", "\n").replace('\r', '\n');
    }

    private void writeCaptureData(String username) throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        root.putObject("values").put("data.username", username);
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);
    }
}
