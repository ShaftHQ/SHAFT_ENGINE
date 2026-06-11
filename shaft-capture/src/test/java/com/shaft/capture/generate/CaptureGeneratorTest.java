package com.shaft.capture.generate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

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
        assertEquals(Files.readString(first.sourcePath()), Files.readString(second.sourcePath()));
        assertEquals(Files.readString(first.testDataPath()), Files.readString(second.testDataPath()));
        assertEquals(first.report(), second.report());

        String source = Files.readString(first.sourcePath());
        String data = Files.readString(first.testDataPath());
        String golden = Files.readString(Path.of(
                "src/test/resources/fixtures/golden-generated-session-1.java"));
        assertEquals(golden, source);
        assertTrue(source.contains("@AfterMethod(alwaysRun = true)"));
        assertTrue(source.contains("driver.quit();"));
        assertTrue(source.contains("SHAFT.GUI.Locator.inputField(\"Username\")"));
        assertTrue(source.contains("driver.assertThat().element(USERNAME_INPUT_LOCATOR).text()"));
        assertFalse(source.contains("alice"));
        assertTrue(data.contains("\"username\" : \"alice\""));
        assertFalse(data.toLowerCase().contains("password"));

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

    private void writeCaptureData(String username) throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        root.putObject("values").put("data.username", username);
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);
    }
}
