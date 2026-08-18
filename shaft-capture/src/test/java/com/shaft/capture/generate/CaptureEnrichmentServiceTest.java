package com.shaft.capture.generate;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.generate.api.ApiCaptureGenerationRequest;
import com.shaft.capture.generate.api.ApiCaptureGenerator;
import com.shaft.capture.generate.api.ApiCodegenStyle;
import com.shaft.capture.generate.api.ApiValidationDepth;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.NetworkTiming;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.storage.NetworkBodyStore;
import com.shaft.driver.SHAFT;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.EvidenceReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureEnrichmentServiceTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final String SECRET_CANARY = "sk-secret-canary-capture-4863";

    @TempDir
    Path temp;

    @AfterEach
    void cleanup() {
        com.shaft.properties.internal.Properties.clearForCurrentThread();
    }

    @Test
    void unavailableEnrichmentLeavesGeneratedSourceByteEquivalentToNoAiPath() throws Exception {
        Path session = writeSession(CaptureFixtures.representativeSession());
        writeCaptureData("alice");
        CaptureGenerationResult noAi = new CaptureGenerator().generate(
                generationRequest(session, temp.resolve("no-ai"), CaptureGenerationRequest.EnrichmentMode.NONE, null));
        assertEquals(CaptureGenerationReport.Status.UNCONFIRMED, noAi.report().status(),
                noAi.report().unsupportedEvents().toString());

        CaptureEnrichmentService unavailable = new CaptureEnrichmentService(request ->
                AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "none", "", "unavailable",
                        Duration.ZERO, request.deterministicFallback()));
        CaptureGenerator generator = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), new GeneratedTestValidator(), unavailable);
        Path preview = temp.resolve("unavailable-preview.json");
        CaptureGenerationResult failedAi = generator.generate(generationRequest(
                session, temp.resolve("failed-ai"), CaptureGenerationRequest.EnrichmentMode.PREVIEW, preview));

        assertTrue(Files.isRegularFile(failedAi.sourcePath()), failedAi.report().unsupportedEvents().toString());
        assertEquals(
                withStableHealingHistoryPath(Files.readString(noAi.sourcePath())),
                withStableHealingHistoryPath(Files.readString(failedAi.sourcePath())));
    }

    @Test
    void plantedSecretNeverEntersEnrichmentRequest() {
        SHAFT.Properties.healing.set().aiEnabled(false);
        SHAFT.Properties.pilot.set().enabled(false);
        ElementSnapshot leaky = new ElementSnapshot(
                "username-input",
                "input",
                "textbox",
                SECRET_CANARY,
                SECRET_CANARY,
                Map.of("autocomplete", "username", "name", "username", "authorization", "Bearer " + SECRET_CANARY),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "textbox:" + SECRET_CANARY, 1, true, true,
                        Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true)),
                true,
                true,
                false);
        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "session-secret",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(2),
                CaptureFixtures.browser(),
                List.of(new CaptureEvent.ClickEvent(CaptureFixtures.context(1), leaky,
                        CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                null,
                Map.of());
        AtomicReference<AiRequest> captured = new AtomicReference<>();
        CaptureEnrichmentService service = new CaptureEnrichmentService(request -> {
            captured.set(request);
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "none", "", "unavailable",
                    Duration.ZERO, request.deterministicFallback());
        });
        try {
            service.preview(
                    session,
                    "1234567890abcdef",
                    "Session1Test",
                    "replaySession1",
                    Map.of("username-input", "USERNAME_INPUT_LOCATOR"),
                    new ApprovalPolicy(true, true, Set.of(EvidenceCategory.TEXT)));
        } catch (RuntimeException ignored) {
            // Fail-closed preview may reject unavailable providers; the request must still be captured.
        }

        assertNotNull(captured.get(), "Preview must submit a request through AiExecutionService.");
        String blob = captured.get().text() + "\n" + captured.get().evidence().stream()
                .map(EvidenceReference::content)
                .reduce("", (left, right) -> left + "\n" + right);
        assertFalse(blob.contains(SECRET_CANARY), blob);
        assertFalse(blob.contains("Bearer "), blob);
        assertFalse(SHAFT.Properties.healing.aiEnabled());
        assertFalse(SHAFT.Properties.pilot.enabled());
    }

    @Test
    void apiUnavailableEnrichmentLeavesDeterministicClassNameAndOmitsSecrets() throws Exception {
        Path outputRoot = Files.createDirectories(temp.resolve("api-unavailable"));
        Path sessionPath = writeApiSessionWithSecret(outputRoot, "session-api-secret");
        Path noAiRoot = Files.createDirectories(temp.resolve("api-no-ai"));
        Path noAiSession = writeApiSessionWithSecret(noAiRoot, "session-api-secret");
        var noAi = new ApiCaptureGenerator().generate(new ApiCaptureGenerationRequest(
                noAiSession, noAiRoot, "tests.generated", "",
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS, true, false, true, null,
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false, null));

        AtomicReference<AiRequest> captured = new AtomicReference<>();
        CaptureEnrichmentService unavailable = new CaptureEnrichmentService(request -> {
            captured.set(request);
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "none", "", "unavailable",
                    Duration.ZERO, request.deterministicFallback());
        });
        Path preview = outputRoot.resolve("target/shaft-capture/api-enrichment-preview.json");
        var failedAi = new ApiCaptureGenerator(
                new CaptureJsonCodec(), new GeneratedTestValidator(), new NetworkBodyStore(), unavailable)
                .generate(new ApiCaptureGenerationRequest(
                        sessionPath, outputRoot, "tests.generated", "",
                        ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS, true, false, true, null,
                        CaptureGenerationRequest.EnrichmentMode.PREVIEW, preview, false,
                        new ApprovalPolicy(true, true, Set.of(EvidenceCategory.TEXT))));

        assertTrue(Files.isRegularFile(failedAi.sourcePath()), String.valueOf(failedAi.report().unsupportedEvents()));
        assertEquals(Files.readString(noAi.sourcePath()), Files.readString(failedAi.sourcePath()));
        assertNotNull(captured.get());
        String blob = captured.get().text() + "\n" + captured.get().evidence().stream()
                .map(EvidenceReference::content)
                .reduce("", (left, right) -> left + "\n" + right);
        assertFalse(blob.contains(SECRET_CANARY), blob);
        assertFalse(blob.contains("Authorization"), blob);
    }

    private CaptureGenerationRequest generationRequest(
            Path session,
            Path output,
            CaptureGenerationRequest.EnrichmentMode mode,
            Path preview) {
        return new CaptureGenerationRequest(
                session, output, "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                mode, preview, false,
                new ApprovalPolicy(true, true, Set.of(EvidenceCategory.TEXT)));
    }

    private Path writeSession(CaptureSession session) {
        Path path = temp.resolve("capture.json");
        new CaptureJsonCodec().write(path, session);
        return path;
    }

    private void writeCaptureData(String username) throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        root.putObject("values").put("data.username", username);
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);
    }

    private static String withStableHealingHistoryPath(String source) {
        return source.replaceAll(
                "(?m)^(\\s*\\.historyPath\\(\")[^\"]*(\"\\)\\r?)$",
                "$1PLACEHOLDER$2");
    }

    private Path writeApiSessionWithSecret(Path outputRoot, String sessionId) throws Exception {
        Path sessionPath = outputRoot.resolve("recordings/" + sessionId + ".json");
        Files.createDirectories(sessionPath.getParent());
        Path bodiesDirectory = sessionPath.getParent().resolve(sessionId + "-network-bodies");
        Files.createDirectories(bodiesDirectory);
        NetworkBodyStore bodyStore = new NetworkBodyStore();
        var responseRef = bodyStore.store(
                "{\"status\":\"ok\"}".getBytes(StandardCharsets.UTF_8), "application/json", bodiesDirectory);
        Map<String, String> headers = new TreeMap<>();
        headers.put("content-type", "application/json");
        headers.put("authorization", "Bearer " + SECRET_CANARY);
        CaptureEvent.NetworkEvent event = new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(1),
                "tx-1",
                ResourceKind.FETCH,
                new HttpRequestRecord(
                        "GET",
                        "https://user:" + SECRET_CANARY + "@api.example.test/orders?token=" + SECRET_CANARY,
                        headers,
                        null),
                new HttpResponseRecord(200, headers, responseRef),
                new NetworkTiming(null, null, null, null, null, null),
                "",
                "https://app.example.test/",
                null);
        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                sessionId,
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(2),
                CaptureFixtures.browser(),
                List.of(event),
                List.of(),
                List.of(),
                null,
                Map.of());
        new CaptureJsonCodec().write(sessionPath, session);
        return sessionPath;
    }
}
