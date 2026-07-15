package com.shaft.capture.generate.api;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.generate.CaptureEnrichmentService;
import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.GeneratedTestValidator;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.NetworkTiming;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.storage.NetworkBodyStore;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ApiCaptureGeneratorTest {

    private static final String CREATED_ID = "3fa85f64-5717-4562-b3fc-2c963f66afa6";

    @TempDir
    Path tempDir;

    @Test
    void generatesCompilesAndReportsSuccessForACreateThenReadRecording() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-api-1");

        ApiCaptureGenerator generator = new ApiCaptureGenerator();
        ApiCaptureGenerationResult result = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.SCHEMA, true, false, true));

        assertEquals(CaptureGenerationReport.Status.SUCCESS, result.report().status(),
                "Unsupported events: " + result.report().unsupportedEvents());
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED, result.report().compilation().status(),
                "Compile diagnostics: " + result.report().compilation().diagnostics());
        assertTrue(Files.exists(result.sourcePath()), "Generated source should exist on disk");
        assertTrue(Files.readString(result.sourcePath()).contains("String id = api.getResponseJSONValue"));
        assertTrue(Files.isDirectory(result.testDataDirectory()));
        assertTrue(Files.list(result.testDataDirectory()).findAny().isPresent(), "Schema artifacts should be written");
        assertTrue(Files.exists(result.reportPath()));
    }

    @Test
    void openApiSpecPathReportsCoverageAlongsideSuccessfulGeneration() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-coverage"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-api-coverage");
        Path specPath = outputRoot.resolve("openapi.json");
        Files.writeString(specPath, """
                {"paths": {"/orders": {"post": {}}, "/orders/{id}": {"get": {}}, "/health": {"get": {}}}}
                """, StandardCharsets.UTF_8);

        ApiCaptureGenerator generator = new ApiCaptureGenerator();
        ApiCaptureGenerationResult result = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS, true, false, true, specPath));

        assertEquals(CaptureGenerationReport.Status.SUCCESS, result.report().status());
        CaptureGenerationReport.OpenApiCoverage coverage = result.report().openApiCoverage();
        assertTrue(coverage.loadable(), "Coverage load failure: " + coverage.loadFailureReason());
        assertEquals(3, coverage.totalDeclaredOperations());
        assertTrue(coverage.coveredOperations().contains("POST /orders"));
        assertTrue(coverage.coveredOperations().contains("GET /orders/{param}"));
        assertTrue(coverage.missingOperations().contains("GET /health"));
    }

    @Test
    void noOpenApiSpecPathLeavesCoverageNotRequested() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-no-coverage"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-api-no-coverage");

        ApiCaptureGenerationResult result = new ApiCaptureGenerator().generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS, true, false, true));

        assertFalse(result.report().openApiCoverage().loadable());
        assertEquals(0, result.report().openApiCoverage().totalDeclaredOperations());
    }

    @Test
    void hybridStyleWithNoEnrichmentModeNeverCallsAiAndUsesTheDeterministicName() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-hybrid-none"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-hybrid-none");
        CaptureEnrichmentService neverCalled = new CaptureEnrichmentService(request -> {
            throw new AssertionError("AI must never be called when enrichmentMode is NONE");
        });

        ApiCaptureGenerator generator = new ApiCaptureGenerator(
                new CaptureJsonCodec(), new GeneratedTestValidator(), new NetworkBodyStore(), neverCalled);
        ApiCaptureGenerationResult result = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS, true, false, true, null,
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false, null));

        assertEquals(CaptureGenerationReport.Status.SUCCESS, result.report().status(),
                "Unsupported: " + result.report().unsupportedEvents());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.NOT_REQUESTED,
                result.report().enrichment().status());
        assertTrue(result.sourcePath().toString().contains("ApiCaptureSession_hybrid_noneTest"));
    }

    @Test
    void hybridStylePreviewModeWritesAReviewablePreviewWithoutChangingTheClassName() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-hybrid-preview"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-hybrid-preview");
        Path previewPath = outputRoot.resolve("target/shaft-capture/api-enrichment-preview.json");
        CaptureEnrichmentService fakeAi = fakeAiNaming("EnrichedApiScenarioTest", "checkoutThenReadOrder");

        ApiCaptureGenerator generator = new ApiCaptureGenerator(
                new CaptureJsonCodec(), new GeneratedTestValidator(), new NetworkBodyStore(), fakeAi);
        ApiCaptureGenerationResult result = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS, true, false, true, null,
                CaptureGenerationRequest.EnrichmentMode.PREVIEW, previewPath, false,
                new ApprovalPolicy(true, true, Set.of(EvidenceCategory.TEXT))));

        assertEquals(CaptureGenerationReport.Status.SUCCESS, result.report().status(),
                "Unsupported: " + result.report().unsupportedEvents());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.PREVIEWED,
                result.report().enrichment().status());
        assertTrue(Files.exists(previewPath), "Preview file should be written");
        assertTrue(Files.readString(previewPath).contains("EnrichedApiScenarioTest"));
        assertFalse(result.sourcePath().toString().contains("EnrichedApiScenarioTest"),
                "PREVIEW must never change the rendered class name");
    }

    @Test
    void hybridStyleApplyModeUsesTheApprovedPreviewedName() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-hybrid-apply"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-hybrid-apply");
        Path previewPath = outputRoot.resolve("target/shaft-capture/api-enrichment-preview.json");
        CaptureEnrichmentService fakeAi = fakeAiNaming("EnrichedApiScenarioTest", "checkoutThenReadOrder");
        ApiCaptureGenerator generator = new ApiCaptureGenerator(
                new CaptureJsonCodec(), new GeneratedTestValidator(), new NetworkBodyStore(), fakeAi);

        ApiCaptureGenerationResult previewResult = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS, true, false, true, null,
                CaptureGenerationRequest.EnrichmentMode.PREVIEW, previewPath, false,
                new ApprovalPolicy(true, true, Set.of(EvidenceCategory.TEXT))));
        assertEquals(CaptureGenerationReport.Status.SUCCESS, previewResult.report().status());

        ApiCaptureGenerationResult applied = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS, true, false, true, null,
                CaptureGenerationRequest.EnrichmentMode.APPLY, previewPath, true, ApprovalPolicy.denyAll()));

        assertEquals(CaptureGenerationReport.Status.SUCCESS, applied.report().status(),
                "Unsupported: " + applied.report().unsupportedEvents());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.APPLIED,
                applied.report().enrichment().status());
        assertTrue(applied.sourcePath().toString().contains("EnrichedApiScenarioTest"),
                "APPLY should use the AI-proposed, approved class name");
        assertTrue(Files.readString(applied.sourcePath()).contains("public class EnrichedApiScenarioTest"));
    }

    @Test
    void hybridStyleApplyModeRejectsAStalePreviewFingerprint() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-hybrid-stale"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-hybrid-stale");
        Path previewPath = outputRoot.resolve("stale-preview.json");
        Files.writeString(previewPath, """
                {"schemaVersion":"1.0","deterministicFingerprint":"not-a-real-fingerprint",
                 "provider":"mock","proposal":{"className":"HijackedTest","methodName":"m",
                 "elementNames":{},"assertions":[]},"diff":[]}
                """, StandardCharsets.UTF_8);

        ApiCaptureGenerator generator = new ApiCaptureGenerator();
        ApiCaptureGenerationResult result = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS, true, false, true, null,
                CaptureGenerationRequest.EnrichmentMode.APPLY, previewPath, true, ApprovalPolicy.denyAll()));

        assertEquals(CaptureGenerationReport.Status.FAILED, result.report().status());
        assertTrue(result.report().unsupportedEvents().stream().anyMatch(m -> m.contains("does not match")));
    }

    @Test
    void nonHybridStyleIgnoresEnrichmentModeEvenIfRequested() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-scenario-enrichment"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-scenario-enrichment");
        CaptureEnrichmentService neverCalled = new CaptureEnrichmentService(request -> {
            throw new AssertionError("AI must never be called for non-hybrid styles");
        });

        ApiCaptureGenerator generator = new ApiCaptureGenerator(
                new CaptureJsonCodec(), new GeneratedTestValidator(), new NetworkBodyStore(), neverCalled);
        ApiCaptureGenerationResult result = generator.generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "",
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS, true, false, true, null,
                CaptureGenerationRequest.EnrichmentMode.PREVIEW,
                outputRoot.resolve("preview.json"), false,
                new ApprovalPolicy(true, true, Set.of(EvidenceCategory.TEXT))));

        assertEquals(CaptureGenerationReport.Status.SUCCESS, result.report().status());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.NOT_REQUESTED,
                result.report().enrichment().status());
    }

    @Test
    void listResponseLeavesReturnsOneEntryPerRenderableTransactionInRecordedOrder() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-leaves"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-leaves");

        List<ApiCaptureGenerator.TransactionLeaves> leaves =
                new ApiCaptureGenerator().listResponseLeaves(sessionPath, List.of());

        assertEquals(2, leaves.size());
        assertEquals("tx-1", leaves.get(0).transactionId());
        assertEquals("POST", leaves.get(0).method());
        assertEquals("tx-2", leaves.get(1).transactionId());
        assertTrue(leaves.get(1).leaves().stream().anyMatch(leaf -> "$.status".equals(leaf.jsonPath())
                && "active".equals(leaf.value())));
    }

    @Test
    void listResponseLeavesRedactsSensitiveValuesButKeepsTheirClassification() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-leaves-sensitive"));
        Path sessionPath = writeSessionWithResponseBody(
                outputRoot, "session-leaves-sensitive", "{\"token\":\"super-secret-value\"}");

        List<ApiCaptureGenerator.TransactionLeaves> leaves =
                new ApiCaptureGenerator().listResponseLeaves(sessionPath, List.of());

        ResponseLeaf tokenLeaf = leaves.get(0).leaves().stream()
                .filter(leaf -> "$.token".equals(leaf.jsonPath()))
                .findFirst().orElseThrow();
        assertEquals(LeafClassification.SENSITIVE, tokenLeaf.classification());
        assertEquals("", tokenLeaf.value(), "A sensitive leaf's value must never leave listResponseLeaves");
    }

    @Test
    void listResponseLeavesHonorsExcludedTransactionIds() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-leaves-excluded"));
        Path sessionPath = writeRecordedSession(outputRoot, "session-leaves-excluded");

        List<ApiCaptureGenerator.TransactionLeaves> leaves =
                new ApiCaptureGenerator().listResponseLeaves(sessionPath, List.of("tx-1"));

        assertEquals(1, leaves.size());
        assertEquals("tx-2", leaves.get(0).transactionId());
    }

    @Test
    void listResponseLeavesReturnsEmptyForAnUnreadableSessionRatherThanThrowing() {
        List<ApiCaptureGenerator.TransactionLeaves> leaves = new ApiCaptureGenerator()
                .listResponseLeaves(tempDir.resolve("does-not-exist.json"), List.of());

        assertEquals(List.of(), leaves);
    }

    private Path writeSessionWithResponseBody(Path outputRoot, String sessionId, String responseBodyJson)
            throws Exception {
        Path sessionPath = outputRoot.resolve("recordings/" + sessionId + ".json");
        Files.createDirectories(sessionPath.getParent());
        Path bodiesDirectory = sessionPath.getParent().resolve(sessionId + "-network-bodies");
        Files.createDirectories(bodiesDirectory);
        NetworkBodyStore bodyStore = new NetworkBodyStore();
        BodyRef responseRef = bodyStore.store(
                responseBodyJson.getBytes(StandardCharsets.UTF_8), "application/json", bodiesDirectory);

        CaptureEvent.NetworkEvent event = new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(1),
                "tx-1",
                ResourceKind.FETCH,
                new HttpRequestRecord("GET", "https://api.example.test/session", headers(), null),
                new HttpResponseRecord(200, headers(), responseRef),
                new NetworkTiming(null, null, null, null, null, null),
                "",
                "https://app.example.test/",
                null);

        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                sessionId,
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(event),
                List.of(),
                List.of(),
                null,
                Map.of());
        new CaptureJsonCodec().write(sessionPath, session);
        return sessionPath;
    }

    private static CaptureEnrichmentService fakeAiNaming(String className, String methodName) {
        ObjectNode payload = new JsonMapper().createObjectNode();
        payload.put("className", className);
        payload.put("methodName", methodName);
        payload.putObject("elementNames");
        payload.putArray("assertions");
        return new CaptureEnrichmentService(request ->
                AiResponse.success("mock", "mock-model", payload, Duration.ZERO,
                        AiUsage.empty(), request.deterministicFallback()));
    }

    @Test
    void sessionWithNoNetworkEventsFailsWithAClearUnsupportedReason() throws Exception {
        Path outputRoot = Files.createDirectories(tempDir.resolve("project-empty"));
        CaptureSession session = CaptureFixtures.representativeSession();
        Path sessionPath = outputRoot.resolve("session.json");
        new CaptureJsonCodec().write(sessionPath, session);

        ApiCaptureGenerationResult result = new ApiCaptureGenerator().generate(new ApiCaptureGenerationRequest(
                sessionPath, outputRoot, "tests.generated", "NoNetworkTest",
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS, false, false, true));

        assertEquals(CaptureGenerationReport.Status.FAILED, result.report().status());
        assertFalse(result.report().unsupportedEvents().isEmpty());
    }

    @Test
    void missingSessionFileFailsCleanlyWithoutThrowing() {
        Path outputRoot = tempDir.resolve("project-missing");
        ApiCaptureGenerationResult result = new ApiCaptureGenerator().generate(new ApiCaptureGenerationRequest(
                tempDir.resolve("does-not-exist.json"), outputRoot, "tests.generated", "MissingSessionTest",
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS, false, false, true));

        assertEquals(CaptureGenerationReport.Status.FAILED, result.report().status());
    }

    private Path writeRecordedSession(Path outputRoot, String sessionId) throws Exception {
        Path sessionPath = outputRoot.resolve("recordings/" + sessionId + ".json");
        Files.createDirectories(sessionPath.getParent());
        Path bodiesDirectory = sessionPath.getParent().resolve(sessionId + "-network-bodies");
        Files.createDirectories(bodiesDirectory);
        NetworkBodyStore bodyStore = new NetworkBodyStore();

        byte[] createResponseBody = ("{\"id\":\"" + CREATED_ID + "\",\"status\":\"created\"}").getBytes(StandardCharsets.UTF_8);
        BodyRef createResponseRef = bodyStore.store(createResponseBody, "application/json", bodiesDirectory);
        byte[] readResponseBody = "{\"status\":\"active\"}".getBytes(StandardCharsets.UTF_8);
        BodyRef readResponseRef = bodyStore.store(readResponseBody, "application/json", bodiesDirectory);

        CaptureEvent.NetworkEvent createOrder = new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(1),
                "tx-1",
                ResourceKind.FETCH,
                new HttpRequestRecord("POST", "https://api.example.test/orders", headers(), null),
                new HttpResponseRecord(201, headers(), createResponseRef),
                new NetworkTiming(null, null, null, null, null, null),
                "",
                "https://app.example.test/",
                null);
        CaptureEvent.NetworkEvent readOrder = new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(2),
                "tx-2",
                ResourceKind.XHR,
                new HttpRequestRecord("GET", "https://api.example.test/orders/" + CREATED_ID, headers(), null),
                new HttpResponseRecord(200, headers(), readResponseRef),
                new NetworkTiming(null, null, null, null, null, null),
                "",
                "https://app.example.test/",
                null);

        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                sessionId,
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(createOrder, readOrder),
                List.of(),
                List.of(),
                null,
                Map.of());
        new CaptureJsonCodec().write(sessionPath, session);
        return sessionPath;
    }

    private static Map<String, String> headers() {
        Map<String, String> headers = new TreeMap<>();
        headers.put("content-type", "application/json");
        return headers;
    }
}
