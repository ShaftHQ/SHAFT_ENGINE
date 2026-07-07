package com.shaft.capture.generate.api;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.NetworkTiming;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.storage.NetworkBodyStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
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
