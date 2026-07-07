package com.shaft.capture.generate.api;

import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.generate.GeneratedTestValidator;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ApiTestRendererTest {

    private static final String CREATED_ID = "3fa85f64-5717-4562-b3fc-2c963f66afa6";

    @TempDir
    Path tempDir;

    @Test
    void statusDepthRendersACompilingClassWithStatusCodeAssertionsOnly() throws Exception {
        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Status", createOrderThenReadScenario(),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_Status");
        assertTrue(rendered.testDataArtifacts().isEmpty(), "STATUS depth should not need test-data artifacts");
    }

    @Test
    void schemaDepthRendersACompilingClassAndInfersASchemaArtifact() throws Exception {
        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Schema", createOrderThenReadScenario(),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.SCHEMA);

        assertCompiles(rendered.source(), "RecordedApiTest_Schema");
        assertTrue(rendered.source().contains(".matchesSchema("));
        assertEquals(2, rendered.testDataArtifacts().size());
        rendered.testDataArtifacts().values().forEach(schema -> assertTrue(schema.contains("\"type\"")));
    }

    @Test
    void fullBodyDepthRendersACompilingClassWithNormalizedGoldenFile() throws Exception {
        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_FullBody", createOrderThenReadScenario(),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.FULL_BODY);

        assertCompiles(rendered.source(), "RecordedApiTest_FullBody");
        assertTrue(rendered.source().contains(".isEqualToFileContentIgnoringOrder("));
        String createGolden = rendered.testDataArtifacts().entrySet().stream()
                .filter(e -> e.getKey().contains("tx_1"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("No tx_1 artifact in " + rendered.testDataArtifacts().keySet()))
                .getValue();
        assertTrue(createGolden.contains("[NORMALIZED]"), "The correlated id must be normalized in the golden file: " + createGolden);
        assertTrue(!createGolden.contains(CREATED_ID), "The raw volatile id must never appear in the golden file");
    }

    @Test
    void correlatedValueIsChainedThroughAVariableNotALiteral() throws Exception {
        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Correlated", createOrderThenReadScenario(),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_Correlated");
        assertTrue(rendered.source().contains("String id = api.getResponseJSONValue(\"$.id\");"),
                "Expected a deterministic extraction statement, got:\n" + rendered.source());
        assertTrue(!rendered.source().contains(CREATED_ID),
                "The raw correlated literal must never appear in source once chained through a variable");
    }

    @Test
    void unmatchedVolatileValueFallsBackToRecordedLiteral() throws Exception {
        ApiTransaction onlyCreate = transaction("tx-1", "POST", "https://api.example.test/orders",
                Map.of(), "", 201, Map.of(), "{\"id\":\"" + CREATED_ID + "\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Unmatched", List.of(onlyCreate),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_Unmatched");
        assertTrue(!rendered.source().contains("getResponseJSONValue"),
                "An id with no later reuse must not be extracted into a variable");
    }

    @Test
    void sensitiveRequestHeaderIsReplayedViaRequiredEnvironmentNeverAsALiteral() throws Exception {
        ApiTransaction authenticated = transaction("tx-1", "GET", "https://api.example.test/me",
                Map.of("Authorization", "secret-ref:CAPTURE_TX1_AUTHORIZATION"), "", 200, Map.of(), "{\"name\":\"Ada\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Secret", List.of(authenticated),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_Secret");
        assertTrue(rendered.source().contains("requiredEnvironment(\"CAPTURE_TX1_AUTHORIZATION\")"),
                "Expected requiredEnvironment call, got:\n" + rendered.source());
        assertTrue(!rendered.source().contains("secret-ref:"),
                "The raw secret-ref token must never appear literally in generated source");
    }

    @Test
    void commonHeaderAcrossAllTransactionsIsHoistedToBeforeClass() throws Exception {
        Map<String, String> sharedHeader = Map.of("X-Api-Version", "2026-07");
        ApiTransaction first = transaction("tx-1", "GET", "https://api.example.test/a", sharedHeader, "", 200, Map.of(), "{}");
        ApiTransaction second = transaction("tx-2", "GET", "https://api.example.test/b", sharedHeader, "", 200, Map.of(), "{}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Hoisted", List.of(first, second),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_Hoisted");
        String beforeClass = rendered.source().substring(
                rendered.source().indexOf("setupApiSessions"), rendered.source().indexOf("recordedApiScenario"));
        assertTrue(beforeClass.contains("addHeader(\"X-Api-Version\", \"2026-07\")"));
    }

    @Test
    void unsupportedHttpMethodIsSkippedNotRenderedAsABrokenCall() throws Exception {
        ApiTransaction headRequest = transaction("tx-1", "HEAD", "https://api.example.test/probe",
                Map.of(), "", 200, Map.of(), "");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Unsupported", List.of(headRequest),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_Unsupported");
        assertEquals(List.of("tx-1"), rendered.skippedTransactionIds());
    }

    @Test
    void perRequestStyleRendersOneIndependentTestMethodPerTransaction() throws Exception {
        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_PerRequest", createOrderThenReadScenario(),
                ApiCodegenStyle.PER_REQUEST, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_PerRequest");
        long testMethodCount = rendered.source().lines().filter(l -> l.trim().equals("@Test")).count();
        assertEquals(2, testMethodCount);
        // Independent methods never correlate -- the id is asserted/replayed as its literal.
        assertTrue(rendered.source().contains(CREATED_ID));
    }

    @Test
    void hybridStyleInterleavesUiActionsWithApiAssertionsAfterTheirCorrelatedAnchor() throws Exception {
        ApiTransaction createOrder = new ApiTransaction("tx-1", "POST", "https://api.example.test/orders",
                "https://api.example.test", Map.of(), "{\"item\":\"widget\"}", 201, Map.of(),
                "{\"id\":\"" + CREATED_ID + "\",\"status\":\"created\"}",
                ResponseNormalizer.classify("{\"id\":\"" + CREATED_ID + "\",\"status\":\"created\"}"), 3L);

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedHybridTest_Basic", List.of(createOrder),
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.SCHEMA,
                Map.of(3L, List.of("driver.element().click(SHAFT.GUI.Locator.id(\"submit\"));")));

        assertCompiles(rendered.source(), "RecordedHybridTest_Basic");
        assertTrue(rendered.source().contains("driver.element().click(SHAFT.GUI.Locator.id(\"submit\"));"),
                "Expected the UI action line to be rendered, got:\n" + rendered.source());
        int uiLineIndex = rendered.source().indexOf("driver.element().click");
        int assertionIndex = rendered.source().indexOf("driver.browser().interceptRequest()");
        assertTrue(uiLineIndex >= 0 && assertionIndex > uiLineIndex,
                "The API assertion must be rendered after its correlated UI anchor, got:\n" + rendered.source());
        assertTrue(rendered.source().contains(".assertResponse(v -> {"));
        assertTrue(rendered.source().contains(".matchesSchema("));
    }

    @Test
    void hybridStyleRendersDeterministicallyWithNoUiActionsSupplied() throws Exception {
        ApiTransaction createOrder = new ApiTransaction("tx-1", "POST", "https://api.example.test/orders",
                "https://api.example.test", Map.of(), "{\"item\":\"widget\"}", 201, Map.of(),
                "{\"id\":\"" + CREATED_ID + "\"}", ResponseNormalizer.classify("{\"id\":\"" + CREATED_ID + "\"}"), 3L);

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedHybridTest_NoUi", List.of(createOrder),
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedHybridTest_NoUi");
        assertTrue(rendered.source().contains(".assertResponse(v -> {"),
                "A hybrid render with no UI lines must still assert the API response, got:\n" + rendered.source());
    }

    @Test
    void hybridStyleAppendsUncorrelatedTransactionsAfterAnchoredOnes() throws Exception {
        ApiTransaction anchored = new ApiTransaction("tx-1", "GET", "https://api.example.test/a",
                "https://api.example.test", Map.of(), "", 200, Map.of(), "{}",
                ResponseNormalizer.classify("{}"), 1L);
        ApiTransaction uncorrelated = new ApiTransaction("tx-2", "GET", "https://api.example.test/b",
                "https://api.example.test", Map.of(), "", 200, Map.of(), "{}",
                ResponseNormalizer.classify("{}"), null);

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedHybridTest_Order", List.of(uncorrelated, anchored),
                ApiCodegenStyle.HYBRID_UI_API, ApiValidationDepth.STATUS,
                Map.of(1L, List.of("driver.element().click(SHAFT.GUI.Locator.id(\"go\"));")));

        assertCompiles(rendered.source(), "RecordedHybridTest_Order");
        int anchoredIndex = rendered.source().indexOf("/a");
        int uncorrelatedIndex = rendered.source().indexOf("/b");
        assertTrue(anchoredIndex >= 0 && uncorrelatedIndex > anchoredIndex,
                "The anchored transaction (/a) must render before the uncorrelated one (/b), got:\n" + rendered.source());
    }

    private void assertCompiles(String source, String className) throws Exception {
        Path moduleDir = Files.createDirectories(tempDir.resolve(className));
        Path sourceFile = moduleDir.resolve(className + ".java");
        Files.writeString(sourceFile, source, StandardCharsets.UTF_8);
        Path classesDir = moduleDir.resolve("classes");

        CaptureGenerationReport.Validation result = new GeneratedTestValidator().compile(sourceFile, classesDir);

        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED, result.status(),
                "Generated source failed to compile: " + result.diagnostics() + "\n\n" + source);
    }

    private static List<ApiTransaction> createOrderThenReadScenario() {
        ApiTransaction createOrder = transaction("tx-1", "POST", "https://api.example.test/orders",
                Map.of(), "{\"item\":\"widget\"}", 201, Map.of(), "{\"id\":\"" + CREATED_ID + "\",\"status\":\"created\"}");
        ApiTransaction readOrder = transaction("tx-2", "GET",
                "https://api.example.test/orders/" + CREATED_ID, Map.of(), "", 200, Map.of(), "{\"status\":\"active\"}");
        return List.of(createOrder, readOrder);
    }

    private static ApiTransaction transaction(
            String id, String method, String url, Map<String, String> requestHeaders, String requestBody,
            int statusCode, Map<String, String> responseHeaders, String responseBody) {
        return new ApiTransaction(id, method, url, "https://api.example.test", requestHeaders, requestBody,
                statusCode, responseHeaders, responseBody, ResponseNormalizer.classify(responseBody));
    }
}
