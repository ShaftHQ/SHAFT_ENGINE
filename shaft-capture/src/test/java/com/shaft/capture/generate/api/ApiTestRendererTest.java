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
    void recordedSessionCookieIsReplayedAsAnAuthBootstrapNeverDropped() throws Exception {
        // Capture tokenizes the Cookie header as a secret-ref, so it must replay from the
        // environment (auth bootstrap) rather than being silently dropped (#3530 A3).
        ApiTransaction authenticated = transaction("tx-1", "GET", "https://api.example.test/me",
                Map.of("Cookie", "secret-ref:COOKIE_ABC_DEF"), "", 200, Map.of(), "{\"name\":\"Ada\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Cookie", List.of(authenticated),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_Cookie");
        assertTrue(source.contains("Auth bootstrap"), source);
        assertTrue(source.contains(".addHeader(\"Cookie\", requiredEnvironment(\"COOKIE_ABC_DEF\"))"), source);
        // The raw secret-ref token must never appear literally.
        assertTrue(!source.contains("secret-ref:"), source);
        // The cookie is rendered once (per request), not also hoisted into the session setup.
        assertEquals(1, source.split("\\.addHeader\\(\"Cookie\"", -1).length - 1, source);
        String setup = source.substring(source.indexOf("setupApiSessions"), source.indexOf("recordedApiScenario"));
        assertTrue(!setup.contains("Cookie"), setup);
    }

    @Test
    void blankCookieHeaderCarriesNoAuthAndIsSkipped() throws Exception {
        ApiTransaction noCookie = transaction("tx-1", "GET", "https://api.example.test/public",
                Map.of("Cookie", ""), "", 200, Map.of(), "{\"ok\":true}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_BlankCookie", List.of(noCookie),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_BlankCookie");
        assertTrue(!source.contains(".addHeader(\"Cookie\""), source);
        assertTrue(!source.contains("Auth bootstrap"), source);
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
        // TRACE has no SHAFT.API builder, so it must be skipped rather than rendered as a broken call.
        ApiTransaction traceRequest = transaction("tx-1", "TRACE", "https://api.example.test/probe",
                Map.of(), "", 200, Map.of(), "");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Unsupported", List.of(traceRequest),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_Unsupported");
        assertEquals(List.of("tx-1"), rendered.skippedTransactionIds());
    }

    @Test
    void headAndOptionsMethodsRenderCompilingBuilderCallsNotSkips() throws Exception {
        ApiTransaction headRequest = transaction("tx-1", "HEAD", "https://api.example.test/probe",
                Map.of(), "", 200, Map.of(), "");
        ApiTransaction optionsRequest = transaction("tx-2", "OPTIONS", "https://api.example.test/orders",
                Map.of(), "", 204, Map.of("Allow", "GET,POST"), "");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_HeadOptions", List.of(headRequest, optionsRequest),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);

        assertCompiles(rendered.source(), "RecordedApiTest_HeadOptions");
        // Both verbs now map to real SHAFT.API builders, so nothing is skipped.
        assertTrue(rendered.skippedTransactionIds().isEmpty(), rendered.skippedTransactionIds().toString());
        assertTrue(rendered.source().contains(".head(\"/probe\")"), rendered.source());
        assertTrue(rendered.source().contains(".options(\"/orders\")"), rendered.source());
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

    @Test
    void businessDepthPinsStableFieldsByJsonPathAndSkipsVolatileValues() throws Exception {
        // status + currency are stable business fields; orderId is a volatile UUID.
        ApiTransaction order = transaction("tx-1", "GET", "https://api.example.test/orders/42",
                Map.of(), "", 200, Map.of(),
                "{\"status\":\"CONFIRMED\",\"orderId\":\"" + CREATED_ID + "\",\"currency\":\"USD\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Business", List.of(order),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.BUSINESS);

        assertCompiles(rendered.source(), "RecordedApiTest_Business");
        // Stable business fields are pinned by JSON path via getResponseJSONValue.
        assertTrue(rendered.source().contains(".object(api.getResponseJSONValue(\"$.status\"))"),
                "Expected a business assertion on $.status, got:\n" + rendered.source());
        assertTrue(rendered.source().contains(".isEqualTo(\"CONFIRMED\")"));
        assertTrue(rendered.source().contains(".object(api.getResponseJSONValue(\"$.currency\"))"));
        assertTrue(rendered.source().contains(".isEqualTo(\"USD\")"));
        // The volatile UUID must never be asserted as a business value.
        assertTrue(!rendered.source().contains(CREATED_ID),
                "The volatile orderId must not be pinned as a business assertion, got:\n" + rendered.source());
        // BUSINESS depth pins live values; it needs no golden/schema artifacts.
        assertTrue(rendered.testDataArtifacts().isEmpty(),
                "BUSINESS depth should not need test-data artifacts");
    }

    @Test
    void businessDepthErrorResponseRendersErrorShapeTemplatePinningCodeThenMessage() throws Exception {
        // A 4xx negative case: code + message are the stable error contract; traceId is volatile.
        ApiTransaction notFound = transaction("tx-1", "GET", "https://api.example.test/orders/42",
                Map.of(), "", 404, Map.of(),
                "{\"message\":\"Order not found\",\"code\":\"NOT_FOUND\",\"traceId\":\"" + CREATED_ID + "\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Error", List.of(notFound),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.BUSINESS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_Error");
        // Negative-case template is labelled and reports the HTTP status.
        assertTrue(source.contains("Negative-case error shape (HTTP 404)"), source);
        // Both the error code and message are pinned by JSON path.
        assertTrue(source.contains(".object(api.getResponseJSONValue(\"$.code\"))"), source);
        assertTrue(source.contains(".isEqualTo(\"NOT_FOUND\")"), source);
        assertTrue(source.contains(".object(api.getResponseJSONValue(\"$.message\"))"), source);
        assertTrue(source.contains(".isEqualTo(\"Order not found\")"), source);
        // Contract-relevant fields lead: the code assertion is rendered before the message one,
        // regardless of their order in the recorded body.
        assertTrue(source.indexOf("$.code") < source.indexOf("$.message"),
                "Error code must be pinned before the message, got:\n" + source);
        // The volatile traceId is never pinned as a business value.
        assertTrue(!source.contains(CREATED_ID),
                "The volatile traceId must not be pinned, got:\n" + source);
    }

    @Test
    void businessDepthErrorResponseWithOnlyVolatileFieldsFallsBackToStatusOnlyComment() throws Exception {
        // A 5xx whose body carries no stable field -- the status assertion must stand alone.
        ApiTransaction serverError = transaction("tx-1", "GET", "https://api.example.test/orders/42",
                Map.of(), "", 500, Map.of(), "{\"traceId\":\"" + CREATED_ID + "\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_ServerError", List.of(serverError),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.BUSINESS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_ServerError");
        assertTrue(source.contains("Negative-case error shape (HTTP 500)"), source);
        assertTrue(source.contains("status assertion above stands alone"), source);
        assertTrue(!source.contains(CREATED_ID),
                "The volatile traceId must not be pinned, got:\n" + source);
    }

    @Test
    void formUrlEncodedBodyRendersSetParametersFormNotRawBody() throws Exception {
        ApiTransaction login = transaction("tx-1", "POST", "https://api.example.test/login",
                Map.of("Content-Type", "application/x-www-form-urlencoded"),
                "username=john&password=secret%21", 200, Map.of(), "{\"status\":\"ok\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Form", List.of(login),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_Form");
        assertTrue(source.contains(".setContentType(\"application/x-www-form-urlencoded\")"), source);
        assertTrue(source.contains("com.shaft.api.RestActions.ParametersType.FORM"), source);
        assertTrue(source.contains("java.util.Map.entry(\"username\", \"john\")"), source);
        // The %21 escape is URL-decoded to '!' before being pinned as a parameter value.
        assertTrue(source.contains("java.util.Map.entry(\"password\", \"secret!\")"), source);
        // A form body must not be replayed as an opaque JSON-defaulted request body.
        assertTrue(!source.contains(".setRequestBody(\"username=john"), source);
    }

    @Test
    void formUrlEncodedBodyWithCorrelationKeepsInterpolatedRawBody() throws Exception {
        // A correlated value flows into the form body, so setParameters (which cannot interpolate)
        // must be skipped in favour of the interpolated raw body -- with the content type preserved.
        ApiTransaction createOrder = transaction("tx-1", "POST", "https://api.example.test/orders",
                Map.of(), "{\"item\":\"widget\"}", 201, Map.of(), "{\"id\":\"" + CREATED_ID + "\"}");
        ApiTransaction confirm = transaction("tx-2", "POST", "https://api.example.test/orders/confirm",
                Map.of("Content-Type", "application/x-www-form-urlencoded"),
                "orderId=" + CREATED_ID + "&note=ok", 200, Map.of(), "{\"status\":\"confirmed\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_FormCorrelated", List.of(createOrder, confirm),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_FormCorrelated");
        assertTrue(source.contains(".setContentType(\"application/x-www-form-urlencoded\")"), source);
        assertTrue(source.contains(".setRequestBody("), source);
        // The correlated id is chained through a variable, never emitted as a literal or a form param.
        assertTrue(!source.contains("ParametersType.FORM"), source);
        assertTrue(!source.contains(CREATED_ID), source);
    }

    @Test
    void multipartBodyPreservesContentTypeAndFlagsFileParts() throws Exception {
        ApiTransaction upload = transaction("tx-1", "POST", "https://api.example.test/upload",
                Map.of("Content-Type", "multipart/form-data; boundary=X"),
                "--X\r\nContent-Disposition: form-data; name=\"f\"\r\n\r\nhi\r\n--X--", 201, Map.of(), "{\"id\":\"1\"}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Multipart", List.of(upload),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_Multipart");
        assertTrue(source.contains("Multipart body captured as raw text"), source);
        assertTrue(source.contains(".setContentType(\"multipart/form-data; boundary=X\")"), source);
        assertTrue(source.contains(".setRequestBody("), source);
    }

    @Test
    void graphQlJsonBodyIsFlaggedAndTheWorkingBodyPreserved() throws Exception {
        ApiTransaction query = transaction("tx-1", "POST", "https://api.example.test/graphql",
                Map.of("Content-Type", "application/json"),
                "{\"query\":\"{ user { id } }\"}", 200, Map.of(), "{\"data\":{\"user\":{\"id\":\"1\"}}}");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_GraphQl", List.of(query),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_GraphQl");
        assertTrue(source.contains("sendGraphQlRequest"), source);
        assertTrue(source.contains(".setRequestBody("), source);
        // JSON is SHAFT's default, so the content type is not re-stated for a GraphQL JSON body.
        assertTrue(!source.contains(".setContentType(\"application/json\")"), source);
    }

    @Test
    void nonJsonBodyPreservesTheRecordedContentType() throws Exception {
        ApiTransaction xml = transaction("tx-1", "POST", "https://api.example.test/soap",
                Map.of("Content-Type", "application/xml"),
                "<user><id>1</id></user>", 200, Map.of(), "<ok/>");

        RenderedApiTest rendered = ApiTestRenderer.render(
                "tests.generated", "RecordedApiTest_Xml", List.of(xml),
                ApiCodegenStyle.SCENARIO, ApiValidationDepth.STATUS);
        String source = rendered.source();

        assertCompiles(source, "RecordedApiTest_Xml");
        assertTrue(source.contains(".setContentType(\"application/xml\")"), source);
        assertTrue(source.contains(".setRequestBody("), source);
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
