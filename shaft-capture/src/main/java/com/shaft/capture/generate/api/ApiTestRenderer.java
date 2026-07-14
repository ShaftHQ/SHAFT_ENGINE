package com.shaft.capture.generate.api;

import com.shaft.tools.io.internal.HttpContractRecorder;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Renders a compiling {@code SHAFT.API} TestNG class from recorded, classified, and correlated
 * API transactions. Mirrors {@code com.shaft.capture.generate.CaptureGenerator}'s
 * validate-&gt;analyze-&gt;render staging, scoped to API transactions rather than UI events.
 *
 * <p>One {@code SHAFT.API} field is rendered per origin (scheme://host[:port]); headers common to
 * every transaction of an origin are hoisted into {@code @BeforeClass} as session-level headers,
 * and any sensitive header (a {@code secret-ref:<ENV_NAME>} token from
 * {@code com.shaft.capture.network.SecretHeaderReplacer}) is replayed via a generated
 * {@code requiredEnvironment(String)} helper rather than ever being written to source as a
 * literal.
 *
 * <p>Correlation (see {@link TransactionCorrelator}) only ever chains values within the same
 * rendered {@code @Test} method -- a variable extracted in one method is not visible from another,
 * so correlation is computed and applied per origin group (one {@code SCENARIO} method), never
 * across origins or across {@code PER_REQUEST} methods.
 */
public final class ApiTestRenderer {
    private static final ObjectMapper MAPPER = new JsonMapper();
    private static final String SECRET_REF_PREFIX = "secret-ref:";
    private static final String SENSITIVE_PLACEHOLDER = "[MASKED]";
    private static final String VOLATILE_PLACEHOLDER = "[NORMALIZED]";

    /**
     * JSON property names that typically carry a machine-readable error code in a 4xx/5xx body
     * (e.g. {@code {"code":"NOT_FOUND"}}, {@code {"error":"invalid_grant"}}).
     */
    private static final Set<String> ERROR_CODE_KEYS = Set.of(
            "code", "error", "errorcode", "error_code", "status", "type", "reason");

    /**
     * JSON property names that typically carry a human-readable error message in a 4xx/5xx body
     * (e.g. {@code {"message":"Order not found"}}, {@code {"error_description":"..."}}).
     */
    private static final Set<String> ERROR_MESSAGE_KEYS = Set.of(
            "message", "detail", "details", "description", "error_description", "error_message", "title");

    /**
     * Request headers that are mechanical/transport-level and never meaningfully replayed:
     * content negotiation and framing are already handled by {@code setContentType}/
     * {@code setRequestBody}, and connection/host/user-agent/cookie headers describe the
     * recording browser session, not the API contract.
     */
    private static final Set<String> SKIPPED_REQUEST_HEADERS = Set.of(
            "content-length", "content-type", "host", "connection", "accept-encoding",
            "user-agent", "cookie", "origin", "referer");

    private ApiTestRenderer() {
    }

    /**
     * Renders one API test class from a set of already-filtered, renderable transactions.
     *
     * @param packageName generated class package
     * @param className generated class name
     * @param transactions renderable transactions (already filtered to XHR/FETCH-like requests
     *                      with a real response) in recorded order
     * @param style how transactions are grouped into test methods
     * @param depth how thoroughly each response is validated
     * @return generated source, any test-data artifacts (schema/golden files) it references, and
     *         any transaction IDs skipped because SHAFT.API has no builder for their HTTP method
     */
    public static RenderedApiTest render(
            String packageName,
            String className,
            List<ApiTransaction> transactions,
            ApiCodegenStyle style,
            ApiValidationDepth depth) {
        return render(packageName, className, transactions, style, depth, Map.of());
    }

    /**
     * Renders one API test class, as {@link #render(String, String, List, ApiCodegenStyle,
     * ApiValidationDepth)}, additionally accepting pre-rendered UI action source lines for
     * {@link ApiCodegenStyle#HYBRID_UI_API}.
     *
     * @param packageName generated class package
     * @param className generated class name
     * @param transactions renderable transactions in recorded order
     * @param style how transactions are grouped into test methods
     * @param depth how thoroughly each response is validated
     * @param uiActionsBySequence for {@link ApiCodegenStyle#HYBRID_UI_API} only: caller-supplied
     *                            UI action source lines (e.g. {@code driver.element().click(...);})
     *                            keyed by the capture-session sequence number they were recorded
     *                            at. Ignored by every other style. May be empty, in which case the
     *                            hybrid method still renders deterministically -- it degrades to
     *                            API-only assertions with no interleaved UI lines.
     * @return generated source, any test-data artifacts it references, and any transaction IDs
     *         skipped because SHAFT.API has no builder for their HTTP method
     */
    public static RenderedApiTest render(
            String packageName,
            String className,
            List<ApiTransaction> transactions,
            ApiCodegenStyle style,
            ApiValidationDepth depth,
            Map<Long, List<String>> uiActionsBySequence) {
        Map<String, List<ApiTransaction>> byOrigin = groupByOrigin(transactions);
        List<String> origins = List.copyOf(byOrigin.keySet());
        Map<String, String> fieldNames = fieldNamesByOrigin(origins);
        Map<String, String> artifacts = new LinkedHashMap<>();
        List<String> skippedTransactionIds = new ArrayList<>();
        boolean hybrid = style == ApiCodegenStyle.HYBRID_UI_API;

        StringBuilder source = new StringBuilder();
        line(source, "package " + packageName + ";");
        line(source, "");
        line(source, "import com.shaft.driver.SHAFT;");
        line(source, "import org.testng.annotations.BeforeClass;");
        line(source, "import org.testng.annotations.Test;");
        line(source, "");
        line(source, "/**");
        line(source, " * Generated by SHAFT ApiCaptureGenerator. Do not edit by hand; regenerate from the");
        line(source, " * recording instead.");
        line(source, " */");
        line(source, "public class " + className + " {");
        if (hybrid) {
            line(source, "    private SHAFT.GUI.WebDriver driver;");
        }
        for (String origin : origins) {
            line(source, "    private SHAFT.API " + fieldNames.get(origin) + ";");
        }
        line(source, "");
        line(source, "    @BeforeClass(alwaysRun = true)");
        line(source, "    public void setupApiSessions() {");
        if (hybrid) {
            line(source, "        driver = new SHAFT.GUI.WebDriver();");
        }
        for (String origin : origins) {
            String field = fieldNames.get(origin);
            line(source, "        " + field + " = new SHAFT.API(\"" + escape(origin) + "\");");
            for (Map.Entry<String, String> hoisted : commonHeaders(byOrigin.get(origin)).entrySet()) {
                line(source, "        " + field + ".addHeader(\"" + escape(hoisted.getKey()) + "\", "
                        + headerValueExpression(hoisted.getValue()) + ");");
            }
        }
        line(source, "    }");
        line(source, "");

        if (hybrid) {
            int scenarioIndex = 0;
            for (String origin : origins) {
                scenarioIndex++;
                String methodName = origins.size() == 1 ? "recordedHybridScenario" : "recordedHybridScenario" + scenarioIndex;
                renderHybridScenario(source, artifacts, skippedTransactionIds, className, methodName,
                        byOrigin.get(origin), depth, uiActionsBySequence);
            }
        } else if (style == ApiCodegenStyle.SCENARIO) {
            int scenarioIndex = 0;
            for (String origin : origins) {
                scenarioIndex++;
                String methodName = origins.size() == 1 ? "recordedApiScenario" : "recordedApiScenario" + scenarioIndex;
                renderScenario(source, artifacts, skippedTransactionIds, className, methodName,
                        fieldNames.get(origin), byOrigin.get(origin), depth);
            }
        } else {
            for (String origin : origins) {
                String field = fieldNames.get(origin);
                for (ApiTransaction transaction : byOrigin.get(origin)) {
                    renderPerRequestTest(source, artifacts, skippedTransactionIds, className, field, transaction, depth);
                }
            }
        }

        if (hybrid) {
            line(source, "    @org.testng.annotations.AfterClass(alwaysRun = true)");
            line(source, "    public void tearDownDriver() {");
            line(source, "        driver.quit();");
            line(source, "    }");
            line(source, "");
        }
        renderHelperMethods(source);
        line(source, "}");
        line(source, "");

        return new RenderedApiTest(source.toString(), artifacts, List.copyOf(skippedTransactionIds));
    }

    // ---- HYBRID_UI_API style: UI codegen lines interleaved with API assertResponse(...) blocks ----

    /**
     * Renders one {@code @Test} method per origin that interleaves caller-supplied UI action
     * lines with a {@code driver.browser().interceptRequest()....assertResponse(...)} block for
     * each transaction, placed immediately after the UI anchor (keyed by
     * {@link ApiTransaction#correlatedUiSequence()}) that triggered it. Transactions with no
     * correlated UI sequence -- e.g. recorded before hybrid correlation existed, or fired with no
     * observable UI trigger -- are appended, in recorded order, after every anchored transaction.
     * This keeps the method deterministic and always valid even when {@code uiActionsBySequence}
     * is empty (pure API-assertion rendering, no UI lines at all).
     */
    private static void renderHybridScenario(
            StringBuilder source,
            Map<String, String> artifacts,
            List<String> skippedTransactionIds,
            String className,
            String methodName,
            List<ApiTransaction> originTransactions,
            ApiValidationDepth depth,
            Map<Long, List<String>> uiActionsBySequence) {
        List<ApiTransaction> anchored = new ArrayList<>();
        List<ApiTransaction> unanchored = new ArrayList<>();
        for (ApiTransaction transaction : originTransactions) {
            (transaction.correlatedUiSequence() == null ? unanchored : anchored).add(transaction);
        }
        anchored.sort(java.util.Comparator.comparingLong(ApiTransaction::correlatedUiSequence));

        line(source, "    @Test");
        line(source, "    public void " + methodName + "() {");
        Set<Long> renderedSequences = new LinkedHashSet<>();
        for (ApiTransaction transaction : anchored) {
            long sequence = transaction.correlatedUiSequence();
            if (renderedSequences.add(sequence)) {
                for (String uiLine : uiActionsBySequence.getOrDefault(sequence, List.of())) {
                    line(source, "        " + uiLine);
                }
            }
            renderHybridAssertion(source, artifacts, skippedTransactionIds, className, transaction, depth);
        }
        for (ApiTransaction transaction : unanchored) {
            renderHybridAssertion(source, artifacts, skippedTransactionIds, className, transaction, depth);
        }
        line(source, "    }");
        line(source, "");
    }

    private static void renderHybridAssertion(
            StringBuilder source,
            Map<String, String> artifacts,
            List<String> skippedTransactionIds,
            String className,
            ApiTransaction transaction,
            ApiValidationDepth depth) {
        if (requestMethodFor(transaction.method()) == null) {
            skippedTransactionIds.add(transaction.transactionId());
            line(source, "        // Skipped " + transaction.method() + " " + escape(transaction.url())
                    + " -- SHAFT.API has no builder for this HTTP method.");
            return;
        }
        line(source, "        // Recorded UI action above triggered: " + transaction.method() + " "
                + escape(relativePath(transaction)) + " (" + transaction.statusCode() + ")");
        line(source, "        driver.browser().interceptRequest()");
        line(source, "                .urlContains(\"" + escape(relativePath(transaction)) + "\")");
        line(source, "                .assertResponse(v -> {");
        line(source, "                    v.time().isLessThan(30000L).perform();");
        renderHybridBodyAssertion(source, artifacts, className, transaction, depth);
        line(source, "                });");
    }

    private static void renderHybridBodyAssertion(
            StringBuilder source,
            Map<String, String> artifacts,
            String className,
            ApiTransaction transaction,
            ApiValidationDepth depth) {
        if (depth != ApiValidationDepth.SCHEMA && depth != ApiValidationDepth.FULL_BODY) {
            return;
        }
        if (transaction.responseBody().isBlank()) {
            return;
        }
        if (depth == ApiValidationDepth.SCHEMA) {
            String relativePath = "api-capture/" + className + "-" + sanitizeIdentifier(transaction.transactionId()) + "-schema.json";
            artifacts.put(relativePath, JsonSchemaInferencer.infer(transaction.responseBody()));
            line(source, "                    v.matchesSchema(\"" + escape(relativePath) + "\").perform();");
        } else {
            String relativePath = "api-capture/" + className + "-" + sanitizeIdentifier(transaction.transactionId()) + "-body.json";
            artifacts.put(relativePath, normalizeForGoldenFile(transaction.responseBody(), transaction.responseLeaves()));
            line(source, "                    v.isEqualToFileContentIgnoringOrder(\"" + escape(relativePath) + "\").perform();");
        }
    }

    // ---- SCENARIO style: one @Test per origin, chaining correlated values via local variables ----

    private static void renderScenario(
            StringBuilder source,
            Map<String, String> artifacts,
            List<String> skippedTransactionIds,
            String className,
            String methodName,
            String apiField,
            List<ApiTransaction> originTransactions,
            ApiValidationDepth depth) {
        List<CorrelatedTransaction> correlated = TransactionCorrelator.correlate(originTransactions);
        Map<String, String> valueToVariable = new LinkedHashMap<>();
        Set<String> usedVariableNames = new LinkedHashSet<>();

        line(source, "    @Test");
        line(source, "    public void " + methodName + "() {");
        for (CorrelatedTransaction ct : correlated) {
            ApiTransaction transaction = ct.transaction();
            String requestMethod = requestMethodFor(transaction.method());
            if (requestMethod == null) {
                skippedTransactionIds.add(transaction.transactionId());
                line(source, "        // Skipped " + transaction.method() + " " + escape(transaction.url())
                        + " -- SHAFT.API has no builder for this HTTP method.");
                continue;
            }
            line(source, "        // " + transaction.method() + " " + commentSafePath(relativePath(transaction), valueToVariable)
                    + " (" + transaction.statusCode() + ")");
            String pathExpression = interpolate(relativePath(transaction), valueToVariable);
            line(source, "        " + apiField + "." + requestMethod + "(" + pathExpression + ")");
            renderRequestHeaders(source, transaction, valueToVariable);
            renderRequestBody(source, transaction, valueToVariable);
            line(source, "                .setTargetStatusCode(" + transaction.statusCode() + ")");
            line(source, "                .perform();");
            renderValidation(source, artifacts, className, apiField, transaction, ct.leaves(), depth);
            renderCorrelatedExtractions(source, apiField, ct, valueToVariable, usedVariableNames);
        }
        line(source, "    }");
        line(source, "");
    }

    private static void renderCorrelatedExtractions(
            StringBuilder source,
            String apiField,
            CorrelatedTransaction ct,
            Map<String, String> valueToVariable,
            Set<String> usedVariableNames) {
        for (ResponseLeaf leaf : ct.leaves()) {
            if (leaf.classification() != LeafClassification.CORRELATED) {
                continue;
            }
            String variableName = uniqueVariableName(leaf.jsonPath(), usedVariableNames);
            line(source, "        String " + variableName + " = " + apiField + ".getResponseJSONValue(\""
                    + escape(leaf.jsonPath()) + "\");");
            valueToVariable.put(leaf.value(), variableName);
        }
    }

    // ---- PER_REQUEST style: one independent @Test per transaction, literals only ----

    private static void renderPerRequestTest(
            StringBuilder source,
            Map<String, String> artifacts,
            List<String> skippedTransactionIds,
            String className,
            String apiField,
            ApiTransaction transaction,
            ApiValidationDepth depth) {
        String requestMethod = requestMethodFor(transaction.method());
        String methodName = "recordedApiRequest_" + sanitizeIdentifier(transaction.transactionId());
        line(source, "    @Test");
        line(source, "    public void " + methodName + "() {");
        if (requestMethod == null) {
            skippedTransactionIds.add(transaction.transactionId());
            line(source, "        // Skipped " + transaction.method() + " " + escape(transaction.url())
                    + " -- SHAFT.API has no builder for this HTTP method.");
            line(source, "    }");
            line(source, "");
            return;
        }
        Map<String, String> noCorrelation = Map.of();
        line(source, "        " + apiField + "." + requestMethod + "("
                + interpolate(relativePath(transaction), noCorrelation) + ")");
        renderRequestHeaders(source, transaction, noCorrelation);
        renderRequestBody(source, transaction, noCorrelation);
        line(source, "                .setTargetStatusCode(" + transaction.statusCode() + ")");
        line(source, "                .perform();");
        renderValidation(source, artifacts, className, apiField, transaction,
                ResponseNormalizer.classify(transaction.responseBody()), depth);
        line(source, "    }");
        line(source, "");
    }

    // ---- shared rendering helpers ----

    private static void renderRequestHeaders(
            StringBuilder source, ApiTransaction transaction, Map<String, String> valueToVariable) {
        for (Map.Entry<String, String> header : transaction.requestHeaders().entrySet()) {
            String name = header.getKey().toLowerCase(Locale.ROOT);
            if (SKIPPED_REQUEST_HEADERS.contains(name)) {
                continue;
            }
            line(source, "                .addHeader(\"" + escape(header.getKey()) + "\", "
                    + headerOrInterpolatedValueExpression(header.getValue(), valueToVariable) + ")");
        }
    }

    /**
     * Renders the request body with content-type fidelity (issue #3530 A3). The recorder captures
     * the {@code Content-Type} request header but drops it from the generated {@code addHeader}
     * calls (it is a mechanical/transport header), which previously made every replay default to
     * JSON. This detects the captured content type and renders each family faithfully:
     * <ul>
     *   <li>{@code application/x-www-form-urlencoded} &rarr; parsed into a {@code setParameters(...,
     *       FORM)} map (unless a correlated value flows through the body, in which case the raw
     *       interpolated body is preserved so chaining still works);</li>
     *   <li>{@code multipart/form-data} &rarr; content type preserved plus a note to wire file
     *       parts, since binary parts cannot be reconstructed from a captured string;</li>
     *   <li>GraphQL (a {@code graphql} content type, or a JSON body with a top-level {@code query})
     *       &rarr; a pointer to {@code sendGraphQlRequest} plus the working JSON body;</li>
     *   <li>any other non-JSON content type &rarr; preserved via {@code setContentType} before the
     *       body;</li>
     *   <li>JSON / no content type &rarr; the existing {@code setRequestBody} default.</li>
     * </ul>
     */
    private static void renderRequestBody(
            StringBuilder source, ApiTransaction transaction, Map<String, String> valueToVariable) {
        String body = transaction.requestBody();
        if (body.isBlank()) {
            return;
        }
        String rawContentType = requestHeaderValue(transaction, "content-type");
        String contentType = rawContentType == null ? "" : rawContentType.toLowerCase(Locale.ROOT);

        if (contentType.contains("x-www-form-urlencoded") && !bodyCarriesCorrelation(body, valueToVariable)) {
            Map<String, String> formParameters = parseFormUrlEncoded(body);
            if (!formParameters.isEmpty()) {
                renderFormParameters(source, formParameters);
                return;
            }
        }
        if (contentType.contains("multipart/form-data")) {
            line(source, "                // Multipart body captured as raw text; add file parts with"
                    + " setParameters(..., RestActions.ParametersType.MULTIPART) if this endpoint uploads files.");
        } else if (isGraphQlRequest(contentType, body)) {
            line(source, "                // GraphQL request; SHAFT.API.sendGraphQlRequest(service, query[, variables])"
                    + " is the idiomatic alternative to this raw body.");
        }
        if (shouldPreserveContentType(contentType)) {
            line(source, "                .setContentType(\"" + escape(rawContentType) + "\")");
        }
        line(source, "                .setRequestBody(" + interpolate(body, valueToVariable) + ")");
    }

    /**
     * Returns the raw value of the first request header matching {@code name} (case-insensitive),
     * or {@code null} when absent.
     */
    private static String requestHeaderValue(ApiTransaction transaction, String name) {
        for (Map.Entry<String, String> header : transaction.requestHeaders().entrySet()) {
            if (header.getKey().equalsIgnoreCase(name)) {
                return header.getValue();
            }
        }
        return null;
    }

    /**
     * True when a correlated value flows through this body, so switching to {@code setParameters}
     * (which cannot interpolate) would drop the chaining -- the raw interpolated body is kept instead.
     */
    private static boolean bodyCarriesCorrelation(String body, Map<String, String> valueToVariable) {
        for (String capturedValue : valueToVariable.keySet()) {
            if (!capturedValue.isEmpty() && body.contains(capturedValue)) {
                return true;
            }
        }
        return false;
    }

    /**
     * A content type worth pinning on the generated request: present and not JSON (SHAFT already
     * defaults an unset content type to JSON, so re-stating it would be noise).
     */
    private static boolean shouldPreserveContentType(String contentType) {
        return !contentType.isBlank() && !contentType.contains("application/json");
    }

    private static boolean isGraphQlRequest(String contentType, String body) {
        if (contentType.contains("graphql")) {
            return true;
        }
        String trimmed = body.trim();
        return contentType.contains("json") && trimmed.startsWith("{") && trimmed.contains("\"query\"");
    }

    /**
     * Parses an {@code a=1&b=2} form body into an ordered, URL-decoded key/value map. Malformed
     * pairs (empty key) are skipped; a value-less key maps to the empty string.
     */
    private static Map<String, String> parseFormUrlEncoded(String body) {
        Map<String, String> parameters = new LinkedHashMap<>();
        for (String pair : body.split("&")) {
            if (pair.isEmpty()) {
                continue;
            }
            int equals = pair.indexOf('=');
            String rawKey = equals < 0 ? pair : pair.substring(0, equals);
            String rawValue = equals < 0 ? "" : pair.substring(equals + 1);
            String key = urlDecode(rawKey);
            if (key.isEmpty()) {
                continue;
            }
            parameters.put(key, urlDecode(rawValue));
        }
        return parameters;
    }

    private static String urlDecode(String value) {
        try {
            return URLDecoder.decode(value, StandardCharsets.UTF_8);
        } catch (IllegalArgumentException e) {
            // A malformed percent-escape is left as-is rather than dropping the value.
            return value;
        }
    }

    private static void renderFormParameters(StringBuilder source, Map<String, String> parameters) {
        line(source, "                .setContentType(\"application/x-www-form-urlencoded\")");
        StringBuilder entries = new StringBuilder();
        boolean first = true;
        for (Map.Entry<String, String> parameter : parameters.entrySet()) {
            if (!first) {
                entries.append(", ");
            }
            entries.append("java.util.Map.entry(\"").append(escape(parameter.getKey()))
                    .append("\", \"").append(escape(parameter.getValue())).append("\")");
            first = false;
        }
        line(source, "                .setParameters(java.util.Map.<String, Object>ofEntries(" + entries
                + "), com.shaft.api.RestActions.ParametersType.FORM)");
    }

    private static void renderValidation(
            StringBuilder source,
            Map<String, String> artifacts,
            String className,
            String apiField,
            ApiTransaction transaction,
            List<ResponseLeaf> leaves,
            ApiValidationDepth depth) {
        switch (depth) {
            case STATUS -> {
                // setTargetStatusCode(...).perform() above already asserted the status code.
            }
            case STATUS_HEADERS -> renderHeaderAssertions(source, apiField, transaction);
            case SCHEMA -> renderSchemaAssertion(source, artifacts, className, apiField, transaction);
            case FULL_BODY -> renderFullBodyAssertion(source, artifacts, className, apiField, transaction, leaves);
            case BUSINESS -> renderBusinessAssertions(source, apiField, transaction, leaves);
            default -> throw new IllegalArgumentException("Unsupported validation depth: " + depth);
        }
    }

    private static void renderBusinessAssertions(
            StringBuilder source, String apiField, ApiTransaction transaction, List<ResponseLeaf> leaves) {
        // A 4xx/5xx response is a negative case: render a dedicated error-shape template that pins
        // the error code and message fields first, rather than the happy-path stable-field pinning.
        if (transaction.statusCode() >= 400) {
            renderErrorShapeAssertions(source, apiField, transaction, leaves);
            return;
        }
        for (ResponseLeaf leaf : leaves) {
            // Pin only business-meaningful fields: stable values a human would check. Volatile,
            // correlated (chained), and sensitive leaves are deliberately skipped so the assertion
            // does not flake on generated IDs/timestamps or leak secrets.
            if (leaf.classification() != LeafClassification.STABLE || leaf.value().isBlank()) {
                continue;
            }
            pinStableLeaf(source, apiField, leaf);
        }
    }

    /**
     * Renders a negative-case (4xx/5xx) error-shape assertion template: the HTTP status is already
     * asserted via {@code setTargetStatusCode(...)}, so this pins the stable error <em>code</em> and
     * <em>message</em> fields (recognized by well-known property names) first, then any remaining
     * stable fields. Volatile/correlated/sensitive leaves are skipped exactly as in the happy path,
     * so timestamps and trace IDs never flake the assertion (issue #3530 negative-case templates).
     */
    private static void renderErrorShapeAssertions(
            StringBuilder source, String apiField, ApiTransaction transaction, List<ResponseLeaf> leaves) {
        line(source, "        // Negative-case error shape (HTTP " + transaction.statusCode()
                + "): the status code is asserted above; pin the stable error code and message");
        line(source, "        // fields, skipping volatile diagnostics (timestamps, trace IDs) so the test documents the error contract.");
        List<ResponseLeaf> ordered = orderedErrorLeaves(leaves);
        for (ResponseLeaf leaf : ordered) {
            pinStableLeaf(source, apiField, leaf);
        }
        if (ordered.isEmpty()) {
            line(source, "        // No stable error fields were recorded; the HTTP "
                    + transaction.statusCode() + " status assertion above stands alone.");
        }
    }

    /**
     * Orders an error response's stable leaves so the contract-relevant fields lead: recognized
     * error <em>code</em> fields first, then <em>message</em> fields, then any remaining stable
     * field. Volatile/correlated/sensitive and blank leaves are dropped.
     */
    private static List<ResponseLeaf> orderedErrorLeaves(List<ResponseLeaf> leaves) {
        List<ResponseLeaf> codeLeaves = new ArrayList<>();
        List<ResponseLeaf> messageLeaves = new ArrayList<>();
        List<ResponseLeaf> otherStable = new ArrayList<>();
        for (ResponseLeaf leaf : leaves) {
            if (leaf.classification() != LeafClassification.STABLE || leaf.value().isBlank()) {
                continue;
            }
            String key = leaf.key().toLowerCase(Locale.ROOT);
            if (ERROR_CODE_KEYS.contains(key)) {
                codeLeaves.add(leaf);
            } else if (ERROR_MESSAGE_KEYS.contains(key)) {
                messageLeaves.add(leaf);
            } else {
                otherStable.add(leaf);
            }
        }
        List<ResponseLeaf> ordered = new ArrayList<>(codeLeaves);
        ordered.addAll(messageLeaves);
        ordered.addAll(otherStable);
        return ordered;
    }

    private static void pinStableLeaf(StringBuilder source, String apiField, ResponseLeaf leaf) {
        line(source, "        SHAFT.Validations.assertThat()");
        line(source, "                .object(" + apiField + ".getResponseJSONValue(\""
                + escape(leaf.jsonPath()) + "\"))");
        line(source, "                .isEqualTo(\"" + escape(leaf.value()) + "\")");
        line(source, "                .perform();");
    }

    private static void renderHeaderAssertions(StringBuilder source, String apiField, ApiTransaction transaction) {
        for (Map.Entry<String, String> header : transaction.responseHeaders().entrySet()) {
            if (HttpContractRecorder.isSensitiveKey(header.getKey())
                    || HttpContractRecorder.isVolatileKey(header.getKey())
                    || HttpContractRecorder.isVolatileValue(header.getValue())) {
                continue;
            }
            line(source, "        SHAFT.Validations.assertThat()");
            line(source, "                .object(" + apiField + ".getResponse().getHeader(\""
                    + escape(header.getKey()) + "\"))");
            line(source, "                .isEqualTo(\"" + escape(header.getValue()) + "\")");
            line(source, "                .perform();");
        }
    }

    private static void renderSchemaAssertion(
            StringBuilder source, Map<String, String> artifacts, String className, String apiField,
            ApiTransaction transaction) {
        if (transaction.responseBody().isBlank()) {
            return;
        }
        String relativePath = "api-capture/" + className + "-" + sanitizeIdentifier(transaction.transactionId()) + "-schema.json";
        artifacts.put(relativePath, JsonSchemaInferencer.infer(transaction.responseBody()));
        line(source, "        " + apiField + ".assertThatResponse().matchesSchema(\"" + escape(relativePath) + "\").perform();");
    }

    private static void renderFullBodyAssertion(
            StringBuilder source, Map<String, String> artifacts, String className, String apiField,
            ApiTransaction transaction, List<ResponseLeaf> leaves) {
        if (transaction.responseBody().isBlank()) {
            return;
        }
        String relativePath = "api-capture/" + className + "-" + sanitizeIdentifier(transaction.transactionId()) + "-body.json";
        artifacts.put(relativePath, normalizeForGoldenFile(transaction.responseBody(), leaves));
        line(source, "        " + apiField + ".assertThatResponse().isEqualToFileContentIgnoringOrder(\""
                + escape(relativePath) + "\").perform();");
    }

    private static void renderHelperMethods(StringBuilder source) {
        line(source, "    private String requiredEnvironment(String name) {");
        line(source, "        String value = System.getenv(name);");
        line(source, "        if (value == null || value.isBlank()) {");
        line(source, "            throw new IllegalStateException(\"Missing required environment variable: \" + name);");
        line(source, "        }");
        line(source, "        return value;");
        line(source, "    }");
        line(source, "");
    }

    // ---- text/value helpers ----

    private static String requestMethodFor(String httpMethod) {
        return switch (httpMethod.toUpperCase(Locale.ROOT)) {
            case "GET" -> "get";
            case "POST" -> "post";
            case "PUT" -> "put";
            case "PATCH" -> "patch";
            case "DELETE" -> "delete";
            case "HEAD" -> "head";
            case "OPTIONS" -> "options";
            default -> null;
        };
    }

    private static String relativePath(ApiTransaction transaction) {
        String url = transaction.url();
        String origin = transaction.origin();
        return url.startsWith(origin) ? url.substring(origin.length()) : url;
    }

    private static String headerValueExpression(String rawValue) {
        if (rawValue.startsWith(SECRET_REF_PREFIX)) {
            return "requiredEnvironment(\"" + escape(rawValue.substring(SECRET_REF_PREFIX.length())) + "\")";
        }
        return "\"" + escape(rawValue) + "\"";
    }

    private static String headerOrInterpolatedValueExpression(String rawValue, Map<String, String> valueToVariable) {
        if (rawValue.startsWith(SECRET_REF_PREFIX)) {
            return headerValueExpression(rawValue);
        }
        return interpolate(rawValue, valueToVariable);
    }

    /**
     * Renders a Java string EXPRESSION (a literal, or a concatenation) for {@code rawText},
     * substituting any known correlated value with a reference to the variable it was extracted
     * into. Does not special-case secret-ref tokens -- callers needing that (request headers)
     * check for the prefix themselves before calling this.
     */
    private static String interpolate(String rawText, Map<String, String> valueToVariable) {
        if (valueToVariable.isEmpty()) {
            return "\"" + escape(rawText) + "\"";
        }
        StringBuilder expression = new StringBuilder();
        String remaining = rawText;
        boolean any = false;
        while (!remaining.isEmpty()) {
            int bestIndex = -1;
            String bestValue = null;
            for (String candidateValue : valueToVariable.keySet()) {
                int index = remaining.indexOf(candidateValue);
                if (index >= 0 && (bestIndex == -1 || index < bestIndex)) {
                    bestIndex = index;
                    bestValue = candidateValue;
                }
            }
            if (bestIndex == -1) {
                appendLiteralPart(expression, remaining, any);
                any = true;
                break;
            }
            String prefix = remaining.substring(0, bestIndex);
            if (!prefix.isEmpty()) {
                appendLiteralPart(expression, prefix, any);
                any = true;
            }
            if (any) {
                expression.append(" + ");
            }
            expression.append(valueToVariable.get(bestValue));
            any = true;
            remaining = remaining.substring(bestIndex + bestValue.length());
        }
        return any ? expression.toString() : "\"\"";
    }

    /**
     * Renders plain (non-code) comment text with every known correlated value replaced by a
     * {@code {variableName}} marker, so a comment never shows the raw value of something the
     * actual generated code references only through a variable.
     */
    private static String commentSafePath(String rawText, Map<String, String> valueToVariable) {
        String result = rawText.replace("\n", " ").replace("\r", "");
        for (Map.Entry<String, String> entry : valueToVariable.entrySet()) {
            result = result.replace(entry.getKey(), "{" + entry.getValue() + "}");
        }
        return result;
    }

    private static void appendLiteralPart(StringBuilder expression, String literal, boolean needsPlus) {
        if (needsPlus) {
            expression.append(" + ");
        }
        expression.append('"').append(escape(literal)).append('"');
    }

    private static String uniqueVariableName(String jsonPath, Set<String> usedNames) {
        String[] segments = jsonPath.replace("[", ".").replace("]", "").split("\\.");
        String base = segments.length == 0 ? "value" : sanitizeIdentifier(segments[segments.length - 1]);
        if (base.isBlank()) {
            base = "value";
        }
        String candidate = base;
        int suffix = 1;
        while (!usedNames.add(candidate)) {
            suffix++;
            candidate = base + suffix;
        }
        return candidate;
    }

    private static String sanitizeIdentifier(String raw) {
        StringBuilder identifier = new StringBuilder();
        for (char character : raw.toCharArray()) {
            if (Character.isLetterOrDigit(character)) {
                identifier.append(character);
            } else {
                identifier.append('_');
            }
        }
        String result = identifier.toString();
        if (result.isEmpty() || Character.isDigit(result.charAt(0))) {
            result = "v" + result;
        }
        return result;
    }

    private static String escape(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "");
    }

    private static Map<String, List<ApiTransaction>> groupByOrigin(List<ApiTransaction> transactions) {
        Map<String, List<ApiTransaction>> byOrigin = new LinkedHashMap<>();
        for (ApiTransaction transaction : transactions) {
            byOrigin.computeIfAbsent(transaction.origin(), origin -> new ArrayList<>()).add(transaction);
        }
        return byOrigin;
    }

    private static Map<String, String> fieldNamesByOrigin(List<String> origins) {
        Map<String, String> names = new LinkedHashMap<>();
        for (int i = 0; i < origins.size(); i++) {
            names.put(origins.get(i), origins.size() == 1 ? "api" : "api" + (i + 1));
        }
        return names;
    }

    private static Map<String, String> commonHeaders(List<ApiTransaction> transactions) {
        Map<String, String> common = new LinkedHashMap<>();
        if (transactions.isEmpty()) {
            return common;
        }
        for (Map.Entry<String, String> candidate : transactions.get(0).requestHeaders().entrySet()) {
            String name = candidate.getKey().toLowerCase(Locale.ROOT);
            if (SKIPPED_REQUEST_HEADERS.contains(name)) {
                continue;
            }
            boolean sharedByAll = transactions.stream()
                    .allMatch(t -> candidate.getValue().equals(t.requestHeaders().get(candidate.getKey())));
            if (sharedByAll) {
                common.put(candidate.getKey(), candidate.getValue());
            }
        }
        return common;
    }

    private static String normalizeForGoldenFile(String rawBody, List<ResponseLeaf> leaves) {
        Map<String, LeafClassification> byPath = new LinkedHashMap<>();
        leaves.forEach(leaf -> byPath.put(leaf.jsonPath(), leaf.classification()));
        try {
            JsonNode root = MAPPER.readTree(rawBody);
            JsonNode normalized = normalizeNode("$", root, byPath);
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(normalized);
        } catch (RuntimeException malformed) {
            return rawBody;
        }
    }

    private static JsonNode normalizeNode(String path, JsonNode node, Map<String, LeafClassification> byPath) {
        if (node == null || node.isNull() || node.isMissingNode()) {
            return node;
        }
        if (node.isObject()) {
            ObjectNode copy = MAPPER.createObjectNode();
            node.forEachEntry((key, value) -> copy.set(key, normalizeNode(path + "." + key, value, byPath)));
            return copy;
        }
        if (node.isArray()) {
            var copy = MAPPER.createArrayNode();
            int index = 0;
            for (JsonNode element : node) {
                copy.add(normalizeNode(path + "[" + index + "]", element, byPath));
                index++;
            }
            return copy;
        }
        LeafClassification classification = byPath.getOrDefault(path, LeafClassification.STABLE);
        return switch (classification) {
            case SENSITIVE -> MAPPER.getNodeFactory().textNode(SENSITIVE_PLACEHOLDER);
            case VOLATILE, CORRELATED -> MAPPER.getNodeFactory().textNode(VOLATILE_PLACEHOLDER);
            case STABLE -> node;
        };
    }

    private static void line(StringBuilder source, String value) {
        source.append(value).append('\n');
    }
}
