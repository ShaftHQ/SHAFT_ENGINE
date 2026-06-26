package com.shaft.tools.io.internal;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptionRule;
import com.shaft.tools.io.ReportManager;
import io.restassured.response.Response;
import io.restassured.specification.FilterableRequestSpecification;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.io.IOException;
import java.net.URI;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.format.DateTimeParseException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

/**
 * Thread-local HTTP contract recorder, validator, and browser replay rule factory.
 */
public final class HttpContractRecorder {
    private static final String MASKED = "********";
    private static final String NORMALIZED = "<normalized>";
    private static final String SCHEMA_VERSION = "1.0";
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
    private static final Pattern UUID_PATTERN = Pattern.compile(
            "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");
    private static final ThreadLocal<RecordingSession> RECORDING = new ThreadLocal<>();
    private static final ThreadLocal<ValidationSession> VALIDATION = new ThreadLocal<>();

    private HttpContractRecorder() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Starts recording current-thread browser and SHAFT API HTTP traffic.
     *
     * @param contractFilePath destination JSON contract path
     * @param urlContains optional URL fragments used to select traffic
     */
    public static void startRecording(String contractFilePath, String... urlContains) {
        RECORDING.set(new RecordingSession(Path.of(requireNonBlank(contractFilePath, "contractFilePath")),
                selectors(urlContains), new ArrayList<>()));
    }

    /**
     * Writes the current recording session and clears it.
     */
    public static void stopRecording() {
        RecordingSession session = RECORDING.get();
        if (session == null) {
            return;
        }
        try {
            writeContract(session.path(), new Contract(SCHEMA_VERSION, session.interactions()));
            ReportManager.log("Saved HTTP contract: " + session.path());
        } finally {
            RECORDING.remove();
        }
    }

    /**
     * Starts hard-assert contract validation for current-thread browser and SHAFT API traffic.
     *
     * @param contractFilePath source JSON contract path
     * @param urlContains optional URL fragments used to select traffic
     */
    public static void startAssertMode(String contractFilePath, String... urlContains) {
        startValidation(contractFilePath, ValidationMode.ASSERT, urlContains);
    }

    /**
     * Starts soft-verify contract validation for current-thread browser and SHAFT API traffic.
     *
     * @param contractFilePath source JSON contract path
     * @param urlContains optional URL fragments used to select traffic
     */
    public static void startVerifyMode(String contractFilePath, String... urlContains) {
        startValidation(contractFilePath, ValidationMode.VERIFY, urlContains);
    }

    /**
     * Clears current-thread validation mode.
     */
    public static void stopValidation() {
        VALIDATION.remove();
    }

    /**
     * Clears current-thread recording and validation state without writing files.
     */
    public static void clear() {
        RECORDING.remove();
        VALIDATION.remove();
    }

    /**
     * Returns whether browser interception must actively fetch responses for contract work.
     *
     * @return true when recording or validation is active
     */
    public static boolean isBrowserContractModeActive() {
        return isContractModeActive();
    }

    /**
     * Records or validates a browser HTTP exchange when a contract mode is active.
     *
     * @param request browser request
     * @param response browser response
     * @param failureReason optional failure reason
     */
    public static void handleBrowserExchange(HttpRequest request, HttpResponse response, String failureReason) {
        if (!isContractModeActive()) {
            return;
        }
        Interaction interaction = interaction("browser", request, response, failureReason);
        record(interaction);
        validate(interaction);
    }

    /**
     * Records or validates a SHAFT API HTTP exchange when a contract mode is active.
     *
     * @param requestSpec REST Assured request spec
     * @param response REST Assured response
     */
    public static void handleApiExchange(FilterableRequestSpecification requestSpec, Response response) {
        if (!isContractModeActive()) {
            return;
        }
        Interaction interaction = interaction("api", requestSpec, response);
        record(interaction);
        validate(interaction);
    }

    /**
     * Builds browser replay rules from a recorded contract.
     *
     * @param contractFilePath source JSON contract path
     * @return replay rules that return recorded responses
     */
    public static List<BrowserNetworkInterceptionRule> browserReplayRules(String contractFilePath) {
        Contract contract = readContract(Path.of(requireNonBlank(contractFilePath, "contractFilePath")));
        List<Interaction> replayable = contract.interactions().stream()
                .filter(interaction -> interaction.response().status() > 0)
                .toList();
        AtomicInteger cursor = new AtomicInteger();
        return List.of(BrowserNetworkInterceptionRule.mock(
                request -> replayable.stream()
                        .anyMatch(interaction -> matches(interaction, requestMethod(request), requestUrl(request),
                                requestBody(request))),
                request -> toBrowserResponse(nextReplayInteraction(replayable, cursor, request))));
    }

    private static Interaction nextReplayInteraction(List<Interaction> replayable, AtomicInteger cursor,
                                                     HttpRequest request) {
        String method = requestMethod(request);
        String url = requestUrl(request);
        String body = requestBody(request);
        for (int index = cursor.get(); index < replayable.size(); index++) {
            Interaction interaction = replayable.get(index);
            if (matches(interaction, method, url, body)) {
                cursor.set(index + 1);
                return interaction;
            }
        }
        return replayable.stream()
                .filter(interaction -> matches(interaction, method, url, body))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("No replay interaction matched " + method + " "
                        + pathOf(url)));
    }

    private static void startValidation(String contractFilePath, ValidationMode mode, String... urlContains) {
        Path path = Path.of(requireNonBlank(contractFilePath, "contractFilePath"));
        VALIDATION.set(new ValidationSession(mode, selectors(urlContains), readContract(path)));
    }

    private static boolean isContractModeActive() {
        return RECORDING.get() != null || VALIDATION.get() != null;
    }

    private static void record(Interaction interaction) {
        RecordingSession session = RECORDING.get();
        if (session != null && selected(interaction.url(), session.selectors())) {
            session.interactions().add(interaction);
        }
    }

    private static void validate(Interaction actual) {
        ValidationSession session = VALIDATION.get();
        if (session == null || !selected(actual.url(), session.selectors())) {
            return;
        }
        Interaction expected = session.contract().interactions().stream()
                .filter(candidate -> matches(candidate, actual.method(), actual.url(), actual.request().body()))
                .findFirst()
                .orElse(null);
        String diff = expected == null ? missingContractDiff(actual) : diff(expected, actual);
        if (diff.isBlank()) {
            return;
        }
        attachDiff(actual, diff);
        if (session.mode() == ValidationMode.ASSERT) {
            throw new AssertionError(diff);
        }
        ReportManager.log("HTTP contract verification mismatch for " + actual.method() + " " + actual.path());
    }

    private static Interaction interaction(String source, HttpRequest request, HttpResponse response,
                                           String failureReason) {
        String url = requestUrl(request);
        return new Interaction(source,
                requestMethod(request),
                sanitizeUrl(url),
                pathOf(url),
                queryOf(url),
                new Message(0, headersOf(request), normalizeBody(requestBody(request))),
                response == null
                        ? new Message(0, Map.of(), "")
                        : new Message(response.getStatus(), headersOf(response), normalizeBody(responseBody(response))),
                value(failureReason),
                TraceEventRecorder.latestActionId());
    }

    private static Interaction interaction(String source, FilterableRequestSpecification requestSpec, Response response) {
        String url = value(requestSpec.getURI());
        return new Interaction(source,
                value(requestSpec.getMethod()).toUpperCase(Locale.ROOT),
                sanitizeUrl(url),
                pathOf(url),
                queryOf(url),
                new Message(0, headersOf(requestSpec), normalizeBody(apiRequestBody(requestSpec))),
                new Message(response == null ? 0 : response.getStatusCode(),
                        response == null ? Map.of() : headersOf(response),
                        response == null ? "" : normalizeBody(apiResponseBody(response))),
                "",
                TraceEventRecorder.latestActionId());
    }

    private static String diff(Interaction expected, Interaction actual) {
        List<String> lines = new ArrayList<>();
        if (expected.response().status() != actual.response().status()) {
            lines.add("Status expected " + expected.response().status() + " but was " + actual.response().status());
        }
        expected.response().headers().forEach((key, expectedValue) -> {
            if (!MASKED.equals(expectedValue) && !NORMALIZED.equals(expectedValue)) {
                String actualValue = actual.response().headers().get(key);
                if (actualValue == null || !expectedValue.equals(actualValue)) {
                    lines.add("Header " + key + " expected `" + expectedValue + "` but was `" + value(actualValue) + "`");
                }
            }
        });
        if (!expected.response().body().equals(actual.response().body())) {
            lines.add("Response body mismatch");
            lines.add("Expected:");
            lines.add(expected.response().body());
            lines.add("Actual:");
            lines.add(actual.response().body());
        }
        if (lines.isEmpty()) {
            return "";
        }
        return diffHeader(actual) + String.join(System.lineSeparator(), lines);
    }

    private static String missingContractDiff(Interaction actual) {
        return diffHeader(actual) + "No matching contract interaction was found.";
    }

    private static String diffHeader(Interaction actual) {
        String traceAction = actual.traceActionId().isBlank() ? "unavailable" : actual.traceActionId();
        return "HTTP contract mismatch" + System.lineSeparator()
                + "Source: " + actual.source() + System.lineSeparator()
                + "Request: " + actual.method() + " " + actual.path() + System.lineSeparator()
                + "Trace action: " + traceAction + System.lineSeparator()
                + System.lineSeparator();
    }

    private static void attachDiff(Interaction actual, String diff) {
        String name = "HTTP Contract Diff - " + actual.method() + " " + actual.path();
        ReportManagerHelper.attach("text/plain", name, diff);
    }

    private static boolean matches(Interaction expected, String method, String url, String requestBody) {
        if (!expected.method().equalsIgnoreCase(method)) {
            return false;
        }
        if (!expected.path().equals(pathOf(url))) {
            return false;
        }
        if (!expected.query().equals(queryOf(url))) {
            return false;
        }
        String expectedBody = expected.request().body();
        return expectedBody == null || expectedBody.isBlank() || expectedBody.equals(normalizeBody(requestBody));
    }

    private static HttpResponse toBrowserResponse(Interaction interaction) {
        HttpResponse response = new HttpResponse().setStatus(interaction.response().status());
        interaction.response().headers().forEach((key, value) -> {
            if (!MASKED.equals(value) && !NORMALIZED.equals(value)) {
                response.addHeader(key, value);
            }
        });
        if (!interaction.response().body().isBlank()) {
            response.setContent(Contents.bytes(interaction.response().body().getBytes(StandardCharsets.UTF_8)));
        }
        return response;
    }

    private static void writeContract(Path path, Contract contract) {
        try {
            Path parent = path.toAbsolutePath().getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            List<Interaction> sorted = contract.interactions().stream()
                    .sorted(Comparator.comparing(Interaction::source)
                            .thenComparing(Interaction::method)
                            .thenComparing(Interaction::path)
                            .thenComparing(Interaction::url))
                    .toList();
            MAPPER.writeValue(path.toFile(), new Contract(contract.schemaVersion(), sorted));
        } catch (IOException e) {
            throw new IllegalStateException("Could not write HTTP contract to " + path, e);
        }
    }

    private static Contract readContract(Path path) {
        try {
            Contract contract = MAPPER.readValue(path.toFile(), Contract.class);
            return new Contract(value(contract.schemaVersion()).isBlank() ? SCHEMA_VERSION : contract.schemaVersion(),
                    contract.interactions() == null ? List.of() : contract.interactions());
        } catch (IOException e) {
            throw new IllegalStateException("Could not read HTTP contract from " + path, e);
        }
    }

    private static String requestUrl(HttpRequest request) {
        return request == null ? "" : value(request.getUri());
    }

    private static String requestMethod(HttpRequest request) {
        return request == null || request.getMethod() == null ? "" : request.getMethod().name();
    }

    private static String requestBody(HttpRequest request) {
        if (request == null) {
            return "";
        }
        try {
            byte[] body = Contents.bytes(request.getContent());
            request.setContent(Contents.bytes(body));
            return new String(body, StandardCharsets.UTF_8);
        } catch (RuntimeException e) {
            return "";
        }
    }

    private static String responseBody(HttpResponse response) {
        try {
            byte[] body = Contents.bytes(response.getContent());
            response.setContent(Contents.bytes(body));
            return new String(body, StandardCharsets.UTF_8);
        } catch (RuntimeException e) {
            return "";
        }
    }

    private static String apiRequestBody(FilterableRequestSpecification requestSpec) {
        Object body = requestSpec.getBody();
        if (body instanceof byte[] bytes) {
            return new String(bytes, StandardCharsets.UTF_8);
        }
        return body == null ? "" : String.valueOf(body);
    }

    private static String apiResponseBody(Response response) {
        try {
            return response.getBody().asString();
        } catch (RuntimeException e) {
            return "";
        }
    }

    private static Map<String, String> headersOf(HttpRequest request) {
        Map<String, String> headers = new LinkedHashMap<>();
        if (request != null) {
            request.forEachHeader(headers::put);
        }
        return sanitizedMap(headers);
    }

    private static Map<String, String> headersOf(HttpResponse response) {
        Map<String, String> headers = new LinkedHashMap<>();
        if (response != null) {
            response.forEachHeader(headers::put);
            if (response.getContentType() != null && headers.keySet().stream()
                    .noneMatch(key -> key.equalsIgnoreCase("content-type"))) {
                headers.put("Content-Type", response.getContentType());
            }
        }
        return sanitizedMap(headers);
    }

    private static Map<String, String> headersOf(FilterableRequestSpecification requestSpec) {
        Map<String, String> headers = new LinkedHashMap<>();
        requestSpec.getHeaders().forEach(header -> headers.put(header.getName(), header.getValue()));
        requestSpec.getCookies().forEach(cookie -> headers.put("Cookie:" + cookie.getName(), cookie.getValue()));
        return sanitizedMap(headers);
    }

    private static Map<String, String> headersOf(Response response) {
        Map<String, String> headers = new LinkedHashMap<>();
        response.getHeaders().forEach(header -> headers.put(header.getName(), header.getValue()));
        return sanitizedMap(headers);
    }

    private static Map<String, String> queryOf(String url) {
        URI uri = uri(url);
        if (uri == null || uri.getRawQuery() == null || uri.getRawQuery().isBlank()) {
            return Map.of();
        }
        Map<String, String> query = new LinkedHashMap<>();
        for (String pair : uri.getRawQuery().split("&")) {
            int separator = pair.indexOf('=');
            String key = separator >= 0 ? pair.substring(0, separator) : pair;
            String value = separator >= 0 ? pair.substring(separator + 1) : "";
            query.put(urlDecode(key), sanitizeValue(key, urlDecode(value)));
        }
        return sorted(query);
    }

    private static String pathOf(String url) {
        URI uri = uri(url);
        if (uri == null || uri.getPath() == null || uri.getPath().isBlank()) {
            return "/";
        }
        return uri.getPath();
    }

    private static String sanitizeUrl(String url) {
        URI uri = uri(url);
        if (uri == null) {
            return FailureTraceReporter.redact(value(url));
        }
        StringBuilder sanitized = new StringBuilder();
        if (uri.getScheme() != null) {
            sanitized.append(uri.getScheme()).append("://");
        }
        if (uri.getHost() != null) {
            sanitized.append(uri.getHost());
        }
        if (uri.getPort() >= 0) {
            sanitized.append(":").append(uri.getPort());
        }
        sanitized.append(pathOf(url));
        Map<String, String> query = queryOf(url);
        if (!query.isEmpty()) {
            sanitized.append("?");
            int index = 0;
            for (Map.Entry<String, String> entry : query.entrySet()) {
                if (index++ > 0) {
                    sanitized.append("&");
                }
                sanitized.append(entry.getKey()).append("=").append(entry.getValue());
            }
        }
        return sanitized.toString();
    }

    private static URI uri(String url) {
        try {
            return URI.create(value(url));
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    private static String urlDecode(String value) {
        try {
            return URLDecoder.decode(value(value), StandardCharsets.UTF_8);
        } catch (IllegalArgumentException e) {
            return value(value);
        }
    }

    private static Map<String, String> sanitizedMap(Map<String, String> values) {
        Map<String, String> sanitized = new LinkedHashMap<>();
        values.forEach((key, value) -> sanitized.put(key, sanitizeValue(key, value)));
        return sorted(sanitized);
    }

    private static Map<String, String> sorted(Map<String, String> values) {
        Map<String, String> sorted = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
        sorted.putAll(values);
        return new LinkedHashMap<>(sorted);
    }

    private static String sanitizeValue(String key, String rawValue) {
        if (isSensitiveKey(key)) {
            return MASKED;
        }
        if (isVolatileKey(key) || isVolatileValue(rawValue)) {
            return NORMALIZED;
        }
        return FailureTraceReporter.redact(value(rawValue));
    }

    private static String normalizeBody(String body) {
        String value = value(body);
        if (value.isBlank()) {
            return "";
        }
        try {
            JsonNode parsed = MAPPER.readTree(value);
            return MAPPER.writeValueAsString(redactJson("", parsed));
        } catch (IOException e) {
            return FailureTraceReporter.redact(value);
        }
    }

    private static JsonNode redactJson(String key, JsonNode node) {
        if (node == null || node.isNull()) {
            return MAPPER.getNodeFactory().nullNode();
        }
        if (isSensitiveKey(key)) {
            return MAPPER.getNodeFactory().textNode(MASKED);
        }
        if (isVolatileKey(key) && node.isValueNode()) {
            return MAPPER.getNodeFactory().textNode(NORMALIZED);
        }
        if (node.isObject()) {
            ObjectNode object = MAPPER.createObjectNode();
            TreeMap<String, JsonNode> fields = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
            node.fields().forEachRemaining(entry -> fields.put(entry.getKey(), entry.getValue()));
            fields.forEach((fieldName, fieldValue) -> object.set(fieldName, redactJson(fieldName, fieldValue)));
            return object;
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            node.forEach(item -> array.add(redactJson(key, item)));
            return array;
        }
        if (node.isTextual() && isVolatileValue(node.asText())) {
            return MAPPER.getNodeFactory().textNode(NORMALIZED);
        }
        return node.deepCopy();
    }

    private static boolean isSensitiveKey(String key) {
        String normalized = normalizeKey(key);
        return configuredKeys(SHAFT.Properties.api.contractSensitiveKeys()).stream()
                .anyMatch(normalized::contains);
    }

    private static boolean isVolatileKey(String key) {
        String normalized = normalizeKey(key);
        return configuredKeys(SHAFT.Properties.api.contractVolatileKeys()).stream()
                .anyMatch(configured -> normalized.equals(configured) || normalized.endsWith(configured));
    }

    private static Set<String> configuredKeys(String csv) {
        return java.util.Arrays.stream(value(csv).split(","))
                .map(HttpContractRecorder::normalizeKey)
                .filter(key -> !key.isBlank())
                .collect(java.util.stream.Collectors.toCollection(java.util.LinkedHashSet::new));
    }

    private static String normalizeKey(String key) {
        return value(key).toLowerCase(Locale.ROOT).replace("-", "").replace("_", "").replace(":", "");
    }

    private static boolean isVolatileValue(String rawValue) {
        String value = value(rawValue).trim();
        if (UUID_PATTERN.matcher(value).matches()) {
            return true;
        }
        try {
            Instant.parse(value);
            return true;
        } catch (DateTimeParseException ignored) {
            return false;
        }
    }

    private static boolean selected(String url, List<String> selectors) {
        return selectors.isEmpty() || selectors.stream().anyMatch(selector -> value(url).contains(selector));
    }

    private static List<String> selectors(String... urlContains) {
        if (urlContains == null) {
            return List.of();
        }
        return java.util.Arrays.stream(urlContains)
                .filter(Objects::nonNull)
                .map(String::trim)
                .filter(selector -> !selector.isBlank())
                .toList();
    }

    private static String requireNonBlank(String value, String fieldName) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(fieldName + " must not be blank.");
        }
        return value;
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    private enum ValidationMode {
        ASSERT, VERIFY
    }

    private record RecordingSession(Path path, List<String> selectors, List<Interaction> interactions) {
    }

    private record ValidationSession(ValidationMode mode, List<String> selectors, Contract contract) {
    }

    @JsonPropertyOrder({"schemaVersion", "interactions"})
    public record Contract(String schemaVersion, List<Interaction> interactions) {
        public Contract {
            schemaVersion = value(schemaVersion).isBlank() ? SCHEMA_VERSION : schemaVersion;
            interactions = interactions == null ? List.of() : interactions;
        }
    }

    @JsonPropertyOrder({"source", "method", "url", "path", "query", "request", "response", "failureReason",
            "traceActionId"})
    public record Interaction(String source, String method, String url, String path, Map<String, String> query,
                              Message request, Message response, String failureReason, String traceActionId) {
        public Interaction {
            source = value(source);
            method = value(method);
            url = value(url);
            path = value(path).isBlank() ? "/" : path;
            query = query == null ? Map.of() : query;
            request = request == null ? new Message(0, Map.of(), "") : request;
            response = response == null ? new Message(0, Map.of(), "") : response;
            failureReason = value(failureReason);
            traceActionId = value(traceActionId);
        }
    }

    @JsonPropertyOrder({"status", "headers", "body"})
    public record Message(int status, Map<String, String> headers, String body) {
        public Message {
            headers = headers == null ? Map.of() : headers;
            body = value(body);
        }
    }
}
