package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Thread-local browser network, console, and capability metadata recorder for SHAFT trace artifacts.
 */
public final class BrowserObservabilityRecorder {
    private static final int BODY_PREVIEW_LIMIT = 2048;
    private static final ThreadLocal<List<NetworkEvent>> NETWORK = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<List<ConsoleEvent>> CONSOLE = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<List<WarningEvent>> WARNINGS = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<Integer> NEXT_NETWORK_ID = ThreadLocal.withInitial(() -> 0);

    private BrowserObservabilityRecorder() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Starts a network exchange and copies the request body so downstream filters can still read it.
     *
     * @param request browser HTTP request
     * @return exchange handle, or a disabled handle when trace network capture is disabled
     */
    public static NetworkExchange startNetwork(HttpRequest request) {
        if (!isNetworkEnabled() || request == null) {
            return NetworkExchange.disabled();
        }
        int index = NEXT_NETWORK_ID.get() + 1;
        NEXT_NETWORK_ID.set(index);
        byte[] requestBody = copyRequestBody(request);
        return new NetworkExchange(true, "network-" + index, value(request.getMethod().name()),
                value(request.getUri()), headers(request), requestBody.length, System.nanoTime());
    }

    /**
     * Finishes a network exchange and records a redacted trace event.
     *
     * @param exchange      exchange handle returned from {@link #startNetwork(HttpRequest)}
     * @param response      browser HTTP response, or {@code null} on failure
     * @param failureReason safe failure reason
     */
    public static void finishNetwork(NetworkExchange exchange, HttpResponse response, String failureReason) {
        if (exchange == null || !exchange.enabled()) {
            return;
        }
        byte[] responseBody = copyResponseBody(response);
        recordNetwork(new NetworkObservation(
                exchange.method(),
                exchange.url(),
                response == null ? 0 : response.getStatus(),
                exchange.requestHeaders(),
                response == null ? Map.of() : headers(response),
                TimeUnit.NANOSECONDS.toMillis(Math.max(0, System.nanoTime() - exchange.startNanos())),
                exchange.requestSizeBytes(),
                responseBody.length,
                value(failureReason),
                preview(responseBody)));
    }

    /**
     * Records a browser network event for the current test thread.
     *
     * @param observation network exchange details
     */
    public static void recordNetwork(NetworkObservation observation) {
        if (!isNetworkEnabled()) {
            return;
        }
        NETWORK.get().add(new NetworkEvent(
                value(observation.method()),
                value(observation.url()),
                observation.status(),
                sanitizedHeaders(observation.requestHeaders()),
                sanitizedHeaders(observation.responseHeaders()),
                Math.max(0, observation.durationMs()),
                Math.max(0, observation.requestSize()),
                Math.max(0, observation.responseSize()),
                value(observation.failureReason()),
                truncate(value(observation.bodyPreview()))));
    }

    /**
     * Collects Selenium browser logs into the console trace section.
     *
     * @param driver active driver, or {@code null}
     */
    public static void collectConsole(WebDriver driver) {
        if (!isConsoleEnabled()) {
            return;
        }
        if (driver == null) {
            recordWarning("console", "Console capture is unavailable because no active driver is registered.");
            return;
        }
        try {
            var logs = driver.manage().logs();
            Set<String> logTypes = logs.getAvailableLogTypes();
            if (!logTypes.contains("browser")) {
                recordWarning("console", "Browser console logs are not supported by this driver.");
                return;
            }
            List<LogEntry> entries = new ArrayList<>(logs.get("browser").getAll());
            entries.sort(Comparator.comparingLong(LogEntry::getTimestamp));
            for (LogEntry entry : entries) {
                recordConsole("browser", entry.getLevel().getName(), entry.getMessage(), entry.getTimestamp());
            }
        } catch (RuntimeException e) {
            recordWarning("console", "Browser console logs are not supported by this driver.");
        }
    }

    /**
     * Records a browser console event for the current test thread.
     *
     * @param source    log source
     * @param level     log level
     * @param message   log message
     * @param timestamp epoch timestamp in milliseconds
     */
    public static void recordConsole(String source, String level, String message, long timestamp) {
        if (!isConsoleEnabled()) {
            return;
        }
        CONSOLE.get().add(new ConsoleEvent(value(source), value(level), value(message), Math.max(0, timestamp)));
    }

    /**
     * Records safe browser observability metadata for unsupported capabilities.
     *
     * @param source  capability source
     * @param message warning message
     */
    public static void recordWarning(String source, String message) {
        if (!isTraceEnabled()) {
            return;
        }
        WARNINGS.get().add(new WarningEvent(value(source), value(message)));
    }

    /**
     * Returns and clears current-thread warnings.
     *
     * @return recorded warnings
     */
    public static List<String> drainWarnings() {
        List<String> warnings = WARNINGS.get().stream()
                .map(warning -> warning.source() + ": " + warning.message())
                .toList();
        WARNINGS.get().clear();
        return warnings;
    }

    static String drainNetworkJson() {
        String json = networkJson(NETWORK.get());
        NETWORK.get().clear();
        NEXT_NETWORK_ID.set(0);
        return json;
    }

    static String drainConsoleJson() {
        String json = consoleJson(CONSOLE.get());
        CONSOLE.get().clear();
        return json;
    }

    static String drainMetadataJson() {
        String json = metadataJson(WARNINGS.get());
        WARNINGS.get().clear();
        return json;
    }

    /**
     * Clears current-thread browser observability state.
     */
    public static void clear() {
        NETWORK.remove();
        CONSOLE.remove();
        WARNINGS.remove();
        NEXT_NETWORK_ID.remove();
    }

    private static String networkJson(List<NetworkEvent> events) {
        StringBuilder json = new StringBuilder("[");
        for (int i = 0; i < events.size(); i++) {
            NetworkEvent event = events.get(i);
            if (i > 0) {
                json.append(",");
            }
            json.append("\n    {\n");
            field(json, 3, "method", event.method(), true);
            field(json, 3, "url", event.url(), true);
            numberField(json, 3, "status", event.status(), true);
            numberField(json, 3, "durationMs", event.durationMs(), true);
            numberField(json, 3, "requestSizeBytes", event.requestSizeBytes(), true);
            numberField(json, 3, "responseSizeBytes", event.responseSizeBytes(), true);
            map(json, 3, "requestHeaders", event.requestHeaders(), true);
            map(json, 3, "responseHeaders", event.responseHeaders(), true);
            field(json, 3, "failureReason", event.failureReason(), true);
            field(json, 3, "bodyPreview", event.bodyPreview(), false);
            indent(json, 2).append("}");
        }
        if (!events.isEmpty()) {
            json.append("\n  ");
        }
        json.append("]");
        return json.toString();
    }

    private static String consoleJson(List<ConsoleEvent> events) {
        StringBuilder json = new StringBuilder("[");
        for (int i = 0; i < events.size(); i++) {
            ConsoleEvent event = events.get(i);
            if (i > 0) {
                json.append(",");
            }
            json.append("\n    {\n");
            field(json, 3, "source", event.source(), true);
            field(json, 3, "level", event.level(), true);
            field(json, 3, "message", event.message(), true);
            numberField(json, 3, "timestamp", event.timestamp(), false);
            indent(json, 2).append("}");
        }
        if (!events.isEmpty()) {
            json.append("\n  ");
        }
        json.append("]");
        return json.toString();
    }

    private static String metadataJson(List<WarningEvent> warnings) {
        StringBuilder json = new StringBuilder("{\n");
        indent(json, 2).append("\"warnings\": [");
        for (int i = 0; i < warnings.size(); i++) {
            WarningEvent warning = warnings.get(i);
            if (i > 0) {
                json.append(", ");
            }
            json.append("{\"source\": \"")
                    .append(escapeJson(FailureTraceReporter.redact(warning.source())))
                    .append("\", \"message\": \"")
                    .append(escapeJson(FailureTraceReporter.redact(warning.message())))
                    .append("\"}");
        }
        json.append("]\n");
        indent(json, 1).append("}");
        return json.toString();
    }

    private static boolean isNetworkEnabled() {
        return isTraceEnabled() && SHAFT.Properties.reporting.traceIncludeNetwork();
    }

    private static boolean isConsoleEnabled() {
        return isTraceEnabled() && SHAFT.Properties.reporting.traceIncludeConsole();
    }

    private static boolean isTraceEnabled() {
        try {
            return SHAFT.Properties.reporting != null
                    && SHAFT.Properties.reporting.traceEnabled();
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static byte[] copyRequestBody(HttpRequest request) {
        try {
            byte[] body = Contents.bytes(request.getContent());
            request.setContent(Contents.bytes(body));
            return body;
        } catch (RuntimeException e) {
            return new byte[0];
        }
    }

    private static byte[] copyResponseBody(HttpResponse response) {
        if (response == null) {
            return new byte[0];
        }
        try {
            byte[] body = Contents.bytes(response.getContent());
            response.setContent(Contents.bytes(body));
            return body;
        } catch (RuntimeException e) {
            return new byte[0];
        }
    }

    private static Map<String, String> headers(HttpRequest request) {
        Map<String, String> headers = new LinkedHashMap<>();
        request.forEachHeader(headers::put);
        return headers;
    }

    private static Map<String, String> headers(HttpResponse response) {
        Map<String, String> headers = new LinkedHashMap<>();
        response.forEachHeader(headers::put);
        return headers;
    }

    private static Map<String, String> sanitizedHeaders(Map<String, String> source) {
        if (source == null || source.isEmpty()) {
            return Map.of();
        }
        Map<String, String> sanitized = new LinkedHashMap<>();
        source.forEach((key, value) -> sanitized.put(value(key), isSensitiveKey(key)
                ? "********"
                : FailureTraceReporter.redact(value(value))));
        return sanitized;
    }

    private static void map(StringBuilder json, int indent, String key, Map<String, String> values, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": {");
        int index = 0;
        for (Map.Entry<String, String> entry : values.entrySet()) {
            if (index++ > 0) {
                json.append(",");
            }
            json.append("\n");
            indent(json, indent + 1)
                    .append("\"")
                    .append(escapeJson(FailureTraceReporter.redact(entry.getKey())))
                    .append("\": \"")
                    .append(escapeJson(FailureTraceReporter.redact(entry.getValue())))
                    .append("\"");
        }
        if (!values.isEmpty()) {
            json.append("\n");
            indent(json, indent);
        }
        json.append("}").append(comma ? "," : "").append("\n");
    }

    private static void field(StringBuilder json, int indent, String key, String value, boolean comma) {
        indent(json, indent).append("\"")
                .append(escapeJson(FailureTraceReporter.redact(key)))
                .append("\": \"")
                .append(escapeJson(FailureTraceReporter.redact(value)))
                .append("\"")
                .append(comma ? "," : "")
                .append("\n");
    }

    private static void numberField(StringBuilder json, int indent, String key, long value, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": ")
                .append(value)
                .append(comma ? "," : "")
                .append("\n");
    }

    private static boolean isSensitiveKey(String key) {
        String normalized = value(key).toLowerCase(Locale.ROOT);
        return normalized.contains("authorization")
                || normalized.contains("cookie")
                || normalized.contains("password")
                || normalized.contains("passwd")
                || normalized.contains("secret")
                || normalized.contains("token")
                || normalized.contains("api-key")
                || normalized.contains("apikey")
                || normalized.contains("access-key")
                || normalized.contains("accesskey");
    }

    private static String preview(byte[] bytes) {
        if (bytes == null || bytes.length == 0) {
            return "";
        }
        return truncate(new String(bytes, java.nio.charset.StandardCharsets.UTF_8));
    }

    private static String truncate(String value) {
        String safe = value(value);
        return safe.length() <= BODY_PREVIEW_LIMIT ? safe : safe.substring(0, BODY_PREVIEW_LIMIT);
    }

    private static StringBuilder indent(StringBuilder builder, int level) {
        return builder.append("  ".repeat(level));
    }

    private static String escapeJson(String value) {
        return value(value)
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    /**
     * Active network exchange handle.
     */
    public record NetworkExchange(boolean enabled, String id, String method, String url,
                                  Map<String, String> requestHeaders, long requestSizeBytes, long startNanos) {
        static NetworkExchange disabled() {
            return new NetworkExchange(false, "", "", "", Map.of(), 0L, 0L);
        }
    }

    /**
     * Browser network exchange details recorded into the trace.
     */
    public record NetworkObservation(String method, String url, int status, Map<String, String> requestHeaders,
                                     Map<String, String> responseHeaders, long durationMs, long requestSize,
                                     long responseSize, String failureReason, String bodyPreview) {
    }

    private record NetworkEvent(String method, String url, int status, Map<String, String> requestHeaders,
                                Map<String, String> responseHeaders, long durationMs, long requestSizeBytes,
                                long responseSizeBytes, String failureReason, String bodyPreview) {
    }

    private record ConsoleEvent(String source, String level, String message, long timestamp) {
    }

    private record WarningEvent(String source, String message) {
    }
}
