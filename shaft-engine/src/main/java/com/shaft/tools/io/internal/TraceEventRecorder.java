package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Thread-local Selenium trace event recorder used by failure trace artifacts.
 */
public final class TraceEventRecorder {
    private static final ThreadLocal<List<ActionEvent>> EVENTS = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<Integer> NEXT_ID = ThreadLocal.withInitial(() -> 0);

    private TraceEventRecorder() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Starts timing a trace action for the current thread.
     *
     * @param category action category
     * @param name     action name
     * @param locator  action locator
     * @param driver   active WebDriver, or {@code null}
     * @return started event handle, or a disabled handle when tracing is off
     */
    public static Event start(String category, String name, By locator, WebDriver driver) {
        return start(category, name, locator == null ? "" : locator.toString(), driver);
    }

    /**
     * Starts timing a trace action for the current thread.
     *
     * @param category action category
     * @param name     action name
     * @param locator  action locator text
     * @param driver   active WebDriver, or {@code null}
     * @return started event handle, or a disabled handle when tracing is off
     */
    public static Event start(String category, String name, String locator, WebDriver driver) {
        if (!isEnabled()) {
            return Event.disabled();
        }
        int index = NEXT_ID.get() + 1;
        NEXT_ID.set(index);
        return new Event(
                true,
                "action-" + index,
                value(category),
                value(name),
                Instant.now().toString(),
                System.nanoTime(),
                value(locator),
                currentUrl(driver));
    }

    /**
     * Records a finished action without measuring an enclosing block.
     *
     * @param category   action category
     * @param name       action name
     * @param status     action status
     * @param locator    action locator text
     * @param driver     active WebDriver, or {@code null}
     * @param message    action message
     * @param exception  failure exception, or {@code null}
     * @param metadata   action metadata
     * @param attachmentSummaries attachment summaries
     */
    public static void record(String category, String name, String status, String locator, WebDriver driver,
                              String message, Throwable exception, Map<String, String> metadata,
                              List<String> attachmentSummaries) {
        Event event = start(category, name, locator, driver);
        finish(event, status, message, exception, metadata, attachmentSummaries);
    }

    /**
     * Finishes a timed trace action.
     *
     * @param event      event handle returned from {@link #start(String, String, By, WebDriver)}
     * @param status     action status
     * @param message    action message
     * @param exception  failure exception, or {@code null}
     * @param metadata   action metadata
     * @param attachmentSummaries attachment summaries
     */
    public static void finish(Event event, String status, String message, Throwable exception,
                              Map<String, String> metadata, List<String> attachmentSummaries) {
        if (event == null || !event.enabled()) {
            return;
        }
        EVENTS.get().add(new ActionEvent(
                event.id(),
                event.category(),
                event.name(),
                normalizeStatus(status),
                event.startTime(),
                TimeUnit.NANOSECONDS.toMillis(Math.max(0, System.nanoTime() - event.startNanos())),
                event.locator(),
                event.url(),
                value(message),
                exceptionType(exception),
                exceptionMessage(exception),
                attachmentSummaries == null ? List.of() : new ArrayList<>(attachmentSummaries),
                metadata == null ? Map.of() : new LinkedHashMap<>(metadata)));
    }

    /**
     * Returns and clears the current thread's action events.
     *
     * @return recorded action events
     */
    static List<ActionEvent> drain() {
        List<ActionEvent> snapshot = snapshot();
        EVENTS.get().clear();
        NEXT_ID.set(0);
        return snapshot;
    }

    /**
     * Returns a stable copy of the current thread's action events.
     *
     * @return recorded action events
     */
    static List<ActionEvent> snapshot() {
        return List.copyOf(EVENTS.get());
    }

    /**
     * Clears current thread action events.
     */
    public static void clear() {
        EVENTS.remove();
        NEXT_ID.remove();
    }

    static String toJson(List<ActionEvent> events) {
        StringBuilder json = new StringBuilder("[");
        for (int i = 0; i < events.size(); i++) {
            ActionEvent event = events.get(i);
            if (i > 0) {
                json.append(",");
            }
            json.append("\n    {\n");
            field(json, 3, "id", event.id(), true);
            field(json, 3, "category", event.category(), true);
            field(json, 3, "name", event.name(), true);
            field(json, 3, "status", event.status(), true);
            field(json, 3, "startTime", event.startTime(), true);
            numberField(json, 3, "durationMs", event.durationMs(), true);
            field(json, 3, "locator", event.locator(), true);
            field(json, 3, "url", event.url(), true);
            field(json, 3, "message", event.message(), true);
            objectStart(json, 3, "exception");
            field(json, 4, "type", event.exceptionType(), true);
            field(json, 4, "message", event.exceptionMessage(), false);
            objectEnd(json, 3, true);
            stringArray(json, 3, "attachments", event.attachments(), true);
            map(json, 3, "metadata", event.metadata(), false);
            indent(json, 2).append("}");
        }
        if (!events.isEmpty()) {
            json.append("\n  ");
        }
        json.append("]");
        return json.toString();
    }

    private static boolean isEnabled() {
        try {
            return SHAFT.Properties.reporting != null && SHAFT.Properties.reporting.traceEnabled();
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static String currentUrl(WebDriver driver) {
        if (driver == null) {
            return "";
        }
        try {
            return value(driver.getCurrentUrl());
        } catch (RuntimeException e) {
            return "";
        }
    }

    private static String normalizeStatus(String status) {
        String normalized = value(status).toLowerCase(Locale.ROOT).trim();
        return switch (normalized) {
            case "passed", "pass", "success", "successful" -> "passed";
            case "skipped", "skip" -> "skipped";
            default -> "failed";
        };
    }

    private static String exceptionType(Throwable exception) {
        return exception == null ? "" : exception.getClass().getName();
    }

    private static String exceptionMessage(Throwable exception) {
        return exception == null ? "" : value(exception.getMessage());
    }

    private static void field(StringBuilder json, int indent, String key, String value, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": \"")
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

    private static void stringArray(StringBuilder json, int indent, String key, List<String> values, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": [");
        for (int i = 0; i < values.size(); i++) {
            if (i > 0) {
                json.append(", ");
            }
            json.append("\"").append(escapeJson(FailureTraceReporter.redact(values.get(i)))).append("\"");
        }
        json.append("]").append(comma ? "," : "").append("\n");
    }

    private static void map(StringBuilder json, int indent, String key, Map<String, String> values, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": {");
        int index = 0;
        for (Map.Entry<String, String> entry : values.entrySet()) {
            if (index++ > 0) {
                json.append(",");
            }
            json.append("\n");
            String metadataValue = isSensitiveKey(entry.getKey())
                    ? "********"
                    : FailureTraceReporter.redact(entry.getValue());
            indent(json, indent + 1)
                    .append("\"")
                    .append(escapeJson(FailureTraceReporter.redact(entry.getKey())))
                    .append("\": \"")
                    .append(escapeJson(metadataValue))
                    .append("\"");
        }
        if (!values.isEmpty()) {
            json.append("\n");
            indent(json, indent);
        }
        json.append("}").append(comma ? "," : "").append("\n");
    }

    private static boolean isSensitiveKey(String key) {
        String normalized = value(key).toLowerCase(Locale.ROOT);
        return normalized.contains("password")
                || normalized.contains("passwd")
                || normalized.contains("pwd")
                || normalized.contains("secret")
                || normalized.contains("token")
                || normalized.contains("api_key")
                || normalized.contains("apikey")
                || normalized.contains("accesskey")
                || normalized.contains("access_key")
                || normalized.contains("authorization")
                || normalized.contains("cookie");
    }

    private static void objectStart(StringBuilder json, int indent, String key) {
        indent(json, indent).append("\"").append(key).append("\": {\n");
    }

    private static void objectEnd(StringBuilder json, int indent, boolean comma) {
        indent(json, indent).append("}").append(comma ? "," : "").append("\n");
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
     * Active action event handle.
     */
    public record Event(boolean enabled, String id, String category, String name, String startTime, long startNanos,
                        String locator, String url) {
        static Event disabled() {
            return new Event(false, "", "", "", "", 0L, "", "");
        }
    }

    record ActionEvent(String id, String category, String name, String status, String startTime, long durationMs,
                       String locator, String url, String message, String exceptionType, String exceptionMessage,
                       List<String> attachments, Map<String, String> metadata) {
    }
}
