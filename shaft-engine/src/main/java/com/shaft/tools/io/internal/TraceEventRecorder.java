package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
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
    private static final ThreadLocal<Map<String, byte[]>> SCREENSHOTS = ThreadLocal.withInitial(LinkedHashMap::new);
    private static final ThreadLocal<Long> SCREENSHOT_BYTES = ThreadLocal.withInitial(() -> 0L);
    private static final int MAX_DOM_SNAPSHOT_CHARACTERS = 200_000;

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
                currentUrl(driver),
                callerFrame(),
                domSnapshot(driver),
                driver);
    }

    /**
     * Best-effort first non-framework stack frame (the user's test or page-object line) so each
     * traced action can be tied back to the exact source line that triggered it. Blank when every
     * frame belongs to SHAFT, the JDK, or the test runner (e.g. SHAFT's own unit tests).
     */
    private static String callerFrame() {
        for (StackTraceElement frame : Thread.currentThread().getStackTrace()) {
            String className = frame.getClassName();
            if (!className.startsWith("com.shaft.")
                    && !className.startsWith("java.")
                    && !className.startsWith("jdk.")
                    && !className.startsWith("sun.")
                    && !className.startsWith("org.testng.")
                    && !className.startsWith("org.junit.")
                    && !className.startsWith("io.qameta.")
                    && !className.startsWith("org.apache.maven.")
                    && !className.startsWith("org.gradle.")) {
                return frame.toString();
            }
        }
        return "";
    }

    /**
     * Buffers a screenshot for the given action, keyed by {@link Event#id()}, so {@link #finish}
     * can embed it in the action's trace JSON. Gated by {@code shaft.trace.includeScreenshots} and
     * bounded by {@code shaft.trace.maxArtifactMb} so buffered PNGs can never grow unbounded within
     * a single thread's trace. Never throws.
     *
     * @param event started event handle from {@link #start}
     * @param png   screenshot bytes, or {@code null}
     */
    public static void recordScreenshot(Event event, byte[] png) {
        if (event == null || !event.enabled() || png == null || png.length == 0 || !isScreenshotEnabled()) {
            return;
        }
        long used = SCREENSHOT_BYTES.get();
        if (used + png.length > screenshotBudgetBytes()) {
            return;
        }
        SCREENSHOTS.get().put(event.id(), png);
        SCREENSHOT_BYTES.set(used + png.length);
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
        record(category, name, status, locator, driver, message, exception, metadata, attachmentSummaries, Map.of());
    }

    /**
     * Records a finished action with actionability diagnostics without measuring an enclosing block.
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
     * @param actionability actionability diagnostics
     */
    public static void record(String category, String name, String status, String locator, WebDriver driver,
                              String message, Throwable exception, Map<String, String> metadata,
                              List<String> attachmentSummaries, Map<String, Object> actionability) {
        Event event = start(category, name, locator, driver);
        finish(event, status, message, exception, metadata, attachmentSummaries, actionability);
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
        finish(event, status, message, exception, metadata, attachmentSummaries, Map.of());
    }

    /**
     * Finishes a timed trace action with actionability diagnostics.
     *
     * @param event      event handle returned from {@link #start(String, String, By, WebDriver)}
     * @param status     action status
     * @param message    action message
     * @param exception  failure exception, or {@code null}
     * @param metadata   action metadata
     * @param attachmentSummaries attachment summaries
     * @param actionability actionability diagnostics
     */
    public static void finish(Event event, String status, String message, Throwable exception,
                              Map<String, String> metadata, List<String> attachmentSummaries,
                              Map<String, Object> actionability) {
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
                event.caller(),
                value(message),
                exceptionType(exception),
                exceptionMessage(exception),
                attachmentSummaries == null ? List.of() : new ArrayList<>(attachmentSummaries),
                metadata == null ? Map.of() : new LinkedHashMap<>(metadata),
                actionability == null ? Map.of() : new LinkedHashMap<>(actionability),
                event.domSnapshotBefore(),
                domSnapshot(event.driver()),
                screenshotBase64(event.id())));
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
     * Returns the latest finished action id for linking follow-up artifacts.
     *
     * @return latest action id, or blank when no action is recorded
     */
    public static String latestActionId() {
        List<ActionEvent> events = EVENTS.get();
        return events.isEmpty() ? "" : events.getLast().id();
    }

    /**
     * Clears current thread action events.
     */
    public static void clear() {
        EVENTS.remove();
        NEXT_ID.remove();
        SCREENSHOTS.remove();
        SCREENSHOT_BYTES.remove();
    }

    /**
     * Consumes the buffered screenshot for the given action id, base64-encoded for JSON
     * embedding. Removes the entry from the buffer once consumed so screenshots never outlive
     * the action they belong to.
     */
    private static String screenshotBase64(String id) {
        byte[] png = SCREENSHOTS.get().remove(id);
        return png == null ? "" : Base64.getEncoder().encodeToString(png);
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
            field(json, 3, "caller", event.caller(), true);
            field(json, 3, "message", event.message(), true);
            objectStart(json, 3, "exception");
            field(json, 4, "type", event.exceptionType(), true);
            field(json, 4, "message", event.exceptionMessage(), false);
            objectEnd(json, 3, true);
            stringArray(json, 3, "attachments", event.attachments(), true);
            boolean hasActionability = !event.actionability().isEmpty();
            boolean hasDomSnapshots = !event.domSnapshotBefore().isEmpty() || !event.domSnapshotAfter().isEmpty();
            boolean hasScreenshot = !event.screenshot().isEmpty();
            map(json, 3, "metadata", event.metadata(), hasActionability || hasDomSnapshots || hasScreenshot);
            if (hasActionability) {
                objectMap(json, 3, "actionability", event.actionability(), hasDomSnapshots || hasScreenshot);
            }
            if (hasDomSnapshots) {
                field(json, 3, "domSnapshotBefore", event.domSnapshotBefore(), true);
                field(json, 3, "domSnapshotAfter", event.domSnapshotAfter(), hasScreenshot);
            }
            if (hasScreenshot) {
                field(json, 3, "screenshot", event.screenshot(), false);
            }
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

    /**
     * Best-effort {@code document.documentElement.outerHTML} snapshot for the current thread's
     * active driver, gated by {@code shaft.trace.includeDomSnapshots} and bounded to
     * {@link #MAX_DOM_SNAPSHOT_CHARACTERS} so a single huge page never blows up trace artifact
     * size. Never throws; capture failures degrade to an empty snapshot rather than failing the
     * action being traced.
     */
    private static String domSnapshot(WebDriver driver) {
        if (driver == null || !isDomSnapshotEnabled()) {
            return "";
        }
        try {
            if (!(driver instanceof org.openqa.selenium.JavascriptExecutor executor)) {
                return "";
            }
            Object result = executor.executeScript(
                    "return document.documentElement ? document.documentElement.outerHTML : '';");
            String html = result == null ? "" : String.valueOf(result);
            return html.length() > MAX_DOM_SNAPSHOT_CHARACTERS
                    ? html.substring(0, MAX_DOM_SNAPSHOT_CHARACTERS)
                    : html;
        } catch (RuntimeException e) {
            return "";
        }
    }

    private static boolean isDomSnapshotEnabled() {
        try {
            return SHAFT.Properties.reporting != null && SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static boolean isScreenshotEnabled() {
        try {
            return SHAFT.Properties.reporting != null && SHAFT.Properties.reporting.traceIncludeScreenshots();
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static long screenshotBudgetBytes() {
        try {
            return SHAFT.Properties.reporting == null
                    ? Long.MAX_VALUE
                    : (long) SHAFT.Properties.reporting.traceMaxArtifactMb() * 1024L * 1024L;
        } catch (RuntimeException e) {
            return Long.MAX_VALUE;
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

    private static void objectMap(StringBuilder json, int indent, String key, Map<String, Object> values, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": ");
        writeObject(json, indent, values);
        json.append(comma ? "," : "").append("\n");
    }

    private static void writeObject(StringBuilder json, int indent, Map<?, ?> values) {
        json.append("{");
        int index = 0;
        for (Map.Entry<?, ?> entry : values.entrySet()) {
            if (index++ > 0) {
                json.append(",");
            }
            json.append("\n");
            String key = value(String.valueOf(entry.getKey()));
            indent(json, indent + 1)
                    .append("\"")
                    .append(escapeJson(FailureTraceReporter.redact(key)))
                    .append("\": ");
            writeValue(json, indent + 1, key, entry.getValue());
        }
        if (!values.isEmpty()) {
            json.append("\n");
            indent(json, indent);
        }
        json.append("}");
    }

    private static void writeArray(StringBuilder json, int indent, Iterable<?> values) {
        json.append("[");
        int index = 0;
        for (Object item : values) {
            if (index++ > 0) {
                json.append(", ");
            }
            writeValue(json, indent, "", item);
        }
        json.append("]");
    }

    private static void writeValue(StringBuilder json, int indent, String key, Object item) {
        if (isSensitiveKey(key)) {
            json.append("\"********\"");
        } else if (item instanceof Number || item instanceof Boolean) {
            json.append(item);
        } else if (item instanceof Map<?, ?> nestedMap) {
            writeObject(json, indent, nestedMap);
        } else if (item instanceof Iterable<?> nestedList) {
            writeArray(json, indent, nestedList);
        } else {
            json.append("\"")
                    .append(escapeJson(FailureTraceReporter.redact(value(item == null ? "" : String.valueOf(item)))))
                    .append("\"");
        }
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
     * Active action event handle. {@code driver} is retained only to capture the "after" DOM
     * snapshot at {@link #finish} time; it is never serialized or exposed outside this class.
     */
    public record Event(boolean enabled, String id, String category, String name, String startTime, long startNanos,
                        String locator, String url, String caller, String domSnapshotBefore, WebDriver driver) {
        static Event disabled() {
            return new Event(false, "", "", "", "", 0L, "", "", "", "", null);
        }
    }

    record ActionEvent(String id, String category, String name, String status, String startTime, long durationMs,
                       String locator, String url, String caller, String message, String exceptionType,
                       String exceptionMessage, List<String> attachments, Map<String, String> metadata,
                       Map<String, Object> actionability, String domSnapshotBefore, String domSnapshotAfter,
                       String screenshot) {
    }
}
