package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.HexFormat;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

/**
 * Thread-local Selenium trace event recorder used by failure trace artifacts.
 */
public final class TraceEventRecorder {
    private static final int MAX_ACTIONS = 10_000;
    private static final ThreadLocal<List<ActionEvent>> EVENTS = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<Map<String, AutomationBackend>> EVENT_BACKENDS =
            ThreadLocal.withInitial(LinkedHashMap::new);
    private static final ThreadLocal<Integer> NEXT_ID = ThreadLocal.withInitial(() -> 0);
    private static final ThreadLocal<Map<String, byte[]>> SCREENSHOTS = ThreadLocal.withInitial(LinkedHashMap::new);
    private static final ThreadLocal<Map<String, ActionSnapshots>> ACTION_SNAPSHOTS =
            ThreadLocal.withInitial(LinkedHashMap::new);
    private static final ThreadLocal<Map<String, String>> ACTION_SNAPSHOT_CONTENT =
            ThreadLocal.withInitial(LinkedHashMap::new);
    private static final ThreadLocal<Long> ACTION_SNAPSHOT_BYTES = ThreadLocal.withInitial(() -> 0L);
    private static final ThreadLocal<Boolean> ACTION_LIMIT_OMITTED = ThreadLocal.withInitial(() -> false);
    private static final ThreadLocal<Integer> ACTIONS_OMITTED = ThreadLocal.withInitial(() -> 0);
    private static final ThreadLocal<Long> SCREENSHOT_BYTES = ThreadLocal.withInitial(() -> 0L);
    private static final ThreadLocal<Integer> SUPPRESSION_DEPTH = ThreadLocal.withInitial(() -> 0);

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
        FailureTraceReporter.activateBrowserEvidenceOwner(driver);
        if (!isEnabled() || SUPPRESSION_DEPTH.get() > 0) {
            return Event.disabled();
        }
        if (NEXT_ID.get() >= MAX_ACTIONS) {
            ACTION_LIMIT_OMITTED.set(true);
            ACTIONS_OMITTED.set(ACTIONS_OMITTED.get() + 1);
            return Event.disabled();
        }
        int index = NEXT_ID.get() + 1;
        NEXT_ID.set(index);
        String id = "action-" + index;
        EVENT_BACKENDS.get().put(id, backend(driver));
        SeleniumTraceCapture.Result beforeSnapshot = domSnapshot(driver);
        if (beforeSnapshot != null) {
            ACTION_SNAPSHOTS.get().put(id, new ActionSnapshots(beforeSnapshot, null));
        }
        return new Event(
                true,
                id,
                value(category),
                value(name),
                Instant.now().toString(),
                System.nanoTime(),
                value(locator),
                currentUrl(driver),
                callerFrame(),
                snapshotContent(beforeSnapshot),
                driver);
    }

    /**
     * Starts a backend-owned action that has no Selenium {@link WebDriver}, such as Playwright.
     * The explicit action-time identity avoids relying on unrelated thread-local session state.
     */
    public static Event startForBackend(String category, String name, String locator, AutomationBackend backend) {
        FailureTraceReporter.activateBrowserEvidenceOwner(null);
        if (!isEnabled() || SUPPRESSION_DEPTH.get() > 0) {
            return Event.disabled();
        }
        if (NEXT_ID.get() >= MAX_ACTIONS) {
            ACTION_LIMIT_OMITTED.set(true);
            ACTIONS_OMITTED.set(ACTIONS_OMITTED.get() + 1);
            return Event.disabled();
        }
        int index = NEXT_ID.get() + 1;
        NEXT_ID.set(index);
        String id = "action-" + index;
        EVENT_BACKENDS.get().put(id, backend == null ? AutomationBackend.UNKNOWN : backend);
        return new Event(true, id, value(category), value(name), Instant.now().toString(),
                System.nanoTime(), value(locator), "", callerFrame(), "", null);
    }

    /** Executes a nested legacy delegate without recording a duplicate event around its owner action. */
    public static <T> T withoutNestedEvents(Supplier<T> action) {
        SUPPRESSION_DEPTH.set(SUPPRESSION_DEPTH.get() + 1);
        try {
            return action.get();
        } finally {
            int remaining = SUPPRESSION_DEPTH.get() - 1;
            if (remaining <= 0) {
                SUPPRESSION_DEPTH.remove();
            } else {
                SUPPRESSION_DEPTH.set(remaining);
            }
        }
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
        SeleniumTraceCapture.Result afterSnapshot = domSnapshot(event.driver());
        if (afterSnapshot != null) {
            ActionSnapshots snapshots = ACTION_SNAPSHOTS.get().get(event.id());
            ACTION_SNAPSHOTS.get().put(event.id(), new ActionSnapshots(
                    snapshots == null ? null : snapshots.before(), afterSnapshot));
        }
        EVENTS.get().add(new ActionEvent(
                event.id(),
                EVENT_BACKENDS.get().getOrDefault(event.id(), AutomationBackend.UNKNOWN),
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
                snapshotContent(afterSnapshot),
                screenshotBase64(event.id())));
        EVENT_BACKENDS.get().remove(event.id());
    }

    /**
     * Returns and clears the current thread's action events.
     *
     * @return recorded action events
     */
    static List<ActionEvent> drain() {
        List<ActionEvent> snapshot = snapshot();
        EVENTS.get().clear();
        EVENT_BACKENDS.get().clear();
        ACTION_LIMIT_OMITTED.remove();
        ACTIONS_OMITTED.remove();
        NEXT_ID.set(0);
        return snapshot;
    }

    static Map<String, ActionSnapshots> drainActionSnapshots() {
        Map<String, ActionSnapshots> snapshots = Map.copyOf(ACTION_SNAPSHOTS.get());
        ACTION_SNAPSHOTS.remove();
        ACTION_SNAPSHOT_CONTENT.remove();
        ACTION_SNAPSHOT_BYTES.remove();
        return snapshots;
    }

    /**
     * Returns a stable copy of the current thread's action events.
     *
     * @return recorded action events
     */
    static List<ActionEvent> snapshot() {
        List<ActionEvent> snapshot = new ArrayList<>(EVENTS.get());
        if (!ACTION_LIMIT_OMITTED.get()) {
            return List.copyOf(snapshot);
        }
        int omittedCount = ACTIONS_OMITTED.get();
        ActionEvent marker = actionLimitMarker(snapshot.isEmpty() ? null : snapshot.getLast(),
                omittedCount + (snapshot.size() >= MAX_ACTIONS ? 1 : 0));
        if (snapshot.size() >= MAX_ACTIONS) {
            snapshot.set(snapshot.size() - 1, marker);
        } else {
            snapshot.add(marker);
        }
        return List.copyOf(snapshot);
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
        clearEvents();
        FailureTraceReporter.clearSensitiveValues();
    }

    static void clearForNewTest() {
        clearEvents();
        FailureTraceReporter.clearInvocationSensitiveValues();
    }

    static void clearPreservingSensitiveValues() {
        clearEvents();
    }

    private static void clearEvents() {
        EVENTS.remove();
        EVENT_BACKENDS.remove();
        NEXT_ID.remove();
        SCREENSHOTS.remove();
        ACTION_SNAPSHOTS.remove();
        ACTION_SNAPSHOT_CONTENT.remove();
        ACTION_SNAPSHOT_BYTES.remove();
        ACTION_LIMIT_OMITTED.remove();
        ACTIONS_OMITTED.remove();
        SCREENSHOT_BYTES.remove();
        SUPPRESSION_DEPTH.remove();
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

    private static AutomationBackend backend(WebDriver driver) {
        try {
            if (driver != null) {
                if (driver instanceof AppiumDriver) {
                    return AutomationBackend.APPIUM;
                }
                return AutomationCapabilityResolver.forWebDriver(driver).backend();
            }
            var playwright = PlaywrightSessionManager.currentSession();
            if (playwright != null) {
                return AutomationCapabilityResolver.forPlaywright(playwright).backend();
            }
            return AutomationCapabilityResolver.forWebDriver(
                    com.shaft.driver.internal.DriverFactory.DriverFactoryHelper.getActiveDriver()).backend();
        } catch (RuntimeException ignored) {
            return AutomationBackend.UNKNOWN;
        }
    }

    /**
     * Best-effort {@code document.documentElement.outerHTML} snapshot for the current thread's
     * active driver, gated by {@code shaft.trace.includeDomSnapshots} and bounded by the shared
     * structural snapshot policy so a single huge page never blows up trace artifact size. Never
     * throws; capture failures degrade to an empty snapshot rather than failing the action being
     * traced.
     */
    private static SeleniumTraceCapture.Result domSnapshot(WebDriver driver) {
        if (driver == null || !isDomSnapshotEnabled()) {
            return null;
        }
        try {
            if (!(driver instanceof org.openqa.selenium.JavascriptExecutor executor)) {
                return new SeleniumTraceCapture.Result("webdriver", "unavailable", "unavailable",
                        "DOM snapshot provider was unavailable.", "unavailable", "", false);
            }
            Object result = executor.executeScript(
                    "return document.documentElement ? document.documentElement.outerHTML : '';");
            String html = result == null ? "" : String.valueOf(result);
            return retainDomSnapshot(SeleniumTraceCapture.fromContent(
                    "webdriver", "structural", "action-dom-snapshot", html,
                    FailureTraceReporter::redactSourceText));
        } catch (RuntimeException e) {
            return new SeleniumTraceCapture.Result("webdriver", "unavailable", "unavailable",
                    "DOM snapshot capture failed.", "unavailable", "", false);
        }
    }

    private static String snapshotContent(SeleniumTraceCapture.Result snapshot) {
        return snapshot == null ? "" : snapshot.content();
    }

    private static SeleniumTraceCapture.Result retainDomSnapshot(SeleniumTraceCapture.Result snapshot) {
        if (snapshot.content().isEmpty()) {
            return snapshot;
        }
        byte[] bytes = snapshot.content().getBytes(StandardCharsets.UTF_8);
        String digest = sha256(bytes);
        String canonical = ACTION_SNAPSHOT_CONTENT.get().get(digest);
        if (canonical != null) {
            return new SeleniumTraceCapture.Result(snapshot.provider(), snapshot.fidelity(), snapshot.status(),
                    snapshot.reason(), snapshot.type(), canonical, snapshot.truncated());
        }
        long used = ACTION_SNAPSHOT_BYTES.get();
        long budget = actionSnapshotBudgetBytes();
        if (used > budget || bytes.length > budget - used) {
            return new SeleniumTraceCapture.Result(snapshot.provider(), "omitted", "omitted-budget",
                    "Action DOM snapshot exceeded the cumulative trace capture budget.",
                    "omitted-budget", "", false);
        }
        ACTION_SNAPSHOT_CONTENT.get().put(digest, snapshot.content());
        ACTION_SNAPSHOT_BYTES.set(used + bytes.length);
        return snapshot;
    }

    private static long actionSnapshotBudgetBytes() {
        try {
            long mebibytes = SHAFT.Properties.reporting == null
                    ? Long.MAX_VALUE / (1024L * 1024L)
                    : Math.max(1L, SHAFT.Properties.reporting.traceMaxArtifactMb());
            return Math.multiplyExact(mebibytes, 1024L * 1024L);
        } catch (RuntimeException exception) {
            return 1024L * 1024L;
        }
    }

    private static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", exception);
        }
    }

    private static ActionEvent actionLimitMarker(ActionEvent lastRetained, int omittedCount) {
        return new ActionEvent("action-limit", AutomationBackend.UNKNOWN, "trace", "omitted-actions", "skipped",
                lastRetained == null ? Instant.EPOCH.toString() : lastRetained.startTime(), 0L,
                "", "", "", "Action evidence exceeded the 10000-action limit.",
                "", "", List.of(), Map.of("omitted", "newest-tail",
                        "omittedCount", String.valueOf(omittedCount)), Map.of(), "", "", "");
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
        return exception == null ? "" : FailureTraceReporter.redactThrowableText(
                exception, value(exception.getMessage()));
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
        return JsonEscapes.escape(value);
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

    record ActionEvent(String id, AutomationBackend backend, String category, String name, String status,
                       String startTime, long durationMs,
                       String locator, String url, String caller, String message, String exceptionType,
                       String exceptionMessage, List<String> attachments, Map<String, String> metadata,
                       Map<String, Object> actionability, String domSnapshotBefore, String domSnapshotAfter,
                       String screenshot) {
    }

    record ActionSnapshots(SeleniumTraceCapture.Result before, SeleniumTraceCapture.Result after) {
    }
}
