package com.shaft.tools.io.internal;

import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.trace.TraceSession;
import com.shaft.tools.io.trace.TraceArtifactReference;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.WebDriver;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Base64;
import java.util.Comparator;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Builds the failure-scoped SHAFT trace viewer artifacts attached to Allure.
 */
public final class FailureTraceReporter {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Pattern AUTHORIZATION_PATTERN = Pattern.compile("(?i)(authorization\\s*[:=]\\s*)(bearer\\s+)?[^\\s,;]+");
    private static final Pattern COOKIE_PATTERN = Pattern.compile("(?i)(cookie|set-cookie)(\\s*[:=]\\s*)[^\\n\\r]+");
    private static final Pattern URL_CREDENTIAL_PATTERN = Pattern.compile("(?i)(://[^:/\\s]+:)[^@/\\s]+(@)");
    private static final Pattern SECRET_ASSIGNMENT_PATTERN = Pattern.compile(
            "(?i)(password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)(\\s*[:=]\\s*)[^\\s,;&\"'<>()\\[\\]{}]+");
    private static final Pattern SECRET_ATTRIBUTE_PATTERN = Pattern.compile(
            "(?i)((?:password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)\\s*=\\s*[\"'])[^\"']*([\"'])");
    private static final Pattern SECRET_JSON_PATTERN = Pattern.compile(
            "(?i)(\"(?:password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)\"\\s*:\\s*\")[^\"]*(\")");
    private static final Pattern NUMERIC_TOKEN_PATTERN = Pattern.compile(
            "(?<![\\d.+\\-])([+\\-]?(?:\\d++(?:\\.\\d*+)?|\\.\\d++)(?:[eE][+\\-]?\\d++)?)"
                    + "(?![\\d.]|[eE][+\\-]?\\d)");
    private static final int SNIPPET_RADIUS = 2;
    private static final int MAX_SOURCE_FILE_CHARACTERS = 100_000;
    private static final ThreadLocal<String> CURRENT_NETWORK_JSON = ThreadLocal.withInitial(() -> "[]");
    private static final ThreadLocal<Map<String, byte[]>> CURRENT_SCREENSHOTS = ThreadLocal.withInitial(Map::of);
    private static final ThreadLocal<TraceArtifactManifest> CURRENT_ARTIFACT_MANIFEST = new ThreadLocal<>();
    private static final ThreadLocal<LinkedHashSet<String>> EXACT_SENSITIVE_VALUES =
            ThreadLocal.withInitial(LinkedHashSet::new);
    private static final ThreadLocal<LinkedHashSet<String>> SOURCE_SENSITIVE_VALUES =
            ThreadLocal.withInitial(LinkedHashSet::new);
    private static final ThreadLocal<Set<Throwable>> SENSITIVE_THROWABLES =
            ThreadLocal.withInitial(() -> Collections.newSetFromMap(new IdentityHashMap<>()));
    private static final ThreadLocal<Boolean> SUPPRESS_SENSITIVE_BROWSER_ARTIFACTS =
            ThreadLocal.withInitial(() -> false);
    private static final ThreadLocal<Boolean> SENSITIVE_VALUE_OVERFLOW = ThreadLocal.withInitial(() -> false);
    private static final ThreadLocal<SensitiveBrowserSessionRegistry> PERSISTENT_BROWSER_SENSITIVITY =
            ThreadLocal.withInitial(SensitiveBrowserSessionRegistry::new);
    private static final int SENSITIVE_VALUE_TRAVERSAL_LIMIT = 1000;
    private static final int SENSITIVE_VALUE_DEPTH_LIMIT = 20;
    private static final int SENSITIVE_VALUE_LIMIT = 128;
    private static final int SENSITIVE_VALUE_LENGTH_LIMIT = 512;
    private static final int NUMERIC_TOKEN_LIMIT = 256;
    private static final int NUMERIC_TOKEN_LENGTH_LIMIT = 128;
    private static final int MAX_PLAYWRIGHT_EVIDENCE_BYTES = 512 * 1024;
    private static final int MAX_PLAYWRIGHT_SNAPSHOT_BYTES = 128 * 1024;
    private static final int MAX_PLAYWRIGHT_SNAPSHOTS = 16;
    private static final String SENSITIVE_BOUNDS_OMISSION =
            "[evidence omitted because sensitive-value bounds were exceeded]";
    private static final ConcurrentMap<String, AtomicInteger> ATTEMPT_COUNTERS = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, List<AttemptRecord>> ATTEMPT_HISTORY = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, Object> TRACE_LOCKS = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, Integer> LATEST_PUBLISHED_ATTEMPT = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, TraceIndexSnapshot> LATEST_INDEX = new ConcurrentHashMap<>();

    private FailureTraceReporter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Attaches the trace ZIP bundle when the current trace mode applies.
     *
     * @param info        current test metadata
     * @param logText     current test log
     * @param attachments generated artifact file paths already known to SHAFT
     */
    public static void attachOnFailure(TestExecutionInfo info, String logText, List<String> attachments) {
        if (!shouldAttachTrace(info)) {
            return;
        }
        Path completedArchive = null;
        try {
            stopPlaywrightTraceIfRunning();
            String testId = safeTestId(info);
            int attempt = ATTEMPT_COUNTERS.computeIfAbsent(testId, id -> new AtomicInteger()).incrementAndGet();
            String json = renderTraceJson(info, logText, attachments, attempt);
            Map<String, byte[]> screenshots = CURRENT_SCREENSHOTS.get();
            List<String> omitted = omittedEntries(json, CURRENT_ARTIFACT_MANIFEST.get());
            // In-report trace launcher (issue #3534 P2): the viewer HTML is fully self-contained --
            // it embeds the trace JSON (with base64 screenshots and inline DOM snapshots) and reads
            // everything from that embedded data, referencing no sibling files -- so attach it
            // directly for a one-click, in-report open, alongside the zip kept for full-fidelity
            // offline download.
            completedArchive = completedArchivePath(info, attempt);
            long maxBytes = configuredMaxArtifactBytes();
            TraceArchiveBundle bundle = convergeTraceArchive(completedArchive, json, CURRENT_NETWORK_JSON.get(),
                    screenshots, CURRENT_ARTIFACT_MANIFEST.get(), maxBytes, Math.multiplyExact((long) maxBytes, 4L),
                    aggregateOmissionMarker(), omitted);
            json = bundle.json();
            String html = bundle.html();
            omitted = bundle.omitted();
            attach("html", "shaft-trace.html", html.getBytes(StandardCharsets.UTF_8), traceViewerLabel(info, attempt));
            if (persistTraceArtifacts(info, completedArchive, screenshots, attempt, omitted)) {
                AttachmentReporter.attachBasedOnFileType("zip", "shaft-trace.zip", completedArchive,
                        traceAttachmentLabel(info, attempt));
            }
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete("Could not attach SHAFT trace report: " + e.getMessage(), Level.WARN);
        } finally {
            if (completedArchive != null) {
                try {
                    Files.deleteIfExists(completedArchive);
                } catch (IOException e) {
                    ReportManagerHelper.logDiscrete("Could not remove temporary SHAFT trace archive: " + e.getMessage(),
                            Level.WARN);
                }
            }
            CURRENT_NETWORK_JSON.remove();
            CURRENT_SCREENSHOTS.remove();
            closeArtifactManifest();
        }
    }

    private static void stopPlaywrightTraceIfRunning() {
        try {
            var session = PlaywrightSessionManager.currentSession();
            if (session != null && session.traceManager() != null && session.traceManager().isTracingStarted()) {
                session.traceManager().stop();
            }
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete("Could not stop Playwright tracing before SHAFT trace generation: "
                    + e.getMessage(), Level.WARN);
        }
    }

    static String renderTraceJson(TestExecutionInfo info, String logText, List<String> attachments) {
        return renderTraceJson(info, logText, attachments, 1);
    }

    static String renderTraceJson(TestExecutionInfo info, String logText, List<String> attachments, int attempt) {
        Throwable throwable = info == null ? null : info.throwable();
        SourceContext source = sourceContext(info);
        boolean suppressBrowserArtifacts = shouldOmitSensitiveBrowserEvidence()
                || containsSensitiveThrowable(throwable);
        Snapshot snapshot = suppressBrowserArtifacts
                ? new Snapshot("none", "omitted", "omitted-sensitive",
                "Browser snapshot was omitted at the sensitive-data boundary.", "omitted-sensitive", "", 0, false)
                : snapshot();
        List<TraceEventRecorder.ActionEvent> actions = TraceEventRecorder.drain();
        Map<String, TraceEventRecorder.ActionSnapshots> actionSnapshots = TraceEventRecorder.drainActionSnapshots();
        actions = actions.stream().map(FailureTraceReporter::resanitizeActionDom).toList();
        actionSnapshots = resanitizeActionSnapshots(actionSnapshots);
        Path nativeTrace = suppressBrowserArtifacts ? null : PlaywrightTraceManager.getLastTracePath();
        if (suppressBrowserArtifacts) {
            actions = actions.stream().map(FailureTraceReporter::withoutBrowserEvidence).toList();
            actionSnapshots = Map.of();
        }
        CURRENT_SCREENSHOTS.set(decodeScreenshots(actions));
        if (suppressBrowserArtifacts) {
            BrowserObservabilityRecorder.clear();
        } else {
            BrowserObservabilityRecorder.collectConsole(DriverFactoryHelper.getActiveDriver());
        }
        String observabilityJson = suppressBrowserArtifacts
                ? "{\"warnings\": []}" : BrowserObservabilityRecorder.drainMetadataJson();
        String networkJson = suppressBrowserArtifacts ? "[]" : BrowserObservabilityRecorder.drainNetworkJson();
        CURRENT_NETWORK_JSON.set(networkJson);
        String consoleJson = suppressBrowserArtifacts ? "[]" : BrowserObservabilityRecorder.drainConsoleJson();
        closeArtifactManifest();
        long maxBytes = configuredMaxArtifactBytes();
        String omissionMarker = "Omitted because artifact exceeded shaft.trace.maxArtifactMb="
                + SHAFT.Properties.reporting.traceMaxArtifactMb();
        TraceArtifactManifest manifest = TraceArtifactManifest.create(networkJson, CURRENT_SCREENSHOTS.get(),
                snapshotResources(actions, actionSnapshots), nativeTrace, maxBytes, omissionMarker);
        CURRENT_ARTIFACT_MANIFEST.set(manifest);
        PlaywrightEvidence playwrightEvidence = importPlaywrightEvidence(actions, manifest.stagedNativeTrace(),
                nativeTrace != null, suppressBrowserArtifacts);
        actions = playwrightEvidence.actions();
        Set<String> omittedScreenshotIds = manifest.references().stream()
                .filter(TraceArtifactReference::omitted)
                .filter(reference -> "screenshot".equals(reference.kind()))
                .map(reference -> reference.id().replaceFirst("^screenshot-", ""))
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        if (!omittedScreenshotIds.isEmpty()) {
            actions = actions.stream().map(action -> omittedScreenshotIds.contains(action.id())
                    ? withoutScreenshot(action) : action).toList();
        }
        TraceSession traceSession = TraceSchemaSerializer.create(safeTestId(info), attempt, actions,
                manifest.references());
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        field(json, 1, "schemaVersion", "3.0", true);
        field(json, 1, "generatedAt", traceSession.generatedAt().toString(), true);
        rawObject(json, 1, "session", TraceSchemaSerializer.toJson(traceSession), true);
        appendTestObject(json, info, throwable, attempt);
        objectStart(json, 1, "environment");
        field(json, 2, "shaftVersion", safeProperty(() -> SHAFT.Properties.internal.shaftEngineVersion()), true);
        field(json, 2, "os", System.getProperty("os.name", ""), true);
        field(json, 2, "osVersion", System.getProperty("os.version", ""), true);
        field(json, 2, "javaVersion", System.getProperty("java.version", ""), true);
        field(json, 2, "targetPlatform", safeProperty(() -> SHAFT.Properties.platform.targetPlatform()), true);
        field(json, 2, "browser", reportedBrowser(), true);
        field(json, 2, "executionAddress", safeProperty(() -> SHAFT.Properties.platform.executionAddress()), true);
        field(json, 2, "headless", safeProperty(() -> String.valueOf(SHAFT.Properties.web.headlessExecution())), true);
        field(json, 2, "thread", Thread.currentThread().getName(), false);
        objectEnd(json, 1, true);
        appendExceptionObject(json, throwable);
        objectStart(json, 1, "source");
        field(json, 2, "frame", source.frame(), true);
        field(json, 2, "file", source.file(), true);
        field(json, 2, "line", source.line(), true);
        field(json, 2, "snippet", source.snippet(), true);
        field(json, 2, "fileContent", source.fileContent(), false);
        objectEnd(json, 1, true);
        objectStart(json, 1, "snapshot");
        field(json, 2, "provider", snapshot.provider(), true);
        field(json, 2, "fidelity", snapshot.fidelity(), true);
        field(json, 2, "status", snapshot.status(), true);
        field(json, 2, "reason", snapshot.reason(), true);
        field(json, 2, "type", snapshot.type(), true);
        field(json, 2, "content", snapshot.content(), true);
        field(json, 2, "byteCount", String.valueOf(snapshot.byteCount()), true);
        field(json, 2, "truncated", String.valueOf(snapshot.truncated()), false);
        objectEnd(json, 1, true);
        rawObject(json, 1, "locatorHealth", locatorHealthJson(), true);
        objectStart(json, 1, "evidence");
        rawObject(json, 2, "browserObservability", observabilityJson, true);
        rawArray(json, 2, "network", networkJson, true);
        rawArray(json, 2, "console", consoleJson, true);
        rawObject(json, 2, "playwright", playwrightEvidence.json(), true);
        rawArray(json, 2, "actions", TraceEventRecorder.toJson(actions), false);
        objectEnd(json, 1, true);
        array(json, 1, "timeline", timeline(throwable, logText), true);
        array(json, 1, "attachments", attachmentEntries(attachments), false);
        json.append("}\n");
        return json.toString();
    }

    private static PlaywrightEvidence importPlaywrightEvidence(List<TraceEventRecorder.ActionEvent> actions,
                                                                Path nativeTrace,
                                                                boolean nativeTraceAdvertised,
                                                                boolean suppressBrowserArtifacts) {
        if (suppressBrowserArtifacts) {
            return new PlaywrightEvidence(actions, playwrightEvidenceJson("suppressed-sensitive", ""));
        }
        if (nativeTrace == null) {
            String reason = nativeTraceAdvertised ? "Playwright native trace was unavailable for import." : "";
            return new PlaywrightEvidence(actions, playwrightEvidenceJson("unavailable", reason));
        }
        try {
            PlaywrightTraceArchiveLoader.LoadedArchive loaded = PlaywrightTraceArchiveLoader.load(nativeTrace);
            PlaywrightTraceImporter.ImportedTrace imported = PlaywrightTraceImporter.importTrace(loaded, actions);
            String json = availablePlaywrightEvidenceJson(imported, loaded);
            if (json == null) {
                return new PlaywrightEvidence(actions, playwrightEvidenceJson("omitted-budget",
                        "Playwright action evidence exceeded its bounded report budget."));
            }
            return new PlaywrightEvidence(imported.correlatedActions(), json);
        } catch (PlaywrightTraceImporter.UnsupportedTraceVersionException exception) {
            return new PlaywrightEvidence(actions,
                    playwrightEvidenceJson("unsupported", "Playwright native trace version is unsupported."));
        } catch (IOException | RuntimeException exception) {
            return new PlaywrightEvidence(actions,
                    playwrightEvidenceJson("malformed", "Playwright native trace is malformed."));
        }
    }

    private static String playwrightEvidenceJson(String status, String reason) {
        ObjectNode root = JSON.createObjectNode();
        root.put("status", status);
        root.put("reason", reason);
        root.putArray("actions");
        root.putArray("correlations");
        return JSON.writeValueAsString(root);
    }

    private static String availablePlaywrightEvidenceJson(PlaywrightTraceImporter.ImportedTrace imported,
                                                           PlaywrightTraceArchiveLoader.LoadedArchive archive) {
        BoundedJson json = new BoundedJson(MAX_PLAYWRIGHT_EVIDENCE_BYTES);
        if (!json.append("{\"status\":\"available\",\"reason\":\"\",\"actions\":[")) {
            return null;
        }
        for (int index = 0; index < imported.actions().size(); index++) {
            PlaywrightTraceImporter.NativeAction action = imported.actions().get(index);
            ObjectNode node = JSON.createObjectNode();
            node.put("callId", action.callId());
            node.put("stepId", action.stepId());
            node.put("className", action.className());
            node.put("method", action.method());
            node.put("title", action.title());
            node.put("startEpochMillis", action.startEpochMillis());
            node.put("endEpochMillis", action.endEpochMillis());
            node.put("beforeSnapshot", action.beforeSnapshot());
            node.put("inputSnapshot", action.inputSnapshot());
            node.put("afterSnapshot", action.afterSnapshot());
            node.put("pageId", action.pageId());
            node.put("source", action.source());
            boolean sourceAvailable = !action.source().isBlank();
            node.put("sourceStatus", sourceAvailable ? "available" : "unavailable");
            node.put("sourceReason", sourceAvailable ? ""
                    : "The native Playwright trace did not provide a source stack for this action.");
            var logs = node.putArray("logs");
            action.logs().forEach(logs::add);
            node.put("error", action.error());
            if (!json.append((index == 0 ? "" : ",") + JSON.writeValueAsString(node))) {
                return null;
            }
        }
        if (!json.append("],\"correlations\":[")) {
            return null;
        }
        for (int index = 0; index < imported.correlations().size(); index++) {
            PlaywrightTraceImporter.Correlation correlation = imported.correlations().get(index);
            ObjectNode node = JSON.createObjectNode();
            node.put("shaftActionId", correlation.shaftActionId());
            node.put("playwrightCallId", correlation.playwrightCallId());
            node.put("basis", correlation.basis());
            if (!json.append((index == 0 ? "" : ",") + JSON.writeValueAsString(node))) {
                return null;
            }
        }
        List<String> allSnapshotNames = imported.actions().stream()
                .flatMap(action -> java.util.stream.Stream.of(
                        action.beforeSnapshot(), action.inputSnapshot(), action.afterSnapshot()))
                .filter(name -> name != null && !name.isBlank())
                .distinct()
                .toList();
        List<String> snapshotNames = allSnapshotNames.stream().limit(MAX_PLAYWRIGHT_SNAPSHOTS).toList();
        int omittedSnapshotCount = allSnapshotNames.size() - snapshotNames.size();
        if (!json.append("],\"snapshotOmission\":{\"status\":\""
                + (omittedSnapshotCount > 0 ? "omitted-budget" : "available")
                + "\",\"omittedCount\":" + omittedSnapshotCount + "},\"snapshots\":{")) {
            return null;
        }
        Map<String, PlaywrightTraceOfflineAdapter.SnapshotDocument> documents;
        try {
            documents = PlaywrightTraceOfflineAdapter.snapshotDocuments(
                    archive, snapshotNames, MAX_PLAYWRIGHT_SNAPSHOT_BYTES);
        } catch (IllegalArgumentException exception) {
            documents = Map.of();
        }
        for (int index = 0; index < snapshotNames.size(); index++) {
            String snapshotName = snapshotNames.get(index);
            ObjectNode snapshot = JSON.createObjectNode();
            PlaywrightTraceOfflineAdapter.SnapshotDocument document = documents.get(snapshotName);
            if (document == null || !"available".equals(document.status())) {
                snapshot.put("status", document == null ? "unavailable" : document.status());
                snapshot.put("fidelity", "omitted");
                snapshot.put("content", "");
            } else {
                snapshot.put("status", "available");
                snapshot.put("fidelity", "native-offline");
                snapshot.put("content", document.content());
            }
            String property = (index == 0 ? "" : ",") + JSON.writeValueAsString(snapshotName)
                    + ":" + JSON.writeValueAsString(snapshot);
            if (!json.append(property)) {
                ObjectNode omitted = JSON.createObjectNode();
                omitted.put("status", "omitted-budget");
                omitted.put("fidelity", "omitted");
                omitted.put("content", "");
                String fallback = (index == 0 ? "" : ",") + JSON.writeValueAsString(snapshotName)
                        + ":" + JSON.writeValueAsString(omitted);
                if (!json.append(fallback)) {
                    return null;
                }
            }
        }
        return json.append("}}") ? json.toString() : null;
    }

    private static final class BoundedJson {
        private final int maximumBytes;
        private final StringBuilder value = new StringBuilder();
        private int bytes;

        private BoundedJson(int maximumBytes) {
            this.maximumBytes = maximumBytes;
        }

        private boolean append(String fragment) {
            int fragmentBytes = fragment.getBytes(StandardCharsets.UTF_8).length;
            if (fragmentBytes > maximumBytes - bytes) {
                return false;
            }
            value.append(fragment);
            bytes += fragmentBytes;
            return true;
        }

        @Override
        public String toString() {
            return value.toString();
        }
    }

    private record PlaywrightEvidence(List<TraceEventRecorder.ActionEvent> actions, String json) {
        private PlaywrightEvidence {
            actions = List.copyOf(actions);
        }
    }

    private static TraceEventRecorder.ActionEvent withoutBrowserEvidence(TraceEventRecorder.ActionEvent action) {
        Map<String, String> safeMetadata = new LinkedHashMap<>();
        action.metadata().forEach((key, value) -> safeMetadata.put(key, redactSourceText(value)));
        return new TraceEventRecorder.ActionEvent(action.id(), action.backend(), action.category(), action.name(),
                action.status(), action.startTime(), action.durationMs(), redactSourceText(action.locator()),
                redactSourceText(action.url()), action.caller(), redactSourceText(action.message()),
                action.exceptionType(), redactSourceText(action.exceptionMessage()),
                action.attachments().stream().map(FailureTraceReporter::redactSourceText).toList(),
                safeMetadata, Map.of(), "", "", "");
    }

    private static void appendTestObject(StringBuilder json, TestExecutionInfo info, Throwable throwable, int attempt) {
        objectStart(json, 1, "test");
        field(json, 2, "className", value(info == null ? null : info.className()), true);
        field(json, 2, "methodName", value(info == null ? null : info.methodName()), true);
        field(json, 2, "displayName", value(info == null ? null : info.displayName()), true);
        field(json, 2, "description", value(info == null ? null : info.description()), true);
        field(json, 2, "status", throwable == null ? "passed" : "failed", true);
        field(json, 2, "attempt", String.valueOf(attempt), true);
        field(json, 2, "retried", String.valueOf(info != null && info.retried()), true);
        field(json, 2, "traceMode", effectiveTraceMode(), false);
        objectEnd(json, 1, true);
    }

    private static void appendExceptionObject(StringBuilder json, Throwable throwable) {
        objectStart(json, 1, "exception");
        field(json, 2, "type", throwable == null ? "" : throwable.getClass().getName(), true);
        field(json, 2, "message", redactThrowableText(throwable,
                throwable == null ? "" : throwable.getMessage()), true);
        field(json, 2, "stacktrace", redactThrowableText(throwable,
                ReportManagerHelper.formatStackTraceToLogEntry(throwable)), false);
        objectEnd(json, 1, true);
    }

    /**
     * Decodes the base64 {@code screenshot} field each drained {@link TraceEventRecorder.ActionEvent}
     * may carry back into raw PNG bytes, keyed by action id, so they can be persisted as standalone
     * files alongside the trace zip/directory. Invalid entries are skipped rather than failing trace
     * generation.
     */
    private static Map<String, byte[]> decodeScreenshots(List<TraceEventRecorder.ActionEvent> actions) {
        Map<String, byte[]> screenshots = new LinkedHashMap<>();
        for (TraceEventRecorder.ActionEvent action : actions) {
            if (action.screenshot().isEmpty()) {
                continue;
            }
            try {
                screenshots.put(action.id(), Base64.getDecoder().decode(action.screenshot()));
            } catch (IllegalArgumentException ignored) {
                // Corrupt base64 must never fail trace generation; just skip persisting that file.
            }
        }
        return screenshots;
    }

    private static TraceEventRecorder.ActionEvent resanitizeActionDom(TraceEventRecorder.ActionEvent action) {
        return new TraceEventRecorder.ActionEvent(action.id(), action.backend(), action.category(), action.name(),
                action.status(), action.startTime(), action.durationMs(), action.locator(), action.url(), action.caller(),
                action.message(), action.exceptionType(), action.exceptionMessage(), action.attachments(),
                action.metadata(), action.actionability(), redactSourceText(action.domSnapshotBefore()),
                redactSourceText(action.domSnapshotAfter()), action.screenshot());
    }

    private static Map<String, TraceEventRecorder.ActionSnapshots> resanitizeActionSnapshots(
            Map<String, TraceEventRecorder.ActionSnapshots> snapshots) {
        Map<String, TraceEventRecorder.ActionSnapshots> sanitized = new LinkedHashMap<>();
        snapshots.forEach((actionId, phases) -> sanitized.put(actionId, new TraceEventRecorder.ActionSnapshots(
                resanitizeSnapshot(phases.before()), resanitizeSnapshot(phases.after()))));
        return Map.copyOf(sanitized);
    }

    private static SeleniumTraceCapture.Result resanitizeSnapshot(SeleniumTraceCapture.Result result) {
        if (result == null || result.content().isEmpty()) {
            return result;
        }
        SeleniumTraceCapture.Result sanitized = SeleniumTraceCapture.fromContent(
                result.provider(), result.fidelity(), result.type(), result.content(),
                FailureTraceReporter::redactSourceText);
        boolean preserveTruncation = result.truncated() && "available".equals(sanitized.status());
        return new SeleniumTraceCapture.Result(sanitized.provider(),
                preserveTruncation ? result.fidelity() : sanitized.fidelity(),
                preserveTruncation ? result.status() : sanitized.status(),
                preserveTruncation ? result.reason() : sanitized.reason(), sanitized.type(), sanitized.content(),
                result.truncated() || sanitized.truncated());
    }

    private static List<TraceArtifactManifest.SnapshotResource> snapshotResources(
            List<TraceEventRecorder.ActionEvent> actions,
            Map<String, TraceEventRecorder.ActionSnapshots> snapshotsByAction) {
        List<TraceArtifactManifest.SnapshotResource> resources = new ArrayList<>();
        Map<String, byte[]> canonicalBytes = new java.util.HashMap<>();
        for (TraceEventRecorder.ActionEvent action : actions) {
            TraceEventRecorder.ActionSnapshots snapshots = snapshotsByAction.get(action.id());
            addSnapshotResource(resources, canonicalBytes, action.id(), "before",
                    snapshots == null ? null : snapshots.before());
            addSnapshotResource(resources, canonicalBytes, action.id(), "after",
                    snapshots == null ? null : snapshots.after());
        }
        return List.copyOf(resources);
    }

    private static void addSnapshotResource(List<TraceArtifactManifest.SnapshotResource> resources,
                                            Map<String, byte[]> canonicalBytes, String actionId, String phase,
                                            SeleniumTraceCapture.Result snapshot) {
        if (snapshot == null) {
            return;
        }
        byte[] bytes = canonicalBytes.computeIfAbsent(snapshot.content(),
                content -> content.getBytes(StandardCharsets.UTF_8));
        resources.add(new TraceArtifactManifest.SnapshotResource(
                "snapshot-" + actionId + "-" + phase, actionId, phase, snapshot, bytes));
    }

    static boolean shouldAttachTrace(TestExecutionInfo info) {
        if (SHAFT.Properties.reporting == null || !SHAFT.Properties.reporting.traceEnabled() || info == null) {
            return false;
        }
        return switch (effectiveTraceMode()) {
            case "always" -> true;
            case "retry" -> info.throwable() != null || info.retried();
            default -> info.throwable() != null;
        };
    }

    /**
     * Resolves the effective trace mode. The default {@code auto} promotes itself to {@code retry}
     * when test retries are configured ({@code retryMaximumNumberOfAttempts > 0}) so flaky-test
     * investigations always keep a timeline, and falls back to {@code failure} otherwise.
     * Explicit {@code always} / {@code retry} / {@code failure} values are honored unchanged.
     */
    static String effectiveTraceMode() {
        String mode = SHAFT.Properties.reporting == null ? "auto"
                : SHAFT.Properties.reporting.traceMode().toLowerCase(Locale.ROOT).trim();
        if (!"auto".equals(mode)) {
            return mode;
        }
        return retriesConfigured() ? "retry" : "failure";
    }

    private static boolean retriesConfigured() {
        try {
            return SHAFT.Properties.flags != null && SHAFT.Properties.flags.retryMaximumNumberOfAttempts() > 0;
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static String traceAttachmentLabel(TestExecutionInfo info, int attempt) {
        return traceLabel("SHAFT Trace Report", info, attempt);
    }

    /**
     * Label for the one-click, in-report trace viewer HTML attachment (issue #3534 P2), distinct
     * from the "SHAFT Trace Report" zip label so the two are unambiguous in the report.
     */
    private static String traceViewerLabel(TestExecutionInfo info, int attempt) {
        return traceLabel("SHAFT Trace Viewer", info, attempt);
    }

    private static String traceLabel(String prefix, TestExecutionInfo info, int attempt) {
        String label = prefix + " - " + safeTestId(info);
        return attempt > 1 || (info != null && info.retried()) ? label + " (attempt " + attempt + ")" : label;
    }

    /**
     * Names of trace bundle entries whose payload exceeds {@code shaft.trace.maxArtifactMb} and will
     * therefore carry an omission marker inside the zip. Surfaced in the viewer and index so
     * truncation is never silent.
     */
    private static List<String> omittedEntries(String json, TraceArtifactManifest manifest) {
        long maxBytes = configuredMaxArtifactBytes();
        List<String> omitted = new ArrayList<>();
        if (json.getBytes(StandardCharsets.UTF_8).length > maxBytes) {
            omitted.add("shaft-trace.json");
        }
        if (manifest != null) {
            omitted.addAll(manifest.omittedPaths());
        }
        return omitted;
    }

    static long configuredMaxArtifactBytes() {
        int configuredMiB = SHAFT.Properties.reporting.traceMaxArtifactMb();
        return Math.multiplyExact((long) Math.max(1, configuredMiB), 1024L * 1024L);
    }

    private static String renderTraceHtml(String json, List<String> omitted) {
        StringBuilder omittedJson = new StringBuilder("[");
        for (int i = 0; i < omitted.size(); i++) {
            omittedJson.append(i > 0 ? ", " : "").append("\"").append(escapeJson(omitted.get(i))).append("\"");
        }
        omittedJson.append("]");
        String escapedOmittedJson = escapeHtml(omittedJson.toString());
        String escapedJson = escapeHtml(json);
        return """
                <!doctype html>
                <html lang="en">
                <head>
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <meta http-equiv="Content-Security-Policy" content="default-src 'none'; script-src 'unsafe-inline'; style-src 'unsafe-inline'; img-src data:; frame-src 'self'; connect-src 'none'; object-src 'none'; base-uri 'none'; form-action 'none'">
                <title>SHAFT Trace Report</title>
                <style>
                """ + ReportHtmlTheme.style() + """
                .trace-layout{display:grid;grid-template-columns:minmax(260px,340px) 1fr;gap:16px}
                .action{width:100%;margin:0 0 8px;text-align:left;background:var(--shaft-surface);color:var(--shaft-text)}
                .action.selected{border-color:var(--shaft-primary);box-shadow:0 0 0 3px rgba(var(--shaft-primary-rgb),.14)}
                .action.failed{border-left:4px solid var(--shaft-fail)}.action.passed{border-left:4px solid var(--shaft-pass)}
                .tabs{display:flex;gap:8px;flex-wrap:wrap;margin:14px 0}
                .tabs button{background:var(--shaft-surface);color:var(--shaft-primary)}
                .tabs button.selected{background:var(--shaft-primary);color:var(--shaft-on-dark)}
                #mobile-category-filter button[aria-pressed="true"]{background:var(--shaft-primary);color:var(--shaft-on-dark);box-shadow:0 0 0 3px rgba(var(--shaft-primary-rgb),.2)}
                dl{display:grid;grid-template-columns:130px 1fr;gap:6px 12px}
                dt{font-weight:700;color:var(--shaft-text-muted)}dd{margin:0;overflow-wrap:anywhere}
                #timeline-list{max-height:560px;overflow:auto;border:1px solid var(--shaft-border,#ccc);border-radius:6px}
                .timeline-entry{display:flex;gap:10px;align-items:baseline;padding:5px 10px;border-left:3px solid transparent;border-bottom:1px solid var(--shaft-border,#eee)}
                .timeline-entry.failed{border-left-color:var(--shaft-fail)}
                .timeline-entry.passed{border-left-color:var(--shaft-pass)}
                .timeline-entry.clickable{cursor:pointer}
                .timeline-entry.clickable:hover{background:rgba(var(--shaft-primary-rgb),.08)}
                .timeline-entry.selected{background:rgba(var(--shaft-primary-rgb),.14)}
                .timeline-entry.inwindow,.action.inwindow{background:rgba(var(--shaft-primary-rgb),.08)}
                .time-cell{white-space:nowrap;font-variant-numeric:tabular-nums;color:var(--shaft-text-muted);min-width:86px}
                .badge{font-size:.72em;font-weight:700;letter-spacing:.4px;padding:1px 7px;border-radius:10px;background:var(--shaft-surface);border:1px solid var(--shaft-border,#ccc);color:var(--shaft-text-muted);min-width:52px;text-align:center;flex:none}
                .badge.kind-action{color:var(--shaft-primary);border-color:var(--shaft-primary)}
                .timeline-label{overflow-wrap:anywhere}
                .trace-table{width:100%;border-collapse:collapse;font-size:.86em}
                .trace-table th,.trace-table td{padding:4px 8px;border-bottom:1px solid var(--shaft-border,#eee);text-align:left;vertical-align:top;overflow-wrap:anywhere}
                .trace-table tbody tr:hover{background:rgba(var(--shaft-primary-rgb),.08)}
                .trace-table tr.inwindow{background:rgba(var(--shaft-primary-rgb),.10)}
                .trace-table tr.failed td{color:var(--shaft-fail)}
                .panel-controls{display:flex;gap:8px;align-items:end;flex-wrap:wrap;margin:10px 0}
                .panel-controls label{display:grid;gap:3px;font-size:.82em;color:var(--shaft-text-muted)}
                .panel-controls input,.panel-controls select{min-width:130px}
                .result-count{margin-left:auto;color:var(--shaft-text-muted)}
                .sort-button{border:0;background:transparent;color:inherit;padding:2px;font:inherit;font-weight:700}
                .trace-navigator{margin-bottom:16px}
                .range-controls{display:grid;grid-template-columns:auto 1fr auto 1fr auto;gap:8px;align-items:center}
                .range-controls input{width:100%}
                .filmstrip{display:flex;gap:8px;overflow-x:auto;padding:8px 2px}
                .filmstrip button{min-width:132px;max-width:180px;padding:6px;text-align:left;background:var(--shaft-surface);color:var(--shaft-text)}
                .filmstrip button.selected{border-color:var(--shaft-primary);box-shadow:0 0 0 3px rgba(var(--shaft-primary-rgb),.14)}
                .filmstrip button:not(.inwindow){opacity:.58}
                .filmstrip img{display:block;width:100%;height:76px;object-fit:contain;background:var(--shaft-bg);margin-bottom:4px}
                .filmstrip-missing{display:grid;place-items:center;height:76px;border:1px dashed var(--shaft-border,#ccc);margin-bottom:4px;color:var(--shaft-text-muted)}
                .comparison-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px}
                .comparison-grid section{min-width:0}
                .comparison-grid iframe,.comparison-grid img{width:100%;height:360px;object-fit:contain;border:1px solid var(--shaft-border,#ccc);background:var(--shaft-bg)}
                .trace-status-strip{display:flex;flex-wrap:wrap;gap:10px 16px;align-items:center}
                .trace-status-strip .status-meta{color:var(--shaft-text-muted)}
                .tab-groups{display:grid;gap:8px;margin:14px 0}
                .tab-groups .tabs{margin:0}
                .tab-groups .tab-secondary button{opacity:.92}
                :focus-visible{outline:3px solid var(--shaft-primary);outline-offset:2px}
                @media(prefers-reduced-motion:reduce){*{scroll-behavior:auto!important;transition:none!important;animation:none!important}}
                @media(max-width:900px){.trace-layout{grid-template-columns:1fr}}
                @media(max-width:700px){.range-controls{grid-template-columns:1fr}.comparison-grid{grid-template-columns:1fr}}
                </style>
                </head>
                <body>
                <div class="report-shell">
                <header class="report-header">
                  <div class="report-header-inner">
                    <span class="brand-mark">S</span>
                    <div>
                      <h1>SHAFT Trace Report</h1>
                      <p class="subtitle" id="trace-subtitle">Failure trace viewer</p>
                    </div>
                  </div>
                </header>
                <main class="report-main">
                <section class="panel" id="truncation-banner" hidden
                         style="border-left:4px solid var(--shaft-fail)">
                  <h2>Trace partially truncated</h2>
                  <p class="muted" id="truncation-detail"></p>
                </section>
                <section class="panel trace-summary" id="trace-summary"></section>
                <section class="panel trace-navigator" aria-labelledby="trace-navigator-title">
                  <div class="toolbar">
                    <h2 id="trace-navigator-title">Trace navigator</h2>
                    <button type="button" class="secondary" id="show-all-range">Show all</button>
                  </div>
                  <div class="range-controls">
                    <label for="range-start">Range start</label>
                    <input id="range-start" type="range" min="0" max="1" value="0" step="1">
                    <output id="range-label" aria-live="polite">Full trace</output>
                    <input id="range-end" type="range" min="0" max="1" value="1" step="1">
                    <label for="range-end">Range end</label>
                  </div>
                  <div id="trace-filmstrip" class="filmstrip" role="listbox" aria-label="Action screenshot filmstrip"></div>
                </section>
                <div class="trace-layout">
                  <aside class="panel">
                    <h2>Actions</h2>
                    <div class="toolbar"><input id="action-search" type="search" aria-label="Search actions" placeholder="Search actions"></div>
                    <div id="action-list"></div>
                  </aside>
                  <section class="panel">
                    <div class="toolbar">
                      <h2 id="details-title">Trace Details</h2>
                      <button type="button" class="secondary" onclick="copyJson()">Copy JSON</button>
                    </div>
                    <dl id="details"></dl>
                    <div class="tab-groups" id="action-tabs">
                      <div class="tabs tab-primary" role="tablist" aria-label="Primary investigation">
                      <button data-tab="timeline" class="selected">Timeline</button>
                      <button data-tab="comparison">Snapshots</button>
                      <button data-tab="network">Network</button>
                      <button data-tab="console">Console</button>
                      <button data-tab="source">Source</button>
                      <button data-tab="artifacts">Artifacts</button>
                      </div>
                      <div class="tabs tab-secondary" role="tablist" aria-label="More evidence">
                      <button data-tab="exception">Exception</button>
                      <button data-tab="snapshot">Snapshot</button>
                      <button data-tab="nativeEvidence">Native evidence</button>
                      <button data-tab="domSnapshot">DOM Snapshot</button>
                      <button data-tab="screenshot">Screenshot</button>
                      <button data-tab="locatorHealth">Locator Health</button>
                       <button data-tab="webSockets">WebSockets</button>
                       <button data-tab="mobile">Mobile</button>
                       <button data-tab="browserObservability">Observability</button>
                      <button data-tab="environment">Environment</button>
                      <button data-tab="attachments">Attachments</button>
                      <button data-tab="log">Test Log</button>
                      <button data-tab="json">JSON</button>
                      </div>
                    </div>
                    <pre id="tab-content"></pre>
                    <div id="timeline-panel" hidden>
                      <p class="muted">Every recorded action, network exchange, and console message in chronological order. Click an action to inspect it.</p>
                      <div class="tabs" id="timeline-filters">
                        <button data-filter="all" class="selected">All</button>
                        <button data-filter="action">Actions</button>
                        <button data-filter="validation">Validations</button>
                        <button data-filter="network">Network</button>
                        <button data-filter="console">Console</button>
                        <button data-filter="failed">Failures</button>
                      </div>
                      <div id="timeline-list"></div>
                    </div>
                    <div id="dom-snapshot-panel" hidden>
                      <div class="tabs" id="dom-snapshot-tabs">
                        <button data-dom="before" class="selected">Before</button>
                        <button data-dom="after">After</button>
                      </div>
                      <iframe id="dom-snapshot-frame" title="DOM snapshot" sandbox=""
                              style="width:100%;height:420px;border:1px solid var(--shaft-border,#ccc);background:var(--shaft-bg)"></iframe>
                    </div>
                    <div id="screenshot-panel" hidden>
                      <img id="screenshot-image" alt="Action screenshot"
                           style="max-width:100%;border:1px solid var(--shaft-border,#ccc)">
                      <p id="screenshot-empty" class="muted" hidden>No screenshot captured for this action.</p>
                    </div>
                    <div id="comparison-panel" hidden>
                      <div class="comparison-grid">
                        <section><h3>Before action</h3><iframe id="comparison-before" title="Before action DOM snapshot" sandbox=""></iframe><p id="comparison-before-empty" class="muted" hidden>No before-action snapshot was captured.</p></section>
                        <section><h3>Action state</h3><iframe id="comparison-input" title="Native action-state DOM snapshot" sandbox=""></iframe><img id="comparison-action" alt="Screenshot captured during the selected action"><p id="comparison-action-empty" class="muted" hidden>No action-state snapshot or screenshot was captured.</p></section>
                        <section><h3>After action</h3><iframe id="comparison-after" title="After action DOM snapshot" sandbox=""></iframe><p id="comparison-after-empty" class="muted" hidden>No after-action snapshot was captured.</p></section>
                      </div>
                    </div>
                    <div id="native-evidence-panel" hidden>
                      <p class="muted" id="native-evidence-hint"></p>
                      <table class="trace-table"><thead><tr>
                        <th>Correlation</th><th>Operation</th><th>Source</th><th>Logs</th><th>Error</th>
                      </tr></thead><tbody id="native-evidence-rows"></tbody></table>
                    </div>
                    <div id="network-panel" hidden>
                      <p class="muted" id="network-hint"></p>
                      <div class="panel-controls">
                        <label>Method<select id="network-method-filter"><option value="">All methods</option></select></label>
                        <label>Status<select id="network-status-filter"><option value="">All statuses</option></select></label>
                        <label>Search<input id="network-text-filter" type="search" placeholder="URL, headers, or body"></label>
                        <output id="network-result-count" class="result-count" aria-live="polite"></output>
                      </div>
                      <table class="trace-table"><thead><tr>
                        <th id="network-sort-time"><button type="button" class="sort-button" data-network-sort="time">Time</button></th>
                        <th id="network-sort-type"><button type="button" class="sort-button" data-network-sort="type">Type</button></th>
                        <th id="network-sort-method" aria-sort="ascending"><button type="button" class="sort-button" data-network-sort="method">Method</button></th>
                        <th id="network-sort-status"><button type="button" class="sort-button" data-network-sort="status">Status</button></th>
                        <th id="network-sort-duration"><button type="button" class="sort-button" data-network-sort="duration">Duration</button></th>
                        <th id="network-sort-size"><button type="button" class="sort-button" data-network-sort="size">Size</button></th>
                        <th>URL</th><th>Details</th>
                      </tr></thead><tbody id="network-rows"></tbody></table>
                      <pre id="network-detail" hidden></pre>
                    </div>
                    <div id="console-panel" hidden>
                      <p class="muted" id="console-hint"></p>
                      <div class="panel-controls">
                        <label>Source<select id="console-source-filter"><option value="">All sources</option></select></label>
                        <label>Level<select id="console-level-filter"><option value="">All levels</option></select></label>
                        <label>Search<input id="console-text-filter" type="search" placeholder="Search console messages"></label>
                        <output id="console-result-count" class="result-count" aria-live="polite"></output>
                      </div>
                      <table class="trace-table"><thead><tr>
                        <th id="console-sort-time" aria-sort="ascending"><button type="button" class="sort-button" data-console-sort="time">Time</button></th>
                        <th id="console-sort-source"><button type="button" class="sort-button" data-console-sort="source">Source</button></th>
                        <th id="console-sort-level"><button type="button" class="sort-button" data-console-sort="level">Level</button></th>
                        <th id="console-sort-message"><button type="button" class="sort-button" data-console-sort="message">Message</button></th><th>Details</th>
                      </tr></thead><tbody id="console-rows"></tbody></table>
                     <pre id="console-detail" hidden></pre>
                   </div>
                   <div id="mobile-panel" hidden>
                     <p class="muted" id="mobile-hint"></p>
                     <div class="toolbar" id="mobile-category-filter" role="group" aria-label="Mobile action category">
                       <button type="button" class="selected" data-mobile-category="all" aria-pressed="true">All</button>
                       <button type="button" data-mobile-category="mobile/app" aria-pressed="false">App</button>
                       <button type="button" data-mobile-category="mobile/context" aria-pressed="false">Context</button>
                       <button type="button" data-mobile-category="mobile/device" aria-pressed="false">Device</button>
                       <button type="button" data-mobile-category="mobile/logs" aria-pressed="false">Logs</button>
                       <button type="button" data-mobile-category="mobile/performance" aria-pressed="false">Performance</button>
                       <button type="button" data-mobile-category="mobile/recording" aria-pressed="false">Recording</button>
                       <button type="button" data-mobile-category="mobile/evidence" aria-pressed="false">Evidence</button>
                       <output id="mobile-result-count" class="result-count" aria-live="polite"></output>
                     </div>
                     <table class="trace-table"><thead><tr>
                       <th>Time</th><th>Area</th><th>Operation</th><th>Status</th><th>Summary</th><th>Details</th>
                     </tr></thead><tbody id="mobile-rows"></tbody></table>
                     <pre id="mobile-detail" hidden></pre>
                   </div>
                   <div id="websocket-panel" hidden>
                     <p class="muted" id="websocket-hint"></p>
                     <div class="panel-controls">
                       <label>Direction<select id="websocket-direction-filter"><option value="">All directions</option></select></label>
                       <label>Type<select id="websocket-type-filter"><option value="">All event types</option></select></label>
                       <label>Search<input id="websocket-text-filter" type="search" placeholder="URL, payload, digest, or reason"></label>
                       <output id="websocket-result-count" class="result-count" aria-live="polite"></output>
                     </div>
                     <table class="trace-table"><thead><tr>
                       <th>Type</th><th>Direction</th><th>URL</th><th>Opcode</th><th>Payload</th><th>Details</th>
                     </tr></thead><tbody id="websocket-rows"></tbody></table>
                     <pre id="websocket-detail" hidden></pre>
                   </div>
                   <div id="artifact-panel" hidden>
                     <p class="muted" id="artifact-hint"></p>
                     <p class="muted" id="native-trace-handoff" hidden></p>
                     <output id="artifact-result-count" class="result-count" aria-live="polite"></output>
                     <table class="trace-table"><thead><tr>
                       <th>Path</th><th>Kind</th><th>Media type</th><th>Status</th><th>Size</th><th>Digest</th><th>Details</th>
                     </tr></thead><tbody id="artifact-rows"></tbody></table>
                   </div>
                 </section>
                </div>
                </main>
                </div>
                <pre hidden id="trace-data">""" + escapedJson + """
                </pre>
                <pre hidden id="trace-truncation">""" + escapedOmittedJson + """
                </pre>
                <script>
                const trace = JSON.parse(document.getElementById('trace-data').textContent);
                const truncation = JSON.parse(document.getElementById('trace-truncation').textContent);
                const evidence = trace && trace.evidence && typeof trace.evidence === 'object'
                    ? trace.evidence : trace;
                const actions = Array.isArray(evidence.actions) ? evidence.actions : [];
                const network = Array.isArray(evidence.network) ? evidence.network : [];
                const consoleEvents = Array.isArray(evidence.console) ? evidence.console : [];
                const playwright = evidence.playwright && typeof evidence.playwright === 'object'
                    ? evidence.playwright : {status:'unavailable', reason:'', actions:[], correlations:[], snapshots:{}};
                const nativeActions = Array.isArray(playwright.actions) ? playwright.actions : [];
                const nativeSnapshots = playwright.snapshots && typeof playwright.snapshots === 'object'
                    ? playwright.snapshots : {};
                const browserObservability = evidence.browserObservability && typeof evidence.browserObservability === 'object'
                    ? evidence.browserObservability : {warnings:[], webSockets:[]};
                const webSockets = Array.isArray(browserObservability.webSockets)
                    ? browserObservability.webSockets : [];
                const artifacts = Array.isArray(trace.session && trace.session.artifacts)
                    ? trace.session.artifacts : [];
                const actionList = document.getElementById('action-list');
                const actionSearch = document.getElementById('action-search');
                const details = document.getElementById('details');
                const tabContent = document.getElementById('tab-content');
                function hashState(){
                  const raw = String(location.hash || '').replace(/^#/, '');
                  if (!raw.startsWith('action-')) return {actionId:null, params:new URLSearchParams()};
                  const separator = raw.indexOf('?');
                  const encodedId = separator < 0 ? raw.slice(7) : raw.slice(7, separator);
                  try {
                    return {actionId:decodeURIComponent(encodedId),
                      params:new URLSearchParams(separator < 0 ? '' : raw.slice(separator + 1))};
                  } catch (ignored) {
                    return {actionId:null, params:new URLSearchParams()};
                  }
                }
                function actionFromHash(){
                  const state = hashState();
                  return state.actionId ? actions.find(action => action.id === state.actionId) : null;
                }
                let selected = actionFromHash()
                    || [...actions].reverse().find(action => action.status !== 'passed')
                    || actions[0] || null;
                let selectedNativeAction = null;
                function selectAction(action, selectItsRange = true, historyMode = 'push'){
                  selectedNativeAction = null;
                  selected = action;
                  if (selectItsRange && action) {
                    const start = actionStartMs(action);
                    if (start != null) {
                      rangeStartMs = start;
                      rangeEndMs = Math.max(start, start + Math.max(0, action.durationMs || 0));
                    }
                  }
                  updateHash(historyMode);
                  renderNavigator();
                  renderActions();
                  renderDetails();
                }
                function esc(value){
                  return String(value || '').replace(/[&<>"']/g, char => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[char]));
                }
                function statusClass(value){
                  value = String(value || '').toLowerCase();
                  if(value.includes('pass')) return 'passed';
                  if(value.includes('fail') || value.includes('error')) return 'failed';
                  if(value.includes('warn') || value.includes('skip')) return 'warn';
                  return 'neutral';
                }
                function actionStartMs(action){
                  const t = Date.parse(action && action.startTime);
                  return isNaN(t) ? null : t;
                }
                function networkStartMs(entry){
                  return entry.timestamp ? entry.timestamp - (entry.durationMs || 0) : null;
                }
                function networkFailed(entry){
                  return !entry.status || entry.status >= 400 || Boolean(entry.failureReason);
                }
                function consoleFailed(entry){
                  const level = String(entry.level || '').toUpperCase();
                  return level.includes('SEVERE') || level.includes('ERROR');
                }
                function timelineEntries(){
                  const entries = [];
                  actions.forEach(action => entries.push({t: actionStartMs(action), kind: 'action',
                      status: statusClass(action.status), durationMs: action.durationMs,
                      label: `${action.name || 'Action'}  ${action.locator || ''}`.trim(), action}));
                  network.forEach(entry => entries.push({t: networkStartMs(entry), kind: 'network',
                      status: networkFailed(entry) ? 'failed' : 'neutral', durationMs: entry.durationMs,
                      label: `${entry.method || ''} ${entry.status || 'FAILED'} ${entry.url || ''}${entry.failureReason ? ' - ' + entry.failureReason : ''}`.trim()}));
                  consoleEvents.forEach(entry => entries.push({t: entry.timestamp || null, kind: 'console',
                      status: consoleFailed(entry) ? 'failed' : 'neutral',
                      label: `${entry.level || ''} ${entry.message || ''}`.trim()}));
                  return entries.sort((left, right) => (left.t ?? Infinity) - (right.t ?? Infinity));
                }
                const allEntries = timelineEntries();
                const baseTime = allEntries.reduce((min, entry) => entry.t != null && (min == null || entry.t < min) ? entry.t : min, null);
                const traceEnd = allEntries.reduce((max, entry) => entry.t == null ? max
                    : Math.max(max == null ? entry.t : max, entry.t + Math.max(0, entry.durationMs || 0)), baseTime);
                const traceDuration = Math.max(1, (traceEnd ?? 1) - (baseTime ?? 0));
                let rangeStartMs = baseTime;
                let rangeEndMs = traceEnd;
                const initialHash = hashState();
                const initialHashRange = initialHash.params;
                if (baseTime != null && initialHashRange.has('start') && initialHashRange.has('end')) {
                  const startOffset = Number(initialHashRange.get('start'));
                  const endOffset = Number(initialHashRange.get('end'));
                  if (Number.isFinite(startOffset) && Number.isFinite(endOffset)) {
                    rangeStartMs = baseTime + Math.max(0, Math.min(traceDuration, startOffset));
                    rangeEndMs = baseTime + Math.max(0, Math.min(traceDuration, endOffset));
                     if (rangeStartMs > rangeEndMs) [rangeStartMs, rangeEndMs] = [rangeEndMs, rangeStartMs];
                   }
                } else if (baseTime != null && initialHash.actionId && selected) {
                  const start = actionStartMs(selected);
                  if (start != null) {
                    rangeStartMs = start;
                    rangeEndMs = Math.max(start, start + Math.max(0, selected.durationMs || 0));
                  }
                }
                function offsetLabel(t){
                  return t == null || baseTime == null ? '' : '+' + ((t - baseTime) / 1000).toFixed(3) + 's';
                }
                function selectedWindow(){
                  return rangeStartMs == null || rangeEndMs == null ? null : [rangeStartMs, rangeEndMs];
                }
                function inWindow(t, range){
                  return range != null && t != null && t >= range[0] && t <= range[1];
                }
                function intervalOverlaps(start, durationMs, range){
                  if (start == null || range == null) return false;
                  const end = start + Math.max(0, durationMs || 0);
                  return end >= range[0] && start <= range[1];
                }
                function actionInWindow(action, range){
                  return intervalOverlaps(actionStartMs(action), action.durationMs, range);
                }
                function updateHash(mode = 'replace'){
                  if (mode === 'none' || !selected || !selected.id) return;
                  const start = baseTime == null || rangeStartMs == null ? 0 : Math.round(rangeStartMs - baseTime);
                  const end = baseTime == null || rangeEndMs == null ? traceDuration : Math.round(rangeEndMs - baseTime);
                  const hash = '#action-' + encodeURIComponent(selected.id) + '?start=' + start + '&end=' + end;
                  if (mode === 'push') history.pushState(null, '', hash);
                  else history.replaceState(null, '', hash);
                }
                function renderSummary(){
                  const test = trace.test || {};
                  const exception = trace.exception || {};
                  const failedActions = actions.filter(action => action.status === 'failed').length;
                  const failedNetwork = network.filter(networkFailed).length;
                  const consoleErrors = consoleEvents.filter(consoleFailed).length;
                  const attempt = parseInt(test.attempt, 10) || 1;
                  const retried = String(test.retried) === 'true';
                  const attemptSuffix = attempt > 1 || retried ? ` - attempt ${attempt}${retried ? ' (retried)' : ''}` : '';
                  document.getElementById('trace-subtitle').textContent = `${test.className || 'Unknown class'}.${test.methodName || 'unknown'}${attemptSuffix} - ${trace.generatedAt || ''}`;
                  const attemptChip = attempt > 1 || retried
                    ? `<span class="status-chip warn">attempt ${attempt}${retried ? ' retried' : ''}</span>`
                    : '';
                  document.getElementById('trace-summary').innerHTML = `
                    <h2>Investigation</h2>
                    <div class="trace-status-strip">
                      <span class="status-chip ${statusClass(test.status)}">${esc(test.status || 'unknown')}</span>
                      ${attemptChip}
                      <span class="status-meta">${actions.length} actions${failedActions ? ` · ${failedActions} failed` : ''}</span>
                      <span class="status-meta">${network.length} network${failedNetwork ? ` · ${failedNetwork} failed` : ''}</span>
                      <span class="status-meta">${consoleErrors} console errors</span>
                      <span class="status-meta">${esc(exception.type || 'No exception')}</span>
                    </div>`;
                  if (truncation.length) {
                    document.getElementById('truncation-banner').hidden = false;
                    const artifactReasons = new Map(artifacts.filter(artifact => artifact.omitted)
                      .map(artifact => [artifact.path, artifact.metadata && artifact.metadata.omissionReason]));
                    const omittedDetails = truncation.map(path => artifactReasons.get(path)
                      ? `${path}: ${artifactReasons.get(path)}`
                      : `${path}: exceeded shaft.trace.maxArtifactMb and was replaced with an omission marker`);
                    document.getElementById('truncation-detail').textContent =
                      `Some bundle entries were omitted: ${omittedDetails.join('; ')}.`;
                  } else {
                    document.getElementById('truncation-banner').hidden = true;
                    document.getElementById('truncation-detail').textContent = '';
                  }
                }
                const filmstrip = document.getElementById('trace-filmstrip');
                const rangeStart = document.getElementById('range-start');
                const rangeEnd = document.getElementById('range-end');
                const rangeLabel = document.getElementById('range-label');
                function applyRangeInputs(historyMode = 'none'){
                  if (baseTime == null) return;
                  selectedNativeAction = null;
                  const startOffset = Math.max(0, Math.min(traceDuration, Number(rangeStart.value)));
                  const endOffset = Math.max(0, Math.min(traceDuration, Number(rangeEnd.value)));
                  rangeStartMs = baseTime + Math.min(startOffset, endOffset);
                  rangeEndMs = baseTime + Math.max(startOffset, endOffset);
                  updateHash(historyMode);
                  renderNavigator();
                  renderActions();
                  renderDetails();
                }
                function renderNavigator(){
                  const startOffset = baseTime == null || rangeStartMs == null ? 0 : Math.max(0, rangeStartMs - baseTime);
                  const endOffset = baseTime == null || rangeEndMs == null ? traceDuration : Math.max(0, rangeEndMs - baseTime);
                  rangeStart.max = String(traceDuration);
                  rangeEnd.max = String(traceDuration);
                  rangeStart.value = String(Math.min(startOffset, endOffset));
                  rangeEnd.value = String(Math.max(startOffset, endOffset));
                  rangeStart.disabled = baseTime == null;
                  rangeEnd.disabled = baseTime == null;
                  rangeLabel.value = baseTime == null ? 'No timed evidence'
                      : `${offsetLabel(rangeStartMs)} to ${offsetLabel(rangeEndMs)}`;
                  filmstrip.innerHTML = '';
                  if (!actions.length) {
                    filmstrip.textContent = 'No actions were recorded for the filmstrip.';
                    return;
                  }
                  const range = selectedWindow();
                  actions.forEach(action => {
                    const button = document.createElement('button');
                    button.type = 'button';
                    button.setAttribute('role', 'option');
                    button.setAttribute('aria-selected', String(Boolean(selected && selected.id === action.id)));
                    button.tabIndex = selected && selected.id === action.id ? 0 : -1;
                    button.dataset.actionId = action.id || '';
                    button.className = `${selected && selected.id === action.id ? 'selected ' : ''}${actionInWindow(action, range) ? 'inwindow' : ''}`.trim();
                    if (action.screenshot) {
                      const image = document.createElement('img');
                      image.alt = '';
                      image.src = 'data:image/png;base64,' + action.screenshot;
                      button.appendChild(image);
                    } else {
                      const missing = document.createElement('span');
                      missing.className = 'filmstrip-missing';
                      missing.textContent = 'No screenshot';
                      button.appendChild(missing);
                    }
                    const label = document.createElement('span');
                    label.textContent = `${offsetLabel(actionStartMs(action))} ${action.name || 'Action'}`.trim();
                    button.appendChild(label);
                    button.addEventListener('click', () => selectAction(action));
                    filmstrip.appendChild(button);
                  });
                }
                function renderActions(){
                  actionList.innerHTML = '';
                  if(!actions.length){ actionList.textContent = 'No structured actions recorded.'; return; }
                  const query = actionSearch.value.toLowerCase();
                  actions.filter(action => !query || JSON.stringify(action).toLowerCase().includes(query)).forEach(action => {
                    const button = document.createElement('button');
                    button.className = `action ${action.status}${selected && selected.id === action.id ? ' selected' : ''}${actionInWindow(action, selectedWindow()) ? ' inwindow' : ''}`;
                    button.innerHTML = `<strong>${esc(action.name || 'Action')}</strong><div class="muted">${esc(action.category)} - ${esc(action.status)} - ${esc(action.durationMs || 0)}ms${action.screenshot ? ' 📷' : ''}</div>`;
                    button.addEventListener('click', () => selectAction(action));
                    actionList.appendChild(button);
                  });
                }
                function row(name, value){ return value ? `<dt>${esc(name)}</dt><dd>${esc(value)}</dd>` : ''; }
                function nativeActionFor(action){
                  if (selectedNativeAction) return selectedNativeAction;
                  const callId = action && action.metadata && action.metadata.playwrightCallId;
                  return callId ? nativeActions.find(candidate => candidate.callId === callId) || null : null;
                }
                function renderDetails(){
                  const action = selected || {};
                  document.getElementById('details-title').textContent = action.name ? `Action: ${action.name}` : 'Trace Details';
                  const metadata = action.metadata || {};
                  details.innerHTML = row('Status', action.status) + row('Category', action.category)
                    + row('Expected', metadata.expected) + row('Actual', metadata.actual)
                    + row('Locator', action.locator) + row('URL', action.url) + row('Caller', action.caller) + row('Started', action.startTime) + row('Duration', action.durationMs == null ? '' : `${action.durationMs}ms`) + row('Message', action.message);
                  const native = nativeActionFor(action);
                  details.innerHTML += row('Native fidelity', native ? 'Playwright correlated' : 'SHAFT capture')
                    + row('Native source', native && (native.source || native.sourceReason))
                    + row('Native error', native && native.error);
                  renderTab(document.querySelector('.tabs button.selected').dataset.tab);
                }
                const timelinePanel = document.getElementById('timeline-panel');
                const timelineList = document.getElementById('timeline-list');
                let timelineFilter = 'all';
                function matchesTimelineFilter(entry){
                  if (timelineFilter === 'all') return true;
                  if (timelineFilter === 'failed') return entry.status === 'failed';
                  if (timelineFilter === 'validation') return entry.kind === 'action' && entry.action && entry.action.category === 'validation';
                  return entry.kind === timelineFilter;
                }
                function renderTimeline(){
                  timelineList.innerHTML = '';
                  if (!allEntries.length) { timelineList.textContent = 'No timeline events were recorded.'; return; }
                  const visibleEntries = allEntries.filter(matchesTimelineFilter);
                  if (!visibleEntries.length) { timelineList.textContent = 'No timeline events match this filter.'; return; }
                  const range = selectedWindow();
                  visibleEntries.forEach(entry => {
                    const div = document.createElement('div');
                    const isSelected = entry.action && selected && entry.action.id === selected.id;
                    const overlaps = entry.kind === 'console'
                        ? inWindow(entry.t, range)
                        : intervalOverlaps(entry.t, entry.durationMs, range);
                    div.className = `timeline-entry ${entry.status}${entry.action ? ' clickable' : ''}${isSelected ? ' selected' : ''}${overlaps ? ' inwindow' : ''}`;
                    const duration = entry.durationMs ? ` (${entry.durationMs}ms)` : '';
                    div.innerHTML = `<span class="time-cell">${esc(offsetLabel(entry.t))}</span><span class="badge kind-${entry.kind}">${entry.kind.toUpperCase()}</span><span class="timeline-label">${esc(entry.label)}${esc(duration)}</span>`;
                    if (entry.action) {
                      div.addEventListener('click', () => selectAction(entry.action));
                    }
                    timelineList.appendChild(div);
                  });
                }
                const networkPanel = document.getElementById('network-panel');
                const networkRows = document.getElementById('network-rows');
                const networkDetail = document.getElementById('network-detail');
                const networkMethodFilter = document.getElementById('network-method-filter');
                const networkStatusFilter = document.getElementById('network-status-filter');
                const networkTextFilter = document.getElementById('network-text-filter');
                const networkResultCount = document.getElementById('network-result-count');
                let networkSort = {key:'method', direction:'ascending'};
                function finiteNumber(value){ return typeof value === 'number' && Number.isFinite(value) ? value : null; }
                function networkStatus(entry){
                  const status = finiteNumber(entry.status);
                  return status == null ? 'Unknown' : status > 0 ? String(status) : 'FAILED';
                }
                function networkType(entry){ return entry.type ? String(entry.type) : 'HTTP'; }
                function headerSearchText(headers){
                  return Object.entries(headers || {}).map(([name, value]) => `${name}: ${value}`).join(' ');
                }
                function networkSearchText(entry){
                  return [entry.url, headerSearchText(entry.requestHeaders),
                    headerSearchText(entry.responseHeaders), entry.bodyPreview]
                    .filter(value => value != null).join(' ').toLowerCase();
                }
                function networkSize(entry){
                  const request = finiteNumber(entry.requestSizeBytes);
                  const response = finiteNumber(entry.responseSizeBytes);
                  return request == null || response == null ? null : request + response;
                }
                function networkSortValue(entry, key){
                  let value = null;
                  if (key === 'time') value = finiteNumber(networkStartMs(entry));
                  if (key === 'type') value = networkType(entry);
                  if (key === 'method') value = entry.method ? String(entry.method) : null;
                  if (key === 'status') value = finiteNumber(entry.status);
                  if (key === 'duration') value = finiteNumber(entry.durationMs);
                  if (key === 'size') value = networkSize(entry);
                  return {missing:value == null, value};
                }
                function compareNetwork(left, right){
                  const a = networkSortValue(left.entry, networkSort.key);
                  const b = networkSortValue(right.entry, networkSort.key);
                  if (a.missing !== b.missing) return a.missing ? 1 : -1;
                  if (a.missing) return left.index - right.index;
                  let comparison = typeof a.value === 'number' && typeof b.value === 'number'
                    ? a.value - b.value : String(a.value).localeCompare(String(b.value));
                  if (networkSort.direction === 'descending') comparison = -comparison;
                  return comparison || left.index - right.index;
                }
                function populateNetworkFilters(){
                  const append = (select, values) => values.forEach(value => {
                    const option = document.createElement('option');
                    option.value = value;
                    option.textContent = value;
                    select.appendChild(option);
                  });
                  append(networkMethodFilter, [...new Set(network.map(entry => String(entry.method || 'UNKNOWN')))].sort());
                  append(networkStatusFilter, [...new Set(network.map(networkStatus))].sort());
                }
                function updateNetworkSortHeaders(){
                  document.querySelectorAll('[data-network-sort]').forEach(button => {
                    const header = button.closest('th');
                    if (button.dataset.networkSort === networkSort.key) {
                      header.setAttribute('aria-sort', networkSort.direction);
                    } else {
                      header.removeAttribute('aria-sort');
                    }
                  });
                }
                function renderNetwork(){
                  const range = selectedWindow();
                  networkRows.innerHTML = '';
                  networkDetail.hidden = true;
                  const query = networkTextFilter.value.trim().toLowerCase();
                  const visible = network.map((entry, index) => ({entry, index})).filter(({entry}) =>
                    (finiteNumber(networkStartMs(entry)) == null
                      || intervalOverlaps(networkStartMs(entry), entry.durationMs, range))
                    && (!networkMethodFilter.value || String(entry.method || 'UNKNOWN') === networkMethodFilter.value)
                    && (!networkStatusFilter.value || networkStatus(entry) === networkStatusFilter.value)
                    && (!query || networkSearchText(entry).includes(query)))
                    .sort(compareNetwork);
                  networkResultCount.textContent = `${visible.length} network ${visible.length === 1 ? 'exchange' : 'exchanges'}`;
                  document.getElementById('network-hint').textContent = !network.length
                    ? 'No network exchanges were recorded.'
                    : !visible.length ? 'No network exchanges match the selected range and filters.'
                    : 'Use View request details to inspect headers and body preview.';
                  visible.forEach(({entry}) => {
                    const tr = document.createElement('tr');
                    const timed = finiteNumber(networkStartMs(entry)) != null;
                    tr.className = `${networkFailed(entry) ? 'failed ' : ''}${timed ? 'inwindow' : ''}`.trim();
                    const duration = finiteNumber(entry.durationMs);
                    const size = networkSize(entry);
                    tr.innerHTML = `<td class="time-cell">${esc(offsetLabel(networkStartMs(entry)))}</td><td>${esc(networkType(entry))}</td><td>${esc(entry.method || 'Unknown')}</td><td>${esc(networkStatus(entry))}</td><td>${duration == null ? 'Unknown' : `${duration}ms`}</td><td>${size == null ? 'Unknown' : `${size} B`}</td><td>${esc(entry.url || 'Unknown')}</td><td><button type="button" class="secondary">View request details</button></td>`;
                    tr.querySelector('button').addEventListener('click', () => {
                      networkDetail.hidden = false;
                      networkDetail.textContent = JSON.stringify(entry, null, 2);
                    });
                    networkRows.appendChild(tr);
                  });
                  updateNetworkSortHeaders();
                }
                const consolePanel = document.getElementById('console-panel');
                const consoleRows = document.getElementById('console-rows');
                const consoleDetail = document.getElementById('console-detail');
                const consoleSourceFilter = document.getElementById('console-source-filter');
                const consoleLevelFilter = document.getElementById('console-level-filter');
                const consoleTextFilter = document.getElementById('console-text-filter');
                const consoleResultCount = document.getElementById('console-result-count');
                let consoleSort = {key:'time', direction:'ascending'};
                function consoleSearchText(entry){
                  return String(entry.message || '').toLowerCase();
                }
                function consoleSortValue(entry, key){
                  const value = key === 'time' ? finiteNumber(entry.timestamp)
                    : entry[key] ? String(entry[key]) : null;
                  return {missing:value == null, value};
                }
                function compareConsole(left, right){
                  const a = consoleSortValue(left.entry, consoleSort.key);
                  const b = consoleSortValue(right.entry, consoleSort.key);
                  if (a.missing !== b.missing) return a.missing ? 1 : -1;
                  if (a.missing) return left.index - right.index;
                  let comparison = typeof a.value === 'number' && typeof b.value === 'number'
                    ? a.value - b.value : String(a.value).localeCompare(String(b.value));
                  if (consoleSort.direction === 'descending') comparison = -comparison;
                  return comparison || left.index - right.index;
                }
                function populateConsoleFilters(){
                  const append = (select, values) => values.forEach(value => {
                    const option = document.createElement('option');
                    option.value = value;
                    option.textContent = value;
                    select.appendChild(option);
                  });
                  append(consoleSourceFilter, [...new Set(consoleEvents.map(entry => String(entry.source || 'Unknown')))].sort());
                  append(consoleLevelFilter, [...new Set(consoleEvents.map(entry => String(entry.level || 'Unknown')))].sort());
                }
                function updateConsoleSortHeaders(){
                  document.querySelectorAll('[data-console-sort]').forEach(button => {
                    const header = button.closest('th');
                    if (button.dataset.consoleSort === consoleSort.key) {
                      header.setAttribute('aria-sort', consoleSort.direction);
                    } else {
                      header.removeAttribute('aria-sort');
                    }
                  });
                }
                function renderConsole(){
                  const range = selectedWindow();
                  consoleRows.innerHTML = '';
                  consoleDetail.hidden = true;
                  const query = consoleTextFilter.value.trim().toLowerCase();
                  const visible = consoleEvents.map((entry, index) => ({entry, index})).filter(({entry}) =>
                    (finiteNumber(entry.timestamp) == null || inWindow(entry.timestamp, range))
                    && (!consoleSourceFilter.value || String(entry.source || 'Unknown') === consoleSourceFilter.value)
                    && (!consoleLevelFilter.value || String(entry.level || 'Unknown') === consoleLevelFilter.value)
                    && (!query || consoleSearchText(entry).includes(query)))
                    .sort(compareConsole);
                  consoleResultCount.textContent = `${visible.length} console ${visible.length === 1 ? 'message' : 'messages'}`;
                  document.getElementById('console-hint').textContent = !consoleEvents.length
                    ? 'No console messages were recorded.'
                    : !visible.length ? 'No console messages match the selected range and filters.'
                    : 'Use View message details to inspect the structured message.';
                  visible.forEach(({entry}) => {
                    const tr = document.createElement('tr');
                    const timed = finiteNumber(entry.timestamp) != null;
                    tr.className = `${consoleFailed(entry) ? 'failed ' : ''}${timed ? 'inwindow' : ''}`.trim();
                    tr.innerHTML = `<td class="time-cell">${timed ? esc(offsetLabel(entry.timestamp)) : 'Unknown'}</td><td>${esc(entry.source || 'Unknown')}</td><td>${esc(entry.level || 'Unknown')}</td><td>${esc(entry.message || 'Unknown')}</td><td><button type="button" class="secondary">View message details</button></td>`;
                    tr.querySelector('button').addEventListener('click', () => {
                      consoleDetail.hidden = false;
                      consoleDetail.textContent = JSON.stringify(entry, null, 2);
                    });
                    consoleRows.appendChild(tr);
                  });
                  updateConsoleSortHeaders();
                }
                const websocketPanel = document.getElementById('websocket-panel');
                const websocketRows = document.getElementById('websocket-rows');
                const websocketDetail = document.getElementById('websocket-detail');
                const websocketDirectionFilter = document.getElementById('websocket-direction-filter');
                const websocketTypeFilter = document.getElementById('websocket-type-filter');
                const websocketTextFilter = document.getElementById('websocket-text-filter');
                function populateWebSocketFilters(){
                  const add = (select, values) => values.forEach(value => {
                    const option = document.createElement('option');
                    option.value = value; option.textContent = value; select.appendChild(option);
                  });
                  add(websocketDirectionFilter, [...new Set(webSockets.map(entry => String(entry.direction || 'none')))].sort());
                  add(websocketTypeFilter, [...new Set(webSockets.map(entry => String(entry.type || 'unknown')))].sort());
                }
                function renderWebSockets(){
                  websocketRows.innerHTML = '';
                  websocketDetail.hidden = true;
                  const query = websocketTextFilter.value.trim().toLowerCase();
                  const visible = webSockets.filter(entry =>
                    (!websocketDirectionFilter.value || String(entry.direction || 'none') === websocketDirectionFilter.value)
                    && (!websocketTypeFilter.value || String(entry.type || 'unknown') === websocketTypeFilter.value)
                    && (!query || [entry.url, entry.text, entry.sha256, entry.reason]
                      .filter(Boolean).join(' ').toLowerCase().includes(query)));
                  document.getElementById('websocket-result-count').textContent = `${visible.length} WebSocket ${visible.length === 1 ? 'event' : 'events'}`;
                  document.getElementById('websocket-hint').textContent = !webSockets.length
                    ? 'WebSocket capture is unavailable for this provider or no socket activity was observed.'
                    : !visible.length ? 'No WebSocket events match the active filters.'
                    : 'Captured lifecycle and frame evidence is bounded and redacted before display.';
                  visible.forEach(entry => {
                    const tr = document.createElement('tr');
                    const payload = entry.text || entry.sha256 || entry.reason || 'None';
                    tr.innerHTML = `<td>${esc(entry.type || 'Unknown')}</td><td>${esc(entry.direction || 'None')}</td><td>${esc(entry.url || 'Unavailable')}</td><td>${esc(entry.opcode == null ? 'N/A' : entry.opcode)}</td><td>${esc(payload)}</td><td><button type="button" class="secondary">Inspect event</button></td>`;
                    tr.querySelector('button').addEventListener('click', () => {
                      websocketDetail.hidden = false;
                      websocketDetail.textContent = JSON.stringify(entry, null, 2);
                    });
                    websocketRows.appendChild(tr);
                  });
                }
                const mobileActions = () => actions.filter(action =>
                    String(action.category || '').startsWith('mobile/'));
                const mobileRows = document.getElementById('mobile-rows');
                const mobileDetail = document.getElementById('mobile-detail');
                const mobileResultCount = document.getElementById('mobile-result-count');
                let mobileCategory = 'all';
                const mobileLabels = {
                  'mobile/app':'App', 'mobile/context':'Context', 'mobile/device':'Device',
                  'mobile/logs':'Logs', 'mobile/performance':'Performance',
                  'mobile/recording':'Recording', 'mobile/evidence':'Evidence'
                };
                function mobileLabel(action){
                  return mobileLabels[String(action.category || '')] || 'Other';
                }
                function mobileSummary(action){
                  const entries = Object.entries(action.metadata || {});
                  return entries.length ? entries.map(([key, value]) => `${key}=${value}`).join(', ') : 'No metadata';
                }
                function renderMobile(){
                  mobileRows.innerHTML = '';
                  mobileDetail.hidden = true;
                  mobileDetail.textContent = '';
                  const all = mobileActions();
                  const range = selectedWindow();
                  const visible = all.filter(action =>
                    (actionStartMs(action) == null || actionInWindow(action, range))
                    && (mobileCategory === 'all' || action.category === mobileCategory));
                  mobileResultCount.textContent = `${visible.length} mobile ${visible.length === 1 ? 'action' : 'actions'}`;
                  document.getElementById('mobile-hint').textContent = !all.length
                    ? 'No mobile actions were recorded.'
                    : !visible.length ? 'No mobile actions match the selected range and category.'
                    : 'Inspect captured mobile operations and safe scalar metadata.';
                  visible.forEach(action => {
                    const tr = document.createElement('tr');
                    const timed = actionStartMs(action) != null;
                    tr.className = `${statusClass(action.status)}${timed ? ' inwindow' : ''}`;
                    tr.innerHTML = `<td class="time-cell">${timed ? esc(offsetLabel(actionStartMs(action))) : 'Unknown'}</td><td>${esc(mobileLabel(action))}</td><td>${esc(action.name || 'Unknown')}</td><td>${esc(action.status || 'Unknown')}</td><td>${esc(mobileSummary(action))}</td><td><button type="button" class="secondary">Inspect action</button></td>`;
                    tr.querySelector('button').addEventListener('click', () => {
                      selectAction(action, false);
                      mobileDetail.hidden = false;
                      mobileDetail.textContent = JSON.stringify(action, null, 2);
                    });
                    mobileRows.appendChild(tr);
                  });
                  document.querySelectorAll('#mobile-category-filter button').forEach(button => {
                    const selectedCategory = button.dataset.mobileCategory === mobileCategory;
                    button.classList.toggle('selected', selectedCategory);
                    button.setAttribute('aria-pressed', String(selectedCategory));
                  });
                }
                const artifactRows = document.getElementById('artifact-rows');
                const artifactResultCount = document.getElementById('artifact-result-count');
                const nativeTraceHandoff = document.getElementById('native-trace-handoff');
                function renderArtifacts(){
                  artifactRows.innerHTML = '';
                  artifactResultCount.textContent = `${artifacts.length} trace ${artifacts.length === 1 ? 'artifact' : 'artifacts'}`;
                  document.getElementById('artifact-hint').textContent = artifacts.length
                    ? 'Artifact paths are relative to the downloaded SHAFT trace ZIP.'
                    : 'No artifact graph was recorded for this trace.';
                  artifacts.forEach(artifact => {
                    const tr = document.createElement('tr');
                    const status = artifact.omitted ? 'Omitted' : 'Available';
                    const metadata = artifact.metadata || {};
                    const reason = artifact.omitted
                      ? metadata.omissionReason || 'No omission reason was recorded.' : '';
                    const size = metadata.sizeBytes ? metadata.sizeBytes + ' B' : '';
                    const digest = metadata.sha256 ? metadata.sha256.slice(0, 12) : '';
                    tr.innerHTML = `<td>${esc(artifact.path || 'Unknown')}</td><td>${esc(artifact.kind || 'Unknown')}</td><td>${esc(artifact.mimeType || 'Unknown')}</td><td>${status}</td><td>${esc(size)}</td><td>${esc(digest)}</td><td>${esc(reason)}</td>`;
                    artifactRows.appendChild(tr);
                  });
                  const nativeTrace = artifacts.find(artifact => artifact.kind === 'native-trace');
                  nativeTraceHandoff.hidden = !nativeTrace;
                  nativeTraceHandoff.textContent = !nativeTrace ? '' : nativeTrace.omitted
                    ? `Native Playwright trace omitted: ${(nativeTrace.metadata && nativeTrace.metadata.omissionReason) || 'No omission reason was recorded.'}`
                    : `Native Playwright trace is available as ${nativeTrace.path} in the downloaded SHAFT trace ZIP. Extract it, then open it with Playwright show-trace ${nativeTrace.path}.`;
                }
                function sourceText(){
                  const source = trace.source || {};
                  const header = `${source.file || source.frame || 'Unknown source'}:${source.line || '?'}`;
                  if (!source.fileContent) {
                    return header + '\\n\\n' + (source.snippet || 'No source context was captured.');
                  }
                  const failingLine = parseInt(source.line, 10);
                  const lines = source.fileContent.split('\\n');
                  const numbered = lines.map((text, index) =>
                      (index + 1 === failingLine ? '> ' : '  ') + String(index + 1).padStart(4) + ': ' + text);
                  return header + '\\n\\n' + numbered.join('\\n');
                }
                const domSnapshotPanel = document.getElementById('dom-snapshot-panel');
                const domSnapshotFrame = document.getElementById('dom-snapshot-frame');
                const snapshotCsp = `<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; img-src data:; connect-src 'none'; object-src 'none'; base-uri 'none'; form-action 'none'">`;
                function snapshotDocument(html){
                  const parsed = new DOMParser().parseFromString(
                      html || '<p>No DOM snapshot captured for this action.</p>', 'text/html');
                  parsed.querySelectorAll('script,style,link,base,meta,iframe,object,embed,video,audio,source,track')
                      .forEach(element => element.remove());
                  const resourceAttributes = ['src', 'srcset', 'href', 'xlink:href', 'poster', 'background',
                    'action', 'formaction', 'ping', 'cite', 'manifest', 'style'];
                  parsed.querySelectorAll('*').forEach(element =>
                    resourceAttributes.forEach(attribute => element.removeAttribute(attribute)));
                  return snapshotCsp + parsed.documentElement.outerHTML;
                }
                function nativeSnapshot(name){
                  const snapshot = name && nativeSnapshots[name];
                  return snapshot && snapshot.status === 'available' && snapshot.content
                    ? snapshot.content : '';
                }
                function preferredSnapshot(action, side){
                  const native = nativeActionFor(action);
                  const nativeName = native && native[side + 'Snapshot'];
                  const content = nativeSnapshot(nativeName);
                  if (content) return snapshotCsp + content;
                  const fallback = side === 'after' ? action.domSnapshotAfter : action.domSnapshotBefore;
                  return fallback ? snapshotDocument(fallback) : '';
                }
                let selectedDomSide = 'before';
                function renderDomSnapshot(){
                  const action = selected || {};
                  const html = domSnapshotFrame && preferredSnapshot(action, selectedDomSide);
                  if (domSnapshotFrame) {
                    domSnapshotFrame.srcdoc = html || snapshotDocument('');
                  }
                  document.querySelectorAll('#dom-snapshot-tabs button').forEach(button =>
                      button.classList.toggle('selected', button.dataset.dom === selectedDomSide));
                }
                const screenshotPanel = document.getElementById('screenshot-panel');
                const screenshotImage = document.getElementById('screenshot-image');
                const screenshotEmpty = document.getElementById('screenshot-empty');
                function renderScreenshot(){
                  const action = selected || {};
                  const hasScreenshot = Boolean(action.screenshot);
                  screenshotImage.hidden = !hasScreenshot;
                  screenshotEmpty.hidden = hasScreenshot;
                  if (hasScreenshot) {
                    screenshotImage.src = 'data:image/png;base64,' + action.screenshot;
                  }
                }
                const comparisonPanel = document.getElementById('comparison-panel');
                const comparisonBefore = document.getElementById('comparison-before');
                const comparisonInput = document.getElementById('comparison-input');
                const comparisonAction = document.getElementById('comparison-action');
                const comparisonAfter = document.getElementById('comparison-after');
                function renderComparison(){
                  const action = selected || {};
                  const native = nativeActionFor(action);
                  const before = preferredSnapshot(action, 'before');
                  const input = nativeSnapshot(native && native.inputSnapshot);
                  const after = preferredSnapshot(action, 'after');
                  const hasBefore = Boolean(before);
                  const hasInput = Boolean(input);
                  const hasAction = hasInput || Boolean(action.screenshot);
                  const hasAfter = Boolean(after);
                  comparisonBefore.hidden = !hasBefore;
                  document.getElementById('comparison-before-empty').hidden = hasBefore;
                  comparisonBefore.srcdoc = before;
                  comparisonInput.hidden = !hasInput;
                  comparisonInput.srcdoc = hasInput ? snapshotCsp + input : '';
                  comparisonAction.hidden = hasInput || !action.screenshot;
                  document.getElementById('comparison-action-empty').hidden = hasAction;
                  comparisonAction.src = !hasInput && action.screenshot ? 'data:image/png;base64,' + action.screenshot : '';
                  comparisonAfter.hidden = !hasAfter;
                  document.getElementById('comparison-after-empty').hidden = hasAfter;
                  comparisonAfter.srcdoc = after;
                }
                const nativeEvidencePanel = document.getElementById('native-evidence-panel');
                const nativeEvidenceRows = document.getElementById('native-evidence-rows');
                function renderNativeEvidence(){
                  nativeEvidenceRows.innerHTML = '';
                  document.getElementById('native-evidence-hint').textContent = playwright.status === 'available'
                    ? `${nativeActions.length} native Playwright ${nativeActions.length === 1 ? 'action' : 'actions'} retained offline.`
                    : `Native Playwright evidence ${playwright.status || 'unavailable'}${playwright.reason ? ': ' + playwright.reason : '.'}`;
                  const selectedNative = nativeActionFor(selected || {});
                  nativeActions.forEach(native => {
                    const tr = document.createElement('tr');
                    const correlation = selectedNative && selectedNative.callId === native.callId
                      ? 'Selected SHAFT action' : (playwright.correlations || []).some(item => item.playwrightCallId === native.callId)
                        ? 'Correlated' : 'Native only';
                    tr.innerHTML = `<td>${esc(correlation)}</td><td>${esc(native.title || native.method || native.callId || 'Unknown')}</td><td>${esc(native.source || native.sourceReason || 'Unavailable')}</td><td>${esc((native.logs || []).join('\\n') || 'None')}</td><td>${esc(native.error || 'None')}<br><button type="button" class="secondary">Inspect native action</button></td>`;
                    tr.querySelector('button').addEventListener('click', () => {
                      selectedNativeAction = native;
                      document.getElementById('details-title').textContent = `Native action: ${native.title || native.method || native.callId}`;
                      details.innerHTML = row('Provider', 'Playwright') + row('Call ID', native.callId)
                        + row('Source', native.source || native.sourceReason) + row('Logs', (native.logs || []).join('\\n'))
                        + row('Error', native.error);
                      renderTab('comparison');
                    });
                    nativeEvidenceRows.appendChild(tr);
                  });
                }
                function renderTab(tab){
                  const action = selected || {};
                  const panels = {timeline: timelinePanel, nativeEvidence: nativeEvidencePanel, comparison: comparisonPanel, domSnapshot: domSnapshotPanel, screenshot: screenshotPanel, network: networkPanel, console: consolePanel, webSockets: websocketPanel, mobile: document.getElementById('mobile-panel'), artifacts: document.getElementById('artifact-panel')};
                  tabContent.hidden = tab in panels;
                  Object.entries(panels).forEach(([name, panel]) => panel.hidden = name !== tab);
                  if (tab === 'timeline') {
                    renderTimeline();
                  } else if (tab === 'nativeEvidence') {
                    renderNativeEvidence();
                  } else if (tab === 'comparison') {
                    renderComparison();
                  } else if (tab === 'domSnapshot') {
                    renderDomSnapshot();
                  } else if (tab === 'screenshot') {
                    renderScreenshot();
                  } else if (tab === 'network') {
                    renderNetwork();
                  } else if (tab === 'console') {
                    renderConsole();
                  } else if (tab === 'webSockets') {
                    renderWebSockets();
                  } else if (tab === 'mobile') {
                    renderMobile();
                  } else if (tab === 'artifacts') {
                    renderArtifacts();
                  } else if (tab === 'source') {
                    tabContent.textContent = sourceText();
                  } else if (tab === 'log') {
                    tabContent.textContent = Array.isArray(trace.timeline) && trace.timeline.length ? trace.timeline.join('\\n') : 'No test log lines were recorded.';
                  } else {
                    const data = tab === 'json' ? trace
                        : tab === 'exception' && action.exception && (action.exception.type || action.exception.message) ? action.exception
                        : tab === 'browserObservability' ? evidence.browserObservability
                        : trace[tab];
                    tabContent.textContent = typeof data === 'string' ? data : JSON.stringify(data || {}, null, 2);
                  }
                  document.querySelectorAll('#action-tabs button').forEach(button => button.classList.toggle('selected', button.dataset.tab === tab));
                }
                async function copyJson(){
                  await navigator.clipboard.writeText(JSON.stringify(trace, null, 2));
                }
                actionSearch.addEventListener('input', renderActions);
                rangeStart.addEventListener('input', () => applyRangeInputs('none'));
                rangeEnd.addEventListener('input', () => applyRangeInputs('none'));
                rangeStart.addEventListener('change', () => applyRangeInputs('push'));
                rangeEnd.addEventListener('change', () => applyRangeInputs('push'));
                document.getElementById('show-all-range').addEventListener('click', () => {
                  rangeStartMs = baseTime;
                  rangeEndMs = traceEnd;
                  updateHash('push');
                  renderNavigator();
                  renderActions();
                  renderDetails();
                });
                filmstrip.addEventListener('keydown', event => {
                  if (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight') return;
                  const options = [...filmstrip.querySelectorAll('button[role="option"]')];
                  const current = Math.max(0, options.indexOf(document.activeElement));
                  const next = event.key === 'ArrowRight'
                      ? Math.min(options.length - 1, current + 1)
                      : Math.max(0, current - 1);
                  if (options[next]) {
                    event.preventDefault();
                    const nextActionId = options[next].dataset.actionId;
                    options[next].click();
                    const renderedOption = [...filmstrip.querySelectorAll('button[role="option"]')]
                        .find(option => option.dataset.actionId === nextActionId);
                    if (renderedOption) renderedOption.focus();
                  }
                });
                function restoreLocationState(){
                  const state = hashState();
                  const action = state.actionId ? actions.find(candidate => candidate.id === state.actionId) : null;
                  if (!action) return;
                  const hasRange = state.params.has('start') && state.params.has('end');
                  const startOffset = hasRange ? Number(state.params.get('start')) : NaN;
                  const endOffset = hasRange ? Number(state.params.get('end')) : NaN;
                  if (baseTime != null && Number.isFinite(startOffset) && Number.isFinite(endOffset)) {
                    rangeStartMs = baseTime + Math.max(0, Math.min(traceDuration, Math.min(startOffset, endOffset)));
                    rangeEndMs = baseTime + Math.max(0, Math.min(traceDuration, Math.max(startOffset, endOffset)));
                  } else {
                    const start = actionStartMs(action);
                    if (start != null) {
                      rangeStartMs = start;
                      rangeEndMs = Math.max(start, start + Math.max(0, action.durationMs || 0));
                    }
                  }
                  selectAction(action, false, 'none');
                }
                window.addEventListener('popstate', restoreLocationState);
                window.addEventListener('hashchange', restoreLocationState);
                document.querySelectorAll('#timeline-filters button').forEach(button => button.addEventListener('click', () => {
                  timelineFilter = button.dataset.filter;
                  document.querySelectorAll('#timeline-filters button').forEach(other => other.classList.toggle('selected', other === button));
                  renderTimeline();
                }));
                [networkMethodFilter, networkStatusFilter, networkTextFilter]
                    .forEach(control => control.addEventListener('input', renderNetwork));
                document.querySelectorAll('[data-network-sort]').forEach(button => button.addEventListener('click', () => {
                  const key = button.dataset.networkSort;
                  networkSort = networkSort.key === key
                    ? {key, direction:networkSort.direction === 'ascending' ? 'descending' : 'ascending'}
                    : {key, direction:'ascending'};
                  renderNetwork();
                }));
                [consoleSourceFilter, consoleLevelFilter, consoleTextFilter]
                    .forEach(control => control.addEventListener('input', renderConsole));
                [websocketDirectionFilter, websocketTypeFilter, websocketTextFilter]
                    .forEach(control => control.addEventListener('input', renderWebSockets));
                document.querySelectorAll('[data-console-sort]').forEach(button => button.addEventListener('click', () => {
                  const key = button.dataset.consoleSort;
                  consoleSort = consoleSort.key === key
                    ? {key, direction:consoleSort.direction === 'ascending' ? 'descending' : 'ascending'}
                    : {key, direction:'ascending'};
                  renderConsole();
                }));
                document.querySelectorAll('#mobile-category-filter button').forEach(button => button.addEventListener('click', () => {
                  mobileCategory = button.dataset.mobileCategory;
                  renderMobile();
                }));
                document.querySelectorAll('#action-tabs button').forEach(button => button.addEventListener('click', () => renderTab(button.dataset.tab)));
                document.querySelectorAll('#dom-snapshot-tabs button').forEach(button => button.addEventListener('click', () => { selectedDomSide = button.dataset.dom; renderDomSnapshot(); }));
                populateNetworkFilters();
                populateConsoleFilters();
                populateWebSocketFilters();
                renderSummary();
                renderNavigator();
                renderActions();
                renderDetails();
                </script>
                </body>
                </html>
                """;
    }

    static List<String> renderTraceZip(Path target, String json, String html, String networkJson,
                                        Map<String, byte[]> screenshots, Path nativeTrace, long maxBytes,
                                        String omissionMarker) {
        TraceArchiveWriter.Entry nativeEntry = nativeTrace == null
                ? null
                : TraceArchiveWriter.Entry.optionalFile(nativeTrace.getFileName().toString(), nativeTrace);
        return renderTraceZip(target, json, html, networkJson, screenshots, nativeEntry, null,
                maxBytes, maxBytes, omissionMarker, aggregateOmissionMarker());
    }

    @SuppressWarnings({"PMD.ExcessiveParameterList", "PMD.NPathComplexity"})
    private static List<String> renderTraceZip(Path target, String json, String html, String networkJson,
                                                Map<String, byte[]> screenshots,
                                                TraceArchiveWriter.Entry nativeEntry,
                                                TraceArtifactManifest manifest,
                                                long maxBytes, long maxTotalBytes, String omissionMarker,
                                                String aggregateReason) {
        List<TraceArchiveWriter.Entry> entries = new ArrayList<>();
        Map<String, String> omittedReasons = manifest == null ? Map.of() : manifest.references().stream()
                        .filter(TraceArtifactReference::omitted)
                        .collect(java.util.stream.Collectors.toUnmodifiableMap(TraceArtifactReference::path,
                                reference -> reference.metadata().getOrDefault("omissionReason", aggregateReason),
                                FailureTraceReporter::mergeSharedOmissionReason));
        entries.add(TraceArchiveWriter.Entry.requiredText("shaft-trace.json", json));
        entries.add(TraceArchiveWriter.Entry.requiredText("SHAFT Trace Report.html", html));
        entries.add(omittedReasons.containsKey("shaft-network.har")
                ? TraceArchiveWriter.Entry.omitted("shaft-network.har", omittedReasons.get("shaft-network.har"))
                : TraceArchiveWriter.Entry.optionalBytes("shaft-network.har",
                BrowserObservabilityRecorder.networkHarJson(networkJson).getBytes(StandardCharsets.UTF_8)));
        Map<String, TraceArtifactReference> screenshotReferences = new LinkedHashMap<>();
        if (manifest != null) {
            manifest.references().stream().filter(reference -> "screenshot".equals(reference.kind()))
                    .forEach(reference -> screenshotReferences.put(reference.id().replaceFirst("^screenshot-", ""),
                            reference));
        }
        Set<String> addedScreenshotPaths = new LinkedHashSet<>();
        for (Map.Entry<String, byte[]> entry : screenshots.entrySet()) {
            TraceArtifactReference reference = screenshotReferences.get(entry.getKey());
            String path = reference == null ? "screenshots/" + entry.getKey() + ".png" : reference.path();
            if (!addedScreenshotPaths.add(path)) {
                continue;
            }
            entries.add(omittedReasons.containsKey(path)
                    ? TraceArchiveWriter.Entry.omitted(path, omittedReasons.get(path))
                    : TraceArchiveWriter.Entry.optionalBytes(path, entry.getValue()));
        }
        if (manifest != null) {
            manifest.resourceBytes().forEach((path, bytes) -> entries.add(omittedReasons.containsKey(path)
                    ? TraceArchiveWriter.Entry.omitted(path, omittedReasons.get(path))
                    : TraceArchiveWriter.Entry.optionalBytes(path, bytes)));
        }
        if (nativeEntry != null) {
            entries.add(omittedReasons.containsKey(nativeEntry.name())
                    ? TraceArchiveWriter.Entry.omitted(nativeEntry.name(), omittedReasons.get(nativeEntry.name()))
                    : nativeEntry);
        }
        try {
            return TraceArchiveWriter.write(target, entries, maxBytes, maxTotalBytes, omissionMarker).omittedPaths();
        } catch (IOException e) {
            throw new IllegalStateException("Could not create SHAFT trace zip.", e);
        }
    }

    static TraceArchiveBundle convergeTraceArchive(Path target, String json, String networkJson,
                                                    Map<String, byte[]> screenshots,
                                                    TraceArtifactManifest manifest,
                                                    long maxEntryBytes, long maxTotalBytes,
                                                    String omissionMarker, List<String> plannedOmissions) {
        List<String> omitted = List.copyOf(plannedOmissions);
        String currentJson = json;
        String html = renderTraceHtml(currentJson, omitted);
        Map<String, byte[]> currentScreenshots = screenshots;
        int optionalEntries = manifest == null ? 0 : manifest.references().size();
        for (int pass = 0; pass <= optionalEntries; pass++) {
            if (hasSnapshotContent(currentJson)
                    && (utf8Size(currentJson) > maxEntryBytes || utf8Size(html) > maxEntryBytes)) {
                currentJson = omitSnapshotForBudget(currentJson);
                html = renderTraceHtml(currentJson, omitted);
            }
            if (hasInlineActionSnapshots(currentJson)
                    && (utf8Size(currentJson) > maxEntryBytes || utf8Size(html) > maxEntryBytes)) {
                currentJson = omitInlineActionSnapshotsForBudget(currentJson, manifest);
                html = renderTraceHtml(currentJson, omitted);
            }
            if (hasAvailablePlaywrightEvidence(currentJson)
                    && (utf8Size(currentJson) > maxEntryBytes || utf8Size(html) > maxEntryBytes)) {
                currentJson = omitPlaywrightEvidenceForBudget(currentJson);
                html = renderTraceHtml(currentJson, omitted);
            }
            if (hasActionEvidence(currentJson)
                    && (utf8Size(currentJson) > maxEntryBytes || utf8Size(html) > maxEntryBytes)) {
                ActionCompaction compacted = compactActionEvidenceForBudget(currentJson, omitted, maxEntryBytes);
                currentJson = compacted.json();
                omitted = omitted.stream().filter(path -> !compacted.removedArtifactPaths().contains(path)).toList();
                if (manifest != null) {
                    manifest.retainActionArtifacts(compacted.retainedArtifactIds());
                }
                currentScreenshots = filterScreenshots(currentScreenshots, compacted.retainedArtifactIds());
                html = renderTraceHtml(currentJson, omitted);
            }
            TraceArchiveWriter.Entry nativeEntry = manifest == null ? null : manifest.nativeEntry();
            List<String> actual = renderTraceZip(target, currentJson, html, networkJson, currentScreenshots,
                    nativeEntry, manifest, maxEntryBytes, maxTotalBytes, omissionMarker, omissionMarker);
            List<String> merged = mergeOmitted(omitted, actual);
            if (merged.equals(omitted)) {
                return new TraceArchiveBundle(currentJson, html, omitted,
                        manifest == null ? List.of() : manifest.references());
            }
            omitted = merged;
            if (manifest != null) {
                manifest.markOmitted(actual, omissionMarker);
                currentJson = reconcileArtifactOmissions(currentJson, manifest.references());
            }
            html = renderTraceHtml(currentJson, omitted);
        }
        throw new IllegalStateException("Trace archive omissions did not stabilize within the bounded pass count.");
    }

    private static boolean hasAvailablePlaywrightEvidence(String json) {
        try {
            return "available".equals(JSON.readTree(json).path("evidence").path("playwright")
                    .path("status").asText());
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private static boolean hasSnapshotContent(String json) {
        try {
            return !JSON.readTree(json).path("snapshot").path("content").asText().isEmpty();
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private static boolean hasInlineActionSnapshots(String json) {
        try {
            for (JsonNode action : JSON.readTree(json).path("evidence").path("actions")) {
                if (!action.path("domSnapshotBefore").asText().isEmpty()
                        || !action.path("domSnapshotAfter").asText().isEmpty()) {
                    return true;
                }
            }
            return false;
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private static boolean hasActionEvidence(String json) {
        try {
            JsonNode root = JSON.readTree(json);
            return !root.path("evidence").path("actions").isEmpty()
                    || !root.path("session").path("events").isEmpty();
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private static long utf8Size(String value) {
        return value.getBytes(StandardCharsets.UTF_8).length;
    }

    private static String omitPlaywrightEvidenceForBudget(String json) {
        JsonNode parsed = JSON.readTree(json);
        if (!(parsed instanceof ObjectNode root) || !(root.path("evidence") instanceof ObjectNode evidence)) {
            return json;
        }
        ObjectNode omitted = JSON.createObjectNode();
        omitted.put("status", "omitted-budget");
        omitted.put("reason", "Playwright action evidence exceeded its bounded report budget.");
        omitted.putArray("actions");
        omitted.putArray("correlations");
        evidence.set("playwright", omitted);
        removePlaywrightCorrelationMetadata(evidence.path("actions"));
        removePlaywrightCorrelationMetadata(root.path("session").path("events"));
        return JSON.writeValueAsString(root);
    }

    private static String omitSnapshotForBudget(String json) {
        JsonNode parsed = JSON.readTree(json);
        if (!(parsed instanceof ObjectNode root) || !(root.path("snapshot") instanceof ObjectNode snapshot)) {
            return json;
        }
        snapshot.put("fidelity", "omitted");
        snapshot.put("status", "omitted-budget");
        snapshot.put("reason", "Browser snapshot exceeded the bounded report budget.");
        snapshot.put("type", "omitted-budget");
        snapshot.put("content", "");
        snapshot.put("byteCount", "0");
        snapshot.put("truncated", "false");
        return JSON.writeValueAsString(root);
    }

    private static String omitInlineActionSnapshotsForBudget(String json, TraceArtifactManifest manifest) {
        JsonNode parsed = JSON.readTree(json);
        if (!(parsed instanceof ObjectNode root)) {
            return json;
        }
        JsonNode actions = root.path("evidence").path("actions");
        if (!actions.isArray()) {
            return json;
        }
        Set<String> resourceActions = manifest == null ? Set.of() : manifest.references().stream()
                .filter(reference -> "dom-snapshot".equals(reference.kind()))
                .filter(reference -> !reference.omitted())
                .map(reference -> reference.metadata().getOrDefault("actionId", ""))
                .filter(actionId -> !actionId.isEmpty())
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        actions.forEach(action -> {
            if (!(action instanceof ObjectNode object)) {
                return;
            }
            boolean hadSnapshot = !object.path("domSnapshotBefore").asText().isEmpty()
                    || !object.path("domSnapshotAfter").asText().isEmpty();
            if (!hadSnapshot) {
                return;
            }
            object.put("domSnapshotBefore", "");
            object.put("domSnapshotAfter", "");
            object.put("domSnapshotInlineStatus",
                    resourceActions.contains(object.path("id").asText()) ? "resource-only" : "omitted-budget");
            object.put("domSnapshotInlineReason",
                    "Inline DOM snapshots exceeded the bounded report entry budget.");
        });
        return JSON.writeValueAsString(root);
    }

    private static ActionCompaction compactActionEvidenceForBudget(String json, List<String> omitted,
                                                                    long maxEntryBytes) {
        JsonNode parsed = JSON.readTree(json);
        if (!(parsed instanceof ObjectNode root)) {
            return new ActionCompaction(json, Set.of(), Set.of());
        }
        List<JsonNode> actions = withoutOmissionMarker(copyNodes(root.path("evidence").path("actions")));
        List<JsonNode> events = withoutOmissionMarker(copyNodes(root.path("session").path("events")));
        int priorOmitted = Math.max(omittedActionCount(root.path("evidence").path("actions")),
                omittedActionCount(root.path("session").path("events")));
        int total = Math.max(actions.size(), events.size());
        int low = 0;
        int high = total;
        ActionCompaction best = null;
        while (low <= high) {
            int retained = low + (high - low) / 2;
            ActionCompaction candidate = compactedActionJson(root, actions, events, retained, total, priorOmitted);
            List<String> candidateOmissions = omitted.stream()
                    .filter(path -> !candidate.removedArtifactPaths().contains(path)).toList();
            String candidateHtml = renderTraceHtml(candidate.json(), candidateOmissions);
            if (utf8Size(candidate.json()) <= maxEntryBytes && utf8Size(candidateHtml) <= maxEntryBytes) {
                best = candidate;
                low = retained + 1;
            } else {
                high = retained - 1;
            }
        }
        if (best == null) {
            throw new IllegalStateException("Trace action metadata exceeded the required entry budget after bounded compaction.");
        }
        return best;
    }

    private static List<JsonNode> withoutOmissionMarker(List<JsonNode> values) {
        if (values.isEmpty() || !isActionOmissionMarker(values.getLast())) {
            return values;
        }
        return List.copyOf(values.subList(0, values.size() - 1));
    }

    private static int omittedActionCount(JsonNode values) {
        if (!values.isArray() || values.isEmpty()) {
            return 0;
        }
        JsonNode last = values.get(values.size() - 1);
        return isActionOmissionMarker(last)
                ? last.path("metadata").path("omittedCount").asInt(0) : 0;
    }

    private static boolean isActionOmissionMarker(JsonNode value) {
        String id = value.path("id").asText();
        boolean ownedId = "action-limit".equals(id) || "action-budget".equals(id)
                || id.endsWith("/action-limit") || id.endsWith("/action-budget");
        return ownedId && "omitted-actions".equals(value.path("name").asText())
                && positiveInteger(value.path("metadata").path("omittedCount").asText());
    }

    private static boolean positiveInteger(String value) {
        try {
            return Integer.parseInt(value) > 0;
        } catch (NumberFormatException exception) {
            return false;
        }
    }

    private static List<JsonNode> copyNodes(JsonNode array) {
        if (!array.isArray()) {
            return List.of();
        }
        List<JsonNode> values = new ArrayList<>();
        array.forEach(value -> values.add(value.deepCopy()));
        return List.copyOf(values);
    }

    private static ActionCompaction compactedActionJson(ObjectNode original, List<JsonNode> actions,
                                                        List<JsonNode> events, int retained, int total,
                                                        int priorOmitted) {
        ObjectNode copy = original.deepCopy();
        var compactActions = copy.withObject("evidence").putArray("actions");
        for (int index = 0; index < Math.min(retained, actions.size()); index++) {
            compactActions.add(actions.get(index));
        }
        var compactEvents = copy.withObject("session").putArray("events");
        for (int index = 0; index < Math.min(retained, events.size()); index++) {
            compactEvents.add(events.get(index));
        }
        Set<String> retainedArtifactIds = new LinkedHashSet<>();
        compactEvents.forEach(event -> event.path("artifactIds").forEach(id -> retainedArtifactIds.add(id.asText())));
        Set<String> originalActionArtifactPaths = new LinkedHashSet<>();
        Set<String> retainedActionArtifactPaths = new LinkedHashSet<>();
        if (copy.path("session").path("artifacts") instanceof ArrayNode artifacts) {
            List<JsonNode> originalArtifacts = copyNodes(artifacts);
            originalArtifacts.stream().filter(FailureTraceReporter::isActionArtifact)
                    .forEach(artifact -> originalActionArtifactPaths.add(artifact.path("path").asText()));
            List<JsonNode> kept = originalArtifacts.stream().filter(artifact -> {
                String kind = artifact.path("kind").asText();
                return !("dom-snapshot".equals(kind) || "screenshot".equals(kind))
                        || retainedArtifactIds.contains(artifact.path("id").asText());
            }).toList();
            kept.stream().filter(FailureTraceReporter::isActionArtifact)
                    .forEach(artifact -> retainedActionArtifactPaths.add(artifact.path("path").asText()));
            artifacts.removeAll();
            kept.forEach(artifacts::add);
        }
        if (retained < total) {
            int omittedCount = priorOmitted + total - retained;
            compactActions.add(actionOmissionNode(omittedCount));
            compactEvents.add(eventOmissionNode(copy.path("session").path("id").asText(), omittedCount));
        }
        originalActionArtifactPaths.removeAll(retainedActionArtifactPaths);
        return new ActionCompaction(JSON.writeValueAsString(copy), Set.copyOf(retainedArtifactIds),
                Set.copyOf(originalActionArtifactPaths));
    }

    private static boolean isActionArtifact(JsonNode artifact) {
        String kind = artifact.path("kind").asText();
        return "dom-snapshot".equals(kind) || "screenshot".equals(kind);
    }

    private static ObjectNode actionOmissionNode(int omittedCount) {
        ObjectNode marker = JSON.createObjectNode();
        marker.put("id", "action-budget");
        marker.put("category", "trace");
        marker.put("name", "omitted-actions");
        marker.put("status", "skipped");
        marker.put("message", omittedCount + " newest actions were omitted to fit the trace report budget.");
        marker.putObject("metadata").put("omittedCount", String.valueOf(omittedCount));
        return marker;
    }

    private static ObjectNode eventOmissionNode(String sessionId, int omittedCount) {
        ObjectNode marker = JSON.createObjectNode();
        marker.put("id", (sessionId == null || sessionId.isBlank() ? "session" : sessionId) + "/action-budget");
        marker.put("backend", "UNKNOWN");
        marker.put("category", "trace");
        marker.put("name", "omitted-actions");
        marker.put("status", "SKIPPED");
        marker.put("startedAt", Instant.EPOCH.toString());
        marker.put("durationMs", 0L);
        marker.put("source", "");
        marker.put("target", "");
        marker.put("message", omittedCount + " newest actions were omitted to fit the trace report budget.");
        marker.putArray("artifactIds");
        marker.putObject("metadata").put("omittedCount", String.valueOf(omittedCount));
        return marker;
    }

    private static Map<String, byte[]> filterScreenshots(Map<String, byte[]> screenshots,
                                                         Set<String> retainedArtifactIds) {
        Map<String, byte[]> retained = new LinkedHashMap<>();
        screenshots.forEach((id, bytes) -> {
            if (retainedArtifactIds.contains("screenshot-" + id)) {
                retained.put(id, bytes);
            }
        });
        return Map.copyOf(retained);
    }

    private record ActionCompaction(String json, Set<String> retainedArtifactIds, Set<String> removedArtifactPaths) {
    }

    private static void removePlaywrightCorrelationMetadata(JsonNode actions) {
        if (!actions.isArray()) {
            return;
        }
        actions.values().forEach(action -> {
            if (action.path("metadata") instanceof ObjectNode metadata) {
                metadata.remove("playwrightCallId");
                metadata.remove("playwrightStepId");
                metadata.remove("playwrightCorrelation");
            }
        });
    }

    private static TraceEventRecorder.ActionEvent withoutScreenshot(TraceEventRecorder.ActionEvent action) {
        return new TraceEventRecorder.ActionEvent(action.id(), action.backend(), action.category(), action.name(),
                action.status(), action.startTime(), action.durationMs(), action.locator(), action.url(), action.caller(),
                action.message(), action.exceptionType(), action.exceptionMessage(), action.attachments(),
                action.metadata(), action.actionability(), action.domSnapshotBefore(), action.domSnapshotAfter(), "");
    }

    private static List<String> mergeOmitted(List<String> planned, List<String> actual) {
        LinkedHashSet<String> paths = new LinkedHashSet<>(planned);
        paths.addAll(actual);
        return List.copyOf(paths);
    }

    private static String mergeSharedOmissionReason(String first, String second) {
        if (!first.equals(second)) {
            throw new IllegalStateException("Shared trace artifact has conflicting omission reasons.");
        }
        return first;
    }

    private static String aggregateOmissionMarker() {
        return "Omitted because the trace archive exceeded its aggregate shaft.trace.maxArtifactMb="
                + SHAFT.Properties.reporting.traceMaxArtifactMb() + " budget";
    }

    static String reconcileArtifactOmissions(String json, List<TraceArtifactReference> artifacts) {
        JsonNode root = JSON.readTree(json);
        JsonNode sessionArtifacts = root.path("session").path("artifacts");
        if (!sessionArtifacts.isArray()) {
            throw new IllegalStateException("Trace session artifacts must be an array.");
        }
        Map<String, TraceArtifactReference> byId = new LinkedHashMap<>();
        artifacts.forEach(reference -> byId.put(reference.id(), reference));
        sessionArtifacts.forEach(node -> {
            TraceArtifactReference reference = byId.get(node.path("id").asText());
            if (reference != null && node instanceof ObjectNode object) {
                object.put("omitted", reference.omitted());
                object.set("metadata", JSON.valueToTree(reference.metadata()));
            }
        });
        if (root instanceof ObjectNode objectRoot) {
            refreshInlineDomStatus(objectRoot, artifacts);
        }
        return JSON.writeValueAsString(root);
    }

    private static void refreshInlineDomStatus(ObjectNode root, List<TraceArtifactReference> artifacts) {
        Map<String, Boolean> availableByAction = new LinkedHashMap<>();
        artifacts.stream().filter(reference -> "dom-snapshot".equals(reference.kind())).forEach(reference -> {
            String actionId = reference.metadata().getOrDefault("actionId", "");
            if (!actionId.isEmpty()) {
                availableByAction.merge(actionId, !reference.omitted(), Boolean::logicalOr);
            }
        });
        JsonNode actions = root.path("evidence").path("actions");
        if (!actions.isArray()) {
            return;
        }
        actions.forEach(action -> {
            if (!(action instanceof ObjectNode object) || !object.has("domSnapshotInlineStatus")) {
                return;
            }
            boolean available = availableByAction.getOrDefault(object.path("id").asText(), false);
            object.put("domSnapshotInlineStatus", available ? "resource-only" : "omitted-budget");
            if (!available) {
                object.put("domSnapshotInlineReason",
                        "Inline and resource DOM snapshots exceeded the bounded trace archive budget.");
            }
        });
    }

    private static void closeArtifactManifest() {
        TraceArtifactManifest manifest = CURRENT_ARTIFACT_MANIFEST.get();
        if (manifest != null) {
            manifest.close();
        }
        CURRENT_ARTIFACT_MANIFEST.remove();
    }

    private static void attach(String type, String name, byte[] bytes, String description) {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            output.write(bytes);
        } catch (IOException e) {
            throw new IllegalStateException("Could not buffer trace attachment.", e);
        }
        AttachmentReporter.attachBasedOnFileType(type, name, output, description);
    }

    static boolean persistTraceArtifacts(TestExecutionInfo info, Path completedArchive, Map<String, byte[]> screenshots,
                                      int attempt, List<String> omitted) {
        try {
            Path directory = traceDirectory(info);
            Files.createDirectories(directory);
            boolean failed = info != null && info.throwable() != null;
            long archiveBytes = Files.size(completedArchive);
            if (!TraceSessionBudget.tryReserve(archiveBytes)) {
                persistSessionOmission(info, directory, attempt, failed, omitted);
                return false;
            }
            String archiveName = "shaft-trace.zip";
            // Retain failed-attempt bundles under attempt-indexed names so a later passing retry
            // (which rewrites shaft-trace.zip) never erases the flake evidence.
            if (failed && retriesConfigured() && SHAFT.Properties.reporting.traceRetainFailedAttempts()
                    && TraceSessionBudget.tryReserve(archiveBytes)) {
                archiveName = "shaft-trace-attempt-" + attempt + ".zip";
                TraceArchiveWriter.copy(completedArchive, directory.resolve(archiveName));
            }
            String testId = safeTestId(info);
            synchronized (TRACE_LOCKS.computeIfAbsent(testId, id -> new Object())) {
                recordAttempt(info, attempt, failed ? "failed" : "passed", archiveName);
                if (publishLatest(testId, attempt, completedArchive, directory.resolve("shaft-trace.zip"))) {
                    boolean persistScreenshots = persistSidecarScreenshots(directory, screenshots);
                    TraceArtifactManifest manifest = CURRENT_ARTIFACT_MANIFEST.get();
                    List<TraceArtifactReference> artifacts = manifest == null ? List.of() : manifest.references();
                    LATEST_INDEX.put(testId, new TraceIndexSnapshot(info, persistScreenshots, attempt,
                            List.copyOf(omitted), artifacts, ""));
                    Files.deleteIfExists(directory.resolve("SHAFT Trace Report.html"));
                    Files.deleteIfExists(directory.resolve("shaft-trace.json"));
                }
                writeLatestIndex(testId, directory);
            }
            return true;
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete("Could not persist SHAFT trace artifacts: " + e.getMessage(), Level.WARN);
            return false;
        }
    }

    private static boolean persistSidecarScreenshots(Path directory, Map<String, byte[]> screenshots)
            throws IOException {
        if (screenshots == null || screenshots.isEmpty()) {
            return false;
        }
        long screenshotBytes = 0;
        for (byte[] bytes : screenshots.values()) {
            screenshotBytes = Math.addExact(screenshotBytes, bytes.length);
        }
        if (!TraceSessionBudget.tryReserve(screenshotBytes)) {
            return false;
        }
        Path screenshotsDirectory = directory.resolve("screenshots");
        Files.createDirectories(screenshotsDirectory);
        for (Map.Entry<String, byte[]> entry : screenshots.entrySet()) {
            Files.write(screenshotsDirectory.resolve(entry.getKey() + ".png"), entry.getValue());
        }
        return true;
    }

    private static void persistSessionOmission(TestExecutionInfo info, Path directory, int attempt, boolean failed,
                                               List<String> omitted) throws IOException {
        String testId = safeTestId(info);
        List<String> sessionOmitted = new ArrayList<>(omitted);
        if (!sessionOmitted.contains("shaft-trace.zip")) {
            sessionOmitted.add("shaft-trace.zip");
        }
        synchronized (TRACE_LOCKS.computeIfAbsent(testId, id -> new Object())) {
            recordAttempt(info, attempt, failed ? "failed" : "passed", "");
            TraceArtifactManifest manifest = CURRENT_ARTIFACT_MANIFEST.get();
            List<TraceArtifactReference> artifacts = manifest == null ? List.of() : manifest.references();
            LATEST_INDEX.put(testId, new TraceIndexSnapshot(info, false, attempt, List.copyOf(sessionOmitted),
                    artifacts, TraceSessionBudget.omissionReason()));
            writeLatestIndex(testId, directory);
        }
        ReportManagerHelper.logDiscrete(TraceSessionBudget.omissionReason(), Level.WARN);
    }

    private static void writeLatestIndex(String testId, Path directory) throws IOException {
        TraceIndexSnapshot latest = LATEST_INDEX.get(testId);
        if (latest == null) {
            return;
        }
        byte[] index = renderTraceIndexJson(latest.info(), directory.resolve("shaft-trace.zip"),
                latest.hasScreenshots(), latest.attempt(), latest.omitted(), latest.artifacts(),
                latest.sessionOmission()).getBytes(StandardCharsets.UTF_8);
        TraceArchiveWriter.writeBytes(directory.resolve("index.json"), index);
    }

    private static void recordAttempt(TestExecutionInfo info, int attempt, String status, String archiveName) {
        ATTEMPT_HISTORY.computeIfAbsent(safeTestId(info), id -> Collections.synchronizedList(new ArrayList<>()))
                .add(new AttemptRecord(attempt, status, archiveName, Instant.now().toString()));
    }

    static Path traceDirectory(TestExecutionInfo info) {
        return Path.of("target", "shaft-traces", safeTestId(info));
    }

    static Path completedArchivePath(TestExecutionInfo info, int attempt) {
        return traceDirectory(info).resolve(".shaft-trace-" + attempt + "-" + UUID.randomUUID() + ".zip");
    }

    static boolean publishLatest(String testId, int attempt, Path completedArchive, Path target) throws IOException {
        synchronized (TRACE_LOCKS.computeIfAbsent(testId, id -> new Object())) {
            int latestAttempt = LATEST_PUBLISHED_ATTEMPT.getOrDefault(testId, 0);
            if (attempt < latestAttempt) {
                return false;
            }
            TraceArchiveWriter.copy(completedArchive, target);
            LATEST_PUBLISHED_ATTEMPT.put(testId, attempt);
            return true;
        }
    }

    static String safeTestId(TestExecutionInfo info) {
        String id = info == null ? "" : value(info.stableId());
        if (id.isBlank() && info != null) {
            id = value(info.className()) + "." + value(info.methodName());
        }
        String safeId = id.replaceAll("[^A-Za-z0-9._-]+", "_");
        while (safeId.startsWith("_")) {
            safeId = safeId.substring(1);
        }
        while (safeId.endsWith("_")) {
            safeId = safeId.substring(0, safeId.length() - 1);
        }
        if (safeId.isBlank()) {
            safeId = "unknown";
        }
        boolean unsafeComponent = safeId.equals(".") || safeId.equals("..") || safeId.endsWith(".")
                || isWindowsReservedName(safeId);
        boolean lossy = !id.isBlank() && (!safeId.equals(id) || safeId.length() > 120 || unsafeComponent);
        if (!lossy) {
            return safeId;
        }
        String suffix = "-" + shortHash(id);
        int prefixLength = Math.min(safeId.length(), 120 - suffix.length());
        return safeId.substring(0, prefixLength) + suffix;
    }

    private static boolean isWindowsReservedName(String value) {
        String baseName = value.contains(".") ? value.substring(0, value.indexOf('.')) : value;
        return baseName.matches("(?i)CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9]");
    }

    private static String shortHash(String value) {
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8));
            return java.util.HexFormat.of().formatHex(digest, 0, 6);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", e);
        }
    }

    static String renderTraceIndexJson(TestExecutionInfo info, Path zipPath, boolean hasScreenshots,
                                               int attempt, List<String> omitted,
                                               List<TraceArtifactReference> artifacts) {
        return renderTraceIndexJson(info, zipPath, hasScreenshots, attempt, omitted, artifacts, "");
    }

    static String renderTraceIndexJson(TestExecutionInfo info, Path zipPath, boolean hasScreenshots,
                                               int attempt, List<String> omitted,
                                               List<TraceArtifactReference> artifacts, String sessionOmission) {
        boolean failed = info != null && info.throwable() != null;
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        field(json, 1, "testId", safeTestId(info), true);
        field(json, 1, "generatedAt", Instant.now().toString(), true);
        field(json, 1, "archive", relative(zipPath), true);
        field(json, 1, "attempt", String.valueOf(attempt), true);
        field(json, 1, "status", failed ? "failed" : "passed", true);
        field(json, 1, "retried", String.valueOf(info != null && info.retried()), true);
        field(json, 1, "traceMode", effectiveTraceMode(), true);
        field(json, 1, "sessionOmission", sessionOmission == null ? "" : sessionOmission, true);
        array(json, 1, "omittedEntries", omitted, true);
        rawArray(json, 1, "artifacts", TraceSchemaSerializer.artifactsToJson(artifacts), true);
        appendAttemptHistory(json, safeTestId(info));
        objectStart(json, 1, "entries");
        field(json, 2, "html", "SHAFT Trace Report.html", true);
        field(json, 2, "json", "shaft-trace.json", true);
        field(json, 2, "network", "shaft-network.har", hasScreenshots);
        if (hasScreenshots) {
            field(json, 2, "screenshots", "screenshots", false);
        }
        objectEnd(json, 1, false);
        json.append("}\n");
        return json.toString();
    }

    private static void appendAttemptHistory(StringBuilder json, String testId) {
        List<AttemptRecord> history = ATTEMPT_HISTORY.getOrDefault(testId, List.of());
        indent(json, 1).append("\"attempts\": [");
        synchronized (history) {
            List<AttemptRecord> orderedHistory = history.stream()
                    .sorted(Comparator.comparingInt(AttemptRecord::attempt))
                    .toList();
            for (int i = 0; i < orderedHistory.size(); i++) {
                AttemptRecord record = orderedHistory.get(i);
                json.append(i > 0 ? "," : "").append("\n");
                indent(json, 2).append("{\"attempt\": ").append(record.attempt())
                        .append(", \"status\": \"").append(escapeJson(record.status()))
                        .append("\", \"archive\": \"").append(escapeJson(record.archive()))
                        .append("\", \"generatedAt\": \"").append(escapeJson(record.generatedAt())).append("\"}");
            }
            if (!history.isEmpty()) {
                json.append("\n");
                indent(json, 1);
            }
        }
        json.append("],\n");
    }

    private static Snapshot snapshot() {
        if (!SHAFT.Properties.reporting.traceIncludeFullPageSnapshots()
                && !SHAFT.Properties.reporting.traceIncludeNativePageSource()) {
            return new Snapshot("none", "disabled", "disabled", "", "disabled", "", 0, false);
        }
        try {
            Page page = PlaywrightSessionManager.currentPage();
            if (page != null && SHAFT.Properties.reporting.traceIncludeFullPageSnapshots()) {
                return snapshot(SeleniumTraceCapture.fromContent("playwright", "structural", "playwright-html",
                        page.content(), FailureTraceReporter::redactSourceText));
            }
        } catch (RuntimeException ignored) {
            // Snapshot collection is best-effort; trace generation must never hide the original failure.
        }
        WebDriver driver = DriverFactoryHelper.getActiveDriver();
        if (driver == null) {
            return new Snapshot("none", "unavailable", "unavailable",
                    "No active browser or native driver was registered for this thread.", "unavailable", "", 0, false);
        }
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                return snapshot(SeleniumTraceCapture.fromContent("appium", "structural", "native-page-source",
                        driver.getPageSource(), FailureTraceReporter::redactSourceText));
            } catch (RuntimeException ignored) {
                return new Snapshot("appium", "unavailable", "unavailable", "Snapshot capture failed.",
                        "unavailable", "", 0, false);
            }
        }
        SeleniumTraceCapture.Result result = SeleniumTraceCapture.capture(driver,
                FailureTraceReporter::redactSourceText,
                SHAFT.Properties.reporting.traceIncludeFullPageSnapshots());
        return snapshot(result);
    }

    private static String reportedBrowser() {
        var session = PlaywrightSessionManager.currentSession();
        if (session != null) {
            try {
                var browser = session.browser();
                String runtimeBrowser = browser == null || browser.browserType() == null
                        ? "" : browser.browserType().name();
                if (runtimeBrowser != null && !runtimeBrowser.isBlank()) return runtimeBrowser;
            } catch (RuntimeException ignored) {
                // Attached or closing sessions may no longer expose their browser type; use configured fallback.
            }
            String playwrightBrowser = safeProperty(() -> SHAFT.Properties.playwright.browserName());
            if (!playwrightBrowser.isBlank()) return playwrightBrowser;
        }
        return safeProperty(() -> SHAFT.Properties.web.targetBrowserName());
    }

    private static Snapshot snapshot(SeleniumTraceCapture.Result result) {
        String content = result.content();
        return new Snapshot(result.provider(), result.fidelity(), result.status(), result.reason(), result.type(),
                content, content.getBytes(StandardCharsets.UTF_8).length, result.truncated());
    }

    private static SourceContext sourceContext(TestExecutionInfo info) {
        if (info == null || info.throwable() == null || !SHAFT.Properties.reporting.traceIncludeCodeContext()) {
            return new SourceContext("", "", "", "", "");
        }
        if (containsSensitiveThrowable(info.throwable())) {
            return new SourceContext("", "", "", "", "");
        }
        StackTraceElement frame = relevantFrame(info.throwable());
        if (frame == null) {
            return new SourceContext("", "", "", "", "");
        }
        Path sourceFile = findSourceFile(frame);
        if (sourceFile == null) {
            return new SourceContext(frame.toString(), "", String.valueOf(frame.getLineNumber()), frame.toString(), "");
        }
        return new SourceContext(frame.toString(), relative(sourceFile), String.valueOf(frame.getLineNumber()),
                snippet(sourceFile, frame.getLineNumber()), fileContent(sourceFile));
    }

    /**
     * Full (bounded, redacted) content of the failing test source file so the trace archive is
     * self-contained for root-cause analysis even when the reviewer has no checkout of the tests.
     */
    private static String fileContent(Path sourceFile) {
        try {
            String content = Files.readString(sourceFile, StandardCharsets.UTF_8);
            return redactSourceText(content.length() > MAX_SOURCE_FILE_CHARACTERS
                    ? content.substring(0, MAX_SOURCE_FILE_CHARACTERS)
                    : content);
        } catch (IOException | RuntimeException e) {
            return "";
        }
    }

    private static StackTraceElement relevantFrame(Throwable throwable) {
        for (Throwable current = throwable; current != null; current = current.getCause()) {
            for (StackTraceElement frame : current.getStackTrace()) {
                String className = frame.getClassName();
                if (!className.startsWith("com.shaft.")
                        && !className.startsWith("org.testng.")
                        && !className.startsWith("org.junit.")
                        && !className.startsWith("io.qameta.")
                        && !className.startsWith("java.")
                        && !className.startsWith("jdk.")) {
                    return frame;
                }
            }
        }
        return throwable.getStackTrace().length == 0 ? null : throwable.getStackTrace()[0];
    }

    private static Path findSourceFile(StackTraceElement frame) {
        String classPath = frame.getClassName().replace('.', '/') + ".java";
        int nestedClassIndex = classPath.indexOf('$');
        if (nestedClassIndex > -1) {
            classPath = classPath.substring(0, nestedClassIndex) + ".java";
        }
        List<Path> candidates = List.of(
                Path.of("src/test/java", classPath),
                Path.of("src/main/java", classPath),
                Path.of("shaft-engine/src/test/java", classPath),
                Path.of("shaft-engine/src/main/java", classPath));
        for (Path candidate : candidates) {
            if (Files.isRegularFile(candidate)) {
                return candidate;
            }
        }
        return null;
    }

    private static String snippet(Path sourceFile, int lineNumber) {
        if (lineNumber < 1) {
            return "";
        }
        try {
            List<String> lines = Files.readAllLines(sourceFile, StandardCharsets.UTF_8);
            int start = Math.max(1, lineNumber - SNIPPET_RADIUS);
            int end = Math.min(lines.size(), lineNumber + SNIPPET_RADIUS);
            StringBuilder snippet = new StringBuilder();
            for (int line = start; line <= end; line++) {
                snippet.append(line == lineNumber ? "> " : "  ")
                        .append(line)
                        .append(": ")
                        .append(lines.get(line - 1))
                        .append(System.lineSeparator());
            }
            return redactSourceText(snippet.toString().trim());
        } catch (IOException e) {
            return sourceFile + ":" + lineNumber;
        }
    }

    private static List<String> timeline(Throwable throwable, String logText) {
        if (logText == null || logText.isBlank()) {
            return List.of();
        }
        List<String> timeline = new ArrayList<>();
        for (String line : logText.split("\\R")) {
            if (!line.isBlank()) {
                timeline.add(redactThrowableText(throwable, line));
            }
        }
        return timeline;
    }

    private static List<String> attachmentEntries(List<String> attachments) {
        List<String> entries = new ArrayList<>();
        if (attachments != null) {
            attachments.stream()
                    .filter(attachment -> attachment != null && !attachment.isBlank())
                    .map(FailureTraceReporter::redactInvocationText)
                    .forEach(entries::add);
        }
        return entries;
    }

    static String redact(String value) {
        String redacted = value(value);
        redacted = AUTHORIZATION_PATTERN.matcher(redacted).replaceAll("$1********");
        redacted = COOKIE_PATTERN.matcher(redacted).replaceAll("$1$2********");
        redacted = URL_CREDENTIAL_PATTERN.matcher(redacted).replaceAll("$1********$2");
        redacted = SECRET_JSON_PATTERN.matcher(redacted).replaceAll("$1********$2");
        redacted = SECRET_ATTRIBUTE_PATTERN.matcher(redacted).replaceAll("$1********$2");
        return SECRET_ASSIGNMENT_PATTERN.matcher(redacted).replaceAll("$1$2********");
    }

    static String redactThrowableText(String value) {
        return redactThrowableText(null, value);
    }

    static String redactThrowableText(Throwable throwable, String value) {
        if (containsSensitiveThrowable(throwable)) {
            return "[provider error text omitted because submitted data may be sensitive]";
        }
        if (SENSITIVE_VALUE_OVERFLOW.get()) {
            return SENSITIVE_BOUNDS_OMISSION;
        }
        String redacted = redactSensitiveValues(value(value), EXACT_SENSITIVE_VALUES.get(),
                "[provider error text omitted because it may contain a sensitive storage value]");
        return redactSourceText(redacted);
    }

    /** Registers an exact value for current-invocation trace redaction. */
    public static void registerSensitiveValue(String value) {
        if (value != null && !value.isEmpty()) {
            addSensitiveValue(EXACT_SENSITIVE_VALUES.get(), value);
        }
    }

    /** Registers a credential that must be removed from later source-code evidence in this invocation. */
    public static void registerSensitiveSourceValue(String value) {
        if (value != null && !value.isEmpty()) {
            addSensitiveValue(SOURCE_SENSITIVE_VALUES.get(), value);
        }
    }

    private static void addSensitiveValue(Set<String> values, String value) {
        if (!addBoundedSensitiveValue(values, value)) {
            SENSITIVE_VALUE_OVERFLOW.set(true);
        }
    }

    private static boolean addBoundedSensitiveValue(Set<String> values, String value) {
        if (value.length() > SENSITIVE_VALUE_LENGTH_LIMIT) {
            return false;
        }
        List<String> additions = new ArrayList<>();
        additions.add(value);
        if ("-0.0".equals(value)) {
            additions.add("0.0");
        } else if ("0.0".equals(value)) {
            additions.add("-0.0");
        }
        for (String addition : additions) {
            if (!values.contains(addition) && values.size() >= SENSITIVE_VALUE_LIMIT) {
                return false;
            }
            values.add(addition);
        }
        return true;
    }

    static String redactSourceText(String value) {
        SensitiveBrowserSessionRegistry registry = PERSISTENT_BROWSER_SENSITIVITY.get();
        if (SENSITIVE_VALUE_OVERFLOW.get() || registry.currentOverflowed()) {
            return SENSITIVE_BOUNDS_OMISSION;
        }
        String redacted = redact(value);
        LinkedHashSet<String> sensitiveValues = new LinkedHashSet<>(SOURCE_SENSITIVE_VALUES.get());
        sensitiveValues.addAll(registry.currentValues());
        return redactSensitiveValues(redacted, sensitiveValues,
                "[source context omitted because it contains a sensitive credential]");
    }

    private static String redactSensitiveValues(String text, Set<String> sensitiveValues, String shortOmission) {
        LinkedHashSet<BigDecimal> numericValues = new LinkedHashSet<>();
        List<String> literalValues = new ArrayList<>();
        for (String sensitiveValue : sensitiveValues) {
            BigDecimal numeric = numericValue(sensitiveValue);
            if (numeric == null) {
                literalValues.add(sensitiveValue);
            } else {
                numericValues.add(normalizeNumericValue(numeric));
            }
        }
        String redacted = numericValues.isEmpty() ? text : redactNumericValues(text, numericValues);
        if (redacted == null) {
            return SENSITIVE_BOUNDS_OMISSION;
        }
        for (String sensitiveValue : literalValues) {
            Matcher matcher = sensitiveValuePattern(sensitiveValue).matcher(redacted);
            if (!matcher.find()) {
                continue;
            }
            if (sensitiveValue.length() < 4) {
                return shortOmission;
            }
            redacted = matcher.replaceAll("********");
        }
        return redacted;
    }

    private static String redactNumericValues(String text, Set<BigDecimal> sensitiveValues) {
        Matcher matcher = NUMERIC_TOKEN_PATTERN.matcher(text);
        StringBuilder redacted = new StringBuilder();
        int candidates = 0;
        while (matcher.find()) {
            if (++candidates > NUMERIC_TOKEN_LIMIT || matcher.group(1).length() > NUMERIC_TOKEN_LENGTH_LIMIT) {
                return null;
            }
            BigDecimal candidate = numericValue(matcher.group(1));
            if (candidate != null && sensitiveValues.contains(normalizeNumericValue(candidate))) {
                matcher.appendReplacement(redacted, "********");
            } else {
                matcher.appendReplacement(redacted, Matcher.quoteReplacement(matcher.group()));
            }
        }
        matcher.appendTail(redacted);
        return redacted.toString();
    }

    private static BigDecimal normalizeNumericValue(BigDecimal value) {
        return value.signum() == 0 ? BigDecimal.ZERO : value.stripTrailingZeros();
    }

    private static Pattern sensitiveValuePattern(String sensitiveValue) {
        if (sensitiveValue.length() < 4) {
            return Pattern.compile("(?<![\\p{Alnum}_])" + Pattern.quote(sensitiveValue)
                    + "(?![\\p{Alnum}_])");
        }
        return Pattern.compile(Pattern.quote(sensitiveValue));
    }

    private static BigDecimal numericValue(String value) {
        try {
            return new BigDecimal(value);
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    /** Redacts current-invocation exact and source-sensitive values for downstream failure consumers. */
    public static String redactInvocationText(String value) {
        return redactThrowableText(value);
    }

    /** Redacts one throwable's identity-sensitive text plus current-invocation exact and source values. */
    public static String redactInvocationText(Throwable throwable, String value) {
        return redactThrowableText(throwable, value);
    }

    /** Registers string values recursively reachable from a script or structured argument. */
    public static void registerSensitiveValues(Object value) {
        try {
            registerSensitiveValues(value, Collections.newSetFromMap(new IdentityHashMap<>()),
                    new int[]{SENSITIVE_VALUE_TRAVERSAL_LIMIT}, 0);
        } catch (RuntimeException ignored) {
            // Redaction bookkeeping must never replace the provider exception being reported.
        }
    }

    private static void registerSensitiveValues(Object value, Set<Object> visited, int[] remaining, int depth) {
        if (value == null || remaining[0]-- <= 0 || depth > SENSITIVE_VALUE_DEPTH_LIMIT) {
            return;
        }
        if (value instanceof CharSequence text) {
            registerSensitiveValue(text.toString());
            return;
        }
        if (!visited.add(value)) {
            return;
        }
        try {
            if (value instanceof Map<?, ?> map) {
                for (Map.Entry<?, ?> entry : map.entrySet()) {
                    registerSensitiveValues(entry.getKey(), visited, remaining, depth + 1);
                    registerSensitiveValues(entry.getValue(), visited, remaining, depth + 1);
                    if (remaining[0] <= 0) {
                        break;
                    }
                }
            } else if (value instanceof Iterable<?> iterable) {
                for (Object entry : iterable) {
                    registerSensitiveValues(entry, visited, remaining, depth + 1);
                    if (remaining[0] <= 0) {
                        break;
                    }
                }
            } else if (value.getClass().isArray()) {
                int length = Math.min(java.lang.reflect.Array.getLength(value), Math.max(0, remaining[0]));
                for (int index = 0; index < length; index++) {
                    registerSensitiveValues(java.lang.reflect.Array.get(value, index), visited, remaining, depth + 1);
                }
            }
        } catch (RuntimeException ignored) {
            // Best effort only; callers must retain the original provider failure.
        }
    }

    /** Marks one provider failure's text as sensitive while retaining its type and object identity. */
    public static void registerSensitiveThrowable(Throwable throwable) {
        if (throwable != null) {
            SENSITIVE_THROWABLES.get().add(throwable);
        }
    }

    /**
     * Omits browser snapshots and backend-native traces for the rest of this test invocation.
     * Use when an otherwise successful browser operation submits values that must not enter later evidence.
     */
    public static void suppressSensitiveBrowserArtifacts() {
        SUPPRESS_SENSITIVE_BROWSER_ARTIFACTS.set(true);
    }

    /** @return whether the current test invocation owns a sensitive browser-artifact boundary */
    public static boolean shouldSuppressSensitiveBrowserArtifacts() {
        return SUPPRESS_SENSITIVE_BROWSER_ARTIFACTS.get()
                || PERSISTENT_BROWSER_SENSITIVITY.get().currentIsSensitive();
    }

    /** @return whether browser-derived evidence may still contain active or stale sensitive state */
    public static boolean shouldOmitSensitiveBrowserEvidence() {
        return SUPPRESS_SENSITIVE_BROWSER_ARTIFACTS.get()
                || PERSISTENT_BROWSER_SENSITIVITY.get().currentHasSensitiveEvidence();
    }

    /** Selects the browser/session whose persistent emulation state owns later browser evidence. */
    public static void activateBrowserEvidenceOwner(Object owner) {
        PERSISTENT_BROWSER_SENSITIVITY.get().activate(owner);
    }

    /** Records sensitive browser state until its matching override is cleared or the session closes. */
    public static void registerPersistentSensitiveBrowserState(Object owner, String channel, Object... values) {
        activateBrowserEvidenceOwner(owner);
        if (owner != null) {
            PERSISTENT_BROWSER_SENSITIVITY.get().current().put(channel, values);
        }
    }

    /** Clears one persistent sensitive browser-state channel after its provider override is cleared. */
    public static void clearPersistentSensitiveBrowserState(Object owner, String channel) {
        SensitiveBrowserSessionRegistry registry = PERSISTENT_BROWSER_SENSITIVITY.get();
        registry.activate(owner);
        PersistentBrowserSensitivity state = registry.current();
        if (state != null) {
            state.retire(channel);
        }
    }

    /** Clears every persistent sensitive browser-state channel owned by the supplied session. */
    public static void clearPersistentSensitiveBrowserState(Object owner) {
        if (owner == null) {
            PERSISTENT_BROWSER_SENSITIVITY.remove();
        } else {
            PERSISTENT_BROWSER_SENSITIVITY.get().remove(owner);
        }
    }

    static boolean containsSensitiveThrowable(Throwable root) {
        if (root == null || SENSITIVE_THROWABLES.get().isEmpty()) {
            return false;
        }
        Set<Throwable> visited = Collections.newSetFromMap(new IdentityHashMap<>());
        ArrayDeque<Throwable> pending = new ArrayDeque<>();
        pending.add(root);
        int remaining = 100;
        while (!pending.isEmpty() && remaining-- > 0) {
            Throwable current = pending.removeFirst();
            if (!visited.add(current)) {
                continue;
            }
            if (SENSITIVE_THROWABLES.get().contains(current)) {
                return true;
            }
            try {
                if (current.getCause() != null) {
                    pending.addLast(current.getCause());
                }
                for (Throwable suppressed : current.getSuppressed()) {
                    if (suppressed != null) {
                        pending.addLast(suppressed);
                    }
                }
            } catch (RuntimeException ignored) {
                // Throwable graph inspection is best-effort and must not hide the original failure.
            }
        }
        return false;
    }

    static void clearSensitiveValues() {
        EXACT_SENSITIVE_VALUES.remove();
        SOURCE_SENSITIVE_VALUES.remove();
        SENSITIVE_THROWABLES.remove();
        SUPPRESS_SENSITIVE_BROWSER_ARTIFACTS.remove();
        SENSITIVE_VALUE_OVERFLOW.remove();
        PERSISTENT_BROWSER_SENSITIVITY.remove();
    }

    static void clearInvocationSensitiveValues() {
        EXACT_SENSITIVE_VALUES.remove();
        SOURCE_SENSITIVE_VALUES.remove();
        SENSITIVE_THROWABLES.remove();
        SUPPRESS_SENSITIVE_BROWSER_ARTIFACTS.remove();
        SENSITIVE_VALUE_OVERFLOW.remove();
    }

    private static final class SensitiveBrowserSessionRegistry {
        private final List<PersistentBrowserSensitivity> sessions = new ArrayList<>();
        private WeakReference<Object> activeOwner = new WeakReference<>(null);

        private void activate(Object owner) {
            if (owner == null) {
                return;
            }
            sessions.removeIf(state -> state.owner() == null);
            activeOwner = new WeakReference<>(owner);
            if (sessions.stream().noneMatch(state -> state.owns(owner))) {
                sessions.add(new PersistentBrowserSensitivity(owner));
            }
        }

        private PersistentBrowserSensitivity current() {
            Object owner = activeOwner.get();
            return owner == null ? null : sessions.stream().filter(state -> state.owns(owner)).findFirst().orElse(null);
        }

        private LinkedHashSet<String> currentValues() {
            PersistentBrowserSensitivity current = current();
            return current == null ? new LinkedHashSet<>() : current.values();
        }

        private boolean currentIsSensitive() {
            PersistentBrowserSensitivity current = current();
            return current != null && current.isActive();
        }

        private boolean currentHasSensitiveEvidence() {
            PersistentBrowserSensitivity current = current();
            return current != null && (!current.values().isEmpty() || current.overflowed());
        }

        private boolean currentOverflowed() {
            PersistentBrowserSensitivity current = current();
            return current != null && current.overflowed();
        }

        @SuppressWarnings("PMD.CompareObjectsWithEquals") // Session ownership is identity based.
        private void remove(Object owner) {
            sessions.removeIf(state -> state.owns(owner) || state.owner() == null);
            if (activeOwner.get() == owner) {
                activeOwner = new WeakReference<>(null);
            }
        }
    }

    private static final class PersistentBrowserSensitivity {
        private final WeakReference<Object> owner;
        private final Map<String, LinkedHashSet<String>> channels = new LinkedHashMap<>();
        private final Map<String, LinkedHashSet<String>> staleChannels = new LinkedHashMap<>();
        private final Set<String> activeOverflowChannels = new LinkedHashSet<>();
        private final Set<String> staleOverflowChannels = new LinkedHashSet<>();

        private PersistentBrowserSensitivity(Object owner) {
            this.owner = new WeakReference<>(owner);
        }

        private Object owner() {
            return owner.get();
        }

        @SuppressWarnings("PMD.CompareObjectsWithEquals") // Session ownership is identity based.
        private boolean owns(Object candidate) {
            return candidate != null && owner.get() == candidate;
        }

        private void put(String channel, Object... submittedValues) {
            LinkedHashSet<String> previousValues = channels.remove(channel);
            if (previousValues != null && !previousValues.isEmpty()) {
                addHistoricalValues(channel, previousValues);
            }
            if (activeOverflowChannels.remove(channel)) {
                staleOverflowChannels.add(channel);
            }

            LinkedHashSet<String> values = new LinkedHashSet<>();
            boolean channelOverflowed = false;
            if (submittedValues != null) {
                for (Object submittedValue : submittedValues) {
                    if (submittedValue != null
                            && !addBoundedSensitiveValue(values, String.valueOf(submittedValue))) {
                        channelOverflowed = true;
                    }
                }
            }
            if (!values.isEmpty()) {
                channels.put(channel, values);
            }
            if (channelOverflowed) {
                activeOverflowChannels.add(channel);
            }
        }

        private void retire(String channel) {
            LinkedHashSet<String> values = channels.remove(channel);
            if (values != null && !values.isEmpty()) {
                addHistoricalValues(channel, values);
            }
            if (activeOverflowChannels.remove(channel)) {
                staleOverflowChannels.add(channel);
            }
        }

        private void addHistoricalValues(String channel, Set<String> values) {
            LinkedHashSet<String> history = staleChannels.computeIfAbsent(channel, ignored -> new LinkedHashSet<>());
            for (String value : values) {
                if (!addBoundedSensitiveValue(history, value)) {
                    staleOverflowChannels.add(channel);
                    break;
                }
            }
        }

        private boolean isActive() {
            return !channels.isEmpty() || !activeOverflowChannels.isEmpty();
        }

        private boolean overflowed() {
            return !activeOverflowChannels.isEmpty() || !staleOverflowChannels.isEmpty();
        }

        private LinkedHashSet<String> values() {
            LinkedHashSet<String> values = new LinkedHashSet<>();
            channels.values().forEach(values::addAll);
            staleChannels.values().forEach(values::addAll);
            return values;
        }
    }

    private static void objectStart(StringBuilder json, int indent, String key) {
        indent(json, indent).append("\"").append(key).append("\": {\n");
    }

    private static void objectEnd(StringBuilder json, int indent, boolean comma) {
        indent(json, indent).append("}").append(comma ? "," : "").append("\n");
    }

    private static void rawObject(StringBuilder json, int indent, String key, String value, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": ")
                .append(value(value).isBlank() ? "{}" : value.strip())
                .append(comma ? "," : "")
                .append("\n");
    }

    private static void rawArray(StringBuilder json, int indent, String key, String value, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": ")
                .append(value(value).isBlank() ? "[]" : value.strip())
                .append(comma ? "," : "")
                .append("\n");
    }

    private static String locatorHealthJson() {
        if (!LocatorHealthReporter.isEnabled()) {
            return "{\"enabled\": false}";
        }
        return LocatorHealthReporter.currentSummaryJson();
    }

    private static void field(StringBuilder json, int indent, String key, String value, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": \"")
                .append(escapeJson(redact(value)))
                .append("\"")
                .append(comma ? "," : "")
                .append("\n");
    }

    private static void array(StringBuilder json, int indent, String key, List<String> values, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": [");
        for (int i = 0; i < values.size(); i++) {
            if (i > 0) {
                json.append(", ");
            }
            json.append("\"").append(escapeJson(values.get(i))).append("\"");
        }
        json.append("]").append(comma ? "," : "").append("\n");
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

    private static String escapeHtml(String value) {
        return value(value)
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;");
    }

    private static String relative(Path path) {
        Path absolute = path.toAbsolutePath().normalize();
        Path current = Path.of("").toAbsolutePath().normalize();
        if (absolute.startsWith(current)) {
            return current.relativize(absolute).toString().replace('\\', '/');
        }
        return path.getFileName().toString();
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    private static String safeProperty(java.util.function.Supplier<String> supplier) {
        try {
            return value(supplier.get());
        } catch (RuntimeException e) {
            return "";
        }
    }

    private record SourceContext(String frame, String file, String line, String snippet, String fileContent) {
    }

    private record AttemptRecord(int attempt, String status, String archive, String generatedAt) {
    }

    private record TraceIndexSnapshot(TestExecutionInfo info, boolean hasScreenshots, int attempt,
                                      List<String> omitted, List<TraceArtifactReference> artifacts,
                                      String sessionOmission) {
        private TraceIndexSnapshot {
            omitted = List.copyOf(omitted);
            artifacts = List.copyOf(artifacts);
            sessionOmission = sessionOmission == null ? "" : sessionOmission;
        }
    }

    record TraceArchiveBundle(String json, String html, List<String> omitted,
                              List<TraceArtifactReference> artifacts) {
        TraceArchiveBundle {
            omitted = List.copyOf(omitted);
            artifacts = List.copyOf(artifacts);
        }
    }

    private record Snapshot(String provider, String fidelity, String status, String reason, String type, String content,
                            int byteCount, boolean truncated) {
    }
}
