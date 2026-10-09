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
        return TraceViewerHtml.render(json, omitted);
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
        return JsonEscapes.escape(value);
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
