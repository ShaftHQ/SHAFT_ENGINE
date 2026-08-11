package com.shaft.tools.io.internal;

import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.trace.TraceSession;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.WebDriver;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
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

/**
 * Builds the failure-scoped SHAFT trace viewer artifacts attached to Allure.
 */
public final class FailureTraceReporter {
    private static final Pattern AUTHORIZATION_PATTERN = Pattern.compile("(?i)(authorization\\s*[:=]\\s*)(bearer\\s+)?[^\\s,;]+");
    private static final Pattern COOKIE_PATTERN = Pattern.compile("(?i)(cookie|set-cookie)(\\s*[:=]\\s*)[^\\n\\r]+");
    private static final Pattern URL_CREDENTIAL_PATTERN = Pattern.compile("(?i)(://[^:/\\s]+:)[^@/\\s]+(@)");
    private static final Pattern SECRET_ASSIGNMENT_PATTERN = Pattern.compile(
            "(?i)(password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)(\\s*[:=]\\s*)[^\\s,;&\"'<>]+");
    private static final Pattern SECRET_ATTRIBUTE_PATTERN = Pattern.compile(
            "(?i)((?:password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)\\s*=\\s*[\"'])[^\"']*([\"'])");
    private static final Pattern SECRET_JSON_PATTERN = Pattern.compile(
            "(?i)(\"(?:password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)\"\\s*:\\s*\")[^\"]*(\")");
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
    private static final int SENSITIVE_VALUE_TRAVERSAL_LIMIT = 1000;
    private static final int SENSITIVE_VALUE_DEPTH_LIMIT = 20;
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
            clearSensitiveValues();
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
            String html = renderTraceHtml(json, omitted);
            // In-report trace launcher (issue #3534 P2): the viewer HTML is fully self-contained --
            // it embeds the trace JSON (with base64 screenshots and inline DOM snapshots) and reads
            // everything from that embedded data, referencing no sibling files -- so attach it
            // directly for a one-click, in-report open, alongside the zip kept for full-fidelity
            // offline download.
            attach("html", "shaft-trace.html", html.getBytes(StandardCharsets.UTF_8), traceViewerLabel(info, attempt));
            completedArchive = completedArchivePath(info, attempt);
            renderTraceZip(completedArchive, json, html, CURRENT_NETWORK_JSON.get(), screenshots);
            persistTraceArtifacts(info, completedArchive, screenshots, attempt, omitted);
            AttachmentReporter.attachBasedOnFileType("zip", "shaft-trace.zip", completedArchive,
                    traceAttachmentLabel(info, attempt));
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
            clearSensitiveValues();
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
        SourceContext source = sourceContext(info);
        Snapshot snapshot = snapshot();
        List<TraceEventRecorder.ActionEvent> actions = TraceEventRecorder.drain();
        CURRENT_SCREENSHOTS.set(decodeScreenshots(actions));
        BrowserObservabilityRecorder.collectConsole(DriverFactoryHelper.getActiveDriver());
        String observabilityJson = BrowserObservabilityRecorder.drainMetadataJson();
        String networkJson = BrowserObservabilityRecorder.drainNetworkJson();
        CURRENT_NETWORK_JSON.set(networkJson);
        String consoleJson = BrowserObservabilityRecorder.drainConsoleJson();
        Throwable throwable = info == null ? null : info.throwable();
        closeArtifactManifest();
        int maxBytes = Math.max(1, SHAFT.Properties.reporting.traceMaxArtifactMb()) * 1024 * 1024;
        String omissionMarker = "Omitted because artifact exceeded shaft.trace.maxArtifactMb="
                + SHAFT.Properties.reporting.traceMaxArtifactMb();
        TraceArtifactManifest manifest = TraceArtifactManifest.create(networkJson, CURRENT_SCREENSHOTS.get(),
                PlaywrightTraceManager.getLastTracePath(), maxBytes, omissionMarker);
        CURRENT_ARTIFACT_MANIFEST.set(manifest);
        TraceSession traceSession = TraceSchemaSerializer.create(safeTestId(info), attempt, actions,
                manifest.references());
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        field(json, 1, "schemaVersion", "1.0", true);
        field(json, 1, "generatedAt", traceSession.generatedAt().toString(), true);
        rawObject(json, 1, "session", TraceSchemaSerializer.toJson(traceSession), true);
        appendTestObject(json, info, throwable, attempt);
        objectStart(json, 1, "environment");
        field(json, 2, "shaftVersion", safeProperty(() -> SHAFT.Properties.internal.shaftEngineVersion()), true);
        field(json, 2, "os", System.getProperty("os.name", ""), true);
        field(json, 2, "osVersion", System.getProperty("os.version", ""), true);
        field(json, 2, "javaVersion", System.getProperty("java.version", ""), true);
        field(json, 2, "targetPlatform", safeProperty(() -> SHAFT.Properties.platform.targetPlatform()), true);
        field(json, 2, "browser", safeProperty(() -> SHAFT.Properties.web.targetBrowserName()), true);
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
        field(json, 2, "type", snapshot.type(), true);
        field(json, 2, "content", snapshot.content(), false);
        objectEnd(json, 1, true);
        rawObject(json, 1, "locatorHealth", locatorHealthJson(), true);
        rawObject(json, 1, "browserObservability", observabilityJson, true);
        rawArray(json, 1, "network", networkJson, true);
        rawArray(json, 1, "console", consoleJson, true);
        rawArray(json, 1, "actions", TraceEventRecorder.toJson(actions), true);
        array(json, 1, "timeline", timeline(logText), true);
        array(json, 1, "attachments", attachmentEntries(attachments), false);
        json.append("}\n");
        return json.toString();
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
        int maxBytes = Math.max(1, SHAFT.Properties.reporting.traceMaxArtifactMb()) * 1024 * 1024;
        List<String> omitted = new ArrayList<>();
        if (json.getBytes(StandardCharsets.UTF_8).length > maxBytes) {
            omitted.add("shaft-trace.json");
        }
        if (manifest != null) {
            omitted.addAll(manifest.omittedPaths());
        }
        return omitted;
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
                dl{display:grid;grid-template-columns:130px 1fr;gap:6px 12px}
                dt{font-weight:700;color:var(--shaft-text-muted)}dd{margin:0;overflow-wrap:anywhere}
                #timeline-list{max-height:560px;overflow:auto;border:1px solid var(--shaft-border,#ccc);border-radius:6px}
                .timeline-entry{display:flex;gap:10px;align-items:baseline;padding:5px 10px;border-left:3px solid transparent;border-bottom:1px solid var(--shaft-border,#eee)}
                .timeline-entry.failed{border-left-color:var(--shaft-fail)}
                .timeline-entry.passed{border-left-color:var(--shaft-pass)}
                .timeline-entry.clickable{cursor:pointer}
                .timeline-entry.clickable:hover{background:rgba(var(--shaft-primary-rgb),.08)}
                .timeline-entry.selected{background:rgba(var(--shaft-primary-rgb),.14)}
                .time-cell{white-space:nowrap;font-variant-numeric:tabular-nums;color:var(--shaft-text-muted);min-width:86px}
                .badge{font-size:.72em;font-weight:700;letter-spacing:.4px;padding:1px 7px;border-radius:10px;background:var(--shaft-surface);border:1px solid var(--shaft-border,#ccc);color:var(--shaft-text-muted);min-width:52px;text-align:center;flex:none}
                .badge.kind-action{color:var(--shaft-primary);border-color:var(--shaft-primary)}
                .timeline-label{overflow-wrap:anywhere}
                .trace-table{width:100%;border-collapse:collapse;font-size:.86em}
                .trace-table th,.trace-table td{padding:4px 8px;border-bottom:1px solid var(--shaft-border,#eee);text-align:left;vertical-align:top;overflow-wrap:anywhere}
                .trace-table tbody tr{cursor:pointer}
                .trace-table tbody tr:hover{background:rgba(var(--shaft-primary-rgb),.08)}
                .trace-table tr.inwindow{background:rgba(var(--shaft-primary-rgb),.10)}
                .trace-table tr.failed td{color:var(--shaft-fail)}
                @media(max-width:900px){.trace-layout{grid-template-columns:1fr}}
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
                <div class="trace-layout">
                  <aside class="panel">
                    <h2>Actions</h2>
                    <div class="toolbar"><input id="action-search" type="search" placeholder="Search actions"></div>
                    <div id="action-list"></div>
                  </aside>
                  <section class="panel">
                    <div class="toolbar">
                      <h2 id="details-title">Trace Details</h2>
                      <button type="button" class="secondary" onclick="copyJson()">Copy JSON</button>
                    </div>
                    <dl id="details"></dl>
                    <div class="tabs" id="action-tabs">
                      <button data-tab="timeline" class="selected">Timeline</button>
                      <button data-tab="exception">Exception</button>
                      <button data-tab="source">Source</button>
                      <button data-tab="snapshot">Snapshot</button>
                      <button data-tab="domSnapshot">DOM Snapshot</button>
                      <button data-tab="screenshot">Screenshot</button>
                      <button data-tab="locatorHealth">Locator Health</button>
                      <button data-tab="network">Network</button>
                      <button data-tab="console">Console</button>
                      <button data-tab="browserObservability">Observability</button>
                      <button data-tab="environment">Environment</button>
                      <button data-tab="attachments">Attachments</button>
                      <button data-tab="log">Test Log</button>
                      <button data-tab="json">JSON</button>
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
                              style="width:100%;height:420px;border:1px solid var(--shaft-border,#ccc);background:#fff"></iframe>
                    </div>
                    <div id="screenshot-panel" hidden>
                      <img id="screenshot-image" alt="Action screenshot"
                           style="max-width:100%;border:1px solid var(--shaft-border,#ccc)">
                      <p id="screenshot-empty" class="muted" hidden>No screenshot captured for this action.</p>
                    </div>
                    <div id="network-panel" hidden>
                      <p class="muted" id="network-hint"></p>
                      <table class="trace-table"><thead><tr><th>Time</th><th>Method</th><th>Status</th><th>ms</th><th>URL</th></tr></thead><tbody id="network-rows"></tbody></table>
                      <pre id="network-detail" hidden></pre>
                    </div>
                    <div id="console-panel" hidden>
                      <p class="muted" id="console-hint"></p>
                      <table class="trace-table"><thead><tr><th>Time</th><th>Level</th><th>Message</th></tr></thead><tbody id="console-rows"></tbody></table>
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
                const actions = Array.isArray(trace.actions) ? trace.actions : [];
                const network = Array.isArray(trace.network) ? trace.network : [];
                const consoleEvents = Array.isArray(trace.console) ? trace.console : [];
                const actionList = document.getElementById('action-list');
                const actionSearch = document.getElementById('action-search');
                const details = document.getElementById('details');
                const tabContent = document.getElementById('tab-content');
                function actionFromHash(){
                  const match = /^#action-(.+)$/.exec(decodeURIComponent(location.hash || ''));
                  return match ? actions.find(action => action.id === match[1]) : null;
                }
                let selected = actionFromHash()
                    || [...actions].reverse().find(action => action.status !== 'passed')
                    || actions[0] || null;
                function selectAction(action){
                  selected = action;
                  if (action && action.id) {
                    history.replaceState(null, '', '#action-' + encodeURIComponent(action.id));
                  }
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
                function offsetLabel(t){
                  return t == null || baseTime == null ? '' : '+' + ((t - baseTime) / 1000).toFixed(3) + 's';
                }
                function selectedWindow(){
                  const start = actionStartMs(selected);
                  if (start == null) return null;
                  return [start - 250, start + (selected.durationMs || 0) + 1000];
                }
                function inWindow(t, range){
                  return range != null && t != null && t >= range[0] && t <= range[1];
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
                  const attemptCard = attempt > 1 || retried
                    ? `<div class="metric-card"><div class="metric-label">Attempt</div><div class="metric-value"><span class="status-chip warn">#${attempt}${retried ? ' retried' : ''}</span></div></div>`
                    : '';
                  document.getElementById('trace-summary').innerHTML = `
                    <h2>Run Snapshot</h2>
                    <div class="metric-grid">
                      <div class="metric-card"><div class="metric-label">Status</div><div class="metric-value"><span class="status-chip ${statusClass(test.status)}">${esc(test.status || 'unknown')}</span></div></div>
                      ${attemptCard}
                      <div class="metric-card"><div class="metric-label">Actions</div><div class="metric-value">${actions.length}${failedActions ? ` <span class="status-chip failed">${failedActions} failed</span>` : ''}</div></div>
                      <div class="metric-card"><div class="metric-label">Network</div><div class="metric-value">${network.length}${failedNetwork ? ` <span class="status-chip failed">${failedNetwork} failed</span>` : ''}</div></div>
                      <div class="metric-card"><div class="metric-label">Console Errors</div><div class="metric-value">${consoleErrors}</div></div>
                      <div class="metric-card"><div class="metric-label">Exception</div><div class="metric-value">${esc(exception.type || 'None')}</div></div>
                    </div>`;
                  if (truncation.length) {
                    document.getElementById('truncation-banner').hidden = false;
                    document.getElementById('truncation-detail').textContent =
                      `These bundle entries exceeded shaft.trace.maxArtifactMb and were replaced with omission markers: ${truncation.join(', ')}. Raise the cap to capture them in full.`;
                  }
                }
                function renderActions(){
                  actionList.innerHTML = '';
                  if(!actions.length){ actionList.textContent = 'No structured actions recorded.'; return; }
                  const query = actionSearch.value.toLowerCase();
                  actions.filter(action => !query || JSON.stringify(action).toLowerCase().includes(query)).forEach(action => {
                    const button = document.createElement('button');
                    button.className = `action ${action.status}${selected && selected.id === action.id ? ' selected' : ''}`;
                    button.innerHTML = `<strong>${esc(action.name || 'Action')}</strong><div class="muted">${esc(action.category)} - ${esc(action.status)} - ${esc(action.durationMs || 0)}ms${action.screenshot ? ' 📷' : ''}</div>`;
                    button.addEventListener('click', () => selectAction(action));
                    actionList.appendChild(button);
                  });
                }
                function row(name, value){ return value ? `<dt>${esc(name)}</dt><dd>${esc(value)}</dd>` : ''; }
                function renderDetails(){
                  const action = selected || {};
                  document.getElementById('details-title').textContent = action.name ? `Action: ${action.name}` : 'Trace Details';
                  const metadata = action.metadata || {};
                  details.innerHTML = row('Status', action.status) + row('Category', action.category)
                    + row('Expected', metadata.expected) + row('Actual', metadata.actual)
                    + row('Locator', action.locator) + row('URL', action.url) + row('Caller', action.caller) + row('Started', action.startTime) + row('Duration', action.durationMs == null ? '' : `${action.durationMs}ms`) + row('Message', action.message);
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
                  visibleEntries.forEach(entry => {
                    const div = document.createElement('div');
                    const isSelected = entry.action && selected && entry.action.id === selected.id;
                    div.className = `timeline-entry ${entry.status}${entry.action ? ' clickable' : ''}${isSelected ? ' selected' : ''}`;
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
                function renderNetwork(){
                  const range = selectedWindow();
                  document.getElementById('network-hint').textContent = network.length
                    ? 'Click a request for headers and body preview.' + (range ? ' Highlighted rows overlap the selected action.' : '')
                    : 'No network exchanges were recorded.';
                  networkRows.innerHTML = '';
                  networkDetail.hidden = true;
                  network.forEach(entry => {
                    const tr = document.createElement('tr');
                    tr.className = `${networkFailed(entry) ? 'failed' : ''}${inWindow(networkStartMs(entry), range) ? ' inwindow' : ''}`;
                    tr.innerHTML = `<td class="time-cell">${esc(offsetLabel(networkStartMs(entry)))}</td><td>${esc(entry.method)}</td><td>${esc(entry.status || 'FAILED')}</td><td>${esc(entry.durationMs || 0)}</td><td>${esc(entry.url)}</td>`;
                    tr.addEventListener('click', () => { networkDetail.hidden = false; networkDetail.textContent = JSON.stringify(entry, null, 2); });
                    networkRows.appendChild(tr);
                  });
                }
                const consolePanel = document.getElementById('console-panel');
                const consoleRows = document.getElementById('console-rows');
                function renderConsole(){
                  const range = selectedWindow();
                  document.getElementById('console-hint').textContent = consoleEvents.length
                    ? (range ? 'Highlighted rows overlap the selected action.' : '')
                    : 'No console messages were recorded.';
                  consoleRows.innerHTML = '';
                  consoleEvents.forEach(entry => {
                    const tr = document.createElement('tr');
                    tr.className = `${consoleFailed(entry) ? 'failed' : ''}${inWindow(entry.timestamp, range) ? ' inwindow' : ''}`;
                    tr.innerHTML = `<td class="time-cell">${esc(offsetLabel(entry.timestamp))}</td><td>${esc(entry.level)}</td><td>${esc(entry.message)}</td>`;
                    consoleRows.appendChild(tr);
                  });
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
                let selectedDomSide = 'before';
                function renderDomSnapshot(){
                  const action = selected || {};
                  const html = domSnapshotFrame && (selectedDomSide === 'after' ? action.domSnapshotAfter : action.domSnapshotBefore);
                  if (domSnapshotFrame) {
                    domSnapshotFrame.srcdoc = html || '<p>No DOM snapshot captured for this action.</p>';
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
                function renderTab(tab){
                  const action = selected || {};
                  const panels = {timeline: timelinePanel, domSnapshot: domSnapshotPanel, screenshot: screenshotPanel, network: networkPanel, console: consolePanel};
                  tabContent.hidden = tab in panels;
                  Object.entries(panels).forEach(([name, panel]) => panel.hidden = name !== tab);
                  if (tab === 'timeline') {
                    renderTimeline();
                  } else if (tab === 'domSnapshot') {
                    renderDomSnapshot();
                  } else if (tab === 'screenshot') {
                    renderScreenshot();
                  } else if (tab === 'network') {
                    renderNetwork();
                  } else if (tab === 'console') {
                    renderConsole();
                  } else if (tab === 'source') {
                    tabContent.textContent = sourceText();
                  } else if (tab === 'log') {
                    tabContent.textContent = Array.isArray(trace.timeline) && trace.timeline.length ? trace.timeline.join('\\n') : 'No test log lines were recorded.';
                  } else {
                    const data = tab === 'json' ? trace : tab === 'exception' && action.exception && (action.exception.type || action.exception.message) ? action.exception : trace[tab];
                    tabContent.textContent = typeof data === 'string' ? data : JSON.stringify(data || {}, null, 2);
                  }
                  document.querySelectorAll('#action-tabs button').forEach(button => button.classList.toggle('selected', button.dataset.tab === tab));
                }
                async function copyJson(){
                  await navigator.clipboard.writeText(JSON.stringify(trace, null, 2));
                }
                actionSearch.addEventListener('input', renderActions);
                document.querySelectorAll('#timeline-filters button').forEach(button => button.addEventListener('click', () => {
                  timelineFilter = button.dataset.filter;
                  document.querySelectorAll('#timeline-filters button').forEach(other => other.classList.toggle('selected', other === button));
                  renderTimeline();
                }));
                document.querySelectorAll('#action-tabs button').forEach(button => button.addEventListener('click', () => renderTab(button.dataset.tab)));
                document.querySelectorAll('#dom-snapshot-tabs button').forEach(button => button.addEventListener('click', () => { selectedDomSide = button.dataset.dom; renderDomSnapshot(); }));
                renderSummary();
                renderActions();
                renderDetails();
                </script>
                </body>
                </html>
                """;
    }

    private static void renderTraceZip(Path target, String json, String html, String networkJson,
                                       Map<String, byte[]> screenshots) {
        int maxBytes = Math.max(1, SHAFT.Properties.reporting.traceMaxArtifactMb()) * 1024 * 1024;
        String omissionMarker = "Omitted because artifact exceeded shaft.trace.maxArtifactMb="
                + SHAFT.Properties.reporting.traceMaxArtifactMb();
        TraceArtifactManifest manifest = CURRENT_ARTIFACT_MANIFEST.get();
        TraceArchiveWriter.Entry nativeEntry = manifest == null ? null : manifest.nativeEntry();
        renderTraceZip(target, json, html, networkJson, screenshots, nativeEntry, maxBytes, omissionMarker);
    }

    static void renderTraceZip(Path target, String json, String html, String networkJson,
                               Map<String, byte[]> screenshots, Path nativeTrace, int maxBytes,
                               String omissionMarker) {
        TraceArchiveWriter.Entry nativeEntry = nativeTrace == null
                ? null
                : TraceArchiveWriter.Entry.optionalFile(nativeTrace.getFileName().toString(), nativeTrace);
        renderTraceZip(target, json, html, networkJson, screenshots, nativeEntry, maxBytes, omissionMarker);
    }

    private static void renderTraceZip(Path target, String json, String html, String networkJson,
                                       Map<String, byte[]> screenshots, TraceArchiveWriter.Entry nativeEntry,
                                       int maxBytes, String omissionMarker) {
        List<TraceArchiveWriter.Entry> entries = new ArrayList<>();
        entries.add(TraceArchiveWriter.Entry.text("shaft-trace.json", json));
        entries.add(TraceArchiveWriter.Entry.text("shaft-network.har",
                BrowserObservabilityRecorder.networkHarJson(networkJson)));
        entries.add(TraceArchiveWriter.Entry.text("SHAFT Trace Report.html", html));
        for (Map.Entry<String, byte[]> entry : screenshots.entrySet()) {
            entries.add(TraceArchiveWriter.Entry.bytes("screenshots/" + entry.getKey() + ".png", entry.getValue()));
        }
        if (nativeEntry != null) {
            entries.add(nativeEntry);
        }
        try {
            TraceArchiveWriter.write(target, entries, maxBytes, omissionMarker);
        } catch (IOException e) {
            throw new IllegalStateException("Could not create SHAFT trace zip.", e);
        }
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

    static void persistTraceArtifacts(TestExecutionInfo info, Path completedArchive, Map<String, byte[]> screenshots,
                                      int attempt, List<String> omitted) {
        try {
            Path directory = traceDirectory(info);
            Files.createDirectories(directory);
            boolean failed = info != null && info.throwable() != null;
            String archiveName = "shaft-trace.zip";
            // Retain failed-attempt bundles under attempt-indexed names so a later passing retry
            // (which rewrites shaft-trace.zip) never erases the flake evidence.
            if (failed && retriesConfigured() && SHAFT.Properties.reporting.traceRetainFailedAttempts()) {
                archiveName = "shaft-trace-attempt-" + attempt + ".zip";
                TraceArchiveWriter.copy(completedArchive, directory.resolve(archiveName));
            }
            String testId = safeTestId(info);
            synchronized (TRACE_LOCKS.computeIfAbsent(testId, id -> new Object())) {
                recordAttempt(info, attempt, failed ? "failed" : "passed", archiveName);
                if (publishLatest(testId, attempt, completedArchive, directory.resolve("shaft-trace.zip"))) {
                    LATEST_INDEX.put(testId, new TraceIndexSnapshot(info, !screenshots.isEmpty(), attempt,
                            List.copyOf(omitted)));
                    Files.deleteIfExists(directory.resolve("SHAFT Trace Report.html"));
                    Files.deleteIfExists(directory.resolve("shaft-trace.json"));
                    if (!screenshots.isEmpty()) {
                        Path screenshotsDirectory = directory.resolve("screenshots");
                        Files.createDirectories(screenshotsDirectory);
                        for (Map.Entry<String, byte[]> entry : screenshots.entrySet()) {
                            Files.write(screenshotsDirectory.resolve(entry.getKey() + ".png"), entry.getValue());
                        }
                    }
                }
                TraceIndexSnapshot latest = LATEST_INDEX.get(testId);
                if (latest != null) {
                    Files.writeString(directory.resolve("index.json"),
                            renderTraceIndexJson(latest.info(), directory.resolve("shaft-trace.zip"),
                                    latest.hasScreenshots(), latest.attempt(), latest.omitted()), StandardCharsets.UTF_8);
                }
            }
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete("Could not persist SHAFT trace artifacts: " + e.getMessage(), Level.WARN);
        }
    }

    private static void recordAttempt(TestExecutionInfo info, int attempt, String status, String archiveName) {
        ATTEMPT_HISTORY.computeIfAbsent(safeTestId(info), id -> java.util.Collections.synchronizedList(new ArrayList<>()))
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

    private static String renderTraceIndexJson(TestExecutionInfo info, Path zipPath, boolean hasScreenshots,
                                               int attempt, List<String> omitted) {
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
        array(json, 1, "omittedEntries", omitted, true);
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
            return new Snapshot("disabled", "");
        }
        try {
            Page page = PlaywrightSessionManager.currentPage();
            if (page != null && SHAFT.Properties.reporting.traceIncludeFullPageSnapshots()) {
                return new Snapshot("playwright-html", redact(page.content()));
            }
        } catch (RuntimeException ignored) {
            // Snapshot collection is best-effort; trace generation must never hide the original failure.
        }
        WebDriver driver = DriverFactoryHelper.getActiveDriver();
        if (driver == null) {
            return new Snapshot("unavailable", "No active browser or native driver was registered for this thread.");
        }
        try {
            return new Snapshot(DriverFactoryHelper.isMobileNativeExecution() ? "native-page-source" : "webdriver-page-source",
                    redact(driver.getPageSource()));
        } catch (RuntimeException e) {
            return new Snapshot("unavailable", "Snapshot capture failed: " + e.getMessage());
        }
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

    private static List<String> timeline(String logText) {
        if (logText == null || logText.isBlank()) {
            return List.of();
        }
        List<String> timeline = new ArrayList<>();
        for (String line : logText.split("\\R")) {
            if (!line.isBlank()) {
                timeline.add(redact(line));
            }
        }
        return timeline;
    }

    private static List<String> attachmentEntries(List<String> attachments) {
        List<String> entries = new ArrayList<>();
        if (attachments != null) {
            attachments.stream()
                    .filter(attachment -> attachment != null && !attachment.isBlank())
                    .map(FailureTraceReporter::redact)
                    .forEach(entries::add);
        }
        Path playwrightTrace = PlaywrightTraceManager.getLastTracePath();
        if (playwrightTrace != null) {
            entries.add("Playwright Trace (raw): " + redact(playwrightTrace.toString()));
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
            return "[provider error text omitted because submitted script data may be sensitive]";
        }
        String redacted = value(value);
        for (String sensitiveValue : EXACT_SENSITIVE_VALUES.get()) {
            if (redacted.contains(sensitiveValue)) {
                if (sensitiveValue.length() < 4) {
                    return "[provider error text omitted because it may contain a sensitive storage value]";
                }
                redacted = redacted.replace(sensitiveValue, "********");
            }
        }
        return redact(redacted);
    }

    /** Registers an exact value for current-invocation trace redaction. */
    public static void registerSensitiveValue(String value) {
        if (value != null && !value.isEmpty()) {
            EXACT_SENSITIVE_VALUES.get().add(value);
        }
    }

    /** Registers a credential that must be removed from later source-code evidence in this invocation. */
    public static void registerSensitiveSourceValue(String value) {
        if (value != null && !value.isEmpty()) {
            SOURCE_SENSITIVE_VALUES.get().add(value);
        }
    }

    private static String redactSourceText(String value) {
        String redacted = redact(value);
        for (String sensitiveValue : SOURCE_SENSITIVE_VALUES.get()) {
            if (!redacted.contains(sensitiveValue)) {
                continue;
            }
            if (sensitiveValue.length() < 4) {
                return "[source context omitted because it contains a sensitive credential]";
            }
            redacted = redacted.replace(sensitiveValue, "********");
        }
        return redacted;
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

    private static boolean containsSensitiveThrowable(Throwable root) {
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
                                      List<String> omitted) {
    }

    private record Snapshot(String type, String content) {
    }
}
