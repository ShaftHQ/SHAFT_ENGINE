package com.shaft.tools.io.internal;

import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.WebDriver;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

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
        try {
            stopPlaywrightTraceIfRunning();
            String json = renderTraceJson(info, logText, attachments);
            String html = renderTraceHtml(json);
            Map<String, byte[]> screenshots = CURRENT_SCREENSHOTS.get();
            byte[] zip = renderTraceZip(json, html, CURRENT_NETWORK_JSON.get(), screenshots);
            persistTraceArtifacts(info, zip, screenshots);
            attach("zip", "shaft-trace.zip", zip, "shaft-trace.zip");
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete("Could not attach SHAFT trace report: " + e.getMessage(), Level.WARN);
        } finally {
            CURRENT_NETWORK_JSON.remove();
            CURRENT_SCREENSHOTS.remove();
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
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        field(json, 1, "schemaVersion", "1.0", true);
        field(json, 1, "generatedAt", Instant.now().toString(), true);
        objectStart(json, 1, "test");
        field(json, 2, "className", value(info == null ? null : info.className()), true);
        field(json, 2, "methodName", value(info == null ? null : info.methodName()), true);
        field(json, 2, "displayName", value(info == null ? null : info.displayName()), true);
        field(json, 2, "description", value(info == null ? null : info.description()), true);
        field(json, 2, "status", throwable == null ? "passed" : "failed", false);
        objectEnd(json, 1, true);
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
        objectStart(json, 1, "exception");
        field(json, 2, "type", throwable == null ? "" : throwable.getClass().getName(), true);
        field(json, 2, "message", redact(throwable == null ? "" : throwable.getMessage()), true);
        field(json, 2, "stacktrace", redact(ReportManagerHelper.formatStackTraceToLogEntry(throwable)), false);
        objectEnd(json, 1, true);
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
        String mode = SHAFT.Properties.reporting.traceMode().toLowerCase(Locale.ROOT).trim();
        return switch (mode) {
            case "always" -> true;
            case "retry" -> info.throwable() != null || info.retried();
            default -> info.throwable() != null;
        };
    }

    private static String renderTraceHtml(String json) {
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
                <script>
                const trace = JSON.parse(document.getElementById('trace-data').textContent);
                const actions = Array.isArray(trace.actions) ? trace.actions : [];
                const network = Array.isArray(trace.network) ? trace.network : [];
                const consoleEvents = Array.isArray(trace.console) ? trace.console : [];
                const actionList = document.getElementById('action-list');
                const actionSearch = document.getElementById('action-search');
                const details = document.getElementById('details');
                const tabContent = document.getElementById('tab-content');
                let selected = actions.find(action => action.status !== 'passed') || actions[0] || null;
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
                  document.getElementById('trace-subtitle').textContent = `${test.className || 'Unknown class'}.${test.methodName || 'unknown'} - ${trace.generatedAt || ''}`;
                  document.getElementById('trace-summary').innerHTML = `
                    <h2>Run Snapshot</h2>
                    <div class="metric-grid">
                      <div class="metric-card"><div class="metric-label">Status</div><div class="metric-value"><span class="status-chip ${statusClass(test.status)}">${esc(test.status || 'unknown')}</span></div></div>
                      <div class="metric-card"><div class="metric-label">Actions</div><div class="metric-value">${actions.length}${failedActions ? ` <span class="status-chip failed">${failedActions} failed</span>` : ''}</div></div>
                      <div class="metric-card"><div class="metric-label">Network</div><div class="metric-value">${network.length}${failedNetwork ? ` <span class="status-chip failed">${failedNetwork} failed</span>` : ''}</div></div>
                      <div class="metric-card"><div class="metric-label">Console Errors</div><div class="metric-value">${consoleErrors}</div></div>
                      <div class="metric-card"><div class="metric-label">Exception</div><div class="metric-value">${esc(exception.type || 'None')}</div></div>
                    </div>`;
                }
                function renderActions(){
                  actionList.innerHTML = '';
                  if(!actions.length){ actionList.textContent = 'No structured actions recorded.'; return; }
                  const query = actionSearch.value.toLowerCase();
                  actions.filter(action => !query || JSON.stringify(action).toLowerCase().includes(query)).forEach(action => {
                    const button = document.createElement('button');
                    button.className = `action ${action.status}${selected && selected.id === action.id ? ' selected' : ''}`;
                    button.innerHTML = `<strong>${esc(action.name || 'Action')}</strong><div class="muted">${esc(action.category)} - ${esc(action.status)} - ${esc(action.durationMs || 0)}ms${action.screenshot ? ' 📷' : ''}</div>`;
                    button.addEventListener('click', () => { selected = action; renderActions(); renderDetails(); });
                    actionList.appendChild(button);
                  });
                }
                function row(name, value){ return value ? `<dt>${esc(name)}</dt><dd>${esc(value)}</dd>` : ''; }
                function renderDetails(){
                  const action = selected || {};
                  document.getElementById('details-title').textContent = action.name ? `Action: ${action.name}` : 'Trace Details';
                  details.innerHTML = row('Status', action.status) + row('Category', action.category) + row('Locator', action.locator) + row('URL', action.url) + row('Caller', action.caller) + row('Started', action.startTime) + row('Duration', action.durationMs == null ? '' : `${action.durationMs}ms`) + row('Message', action.message);
                  renderTab(document.querySelector('.tabs button.selected').dataset.tab);
                }
                const timelinePanel = document.getElementById('timeline-panel');
                const timelineList = document.getElementById('timeline-list');
                function renderTimeline(){
                  timelineList.innerHTML = '';
                  if (!allEntries.length) { timelineList.textContent = 'No timeline events were recorded.'; return; }
                  allEntries.forEach(entry => {
                    const div = document.createElement('div');
                    const isSelected = entry.action && selected && entry.action.id === selected.id;
                    div.className = `timeline-entry ${entry.status}${entry.action ? ' clickable' : ''}${isSelected ? ' selected' : ''}`;
                    const duration = entry.durationMs ? ` (${entry.durationMs}ms)` : '';
                    div.innerHTML = `<span class="time-cell">${esc(offsetLabel(entry.t))}</span><span class="badge kind-${entry.kind}">${entry.kind.toUpperCase()}</span><span class="timeline-label">${esc(entry.label)}${esc(duration)}</span>`;
                    if (entry.action) {
                      div.addEventListener('click', () => { selected = entry.action; renderActions(); renderDetails(); });
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

    private static byte[] renderTraceZip(String json, String html, String networkJson, Map<String, byte[]> screenshots) {
        int maxBytes = Math.max(1, SHAFT.Properties.reporting.traceMaxArtifactMb()) * 1024 * 1024;
        try (ByteArrayOutputStream output = new ByteArrayOutputStream();
             ZipOutputStream zip = new ZipOutputStream(output)) {
            addZipEntry(zip, "shaft-trace.json", json.getBytes(StandardCharsets.UTF_8), maxBytes);
            addZipEntry(zip, "shaft-network.har", BrowserObservabilityRecorder.networkHarJson(networkJson)
                    .getBytes(StandardCharsets.UTF_8), maxBytes);
            addZipEntry(zip, "SHAFT Trace Report.html", html.getBytes(StandardCharsets.UTF_8), maxBytes);
            for (Map.Entry<String, byte[]> entry : screenshots.entrySet()) {
                addZipEntry(zip, "screenshots/" + entry.getKey() + ".png", entry.getValue(), maxBytes);
            }
            Path playwrightTrace = PlaywrightTraceManager.getLastTracePath();
            if (playwrightTrace != null && Files.isRegularFile(playwrightTrace)) {
                addZipEntry(zip, playwrightTrace.getFileName().toString(), Files.readAllBytes(playwrightTrace), maxBytes);
            }
            zip.finish();
            return output.toByteArray();
        } catch (IOException e) {
            throw new IllegalStateException("Could not create SHAFT trace zip.", e);
        }
    }

    private static void addZipEntry(ZipOutputStream zip, String name, byte[] bytes, int maxBytes) throws IOException {
        zip.putNextEntry(new ZipEntry(name));
        if (bytes.length <= maxBytes) {
            zip.write(bytes);
        } else {
            zip.write(("Omitted because artifact exceeded shaft.trace.maxArtifactMb=" + SHAFT.Properties.reporting.traceMaxArtifactMb())
                    .getBytes(StandardCharsets.UTF_8));
        }
        zip.closeEntry();
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

    private static void persistTraceArtifacts(TestExecutionInfo info, byte[] zip, Map<String, byte[]> screenshots) {
        try {
            Path directory = traceDirectory(info);
            Files.createDirectories(directory);
            Path zipPath = directory.resolve("shaft-trace.zip");
            Files.deleteIfExists(directory.resolve("SHAFT Trace Report.html"));
            Files.deleteIfExists(directory.resolve("shaft-trace.json"));
            Files.write(zipPath, zip);
            if (!screenshots.isEmpty()) {
                Path screenshotsDirectory = directory.resolve("screenshots");
                Files.createDirectories(screenshotsDirectory);
                for (Map.Entry<String, byte[]> entry : screenshots.entrySet()) {
                    Files.write(screenshotsDirectory.resolve(entry.getKey() + ".png"), entry.getValue());
                }
            }
            Files.writeString(directory.resolve("index.json"),
                    renderTraceIndexJson(info, zipPath, !screenshots.isEmpty()), StandardCharsets.UTF_8);
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete("Could not persist SHAFT trace artifacts: " + e.getMessage(), Level.WARN);
        }
    }

    static Path traceDirectory(TestExecutionInfo info) {
        return Path.of("target", "shaft-traces", safeTestId(info));
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
        return safeId.length() <= 120 ? safeId : safeId.substring(0, 120);
    }

    private static String renderTraceIndexJson(TestExecutionInfo info, Path zipPath, boolean hasScreenshots) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        field(json, 1, "testId", safeTestId(info), true);
        field(json, 1, "generatedAt", Instant.now().toString(), true);
        field(json, 1, "archive", relative(zipPath), true);
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
            return redact(content.length() > MAX_SOURCE_FILE_CHARACTERS
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
            return redact(snippet.toString().trim());
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
            entries.add(redact(playwrightTrace.toString()));
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

    private record Snapshot(String type, String content) {
    }
}
