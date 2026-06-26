package com.shaft.tools.io.internal;

import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.WebDriver;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
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
    private static final ThreadLocal<String> CURRENT_NETWORK_JSON = ThreadLocal.withInitial(() -> "[]");

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
            byte[] zip = renderTraceZip(json, html, CURRENT_NETWORK_JSON.get());
            persistTraceArtifacts(info, zip);
            attach("zip", "shaft-trace.zip", zip, "shaft-trace.zip");
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete("Could not attach SHAFT trace report: " + e.getMessage(), Level.WARN);
        } finally {
            CURRENT_NETWORK_JSON.remove();
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
        objectStart(json, 1, "exception");
        field(json, 2, "type", throwable == null ? "" : throwable.getClass().getName(), true);
        field(json, 2, "message", redact(throwable == null ? "" : throwable.getMessage()), true);
        field(json, 2, "stacktrace", redact(ReportManagerHelper.formatStackTraceToLogEntry(throwable)), false);
        objectEnd(json, 1, true);
        objectStart(json, 1, "source");
        field(json, 2, "frame", source.frame(), true);
        field(json, 2, "file", source.file(), true);
        field(json, 2, "line", source.line(), true);
        field(json, 2, "snippet", source.snippet(), false);
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
                <title>SHAFT Trace Report</title>
                <style>
                body{font-family:Arial,sans-serif;margin:0;background:#f7f9fb;color:#17202a}
                header{background:#17202a;color:#fff;padding:16px 24px}
                main{display:grid;grid-template-columns:320px 1fr;gap:14px;padding:14px}
                section,aside{background:#fff;border:1px solid #d7dde5;border-radius:6px;min-width:0}
                aside{padding:12px}
                section{padding:14px}
                h1{font-size:22px;margin:0} h2{font-size:16px;margin:0 0 10px}
                a{color:#0b63ce}
                .links{display:flex;gap:10px;margin:10px 0 12px;flex-wrap:wrap}
                .action{width:100%;text-align:left;border:1px solid #d7dde5;background:#fff;border-radius:4px;margin:0 0 8px;padding:9px;cursor:pointer}
                .action:hover,.action.selected{border-color:#0b63ce;background:#edf5ff}
                .action.failed{border-left:4px solid #cc2936}.action.passed{border-left:4px solid #168a45}
                .meta{color:#5b6570;font-size:12px;margin-top:4px}
                .tabs{display:flex;gap:8px;flex-wrap:wrap;margin:14px 0}
                .tabs button{border:1px solid #c5ccd6;background:#fff;border-radius:4px;padding:7px 10px;cursor:pointer}
                .tabs button.selected{background:#17202a;color:#fff;border-color:#17202a}
                pre{white-space:pre-wrap;word-break:break-word;background:#0f1720;color:#e6edf3;padding:12px;border-radius:4px;max-height:65vh;overflow:auto}
                dl{display:grid;grid-template-columns:130px 1fr;gap:6px 12px}
                dt{font-weight:bold;color:#39424e}dd{margin:0;word-break:break-word}
                @media(max-width:800px){main{grid-template-columns:1fr}}
                </style>
                </head>
                <body>
                <header><h1>SHAFT Trace Report</h1></header>
                <main>
                <aside>
                <h2>Actions</h2>
                <div id="action-list"></div>
                </aside>
                <section>
                <h2 id="details-title">Trace Details</h2>
                <dl id="details"></dl>
                <div class="tabs">
                <button data-tab="exception" class="selected">Exception</button>
                <button data-tab="source">Source</button>
                <button data-tab="snapshot">Snapshot</button>
                <button data-tab="locatorHealth">Locator Health</button>
                <button data-tab="network">Network</button>
                <button data-tab="console">Console</button>
                <button data-tab="browserObservability">Observability</button>
                <button data-tab="json">JSON</button>
                </div>
                <pre id="tab-content"></pre>
                </section>
                </main>
                <pre hidden id="trace-data">""" + escapedJson + """
                </pre>
                <script>
                const trace = JSON.parse(document.getElementById('trace-data').textContent);
                const actions = Array.isArray(trace.actions) ? trace.actions : [];
                const actionList = document.getElementById('action-list');
                const details = document.getElementById('details');
                const tabContent = document.getElementById('tab-content');
                let selected = actions.find(action => action.status !== 'passed') || actions[0] || null;
                function esc(value){
                  return String(value || '').replace(/[&<>"']/g, char => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[char]));
                }
                function renderActions(){
                  actionList.innerHTML = '';
                  if(!actions.length){ actionList.textContent = 'No structured actions recorded.'; return; }
                  actions.forEach(action => {
                    const button = document.createElement('button');
                    button.className = `action ${action.status}${selected && selected.id === action.id ? ' selected' : ''}`;
                    button.innerHTML = `<strong>${esc(action.name || 'Action')}</strong><div class="meta">${esc(action.category)} - ${esc(action.status)} - ${esc(action.durationMs || 0)}ms</div>`;
                    button.addEventListener('click', () => { selected = action; renderActions(); renderDetails(); });
                    actionList.appendChild(button);
                  });
                }
                function row(name, value){ return `<dt>${esc(name)}</dt><dd>${esc(value)}</dd>`; }
                function renderDetails(){
                  const action = selected || {};
                  document.getElementById('details-title').textContent = action.name ? `Action: ${action.name}` : 'Trace Details';
                  details.innerHTML = row('Status', action.status) + row('Category', action.category) + row('Locator', action.locator) + row('URL', action.url) + row('Duration', action.durationMs == null ? '' : `${action.durationMs}ms`) + row('Message', action.message);
                  renderTab(document.querySelector('.tabs button.selected').dataset.tab);
                }
                function renderTab(tab){
                  const action = selected || {};
                  const data = tab === 'json' ? trace : tab === 'exception' && action.exception && (action.exception.type || action.exception.message) ? action.exception : trace[tab];
                  tabContent.textContent = typeof data === 'string' ? data : JSON.stringify(data || {}, null, 2);
                  document.querySelectorAll('.tabs button').forEach(button => button.classList.toggle('selected', button.dataset.tab === tab));
                }
                document.querySelectorAll('.tabs button').forEach(button => button.addEventListener('click', () => renderTab(button.dataset.tab)));
                renderActions();
                renderDetails();
                </script>
                </body>
                </html>
                """;
    }

    private static byte[] renderTraceZip(String json, String html, String networkJson) {
        int maxBytes = Math.max(1, SHAFT.Properties.reporting.traceMaxArtifactMb()) * 1024 * 1024;
        try (ByteArrayOutputStream output = new ByteArrayOutputStream();
             ZipOutputStream zip = new ZipOutputStream(output)) {
            addZipEntry(zip, "shaft-trace.json", json.getBytes(StandardCharsets.UTF_8), maxBytes);
            addZipEntry(zip, "shaft-network.har", renderNetworkHarJson(networkJson).getBytes(StandardCharsets.UTF_8), maxBytes);
            addZipEntry(zip, "SHAFT Trace Report.html", html.getBytes(StandardCharsets.UTF_8), maxBytes);
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

    private static String renderNetworkHarJson(String networkJson) {
        return """
                {
                  "log": {
                    "version": "1.2",
                    "creator": {
                      "name": "SHAFT",
                      "comment": "HAR-like browser network trace emitted by SHAFT observability"
                    },
                    "entries": %s
                  }
                }
                """.formatted(networkJson == null || networkJson.isBlank() ? "[]" : networkJson);
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

    private static void persistTraceArtifacts(TestExecutionInfo info, byte[] zip) {
        try {
            Path directory = traceDirectory(info);
            Files.createDirectories(directory);
            Path zipPath = directory.resolve("shaft-trace.zip");
            Files.deleteIfExists(directory.resolve("SHAFT Trace Report.html"));
            Files.deleteIfExists(directory.resolve("shaft-trace.json"));
            Files.write(zipPath, zip);
            Files.writeString(directory.resolve("index.json"), renderTraceIndexJson(info, zipPath),
                    StandardCharsets.UTF_8);
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

    private static String renderTraceIndexJson(TestExecutionInfo info, Path zipPath) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        field(json, 1, "testId", safeTestId(info), true);
        field(json, 1, "generatedAt", Instant.now().toString(), true);
        field(json, 1, "archive", relative(zipPath), true);
        objectStart(json, 1, "entries");
        field(json, 2, "html", "SHAFT Trace Report.html", true);
        field(json, 2, "json", "shaft-trace.json", true);
        field(json, 2, "network", "shaft-network.har", false);
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
            return new SourceContext("", "", "", "");
        }
        StackTraceElement frame = relevantFrame(info.throwable());
        if (frame == null) {
            return new SourceContext("", "", "", "");
        }
        Path sourceFile = findSourceFile(frame);
        if (sourceFile == null) {
            return new SourceContext(frame.toString(), "", String.valueOf(frame.getLineNumber()), frame.toString());
        }
        return new SourceContext(frame.toString(), relative(sourceFile), String.valueOf(frame.getLineNumber()),
                snippet(sourceFile, frame.getLineNumber()));
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

    private record SourceContext(String frame, String file, String line, String snippet) {
    }

    private record Snapshot(String type, String content) {
    }
}
