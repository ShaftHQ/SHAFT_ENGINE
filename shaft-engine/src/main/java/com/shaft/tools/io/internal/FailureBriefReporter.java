package com.shaft.tools.io.internal;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import io.qameta.allure.model.Status;
import org.apache.logging.log4j.Level;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Builds the human-first failed-test brief and machine-readable attachment manifest.
 */
public final class FailureBriefReporter {
    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
    private static final int MAX_OPEN_FIRST = 5;

    private FailureBriefReporter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Attaches the SHAFT failure brief and attachment manifest for failed or broken tests.
     *
     * @param info        current test metadata
     * @param logText     current test log
     * @param attachments generated artifact file paths already known to SHAFT
     */
    public static void attachOnFailure(TestExecutionInfo info, String logText, List<String> attachments) {
        if (!shouldAttachBrief(info)) {
            return;
        }
        try {
            List<ReportContext.AttachmentRecord> records = mergeAttachmentRecords(
                    ReportContext.snapshotAttachments(), attachments);
            String json = renderBriefJson(info, logText, records);
            attach("html", "SHAFT Failure Brief.html", renderBriefHtml(json).getBytes(StandardCharsets.UTF_8),
                    "SHAFT Failure Brief.html");
            attach("json", "shaft-failure-brief.json", json.getBytes(StandardCharsets.UTF_8),
                    "shaft-failure-brief.json");
            attach("json", "shaft-attachments-manifest.json", renderManifestJson(records).getBytes(StandardCharsets.UTF_8),
                    "shaft-attachments-manifest.json");
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete("Could not attach SHAFT failure brief: " + e.getMessage(), Level.WARN);
        }
    }

    static String renderBriefJson(TestExecutionInfo info, String logText,
                                  List<ReportContext.AttachmentRecord> attachments) {
        FailureDiagnosticsReporter.Redactor redactor = new FailureDiagnosticsReporter.Redactor();
        Throwable throwable = info == null ? null : info.throwable();
        SourceContext source = sourceContext(throwable);
        String stacktrace = ReportManagerHelper.formatStackTraceToLogEntry(throwable);
        String message = throwable == null ? "" : throwable.getMessage();
        String category = classifyFailure(message, stacktrace, logText);
        redactor.redact(logText);

        StringBuilder json = new StringBuilder();
        json.append("{\n");
        numberField(json, 1, "schemaVersion", 1, true);
        stringField(json, 1, "status", status(), true, redactor);
        stringField(json, 1, "generatedAt", Instant.now().toString(), true, redactor);
        objectStart(json, 1, "test");
        stringField(json, 2, "className", info == null ? "" : info.className(), true, redactor);
        stringField(json, 2, "methodName", info == null ? "" : info.methodName(), true, redactor);
        stringField(json, 2, "displayName", info == null ? "" : info.displayName(), false, redactor);
        objectEnd(json, 1, true);
        objectStart(json, 1, "failure");
        stringField(json, 2, "category", category, true, redactor);
        stringField(json, 2, "type", throwable == null ? "" : throwable.getClass().getName(), true, redactor);
        stringField(json, 2, "message", message, true, redactor);
        stringField(json, 2, "topProjectFrame", source.frame(), false, redactor);
        objectEnd(json, 1, true);
        stringArray(json, 1, "openFirst", openFirst(attachments), true, redactor);
        artifacts(json, 1, attachments, true, redactor);
        objectStart(json, 1, "redaction");
        stringArray(json, 2, "rulesApplied", redactor.rulesApplied(), false, null);
        objectEnd(json, 1, false);
        json.append("}\n");
        return json.toString();
    }

    static String renderBriefHtml(String briefJson) {
        try {
            JsonNode root = JSON_MAPPER.readTree(briefJson);
            StringBuilder html = new StringBuilder();
            html.append("<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">");
            html.append("<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">");
            html.append("<title>SHAFT Failure Brief</title>");
            html.append("<style>");
            html.append(ReportHtmlTheme.style());
            html.append(".meta{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:10px}");
            html.append(".label{color:var(--shaft-text-muted);font-size:12px;font-weight:700;text-transform:uppercase}.value{font-weight:700}");
            html.append("</style></head><body>");
            html.append("<div class=\"report-shell\"><header class=\"report-header\"><div class=\"report-header-inner\">");
            html.append("<span class=\"brand-mark\">S</span><div><h1>SHAFT Failure Brief</h1>");
            html.append("<p class=\"subtitle\">Prioritized failure context and artifacts</p></div></div></header><main class=\"report-main\">");
            html.append("<section class=\"panel\"><h2>Status</h2><div class=\"meta\">");
            statusMeta(html, root.path("status").asText());
            meta(html, "Category", root.path("failure").path("category").asText());
            meta(html, "Test", root.path("test").path("className").asText() + "."
                    + root.path("test").path("methodName").asText());
            meta(html, "Top project frame", root.path("failure").path("topProjectFrame").asText());
            html.append("</div></section>");
            html.append("<section class=\"panel\"><h2>Failure Message</h2><pre>")
                    .append(escapeHtml(root.path("failure").path("message").asText()))
                    .append("</pre></section>");
            html.append("<section class=\"panel\"><h2>Open First</h2><ol>");
            for (JsonNode item : root.path("openFirst")) {
                html.append("<li>").append(escapeHtml(item.asText())).append("</li>");
            }
            html.append("</ol></section>");
            html.append("<section class=\"panel\"><h2>Artifacts</h2><div class=\"table-wrap\"><table><thead><tr><th>Description</th><th>Type</th><th>Purpose</th><th>Size</th></tr></thead><tbody>");
            for (JsonNode artifact : root.path("artifacts")) {
                html.append("<tr><td>").append(escapeHtml(artifact.path("description").asText())).append("</td><td>")
                        .append(escapeHtml(artifact.path("contentType").asText())).append("</td><td>")
                        .append(escapeHtml(artifact.path("purpose").asText())).append("</td><td>")
                        .append(artifact.path("sizeBytes").asLong()).append("</td></tr>");
            }
            html.append("</tbody></table></div></section>");
            html.append("</main></div></body></html>");
            return html.toString();
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("Could not render failure brief HTML.", e);
        }
    }

    private static boolean shouldAttachBrief(TestExecutionInfo info) {
        if (info == null || info.throwable() == null) {
            return false;
        }
        Status status = ReportContext.getStatus();
        return status == null || Status.FAILED.equals(status) || Status.BROKEN.equals(status);
    }

    private static List<ReportContext.AttachmentRecord> mergeAttachmentRecords(
            List<ReportContext.AttachmentRecord> records,
            List<String> attachmentPaths) {
        List<ReportContext.AttachmentRecord> merged = new ArrayList<>(records == null ? List.of() : records);
        Set<String> descriptions = new LinkedHashSet<>();
        for (ReportContext.AttachmentRecord record : merged) {
            descriptions.add(record.description());
        }
        if (attachmentPaths != null) {
            for (String attachmentPath : attachmentPaths) {
                if (attachmentPath == null || attachmentPath.isBlank()) {
                    continue;
                }
                ReportContext.AttachmentRecord record = recordFromPath(attachmentPath);
                if (descriptions.add(record.description())) {
                    merged.add(record);
                }
            }
        }
        return List.copyOf(merged);
    }

    private static ReportContext.AttachmentRecord recordFromPath(String attachmentPath) {
        String fileName = attachmentPath;
        long sizeBytes = -1;
        try {
            Path path = Path.of(attachmentPath);
            fileName = path.getFileName() == null ? attachmentPath : path.getFileName().toString();
            if (Files.isRegularFile(path)) {
                sizeBytes = Files.size(path);
            }
        } catch (IOException | InvalidPathException ignored) {
            // Path records are best-effort; failure brief generation must not hide the original failure.
        }
        String extension = extension(fileName);
        return new ReportContext.AttachmentRecord(fileName, mediaType(fileName), extension, purpose(fileName), sizeBytes);
    }

    private static List<String> openFirst(List<ReportContext.AttachmentRecord> attachments) {
        if (attachments == null || attachments.isEmpty()) {
            return List.of();
        }
        return attachments.stream()
                .sorted(Comparator.comparingInt(FailureBriefReporter::priority))
                .map(ReportContext.AttachmentRecord::description)
                .filter(description -> !description.isBlank())
                .distinct()
                .limit(MAX_OPEN_FIRST)
                .toList();
    }

    private static int priority(ReportContext.AttachmentRecord record) {
        return switch (record.purpose()) {
            case "screenshot" -> 0;
            case "page-snapshot" -> 1;
            case "api" -> 2;
            case "trace-or-bundle" -> 3;
            case "video" -> 4;
            case "accessibility", "performance" -> 5;
            case "log" -> 6;
            default -> 7;
        };
    }

    private static String classifyFailure(String message, String stacktrace, String logText) {
        String text = (value(message) + "\n" + value(stacktrace) + "\n" + value(logText)).toLowerCase(Locale.ROOT);
        if (text.matches("(?s).*(assertion|assert|verification|expected .* actual|expected .* but found).*")) {
            return "assertion";
        }
        if (text.matches("(?s).*(nosuchelementexception|staleelementreferenceexception|timeoutexception|elementclickinterceptedexception|elementnotinteractableexception|locator|element).*")) {
            return "locator";
        }
        if (text.matches("(?s).*(openapi|swagger|json|xml|schema|status code|response).*")) {
            return "api";
        }
        if (text.matches("(?s).*(accessibility|axe|wcag|violation).*")) {
            return "accessibility";
        }
        if (text.matches("(?s).*(performance budget|p95|latency|duration).*")) {
            return "performance";
        }
        if (text.matches("(?s).*(sessionnotcreatedexception|unreachablebrowserexception|webdriverexception|browserstack|lambdatest|appium|selenium grid|connection refused).*")) {
            return "provider";
        }
        if (text.matches("(?s).*(configuration|property|missing|not found|permission denied|invalid).*")) {
            return "environment";
        }
        if (text.matches("(?s).*(test data|fixture|csv|xlsx|file).*")) {
            return "test-data";
        }
        return "unknown";
    }

    private static SourceContext sourceContext(Throwable throwable) {
        StackTraceElement frame = relevantFrame(throwable);
        return new SourceContext(frame == null ? "" : frame.toString());
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
        return throwable == null || throwable.getStackTrace().length == 0 ? null : throwable.getStackTrace()[0];
    }

    private static String renderManifestJson(List<ReportContext.AttachmentRecord> attachments) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        numberField(json, 1, "schemaVersion", 1, true);
        artifacts(json, 1, attachments, false, null);
        json.append("}\n");
        return json.toString();
    }

    private static void attach(String type, String name, byte[] bytes, String description) {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            output.write(bytes);
        } catch (IOException e) {
            throw new IllegalStateException("Could not buffer failure brief attachment.", e);
        }
        AttachmentReporter.attachBasedOnFileType(type, name, output, description);
    }

    private static String status() {
        Status status = ReportContext.getStatus();
        return Status.BROKEN.equals(status) ? "broken" : "failed";
    }

    private static String mediaType(String path) {
        String lower = value(path).toLowerCase(Locale.ROOT);
        if (lower.endsWith(".png")) return "image/png";
        if (lower.endsWith(".jpg") || lower.endsWith(".jpeg")) return "image/jpeg";
        if (lower.endsWith(".gif")) return "image/gif";
        if (lower.endsWith(".mp4")) return "video/mp4";
        if (lower.endsWith(".zip")) return "application/zip";
        if (lower.endsWith(".json") || lower.endsWith(".har")) return "application/json";
        if (lower.endsWith(".html") || lower.endsWith(".htm")) return "text/html";
        if (lower.endsWith(".mhtml")) return "multipart/related";
        return "text/plain";
    }

    private static String purpose(String path) {
        String lower = value(path).toLowerCase(Locale.ROOT);
        if (lower.contains("screenshot") || lower.endsWith(".png") || lower.endsWith(".jpg")) return "screenshot";
        if (lower.contains("page snapshot") || lower.endsWith(".mhtml")) return "page-snapshot";
        if (lower.contains("request") || lower.contains("response") || lower.contains("api")) return "api";
        if (lower.contains("trace") || lower.contains("diagnostics") || lower.endsWith(".zip")) return "trace-or-bundle";
        if (lower.endsWith(".gif") || lower.endsWith(".mp4")) return "video";
        if (lower.contains("accessibility")) return "accessibility";
        if (lower.contains("performance")) return "performance";
        if (lower.contains("log")) return "log";
        return "general";
    }

    private static String extension(String path) {
        String fileName = value(path);
        int dot = fileName.lastIndexOf('.');
        return dot > -1 ? fileName.substring(dot) : "";
    }

    private static void objectStart(StringBuilder json, int indent, String key) {
        indent(json, indent).append("\"").append(key).append("\": {\n");
    }

    private static void objectEnd(StringBuilder json, int indent, boolean comma) {
        indent(json, indent).append("}").append(comma ? "," : "").append("\n");
    }

    private static void stringField(
            StringBuilder json,
            int indent,
            String key,
            String value,
            boolean comma,
            FailureDiagnosticsReporter.Redactor redactor) {
        String safe = redactor == null ? value(value) : redactor.redact(value);
        indent(json, indent).append("\"").append(key).append("\": \"")
                .append(escapeJson(safe))
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

    private static void stringArray(
            StringBuilder json,
            int indent,
            String key,
            List<String> values,
            boolean comma,
            FailureDiagnosticsReporter.Redactor redactor) {
        indent(json, indent).append("\"").append(key).append("\": [");
        for (int i = 0; i < values.size(); i++) {
            if (i > 0) {
                json.append(", ");
            }
            String safe = redactor == null ? value(values.get(i)) : redactor.redact(values.get(i));
            json.append("\"").append(escapeJson(safe)).append("\"");
        }
        json.append("]").append(comma ? "," : "").append("\n");
    }

    private static void artifacts(StringBuilder json, int indent, List<ReportContext.AttachmentRecord> records,
                                  boolean comma, FailureDiagnosticsReporter.Redactor redactor) {
        indent(json, indent).append("\"artifacts\": [");
        List<ReportContext.AttachmentRecord> safeRecords = records == null ? List.of() : records;
        for (int i = 0; i < safeRecords.size(); i++) {
            ReportContext.AttachmentRecord record = safeRecords.get(i);
            if (i > 0) {
                json.append(", ");
            }
            json.append("{\"description\": \"").append(escapeJson(redact(record.description(), redactor))).append("\", ")
                    .append("\"contentType\": \"").append(escapeJson(redact(record.contentType(), redactor))).append("\", ")
                    .append("\"fileExtension\": \"").append(escapeJson(redact(record.fileExtension(), redactor))).append("\", ")
                    .append("\"purpose\": \"").append(escapeJson(redact(record.purpose(), redactor))).append("\", ")
                    .append("\"sizeBytes\": ").append(record.sizeBytes()).append("}");
        }
        json.append("]").append(comma ? "," : "").append("\n");
    }

    private static String redact(String value, FailureDiagnosticsReporter.Redactor redactor) {
        return redactor == null ? value(value) : redactor.redact(value);
    }

    private static void meta(StringBuilder html, String label, String value) {
        html.append("<div><div class=\"label\">").append(escapeHtml(label)).append("</div><div class=\"value\">")
                .append(escapeHtml(value)).append("</div></div>");
    }

    private static void statusMeta(StringBuilder html, String status) {
        html.append("<div><div class=\"label\">Status</div><div class=\"value\"><span class=\"status-chip ")
                .append(ReportHtmlTheme.statusClass(status))
                .append("\">")
                .append(escapeHtml(status))
                .append("</span></div></div>");
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
                .replace(">", "&gt;")
                .replace("\"", "&quot;");
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    private record SourceContext(String frame) {
    }
}
