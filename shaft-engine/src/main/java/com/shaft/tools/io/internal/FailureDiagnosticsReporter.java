package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.qameta.allure.model.Status;
import org.apache.logging.log4j.Level;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Builds the agent-readable failure diagnostics bundle attached to failed tests.
 */
public final class FailureDiagnosticsReporter {
    private static final Pattern AUTHORIZATION_PATTERN = Pattern.compile(
            "(?i)(authorization|proxy-authorization)(\\s*[:=]\\s*)[^\\r\\n]+");
    private static final Pattern COOKIE_PATTERN = Pattern.compile(
            "(?i)(cookie|set-cookie)(\\s*[:=]\\s*)[^\\r\\n]+");
    private static final Pattern SECRET_ASSIGNMENT_PATTERN = Pattern.compile(
            "(?i)(password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)(\\s*[:=]\\s*)[^\\s,;&\"'<>]+");
    private static final Pattern SECRET_QUERY_PATTERN = Pattern.compile(
            "(?i)([?&](?:password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)=)[^&#\\s]+");
    private static final int SNIPPET_RADIUS = 2;
    // ponytail: bounded log list, make configurable if agent handoff needs deeper local history.
    private static final int MAX_LOG_LINES = 200;

    private FailureDiagnosticsReporter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Attaches {@code shaft-diagnostics.zip} for failed or broken tests.
     *
     * @param info        current test metadata
     * @param logText     current test log
     * @param attachments generated artifact file paths already known to SHAFT
     */
    public static void attachOnFailure(TestExecutionInfo info, String logText, List<String> attachments) {
        if (!shouldAttachDiagnostics(info)) {
            return;
        }
        try {
            attach("zip", "shaft-diagnostics.zip",
                    renderDiagnosticsZip(renderDiagnosticsJson(info, logText, attachments)),
                    "shaft-diagnostics");
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete("Could not attach SHAFT diagnostics bundle: " + e.getMessage(), Level.WARN);
        }
    }

    static String renderDiagnosticsJson(TestExecutionInfo info, String logText, List<String> attachments) {
        Redactor redactor = new Redactor();
        Throwable throwable = info == null ? null : info.throwable();
        SourceContext source = sourceContext(throwable);
        List<String> logLines = logLines(logText, redactor);
        List<ArtifactReference> artifactReferences = artifactReferences(attachments, redactor, info);

        StringBuilder json = new StringBuilder();
        json.append("{\n");
        numberField(json, 1, "schemaVersion", 1, true);
        stringField(json, 1, "generatedAt", Instant.now().toString(), true, redactor);
        objectStart(json, 1, "test");
        stringField(json, 2, "className", value(info == null ? null : info.className()), true, redactor);
        stringField(json, 2, "methodName", value(info == null ? null : info.methodName()), true, redactor);
        stringField(json, 2, "displayName", value(info == null ? null : info.displayName()), true, redactor);
        stringField(json, 2, "description", value(info == null ? null : info.description()), true, redactor);
        numberField(json, 2, "retryAttempt", info != null && info.retried() ? 1 : 0, true);
        booleanField(json, 2, "retried", info != null && info.retried(), true);
        stringField(json, 2, "thread", Thread.currentThread().getName(), false, redactor);
        objectEnd(json, 1, true);
        objectStart(json, 1, "failure");
        stringField(json, 2, "status", status(), true, redactor);
        stringField(json, 2, "type", throwable == null ? "" : throwable.getClass().getName(), true, redactor);
        stringField(json, 2, "message", throwable == null ? "" : throwable.getMessage(), true, redactor);
        stringField(json, 2, "stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(throwable), true, redactor);
        stringField(json, 2, "topProjectFrame", source.frame(), true, redactor);
        stringArray(json, 2, "causes", causes(throwable), false, redactor);
        objectEnd(json, 1, true);
        objectStart(json, 1, "codeContext");
        stringField(json, 2, "file", source.file(), true, redactor);
        stringField(json, 2, "line", source.line(), true, redactor);
        stringField(json, 2, "snippet", source.snippet(), false, redactor);
        objectEnd(json, 1, true);
        objectStart(json, 1, "configuration");
        booleanField(json, 2, "diagnosticsBundleEnabled", SHAFT.Properties.reporting.diagnosticsBundleEnabled(), true);
        numberField(json, 2, "diagnosticsMaxArtifactMb", SHAFT.Properties.reporting.diagnosticsMaxArtifactMb(), true);
        booleanField(json, 2, "traceEnabled", SHAFT.Properties.reporting.traceEnabled(), true);
        stringField(json, 2, "traceMode", SHAFT.Properties.reporting.traceMode(), false, redactor);
        objectEnd(json, 1, true);
        objectStart(json, 1, "environment");
        stringField(json, 2, "javaRuntime", Runtime.version().toString(), true, redactor);
        numberField(json, 2, "availableProcessors", Runtime.getRuntime().availableProcessors(), false);
        objectEnd(json, 1, true);
        objectStart(json, 1, "limits");
        numberField(json, 2, "maxArtifactMb", SHAFT.Properties.reporting.diagnosticsMaxArtifactMb(), true);
        numberField(json, 2, "logLinesIncluded", logLines.size(), true);
        numberField(json, 2, "logLinesOmitted", omittedLogLines(logText, logLines.size()), false);
        objectEnd(json, 1, true);
        stringArray(json, 1, "logs", logLines, true, redactor);
        artifacts(json, 1, artifactReferences, true);
        stringArray(json, 1, "nextCommands", List.of(
                "shaft-doctor analyze --input allure-results --allowed-root . --output-dir target/shaft-doctor",
                "doctor_analyze_failed_allure(allureResultPaths=[\"allure-results\"])"), true, redactor);
        objectStart(json, 1, "redaction");
        stringArray(json, 2, "rulesApplied", redactor.rulesApplied(), false, null);
        objectEnd(json, 1, false);
        json.append("}\n");
        return json.toString();
    }

    static byte[] renderDiagnosticsZip(String diagnosticsJson) {
        int maxBytes = Math.max(1, SHAFT.Properties.reporting.diagnosticsMaxArtifactMb()) * 1024 * 1024;
        try (ByteArrayOutputStream output = new ByteArrayOutputStream();
             ZipOutputStream zip = new ZipOutputStream(output)) {
            addZipEntry(zip, "diagnostics.json", diagnosticsJson.getBytes(StandardCharsets.UTF_8), maxBytes);
            zip.finish();
            return output.toByteArray();
        } catch (IOException e) {
            throw new IllegalStateException("Could not create SHAFT diagnostics zip.", e);
        }
    }

    static boolean shouldAttachDiagnostics(TestExecutionInfo info) {
        if (SHAFT.Properties.reporting == null
                || !SHAFT.Properties.reporting.diagnosticsBundleEnabled()
                || info == null
                || info.throwable() == null) {
            return false;
        }
        Status status = ReportContext.getStatus();
        return status == null || Status.FAILED.equals(status) || Status.BROKEN.equals(status);
    }

    private static void addZipEntry(ZipOutputStream zip, String name, byte[] bytes, int maxBytes) throws IOException {
        zip.putNextEntry(new ZipEntry(name));
        if (bytes.length <= maxBytes) {
            zip.write(bytes);
        } else {
            zip.write(("Omitted because artifact exceeded shaft.diagnostics.maxArtifactMb="
                    + SHAFT.Properties.reporting.diagnosticsMaxArtifactMb()).getBytes(StandardCharsets.UTF_8));
        }
        zip.closeEntry();
    }

    private static void attach(String type, String name, byte[] bytes, String description) {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            output.write(bytes);
        } catch (IOException e) {
            throw new IllegalStateException("Could not buffer diagnostics attachment.", e);
        }
        AttachmentReporter.attachBasedOnFileType(type, name, output, description);
    }

    private static List<String> causes(Throwable throwable) {
        List<String> causes = new ArrayList<>();
        for (Throwable current = throwable == null ? null : throwable.getCause();
             current != null;
             current = current.getCause()) {
            causes.add(current.getClass().getName() + ": " + value(current.getMessage()));
        }
        return causes;
    }

    private static List<String> logLines(String logText, Redactor redactor) {
        if (logText == null || logText.isBlank()) {
            return List.of();
        }
        List<String> lines = new ArrayList<>();
        for (String line : logText.split("\\R")) {
            if (!line.isBlank()) {
                lines.add(redactor.redact(line));
                if (lines.size() == MAX_LOG_LINES) {
                    break;
                }
            }
        }
        return List.copyOf(lines);
    }

    private static int omittedLogLines(String logText, int included) {
        if (logText == null || logText.isBlank()) {
            return 0;
        }
        long total = logText.lines().filter(line -> !line.isBlank()).count();
        return Math.toIntExact(Math.max(0, total - included));
    }

    private static List<ArtifactReference> artifactReferences(
            List<String> attachments,
            Redactor redactor,
            TestExecutionInfo info) {
        List<ArtifactReference> references = new ArrayList<>();
        if (attachments != null) {
            for (String attachment : attachments) {
                if (attachment != null && !attachment.isBlank()) {
                    references.add(new ArtifactReference("artifact-" + (references.size() + 1),
                            mediaType(attachment), redactor.redact(attachment)));
                }
            }
        }
        if (FailureTraceReporter.shouldAttachTrace(info)) {
            references.add(new ArtifactReference("trace-zip", "application/zip", "shaft-trace.zip"));
        }
        Path playwrightTrace = PlaywrightTraceManager.getLastTracePath();
        if (playwrightTrace != null) {
            references.add(new ArtifactReference("playwright-trace", "application/zip",
                    redactor.redact(playwrightTrace.toString())));
        }
        return List.copyOf(references);
    }

    private static String mediaType(String path) {
        String lower = path.toLowerCase();
        if (lower.endsWith(".png")) {
            return "image/png";
        }
        if (lower.endsWith(".gif")) {
            return "image/gif";
        }
        if (lower.endsWith(".mp4")) {
            return "video/mp4";
        }
        if (lower.endsWith(".zip")) {
            return "application/zip";
        }
        if (lower.endsWith(".json")) {
            return "application/json";
        }
        if (lower.endsWith(".html") || lower.endsWith(".htm")) {
            return "text/html";
        }
        return "text/plain";
    }

    private static SourceContext sourceContext(Throwable throwable) {
        StackTraceElement frame = relevantFrame(throwable);
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
        return throwable == null || throwable.getStackTrace().length == 0 ? null : throwable.getStackTrace()[0];
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
            return snippet.toString().trim();
        } catch (IOException e) {
            return sourceFile + ":" + lineNumber;
        }
    }

    private static String status() {
        Status status = ReportContext.getStatus();
        if (Status.BROKEN.equals(status)) {
            return "broken";
        }
        return "failed";
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
            Redactor redactor) {
        String safe = redactor == null ? value(value) : redactor.redact(value);
        indent(json, indent).append("\"").append(key).append("\": \"")
                .append(escapeJson(safe))
                .append("\"")
                .append(comma ? "," : "")
                .append("\n");
    }

    private static void numberField(StringBuilder json, int indent, String key, int value, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": ")
                .append(value)
                .append(comma ? "," : "")
                .append("\n");
    }

    private static void booleanField(StringBuilder json, int indent, String key, boolean value, boolean comma) {
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
            Redactor redactor) {
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

    private static void artifacts(
            StringBuilder json,
            int indent,
            List<ArtifactReference> references,
            boolean comma) {
        indent(json, indent).append("\"artifacts\": [");
        for (int i = 0; i < references.size(); i++) {
            ArtifactReference reference = references.get(i);
            if (i > 0) {
                json.append(", ");
            }
            json.append("{\"id\": \"").append(escapeJson(reference.id())).append("\", ")
                    .append("\"type\": \"").append(escapeJson(reference.type())).append("\", ")
                    .append("\"path\": \"").append(escapeJson(reference.path())).append("\"}");
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

    private record ArtifactReference(String id, String type, String path) {
    }

    static final class Redactor {
        private final Set<String> rulesApplied = new LinkedHashSet<>();

        String redact(String value) {
            String redacted = value(value);
            redacted = replace(redacted, AUTHORIZATION_PATTERN, "$1$2********", "authorization-header");
            redacted = replace(redacted, COOKIE_PATTERN, "$1$2********", "cookie-header");
            redacted = replace(redacted, SECRET_ASSIGNMENT_PATTERN, "$1$2********", "secret-assignment");
            return replace(redacted, SECRET_QUERY_PATTERN, "$1********", "secret-url-query");
        }

        private String replace(String input, Pattern pattern, String replacement, String rule) {
            String output = pattern.matcher(input).replaceAll(replacement);
            if (!output.equals(input)) {
                rulesApplied.add(rule);
            }
            return output;
        }

        List<String> rulesApplied() {
            return rulesApplied.stream().sorted().toList();
        }
    }
}
