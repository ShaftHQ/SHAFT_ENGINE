package com.shaft.mcp;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.capture.generate.CaptureGenerator.CodegenBackend;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.RedactionSummary;
import com.shaft.doctor.model.Remediation;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * MCP tools for persisted SHAFT trace discovery, reading, and deterministic failure analysis.
 */
@Service
public class TraceService {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final int DEFAULT_LATEST_LIMIT = 5;
    private static final int MAX_LATEST_LIMIT = 50;
    private static final int DEFAULT_MAX_CHARACTERS = 20_000;
    private static final int MAX_CHARACTERS = 100_000;
    private static final int MAX_FINDINGS = 10;
    private static final Pattern AUTHORIZATION_PATTERN = Pattern.compile("(?i)(authorization\\s*[:=]\\s*)(bearer\\s+)?[^\\s,;]+");
    private static final Pattern COOKIE_PATTERN = Pattern.compile("(?i)(cookie|set-cookie)(\\s*[:=]\\s*)[^\\n\\r]+");
    private static final Pattern URL_CREDENTIAL_PATTERN = Pattern.compile("(?i)(://[^:/\\s]+:)[^@/\\s]+(@)");
    private static final Pattern SECRET_JSON_PATTERN = Pattern.compile(
            "(?i)(\"(?:password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)\"\\s*:\\s*\")[^\"]*(\")");
    private static final Pattern SECRET_ASSIGNMENT_PATTERN = Pattern.compile(
            "(?i)(password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)(\\s*[:=]\\s*)[^\\s,;&\"'<>]+");

    private final McpWorkspacePolicy workspacePolicy;
    private final McpDoctorRemediationService remediationService;

    /**
     * Creates the default local MCP trace service.
     */
    public TraceService() {
        this(McpWorkspacePolicy.current(), new McpDoctorRemediationService());
    }

    TraceService(McpWorkspacePolicy workspacePolicy, McpDoctorRemediationService remediationService) {
        this.workspacePolicy = workspacePolicy;
        this.remediationService = remediationService;
    }

    /**
     * Lists recent persisted SHAFT trace indexes under {@code target/shaft-traces}.
     *
     * @param maxResults maximum trace indexes to return
     * @return recent trace index metadata
     */
    @Tool(name = "trace_latest",
            description = "returns recent persisted SHAFT trace indexes from target/shaft-traces")
    public McpTraceLatestResult traceLatest(int maxResults) {
        Path traceRoot = workspacePolicy.output("target/shaft-traces", "SHAFT trace directory");
        if (!Files.isDirectory(traceRoot)) {
            return new McpTraceLatestResult("1.0", List.of(),
                    List.of("No SHAFT traces were found under target/shaft-traces."));
        }
        List<String> warnings = new ArrayList<>();
        List<TraceIndex> indexes = new ArrayList<>();
        try (Stream<Path> paths = Files.walk(traceRoot, 3)) {
            paths.filter(path -> path.getFileName().toString().equals("index.json"))
                    .forEach(path -> readIndex(path, warnings).ifPresent(indexes::add));
        } catch (IOException exception) {
            warnings.add("Could not scan target/shaft-traces: " + exception.getMessage());
        }
        int limit = maxResults <= 0 ? DEFAULT_LATEST_LIMIT : Math.min(maxResults, MAX_LATEST_LIMIT);
        List<McpTraceIndexRecord> traces = indexes.stream()
                .sorted((left, right) -> right.sortKey().compareTo(left.sortKey()))
                .limit(limit)
                .map(TraceIndex::record)
                .toList();
        if (traces.isEmpty() && warnings.isEmpty()) {
            warnings.add("No SHAFT trace indexes were found under target/shaft-traces.");
        }
        return new McpTraceLatestResult("1.0", traces, warnings);
    }

    /**
     * Reads a persisted trace JSON file, trace ZIP, trace directory, or index path with bounded output.
     *
     * @param tracePath trace path inside the MCP workspace
     * @param maxCharacters maximum characters to return
     * @return bounded redacted trace JSON content
     */
    @Tool(name = "trace_read",
            description = "returns redacted SHAFT trace JSON from a trace path with explicit output bounds")
    public McpTraceReadResult traceRead(String tracePath, int maxCharacters) {
        TraceDocument trace = readTrace(tracePath);
        String content = redact(trace.content());
        int limit = characterLimit(maxCharacters);
        boolean truncated = content.length() > limit;
        return new McpTraceReadResult(
                "1.0",
                relative(trace.path()),
                truncated ? content.substring(0, limit) : content,
                limit,
                truncated,
                truncated ? List.of("Trace output was truncated to maxCharacters=" + limit + ".") : List.of());
    }

    /**
     * Summarizes a persisted SHAFT trace deterministically without provider calls.
     *
     * @param tracePath trace path inside the MCP workspace
     * @return deterministic failure summary
     */
    @Tool(name = "trace_summarize",
            description = "returns a deterministic summary of a persisted SHAFT trace without AI")
    public McpTraceSummary traceSummarize(String tracePath) {
        return summarize(readTrace(tracePath));
    }

    /**
     * Resolves (extracting from the trace ZIP if needed) the static offline "SHAFT Trace Report.html"
     * viewer for a persisted trace -- the same time-travel timeline / action-list / DOM-snapshot /
     * network / console viewer produced at trace-generation time, so callers (an IDE panel, a CI
     * step, or a human) can open a real file path without re-deriving the HTML themselves.
     *
     * @param tracePath trace path inside the MCP workspace (directory, index.json, or ZIP)
     * @return the resolved viewer HTML path and whether it had to be freshly extracted
     */
    @Tool(name = "trace_open_viewer",
            description = "resolves (extracting from the trace ZIP if needed) the offline SHAFT Trace Report HTML viewer for a persisted trace")
    public McpTraceViewerResult traceOpenViewer(String tracePath) {
        Path path = workspacePolicy.existing(tracePath, "Trace path");
        Path directory = Files.isDirectory(path) ? path : path.getParent();
        Path htmlPath = directory.resolve("SHAFT Trace Report.html");
        if (Files.isRegularFile(htmlPath)) {
            return new McpTraceViewerResult("1.0", relative(htmlPath), false, List.of());
        }
        Path archive = resolveArchiveForViewer(path, directory);
        if (archive == null) {
            return new McpTraceViewerResult("1.0", "", false,
                    List.of("No SHAFT Trace Report.html or trace ZIP was found for " + relative(path) + "."));
        }
        try (InputStream input = Files.newInputStream(archive); ZipInputStream zip = new ZipInputStream(input)) {
            ZipEntry entry;
            while ((entry = zip.getNextEntry()) != null) {
                if ("SHAFT Trace Report.html".equals(entry.getName())) {
                    Files.write(htmlPath, zip.readAllBytes());
                    return new McpTraceViewerResult("1.0", relative(htmlPath), true, List.of());
                }
            }
        } catch (IOException exception) {
            return new McpTraceViewerResult("1.0", "", false,
                    List.of("Trace ZIP could not be read: " + exception.getMessage()));
        }
        return new McpTraceViewerResult("1.0", "", false,
                List.of("Trace ZIP " + relative(archive) + " does not contain SHAFT Trace Report.html."));
    }

    private Path resolveArchiveForViewer(Path path, Path directory) {
        if (!Files.isDirectory(path) && path.getFileName().toString().endsWith(".zip")) {
            return path;
        }
        String fileName = path.getFileName() == null ? "" : path.getFileName().toString();
        if ("index.json".equals(fileName)) {
            try {
                JsonNode index = JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
                Path fromIndex = archivePath(path, index);
                if (fromIndex != null && Files.isRegularFile(fromIndex)) {
                    return fromIndex;
                }
            } catch (IOException | RuntimeException ignored) {
                // Fall through to the directory-default archive below.
            }
        }
        Path fallback = directory.resolve("shaft-trace.zip");
        return Files.isRegularFile(fallback) ? fallback : null;
    }

    /**
     * Analyzes a persisted SHAFT trace and returns the existing MCP Doctor remediation report shape.
     *
     * @param tracePath trace path inside the MCP workspace
     * @param backend optional backend label, {@code playwright} or {@code webdriver}
     * @return deterministic MCP Doctor analysis and remediation snippets
     */
    @Tool(name = "doctor_analyze_trace",
            description = "analyzes a persisted SHAFT trace and returns deterministic Doctor remediation guidance")
    public McpAnalysisReport doctorAnalyzeTrace(String tracePath, String backend) {
        TraceDocument trace = readTrace(tracePath);
        McpTraceSummary summary = summarize(trace);
        CauseCategory cause = cause(summary);
        List<String> evidenceIds = List.of("trace-failed-action", "trace-exception", "trace-source");
        Diagnosis diagnosis = new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                cause,
                List.of(),
                summary.failedAction().name().isBlank() ? Confidence.MEDIUM : Confidence.HIGH,
                diagnosisSummary(summary),
                "Trace analysis uses the failed structured action, exception, source context, and optional trace evidence.",
                List.of(new Finding(
                        "trace-failure",
                        Finding.Kind.OBSERVATION,
                        cause,
                        Finding.Severity.ERROR,
                        "SHAFT trace captured the failed action",
                        findingDetail(summary),
                        "mcp-trace-failure",
                        evidenceIds)),
                List.of(new Remediation(
                        "trace-next-step",
                        remediationTitle(cause),
                        remediationAction(summary, cause),
                        List.of("trace-failure"),
                        evidenceIds)),
                missingEvidence(summary));
        DoctorAnalysisResult result = new DoctorAnalysisResult(
                evidenceBundle(trace, summary, evidenceIds),
                diagnosis,
                relative(trace.path()),
                relative(trace.path()),
                "");
        return remediationService.build(
                result,
                null,
                List.of(),
                false,
                ApprovalPolicy.denyAll(),
                "driver",
                backend(backend));
    }

    private java.util.Optional<TraceIndex> readIndex(Path indexPath, List<String> warnings) {
        try {
            JsonNode index = JSON.readTree(Files.readString(indexPath, StandardCharsets.UTF_8));
            Path archive = archivePath(indexPath, index);
            Map<String, String> entries = entries(index.path("entries"));
            Instant sortKey = generatedAt(textOr(index.path("generatedAt"), ""), indexPath);
            return java.util.Optional.of(new TraceIndex(new McpTraceIndexRecord(
                    text(index.path("testId")),
                    text(index.path("generatedAt")),
                    relative(indexPath),
                    relative(indexPath),
                    archive == null ? "" : relative(archive),
                    entries), sortKey));
        } catch (RuntimeException | IOException exception) {
            warnings.add("Could not read trace index " + relative(indexPath) + ": " + exception.getMessage());
            return java.util.Optional.empty();
        }
    }

    private TraceDocument readTrace(String tracePath) {
        Path path = workspacePolicy.existing(tracePath, "Trace path");
        try {
            if (Files.isDirectory(path)) {
                return traceFromDirectory(path);
            }
            String fileName = path.getFileName().toString();
            if ("index.json".equals(fileName)) {
                return traceFromIndex(path);
            }
            if (fileName.endsWith(".zip")) {
                return traceFromZip(path);
            }
            String content = Files.readString(path, StandardCharsets.UTF_8);
            return new TraceDocument(path, content, JSON.readTree(content));
        } catch (IOException exception) {
            throw new IllegalArgumentException("Trace path could not be read.", exception);
        }
    }

    private TraceDocument traceFromDirectory(Path directory) throws IOException {
        Path index = directory.resolve("index.json");
        if (Files.isRegularFile(index)) {
            return traceFromIndex(index);
        }
        Path json = directory.resolve("shaft-trace.json");
        if (Files.isRegularFile(json)) {
            return readTrace(relative(json));
        }
        Path zip = directory.resolve("shaft-trace.zip");
        if (Files.isRegularFile(zip)) {
            return traceFromZip(workspacePolicy.existing(relative(zip), "Trace archive"));
        }
        throw new IllegalArgumentException("Trace directory does not contain index.json, shaft-trace.json, or shaft-trace.zip.");
    }

    private TraceDocument traceFromIndex(Path indexPath) throws IOException {
        JsonNode index = JSON.readTree(Files.readString(indexPath, StandardCharsets.UTF_8));
        Path archive = archivePath(indexPath, index);
        if (archive != null && Files.isRegularFile(archive)) {
            return traceFromZip(archive, indexPath);
        }
        String jsonName = textOr(index.path("entries").path("json"), "shaft-trace.json");
        Path json = indexPath.getParent().resolve(jsonName).normalize();
        if (json.startsWith(workspacePolicy.root()) && Files.isRegularFile(json)) {
            return readTrace(relative(json));
        }
        throw new IllegalArgumentException("Trace index does not point to a readable shaft-trace.json.");
    }

    private TraceDocument traceFromZip(Path archive) throws IOException {
        return traceFromZip(archive, archive);
    }

    private TraceDocument traceFromZip(Path archive, Path publicPath) throws IOException {
        try (InputStream input = Files.newInputStream(archive);
             ZipInputStream zip = new ZipInputStream(input)) {
            ZipEntry entry;
            while ((entry = zip.getNextEntry()) != null) {
                if ("shaft-trace.json".equals(entry.getName())) {
                    String content = new String(zip.readAllBytes(), StandardCharsets.UTF_8);
                    return new TraceDocument(publicPath, content, JSON.readTree(content));
                }
            }
        }
        throw new IllegalArgumentException("Trace archive does not contain shaft-trace.json.");
    }

    private Path archivePath(Path indexPath, JsonNode index) {
        String archive = textOr(index.path("archive"), "");
        if (archive.isBlank()) {
            Path fallback = indexPath.getParent().resolve("shaft-trace.zip").normalize();
            return fallback.startsWith(workspacePolicy.root()) && Files.exists(fallback)
                    ? workspacePolicy.existing(relative(fallback), "Trace archive")
                    : null;
        }
        return workspacePolicy.existing(archive, "Trace archive");
    }

    private McpTraceSummary summarize(TraceDocument trace) {
        JsonNode root = trace.root();
        JsonNode test = root.path("test");
        JsonNode source = root.path("source");
        JsonNode exception = root.path("exception");
        JsonNode actionNode = failedAction(root.path("actions"));
        McpTraceActionSummary action = action(actionNode);
        String exceptionType = action.exceptionType().isBlank() ? text(exception.path("type")) : action.exceptionType();
        String exceptionMessage = action.exceptionMessage().isBlank()
                ? text(exception.path("message"))
                : action.exceptionMessage();
        ObjectNode actionability = JSON.createObjectNode();
        if (actionNode.path("actionability").isObject()) {
            actionability.setAll((ObjectNode) redactedJson(actionNode.path("actionability")));
        }
        ObjectNode locatorHealth = JSON.createObjectNode();
        if (root.path("locatorHealth").isObject()) {
            locatorHealth.setAll((ObjectNode) redactedJson(root.path("locatorHealth")));
        }
        return new McpTraceSummary(
                "1.0",
                relative(trace.path()),
                text(test.path("status")),
                text(test.path("className")),
                text(test.path("methodName")),
                text(test.path("displayName")),
                action,
                exceptionType,
                exceptionMessage,
                text(source.path("file")),
                text(source.path("line")),
                text(source.path("snippet")),
                actionability,
                locatorHealth,
                networkFindings(root.path("network")),
                consoleFindings(root.path("console")),
                recommendations(action, exceptionType, exceptionMessage),
                List.of());
    }

    private JsonNode failedAction(JsonNode actions) {
        if (!actions.isArray()) {
            return JSON.createObjectNode();
        }
        for (JsonNode action : actions) {
            if (!"passed".equalsIgnoreCase(textOr(action.path("status"), ""))) {
                return action;
            }
        }
        return actions.isEmpty() ? JSON.createObjectNode() : actions.get(0);
    }

    private McpTraceActionSummary action(JsonNode action) {
        JsonNode exception = action.path("exception");
        return new McpTraceActionSummary(
                text(action.path("id")),
                text(action.path("category")),
                text(action.path("name")),
                text(action.path("status")),
                text(action.path("locator")),
                text(action.path("url")),
                text(action.path("message")),
                text(exception.path("type")),
                text(exception.path("message")),
                action.path("durationMs").asLong(0L));
    }

    private List<String> networkFindings(JsonNode network) {
        if (!network.isArray()) {
            return List.of();
        }
        List<String> findings = new ArrayList<>();
        for (JsonNode entry : network) {
            int status = entry.path("status").asInt(-1);
            String failureReason = text(entry.path("failureReason"));
            if (status >= 400 || status <= 0 || !failureReason.isBlank()) {
                String method = text(entry.path("method"));
                String url = text(entry.path("url"));
                String suffix = failureReason.isBlank() ? "" : " " + failureReason;
                findings.add((method + " " + status + " " + url + suffix).trim());
            }
            if (findings.size() >= MAX_FINDINGS) {
                break;
            }
        }
        return List.copyOf(findings);
    }

    private List<String> consoleFindings(JsonNode console) {
        if (!console.isArray()) {
            return List.of();
        }
        List<String> findings = new ArrayList<>();
        for (JsonNode entry : console) {
            String level = text(entry.path("level"));
            String message = text(entry.path("message"));
            if (!level.isBlank() || !message.isBlank()) {
                findings.add((level + " " + message).trim());
            }
            if (findings.size() >= MAX_FINDINGS) {
                break;
            }
        }
        return List.copyOf(findings);
    }

    private List<String> recommendations(McpTraceActionSummary action, String exceptionType, String exceptionMessage) {
        LinkedHashSet<String> recommendations = new LinkedHashSet<>();
        recommendations.add("Use trace_read on the returned tracePath for full bounded evidence.");
        recommendations.add("Use doctor_analyze_trace for deterministic remediation guidance.");
        if (!action.locator().isBlank()) {
            recommendations.add("Inspect the current DOM before changing locator: browser_get_page_dom or playwright_browser_get_page_dom.");
        }
        String combined = (exceptionType + " " + exceptionMessage).toLowerCase(Locale.ROOT);
        if (combined.contains("timeout") || combined.contains("clickable") || combined.contains("interactable")) {
            recommendations.add("Check page readiness and add an evidence-backed SHAFT wait or assertion before the action.");
        }
        return List.copyOf(recommendations);
    }

    private CauseCategory cause(McpTraceSummary summary) {
        String combined = (summary.failedAction().locator() + " " + summary.exceptionType() + " "
                + summary.exceptionMessage() + " " + summary.failedAction().message()).toLowerCase(Locale.ROOT);
        if (combined.contains("nosuchelement") || combined.contains("no such element")
                || combined.contains("staleelement") || combined.contains("locator")
                || combined.contains("by.cssselector") || combined.contains("by.id")
                || combined.contains("by.xpath")) {
            return CauseCategory.LOCATOR;
        }
        if (combined.contains("timeout") || combined.contains("clickable") || combined.contains("interactable")
                || combined.contains("displayed") || combined.contains("enabled")) {
            return CauseCategory.TIMING_SYNCHRONIZATION;
        }
        if (combined.contains("assert")) {
            return CauseCategory.TEST;
        }
        return CauseCategory.UNKNOWN;
    }

    private EvidenceBundle evidenceBundle(TraceDocument trace, McpTraceSummary summary, List<String> evidenceIds) {
        List<EvidenceItem> evidence = List.of(
                evidence(evidenceIds.get(0), EvidenceCategory.OTHER, trace.path(), failedActionEvidence(summary),
                        Map.of("failureMessage", summary.failedAction().message() + " " + summary.exceptionMessage(),
                                "traceTop", summary.failedAction().locator())),
                evidence(evidenceIds.get(1), EvidenceCategory.EXCEPTION_CHAIN, trace.path(),
                        summary.exceptionType() + ": " + summary.exceptionMessage(), Map.of()),
                evidence(evidenceIds.get(2), EvidenceCategory.OTHER, trace.path(),
                        summary.sourceFile() + ":" + summary.sourceLine() + System.lineSeparator()
                                + summary.sourceSnippet(), Map.of()));
        return new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "trace-" + sha256((relative(trace.path()) + summary.failedAction().id()).getBytes(StandardCharsets.UTF_8)).substring(0, 16),
                evidence,
                new RedactionSummary(List.of("mcp-trace-output-redaction"), List.of(), 0),
                Map.of("tracePath", relative(trace.path())));
    }

    private EvidenceItem evidence(
            String id,
            EvidenceCategory category,
            Path tracePath,
            String content,
            Map<String, String> attributes) {
        String safeContent = clip(redact(content), 8_000);
        String digest = sha256(safeContent.getBytes(StandardCharsets.UTF_8));
        return new EvidenceItem(
                id,
                category,
                "text/plain",
                relative(tracePath),
                digest,
                safeContent.getBytes(StandardCharsets.UTF_8).length,
                safeContent,
                true,
                content.length() > safeContent.length(),
                attributes,
                new EvidenceProvenance("mcp-trace", relative(tracePath), digest));
    }

    private String failedActionEvidence(McpTraceSummary summary) {
        try {
            ObjectNode node = JSON.createObjectNode();
            node.put("action", summary.failedAction().name());
            node.put("status", summary.failedAction().status());
            node.put("locator", summary.failedAction().locator());
            node.put("url", summary.failedAction().url());
            node.put("message", summary.failedAction().message());
            node.put("exceptionType", summary.exceptionType());
            node.put("exceptionMessage", summary.exceptionMessage());
            node.set("actionability", summary.actionability());
            node.set("locatorHealth", summary.locatorHealth());
            node.set("networkFindings", JSON.valueToTree(summary.networkFindings()));
            node.set("consoleFindings", JSON.valueToTree(summary.consoleFindings()));
            return JSON.writeValueAsString(node);
        } catch (RuntimeException exception) {
            return findingDetail(summary);
        }
    }

    private String diagnosisSummary(McpTraceSummary summary) {
        String action = summary.failedAction().name().isBlank() ? "trace action" : summary.failedAction().name();
        String locator = summary.failedAction().locator().isBlank() ? "without locator" : "on " + summary.failedAction().locator();
        String exception = summary.exceptionType().isBlank() ? summary.exceptionMessage() : summary.exceptionType();
        return (action + " failed " + locator + " in " + summary.testClass() + "." + summary.testMethod()
                + (exception.isBlank() ? "." : " with " + exception + ".")).trim();
    }

    private String findingDetail(McpTraceSummary summary) {
        return "Failed action=" + summary.failedAction().name()
                + ", locator=" + summary.failedAction().locator()
                + ", exception=" + summary.exceptionType()
                + ", source=" + summary.sourceFile() + ":" + summary.sourceLine();
    }

    private String remediationTitle(CauseCategory cause) {
        return switch (cause) {
            case LOCATOR -> "Inspect and repair the failing locator";
            case TIMING_SYNCHRONIZATION -> "Add an evidence-backed wait or state assertion";
            default -> "Inspect trace evidence before changing test code";
        };
    }

    private String remediationAction(McpTraceSummary summary, CauseCategory cause) {
        String base = "Start from trace_read, then inspect the failed action and source line "
                + summary.sourceFile() + ":" + summary.sourceLine() + ". ";
        return switch (cause) {
            case LOCATOR -> base + "Use DOM/screenshot evidence before replacing locator "
                    + summary.failedAction().locator() + ".";
            case TIMING_SYNCHRONIZATION -> base + "Add the smallest SHAFT wait or assertion before "
                    + summary.failedAction().name() + ".";
            default -> base + "Classify whether this is product, test data, environment, or locator behavior.";
        };
    }

    private List<String> missingEvidence(McpTraceSummary summary) {
        List<String> missing = new ArrayList<>();
        if (summary.failedAction().locator().isBlank()) {
            missing.add("No failed action locator was present in the trace.");
        }
        if (summary.sourceFile().isBlank()) {
            missing.add("No source context was present in the trace.");
        }
        return List.copyOf(missing);
    }

    private CodegenBackend backend(String value) {
        String backend = value == null ? "" : value.trim();
        return "playwright".equalsIgnoreCase(backend) ? CodegenBackend.PLAYWRIGHT : CodegenBackend.WEBDRIVER;
    }

    private int characterLimit(int maxCharacters) {
        if (maxCharacters <= 0) {
            return DEFAULT_MAX_CHARACTERS;
        }
        return Math.min(maxCharacters, MAX_CHARACTERS);
    }

    private Map<String, String> entries(JsonNode entries) {
        if (!entries.isObject()) {
            return Map.of();
        }
        Map<String, String> result = new LinkedHashMap<>();
        entries.properties().forEach(entry -> result.put(entry.getKey(), text(entry.getValue())));
        return Map.copyOf(result);
    }

    private Instant generatedAt(String value, Path path) {
        try {
            return Instant.parse(value);
        } catch (RuntimeException ignored) {
            try {
                FileTime modified = Files.getLastModifiedTime(path);
                return modified.toInstant();
            } catch (IOException exception) {
                return Instant.EPOCH;
            }
        }
    }

    private String relative(Path path) {
        Path absolute = path.toAbsolutePath().normalize();
        if (absolute.startsWith(workspacePolicy.root())) {
            return workspacePolicy.root().relativize(absolute).toString().replace('\\', '/');
        }
        return path.getFileName().toString();
    }

    private static String text(JsonNode node) {
        return node == null || node.isMissingNode() || node.isNull() ? "" : clip(redact(node.asText()), 2_000);
    }

    private static String textOr(JsonNode node, String fallback) {
        if (node == null || node.isMissingNode() || node.isNull()) {
            return fallback;
        }
        String value = node.asText();
        return value.isBlank() ? fallback : value;
    }

    private static String redact(String value) {
        String redacted = value == null ? "" : value;
        redacted = AUTHORIZATION_PATTERN.matcher(redacted).replaceAll("$1********");
        redacted = COOKIE_PATTERN.matcher(redacted).replaceAll("$1$2********");
        redacted = URL_CREDENTIAL_PATTERN.matcher(redacted).replaceAll("$1********$2");
        redacted = SECRET_JSON_PATTERN.matcher(redacted).replaceAll("$1********$2");
        return SECRET_ASSIGNMENT_PATTERN.matcher(redacted).replaceAll("$1$2********");
    }

    private static JsonNode redactedJson(JsonNode node) {
        try {
            return JSON.readTree(redact(JSON.writeValueAsString(node)));
        } catch (RuntimeException exception) {
            return JSON.createObjectNode();
        }
    }

    private static String clip(String value, int maxLength) {
        String safe = value == null ? "" : value;
        return safe.length() <= maxLength ? safe : safe.substring(0, maxLength);
    }

    private static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    private record TraceDocument(Path path, String content, JsonNode root) {
    }

    private record TraceIndex(McpTraceIndexRecord record, Instant sortKey) {
    }

    /**
     * Recent trace index listing result.
     *
     * @param schemaVersion result schema version
     * @param traces trace indexes in newest-first order
     * @param warnings safe warnings
     */
    public record McpTraceLatestResult(String schemaVersion, List<McpTraceIndexRecord> traces, List<String> warnings) {
        /**
         * Creates an immutable latest-trace result.
         */
        public McpTraceLatestResult {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
            traces = traces == null ? List.of() : List.copyOf(traces);
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

    /**
     * One persisted SHAFT trace index.
     *
     * @param testId safe test identifier
     * @param generatedAt trace generation timestamp
     * @param tracePath path to pass to {@code trace_read}, {@code trace_summarize}, or {@code doctor_analyze_trace}
     * @param indexPath persisted index path
     * @param archivePath persisted ZIP path
     * @param entries artifact entries recorded by the index
     */
    public record McpTraceIndexRecord(
            String testId,
            String generatedAt,
            String tracePath,
            String indexPath,
            String archivePath,
            Map<String, String> entries) {
        /**
         * Creates an immutable trace index record.
         */
        public McpTraceIndexRecord {
            testId = testId == null ? "" : testId.trim();
            generatedAt = generatedAt == null ? "" : generatedAt.trim();
            tracePath = tracePath == null ? "" : tracePath.trim();
            indexPath = indexPath == null ? "" : indexPath.trim();
            archivePath = archivePath == null ? "" : archivePath.trim();
            entries = entries == null ? Map.of() : Map.copyOf(entries);
        }
    }

    /**
     * Resolved offline SHAFT Trace Report HTML viewer path.
     *
     * @param schemaVersion result schema version
     * @param viewerPath resolved viewer HTML path, or blank when it could not be resolved
     * @param extracted whether the HTML was freshly extracted from the trace ZIP this call
     * @param warnings safe warnings, non-empty only when {@code viewerPath} is blank
     */
    public record McpTraceViewerResult(
            String schemaVersion,
            String viewerPath,
            boolean extracted,
            List<String> warnings) {
        /**
         * Creates an immutable trace viewer result.
         */
        public McpTraceViewerResult {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
            viewerPath = viewerPath == null ? "" : viewerPath.trim();
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

    /**
     * Bounded trace JSON read result.
     *
     * @param schemaVersion result schema version
     * @param tracePath resolved trace path
     * @param content redacted bounded JSON text
     * @param maxCharacters applied character limit
     * @param truncated whether content was truncated
     * @param warnings safe warnings
     */
    public record McpTraceReadResult(
            String schemaVersion,
            String tracePath,
            String content,
            int maxCharacters,
            boolean truncated,
            List<String> warnings) {
        /**
         * Creates an immutable trace read result.
         */
        public McpTraceReadResult {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
            tracePath = tracePath == null ? "" : tracePath.trim();
            content = content == null ? "" : content;
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

    /**
     * Deterministic SHAFT trace summary.
     *
     * @param schemaVersion result schema version
     * @param tracePath resolved trace path
     * @param status test status
     * @param testClass failing test class
     * @param testMethod failing test method
     * @param displayName display name
     * @param failedAction failed structured action
     * @param exceptionType exception type
     * @param exceptionMessage exception message
     * @param sourceFile relevant source file
     * @param sourceLine relevant source line
     * @param sourceSnippet bounded source snippet
     * @param actionability optional actionability diagnostics
     * @param locatorHealth optional locator health summary
     * @param networkFindings optional network findings
     * @param consoleFindings optional console findings
     * @param recommendations deterministic next MCP actions
     * @param warnings safe warnings
     */
    public record McpTraceSummary(
            String schemaVersion,
            String tracePath,
            String status,
            String testClass,
            String testMethod,
            String displayName,
            McpTraceActionSummary failedAction,
            String exceptionType,
            String exceptionMessage,
            String sourceFile,
            String sourceLine,
            String sourceSnippet,
            JsonNode actionability,
            JsonNode locatorHealth,
            List<String> networkFindings,
            List<String> consoleFindings,
            List<String> recommendations,
            List<String> warnings) {
        /**
         * Creates an immutable trace summary.
         */
        public McpTraceSummary {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
            tracePath = tracePath == null ? "" : tracePath.trim();
            status = status == null ? "" : status.trim();
            testClass = testClass == null ? "" : testClass.trim();
            testMethod = testMethod == null ? "" : testMethod.trim();
            displayName = displayName == null ? "" : displayName.trim();
            failedAction = failedAction == null ? McpTraceActionSummary.empty() : failedAction;
            exceptionType = exceptionType == null ? "" : exceptionType.trim();
            exceptionMessage = exceptionMessage == null ? "" : exceptionMessage.trim();
            sourceFile = sourceFile == null ? "" : sourceFile.trim();
            sourceLine = sourceLine == null ? "" : sourceLine.trim();
            sourceSnippet = sourceSnippet == null ? "" : sourceSnippet;
            actionability = actionability == null ? JSON.createObjectNode() : actionability;
            locatorHealth = locatorHealth == null ? JSON.createObjectNode() : locatorHealth;
            networkFindings = networkFindings == null ? List.of() : List.copyOf(networkFindings);
            consoleFindings = consoleFindings == null ? List.of() : List.copyOf(consoleFindings);
            recommendations = recommendations == null ? List.of() : List.copyOf(recommendations);
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

    /**
     * Failed action summary from a SHAFT trace.
     *
     * @param id action identifier
     * @param category action category
     * @param name action name
     * @param status action status
     * @param locator action locator
     * @param url current URL
     * @param message action message
     * @param exceptionType action exception type
     * @param exceptionMessage action exception message
     * @param durationMs action duration in milliseconds
     */
    public record McpTraceActionSummary(
            String id,
            String category,
            String name,
            String status,
            String locator,
            String url,
            String message,
            String exceptionType,
            String exceptionMessage,
            long durationMs) {
        /**
         * Creates an immutable action summary.
         */
        public McpTraceActionSummary {
            id = id == null ? "" : id.trim();
            category = category == null ? "" : category.trim();
            name = name == null ? "" : name.trim();
            status = status == null ? "" : status.trim();
            locator = locator == null ? "" : locator.trim();
            url = url == null ? "" : url.trim();
            message = message == null ? "" : message.trim();
            exceptionType = exceptionType == null ? "" : exceptionType.trim();
            exceptionMessage = exceptionMessage == null ? "" : exceptionMessage.trim();
        }

        static McpTraceActionSummary empty() {
            return new McpTraceActionSummary("", "", "", "", "", "", "", "", "", 0);
        }
    }
}
