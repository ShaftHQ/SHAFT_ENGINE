package com.shaft.mcp;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.capture.generate.CaptureGenerator.CodegenBackend;
import com.shaft.doctor.history.AllureHistoryIngestor;
import com.shaft.doctor.history.AllureHistoryModels;
import com.shaft.doctor.history.DualFlakeComputer;
import com.shaft.doctor.history.SmartTagModels;
import com.shaft.doctor.history.SmartTagsComputer;
import com.shaft.doctor.history.ErrorClusterModels;
import com.shaft.doctor.history.HealInsightModels;
import com.shaft.doctor.history.HealInsightsAggregator;
import com.shaft.doctor.history.ReportSummaryComputer;
import com.shaft.doctor.history.ReportSummaryModels;
import com.shaft.doctor.history.LocalMuteModels;
import com.shaft.doctor.history.LocalMuteStore;
import com.shaft.doctor.history.FlakeModels;
import com.shaft.doctor.history.UniqueErrorClusterer;
import com.shaft.doctor.shard.FlakyCluster;
import com.shaft.doctor.shard.MergedReport;
import com.shaft.doctor.shard.ShardIntelligence;
import com.shaft.doctor.shard.ShardMerger;
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
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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
    private static final int MAX_TRACE_JSON_BYTES = 64 * 1024 * 1024;
    private static final long MAX_VIEWER_BYTES = 64L * 1024 * 1024;
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
        try {
            com.shaft.tools.io.internal.TraceArchiveReader.extractNamed(
                    archive, "SHAFT Trace Report.html", htmlPath, MAX_VIEWER_BYTES);
            return new McpTraceViewerResult("1.0", relative(htmlPath), true, List.of());
        } catch (IOException exception) {
            String message = exception.getMessage() == null ? "" : exception.getMessage();
            if (message.toLowerCase(Locale.ROOT).contains("limit")) {
                return new McpTraceViewerResult("1.0", "", false,
                        List.of("Trace viewer exceeds the 64 MiB extraction limit."));
            }
            if (message.contains("does not contain") || message.contains("unsafe")) {
                return new McpTraceViewerResult("1.0", "", false,
                        List.of("Trace ZIP " + relative(archive) + " does not contain SHAFT Trace Report.html."));
            }
            return new McpTraceViewerResult("1.0", "", false,
                    List.of("Trace ZIP could not be read."));
        }
    }

    private static void copyBounded(InputStream input, OutputStream output, long maxBytes) throws IOException {
        byte[] buffer = new byte[16 * 1024];
        long retained = 0;
        int read;
        while ((read = input.read(buffer)) != -1) {
            if (retained > maxBytes - read) {
                throw new EntryTooLargeException();
            }
            output.write(buffer, 0, read);
            retained += read;
        }
    }

    private static final class EntryTooLargeException extends IOException {
    }

    /**
     * Merges N per-shard blobs (raw Allure results + optional traces + optional doctor
     * {@code ExecutionIntelligence}, produced by independent {@code -Dshaft.shard=N/M} runs) into
     * one Allure result set plus a timeline "speedboard" HTML and a cross-shard flaky-clustering
     * summary. See {@link ShardMerger} for the merge semantics.
     *
     * @param shardBlobPaths shard blob root directories inside the MCP workspace, in merge order
     * @param outputDirectory merged output directory inside the MCP workspace; blank selects
     *                        {@code target/shaft-merged-report}
     * @return merged Allure results directory, speedboard path, and flaky-clustering summary
     */
    @Tool(name = "report_merge_shards",
            description = "merges N per-shard Allure/trace/doctor-intelligence blobs into one Allure result set plus a flaky-clustering speedboard HTML")
    public McpMergeShardsResult reportMergeShards(List<String> shardBlobPaths, String outputDirectory) {
        List<Path> shardRoots = workspacePolicy.existingList(shardBlobPaths, "Shard blob path");
        Path output = outputDirectory == null || outputDirectory.isBlank()
                ? workspacePolicy.output("target/shaft-merged-report", "Merged report output directory")
                : workspacePolicy.output(outputDirectory, "Merged report output directory");
        MergedReport report = ShardMerger.merge(shardRoots, output);
        return new McpMergeShardsResult(
                "1.0",
                relative(report.mergedAllureResultsDirectory()),
                relative(report.speedboardHtmlPath()),
                report.shardCount(),
                report.totalResults(),
                report.flakyClusters(),
                report.shardIntelligence(),
                report.warnings());
    }

    /**
     * Ingests Allure {@code history.jsonl} plus optional Doctor JSON for the Reporting canvas
     * (issue #5967 / S3-01). Never replaces the {@code allure-results} root; missing history is an
     * empty state. Distinguishes cross-run {@code HISTORY} launches from intra-launch {@code RETRY}
     * attempts discovered in the current allure-results tree.
     *
     * @param historyPath optional path to history.jsonl; blank defaults to {@code target/history.jsonl}
     * @param doctorReportPath optional Doctor JSON path inside the workspace
     * @param allureResultsPath optional allure-results directory for retry detection; blank defaults to
     *                          {@code target/allure-results} when present
     * @param limitPerHistoryId max launches retained per historyId (default 10, max 50)
     * @return append-only history view for the IDE Reporting canvas / CLI
     */
    @Tool(name = "report_history",
            description = "ingests Allure history.jsonl plus optional Doctor JSON for cross-run history in the Reporting canvas; never replaces allure-results; missing history is empty-state")
    public AllureHistoryModels.HistoryView reportHistory(
            @ToolParam(required = false) String historyPath,
            @ToolParam(required = false) String doctorReportPath,
            @ToolParam(required = false) String allureResultsPath,
            @ToolParam(required = false) Integer limitPerHistoryId) {
        Path history = resolveOptionalReadable(
                historyPath, "target/history.jsonl", "Allure history.jsonl");
        Path doctor = resolveOptionalReadable(doctorReportPath, null, "Doctor report JSON");
        Path results = resolveOptionalReadable(
                allureResultsPath, "target/allure-results", "Allure results directory");
        int limit = limitPerHistoryId == null ? 10 : limitPerHistoryId;
        return AllureHistoryIngestor.ingest(history, doctor, results, limit);
    }

    /**
     * Dual flake table: retry-hidden (intra-run) and cross-launch transitions as separate columns
     * (issue #5968 / S3-02). Never collapses into one score. CI commit metadata is optional.
     * Insufficient history is {@code unknown}, not a fabricated 0% rate; 100% failing is not flaky.
     *
     * @param historyPath optional history.jsonl; blank defaults to {@code target/history.jsonl}
     * @param doctorReportPath optional Doctor JSON
     * @param allureResultsPath optional allure-results for retry-hidden detection
     * @param limitPerHistoryId max launches in the transition window (default 10, max 50)
     * @param transitionThreshold min flips to tag transitions (default 3)
     * @return flake table with independent retry-hidden and transition assessments
     */
    @Tool(name = "report_flake",
            description = "builds a dual flake table with separate retry-hidden (intra-run) and cross-launch transition tags; never a single combined score; unknown history is explicit; CI commit metadata optional")
    public FlakeModels.FlakeTable reportFlake(
            @ToolParam(required = false) String historyPath,
            @ToolParam(required = false) String doctorReportPath,
            @ToolParam(required = false) String allureResultsPath,
            @ToolParam(required = false) Integer limitPerHistoryId,
            @ToolParam(required = false) Integer transitionThreshold) {
        Path history = resolveOptionalReadable(
                historyPath, "target/history.jsonl", "Allure history.jsonl");
        Path doctor = resolveOptionalReadable(doctorReportPath, null, "Doctor report JSON");
        Path results = resolveOptionalReadable(
                allureResultsPath, "target/allure-results", "Allure results directory");
        int limit = limitPerHistoryId == null ? 10 : limitPerHistoryId;
        int threshold = transitionThreshold == null ? 3 : transitionThreshold;
        AllureHistoryModels.HistoryView view =
                AllureHistoryIngestor.ingest(history, doctor, results, limit);
        return DualFlakeComputer.compute(view, limit, threshold);
    }

    /**
     * Smart tags: New / Always-failing / Flaky / Regressed / Fixed from Allure history
     * (issue #5975 / S3-09). First-seen failure is New, not Regressed. Insufficient history
     * never invents Flaky. Duration-anomaly is optional when timings exist.
     *
     * @param historyPath optional history.jsonl; blank defaults to {@code target/history.jsonl}
     * @param doctorReportPath optional Doctor JSON
     * @param allureResultsPath optional allure-results (unused for tags; accepted for path parity)
     * @param limitPerHistoryId max launches in the window (default 10, max 50)
     * @param flakyTransitionThreshold min flips to tag Flaky (default 3)
     * @return smart-tag table for Reporting / SHAFT Tests
     */
    @Tool(name = "report_smart_tags",
            description = "computes New / Always-failing / Flaky / Regressed / Fixed smart tags from Allure history; first-seen failure is New not Regressed; insufficient history never invents Flaky; duration anomaly optional when timings exist")
    public SmartTagModels.SmartTagTable reportSmartTags(
            @ToolParam(required = false) String historyPath,
            @ToolParam(required = false) String doctorReportPath,
            @ToolParam(required = false) String allureResultsPath,
            @ToolParam(required = false) Integer limitPerHistoryId,
            @ToolParam(required = false) Integer flakyTransitionThreshold) {
        Path history = resolveOptionalReadable(
                historyPath, "target/history.jsonl", "Allure history.jsonl");
        Path doctor = resolveOptionalReadable(doctorReportPath, null, "Doctor report JSON");
        Path results = resolveOptionalReadable(
                allureResultsPath, "target/allure-results", "Allure results directory");
        int limit = limitPerHistoryId == null ? 10 : limitPerHistoryId;
        int threshold = flakyTransitionThreshold == null ? 3 : flakyTransitionThreshold;
        AllureHistoryModels.HistoryView view =
                AllureHistoryIngestor.ingest(history, doctor, results, limit);
        return SmartTagsComputer.compute(view, limit, threshold);
    }

    /**
     * Unique-error clusters keyed by Doctor historical-signature / clusterFingerprint
     * (issue #5969 / S3-03). Groups failed/broken Allure results into error → impacted tests.
     * Deterministic — no cloud ML. Empty results yield empty-state.
     *
     * @param allureResultsPath optional allure-results directory; blank defaults to
     *                          {@code target/allure-results} when present
     * @param doctorReportPath optional Doctor JSON that may carry clusterFingerprint evidence
     * @return unique-error cluster table for the IDE Reporting canvas / CLI
     */
    @Tool(name = "report_clusters",
            description = "clusters failed Allure results by Doctor historical-signature keys into error → impacted tests; deterministic, no cloud ML; empty results are empty-state")
    public ErrorClusterModels.ClusterTable reportClusters(
            @ToolParam(required = false) String allureResultsPath,
            @ToolParam(required = false) String doctorReportPath) {
        Path results = resolveOptionalReadable(
                allureResultsPath, "target/allure-results", "Allure results directory");
        Path doctor = resolveOptionalReadable(doctorReportPath, null, "Doctor report JSON");
        return UniqueErrorClusterer.cluster(results, doctor);
    }

    /**
     * Heal insights: counts by HealingDecision status with persist-on-pass review gate
     * (issue #5972 / S3-06). Proposals are never auto-landed; only RECOVERED + passing replay
     * offers a reviewable diff via {@code doctor_propose_healed_locator}. AMBIGUOUS has no
     * apply-to-source primary action. NO_CANDIDATES is shown, not silent. Reuse
     * {@code healer_run_failed_test} for guarded replay. Policy: locator-healing.md / #5454.
     *
     * @param reportsPath optional heal-reports directory; blank defaults to
     *                    {@code target/shaft-heal/reports} when present
     * @param proposalsPath optional proposal-manifests directory; blank defaults to
     *                      {@code target/shaft-doctor/healing-proposals} when present
     * @return heal insights table for the IDE Reporting canvas / CLI
     */
    @Tool(name = "report_heal",
            description = "dashboards SHAFT Heal insights by HealingDecision status with persist-on-pass review gate; RECOVERED+pass offers reviewable patch; AMBIGUOUS cannot apply source; NO_CANDIDATES shown; never auto-writes locators")
    public HealInsightModels.HealInsightsTable reportHeal(
            @ToolParam(required = false) String reportsPath,
            @ToolParam(required = false) String proposalsPath) {
        Path reports = resolveOptionalReadable(
                reportsPath, "target/shaft-heal/reports", "SHAFT Heal reports directory");
        Path proposals = resolveOptionalReadable(
                proposalsPath, "target/shaft-doctor/healing-proposals",
                "SHAFT Heal proposal manifests directory");
        return HealInsightsAggregator.aggregate(reports, proposals);
    }


    /**
     * Opens the newest Allure HTML report when present; otherwise returns an empty-state CTA for
     * {@code generate_test_report} (issue #5976 / S3-10). Never rewrites Allure HTML.
     *
     * @param reportPath optional Allure HTML path; blank auto-discovers {@code *AllureReport.html}
     * @param allureResultsPath optional allure-results directory for empty-state context
     * @param openInBrowser when true (default), attempt to open the HTML in the host browser
     * @return open view with path or generate_test_report CTA
     */
    @Tool(name = "report_open",
            description = "opens the newest Allure HTML report when present; empty-state returns generate_test_report CTA; never rewrites Allure HTML; CLI/MCP parity for Reporting canvas Open Allure")
    public ReportSummaryModels.OpenView reportOpen(
            @ToolParam(required = false) String reportPath,
            @ToolParam(required = false) String allureResultsPath,
            @ToolParam(required = false) Boolean openInBrowser) {
        Path results = resolveOptionalReadable(
                allureResultsPath, "target/allure-results", "Allure results directory");
        Path report = resolveExplicitReport(reportPath);
        if (report == null) {
            report = McpAllureResultsLocator.latestReport(workspacePolicy.root()).orElse(null);
        }
        if (results == null) {
            results = McpAllureResultsLocator.latest(workspacePolicy.root());
        }
        boolean wantOpen = openInBrowser == null || openInBrowser;
        boolean opened = false;
        if (report != null && wantOpen) {
            opened = browseHtml(report);
        }
        ReportSummaryModels.OpenView view = ReportSummaryComputer.openView(report, results, opened);
        if (!view.empty()) {
            return new ReportSummaryModels.OpenView(
                    view.schemaVersion(),
                    false,
                    "",
                    relativeOrAbsolute(report),
                    results == null ? "" : relativeOrAbsolute(results),
                    opened,
                    "",
                    view.warnings());
        }
        return view;
    }

    /**
     * Builds engineer + stakeholder summaries with reconciled Allure counts (issue #5976 / S3-10).
     * Reuses shaft-execution-reporting / shaft-stakeholder-reporting playbook shapes. Never secrets.
     *
     * @param allureResultsPath optional allure-results; blank defaults to target/allure-results
     * @param reportPath optional Allure HTML path
     * @param historyPath optional history.jsonl for flake tallies
     * @param doctorReportPath optional Doctor JSON (path only; contents never copied)
     * @param healReportsPath optional heal reports directory
     * @param healProposalsPath optional heal proposals directory
     * @return dual-audience summary with reconciled counts
     */
    @Tool(name = "report_summary",
            description = "builds engineer and stakeholder Reporting summaries with reconciled Allure counts, flake and heal tallies; playbook-shaped; never includes secrets; empty-state cites generate_test_report CTA")
    public ReportSummaryModels.SummaryView reportSummary(
            @ToolParam(required = false) String allureResultsPath,
            @ToolParam(required = false) String reportPath,
            @ToolParam(required = false) String historyPath,
            @ToolParam(required = false) String doctorReportPath,
            @ToolParam(required = false) String healReportsPath,
            @ToolParam(required = false) String healProposalsPath) {
        Path results = resolveOptionalReadable(
                allureResultsPath, "target/allure-results", "Allure results directory");
        if (results == null || !Files.isDirectory(results)) {
            Path discovered = McpAllureResultsLocator.latest(workspacePolicy.root());
            if (discovered != null) {
                results = discovered;
            }
        }
        Path report = resolveExplicitReport(reportPath);
        if (report == null) {
            report = McpAllureResultsLocator.latestReport(workspacePolicy.root()).orElse(null);
        }
        Path history = resolveOptionalReadable(
                historyPath, "target/history.jsonl", "Allure history.jsonl");
        Path doctor = resolveOptionalReadable(doctorReportPath, null, "Doctor report JSON");
        Path healReports = resolveOptionalReadable(
                healReportsPath, "target/shaft-heal/reports", "SHAFT Heal reports directory");
        Path healProposals = resolveOptionalReadable(
                healProposalsPath, "target/shaft-doctor/healing-proposals",
                "SHAFT Heal proposal manifests directory");
        ReportSummaryModels.SummaryView view = ReportSummaryComputer.compute(
                results, report, history, doctor, healReports, healProposals);
        if (view.empty()) {
            return view;
        }
        return new ReportSummaryModels.SummaryView(
                view.schemaVersion(),
                false,
                "",
                results == null ? view.allureResultsPath() : relativeOrAbsolute(results),
                report == null ? view.reportPath() : relativeOrAbsolute(report),
                view.counts(),
                view.flakeRetryHiddenCount(),
                view.flakeTransitionsCount(),
                view.healRecoveredCount(),
                view.healAmbiguousCount(),
                view.healNoCandidatesCount(),
                view.engineerSummary(),
                view.stakeholderSummary(),
                view.warnings());
    }

    /**
     * Local flake mute / quarantine lifecycle for the SHAFT Tests panel and CLI
     * (issue #5974 / S3-08). Mute requires a reason. Recover clears after N consecutive
     * local passes. Never writes Maven Surefire excludes in v1 (FR-002 / SC-002).
     *
     * @param action {@code mute}, {@code unmute}, {@code list}, or {@code observe}
     * @param testId test id (required for mute/unmute/observe)
     * @param reason mute reason (required for mute)
     * @param muteStorePath optional store path; blank uses {@code .shaft/local-mutes.json}
     * @param recoverAfterPasses optional recover threshold for new mutes (default 3)
     * @param passed observe outcome; required when action is observe
     * @param writeSurefireExcludes refused in v1 — warning only
     * @return mute table (active mutes; observe may include a RECOVERED row)
     */
    @Tool(name = "report_mute",
            description = "local flake mute/quarantine with required reason and recover-after-N-local-passes; never writes Maven Surefire excludes by default; store is gitignored .shaft/local-mutes.json unless muteStorePath opts into a shared project file")
    public LocalMuteModels.MuteTable reportMute(
            @ToolParam(required = false) String action,
            @ToolParam(required = false) String testId,
            @ToolParam(required = false) String reason,
            @ToolParam(required = false) String muteStorePath,
            @ToolParam(required = false) Integer recoverAfterPasses,
            @ToolParam(required = false) Boolean passed,
            @ToolParam(required = false) Boolean writeSurefireExcludes) {
        String act = action == null || action.isBlank() ? "list" : action.trim().toLowerCase(Locale.ROOT);
        LocalMuteStore store = LocalMuteStore.open(
                resolveMuteStorePath(muteStorePath),
                recoverAfterPasses == null ? LocalMuteModels.DEFAULT_RECOVER_AFTER_PASSES : recoverAfterPasses);
        return dispatchMuteAction(act, store, testId, reason, recoverAfterPasses, passed, writeSurefireExcludes, action);
    }

    private static LocalMuteModels.MuteTable dispatchMuteAction(
            String act,
            LocalMuteStore store,
            String testId,
            String reason,
            Integer recoverAfterPasses,
            Boolean passed,
            Boolean writeSurefireExcludes,
            String originalAction) {
        return switch (act) {
            case "mute", "add", "quarantine" -> store.mute(testId, reason, recoverAfterPasses, writeSurefireExcludes);
            case "unmute", "remove", "clear" -> store.unmute(testId);
            case "observe", "record", "outcome" -> observeMute(store, testId, passed);
            case "list", "mutes", "status" -> store.list();
            default -> throw new IllegalArgumentException(
                    "Unknown report_mute action '" + originalAction + "'. Use mute, unmute, list, or observe.");
        };
    }

    private static LocalMuteModels.MuteTable observeMute(LocalMuteStore store, String testId, Boolean passed) {
        if (passed == null) {
            throw new IllegalArgumentException("passed=true|false is required for action=observe");
        }
        return store.observe(testId, passed);
    }

    private Path resolveMuteStorePath(String muteStorePath) {
        if (muteStorePath == null || muteStorePath.isBlank()) {
            return LocalMuteStore.defaultStorePath(workspacePolicy.root());
        }
        return workspacePolicy.output(muteStorePath.trim(), "Local mute store");
    }


    private Path resolveExplicitReport(String reportPath) {
        if (reportPath == null || reportPath.isBlank()) {
            return null;
        }
        Path candidate = workspacePolicy.output(reportPath.trim(), "Allure HTML report");
        return Files.isRegularFile(candidate) ? candidate : null;
    }

    private String relativeOrAbsolute(Path path) {
        if (path == null) {
            return "";
        }
        try {
            Path root = workspacePolicy.root();
            Path absolute = path.toAbsolutePath().normalize();
            if (absolute.startsWith(root)) {
                return relative(absolute);
            }
        } catch (RuntimeException ignored) {
            // fall through to absolute path
        }
        return path.toAbsolutePath().normalize().toString().replace('\\', '/');
    }

    private static boolean browseHtml(Path report) {
        try {
            if (!java.awt.Desktop.isDesktopSupported()) {
                return false;
            }
            java.awt.Desktop desktop = java.awt.Desktop.getDesktop();
            if (!desktop.isSupported(java.awt.Desktop.Action.BROWSE)) {
                return false;
            }
            desktop.browse(report.toUri());
            return true;
        } catch (Exception exception) {
            return false;
        }
    }

    private Path resolveOptionalReadable(String value, String defaultRelative, String label) {
        String raw = value == null || value.isBlank() ? defaultRelative : value.trim();
        if (raw == null || raw.isBlank()) {
            return null;
        }
        Path candidate = workspacePolicy.output(raw, label);
        if (Files.exists(candidate)) {
            return candidate;
        }
        return candidate;
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
                    try {
                        String content = readBoundedUtf8(zip, entry, MAX_TRACE_JSON_BYTES, archive.getParent());
                        zip.closeEntry();
                        return new TraceDocument(publicPath, content, JSON.readTree(content));
                    } catch (EntryTooLargeException exception) {
                        throw new IllegalArgumentException("Trace JSON exceeds the 64 MiB read limit.");
                    }
                }
            }
        }
        throw new IllegalArgumentException("Trace archive does not contain shaft-trace.json.");
    }

    private static String readBoundedUtf8(InputStream input, ZipEntry entry, int maxBytes, Path stagingDirectory)
            throws IOException {
        if (entry.getSize() > maxBytes) {
            throw new EntryTooLargeException();
        }
        Path staging = Files.createTempFile(stagingDirectory, ".shaft-trace-json-", ".tmp");
        try {
            try (OutputStream output = Files.newOutputStream(staging)) {
                copyBounded(input, output, maxBytes);
            }
            return Files.readString(staging, StandardCharsets.UTF_8);
        } finally {
            Files.deleteIfExists(staging);
        }
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
        TraceEvidence evidence = traceEvidence(root);
        JsonNode test = evidence.test();
        JsonNode source = evidence.source();
        JsonNode exception = root.path("exception");
        JsonNode actionNode = failedAction(evidence.actions());
        McpTraceActionSummary action = action(actionNode, evidence.publicV2());
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
                networkFindings(evidence.network()),
                consoleFindings(evidence.console()),
                recommendations(action, exceptionType, exceptionMessage),
                List.of());
    }

    private TraceEvidence traceEvidence(JsonNode root) {
        String schemaVersion = text(root.path("schemaVersion"));
        if (schemaVersion.startsWith("3.")) {
            JsonNode evidence = root.path("evidence");
            if (!evidence.path("actions").isArray()) {
                throw new IllegalArgumentException("Trace v3 evidence.actions must be an array.");
            }
            return new TraceEvidence(root.path("test"), evidence.path("actions"),
                    evidence.path("network"), evidence.path("console"), root.path("source"), false);
        }
        if (schemaVersion.startsWith("2.")) {
            if (!root.path("events").isArray()) {
                throw new IllegalArgumentException("Trace v2 events must be an array.");
            }
            ObjectNode test = JSON.createObjectNode();
            String testId = text(root.path("testId"));
            JsonNode primaryAction = failedAction(root.path("events"));
            test.put("className", "");
            test.put("methodName", "");
            test.put("displayName", testId);
            test.put("status", text(primaryAction.path("status")).toLowerCase(Locale.ROOT));
            return new TraceEvidence(test, root.path("events"), root.path("network"), root.path("console"),
                    source(primaryAction), true);
        }
        if (!schemaVersion.isBlank() && !schemaVersion.startsWith("1.")) {
            throw new IllegalArgumentException("Unsupported trace schema major version "
                    + schemaVersion.split("\\.", 2)[0] + ".");
        }
        return new TraceEvidence(root.path("test"), root.path("actions"), root.path("network"),
                root.path("console"), root.path("source"), false);
    }

    private ObjectNode source(JsonNode action) {
        String frame = text(action.path("source"));
        ObjectNode source = JSON.createObjectNode();
        source.put("frame", frame);
        java.util.regex.Matcher matcher = Pattern.compile("\\(([^():]+):(\\d+)\\)$").matcher(frame);
        boolean matched = matcher.find();
        source.put("file", matched ? matcher.group(1) : "");
        source.put("line", matched ? matcher.group(2) : "");
        source.put("snippet", "");
        return source;
    }

    private record TraceEvidence(JsonNode test, JsonNode actions, JsonNode network, JsonNode console, JsonNode source,
                                 boolean publicV2) { }

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

    private McpTraceActionSummary action(JsonNode action, boolean publicV2) {
        JsonNode exception = action.path("exception");
        JsonNode metadata = action.path("metadata");
        return new McpTraceActionSummary(
                text(action.path("id")),
                text(action.path("category")),
                text(action.path("name")),
                text(action.path("status")).toLowerCase(Locale.ROOT),
                publicV2 ? text(action.path("target")) : text(action.path("locator")),
                publicV2 ? text(metadata.path("url")) : text(action.path("url")),
                text(action.path("message")),
                publicV2 ? text(metadata.path("exceptionType")) : text(exception.path("type")),
                publicV2 ? text(metadata.path("exceptionMessage")) : text(exception.path("message")),
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
            recommendations.add("Inspect the current DOM before changing locator: browser_get_page_dom (dispatches to the active engine).");
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
     * Merged-shard report result.
     *
     * @param schemaVersion result schema version
     * @param mergedAllureResultsDirectory merged Allure results directory, workspace-relative
     * @param speedboardHtmlPath timeline speedboard HTML path, workspace-relative
     * @param shardCount number of shard blobs merged
     * @param totalResults total Allure result files merged
     * @param flakyClusters tests observed with inconsistent pass/fail outcomes across shards
     * @param shardIntelligence per-shard doctor {@code ExecutionIntelligence} digests, for shards that had one
     * @param warnings safe warnings (e.g. an unreadable shard blob was skipped)
     */
    public record McpMergeShardsResult(
            String schemaVersion,
            String mergedAllureResultsDirectory,
            String speedboardHtmlPath,
            int shardCount,
            int totalResults,
            List<FlakyCluster> flakyClusters,
            List<ShardIntelligence> shardIntelligence,
            List<String> warnings) {
        /**
         * Creates an immutable merge-shards result.
         */
        public McpMergeShardsResult {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
            mergedAllureResultsDirectory = mergedAllureResultsDirectory == null ? "" : mergedAllureResultsDirectory.trim();
            speedboardHtmlPath = speedboardHtmlPath == null ? "" : speedboardHtmlPath.trim();
            flakyClusters = flakyClusters == null ? List.of() : List.copyOf(flakyClusters);
            shardIntelligence = shardIntelligence == null ? List.of() : List.copyOf(shardIntelligence);
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
