package com.shaft.doctor;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.shaft.doctor.ai.DoctorAiAnalysisService;
import com.shaft.doctor.analysis.DeterministicRuleEngine;
import com.shaft.doctor.collect.EvidenceCollector;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.model.ExecutionIntelligence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAdvisory;
import com.shaft.doctor.model.DoctorAiAnalysisResult;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.DoctorTriage;
import com.shaft.doctor.report.DoctorReportWriter;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Offline deterministic SHAFT evidence analyzer.
 */
public final class DoctorAnalyzer {
    private static final ObjectMapper JSON = new ObjectMapper()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
    private static final Set<String> FAILURE_STATUSES = Set.of("failed", "broken");
    private static final Set<String> NON_FINAL_ISSUE_STATUSES = Set.of("failed", "broken", "skipped");

    private final EvidenceCollector collector;
    private final DeterministicRuleEngine rules;
    private final DoctorJsonCodec codec;
    private final DoctorReportWriter reports;
    private final DoctorAiAnalysisService aiAnalysis;

    /**
     * Creates the default analyzer.
     */
    public DoctorAnalyzer() {
        this(new EvidenceCollector(), new DeterministicRuleEngine(),
                new DoctorJsonCodec(), new DoctorReportWriter(), new DoctorAiAnalysisService());
    }

    DoctorAnalyzer(
            EvidenceCollector collector,
            DeterministicRuleEngine rules,
            DoctorJsonCodec codec,
            DoctorReportWriter reports) {
        this(collector, rules, codec, reports, new DoctorAiAnalysisService());
    }

    DoctorAnalyzer(
            EvidenceCollector collector,
            DeterministicRuleEngine rules,
            DoctorJsonCodec codec,
            DoctorReportWriter reports,
            DoctorAiAnalysisService aiAnalysis) {
        this.collector = collector;
        this.rules = rules;
        this.codec = codec;
        this.reports = reports;
        this.aiAnalysis = aiAnalysis;
    }

    /**
     * Collects evidence, diagnoses it, and writes portable JSON and Markdown reports.
     *
     * @param request explicit local analysis policy
     * @return analysis outputs
     */
    public DoctorAnalysisResult analyze(DoctorAnalysisRequest request) {
        DoctorAnalysisRequest resolvedRequest = resolveOutputBoundary(request);
        EvidenceBundle bundle = collector.collect(resolvedRequest);
        List<EvidenceBundle> history = collector.loadHistoricalBundles(resolvedRequest);
        Diagnosis diagnosis = rules.diagnose(bundle, history);
        Path output = resolvedRequest.outputDirectory();
        Path bundlePath = output.resolve("doctor-evidence.json");
        Path jsonReportPath = output.resolve("doctor-report.json");
        Path markdownReportPath = output.resolve("doctor-report.md");
        DoctorTriage triage = triage(bundle, history, diagnosis);
        ExecutionIntelligence intelligence = intelligence(triage, diagnosis);
        codec.write(bundlePath, bundle);
        codec.writeReport(jsonReportPath, bundle, diagnosis);
        reports.writeMarkdown(markdownReportPath, bundle, diagnosis);
        writeJson(output.resolve("doctor-triage.json"), triage);
        writeMarkdown(output.resolve("doctor-triage.md"), triageMarkdown(triage));
        writeJson(output.resolve("execution-intelligence.json"), intelligence);
        writeMarkdown(output.resolve("execution-intelligence.md"), intelligenceMarkdown(intelligence));
        return new DoctorAnalysisResult(
                bundle,
                diagnosis,
                bundlePath.toString(),
                jsonReportPath.toString(),
                markdownReportPath.toString());
    }

    /**
     * Runs deterministic analysis first, then optionally appends a separate provider advisory.
     *
     * <p>The deterministic diagnosis is never replaced or rewritten. A disabled request
     * delegates to {@link #analyze(DoctorAnalysisRequest)} without changing report bytes.</p>
     *
     * @param request explicit local analysis policy
     * @param aiRequest explicit provider-analysis policy
     * @return deterministic result and separately identified advisory
     */
    public DoctorAiAnalysisResult analyzeWithAi(
            DoctorAnalysisRequest request,
            DoctorAiAnalysisRequest aiRequest) {
        DoctorAiAnalysisRequest resolvedAiRequest =
                aiRequest == null ? DoctorAiAnalysisRequest.disabled() : aiRequest;
        DoctorAnalysisResult deterministic = analyze(request);
        if (!resolvedAiRequest.enabled()) {
            return new DoctorAiAnalysisResult(deterministic, DoctorAdvisory.disabled());
        }

        Path output = Path.of(deterministic.jsonReportPath()).toAbsolutePath().normalize().getParent();
        DoctorAdvisory advisory = aiAnalysis.analyze(
                deterministic.bundle(),
                deterministic.diagnosis(),
                resolvedAiRequest,
                output);
        codec.writeReport(
                Path.of(deterministic.jsonReportPath()),
                deterministic.bundle(),
                deterministic.diagnosis(),
                advisory);
        reports.writeMarkdown(
                Path.of(deterministic.markdownReportPath()),
                deterministic.bundle(),
                deterministic.diagnosis(),
                advisory);
        return new DoctorAiAnalysisResult(deterministic, advisory);
    }

    private static DoctorAnalysisRequest resolveOutputBoundary(DoctorAnalysisRequest request) {
        if (request == null) {
            throw new IllegalArgumentException("Doctor analysis request is required.");
        }
        List<Path> roots = request.allowedRoots().stream()
                .map(DoctorAnalyzer::realPath)
                .distinct()
                .toList();
        Path output = request.outputDirectory().toAbsolutePath().normalize();
        if (Files.exists(output) && !Files.isDirectory(output)) {
            throw new IllegalArgumentException("Doctor output path must be a directory.");
        }

        Path existingAncestor = output;
        List<Path> missingSegments = new ArrayList<>();
        while (existingAncestor != null && !Files.exists(existingAncestor)) {
            missingSegments.add(existingAncestor.getFileName());
            existingAncestor = existingAncestor.getParent();
        }
        if (existingAncestor == null) {
            throw new IllegalArgumentException("Doctor output path has no resolvable ancestor.");
        }

        Path resolvedOutput = realPath(existingAncestor);
        for (int index = missingSegments.size() - 1; index >= 0; index--) {
            resolvedOutput = resolvedOutput.resolve(missingSegments.get(index));
        }
        resolvedOutput = resolvedOutput.normalize();
        if (roots.stream().noneMatch(resolvedOutput::startsWith)) {
            throw new IllegalArgumentException("Doctor output directory is outside the explicit allowed roots.");
        }
        return new DoctorAnalysisRequest(
                request.inputPaths(),
                request.historicalBundlePaths(),
                roots,
                resolvedOutput,
                request.includeScreenshots(),
                request.includePageSnapshots(),
                request.minimumAllureResults(),
                request.maxItemBytes(),
                request.maxBundleBytes());
    }

    private static Path realPath(Path path) {
        try {
            return path.toRealPath();
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor allowed root or output ancestor cannot be resolved.", exception);
        }
    }

    private static DoctorTriage triage(
            EvidenceBundle bundle,
            List<EvidenceBundle> history,
            Diagnosis diagnosis) {
        List<EvidenceItem> allure = validAllure(bundle);
        List<EvidenceItem> failures = allure.stream()
                .filter(item -> FAILURE_STATUSES.contains(item.attributes().get("status")))
                .toList();
        Set<String> historicalSignatures = new LinkedHashSet<>();
        for (EvidenceBundle historical : history == null ? List.<EvidenceBundle>of() : history) {
            validAllure(historical).stream()
                    .filter(item -> FAILURE_STATUSES.contains(item.attributes().get("status")))
                    .map(item -> item.attributes().getOrDefault("signature", ""))
                    .filter(signature -> !signature.isBlank())
                    .forEach(historicalSignatures::add);
        }
        int recurring = Math.toIntExact(failures.stream()
                .map(item -> item.attributes().getOrDefault("signature", ""))
                .filter(historicalSignatures::contains)
                .count());
        String primarySignature = failures.stream()
                .map(item -> item.attributes().getOrDefault("signature", ""))
                .filter(signature -> !signature.isBlank())
                .findFirst()
                .orElse("");
        return new DoctorTriage(
                DoctorTriage.CURRENT_SCHEMA_VERSION,
                bundle.bundleId(),
                allure.size(),
                failures.size(),
                hiddenRetryFailures(allure),
                recurring,
                primarySignature,
                diagnosis.summary(),
                citedEvidence(diagnosis));
    }

    private static ExecutionIntelligence intelligence(DoctorTriage triage, Diagnosis diagnosis) {
        String summary = triage.failingAttempts() == 0
                ? "No failed or broken Allure attempt was present."
                : triage.failingAttempts() + " failing attempt(s); primary cause "
                        + diagnosis.primaryCause() + " with " + diagnosis.confidence() + " confidence.";
        if (triage.hiddenRetryFailures() > 0) {
            summary += " " + triage.hiddenRetryFailures() + " retry-hidden failure group(s) need review.";
        }
        if (triage.recurringFailures() > 0) {
            summary += " " + triage.recurringFailures() + " recurring failure(s) matched historical bundles.";
        }
        return new ExecutionIntelligence(
                ExecutionIntelligence.CURRENT_SCHEMA_VERSION,
                triage.bundleId(),
                triage.totalAllureResults(),
                triage.failingAttempts(),
                triage.hiddenRetryFailures(),
                triage.recurringFailures(),
                diagnosis.primaryCause(),
                diagnosis.confidence(),
                summary);
    }

    private static List<EvidenceItem> validAllure(EvidenceBundle bundle) {
        return bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .filter(item -> !"true".equals(item.attributes().get("invalid")))
                .toList();
    }

    private static int hiddenRetryFailures(List<EvidenceItem> attempts) {
        Map<String, List<EvidenceItem>> groups = new LinkedHashMap<>();
        for (EvidenceItem attempt : attempts) {
            String key = attempt.attributes().getOrDefault(
                    "historyId", attempt.attributes().getOrDefault("name", attempt.id()));
            groups.computeIfAbsent(key, ignored -> new ArrayList<>()).add(attempt);
        }
        int hidden = 0;
        for (List<EvidenceItem> group : groups.values()) {
            group.sort(Comparator.comparingLong(DoctorAnalyzer::start).thenComparing(EvidenceItem::id));
            EvidenceItem last = group.getLast();
            boolean priorFailure = group.subList(0, Math.max(0, group.size() - 1)).stream()
                    .anyMatch(item -> NON_FINAL_ISSUE_STATUSES.contains(item.attributes().get("status")));
            if (group.size() > 1 && priorFailure && "passed".equals(last.attributes().get("status"))) {
                hidden++;
            }
        }
        return hidden;
    }

    private static List<String> citedEvidence(Diagnosis diagnosis) {
        LinkedHashSet<String> ids = new LinkedHashSet<>();
        diagnosis.findings().forEach(finding -> ids.addAll(finding.evidenceIds()));
        diagnosis.remediations().forEach(remediation -> ids.addAll(remediation.evidenceIds()));
        return List.copyOf(ids);
    }

    private static long start(EvidenceItem item) {
        try {
            return Long.parseLong(item.attributes().getOrDefault("start", "0"));
        } catch (NumberFormatException exception) {
            return 0;
        }
    }

    private static void writeJson(Path path, Object value) {
        try {
            Files.createDirectories(path.getParent());
            Files.writeString(path, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(value) + "\n",
                    StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor intelligence artifact could not be written.", exception);
        }
    }

    private static void writeMarkdown(Path path, String value) {
        try {
            Files.createDirectories(path.getParent());
            Files.writeString(path, value, StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor intelligence Markdown could not be written.", exception);
        }
    }

    private static String triageMarkdown(DoctorTriage triage) {
        return "# SHAFT Doctor Triage\n\n"
                + "- Bundle: `" + triage.bundleId() + "`\n"
                + "- Valid Allure results: " + triage.totalAllureResults() + "\n"
                + "- Failing attempts: " + triage.failingAttempts() + "\n"
                + "- Retry-hidden failures: " + triage.hiddenRetryFailures() + "\n"
                + "- Recurring failures: " + triage.recurringFailures() + "\n"
                + "- Primary signature: `" + triage.primarySignature() + "`\n\n"
                + triage.summary() + "\n";
    }

    private static String intelligenceMarkdown(ExecutionIntelligence intelligence) {
        return "# SHAFT Execution Intelligence\n\n"
                + "- Bundle: `" + intelligence.bundleId() + "`\n"
                + "- Primary cause: `" + intelligence.primaryCause() + "`\n"
                + "- Confidence: `" + intelligence.confidence() + "`\n"
                + "- Valid Allure results: " + intelligence.totalAllureResults() + "\n"
                + "- Failing attempts: " + intelligence.failingAttempts() + "\n"
                + "- Retry-hidden failures: " + intelligence.hiddenRetryFailures() + "\n"
                + "- Recurring failures: " + intelligence.recurringFailures() + "\n\n"
                + intelligence.summary() + "\n";
    }
}
