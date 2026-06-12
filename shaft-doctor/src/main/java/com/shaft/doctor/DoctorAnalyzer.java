package com.shaft.doctor;

import com.shaft.doctor.ai.DoctorAiAnalysisService;
import com.shaft.doctor.analysis.DeterministicRuleEngine;
import com.shaft.doctor.collect.EvidenceCollector;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAdvisory;
import com.shaft.doctor.model.DoctorAiAnalysisResult;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.report.DoctorReportWriter;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Offline deterministic SHAFT evidence analyzer.
 */
public final class DoctorAnalyzer {
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
        Diagnosis diagnosis = rules.diagnose(bundle, collector.loadHistoricalBundles(resolvedRequest));
        Path output = resolvedRequest.outputDirectory();
        Path bundlePath = output.resolve("doctor-evidence.json");
        Path jsonReportPath = output.resolve("doctor-report.json");
        Path markdownReportPath = output.resolve("doctor-report.md");
        codec.write(bundlePath, bundle);
        codec.writeReport(jsonReportPath, bundle, diagnosis);
        reports.writeMarkdown(markdownReportPath, bundle, diagnosis);
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
}
