package com.shaft.mcp;

import com.shaft.doctor.DoctorAiAnalysisRequest;
import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.DoctorAnalyzer;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.DoctorAnalysisSummary;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.repair.DoctorRepairAiRequest;
import com.shaft.doctor.repair.DoctorRepairAiService;
import com.shaft.doctor.repair.DoctorRepairPatchResult;
import com.shaft.doctor.repair.DoctorRepairProposalResult;
import com.shaft.doctor.repair.DoctorRepairPublicationRequest;
import com.shaft.doctor.repair.DoctorRepairRequest;
import com.shaft.doctor.repair.DoctorRepairService;
import com.shaft.doctor.repair.HealingLocatorProposal;
import com.shaft.doctor.repair.HealingLocatorProposalRequest;
import com.shaft.doctor.repair.HealingLocatorProposalService;
import com.shaft.doctor.repair.RepairPublicationResult;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.config.PilotConfiguration;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.EnumSet;

/**
 * MCP adapter for deterministic SHAFT Doctor analysis with optional configured provider advisory.
 */
@Service
public class DoctorService {
    private static final ObjectMapper JSON = new ObjectMapper();
    private final McpWorkspacePolicy workspacePolicy;
    private final McpDoctorRemediationService remediationService;

    /**
     * Creates the default local MCP Doctor service.
     */
    public DoctorService() {
        this(McpWorkspacePolicy.current(), new McpDoctorRemediationService());
    }

    DoctorService(McpWorkspacePolicy workspacePolicy, McpDoctorRemediationService remediationService) {
        this.workspacePolicy = workspacePolicy;
        this.remediationService = remediationService;
    }

    /**
     * Analyzes failed Allure evidence and returns deterministic remediation plus optional AI advisory snippets.
     *
     * @param allureResultPaths Allure result directories or files inside the MCP workspace
     * @param historicalBundlePaths optional older Doctor bundles inside the MCP workspace
     * @param outputDirectory output directory inside the MCP workspace
     * @param includeScreenshots explicit approval to retain local screenshot evidence
     * @param includePageSnapshots explicit approval to retain local page-snapshot evidence
     * @param minimumAllureResults minimum populated Allure result count
     * @param repositoryRoot optional repository root inside the MCP workspace
     * @param allowedSourcePaths optional repository-relative source paths approved for AI SOURCE evidence
     * @param useAi whether to request optional provider snippet fallback
     * @param allowLocalAi local provider consent for this request
     * @param allowRemoteAi remote provider consent for this request
     * @param driverVariableName Java driver variable name used in snippets
     * @return deterministic diagnosis, action records, code blocks, report paths, and provider metadata
     */
    @Tool(name = "doctor_analyze_failed_allure",
            description = "analyzes failed Allure results and returns deterministic actions plus copy-paste code blocks")
    public McpAnalysisReport analyzeFailedAllure(
            List<String> allureResultPaths,
            List<String> historicalBundlePaths,
            String outputDirectory,
            boolean includeScreenshots,
            boolean includePageSnapshots,
            int minimumAllureResults,
            String repositoryRoot,
            List<String> allowedSourcePaths,
            boolean useAi,
            boolean allowLocalAi,
            boolean allowRemoteAi,
            String driverVariableName) {
        Path repository = repositoryRoot == null || repositoryRoot.isBlank()
                ? null
                : workspacePolicy.existing(repositoryRoot, "Repository root");
        List<String> sourceAllowlist = repository == null
                ? List.of()
                : workspacePolicy.sourceAllowlist(repository, allowedSourcePaths);
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                workspacePolicy.existingList(allureResultPaths, "Allure result path"),
                workspacePolicy.existingList(historicalBundlePaths, "Historical Doctor bundle path"),
                List.of(workspacePolicy.root()),
                outputDirectory == null || outputDirectory.isBlank()
                        ? workspacePolicy.output("target/shaft-doctor", "Doctor output directory")
                        : workspacePolicy.output(outputDirectory, "Doctor output directory"),
                includeScreenshots,
                includePageSnapshots,
                minimumAllureResults <= 0 ? 1 : minimumAllureResults,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);
        DoctorAnalysisResult result = new DoctorAnalyzer().analyze(request);
        return remediationService.build(
                result,
                repository,
                sourceAllowlist,
                useAi,
                approvalPolicy(allowLocalAi, allowRemoteAi, !sourceAllowlist.isEmpty()),
                driverVariableName);
    }

    /**
     * Rebuilds MCP remediation actions and code blocks from an existing Doctor JSON report.
     *
     * @param jsonReportPath Doctor JSON report path inside the MCP workspace
     * @param repositoryRoot optional repository root inside the MCP workspace
     * @param allowedSourcePaths optional repository-relative source paths approved for AI SOURCE evidence
     * @param useAi whether to request optional provider snippet fallback
     * @param allowLocalAi local provider consent for this request
     * @param allowRemoteAi remote provider consent for this request
     * @param driverVariableName Java driver variable name used in snippets
     * @return deterministic diagnosis, action records, code blocks, report paths, and provider metadata
     */
    @Tool(name = "doctor_suggest_fix",
            description = "returns copy-paste remediation code blocks from an existing SHAFT Doctor report")
    public McpAnalysisReport suggestFix(
            String jsonReportPath,
            String repositoryRoot,
            List<String> allowedSourcePaths,
            boolean useAi,
            boolean allowLocalAi,
            boolean allowRemoteAi,
            String driverVariableName) {
        Path report = workspacePolicy.existing(jsonReportPath, "Doctor JSON report");
        Path repository = repositoryRoot == null || repositoryRoot.isBlank()
                ? null
                : workspacePolicy.existing(repositoryRoot, "Repository root");
        List<String> sourceAllowlist = repository == null
                ? List.of()
                : workspacePolicy.sourceAllowlist(repository, allowedSourcePaths);
        DoctorAnalysisResult result = readReport(report);
        return remediationService.build(
                result,
                repository,
                sourceAllowlist,
                useAi,
                approvalPolicy(allowLocalAi, allowRemoteAi, !sourceAllowlist.isEmpty()),
                driverVariableName);
    }

    /**
     * Analyzes explicitly allowed local evidence and optionally appends a configured provider advisory.
     *
     * @param inputPaths explicit evidence files or directories
     * @param historicalBundlePaths optional older Doctor bundle files
     * @param allowedRoots roots that all readable inputs must remain within
     * @param outputDirectory report destination inside an allowed root
     * @param includeScreenshots explicit screenshot retention approval
     * @param includePageSnapshots explicit page-source or snapshot retention approval
     * @param minimumAllureResults minimum valid Allure result count expected
     * @return deterministic diagnosis and local report paths
     */
    public DoctorAnalysisSummary analyze(
            List<String> inputPaths,
            List<String> historicalBundlePaths,
            List<String> allowedRoots,
            String outputDirectory,
            boolean includeScreenshots,
            boolean includePageSnapshots,
            int minimumAllureResults) {
        List<Path> inputs = paths(inputPaths);
        List<Path> history = paths(historicalBundlePaths);
        List<Path> roots = paths(allowedRoots);
        Path output = outputDirectory == null || outputDirectory.isBlank()
                ? Path.of("target", "shaft-doctor")
                : Path.of(outputDirectory);
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                inputs,
                history,
                roots,
                output,
                includeScreenshots,
                includePageSnapshots,
                minimumAllureResults <= 0 ? 1 : minimumAllureResults,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);
        DoctorAnalyzer analyzer = new DoctorAnalyzer();
        DoctorAnalysisResult result;
        PilotConfiguration configuration = currentConfiguration();
        if (configuration != null && configuration.enabled()) {
            result = analyzer.analyzeWithAi(
                    request,
                    DoctorAiAnalysisRequest.defaults(configuration.approvalPolicy()))
                    .deterministic();
        } else {
            result = analyzer.analyze(request);
        }
        return DoctorAnalysisSummary.from(result);
    }

    /**
     * Creates a repair proposal in an isolated worktree without writing to GitHub.
     *
     * @param repositoryRoot approved Git repository root
     * @param baseSha exact approved base commit
     * @param diagnosisPath Doctor diagnosis or combined report JSON
     * @param evidenceBundlePath optional evidence bundle
     * @param issueReference linked issue or session
     * @param allowedPaths approved repository-relative repair scope
     * @param patches reviewed structured patches
     * @param validationCommands tokenized Maven validation commands
     * @param networkValidationApproved whether Maven validation may use the network
     * @param outputDirectory proposal manifest directory
     * @param useAi whether to request an optional provider-generated patch instead
     * @return complete proposal and optional provider status
     */
    public DoctorRepairProposalResult proposeFix(
            String repositoryRoot,
            String baseSha,
            String diagnosisPath,
            String evidenceBundlePath,
            String issueReference,
            List<String> allowedPaths,
            List<DoctorRepairRequest.FilePatch> patches,
            List<List<String>> validationCommands,
            boolean networkValidationApproved,
            String outputDirectory,
            boolean useAi) {
        List<DoctorRepairRequest.FilePatch> resolvedPatches =
                patches == null ? List.of() : List.copyOf(patches);
        DoctorRepairPatchResult providerPatch = null;
        if (useAi) {
            PilotConfiguration configuration = currentConfiguration();
            providerPatch = new DoctorRepairAiService().generate(
                    Path.of(repositoryRoot),
                    readDiagnosis(Path.of(diagnosisPath)),
                    allowedPaths,
                    DoctorRepairAiRequest.defaults(configuration == null
                            ? com.shaft.pilot.ai.ApprovalPolicy.denyAll()
                            : configuration.approvalPolicy()));
            if (!providerPatch.successful()) {
                return new DoctorRepairProposalResult(null, providerPatch);
            }
            resolvedPatches = providerPatch.patches();
        }
        DoctorRepairRequest request = new DoctorRepairRequest(
                Path.of(repositoryRoot),
                baseSha,
                Path.of(diagnosisPath),
                evidenceBundlePath == null || evidenceBundlePath.isBlank()
                        ? null : Path.of(evidenceBundlePath),
                issueReference,
                allowedPaths,
                resolvedPatches,
                validationCommands == null ? List.of() : validationCommands.stream()
                        .map(DoctorRepairRequest.ValidationCommand::new)
                        .toList(),
                networkValidationApproved,
                outputDirectory == null || outputDirectory.isBlank()
                        ? Path.of("target", "shaft-doctor", "repairs")
                        : Path.of(outputDirectory),
                DoctorRepairRequest.DEFAULT_MAX_PATCH_BYTES,
                DoctorRepairRequest.DEFAULT_COMMAND_TIMEOUT);
        return new DoctorRepairProposalResult(
                new DoctorRepairService().propose(request), providerPatch);
    }

    /**
     * Creates a proposal-only locator patch from a verified SHAFT Heal report.
     *
     * @param repositoryRoot approved Git repository root
     * @param healingReportPath verified SHAFT Heal report JSON
     * @param sourcePath repository-relative Java source file
     * @param sourcePatchConsent explicit proposal consent
     * @param outputDirectory proposal artifact directory
     * @return reviewable locator proposal and structured Doctor patch
     */
    @Tool(name = "doctor_propose_healed_locator",
            description = "maps a verified SHAFT Heal report to one reviewable locator patch"
                    + " without editing source or publishing")
    public HealingLocatorProposal proposeHealedLocator(
            String repositoryRoot,
            String healingReportPath,
            String sourcePath,
            boolean sourcePatchConsent,
            String outputDirectory) {
        return new HealingLocatorProposalService().propose(new HealingLocatorProposalRequest(
                Path.of(repositoryRoot),
                Path.of(healingReportPath),
                sourcePath,
                sourcePatchConsent,
                outputDirectory == null || outputDirectory.isBlank()
                        ? Path.of("target", "shaft-doctor", "healing-proposals")
                        : Path.of(outputDirectory)));
    }

    /**
     * Publishes an approved proposal as a draft pull request only.
     *
     * @param manifestPath persisted proposal manifest
     * @param approved explicit publication approval
     * @param approvalToken exact proposal token
     * @param overrideFailedValidation explicit failed-validation override
     * @param overrideRationale required override rationale
     * @param title optional draft pull-request title
     * @return draft publication details
     */
    public RepairPublicationResult publishDraftPr(
            String manifestPath,
            boolean approved,
            String approvalToken,
            boolean overrideFailedValidation,
            String overrideRationale,
            String title) {
        return new DoctorRepairService().publishDraft(new DoctorRepairPublicationRequest(
                Path.of(manifestPath),
                approved,
                approvalToken,
                overrideFailedValidation,
                overrideRationale,
                title));
    }

    private static List<Path> paths(List<String> values) {
        return values == null ? List.of() : values.stream().map(Path::of).toList();
    }

    private static ApprovalPolicy approvalPolicy(boolean allowLocalAi, boolean allowRemoteAi, boolean sourceEvidence) {
        EnumSet<EvidenceCategory> categories = EnumSet.of(EvidenceCategory.TEXT);
        if (sourceEvidence) {
            categories.add(EvidenceCategory.SOURCE);
        }
        return new ApprovalPolicy(allowLocalAi, allowRemoteAi, categories);
    }

    private static PilotConfiguration currentConfiguration() {
        try {
            return PilotConfiguration.current();
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    private static Diagnosis readDiagnosis(Path path) {
        try {
            var root = JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
            return JSON.treeToValue(root.has("diagnosis") ? root.path("diagnosis") : root, Diagnosis.class);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor diagnosis could not be read.", exception);
        }
    }

    private static DoctorAnalysisResult readReport(Path path) {
        try {
            var root = JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
            EvidenceBundle bundle = JSON.treeToValue(root.path("bundle"), EvidenceBundle.class);
            Diagnosis diagnosis = JSON.treeToValue(root.path("diagnosis"), Diagnosis.class);
            Path parent = path.toAbsolutePath().normalize().getParent();
            String bundlePath = parent == null
                    ? ""
                    : parent.resolve("doctor-evidence.json").toString();
            return new DoctorAnalysisResult(
                    bundle,
                    diagnosis,
                    bundlePath,
                    path.toString(),
                    parent == null ? "" : parent.resolve("doctor-report.md").toString());
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor report could not be read.", exception);
        }
    }
}
