package io.github.shafthq.SHAFT_MCP;

import com.shaft.doctor.DoctorAiAnalysisRequest;
import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.DoctorAnalyzer;
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
import com.shaft.doctor.repair.RepairPublicationResult;
import com.shaft.pilot.config.PilotConfiguration;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * MCP adapter for deterministic SHAFT Doctor analysis with optional configured provider advisory.
 */
@Service
public class DoctorService {
    private static final ObjectMapper JSON = new ObjectMapper();

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
    @Tool(name = "doctor_analyze",
            description = "analyzes allowlisted SHAFT evidence with deterministic cited rules"
                    + " and an optional separately identified provider advisory")
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
    @Tool(name = "doctor_propose_fix",
            description = "creates and validates an isolated reviewable repair diff without GitHub writes")
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
    @Tool(name = "doctor_publish_draft_pr",
            description = "publishes a reviewed Doctor proposal as draft after separate explicit approval")
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
}
