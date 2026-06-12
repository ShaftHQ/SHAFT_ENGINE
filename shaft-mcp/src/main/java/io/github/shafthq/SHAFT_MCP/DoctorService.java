package io.github.shafthq.SHAFT_MCP;

import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.DoctorAnalyzer;
import com.shaft.doctor.model.DoctorAnalysisSummary;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.nio.file.Path;
import java.util.List;

/**
 * MCP adapter for offline deterministic SHAFT Doctor analysis.
 */
@Service
public class DoctorService {
    /**
     * Analyzes explicitly allowed local evidence without AI or network access.
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
            description = "analyzes allowlisted local SHAFT evidence offline with deterministic cited rules")
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
        return DoctorAnalysisSummary.from(new DoctorAnalyzer().analyze(new DoctorAnalysisRequest(
                inputs,
                history,
                roots,
                output,
                includeScreenshots,
                includePageSnapshots,
                minimumAllureResults <= 0 ? 1 : minimumAllureResults,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES)));
    }

    private static List<Path> paths(List<String> values) {
        return values == null ? List.of() : values.stream().map(Path::of).toList();
    }
}
