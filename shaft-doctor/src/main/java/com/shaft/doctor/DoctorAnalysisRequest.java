package com.shaft.doctor;

import java.nio.file.Path;
import java.util.List;

/**
 * Explicit local input and privacy policy for one deterministic analysis.
 *
 * @param inputPaths explicit evidence paths or existing bundle JSON files
 * @param historicalBundlePaths optional older bundle JSON files
 * @param allowedRoots roots that every input and referenced attachment must remain within
 * @param outputDirectory report and portable bundle destination
 * @param includeScreenshots whether screenshot bytes may be retained
 * @param includePageSnapshots whether page source or snapshot text may be retained
 * @param minimumAllureResults minimum valid Allure result count expected from the run
 * @param maxItemBytes maximum retained bytes per evidence item
 * @param maxBundleBytes maximum retained bytes across the bundle
 */
public record DoctorAnalysisRequest(
        List<Path> inputPaths,
        List<Path> historicalBundlePaths,
        List<Path> allowedRoots,
        Path outputDirectory,
        boolean includeScreenshots,
        boolean includePageSnapshots,
        int minimumAllureResults,
        long maxItemBytes,
        long maxBundleBytes) {
    /**
     * Default maximum retained size for one evidence item.
     */
    public static final long DEFAULT_MAX_ITEM_BYTES = 262_144;
    /**
     * Default maximum retained size for an evidence bundle.
     */
    public static final long DEFAULT_MAX_BUNDLE_BYTES = 8_388_608;

    /**
     * Creates a validated analysis request.
     */
    public DoctorAnalysisRequest {
        inputPaths = inputPaths == null ? List.of() : inputPaths.stream()
                .map(path -> path.toAbsolutePath().normalize()).toList();
        historicalBundlePaths = historicalBundlePaths == null ? List.of() : historicalBundlePaths.stream()
                .map(path -> path.toAbsolutePath().normalize()).toList();
        allowedRoots = allowedRoots == null ? List.of() : allowedRoots.stream()
                .map(path -> path.toAbsolutePath().normalize()).distinct().toList();
        if (inputPaths.isEmpty()) {
            throw new IllegalArgumentException("At least one explicit Doctor input path is required.");
        }
        if (allowedRoots.isEmpty()) {
            throw new IllegalArgumentException("At least one explicit Doctor allowed root is required.");
        }
        if (outputDirectory == null) {
            throw new IllegalArgumentException("Doctor output directory is required.");
        }
        outputDirectory = outputDirectory.toAbsolutePath().normalize();
        if (allowedRoots.stream().noneMatch(outputDirectory::startsWith)) {
            throw new IllegalArgumentException("Doctor output directory must be inside an explicit allowed root.");
        }
        if (minimumAllureResults <= 0) {
            throw new IllegalArgumentException("Doctor minimum Allure result count must be positive.");
        }
        if (maxItemBytes <= 0 || maxItemBytes > Integer.MAX_VALUE
                || maxBundleBytes <= 0 || maxItemBytes > maxBundleBytes) {
            throw new IllegalArgumentException("Doctor artifact limits must be positive and internally consistent.");
        }
    }

    /**
     * Creates a request with conservative default size limits.
     *
     * @param inputPaths explicit evidence paths
     * @param allowedRoots allowed roots
     * @param outputDirectory output directory
     * @return default request
     */
    public static DoctorAnalysisRequest defaults(
            List<Path> inputPaths,
            List<Path> allowedRoots,
            Path outputDirectory) {
        return new DoctorAnalysisRequest(inputPaths, List.of(), allowedRoots, outputDirectory,
                false, false, 1, DEFAULT_MAX_ITEM_BYTES, DEFAULT_MAX_BUNDLE_BYTES);
    }
}
