package com.shaft.capture.generate.api;

import com.shaft.capture.generate.CaptureGenerationReport;

import java.nio.file.Path;

/**
 * Artifacts and report produced by {@link ApiCaptureGenerator}.
 *
 * @param sourcePath generated source path, or {@code null} if generation failed before a path
 *                   could be determined
 * @param testDataDirectory directory holding generated schema/golden-file artifacts, or
 *                          {@code null} if generation failed before one could be determined
 * @param reportPath generation report path
 * @param report deterministic generation report
 */
public record ApiCaptureGenerationResult(Path sourcePath, Path testDataDirectory, Path reportPath, CaptureGenerationReport report) {
}
