package com.shaft.capture.generate.api;

import java.nio.file.Path;

/**
 * Options for generating a {@code SHAFT.API} test class from a recorded session.
 *
 * @param sessionPath path to the recorded {@code CaptureSession} JSON file
 * @param outputDirectory project root the generated source/test-data/report are written under
 * @param packageName generated class package
 * @param className generated class name; blank derives one deterministically from the session ID
 * @param style how transactions are grouped into test methods
 * @param validationDepth how thoroughly each response is validated
 * @param compile whether to compile the generated source
 * @param replay whether to replay the compiled class (ignored unless {@code compile} is also true;
 *               off by default at the caller level since replaying non-idempotent HTTP methods
 *               against a live service is unsafe to do automatically)
 * @param overwrite whether existing output files may be overwritten
 */
public record ApiCaptureGenerationRequest(
        Path sessionPath,
        Path outputDirectory,
        String packageName,
        String className,
        ApiCodegenStyle style,
        ApiValidationDepth validationDepth,
        boolean compile,
        boolean replay,
        boolean overwrite) {
    public ApiCaptureGenerationRequest {
        packageName = packageName == null || packageName.isBlank() ? "tests.generated" : packageName;
        className = className == null ? "" : className.trim();
        style = style == null ? ApiCodegenStyle.SCENARIO : style;
        validationDepth = validationDepth == null ? ApiValidationDepth.SCHEMA : validationDepth;
    }
}
