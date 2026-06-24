package com.shaft.mcp;

/**
 * Diagnostic details for one local mobile toolchain dependency.
 *
 * @param dependencyId stable machine-readable dependency identifier
 * @param available whether the dependency is ready for use
 * @param detectedPath resolved executable or installation path when available
 * @param detectedVersion detected version when the tool reports one
 * @param failureCause human-readable explanation when the dependency is unavailable
 * @param repairGuidance command or manual action that can make the dependency available
 */
public record McpMobileToolchainDiagnostic(
        String dependencyId,
        boolean available,
        String detectedPath,
        String detectedVersion,
        String failureCause,
        String repairGuidance) {
    /**
     * Creates an immutable dependency diagnostic.
     */
    public McpMobileToolchainDiagnostic {
        dependencyId = text(dependencyId);
        detectedPath = text(detectedPath);
        detectedVersion = text(detectedVersion);
        failureCause = text(failureCause);
        repairGuidance = text(repairGuidance);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
