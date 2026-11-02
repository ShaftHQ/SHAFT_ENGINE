package com.shaft.mcp;

import com.shaft.pilot.ai.AiModelDiscovery;
import java.util.List;

/** Safe model-discovery result for an Autobot provider. */
public record AutobotProviderModels(
        String schemaVersion,
        String provider,
        String state,
        List<String> modelIds,
        List<String> warnings) {
    /** Creates an immutable, response-safe model-discovery result. */
    public AutobotProviderModels {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
        provider = provider == null ? "" : provider.trim();
        state = state == null || state.isBlank() ? "FAILED" : state.trim();
        modelIds = modelIds == null ? List.of() : modelIds.stream().filter(AiModelDiscovery::isSafeModelId)
                .map(String::trim).distinct().sorted().toList();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
