package com.shaft.mcp;

import com.shaft.ai.local.ManagedLocalAiSnapshot;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/** Privacy-safe MCP projection of the shared managed local AI lifecycle snapshot. */
public record McpManagedLocalAiStatus(
        String state,
        String action,
        boolean enabled,
        boolean transparentProvisioning,
        String storageClass,
        String requestedModelId,
        String selectedModelId,
        String runtimeId,
        String runtimeVersion,
        String runtimeLicense,
        String runtimeCacheHealth,
        String modelCacheHealth,
        String phase,
        long completedBytes,
        long totalBytes,
        List<String> eligibleModels,
        Map<String, List<String>> modelExclusions) {

    public McpManagedLocalAiStatus {
        Objects.requireNonNull(state, "state");
        Objects.requireNonNull(action, "action");
        Objects.requireNonNull(storageClass, "storageClass");
        Objects.requireNonNull(requestedModelId, "requestedModelId");
        Objects.requireNonNull(runtimeId, "runtimeId");
        Objects.requireNonNull(runtimeVersion, "runtimeVersion");
        Objects.requireNonNull(runtimeLicense, "runtimeLicense");
        Objects.requireNonNull(runtimeCacheHealth, "runtimeCacheHealth");
        Objects.requireNonNull(modelCacheHealth, "modelCacheHealth");
        Objects.requireNonNull(phase, "phase");
        eligibleModels = List.copyOf(eligibleModels);
        LinkedHashMap<String, List<String>> exclusions = new LinkedHashMap<>();
        modelExclusions.forEach((model, reasons) -> exclusions.put(model, List.copyOf(reasons)));
        modelExclusions = Map.copyOf(exclusions);
    }

    static McpManagedLocalAiStatus from(ManagedLocalAiSnapshot snapshot) {
        Objects.requireNonNull(snapshot, "snapshot");
        List<String> eligible = snapshot.models().entrySet().stream()
                .filter(entry -> entry.getValue().eligible())
                .map(Map.Entry::getKey)
                .sorted()
                .toList();
        LinkedHashMap<String, List<String>> exclusions = new LinkedHashMap<>();
        snapshot.models().entrySet().stream()
                .filter(entry -> !entry.getValue().eligible())
                .sorted(Map.Entry.comparingByKey())
                .forEach(entry -> exclusions.put(entry.getKey(), entry.getValue().reasons()));
        return new McpManagedLocalAiStatus(snapshot.state().name(), snapshot.action(), snapshot.enabled(),
                snapshot.transparentProvisioning(), "SHAFT_USER_CACHE", snapshot.requestedModelId(),
                snapshot.selectedModelId(), snapshot.runtimeId(), snapshot.runtimeVersion(),
                snapshot.runtimeLicense(), snapshot.runtimeCacheHealth().name(), snapshot.modelCacheHealth().name(),
                snapshot.phase().name(), snapshot.completedBytes(), snapshot.totalBytes(), eligible, exclusions);
    }
}
