package com.shaft.ai.local;

import java.nio.file.Path;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/** Immutable, credential-free view of the managed local AI lifecycle. */
public record ManagedLocalAiSnapshot(
        State state,
        String action,
        Path cacheDirectory,
        boolean enabled,
        boolean transparentProvisioning,
        String requestedModelId,
        String selectedModelId,
        String platform,
        String runtimeId,
        String runtimeVersion,
        String runtimeLicense,
        String runtimeAssetFile,
        String runtimeAssetSha256,
        String runtimeExecutable,
        long runtimeAssetBytes,
        CacheHealth runtimeCacheHealth,
        CacheHealth modelCacheHealth,
        Phase phase,
        long completedBytes,
        long totalBytes,
        long effectiveMemoryBytes,
        int cpuCount,
        long freeDiskBytes,
        Map<String, Model> models) {

    public ManagedLocalAiSnapshot {
        if (state == null || action == null || action.isBlank() || cacheDirectory == null
                || !cacheDirectory.isAbsolute() || requestedModelId == null || platform == null
                || runtimeId == null || runtimeVersion == null || runtimeLicense == null
                || runtimeAssetFile == null || runtimeAssetSha256 == null || runtimeExecutable == null
                || runtimeAssetBytes < 0 || runtimeCacheHealth == null || modelCacheHealth == null
                || phase == null || completedBytes < 0 || totalBytes < completedBytes || effectiveMemoryBytes < 0
                || cpuCount < 0 || freeDiskBytes < 0 || models == null) {
            throw new IllegalArgumentException("Invalid managed local AI lifecycle snapshot.");
        }
        cacheDirectory = cacheDirectory.normalize();
        models = Collections.unmodifiableMap(new LinkedHashMap<>(models));
    }

    /** Stable lifecycle states shared by Java, Doctor, CLI/MCP, and IDE adapters. */
    public enum State {
        DISABLED,
        UNSUPPORTED,
        EXCLUDED,
        NOT_PROVISIONED,
        READY,
        CORRUPT
    }

    /** Current operation phase. Inspection returns {@link #IDLE}; mutations publish active phases. */
    public enum Phase {
        IDLE,
        DOWNLOADING_RUNTIME,
        EXTRACTING_RUNTIME,
        DOWNLOADING_MODEL,
        ADOPTING,
        LAUNCHING,
        INFERENCE,
        CLEANING
    }

    /** Independent health of each reviewed cache installation. */
    public enum CacheHealth {
        NOT_APPLICABLE,
        MISSING,
        READY,
        CORRUPT
    }

    /** Reviewed model inventory and its host-specific eligibility decision. */
    public record Model(String displayName, String tier, String license, String revision, String file,
                        String sha256, boolean automatic, boolean eligible, List<String> reasons,
                        long requiredDiskBytes, long artifactBytes) {
        public Model {
            if (displayName == null || displayName.isBlank() || tier == null || tier.isBlank()
                    || license == null || license.isBlank() || revision == null || revision.isBlank()
                    || file == null || file.isBlank() || sha256 == null || sha256.isBlank() || reasons == null
                    || requiredDiskBytes < 0 || artifactBytes <= 0) {
                throw new IllegalArgumentException("Invalid managed local AI model snapshot.");
            }
            reasons = List.copyOf(reasons);
        }
    }
}
