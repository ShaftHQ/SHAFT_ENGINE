package com.shaft.ai.local;

import java.io.IOException;
import java.nio.file.Path;

/** Read-only lifecycle state consumed by configuration and Doctor integrations. */
public record ManagedLocalAiStatus(State state, String action) {
    public enum State {
        DISABLED,
        UNSUPPORTED,
        NOT_PROVISIONED,
        READY,
        CORRUPT
    }

    public static ManagedLocalAiStatus inspect(Path cache, boolean enabled, String osName, String architecture,
                                               String abi, String abiVersion, String installationId) {
        if (!enabled) {
            return new ManagedLocalAiStatus(State.DISABLED, "Enable managed local AI to provision a local model.");
        }
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        if (!manifest.supportsRuntime(osName, architecture, abi, abiVersion)) {
            return new ManagedLocalAiStatus(State.UNSUPPORTED,
                    "Use an external local provider or a supported desktop OS, architecture, and ABI.");
        }
        if (installationId == null || installationId.isBlank()) {
            return new ManagedLocalAiStatus(State.NOT_PROVISIONED,
                    "Provision the reviewed managed runtime and model explicitly.");
        }
        try {
            ManagedLocalAiCache.verify(cache, installationId);
            return new ManagedLocalAiStatus(State.READY, "No lifecycle action is required.");
        } catch (IllegalStateException missingOrChanged) {
            try {
                if (!ManagedLocalAiCache.ownsInstallation(cache, installationId)) {
                    return new ManagedLocalAiStatus(State.NOT_PROVISIONED,
                            "Provision the reviewed managed runtime and model explicitly.");
                }
            } catch (IOException | IllegalStateException unreadable) {
                return new ManagedLocalAiStatus(State.CORRUPT,
                        "Inspect cache permissions and rebuild the managed installation.");
            }
            {
                return new ManagedLocalAiStatus(State.CORRUPT,
                        "Rebuild the changed managed installation; unknown files will be preserved.");
            }
        } catch (IOException unreadable) {
            return new ManagedLocalAiStatus(State.CORRUPT,
                    "Inspect cache permissions and rebuild the managed installation.");
        }
    }
}
