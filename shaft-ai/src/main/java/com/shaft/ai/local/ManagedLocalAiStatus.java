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
        String inventory = reviewedInventory(osName, architecture, abi, abiVersion);
        if (!enabled) {
            return new ManagedLocalAiStatus(State.DISABLED,
                    "Enable managed local AI to provision a local model. " + inventory);
        }
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        if (!manifest.supportsRuntime(osName, architecture, abi, abiVersion)) {
            return new ManagedLocalAiStatus(State.UNSUPPORTED,
                    "Use an external local provider or a supported desktop OS, architecture, and ABI. " + inventory);
        }
        if (installationId == null || installationId.isBlank()) {
            return new ManagedLocalAiStatus(State.NOT_PROVISIONED,
                    "Provision the reviewed managed runtime and model explicitly. " + inventory);
        }
        try {
            ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.verify(cache, installationId);
            if (!matchesReviewedPin(installed, manifest)) {
                return new ManagedLocalAiStatus(State.CORRUPT,
                        "Rebuild the changed managed installation; unknown files will be preserved. " + inventory);
            }
            return new ManagedLocalAiStatus(State.READY, "No lifecycle action is required. " + inventory);
        } catch (IllegalStateException missingOrChanged) {
            try {
                if (!ManagedLocalAiCache.ownsInstallation(cache, installationId)) {
                    return new ManagedLocalAiStatus(State.NOT_PROVISIONED,
                            "Provision the reviewed managed runtime and model explicitly. " + inventory);
                }
            } catch (IOException | IllegalStateException unreadable) {
                return new ManagedLocalAiStatus(State.CORRUPT,
                        "Inspect cache permissions and rebuild the managed installation. " + inventory);
            }
            {
                return new ManagedLocalAiStatus(State.CORRUPT,
                        "Rebuild the changed managed installation; unknown files will be preserved. " + inventory);
            }
        } catch (IOException unreadable) {
            return new ManagedLocalAiStatus(State.CORRUPT,
                    "Inspect cache permissions and rebuild the managed installation. " + inventory);
        }
    }

    static String reviewedInventory(ManagedLocalAiSnapshot snapshot) {
        return reviewedInventoryForPlatform(snapshot.platform());
    }

    static String reviewedInventory(String osName, String architecture, String abi, String abiVersion) {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        if (!manifest.supportsRuntime(osName, architecture, abi, abiVersion)) {
            return reviewedInventoryForPlatform("");
        }
        return reviewedInventoryForPlatform(manifest.selectRuntime(osName, architecture, abi, abiVersion).platform());
    }

    private static String reviewedInventoryForPlatform(String platform) {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset runtime = manifest.runtime().assets().stream()
                .filter(asset -> asset.platform().equals(platform)).findFirst().orElse(null);
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals("qwen3-0.6b-q8_0")).findFirst().orElseThrow();
        String runtimeSize = runtime == null ? "" : Long.toString(runtime.size());
        return "revision=" + manifest.runtime().version() + "/" + model.revision()
                + " license=" + manifest.runtime().license() + "/" + model.license()
                + " provenance=github.com/ggml-org/llama.cpp|huggingface.co/" + model.source()
                + " size=" + runtimeSize + "/" + model.size()
                + " storage=SHAFT_USER_CACHE"
                + " resources=" + trimNumber(model.minimumRamGb()) + "/" + model.minimumCpuCount()
                + "/" + trimNumber(model.minimumFreeDiskGb())
                + " update=explicit reviewed plan; pin-bound; no silent float"
                + " cleanup=owner-manifest only; unknown siblings preserved"
                + " fallback=deterministic SHAFT result remains authoritative";
    }

    private static boolean matchesReviewedPin(ManagedLocalAiCache.Installation installed,
                                             ManagedLocalAiManifest manifest) {
        for (ManagedLocalAiManifest.RuntimeAsset asset : manifest.runtime().assets()) {
            if (installed.id().equals(ManagedLocalAiService.runtimeInstallationId(manifest, asset.platform()))) {
                return hasExactOwnedFile(installed, asset.file(), asset.size(), asset.sha256());
            }
        }
        for (ManagedLocalAiManifest.ModelManifest model : manifest.models()) {
            if (installed.id().equals(ManagedLocalAiService.modelInstallationId(model))) {
                return hasExactOwnedFile(installed, model.file(), model.size(), model.sha256());
            }
        }
        return true;
    }

    private static boolean hasExactOwnedFile(ManagedLocalAiCache.Installation installed, String name, long size,
                                             String sha256) {
        return installed.files().stream().filter(file -> {
            Path path = Path.of(file.path());
            return path.getFileName() != null && name.equals(path.getFileName().toString())
                    && file.size() == size && sha256.equals(file.sha256());
        }).count() == 1;
    }

    private static String trimNumber(double value) {
        return value == Math.rint(value) ? Long.toString(Math.round(value)) : Double.toString(value);
    }
}
