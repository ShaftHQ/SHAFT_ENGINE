package com.shaft.ai.local;

import com.shaft.driver.SHAFT;

import java.nio.file.Path;
import java.io.IOException;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Public batteries-included lifecycle boundary for SHAFT-managed local inference. */
public final class ManagedLocalAiService {
    private static final String AUTOMATIC_MODEL = "auto";

    private final Supplier<Settings> settings;
    private final ManagedLocalAiHardware.HostAccess host;
    private final Supplier<ManagedLocalAiManifest> manifests;

    /** Creates a service backed by the effective SHAFT properties for the calling thread. */
    public ManagedLocalAiService() {
        this(Settings::current, ManagedLocalAiHardware.systemHost(), ManagedLocalAiManifest::loadDefault);
    }

    ManagedLocalAiService(Supplier<Settings> settings, ManagedLocalAiHardware.HostAccess host) {
        this(settings, host, ManagedLocalAiManifest::loadDefault);
    }

    ManagedLocalAiService(Supplier<Settings> settings, ManagedLocalAiHardware.HostAccess host,
                          Supplier<ManagedLocalAiManifest> manifests) {
        this.settings = Objects.requireNonNull(settings, "settings");
        this.host = Objects.requireNonNull(host, "host");
        this.manifests = Objects.requireNonNull(manifests, "manifests");
    }

    /** Inspects configuration, hardware, reviewed inventory, and cache state without mutation. */
    public ManagedLocalAiSnapshot inspect() {
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        Path cache = resolveCache(configured.cacheDirectory());
        if (!configured.enabled()) {
            return snapshot(ManagedLocalAiSnapshot.State.DISABLED,
                    "Enable managed local AI to provision a local model.", cache, configured,
                    "", null, null, null, Map.of());
        }

        ManagedLocalAiManifest manifest = Objects.requireNonNull(manifests.get(), "managed local AI manifest");
        ManagedLocalAiHardware.Profile profile = ManagedLocalAiHardware.profile(cache, host);
        if (!profile.runtimeCompatible()) {
            return snapshot(ManagedLocalAiSnapshot.State.UNSUPPORTED,
                    "Use an external local provider or a supported desktop OS, architecture, and ABI.",
                    cache, configured, profile.platform(), profile, manifest, null, Map.of());
        }

        String requested = AUTOMATIC_MODEL.equalsIgnoreCase(configured.model()) ? null : configured.model();
        ManagedLocalAiHardware.Selection selection = ManagedLocalAiHardware.select(manifest, profile, requested);
        Map<String, ManagedLocalAiSnapshot.Model> models = modelInventory(manifest, selection);
        if (selection.selectedModelId() == null) {
            return snapshot(ManagedLocalAiSnapshot.State.EXCLUDED,
                    "No reviewed model safely fits the effective memory, CPU, and free-disk limits.",
                    cache, configured, profile.platform(), profile, manifest, null, models);
        }

        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals(selection.selectedModelId())).findFirst().orElseThrow();
        ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(profile.platform())).findFirst().orElseThrow();
        ManagedLocalAiSnapshot.CacheHealth runtimeStatus = inspectRuntime(cache, manifest, asset);
        ManagedLocalAiSnapshot.CacheHealth modelStatus = inspectModel(cache, model);
        ManagedLocalAiSnapshot.State state;
        String action;
        if (runtimeStatus == ManagedLocalAiSnapshot.CacheHealth.CORRUPT
                || modelStatus == ManagedLocalAiSnapshot.CacheHealth.CORRUPT) {
            state = ManagedLocalAiSnapshot.State.CORRUPT;
            action = "Rebuild the changed managed installation; unknown files will be preserved.";
        } else if (runtimeStatus == ManagedLocalAiSnapshot.CacheHealth.READY
                && modelStatus == ManagedLocalAiSnapshot.CacheHealth.READY) {
            state = ManagedLocalAiSnapshot.State.READY;
            action = "No lifecycle action is required.";
        } else {
            state = ManagedLocalAiSnapshot.State.NOT_PROVISIONED;
            action = "Provision the reviewed managed runtime and model.";
        }
        return snapshot(state, action, cache, configured, profile.platform(), profile, manifest, model, models,
                runtimeStatus, modelStatus);
    }

    static String runtimeInstallationId(ManagedLocalAiManifest manifest, String platform) {
        ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(platform)).findFirst().orElseThrow();
        return manifest.runtime().id() + "-" + manifest.runtime().version() + "-" + platform + "-" + asset.sha256();
    }

    static String modelInstallationId(ManagedLocalAiManifest.ModelManifest model) {
        return "model-" + model.id() + "-" + model.revision() + "-" + model.sha256();
    }

    private static Map<String, ManagedLocalAiSnapshot.Model> modelInventory(
            ManagedLocalAiManifest manifest, ManagedLocalAiHardware.Selection selection) {
        Map<String, ManagedLocalAiSnapshot.Model> inventory = new LinkedHashMap<>();
        for (ManagedLocalAiManifest.ModelManifest model : manifest.models()) {
            ManagedLocalAiHardware.ModelEvaluation evaluation = selection.models().get(model.id());
            inventory.put(model.id(), new ManagedLocalAiSnapshot.Model(model.displayName(), model.tier(),
                    model.license(), model.revision(), model.file(), model.sha256(), model.automatic(),
                    evaluation.eligible(), evaluation.reasons(),
                    evaluation.requiredDiskBytes(), model.size()));
        }
        return inventory;
    }

    private static ManagedLocalAiSnapshot snapshot(ManagedLocalAiSnapshot.State state, String action, Path cache,
                                                    Settings settings, String platform,
                                                    ManagedLocalAiHardware.Profile profile,
                                                    ManagedLocalAiManifest manifest,
                                                    ManagedLocalAiManifest.ModelManifest model,
                                                    Map<String, ManagedLocalAiSnapshot.Model> models) {
        return snapshot(state, action, cache, settings, platform, profile, manifest, model, models,
                ManagedLocalAiSnapshot.CacheHealth.NOT_APPLICABLE,
                ManagedLocalAiSnapshot.CacheHealth.NOT_APPLICABLE);
    }

    private static ManagedLocalAiSnapshot snapshot(ManagedLocalAiSnapshot.State state, String action, Path cache,
                                                    Settings settings, String platform,
                                                    ManagedLocalAiHardware.Profile profile,
                                                    ManagedLocalAiManifest manifest,
                                                    ManagedLocalAiManifest.ModelManifest model,
                                                    Map<String, ManagedLocalAiSnapshot.Model> models,
                                                    ManagedLocalAiSnapshot.CacheHealth runtimeHealth,
                                                    ManagedLocalAiSnapshot.CacheHealth modelHealth) {
        ManagedLocalAiManifest.RuntimeAsset asset = manifest == null ? null : manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(platform)).findFirst().orElse(null);
        return new ManagedLocalAiSnapshot(state, action, cache, settings.enabled(),
                settings.transparentProvisioning(), settings.model(), model == null ? null : model.id(), platform,
                manifest == null ? "" : manifest.runtime().id(),
                manifest == null ? "" : manifest.runtime().version(),
                manifest == null ? "" : manifest.runtime().license(), asset == null ? "" : asset.file(),
                asset == null ? "" : asset.sha256(), asset == null ? "" : asset.executable(),
                asset == null ? 0 : asset.size(), runtimeHealth, modelHealth, ManagedLocalAiSnapshot.Phase.IDLE,
                0, 0, profile == null ? 0 : profile.effectiveMemoryBytes(),
                profile == null ? 0 : profile.cpuCount(), profile == null ? 0 : profile.freeDiskBytes(), models);
    }

    private static ManagedLocalAiSnapshot.CacheHealth inspectRuntime(Path cache,
                                                                      ManagedLocalAiManifest manifest,
                                                                      ManagedLocalAiManifest.RuntimeAsset asset) {
        String id = runtimeInstallationId(manifest, asset.platform());
        return inspectInstallation(cache, id, installation -> {
            requireCanonicalFile(cache, installation, asset.file(), asset.size(), asset.sha256());
            requireOwnedNamedFile(cache, installation, asset.executable());
        });
    }

    private static ManagedLocalAiSnapshot.CacheHealth inspectModel(Path cache,
                                                                    ManagedLocalAiManifest.ModelManifest model) {
        return inspectInstallation(cache, modelInstallationId(model), installation ->
                requireCanonicalFile(cache, installation, model.file(), model.size(), model.sha256()));
    }

    private static ManagedLocalAiSnapshot.CacheHealth inspectInstallation(Path cache, String id,
                                                                           InstallationCheck check) {
        try {
            if (!ManagedLocalAiCache.ownsInstallation(cache, id)) {
                return ManagedLocalAiSnapshot.CacheHealth.MISSING;
            }
            ManagedLocalAiCache.Installation installation = ManagedLocalAiCache.verify(cache, id);
            check.verify(installation);
            return ManagedLocalAiSnapshot.CacheHealth.READY;
        } catch (IOException | IllegalStateException failure) {
            try {
                return ManagedLocalAiCache.ownsInstallation(cache, id)
                        ? ManagedLocalAiSnapshot.CacheHealth.CORRUPT
                        : ManagedLocalAiSnapshot.CacheHealth.MISSING;
            } catch (IOException | IllegalStateException unreadable) {
                return ManagedLocalAiSnapshot.CacheHealth.CORRUPT;
            }
        }
    }

    private static void requireCanonicalFile(Path cache, ManagedLocalAiCache.Installation installation,
                                             String name, long size, String digest) throws IOException {
        Path file = requireOwnedNamedFile(cache, installation, name);
        if (Files.size(file) != size || !sha256(file).equals(digest)) {
            throw new IllegalStateException("Managed local AI artifact does not match the reviewed manifest.");
        }
    }

    private static Path requireOwnedNamedFile(Path cache, ManagedLocalAiCache.Installation installation,
                                              String name) throws IOException {
        Path match = null;
        for (ManagedLocalAiCache.OwnedFile file : installation.files()) {
            Path candidate = cache.resolve(file.path()).toAbsolutePath().normalize();
            if (candidate.getFileName().toString().equals(name)) {
                if (match != null) {
                    throw new IllegalStateException("Managed local AI installation has duplicate artifact names.");
                }
                match = ManagedLocalAiCache.verifyOwnedFile(cache, candidate);
            }
        }
        if (match == null) {
            throw new IllegalStateException("Managed local AI installation is missing a reviewed artifact.");
        }
        return match;
    }

    private static String sha256(Path path) throws IOException {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (var input = Files.newInputStream(path)) {
                byte[] buffer = new byte[64 * 1024];
                int count;
                while ((count = input.read(buffer)) != -1) {
                    digest.update(buffer, 0, count);
                }
            }
            return HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("JDK SHA-256 support is unavailable.", impossible);
        }
    }

    @FunctionalInterface
    private interface InstallationCheck {
        void verify(ManagedLocalAiCache.Installation installation) throws IOException;
    }

    private static Path resolveCache(String configured) {
        if (configured != null && !configured.isBlank()) {
            return Path.of(configured).toAbsolutePath().normalize();
        }
        String userHome = System.getProperty("user.home", "").trim();
        if (userHome.isEmpty()) {
            throw new IllegalStateException("user.home is required for the managed local AI cache.");
        }
        return Path.of(userHome, ".shaft", "local-ai").toAbsolutePath().normalize();
    }

    record Settings(boolean enabled, boolean transparentProvisioning, String model, String cacheDirectory) {
        Settings {
            if (model == null || model.isBlank() || cacheDirectory == null) {
                throw new IllegalArgumentException("Invalid managed local AI settings.");
            }
        }

        static Settings current() {
            return new Settings(SHAFT.Properties.managedLocalAi.enabled(),
                    SHAFT.Properties.managedLocalAi.transparentProvisioning(),
                    SHAFT.Properties.managedLocalAi.model(), SHAFT.Properties.managedLocalAi.cacheDirectory());
        }
    }
}
