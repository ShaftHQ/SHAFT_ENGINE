package com.shaft.ai.local;

import com.shaft.driver.SHAFT;

import java.nio.file.Path;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.time.Duration;
import java.util.UUID;

/** Public batteries-included lifecycle boundary for SHAFT-managed local inference. */
public final class ManagedLocalAiService {
    private static final String AUTOMATIC_MODEL = "auto";

    private final Supplier<Settings> settings;
    private final ManagedLocalAiHardware.HostAccess host;
    private final Supplier<ManagedLocalAiManifest> manifests;
    private final Provisioning provisioning;

    /** Creates a service backed by the effective SHAFT properties for the calling thread. */
    public ManagedLocalAiService() {
        this(Settings::current, ManagedLocalAiHardware.systemHost(), ManagedLocalAiManifest::loadDefault,
                new DefaultProvisioning());
    }

    ManagedLocalAiService(Supplier<Settings> settings, ManagedLocalAiHardware.HostAccess host) {
        this(settings, host, ManagedLocalAiManifest::loadDefault, new DefaultProvisioning());
    }

    ManagedLocalAiService(Supplier<Settings> settings, ManagedLocalAiHardware.HostAccess host,
                          Supplier<ManagedLocalAiManifest> manifests) {
        this(settings, host, manifests, new DefaultProvisioning());
    }

    ManagedLocalAiService(Supplier<Settings> settings, ManagedLocalAiHardware.HostAccess host,
                          Supplier<ManagedLocalAiManifest> manifests, Provisioning provisioning) {
        this.settings = Objects.requireNonNull(settings, "settings");
        this.host = Objects.requireNonNull(host, "host");
        this.manifests = Objects.requireNonNull(manifests, "manifests");
        this.provisioning = Objects.requireNonNull(provisioning, "provisioning");
    }

    /** Starts transparent provisioning and reports immutable phase snapshots to the caller. */
    public ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress) {
        Objects.requireNonNull(progress, "progress");
        ManagedLocalAiSnapshot initial = inspect();
        ManagedLocalAiOperation operation = new ManagedLocalAiOperation(initial);
        Thread worker = Thread.ofVirtual().name("shaft-managed-local-ai-provision").start(() -> {
            try {
                if (initial.state() == ManagedLocalAiSnapshot.State.READY) {
                    publish(operation, progress, initial);
                    if (!operation.complete(initial)) {
                        operation.cancelled();
                    }
                    return;
                }
                if (initial.state() != ManagedLocalAiSnapshot.State.NOT_PROVISIONED) {
                    throw new IllegalStateException("Managed local AI cannot be provisioned from state "
                            + initial.state() + ".");
                }
                Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
                ManagedLocalAiManifest manifest = Objects.requireNonNull(manifests.get(), "managed local AI manifest");
                ManagedLocalAiHardware.Profile profile = ManagedLocalAiHardware.profile(initial.cacheDirectory(), host);
                String requested = AUTOMATIC_MODEL.equalsIgnoreCase(configured.model()) ? null : configured.model();
                ManagedLocalAiHardware.Selection selection = ManagedLocalAiHardware.select(manifest, profile, requested);
                ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                        .filter(candidate -> candidate.id().equals(selection.selectedModelId())).findFirst().orElseThrow();
                ProvisionResult provisioned = provisioning.provision(initial.cacheDirectory(), manifest, profile,
                        model, configured,
                        host, operation::isCancelled, phase -> publish(operation, progress,
                                withProgress(initial, phase.phase(), phase.completedBytes(), phase.totalBytes())));
                if (operation.isCancelled()) {
                    rollbackCancellation(initial.cacheDirectory(), configured, provisioned.installationIds());
                    throw new InterruptedException("Managed local AI provisioning was cancelled.");
                }
                ManagedLocalAiSnapshot ready = inspect();
                if (ready.state() != ManagedLocalAiSnapshot.State.READY) {
                    throw new IllegalStateException("Managed local AI provisioning did not produce a ready cache.");
                }
                operation.publish(ready);
                try {
                    progress.accept(ready);
                } catch (RuntimeException observerFailure) {
                    // Lifecycle ownership and rollback must not depend on an observer callback.
                }
                if (!operation.complete(ready)) {
                    rollbackCancellation(initial.cacheDirectory(), configured, provisioned.installationIds());
                    operation.cancelled();
                }
            } catch (InterruptedException cancelled) {
                Thread.currentThread().interrupt();
                operation.cancelled();
            } catch (Exception failure) {
                operation.fail(failure);
            }
        });
        operation.attach(worker);
        return operation;
    }

    /** Removes unchanged SHAFT-owned managed artifacts. Unknown or changed content is preserved. */
    public ManagedLocalAiSnapshot clean() throws Exception {
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        Path cache = resolveCache(configured.cacheDirectory());
        Duration lockTimeout = Duration.ofSeconds(configured.lockTimeoutSeconds());
        ManagedLocalAiProcess.withLaunchExclusion(lockTimeout, () -> {
            ManagedLocalAiProcess.terminateRetainedLaunches();
            ManagedLocalAiCache.withLock(cache, lockTimeout, () -> ManagedLocalAiCache.clean(cache));
        });
        return inspect();
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

    ReadyRuntime readyRuntime() throws IOException {
        ManagedLocalAiSnapshot snapshot = inspect();
        if (snapshot.state() != ManagedLocalAiSnapshot.State.READY || snapshot.selectedModelId() == null) {
            throw new IllegalStateException("Managed local AI is not ready for inference.");
        }
        ManagedLocalAiManifest manifest = Objects.requireNonNull(manifests.get(), "managed local AI manifest");
        ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(snapshot.platform())).findFirst().orElseThrow();
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals(snapshot.selectedModelId())).findFirst().orElseThrow();
        ManagedLocalAiCache.Installation runtimeInstallation = ManagedLocalAiCache.verify(snapshot.cacheDirectory(),
                runtimeInstallationId(manifest, snapshot.platform()));
        ManagedLocalAiCache.Installation modelInstallation = ManagedLocalAiCache.verify(snapshot.cacheDirectory(),
                modelInstallationId(model));
        int launchTimeoutSeconds = SHAFT.Properties.managedLocalAi.launchTimeoutSeconds();
        if (launchTimeoutSeconds <= 0) {
            throw new IllegalStateException("Managed local AI launch timeout must be positive.");
        }
        return new ReadyRuntime(snapshot.cacheDirectory(),
                requireOwnedNamedFile(snapshot.cacheDirectory(), runtimeInstallation, asset.executable()),
                requireOwnedNamedFile(snapshot.cacheDirectory(), modelInstallation, model.file()),
                inferenceLog(snapshot.cacheDirectory()), model.id(),
                Math.max(1, snapshot.cpuCount()), Duration.ofSeconds(launchTimeoutSeconds));
    }

    static Path inferenceLog(Path cache) {
        return cache.toAbsolutePath().normalize().resolve("staging/logs/server-" + UUID.randomUUID() + ".log");
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

    private static ManagedLocalAiSnapshot withProgress(ManagedLocalAiSnapshot base,
                                                        ManagedLocalAiSnapshot.Phase phase,
                                                        long completed, long total) {
        return new ManagedLocalAiSnapshot(base.state(), base.action(), base.cacheDirectory(), base.enabled(),
                base.transparentProvisioning(), base.requestedModelId(), base.selectedModelId(), base.platform(),
                base.runtimeId(), base.runtimeVersion(), base.runtimeLicense(), base.runtimeAssetFile(),
                base.runtimeAssetSha256(), base.runtimeExecutable(), base.runtimeAssetBytes(),
                base.runtimeCacheHealth(), base.modelCacheHealth(), phase, completed, total,
                base.effectiveMemoryBytes(), base.cpuCount(), base.freeDiskBytes(), base.models());
    }

    private static void publish(ManagedLocalAiOperation operation, Consumer<ManagedLocalAiSnapshot> progress,
                                ManagedLocalAiSnapshot snapshot) {
        operation.publish(snapshot);
        try {
            progress.accept(snapshot);
        } catch (RuntimeException observerFailure) {
            // Lifecycle ownership and rollback must not depend on an observer callback.
        }
    }

    private static void rollback(Path cache, Settings settings, java.util.Set<String> installationIds)
            throws Exception {
        if (!installationIds.isEmpty()) {
            boolean interrupted = Thread.interrupted();
            try {
                ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(settings.lockTimeoutSeconds()),
                        () -> ManagedLocalAiCache.clean(cache, installationIds));
            } finally {
                if (interrupted) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

    private static void rollbackCancellation(Path cache, Settings settings,
                                             java.util.Set<String> installationIds) {
        try {
            rollback(cache, settings, installationIds);
        } catch (Exception cleanup) {
            // Cancellation stays primary; incomplete cleanup remains discoverable through cache status/recovery.
        }
    }

    record Settings(boolean enabled, boolean transparentProvisioning, String model, String cacheDirectory,
                    int downloadTimeoutSeconds, int lockTimeoutSeconds) {
        Settings {
            if (model == null || model.isBlank() || cacheDirectory == null
                    || downloadTimeoutSeconds <= 0 || lockTimeoutSeconds <= 0) {
                throw new IllegalArgumentException("Invalid managed local AI settings.");
            }
        }

        Settings(boolean enabled, boolean transparentProvisioning, String model, String cacheDirectory) {
            this(enabled, transparentProvisioning, model, cacheDirectory, 900, 30);
        }

        static Settings current() {
            return new Settings(SHAFT.Properties.managedLocalAi.enabled(),
                    SHAFT.Properties.managedLocalAi.transparentProvisioning(),
                    SHAFT.Properties.managedLocalAi.model(), SHAFT.Properties.managedLocalAi.cacheDirectory(),
                    SHAFT.Properties.managedLocalAi.downloadTimeoutSeconds(),
                    SHAFT.Properties.managedLocalAi.lockTimeoutSeconds());
        }
    }

    interface Provisioning {
        ProvisionResult provision(Path cache, ManagedLocalAiManifest manifest,
                                  ManagedLocalAiHardware.Profile profile,
                                  ManagedLocalAiManifest.ModelManifest model, Settings settings,
                                  ManagedLocalAiHardware.HostAccess host, BooleanSupplier cancelled,
                                  Consumer<Progress> progress) throws Exception;
    }

    record Progress(ManagedLocalAiSnapshot.Phase phase, long completedBytes, long totalBytes) {
    }

    record ProvisionResult(java.util.Set<String> installationIds) {
        ProvisionResult {
            installationIds = java.util.Set.copyOf(installationIds);
        }
    }

    record ReadyRuntime(Path cache, Path executable, Path model, Path log, String alias, int threads,
                        Duration launchTimeout) {
    }

    static final class DefaultProvisioning implements Provisioning {
        private final ArtifactAccess artifacts;

        DefaultProvisioning() {
            this(new DefaultArtifactAccess());
        }

        DefaultProvisioning(ArtifactAccess artifacts) {
            this.artifacts = Objects.requireNonNull(artifacts, "artifacts");
        }

        @Override
        public ProvisionResult provision(Path cache, ManagedLocalAiManifest manifest,
                                         ManagedLocalAiHardware.Profile profile,
                                         ManagedLocalAiManifest.ModelManifest model, Settings settings,
                                         ManagedLocalAiHardware.HostAccess host, BooleanSupplier cancelled,
                                         Consumer<Progress> progress) throws Exception {
            ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                    .filter(candidate -> candidate.platform().equals(profile.platform())).findFirst().orElseThrow();
            long total = Math.addExact(asset.size(), model.size());
            return ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(settings.lockTimeoutSeconds()), () -> {
                ManagedLocalAiHardware.Profile lockedProfile = ManagedLocalAiHardware.profile(cache,
                        host);
                ManagedLocalAiHardware.Selection lockedSelection = ManagedLocalAiHardware.select(manifest,
                        lockedProfile, model.id());
                if (!model.id().equals(lockedSelection.selectedModelId())) {
                    throw new IllegalStateException("Managed local AI resources changed before provisioning.");
                }
                String runtimeId = runtimeInstallationId(manifest, profile.platform());
                String modelId = modelInstallationId(model);
                ManagedLocalAiSnapshot.CacheHealth runtimeHealth = inspectRuntime(cache, manifest, asset);
                ManagedLocalAiSnapshot.CacheHealth modelHealth = inspectModel(cache, model);
                if (runtimeHealth == ManagedLocalAiSnapshot.CacheHealth.CORRUPT
                        || modelHealth == ManagedLocalAiSnapshot.CacheHealth.CORRUPT) {
                    throw new IllegalStateException("Changed managed local AI cache must be rebuilt explicitly.");
                }
                boolean installedRuntime = false;
                boolean installedModel = false;
                try {
                    if (runtimeHealth == ManagedLocalAiSnapshot.CacheHealth.MISSING) {
                        progress.accept(new Progress(ManagedLocalAiSnapshot.Phase.DOWNLOADING_RUNTIME, 0, total));
                        installRuntime(cache, asset, runtimeId, settings, cancelled, progress, total);
                        installedRuntime = true;
                    }
                    if (modelHealth == ManagedLocalAiSnapshot.CacheHealth.MISSING) {
                        progress.accept(new Progress(ManagedLocalAiSnapshot.Phase.DOWNLOADING_MODEL,
                                asset.size(), total));
                        installModel(cache, model, modelId, settings, cancelled);
                        installedModel = true;
                        progress.accept(new Progress(ManagedLocalAiSnapshot.Phase.ADOPTING, total, total));
                    }
                } catch (Exception primary) {
                    boolean interrupted = primary instanceof InterruptedException
                            || cancelled.getAsBoolean() || Thread.interrupted();
                    Thread.interrupted();
                    if (installedRuntime) {
                        try {
                            ManagedLocalAiCache.clean(cache, java.util.Set.of(runtimeId));
                        } catch (Exception rollback) {
                            primary.addSuppressed(rollback);
                        }
                    }
                    if (interrupted) {
                        Thread.currentThread().interrupt();
                    }
                    throw primary;
                }
                java.util.Set<String> installed = new java.util.LinkedHashSet<>();
                if (installedRuntime) installed.add(runtimeId);
                if (installedModel) installed.add(modelId);
                return new ProvisionResult(installed);
            });
        }

        private void installRuntime(Path cache, ManagedLocalAiManifest.RuntimeAsset asset, String id,
                                           Settings settings, BooleanSupplier cancelled, Consumer<Progress> progress,
                                           long total) throws Exception {
            Path staging = cache.resolve("staging");
            Files.createDirectories(staging);
            Path archive = staging.resolve(id + ".archive-" + UUID.randomUUID() + "-" + asset.file());
            Path stage = null;
            List<Path> ownedFiles = new java.util.ArrayList<>();
            List<Path> ownedDirectories = new java.util.ArrayList<>();
            Throwable primary = null;
            try {
                artifacts.download(asset, archive, Duration.ofSeconds(settings.downloadTimeoutSeconds()), cancelled);
                progress.accept(new Progress(ManagedLocalAiSnapshot.Phase.EXTRACTING_RUNTIME, asset.size(), total));
                ManagedLocalAiArtifacts.Extraction extraction = artifacts.extract(
                        archive, staging.resolve(id), cancelled);
                stage = extraction.root();
                ownedFiles.addAll(extraction.files());
                ownedDirectories.addAll(extraction.directories());
                Path archiveInStage = stage.resolve(asset.file());
                Files.createLink(archiveInStage, archive);
                ownedFiles.add(archiveInStage);
                Files.delete(archive);
                progress.accept(new Progress(ManagedLocalAiSnapshot.Phase.ADOPTING, asset.size(), total));
                ManagedLocalAiCache.adopt(cache, id, stage, ownedFiles);
                stage = null;
            } catch (IOException | InterruptedException | RuntimeException failure) {
                primary = failure;
                throw failure;
            } finally {
                cleanupProvisionFiles(archive, stage, ownedFiles, ownedDirectories, primary);
            }
        }

        private void installModel(Path cache, ManagedLocalAiManifest.ModelManifest model, String id,
                                         Settings settings, BooleanSupplier cancelled) throws Exception {
            Path staging = cache.resolve("staging");
            Files.createDirectories(staging);
            Path stage = staging.resolve(id + ".extract-" + UUID.randomUUID());
            Files.createDirectory(stage);
            List<Path> ownedFiles = new java.util.ArrayList<>();
            List<Path> ownedDirectories = List.of(stage);
            Throwable primary = null;
            try {
                Path modelFile = stage.resolve(model.file());
                artifacts.download(model, modelFile, Duration.ofSeconds(settings.downloadTimeoutSeconds()), cancelled);
                ownedFiles.add(modelFile);
                if (cancelled.getAsBoolean()) {
                    throw new InterruptedException("Managed local AI provisioning was cancelled.");
                }
                Path marker = stage.resolve(".shaft-ready");
                Files.writeString(marker, "ready",
                        java.nio.file.StandardOpenOption.CREATE_NEW, java.nio.file.StandardOpenOption.WRITE);
                ownedFiles.add(marker);
                ManagedLocalAiCache.adopt(cache, id, stage, ownedFiles);
                stage = null;
            } catch (IOException | InterruptedException | RuntimeException failure) {
                primary = failure;
                throw failure;
            } finally {
                cleanupProvisionFiles(null, stage, ownedFiles, ownedDirectories, primary);
            }
        }

        private static void cleanupProvisionFiles(Path file, Path tree, List<Path> files,
                                                  List<Path> directories, Throwable primary) throws IOException {
            IOException cleanup = null;
            try {
                if (file != null) {
                    Files.deleteIfExists(file);
                }
            } catch (IOException failure) {
                cleanup = failure;
            }
            try {
                if (tree != null) {
                    Throwable cleanupOwner = primary == null
                            ? new IOException("Managed local AI provisioning cleanup failed.") : primary;
                    ManagedLocalAiArtifacts.cleanupExact(files, directories, cleanupOwner);
                    if (primary == null && cleanupOwner.getSuppressed().length > 0) {
                        throw (IOException) cleanupOwner;
                    }
                }
            } catch (IOException failure) {
                if (cleanup == null) {
                    cleanup = failure;
                } else {
                    cleanup.addSuppressed(failure);
                }
            }
            if (cleanup != null) {
                if (primary != null) {
                    primary.addSuppressed(cleanup);
                } else {
                    throw cleanup;
                }
            }
        }

    }

    interface ArtifactAccess {
        void download(ManagedLocalAiManifest.RuntimeAsset asset, Path target, Duration timeout,
                      BooleanSupplier cancelled) throws IOException, InterruptedException;
        void download(ManagedLocalAiManifest.ModelManifest model, Path target, Duration timeout,
                      BooleanSupplier cancelled) throws IOException, InterruptedException;
        ManagedLocalAiArtifacts.Extraction extract(Path archive, Path destination, BooleanSupplier cancelled)
                throws IOException, InterruptedException;
    }

    private static final class DefaultArtifactAccess implements ArtifactAccess {
        public void download(ManagedLocalAiManifest.RuntimeAsset asset, Path target, Duration timeout,
                             BooleanSupplier cancelled) throws IOException, InterruptedException {
            ManagedLocalAiArtifacts.download(asset, target, timeout, cancelled);
        }
        public void download(ManagedLocalAiManifest.ModelManifest model, Path target, Duration timeout,
                             BooleanSupplier cancelled) throws IOException, InterruptedException {
            ManagedLocalAiArtifacts.download(model, target, timeout, cancelled);
        }
        public ManagedLocalAiArtifacts.Extraction extract(Path archive, Path destination,
                                                           BooleanSupplier cancelled)
                throws IOException, InterruptedException {
            return ManagedLocalAiArtifacts.extractStage(archive, destination, cancelled);
        }
    }
}
