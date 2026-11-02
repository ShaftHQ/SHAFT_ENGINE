package com.shaft.ai.local;

import com.shaft.driver.SHAFT;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.Supplier;

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
        return provision(progress, true);
    }

    ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress, boolean allowDownloads) {
        Objects.requireNonNull(progress, "progress");
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        ManagedLocalAiSnapshot initial = inspect(true, configured);
        ManagedLocalAiSnapshot reviewedInitial = inspect(false, configured);
        ManagedLocalAiOperation operation = new ManagedLocalAiOperation(initial);
        ProvisionContext context = new ProvisionContext(
                configured, initial, reviewedInitial, operation, progress, allowDownloads);
        Thread worker = Thread.ofVirtual().name("shaft-managed-local-ai-provision")
                .start(() -> runProvisioning(context));
        operation.attach(worker);
        return operation;
    }

    private void runProvisioning(ProvisionContext context) {
        ProvisionResult provisioned = null;
        try {
            if (context.reviewed().state() == ManagedLocalAiSnapshot.State.READY) {
                completeExisting(context);
                return;
            }
            ProvisionPlan plan = planProvisioning(context);
            provisioned = provision(context, plan);
            requireActive(context);
            publishProvisioned(context, plan.manifest(), provisioned);
        } catch (InterruptedException cancelled) {
            cancelProvisioning(context, provisioned);
        } catch (Exception failure) {
            failProvisioning(context, provisioned, failure);
        }
    }

    private void completeExisting(ProvisionContext context) throws Exception {
        publish(context.operation(), context.progress(), context.reviewed());
        ManagedLocalAiManifest manifest = Objects.requireNonNull(manifests.get(), "managed local AI manifest");
        if (!publishActivation(context.operation(), context.reviewed(), context.settings(), manifest)) {
            context.operation().cancelled();
        }
    }

    private ProvisionPlan planProvisioning(ProvisionContext context) throws IOException {
        if (context.reviewed().state() != ManagedLocalAiSnapshot.State.NOT_PROVISIONED) {
            throw new IllegalStateException("Managed local AI cannot be provisioned from state "
                    + context.reviewed().state() + ".");
        }
        if (!context.allowDownloads()) {
            throw new IOException("Managed local AI is not ready and offline setup cannot download artifacts.");
        }
        ManagedLocalAiManifest manifest = Objects.requireNonNull(manifests.get(), "managed local AI manifest");
        ManagedLocalAiHardware.Profile profile = ManagedLocalAiHardware.profile(
                context.reviewed().cacheDirectory(), host);
        String requested = AUTOMATIC_MODEL.equalsIgnoreCase(context.settings().model())
                ? null : context.settings().model();
        ManagedLocalAiHardware.Selection selection = ManagedLocalAiHardware.select(manifest, profile, requested);
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals(selection.selectedModelId())).findFirst().orElseThrow();
        return new ProvisionPlan(manifest, profile, model);
    }

    private ProvisionResult provision(ProvisionContext context, ProvisionPlan plan) throws Exception {
        return provisioning.provision(context.reviewed().cacheDirectory(), plan.manifest(),
                plan.profile(), plan.model(), context.settings(), host, context.operation()::isCancelled,
                phase -> publish(context.operation(), context.progress(), withProgress(context.reviewed(),
                        phase.phase(), phase.completedBytes(), phase.totalBytes())));
    }

    private static void requireActive(ProvisionContext context) throws InterruptedException {
        if (context.operation().isCancelled()) {
            throw new InterruptedException("Managed local AI provisioning was cancelled.");
        }
    }

    private void publishProvisioned(ProvisionContext context, ManagedLocalAiManifest manifest,
                                    ProvisionResult provisioned) throws Exception {
        ManagedLocalAiSnapshot ready = inspect(false, context.settings());
        if (ready.state() != ManagedLocalAiSnapshot.State.READY) {
            throw new IllegalStateException("Managed local AI provisioning did not produce a ready cache.");
        }
        context.operation().publish(ready);
        notifyProgress(context.progress(), ready);
        if (!publishActivation(context.operation(), ready, context.settings(), manifest)) {
            rollbackCancellation(context.initial().cacheDirectory(), context.settings(), provisioned.installationIds());
            context.operation().cancelled();
        }
    }

    private static void notifyProgress(Consumer<ManagedLocalAiSnapshot> progress, ManagedLocalAiSnapshot ready) {
        try {
            progress.accept(ready);
        } catch (RuntimeException observerFailure) {
            // Lifecycle ownership and rollback must not depend on an observer callback.
        }
    }

    private static void cancelProvisioning(ProvisionContext context, ProvisionResult provisioned) {
        if (provisioned != null) {
            rollbackCancellation(
                    context.initial().cacheDirectory(), context.settings(), provisioned.installationIds());
        }
        Thread.currentThread().interrupt();
        context.operation().cancelled();
    }

    private static void failProvisioning(ProvisionContext context, ProvisionResult provisioned, Exception failure) {
        if (provisioned != null) {
            try {
                rollback(context.initial().cacheDirectory(), context.settings(), provisioned.installationIds());
            } catch (Exception cleanup) {
                failure.addSuppressed(cleanup);
            }
        }
        context.operation().fail(failure);
    }

    private static boolean publishActivation(ManagedLocalAiOperation operation, ManagedLocalAiSnapshot ready,
                                             Settings configured, ManagedLocalAiManifest manifest) throws Exception {
        boolean published = ManagedLocalAiActivationHistory.publish(ready.cacheDirectory(),
                Duration.ofSeconds(configured.lockTimeoutSeconds()),
                ManagedLocalAiActivationHistory.from(ready, manifest), operation::claimCompletion);
        if (published) {
            operation.finishCompletion(ready);
        }
        return published;
    }

    /** Removes unchanged SHAFT-owned managed artifacts. Unknown or changed content is preserved. */
    public ManagedLocalAiSnapshot clean() throws Exception {
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        Path cache = resolveCache(configured.cacheDirectory());
        Duration lockTimeout = Duration.ofSeconds(configured.lockTimeoutSeconds());
        ManagedLocalAiProcess.withLaunchExclusion(lockTimeout, () -> {
            ManagedLocalAiProcess.terminateRetainedLaunches();
            ManagedLocalAiCache.withLock(cache, lockTimeout, () -> {
                ManagedLocalAiActivationHistory.readVerified(cache);
                return ManagedLocalAiCache.clean(cache, null, () -> { },
                        () -> ManagedLocalAiActivationHistory.clearLocked(cache));
            });
        });
        return inspect();
    }

    /** Removes only unchanged artifacts named by the current reviewed manifest. */
    boolean cleanReviewed() throws Exception {
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        ManagedLocalAiManifest manifest = Objects.requireNonNull(manifests.get(), "managed local AI manifest");
        Path cache = resolveCache(configured.cacheDirectory());
        Duration lockTimeout = Duration.ofSeconds(configured.lockTimeoutSeconds());
        Set<String> reviewed = new LinkedHashSet<>();
        manifest.runtime().assets().forEach(asset -> reviewed.add(runtimeInstallationId(manifest, asset.platform())));
        manifest.models().forEach(model -> reviewed.add(modelInstallationId(model)));
        AtomicReference<ManagedLocalAiCache.CleanResult> result = new AtomicReference<>();
        ManagedLocalAiProcess.withLaunchExclusion(lockTimeout, () -> {
            ManagedLocalAiProcess.terminateRetainedLaunches();
            ManagedLocalAiCache.withLock(cache, lockTimeout, () -> {
                ManagedLocalAiActivationHistory.readVerified(cache);
                ManagedLocalAiCache.CleanResult cleaned = ManagedLocalAiCache.clean(cache, reviewed, () -> { },
                        () -> ManagedLocalAiActivationHistory.clearLocked(cache));
                result.set(cleaned);
                return cleaned;
            });
        });
        return result.get().conflicts().isEmpty();
    }

    /** Inspects configuration, hardware, reviewed inventory, and cache state without mutation. */
    public ManagedLocalAiSnapshot inspect() {
        return inspect(true);
    }

    ManagedLocalAiSnapshot inspectReviewed() {
        return inspect(false);
    }

    private ManagedLocalAiSnapshot inspect(boolean effectiveActivation) {
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        return inspect(effectiveActivation, configured);
    }

    private ManagedLocalAiSnapshot inspect(boolean effectiveActivation, Settings configured) {
        Path cache = resolveCache(configured.cacheDirectory());
        if (!configured.enabled()) {
            return disabledSnapshot(cache, configured);
        }

        ManagedLocalAiManifest manifest = Objects.requireNonNull(manifests.get(), "managed local AI manifest");
        ManagedLocalAiHardware.Profile profile = ManagedLocalAiHardware.profile(cache, host);
        if (effectiveActivation) {
            ManagedLocalAiSnapshot active = inspectActivation(cache, configured);
            if (active != null) {
                return active;
            }
        }
        if (!profile.runtimeCompatible()) {
            return snapshot(ManagedLocalAiSnapshot.State.UNSUPPORTED,
                    "Use an external local provider or a supported desktop OS, architecture, and ABI.",
                    new SnapshotContext(cache, configured, profile.platform(), profile, manifest, null, Map.of()));
        }
        return inspectSelection(cache, configured, manifest, profile);
    }

    private static ManagedLocalAiSnapshot disabledSnapshot(Path cache, Settings configured) {
        return snapshot(ManagedLocalAiSnapshot.State.DISABLED,
                "Enable managed local AI to provision a local model.",
                new SnapshotContext(cache, configured, "", null, null, null, Map.of()));
    }

    private ManagedLocalAiSnapshot inspectActivation(Path cache, Settings configured) {
        try {
            ManagedLocalAiActivationHistory.History history = ManagedLocalAiActivationHistory.readVerified(cache);
            return history == null ? null : activationSnapshot(cache, configured, history.active());
        } catch (ManagedLocalAiActivationHistory.ActiveArtifactDrift changedActivation) {
            return null;
        } catch (IOException failure) {
            throw new IllegalStateException("Managed-local activation history cannot be read.", failure);
        }
    }

    private static ManagedLocalAiSnapshot inspectSelection(Path cache, Settings configured,
                                                           ManagedLocalAiManifest manifest,
                                                           ManagedLocalAiHardware.Profile profile) {
        String requested = AUTOMATIC_MODEL.equalsIgnoreCase(configured.model()) ? null : configured.model();
        ManagedLocalAiHardware.Selection selection = ManagedLocalAiHardware.select(manifest, profile, requested);
        Map<String, ManagedLocalAiSnapshot.Model> models = modelInventory(manifest, selection);
        if (selection.selectedModelId() == null) {
            return snapshot(ManagedLocalAiSnapshot.State.EXCLUDED,
                    "No reviewed model safely fits the effective memory, CPU, and free-disk limits.",
                    new SnapshotContext(cache, configured, profile.platform(), profile, manifest, null, models));
        }

        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals(selection.selectedModelId())).findFirst().orElseThrow();
        ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(profile.platform())).findFirst().orElseThrow();
        ManagedLocalAiSnapshot.CacheHealth runtimeStatus = inspectRuntime(cache, manifest, asset);
        ManagedLocalAiSnapshot.CacheHealth modelStatus = inspectModel(cache, model);
        SelectionContext context = new SelectionContext(cache, configured, profile, manifest, model, models);
        return cacheSnapshot(context, runtimeStatus, modelStatus);
    }

    private static ManagedLocalAiSnapshot cacheSnapshot(SelectionContext context,
                                                        ManagedLocalAiSnapshot.CacheHealth runtimeStatus,
                                                        ManagedLocalAiSnapshot.CacheHealth modelStatus) {
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
        return snapshot(state, action,
                new SnapshotContext(context.cache(), context.settings(), context.profile().platform(),
                        context.profile(), context.manifest(), context.model(), context.models()),
                runtimeStatus, modelStatus);
    }

    ReadyRuntime readyRuntime() throws IOException {
        ManagedLocalAiSnapshot snapshot = inspect();
        if (snapshot.state() != ManagedLocalAiSnapshot.State.READY || snapshot.selectedModelId() == null) {
            throw new IllegalStateException("Managed local AI is not ready for inference.");
        }
        ManagedLocalAiActivationHistory.History history = ManagedLocalAiActivationHistory.readVerified(
                snapshot.cacheDirectory());
        if (history != null) {
            ManagedLocalAiActivationHistory.Activation active = history.active();
            ManagedLocalAiCache.Installation runtimeInstallation = ManagedLocalAiCache.verify(
                    snapshot.cacheDirectory(), active.runtimeId());
            ManagedLocalAiCache.Installation modelInstallation = ManagedLocalAiCache.verify(
                    snapshot.cacheDirectory(), active.modelId());
            int launchTimeoutSeconds = SHAFT.Properties.managedLocalAi.launchTimeoutSeconds();
            if (launchTimeoutSeconds <= 0) {
                throw new IllegalStateException("Managed local AI launch timeout must be positive.");
            }
            return new ReadyRuntime(snapshot.cacheDirectory(), requireOwnedNamedFile(snapshot.cacheDirectory(),
                    runtimeInstallation, active.runtimeExecutable()), requireOwnedNamedFile(snapshot.cacheDirectory(),
                    modelInstallation, active.modelFile()), inferenceLog(snapshot.cacheDirectory()),
                    active.modelArtifactId(), Math.max(1, snapshot.cpuCount()),
                    Duration.ofSeconds(launchTimeoutSeconds));
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

    private ManagedLocalAiSnapshot activationSnapshot(Path cache, Settings configured,
                                                       ManagedLocalAiActivationHistory.Activation active) {
        ManagedLocalAiManifest.RuntimeAsset runtimeAsset = new ManagedLocalAiManifest.RuntimeAsset(
                active.runtimePlatform(), active.runtimeFile(), java.net.URI.create(active.runtimeUrl()),
                active.runtimeArtifactBytes(), active.runtimeSha256(), active.runtimeExecutable(),
                active.runtimeAbi(), active.runtimeMinimumAbiVersion());
        ManagedLocalAiManifest.RuntimeManifest runtime = new ManagedLocalAiManifest.RuntimeManifest(
                "llama.cpp", active.runtimeVersion(), active.runtimeLicense(), java.net.URI.create(
                "https://github.com/ggml-org/llama.cpp/releases/tag/" + active.runtimeVersion()),
                List.of(runtimeAsset));
        ManagedLocalAiManifest.ModelManifest model = new ManagedLocalAiManifest.ModelManifest(
                active.modelArtifactId(), active.modelName(), active.modelTier(), active.modelAutomatic(),
                active.modelFirstPartyQuantization(), active.modelLicense(), active.modelSource(),
                active.modelRevision(), active.modelFile(), java.net.URI.create(active.modelUrl()),
                active.modelArtifactBytes(), active.modelSha256(), active.modelMinimumRamGb(),
                active.modelMinimumCpuCount(), active.modelMinimumFreeDiskGb());
        ManagedLocalAiManifest historical = new ManagedLocalAiManifest(1, runtime, List.of(model));
        ManagedLocalAiHardware.Profile profile = ManagedLocalAiHardware.profile(cache, host, historical);
        ManagedLocalAiHardware.Selection selection = ManagedLocalAiHardware.select(
                historical, profile, active.modelArtifactId());
        ManagedLocalAiHardware.ModelEvaluation evaluation = selection.models().get(active.modelArtifactId());
        Map<String, ManagedLocalAiSnapshot.Model> models = Map.of(active.modelArtifactId(),
                new ManagedLocalAiSnapshot.Model(active.modelName(), active.modelTier(), active.modelLicense(),
                        active.modelRevision(), active.modelFile(), active.modelSha256(), active.modelAutomatic(),
                        evaluation.eligible(), evaluation.reasons(), evaluation.requiredDiskBytes(),
                        active.modelArtifactBytes()));
        boolean compatible = profile.platform().equals(active.runtimePlatform()) && evaluation.eligible();
        return new ManagedLocalAiSnapshot(compatible ? ManagedLocalAiSnapshot.State.READY
                : ManagedLocalAiSnapshot.State.EXCLUDED,
                compatible ? "The exact reviewed managed-local activation is ready."
                        : "The active reviewed rollback candidate no longer meets this host's eligibility limits.",
                cache, configured.enabled(), configured.transparentProvisioning(), configured.model(),
                compatible ? active.modelArtifactId() : null, active.runtimePlatform(), "llama.cpp",
                active.runtimeVersion(), active.runtimeLicense(), active.runtimeFile(), active.runtimeSha256(),
                active.runtimeExecutable(), active.runtimeArtifactBytes(), ManagedLocalAiSnapshot.CacheHealth.READY,
                ManagedLocalAiSnapshot.CacheHealth.READY, ManagedLocalAiSnapshot.Phase.IDLE, 0, 0,
                profile.effectiveMemoryBytes(), profile.cpuCount(), profile.freeDiskBytes(), models);
    }

    private static ManagedLocalAiSnapshot snapshot(ManagedLocalAiSnapshot.State state, String action,
                                                   SnapshotContext context) {
        return snapshot(state, action, context,
                ManagedLocalAiSnapshot.CacheHealth.NOT_APPLICABLE,
                ManagedLocalAiSnapshot.CacheHealth.NOT_APPLICABLE);
    }

    ManagedLocalAiActivationHistory.Activation rollbackCandidate() throws IOException {
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        Path cache = resolveCache(configured.cacheDirectory());
        ManagedLocalAiActivationHistory.Activation candidate =
                ManagedLocalAiActivationHistory.rollbackCandidate(cache);
        if (candidate != null && activationSnapshot(cache, configured, candidate).state()
                != ManagedLocalAiSnapshot.State.READY) {
            throw new IllegalStateException("Reviewed managed-local rollback candidate is ineligible on this host.");
        }
        return candidate;
    }

    ManagedLocalAiSnapshot rollbackReviewed() throws Exception {
        ManagedLocalAiActivationHistory.Activation candidate = rollbackCandidate();
        if (candidate == null) {
            throw new IllegalStateException("No reviewed managed-local rollback candidate is available.");
        }
        return rollbackReviewed(candidate);
    }

    ManagedLocalAiSnapshot rollbackReviewed(ManagedLocalAiActivationHistory.Activation expected) throws Exception {
        Objects.requireNonNull(expected, "expected");
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        Path cache = resolveCache(configured.cacheDirectory());
        Duration lockTimeout = Duration.ofSeconds(configured.lockTimeoutSeconds());
        AtomicReference<ManagedLocalAiSnapshot> result = new AtomicReference<>();
        ManagedLocalAiProcess.withLaunchExclusion(lockTimeout, () -> {
            ManagedLocalAiCache.withLock(cache, lockTimeout, () -> {
                ManagedLocalAiActivationHistory.rollbackLocked(cache, expected, () -> {
                    ManagedLocalAiSnapshot eligible = activationSnapshot(cache, configured, expected);
                    if (eligible.state() != ManagedLocalAiSnapshot.State.READY) {
                        throw new IllegalStateException(
                                "Reviewed managed-local rollback candidate is ineligible on this host.");
                    }
                    result.set(eligible);
                    ManagedLocalAiProcess.terminateRetainedLaunches();
                });
                return null;
            });
        });
        return result.get();
    }

    /** Returns the effective inference cache without inspecting hardware or mutating the host. */
    public Path effectiveCacheDirectory() {
        Settings configured = Objects.requireNonNull(settings.get(), "managed local AI settings");
        return resolveCache(configured.cacheDirectory());
    }

    private static ManagedLocalAiSnapshot snapshot(ManagedLocalAiSnapshot.State state, String action,
                                                   SnapshotContext context,
                                                   ManagedLocalAiSnapshot.CacheHealth runtimeHealth,
                                                   ManagedLocalAiSnapshot.CacheHealth modelHealth) {
        RuntimeDetails runtime = RuntimeDetails.from(context.manifest(), context.platform());
        HardwareDetails hardware = HardwareDetails.from(context.profile());
        return new ManagedLocalAiSnapshot(state, action, context.cache(), context.settings().enabled(),
                context.settings().transparentProvisioning(), context.settings().model(),
                selectedModelId(context.model()), context.platform(), runtime.id(), runtime.version(),
                runtime.license(), runtime.file(), runtime.sha256(), runtime.executable(), runtime.size(),
                runtimeHealth, modelHealth, ManagedLocalAiSnapshot.Phase.IDLE, 0, 0,
                hardware.effectiveMemoryBytes(), hardware.cpuCount(), hardware.freeDiskBytes(), context.models());
    }

    private static String selectedModelId(ManagedLocalAiManifest.ModelManifest model) {
        return model == null ? null : model.id();
    }

    private record SnapshotContext(Path cache, Settings settings, String platform,
                                   ManagedLocalAiHardware.Profile profile, ManagedLocalAiManifest manifest,
                                   ManagedLocalAiManifest.ModelManifest model,
                                   Map<String, ManagedLocalAiSnapshot.Model> models) {
    }

    private record SelectionContext(Path cache, Settings settings, ManagedLocalAiHardware.Profile profile,
                                    ManagedLocalAiManifest manifest, ManagedLocalAiManifest.ModelManifest model,
                                    Map<String, ManagedLocalAiSnapshot.Model> models) {
    }

    private record RuntimeDetails(String id, String version, String license, String file, String sha256,
                                  String executable, long size) {
        private static RuntimeDetails from(ManagedLocalAiManifest manifest, String platform) {
            if (manifest == null) {
                return new RuntimeDetails("", "", "", "", "", "", 0);
            }
            ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                    .filter(candidate -> candidate.platform().equals(platform)).findFirst().orElse(null);
            if (asset == null) {
                return new RuntimeDetails(manifest.runtime().id(), manifest.runtime().version(),
                        manifest.runtime().license(), "", "", "", 0);
            }
            return new RuntimeDetails(manifest.runtime().id(), manifest.runtime().version(),
                    manifest.runtime().license(), asset.file(), asset.sha256(), asset.executable(), asset.size());
        }
    }

    private record HardwareDetails(long effectiveMemoryBytes, int cpuCount, long freeDiskBytes) {
        private static HardwareDetails from(ManagedLocalAiHardware.Profile profile) {
            if (profile == null) {
                return new HardwareDetails(0, 0, 0);
            }
            return new HardwareDetails(profile.effectiveMemoryBytes(), profile.cpuCount(), profile.freeDiskBytes());
        }
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

    private static void rollback(Path cache, Settings settings, Set<String> installationIds)
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
                                             Set<String> installationIds) {
        try {
            rollback(cache, settings, installationIds);
        } catch (Exception cleanup) {
            // Cancellation stays primary; incomplete cleanup remains discoverable through cache status/recovery.
        }
    }

    private record ProvisionContext(Settings settings, ManagedLocalAiSnapshot initial,
                                    ManagedLocalAiSnapshot reviewed, ManagedLocalAiOperation operation,
                                    Consumer<ManagedLocalAiSnapshot> progress, boolean allowDownloads) {
    }

    private record ProvisionPlan(ManagedLocalAiManifest manifest, ManagedLocalAiHardware.Profile profile,
                                 ManagedLocalAiManifest.ModelManifest model) {
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

    record ProvisionResult(Set<String> installationIds) {
        ProvisionResult {
            installationIds = Set.copyOf(installationIds);
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
            ProvisionContext context = new ProvisionContext(cache, manifest, profile, model, settings, host,
                    cancelled, progress, asset, total);
            return ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(settings.lockTimeoutSeconds()),
                    () -> provisionLocked(context));
        }

        private ProvisionResult provisionLocked(ProvisionContext context) throws Exception {
            validateLockedSelection(context);
            String runtimeId = runtimeInstallationId(context.manifest(), context.profile().platform());
            String modelId = modelInstallationId(context.model());
            ManagedLocalAiSnapshot.CacheHealth runtimeHealth = inspectRuntime(
                    context.cache(), context.manifest(), context.asset());
            ManagedLocalAiSnapshot.CacheHealth modelHealth = inspectModel(context.cache(), context.model());
            requireHealthyCache(runtimeHealth, modelHealth);
            return installMissing(context, runtimeId, modelId, runtimeHealth, modelHealth);
        }

        private static void validateLockedSelection(ProvisionContext context) {
            ManagedLocalAiHardware.Profile lockedProfile = ManagedLocalAiHardware.profile(
                    context.cache(), context.host());
            ManagedLocalAiHardware.Selection lockedSelection = ManagedLocalAiHardware.select(
                    context.manifest(), lockedProfile, context.model().id());
            if (!context.model().id().equals(lockedSelection.selectedModelId())) {
                throw new IllegalStateException("Managed local AI resources changed before provisioning.");
            }
        }

        private static void requireHealthyCache(ManagedLocalAiSnapshot.CacheHealth runtimeHealth,
                                                ManagedLocalAiSnapshot.CacheHealth modelHealth) {
            if (runtimeHealth == ManagedLocalAiSnapshot.CacheHealth.CORRUPT
                    || modelHealth == ManagedLocalAiSnapshot.CacheHealth.CORRUPT) {
                throw new IllegalStateException("Changed managed local AI cache must be rebuilt explicitly.");
            }
        }

        private ProvisionResult installMissing(ProvisionContext context, String runtimeId, String modelId,
                                               ManagedLocalAiSnapshot.CacheHealth runtimeHealth,
                                               ManagedLocalAiSnapshot.CacheHealth modelHealth) throws Exception {
            boolean installedRuntime = false;
            boolean installedModel = false;
            try {
                installedRuntime = installRuntimeIfMissing(context, runtimeId, runtimeHealth);
                installedModel = installModelIfMissing(context, modelId, modelHealth);
            } catch (InterruptedException primary) {
                rollbackFailedProvision(context.cache(), runtimeId, installedRuntime, primary, true);
                throw primary;
            } catch (Exception primary) {
                boolean interrupted = context.cancelled().getAsBoolean() || Thread.interrupted();
                Thread.interrupted();
                rollbackFailedProvision(context.cache(), runtimeId, installedRuntime, primary, interrupted);
                throw primary;
            }
            Set<String> installed = new LinkedHashSet<>();
            if (installedRuntime) {
                installed.add(runtimeId);
            }
            if (installedModel) {
                installed.add(modelId);
            }
            return new ProvisionResult(installed);
        }

        private boolean installRuntimeIfMissing(ProvisionContext context, String runtimeId,
                                                ManagedLocalAiSnapshot.CacheHealth health) throws Exception {
            if (health != ManagedLocalAiSnapshot.CacheHealth.MISSING) {
                return false;
            }
            context.progress().accept(new Progress(ManagedLocalAiSnapshot.Phase.DOWNLOADING_RUNTIME,
                    0, context.total()));
            installRuntime(context.cache(), context.asset(), runtimeId, context.settings(), context.cancelled(),
                    context.progress(), context.total());
            return true;
        }

        private boolean installModelIfMissing(ProvisionContext context, String modelId,
                                              ManagedLocalAiSnapshot.CacheHealth health) throws Exception {
            if (health != ManagedLocalAiSnapshot.CacheHealth.MISSING) {
                return false;
            }
            context.progress().accept(new Progress(ManagedLocalAiSnapshot.Phase.DOWNLOADING_MODEL,
                    context.asset().size(), context.total()));
            installModel(context.cache(), context.model(), modelId, context.settings(), context.cancelled());
            context.progress().accept(new Progress(ManagedLocalAiSnapshot.Phase.ADOPTING,
                    context.total(), context.total()));
            return true;
        }

        private static void rollbackFailedProvision(Path cache, String runtimeId, boolean installedRuntime,
                                                    Exception primary, boolean interrupted) {
            Thread.interrupted();
            if (installedRuntime) {
                try {
                    ManagedLocalAiCache.clean(cache, Set.of(runtimeId));
                } catch (Exception rollback) {
                    primary.addSuppressed(rollback);
                }
            }
            if (interrupted) {
                Thread.currentThread().interrupt();
            }
        }

        private record ProvisionContext(Path cache, ManagedLocalAiManifest manifest,
                                        ManagedLocalAiHardware.Profile profile,
                                        ManagedLocalAiManifest.ModelManifest model, Settings settings,
                                        ManagedLocalAiHardware.HostAccess host, BooleanSupplier cancelled,
                                        Consumer<Progress> progress, ManagedLocalAiManifest.RuntimeAsset asset,
                                        long total) {
        }

        private void installRuntime(Path cache, ManagedLocalAiManifest.RuntimeAsset asset, String id,
                                           Settings settings, BooleanSupplier cancelled, Consumer<Progress> progress,
                                           long total) throws Exception {
            Path staging = cache.resolve("staging");
            Files.createDirectories(staging);
            Path archive = staging.resolve(id + ".archive-" + UUID.randomUUID() + "-" + asset.file());
            Path stage = null;
            List<Path> ownedFiles = new ArrayList<>();
            List<Path> ownedDirectories = new ArrayList<>();
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
            List<Path> ownedFiles = new ArrayList<>();
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
        @Override
        public void download(ManagedLocalAiManifest.RuntimeAsset asset, Path target, Duration timeout,
                             BooleanSupplier cancelled) throws IOException, InterruptedException {
            ManagedLocalAiArtifacts.download(asset, target, timeout, cancelled);
        }

        @Override
        public void download(ManagedLocalAiManifest.ModelManifest model, Path target, Duration timeout,
                             BooleanSupplier cancelled) throws IOException, InterruptedException {
            ManagedLocalAiArtifacts.download(model, target, timeout, cancelled);
        }

        @Override
        public ManagedLocalAiArtifacts.Extraction extract(Path archive, Path destination,
                                                           BooleanSupplier cancelled)
                throws IOException, InterruptedException {
            return ManagedLocalAiArtifacts.extractStage(archive, destination, cancelled);
        }
    }
}
