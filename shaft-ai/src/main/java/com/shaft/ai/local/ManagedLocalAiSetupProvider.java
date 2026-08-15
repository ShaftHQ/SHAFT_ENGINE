package com.shaft.ai.local;

import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupAction;
import com.shaft.infrastructure.SetupActionKind;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupOperation;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlatform;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupProvider;
import com.shaft.infrastructure.SetupReceipt;
import com.shaft.infrastructure.SetupReadiness;
import com.shaft.infrastructure.SetupReport;
import com.shaft.infrastructure.SetupStatus;
import com.shaft.infrastructure.SetupTarget;
import com.shaft.infrastructure.SetupExecutor;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupProgress;
import com.shaft.infrastructure.SetupSelection;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Objects;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

/** Shared setup-provider adapter for SHAFT-managed local inference. */
public final class ManagedLocalAiSetupProvider implements SetupProvider {
    private final LifecycleFactory lifecycles;

    public ManagedLocalAiSetupProvider() {
        this(ignored -> new ServiceLifecycle(new ManagedLocalAiService()));
    }

    ManagedLocalAiSetupProvider(LifecycleFactory lifecycles) {
        this.lifecycles = Objects.requireNonNull(lifecycles, "lifecycles");
    }

    @Override
    public SetupProfile profile() {
        return SetupProfile.LOCAL_AI;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return plan(options, SetupSelection.defaults(), SetupOperation.INSTALL, platform, architecture);
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupSelection selection, SetupOperation operation,
                          SetupPlatform platform, SetupArchitecture architecture) {
        validatePlanRequest(options, selection, operation);
        if (operation == SetupOperation.ROLLBACK) {
            return planRollback(options, platform, architecture);
        }
        ManagedLocalAiSnapshot snapshot = operation == SetupOperation.INSTALL
                ? inspectReviewed(options) : inspect(options);
        if (operation == SetupOperation.INSTALL && snapshot.selectedModelId() == null) {
            throw new IllegalStateException("Managed local AI cannot select a reviewed model: " + snapshot.action());
        }
        return artifactPlan(options, operation, platform, architecture, snapshot,
                ManagedLocalAiManifest.loadDefault());
    }

    private static void validatePlanRequest(SetupOptions options, SetupSelection selection,
                                            SetupOperation operation) {
        Objects.requireNonNull(selection, "selection");
        Objects.requireNonNull(operation, "operation");
        if (!selection.components().isEmpty()) {
            throw new IllegalArgumentException("Profile LOCAL_AI does not accept component selection.");
        }
        if (operation != SetupOperation.INSTALL && options.effectiveMode() != SetupMode.MANAGED) {
            throw new IllegalArgumentException(operation + " requires MANAGED mode for profile LOCAL_AI.");
        }
    }

    private SetupPlan planRollback(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        Lifecycle lifecycle = lifecycles.create(options);
        requireCacheRoot(options, lifecycle.inspect());
        try {
            ManagedLocalAiActivationHistory.Activation candidate = lifecycle.rollbackCandidate();
            if (candidate == null) {
                throw new IllegalStateException("No reviewed managed local AI rollback candidate is available.");
            }
            return rollbackPlan(options, platform, architecture, candidate);
        } catch (IOException failure) {
            throw new IllegalStateException("Managed local AI rollback candidate cannot be inspected.", failure);
        }
    }

    private static SetupPlan artifactPlan(SetupOptions options, SetupOperation operation,
                                          SetupPlatform platform, SetupArchitecture architecture,
                                          ManagedLocalAiSnapshot snapshot, ManagedLocalAiManifest manifest) {
        SetupActionKind kind = options.effectiveMode() == SetupMode.EXTERNAL
                ? SetupActionKind.DIAGNOSE
                : operation == SetupOperation.CLEAN ? SetupActionKind.CLEAN : SetupActionKind.INSTALL;
        Set<String> runtimeLicenses = kind == SetupActionKind.CLEAN ? Set.of()
                : Set.of(manifest.runtime().license());
        List<ManagedLocalAiManifest.RuntimeAsset> runtimes = operation == SetupOperation.CLEAN
                ? manifest.runtime().assets()
                : manifest.runtime().assets().stream().filter(candidate -> candidate.platform().equals(snapshot.platform()))
                .toList();
        List<ManagedLocalAiManifest.ModelManifest> models = operation == SetupOperation.CLEAN
                ? manifest.models()
                : manifest.models().stream().filter(candidate -> candidate.id().equals(snapshot.selectedModelId()))
                .toList();
        ArrayList<SetupAction> actions = new ArrayList<>();
        runtimes.forEach(runtime -> actions.add(new SetupAction(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, kind,
                manifest.runtime().version(), runtime.url(), "sha256:" + runtime.sha256(), runtime.size(), false,
                runtimeLicenses)));
        models.forEach(model -> actions.add(new SetupAction(SetupTarget.MANAGED_LOCAL_AI_MODEL, kind,
                model.revision(), model.url(), "sha256:" + model.sha256(), model.size(), false,
                kind == SetupActionKind.CLEAN ? Set.of() : Set.of(model.license()))));
        return SetupPlan.create(SetupProfile.LOCAL_AI, platform, architecture, options.effectiveMode(), actions);
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        ManagedLocalAiSnapshot snapshot = inspect(options);
        SetupReadiness overall = switch (snapshot.state()) {
            case READY -> SetupReadiness.READY;
            case CORRUPT -> SetupReadiness.DEGRADED;
            case UNSUPPORTED, EXCLUDED -> SetupReadiness.UNSUPPORTED;
            case DISABLED, NOT_PROVISIONED -> SetupReadiness.MISSING;
        };
        SetupReadiness runtime = artifactReadiness(snapshot, snapshot.runtimeCacheHealth());
        SetupReadiness model = artifactReadiness(snapshot, snapshot.modelCacheHealth());
        String runtimeVersion = runtime == SetupReadiness.READY ? snapshot.runtimeVersion() : "";
        String modelVersion = "";
        if (model == SetupReadiness.READY && snapshot.selectedModelId() != null) {
            modelVersion = snapshot.models().get(snapshot.selectedModelId()).revision();
        }
        return new SetupReport(1, SetupProfile.LOCAL_AI, overall, List.of(
                new SetupStatus(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, runtime, runtimeVersion, snapshot.action()),
                new SetupStatus(SetupTarget.MANAGED_LOCAL_AI_MODEL, model, modelVersion, snapshot.action())),
                List.of());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        return install(plan, approval, options, ignored -> { });
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                Consumer<SetupProgress> progress) throws IOException {
        validateInstallRequest(plan, approval, options, progress);
        ReviewedInstall reviewed = reviewInstall(plan, options);
        return switch (reviewed.operation()) {
            case ROLLBACK -> installRollback(plan, reviewed);
            case CLEAN -> installClean(plan, reviewed);
            case INSTALL -> installProvision(plan, options, progress, reviewed);
        };
    }

    private static void validateInstallRequest(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                               Consumer<SetupProgress> progress) {
        Objects.requireNonNull(options, "options");
        Objects.requireNonNull(progress, "progress");
        if (plan.profile() != SetupProfile.LOCAL_AI || options.profile() != SetupProfile.LOCAL_AI
                || plan.mode() != options.effectiveMode()) {
            throw new IllegalArgumentException("Managed local AI plan does not match the requested setup options.");
        }
        if (options.effectiveMode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External local AI setup is diagnostic-only.");
        }
        SetupExecutor.validate(plan, approval);
    }

    private ReviewedInstall reviewInstall(SetupPlan plan, SetupOptions options) throws IOException {
        SetupOperation operation = SetupOperation.fromPlan(plan);
        Lifecycle lifecycle = lifecycles.create(options);
        ManagedLocalAiSnapshot initial = operation == SetupOperation.INSTALL
                ? lifecycle.inspectReviewed() : lifecycle.inspect();
        requireCacheRoot(options, initial);
        ManagedLocalAiActivationHistory.Activation rollbackCandidate = null;
        SetupPlan reviewed;
        if (operation == SetupOperation.ROLLBACK) {
            rollbackCandidate = lifecycle.rollbackCandidate();
            if (rollbackCandidate == null) {
                throw new IllegalArgumentException("Managed local AI rollback plan has no reviewed cached candidate.");
            }
            reviewed = rollbackPlan(options, plan.platform(), plan.architecture(), rollbackCandidate);
        } else {
            reviewed = plan(options, SetupSelection.defaults(), operation, plan.platform(), plan.architecture());
        }
        SetupPlan expected = SetupPlan.bind(reviewed, options.policyDigest());
        if (!expected.equals(plan)) {
            throw new IllegalArgumentException("Managed local AI plan does not match the reviewed manifest and operation.");
        }
        return new ReviewedInstall(operation, lifecycle, initial, rollbackCandidate);
    }

    private static SetupReceipt installRollback(SetupPlan plan, ReviewedInstall reviewed) throws IOException {
        ManagedLocalAiSnapshot rolledBack;
        try {
            rolledBack = reviewed.lifecycle().rollback(reviewed.rollbackCandidate());
        } catch (IOException failure) {
            throw failure;
        } catch (InterruptedException cancelled) {
            Thread.currentThread().interrupt();
            throw new IOException("Managed local AI rollback was interrupted.", cancelled);
        } catch (Exception failure) {
            throw new IOException("Managed local AI rollback failed.", failure);
        }
        if (!matches(rolledBack, reviewed.rollbackCandidate())) {
            throw new IOException("Managed local AI rollback completed without the reviewed active pair.");
        }
        return receipt(plan);
    }

    private static SetupReceipt installClean(SetupPlan plan, ReviewedInstall reviewed) throws IOException {
        boolean cleaned;
        try {
            cleaned = reviewed.lifecycle().clean();
        } catch (IOException failure) {
            throw failure;
        } catch (InterruptedException cancelled) {
            Thread.currentThread().interrupt();
            throw new IOException("Managed local AI clean was interrupted.", cancelled);
        } catch (Exception failure) {
            throw new IOException("Managed local AI clean failed.", failure);
        }
        if (!cleaned) {
            throw new IOException(
                    "Managed local AI clean preserved changed or unknown owned content; no receipt was created.");
        }
        return receipt(plan);
    }

    private static SetupReceipt installProvision(SetupPlan plan, SetupOptions options,
                                                 Consumer<SetupProgress> progress, ReviewedInstall reviewed)
            throws IOException {
        if (options.offline() && reviewed.initial().state() != ManagedLocalAiSnapshot.State.READY) {
            throw new IOException("Managed local AI is not ready and offline setup cannot download artifacts.");
        }
        ManagedLocalAiSnapshot ready = await(reviewed.lifecycle().provision(
                snapshot -> progress.accept(SetupProgress.of(
                SetupProfile.LOCAL_AI, snapshot.phase().name(), snapshot.completedBytes(),
                snapshot.totalBytes())), !options.offline()), options);
        if (ready.state() != ManagedLocalAiSnapshot.State.READY) {
            throw new IOException("Managed local AI provisioning completed without a ready installation.");
        }
        return receipt(plan);
    }

    private static SetupReceipt receipt(SetupPlan plan) {
        return new SetupReceipt(plan.digest(), Instant.now(), plan.actions());
    }

    private record ReviewedInstall(SetupOperation operation, Lifecycle lifecycle,
                                   ManagedLocalAiSnapshot initial,
                                   ManagedLocalAiActivationHistory.Activation rollbackCandidate) {
    }

    private ManagedLocalAiSnapshot inspect(SetupOptions options) {
        Objects.requireNonNull(options, "options");
        ManagedLocalAiSnapshot snapshot = lifecycles.create(options).inspect();
        requireCacheRoot(options, snapshot);
        return snapshot;
    }

    private ManagedLocalAiSnapshot inspectReviewed(SetupOptions options) {
        Objects.requireNonNull(options, "options");
        ManagedLocalAiSnapshot snapshot = lifecycles.create(options).inspectReviewed();
        requireCacheRoot(options, snapshot);
        return snapshot;
    }

    private static void requireCacheRoot(SetupOptions options, ManagedLocalAiSnapshot snapshot) {
        if (!snapshot.cacheDirectory().equals(options.paths().cacheRoot())) {
            throw new IllegalArgumentException("LOCAL_AI cache root must exactly match the effective managed local "
                    + "AI cache used by inference.");
        }
    }

    private static boolean matches(ManagedLocalAiSnapshot snapshot,
                                   ManagedLocalAiActivationHistory.Activation activation) {
        ManagedLocalAiSnapshot.Model model = snapshot.models().get(snapshot.selectedModelId());
        return snapshot.state() == ManagedLocalAiSnapshot.State.READY
                && activation.modelArtifactId().equals(snapshot.selectedModelId())
                && activation.runtimeVersion().equals(snapshot.runtimeVersion())
                && activation.runtimePlatform().equals(snapshot.platform())
                && activation.runtimeLicense().equals(snapshot.runtimeLicense())
                && activation.runtimeFile().equals(snapshot.runtimeAssetFile())
                && activation.runtimeSha256().equals(snapshot.runtimeAssetSha256())
                && activation.runtimeArtifactBytes() == snapshot.runtimeAssetBytes()
                && activation.runtimeExecutable().equals(snapshot.runtimeExecutable())
                && model != null
                && activation.modelName().equals(model.displayName())
                && activation.modelTier().equals(model.tier())
                && activation.modelLicense().equals(model.license())
                && activation.modelRevision().equals(model.revision())
                && activation.modelFile().equals(model.file())
                && activation.modelSha256().equals(model.sha256())
                && activation.modelArtifactBytes() == model.artifactBytes()
                && activation.modelAutomatic() == model.automatic();
    }

    private static SetupReadiness artifactReadiness(ManagedLocalAiSnapshot snapshot,
                                                    ManagedLocalAiSnapshot.CacheHealth health) {
        if (snapshot.state() == ManagedLocalAiSnapshot.State.UNSUPPORTED
                || snapshot.state() == ManagedLocalAiSnapshot.State.EXCLUDED) {
            return SetupReadiness.UNSUPPORTED;
        }
        return switch (health) {
            case READY -> SetupReadiness.READY;
            case CORRUPT -> SetupReadiness.DEGRADED;
            case MISSING, NOT_APPLICABLE -> SetupReadiness.MISSING;
        };
    }

    private static SetupPlan rollbackPlan(SetupOptions options, SetupPlatform platform,
                                          SetupArchitecture architecture,
                                          ManagedLocalAiActivationHistory.Activation candidate) {
        return SetupPlan.create(SetupProfile.LOCAL_AI, platform, architecture, options.effectiveMode(), List.of(
                new SetupAction(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, SetupActionKind.ROLLBACK,
                        candidate.runtimeVersion(), java.net.URI.create(candidate.runtimeUrl()),
                        "sha256:" + candidate.runtimeSha256(), candidate.runtimeArtifactBytes(), false,
                        Set.of(candidate.runtimeLicense())),
                new SetupAction(SetupTarget.MANAGED_LOCAL_AI_MODEL, SetupActionKind.ROLLBACK,
                        candidate.modelRevision(), java.net.URI.create(candidate.modelUrl()),
                        "sha256:" + candidate.modelSha256(), candidate.modelArtifactBytes(), false,
                        Set.of(candidate.modelLicense()))));
    }

    private static ManagedLocalAiSnapshot await(ManagedLocalAiOperation operation, SetupOptions options)
            throws IOException {
        try {
            return operation.completion().get(options.startupTimeout().toMillis(), TimeUnit.MILLISECONDS);
        } catch (InterruptedException cancelled) {
            operation.cancel();
            Thread.currentThread().interrupt();
            throw new IOException("Managed local AI setup was interrupted.", cancelled);
        } catch (TimeoutException timeout) {
            operation.cancel();
            throw new IOException("Managed local AI setup exceeded " + options.startupTimeout() + '.', timeout);
        } catch (CancellationException cancelled) {
            throw new IOException("Managed local AI setup was cancelled.", cancelled);
        } catch (ExecutionException failed) {
            Throwable cause = failed.getCause();
            if (cause instanceof IOException io) throw io;
            throw new IOException("Managed local AI setup failed.", cause);
        }
    }

    @FunctionalInterface
    interface LifecycleFactory {
        Lifecycle create(SetupOptions options);
    }

    interface Lifecycle {
        ManagedLocalAiSnapshot inspect();

        ManagedLocalAiSnapshot inspectReviewed();

        ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress, boolean allowDownloads);

        boolean clean() throws Exception;

        ManagedLocalAiActivationHistory.Activation rollbackCandidate() throws IOException;

        ManagedLocalAiSnapshot rollback(ManagedLocalAiActivationHistory.Activation expected) throws Exception;
    }

    record ServiceLifecycle(ManagedLocalAiService service) implements Lifecycle {
        ServiceLifecycle {
            Objects.requireNonNull(service, "service");
        }

        @Override
        public ManagedLocalAiSnapshot inspect() {
            return service.inspect();
        }

        @Override
        public ManagedLocalAiSnapshot inspectReviewed() {
            return service.inspectReviewed();
        }

        @Override
        public ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress, boolean allowDownloads) {
            return service.provision(progress, allowDownloads);
        }

        @Override
        public boolean clean() throws Exception {
            return service.cleanReviewed();
        }

        @Override
        public ManagedLocalAiActivationHistory.Activation rollbackCandidate() throws IOException {
            return service.rollbackCandidate();
        }

        @Override
        public ManagedLocalAiSnapshot rollback(ManagedLocalAiActivationHistory.Activation expected) throws Exception {
            return service.rollbackReviewed(expected);
        }
    }
}
