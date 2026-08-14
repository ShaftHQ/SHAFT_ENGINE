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
        Objects.requireNonNull(selection, "selection");
        Objects.requireNonNull(operation, "operation");
        if (!selection.components().isEmpty()) {
            throw new IllegalArgumentException("Profile LOCAL_AI does not accept component selection.");
        }
        if (operation != SetupOperation.INSTALL && options.effectiveMode() != SetupMode.MANAGED) {
            throw new IllegalArgumentException(operation + " requires MANAGED mode for profile LOCAL_AI.");
        }
        if (operation == SetupOperation.ROLLBACK) {
            throw new IllegalStateException("No reviewed managed local AI rollback candidate is available in this release.");
        }
        ManagedLocalAiSnapshot snapshot = inspect(options);
        if (operation == SetupOperation.INSTALL && snapshot.selectedModelId() == null) {
            throw new IllegalStateException("Managed local AI cannot select a reviewed model: " + snapshot.action());
        }
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
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
        java.util.ArrayList<SetupAction> actions = new java.util.ArrayList<>();
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
        SetupOperation operation = SetupOperation.fromPlan(plan);
        if (operation == SetupOperation.ROLLBACK) {
            throw new IllegalArgumentException("Managed local AI rollback is unavailable without a reviewed candidate.");
        }
        SetupPlan expected = SetupPlan.bind(plan(options, SetupSelection.defaults(), operation,
                plan.platform(), plan.architecture()), options.policyDigest());
        if (!expected.equals(plan)) {
            throw new IllegalArgumentException("Managed local AI plan does not match the reviewed manifest and operation.");
        }
        Lifecycle lifecycle = lifecycles.create(options);
        ManagedLocalAiSnapshot initial = lifecycle.inspect();
        requireCacheRoot(options, initial);
        if (operation == SetupOperation.CLEAN) {
            boolean cleaned;
            try {
                cleaned = lifecycle.clean();
            } catch (InterruptedException cancelled) {
                Thread.currentThread().interrupt();
                throw new IOException("Managed local AI clean was interrupted.", cancelled);
            } catch (Exception failure) {
                if (failure instanceof IOException io) throw io;
                throw new IOException("Managed local AI clean failed.", failure);
            }
            if (!cleaned) {
                throw new IOException("Managed local AI clean preserved changed or unknown owned content; no receipt was created.");
            }
            return new SetupReceipt(plan.digest(), Instant.now(), plan.actions());
        }
        if (options.offline() && initial.state() != ManagedLocalAiSnapshot.State.READY) {
            throw new IOException("Managed local AI is not ready and offline setup cannot download artifacts.");
        }
        ManagedLocalAiSnapshot ready = initial.state() == ManagedLocalAiSnapshot.State.READY
                ? initial : await(lifecycle.provision(snapshot -> progress.accept(SetupProgress.of(
                        SetupProfile.LOCAL_AI, snapshot.phase().name(), snapshot.completedBytes(),
                        snapshot.totalBytes()))), options);
        if (ready.state() != ManagedLocalAiSnapshot.State.READY) {
            throw new IOException("Managed local AI provisioning completed without a ready installation.");
        }
        return new SetupReceipt(plan.digest(), Instant.now(), plan.actions());
    }

    private ManagedLocalAiSnapshot inspect(SetupOptions options) {
        Objects.requireNonNull(options, "options");
        ManagedLocalAiSnapshot snapshot = lifecycles.create(options).inspect();
        requireCacheRoot(options, snapshot);
        return snapshot;
    }

    private static void requireCacheRoot(SetupOptions options, ManagedLocalAiSnapshot snapshot) {
        if (!snapshot.cacheDirectory().equals(options.paths().cacheRoot())) {
            throw new IllegalArgumentException("LOCAL_AI cache root must exactly match the effective managed local "
                    + "AI cache used by inference: " + snapshot.cacheDirectory());
        }
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

        ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress);

        boolean clean() throws Exception;
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
        public ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress) {
            return service.provision(progress);
        }

        @Override
        public boolean clean() throws Exception {
            return service.cleanReviewed();
        }
    }
}
