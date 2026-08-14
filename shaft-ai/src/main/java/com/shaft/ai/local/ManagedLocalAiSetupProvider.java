package com.shaft.ai.local;

import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupAction;
import com.shaft.infrastructure.SetupActionKind;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupOptions;
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
        ManagedLocalAiSnapshot snapshot = inspect(options);
        if (snapshot.selectedModelId() == null) {
            throw new IllegalStateException("Managed local AI cannot select a reviewed model: " + snapshot.action());
        }
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset runtime = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(snapshot.platform()))
                .findFirst().orElseThrow();
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals(snapshot.selectedModelId()))
                .findFirst().orElseThrow();
        SetupActionKind kind = options.effectiveMode() == com.shaft.infrastructure.SetupMode.EXTERNAL
                ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        return SetupPlan.create(SetupProfile.LOCAL_AI, platform, architecture, options.effectiveMode(), List.of(
                new SetupAction(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, kind, manifest.runtime().version(),
                        runtime.url(), "sha256:" + runtime.sha256(), runtime.size(), false,
                        Set.of(manifest.runtime().license())),
                new SetupAction(SetupTarget.MANAGED_LOCAL_AI_MODEL, kind, model.revision(), model.url(),
                        "sha256:" + model.sha256(), model.size(), false, Set.of(model.license()))));
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
        Lifecycle lifecycle = lifecycles.create(options);
        ManagedLocalAiSnapshot initial = lifecycle.inspect();
        requireCacheRoot(options, initial);
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
    }
}
