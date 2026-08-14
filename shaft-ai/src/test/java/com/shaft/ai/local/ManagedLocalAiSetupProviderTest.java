package com.shaft.ai.local;

import com.shaft.infrastructure.InfrastructureSetupService;
import com.shaft.infrastructure.SetupAction;
import com.shaft.infrastructure.SetupActionKind;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlatform;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupProviderRegistry;
import com.shaft.infrastructure.SetupReadiness;
import com.shaft.infrastructure.SetupReceipt;
import com.shaft.infrastructure.SetupProgress;
import com.shaft.infrastructure.SetupTarget;
import com.shaft.infrastructure.ShaftCachePaths;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiSetupProviderTest {
    @Test
    void builtInCoordinatorDiscoversLocalAiProvider() {
        assertTrue(InfrastructureSetupService.builtIn().supports(SetupProfile.LOCAL_AI));
    }

    @Test
    void managedPlanBindsExactReviewedRuntimeAndSelectedModelArtifacts(@TempDir Path temp) {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot missing = snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED);
        InfrastructureSetupService setup = setup(new FakeLifecycle(missing));

        SetupPlan plan = setup.plan(options);

        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset runtime = manifest.runtime().assets().stream()
                .filter(asset -> asset.platform().equals("windows-x86_64")).findFirst().orElseThrow();
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals("qwen3-0.6b-q8_0")).findFirst().orElseThrow();
        assertEquals(4, plan.schemaVersion());
        assertEquals(List.of(
                new SetupAction(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, SetupActionKind.INSTALL,
                        manifest.runtime().version(), runtime.url(), "sha256:" + runtime.sha256(), runtime.size(),
                        false, Set.of(manifest.runtime().license())),
                new SetupAction(SetupTarget.MANAGED_LOCAL_AI_MODEL, SetupActionKind.INSTALL,
                        model.revision(), model.url(), "sha256:" + model.sha256(), model.size(), false,
                        Set.of(model.license()))), plan.actions());
        assertEquals("23749fefcc72300e3a2ad315e1317431b06b590a", plan.actions().get(1).version());
        assertEquals("sha256:9465e63a22add5354d9bb4b99e90117043c7124007664907259bd16d043bb031",
                plan.actions().get(1).checksum());
        assertEquals(639_446_688L, plan.actions().get(1).artifactBytes());
        assertFalse(Files.exists(options.paths().cacheRoot()));
        assertFalse(Files.exists(options.paths().dataRoot()));
    }

    @Test
    void mapsEveryLifecycleStateWithoutMutatingTheHost(@TempDir Path temp) {
        SetupOptions options = options(temp);
        Map<ManagedLocalAiSnapshot.State, SetupReadiness> expected = new LinkedHashMap<>();
        expected.put(ManagedLocalAiSnapshot.State.DISABLED, SetupReadiness.MISSING);
        expected.put(ManagedLocalAiSnapshot.State.EXCLUDED, SetupReadiness.UNSUPPORTED);
        expected.put(ManagedLocalAiSnapshot.State.UNSUPPORTED, SetupReadiness.UNSUPPORTED);
        expected.put(ManagedLocalAiSnapshot.State.CORRUPT, SetupReadiness.DEGRADED);
        expected.put(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, SetupReadiness.MISSING);
        expected.put(ManagedLocalAiSnapshot.State.READY, SetupReadiness.READY);

        for (var entry : expected.entrySet()) {
            FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, entry.getKey()));
            var report = setup(lifecycle).status(options);
            assertEquals(entry.getValue(), report.readiness(), entry.getKey().name());
            assertEquals(List.of(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, SetupTarget.MANAGED_LOCAL_AI_MODEL),
                    report.targets().stream().map(status -> status.target()).toList());
            assertEquals(0, lifecycle.provisions.get());
        }
        assertFalse(Files.exists(options.paths().cacheRoot()));
        assertFalse(Files.exists(options.paths().dataRoot()));
    }

    @Test
    void planAndStatusRejectACacheRootDifferentFromInference(@TempDir Path temp) {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot elsewhere = snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED,
                temp.resolve("other-managed-cache").toAbsolutePath());
        InfrastructureSetupService setup = setup(new FakeLifecycle(elsewhere));

        IllegalArgumentException status = assertThrows(IllegalArgumentException.class,
                () -> setup.status(options));
        IllegalArgumentException plan = assertThrows(IllegalArgumentException.class,
                () -> setup.plan(options));
        assertTrue(status.getMessage().contains("cache root"));
        assertTrue(plan.getMessage().contains("cache root"));
    }

    @Test
    void installRequiresOnlineExactApprovalAndBothLicenses(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED));
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);
        Set<String> licenses = plan.actions().stream().flatMap(action -> action.requiredLicenses().stream())
                .collect(java.util.stream.Collectors.toSet());

        assertThrows(IllegalArgumentException.class, () -> setup.install(plan,
                new SetupApproval("sha256:" + "0".repeat(64), Instant.EPOCH, licenses), options));
        assertThrows(IllegalArgumentException.class, () -> setup.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        SetupOptions offline = options.withOffline(true);
        SetupPlan offlinePlan = setup.plan(offline);
        assertThrows(IOException.class, () -> setup.install(offlinePlan,
                new SetupApproval(offlinePlan.digest(), Instant.EPOCH, licenses), offline));
        assertEquals(0, lifecycle.provisions.get());
    }

    @Test
    void successfulProvisionReturnsReceiptButFailureAndCancellationDoNot(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot missing = snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED);
        ManagedLocalAiSnapshot ready = snapshot(options, ManagedLocalAiSnapshot.State.READY);
        FakeLifecycle successful = new FakeLifecycle(missing);
        successful.provisioned = ready;
        InfrastructureSetupService setup = setup(successful);
        SetupPlan plan = setup.plan(options);
        SetupApproval approval = approval(plan);

        SetupReceipt receipt = setup.install(plan, approval, options);
        assertEquals(plan.digest(), receipt.planDigest());
        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(1, successful.provisions.get());

        for (Exception failure : List.of(new IOException("download failed"), new CancellationException("cancelled"))) {
            FakeLifecycle failed = new FakeLifecycle(missing);
            failed.failure = failure;
            InfrastructureSetupService failingSetup = setup(failed);
            SetupPlan failingPlan = failingSetup.plan(options);
            assertThrows(IOException.class, () -> failingSetup.install(failingPlan, approval(failingPlan), options));
            assertEquals(1, failed.provisions.get());
        }
    }

    @Test
    void installPublishesManagedPhaseBytesAndPercentage(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot missing = snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED);
        FakeLifecycle lifecycle = new FakeLifecycle(missing);
        lifecycle.progressSnapshots = List.of(withProgress(missing,
                ManagedLocalAiSnapshot.Phase.DOWNLOADING_MODEL, 25, 100));
        lifecycle.provisioned = snapshot(options, ManagedLocalAiSnapshot.State.READY);
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);
        java.util.ArrayList<SetupProgress> progress = new java.util.ArrayList<>();

        setup.install(plan, approval(plan), options, progress::add);

        assertEquals(2, progress.size());
        assertEquals(SetupProfile.LOCAL_AI, progress.getFirst().profile());
        assertEquals("DOWNLOADING_MODEL", progress.getFirst().phase());
        assertEquals(25, progress.getFirst().completedBytes());
        assertEquals(100, progress.getFirst().totalBytes());
        assertEquals(25, progress.getFirst().percentage());
        assertEquals("IDLE", progress.getLast().phase());
    }

    private static SetupApproval approval(SetupPlan plan) {
        Set<String> licenses = plan.actions().stream().flatMap(action -> action.requiredLicenses().stream())
                .collect(java.util.stream.Collectors.toSet());
        return new SetupApproval(plan.digest(), Instant.EPOCH, licenses);
    }

    private static InfrastructureSetupService setup(FakeLifecycle lifecycle) {
        return new InfrastructureSetupService(new SetupProviderRegistry(List.of(
                new ManagedLocalAiSetupProvider(ignored -> lifecycle))),
                SetupPlatform.WINDOWS, SetupArchitecture.X64);
    }

    private static SetupOptions options(Path temp) {
        return SetupOptions.defaults(SetupProfile.LOCAL_AI, paths(temp)).withMode(SetupMode.MANAGED);
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static ManagedLocalAiSnapshot snapshot(SetupOptions options, ManagedLocalAiSnapshot.State state) {
        return snapshot(options, state, options.paths().cacheRoot());
    }

    private static ManagedLocalAiSnapshot snapshot(SetupOptions options, ManagedLocalAiSnapshot.State state,
                                                   Path cache) {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset runtime = manifest.runtime().assets().stream()
                .filter(asset -> asset.platform().equals("windows-x86_64")).findFirst().orElseThrow();
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals("qwen3-0.6b-q8_0")).findFirst().orElseThrow();
        boolean selected = state != ManagedLocalAiSnapshot.State.DISABLED
                && state != ManagedLocalAiSnapshot.State.EXCLUDED
                && state != ManagedLocalAiSnapshot.State.UNSUPPORTED;
        ManagedLocalAiSnapshot.CacheHealth health = switch (state) {
            case READY -> ManagedLocalAiSnapshot.CacheHealth.READY;
            case CORRUPT -> ManagedLocalAiSnapshot.CacheHealth.CORRUPT;
            case NOT_PROVISIONED -> ManagedLocalAiSnapshot.CacheHealth.MISSING;
            default -> ManagedLocalAiSnapshot.CacheHealth.NOT_APPLICABLE;
        };
        Map<String, ManagedLocalAiSnapshot.Model> models = selected ? Map.of(model.id(),
                new ManagedLocalAiSnapshot.Model(model.displayName(), model.tier(), model.license(), model.revision(),
                        model.file(), model.sha256(), model.automatic(), true, List.of(), model.size(), model.size()))
                : Map.of();
        return new ManagedLocalAiSnapshot(state, "reviewed action", cache.toAbsolutePath(),
                state != ManagedLocalAiSnapshot.State.DISABLED, false, "qwen3-0.6b-q8_0",
                selected ? model.id() : null, "windows-x86_64", manifest.runtime().id(),
                manifest.runtime().version(), manifest.runtime().license(), runtime.file(), runtime.sha256(),
                runtime.executable(), runtime.size(), health, health, ManagedLocalAiSnapshot.Phase.IDLE, 0, 0,
                16L * 1024 * 1024 * 1024, 8, 64L * 1024 * 1024 * 1024, models);
    }

    private static ManagedLocalAiSnapshot withProgress(ManagedLocalAiSnapshot snapshot,
                                                       ManagedLocalAiSnapshot.Phase phase,
                                                       long completedBytes, long totalBytes) {
        return new ManagedLocalAiSnapshot(snapshot.state(), snapshot.action(), snapshot.cacheDirectory(),
                snapshot.enabled(), snapshot.transparentProvisioning(), snapshot.requestedModelId(),
                snapshot.selectedModelId(), snapshot.platform(), snapshot.runtimeId(), snapshot.runtimeVersion(),
                snapshot.runtimeLicense(), snapshot.runtimeAssetFile(), snapshot.runtimeAssetSha256(),
                snapshot.runtimeExecutable(), snapshot.runtimeAssetBytes(), snapshot.runtimeCacheHealth(),
                snapshot.modelCacheHealth(), phase, completedBytes, totalBytes, snapshot.effectiveMemoryBytes(),
                snapshot.cpuCount(), snapshot.freeDiskBytes(), snapshot.models());
    }

    private static final class FakeLifecycle implements ManagedLocalAiSetupProvider.Lifecycle {
        private final ManagedLocalAiSnapshot inspected;
        private final AtomicInteger provisions = new AtomicInteger();
        private ManagedLocalAiSnapshot provisioned;
        private List<ManagedLocalAiSnapshot> progressSnapshots = List.of();
        private Exception failure;

        private FakeLifecycle(ManagedLocalAiSnapshot inspected) {
            this.inspected = inspected;
            this.provisioned = inspected;
        }

        @Override
        public ManagedLocalAiSnapshot inspect() {
            return inspected;
        }

        @Override
        public ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress) {
            provisions.incrementAndGet();
            ManagedLocalAiOperation operation = new ManagedLocalAiOperation(inspected);
            if (failure instanceof CancellationException) {
                operation.cancelled();
            } else if (failure != null) {
                operation.fail(failure);
            } else {
                progressSnapshots.forEach(progress);
                progress.accept(provisioned);
                operation.complete(provisioned);
            }
            return operation;
        }
    }
}
