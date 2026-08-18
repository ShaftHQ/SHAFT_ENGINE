package com.shaft.ai.local;

import com.shaft.infrastructure.InfrastructureSetupService;
import com.shaft.infrastructure.SetupAction;
import com.shaft.infrastructure.SetupActionKind;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupOperation;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlatform;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupProviderRegistry;
import com.shaft.infrastructure.SetupReadiness;
import com.shaft.infrastructure.SetupReceipt;
import com.shaft.infrastructure.SetupProgress;
import com.shaft.infrastructure.SetupSelection;
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
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
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
    void cleanPlanBindsTheExactReviewedOwnedArtifactsWithoutMutating(@TempDir Path temp) {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot ready = snapshot(options, ManagedLocalAiSnapshot.State.READY);
        InfrastructureSetupService setup = setup(new FakeLifecycle(ready));

        SetupPlan plan = setup.plan(options, SetupSelection.defaults(), SetupOperation.CLEAN);

        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        assertEquals(manifest.runtime().assets().size() + manifest.models().size(), plan.actions().size());
        assertTrue(plan.actions().stream().allMatch(action -> action.kind() == SetupActionKind.CLEAN));
        assertEquals(manifest.runtime().assets().size(), plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.MANAGED_LOCAL_AI_RUNTIME).count());
        assertEquals(manifest.models().size(), plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.MANAGED_LOCAL_AI_MODEL).count());
        assertFalse(Files.exists(options.paths().cacheRoot()));
        assertFalse(Files.exists(options.paths().dataRoot()));
    }

    @Test
    void approvedCleanReturnsAReceiptOnlyAfterTheOwnedArtifactsAreAbsent(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        lifecycle.cleanComplete = true;
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options, SetupSelection.defaults(), SetupOperation.CLEAN);

        SetupReceipt receipt = setup.install(plan, approval(plan), options);

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(1, lifecycle.cleans.get());
        assertEquals(0, lifecycle.provisions.get());
    }

    @Test
    void changedOwnedContentPreventsACleanReceipt(@TempDir Path temp) {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        lifecycle.cleanComplete = false;
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options, SetupSelection.defaults(), SetupOperation.CLEAN);

        IOException failure = assertThrows(IOException.class,
                () -> setup.install(plan, approval(plan), options));

        assertTrue(failure.getMessage().contains("no receipt"));
        assertEquals(1, lifecycle.cleans.get());
        assertEquals(0, lifecycle.provisions.get());
    }

    @Test
    void rollbackPlanningRejectsAnUnreviewedCachedVersionBeforeMutation(@TempDir Path temp) {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        InfrastructureSetupService setup = setup(lifecycle);

        IllegalStateException failure = assertThrows(IllegalStateException.class,
                () -> setup.plan(options, SetupSelection.defaults(), SetupOperation.ROLLBACK));

        assertTrue(failure.getMessage().contains("No reviewed managed local AI rollback candidate"));
        assertEquals(0, lifecycle.cleans.get());
        assertEquals(0, lifecycle.provisions.get());
    }

    @Test
    void reviewedRollbackPlanAndReceiptBindTheExactCachedPriorPair(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot ready = snapshot(options, ManagedLocalAiSnapshot.State.READY);
        FakeLifecycle lifecycle = new FakeLifecycle(ready);
        lifecycle.rollbackCandidate = ManagedLocalAiActivationHistory.from(
                ready, ManagedLocalAiManifest.loadDefault());
        InfrastructureSetupService setup = setup(lifecycle);

        SetupPlan plan = setup.plan(options, SetupSelection.defaults(), SetupOperation.ROLLBACK);

        assertEquals(2, plan.actions().size());
        assertTrue(plan.actions().stream().allMatch(action -> action.kind() == SetupActionKind.ROLLBACK));
        assertEquals("sha256:" + lifecycle.rollbackCandidate.runtimeSha256(),
                plan.actions().getFirst().checksum());
        assertEquals(lifecycle.rollbackCandidate.runtimeUrl(), plan.actions().getFirst().source().toString());
        assertEquals("sha256:" + lifecycle.rollbackCandidate.modelSha256(),
                plan.actions().get(1).checksum());
        assertEquals(Set.of(lifecycle.rollbackCandidate.runtimeLicense()),
                plan.actions().getFirst().requiredLicenses());
        assertEquals(Set.of(lifecycle.rollbackCandidate.modelLicense()),
                plan.actions().get(1).requiredLicenses());

        SetupReceipt receipt = setup.install(plan, approval(plan), options);

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(1, lifecycle.rollbacks.get());
        assertEquals(0, lifecycle.provisions.get());
        assertEquals(0, lifecycle.cleans.get());
    }

    @Test
    void rollbackDoesNotIssueAReceiptForTheWrongEffectiveModel(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot ready = snapshot(options, ManagedLocalAiSnapshot.State.READY);
        FakeLifecycle lifecycle = new FakeLifecycle(ready);
        lifecycle.rollbackCandidate = ManagedLocalAiActivationHistory.from(
                ready, ManagedLocalAiManifest.loadDefault());
        ManagedLocalAiSnapshot.Model model = ready.models().get(ready.selectedModelId());
        ManagedLocalAiSnapshot.Model wrongModel = new ManagedLocalAiSnapshot.Model(
                model.displayName(), model.tier(), model.license(), model.revision(), model.file(),
                "0".repeat(64), model.automatic(), model.eligible(), model.reasons(),
                model.requiredDiskBytes(), model.artifactBytes());
        lifecycle.rollbackResult = new ManagedLocalAiSnapshot(ready.state(), ready.action(), ready.cacheDirectory(),
                ready.enabled(), ready.transparentProvisioning(), ready.requestedModelId(), ready.selectedModelId(),
                ready.platform(), ready.runtimeId(), ready.runtimeVersion(), ready.runtimeLicense(),
                ready.runtimeAssetFile(), ready.runtimeAssetSha256(), ready.runtimeExecutable(),
                ready.runtimeAssetBytes(), ready.runtimeCacheHealth(), ready.modelCacheHealth(), ready.phase(),
                ready.completedBytes(), ready.totalBytes(), ready.effectiveMemoryBytes(), ready.cpuCount(),
                ready.freeDiskBytes(), Map.of(ready.selectedModelId(), wrongModel));
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options, SetupSelection.defaults(), SetupOperation.ROLLBACK);

        assertThrows(IOException.class, () -> setup.install(plan, approval(plan), options));
        assertEquals(1, lifecycle.rollbacks.get());
        assertEquals(0, lifecycle.provisions.get());
    }

    @Test
    void readyReviewedInstallRepublishesTheCurrentActivationAfterRollback(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);

        SetupReceipt receipt = setup.install(plan, approval(plan), options);

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(1, lifecycle.provisions.get());
    }

    @Test
    void rolledBackEffectivePairDoesNotSatisfyAnOfflineCurrentInstall(@TempDir Path temp) {
        SetupOptions options = options(temp).withOffline(true);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        lifecycle.reviewed = snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED);
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);

        IOException failure = assertThrows(IOException.class,
                () -> setup.install(plan, approval(plan), options));

        assertTrue(failure.getMessage().contains("offline setup cannot download"));
        assertEquals(0, lifecycle.provisions.get());
        assertEquals(0, lifecycle.rollbacks.get());
    }

    @Test
    void offlineReadyInstallReusesTheReviewedCacheWithoutDownloading(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp).withOffline(true);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);

        SetupReceipt receipt = setup.install(plan, approval(plan), options);

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(1, lifecycle.provisions.get());
        assertFalse(lifecycle.allowDownloads);
        assertEquals(0, lifecycle.downloadAttempts.get());
        assertEquals(0, lifecycle.rollbacks.get());
        assertEquals(0, lifecycle.cleans.get());
    }

    @Test
    void offlineReadyPreflightCannotAuthorizeDownloadsAfterAReadinessRace(@TempDir Path temp) {
        SetupOptions options = options(temp).withOffline(true);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        lifecycle.provisioned = snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED);
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);

        assertThrows(IOException.class, () -> setup.install(plan, approval(plan), options));

        assertFalse(lifecycle.allowDownloads);
        assertEquals(0, lifecycle.downloadAttempts.get());
    }

    @Test
    void cleanPlanningDoesNotDependOnEnablementOrHardwareEligibility(@TempDir Path temp) {
        SetupOptions options = options(temp);
        for (ManagedLocalAiSnapshot.State state : List.of(ManagedLocalAiSnapshot.State.DISABLED,
                ManagedLocalAiSnapshot.State.EXCLUDED, ManagedLocalAiSnapshot.State.UNSUPPORTED)) {
            FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, state));

            SetupPlan plan = setup(lifecycle).plan(options, SetupSelection.defaults(), SetupOperation.CLEAN);

            assertTrue(plan.actions().stream().allMatch(action -> action.kind() == SetupActionKind.CLEAN),
                    state.name());
            assertEquals(0, lifecycle.cleans.get());
            assertEquals(0, lifecycle.provisions.get());
        }
    }

    @Test
    void directProviderExecutionRejectsRollbackBeforeLifecycleMutation(@TempDir Path temp) {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        ManagedLocalAiSetupProvider provider = new ManagedLocalAiSetupProvider(ignored -> lifecycle);
        SetupPlan install = provider.plan(options, SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupPlan rollback = SetupPlan.create(install.profile(), install.platform(), install.architecture(),
                install.mode(), install.actions().stream().map(action -> new SetupAction(action.target(),
                        SetupActionKind.ROLLBACK, action.version(), action.source(), action.checksum(),
                        action.artifactBytes(), false, Set.of())).toList());

        assertThrows(IllegalArgumentException.class,
                () -> provider.install(rollback, approval(rollback), options));
        assertEquals(0, lifecycle.cleans.get());
        assertEquals(0, lifecycle.provisions.get());
    }

    @Test
    void directProviderRejectsAPartialCleanPlanBeforeMutation(@TempDir Path temp) {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.READY));
        ManagedLocalAiSetupProvider provider = new ManagedLocalAiSetupProvider(ignored -> lifecycle);
        SetupPlan exact = provider.plan(options, SetupSelection.defaults(), SetupOperation.CLEAN,
                SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupPlan partial = SetupPlan.bind(SetupPlan.create(exact.profile(), exact.platform(), exact.architecture(),
                exact.mode(), List.of(exact.actions().getFirst())), options.policyDigest());

        assertThrows(IllegalArgumentException.class,
                () -> provider.install(partial, approval(partial), options));
        assertEquals(0, lifecycle.cleans.get());
        assertEquals(0, lifecycle.provisions.get());
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
            String listed = String.join("\n", report.diagnostics()) + "\n"
                    + report.targets().stream().map(status -> status.detail()).reduce("", (left, right) -> left + "\n" + right);
            ManagedLocalAiStatusTest.assertReviewedInventory(listed, "windows-x86_64");
        }
        assertFalse(Files.exists(options.paths().cacheRoot()));
        assertFalse(Files.exists(options.paths().dataRoot()));
    }

    @Test
    void planAndStatusRejectACacheRootDifferentFromInference(@TempDir Path temp) {
        SetupOptions options = options(temp);
        ManagedLocalAiSnapshot elsewhere = snapshot(ManagedLocalAiSnapshot.State.NOT_PROVISIONED,
                temp.resolve("other-managed-cache").toAbsolutePath());
        InfrastructureSetupService setup = setup(new FakeLifecycle(elsewhere));

        IllegalArgumentException status = assertThrows(IllegalArgumentException.class,
                () -> setup.status(options));
        IllegalArgumentException plan = assertThrows(IllegalArgumentException.class,
                () -> setup.plan(options));
        assertTrue(status.getMessage().contains("cache root"));
        assertTrue(plan.getMessage().contains("cache root"));
        assertFalse(status.getMessage().contains(elsewhere.cacheDirectory().toString()));
        assertFalse(plan.getMessage().contains(elsewhere.cacheDirectory().toString()));
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

    @Test
    void interruptedInstallCancelsUnderlyingOperationAndReturnsNoReceipt(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED));
        lifecycle.leaveRunning = true;
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread install = Thread.ofPlatform().start(() -> {
            try {
                setup.install(plan, approval(plan), options);
            } catch (Throwable thrown) {
                failure.set(thrown);
            }
        });

        assertTrue(lifecycle.provisionStarted.await(5, TimeUnit.SECONDS));
        install.interrupt();
        install.join(5_000);

        assertFalse(install.isAlive());
        assertTrue(failure.get() instanceof IOException);
        assertTrue(failure.get().getMessage().contains("interrupted"));
        assertFalse(lifecycle.lastOperation.cancel(), "provider must already have requested cancellation");
        assertEquals(0, lifecycle.cleans.get());
        assertEquals(0, lifecycle.rollbacks.get());
        assertEquals(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, lifecycle.inspect().state());
    }

    @Test
    void failedProvisionCanBeRetriedByAnExplicitFreshInstall(@TempDir Path temp) throws Exception {
        SetupOptions options = options(temp);
        FakeLifecycle lifecycle = new FakeLifecycle(snapshot(options, ManagedLocalAiSnapshot.State.NOT_PROVISIONED));
        InfrastructureSetupService setup = setup(lifecycle);
        SetupPlan plan = setup.plan(options);
        lifecycle.failure = new IOException("temporary download failure");
        assertThrows(IOException.class, () -> setup.install(plan, approval(plan), options));

        lifecycle.failure = null;
        lifecycle.provisioned = snapshot(options, ManagedLocalAiSnapshot.State.READY);
        SetupReceipt receipt = setup.install(plan, approval(plan), options);

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(2, lifecycle.provisions.get());
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
        return snapshot(state, options.paths().cacheRoot());
    }

    private static ManagedLocalAiSnapshot snapshot(ManagedLocalAiSnapshot.State state, Path cache) {
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
        private ManagedLocalAiSnapshot reviewed;
        private final AtomicInteger provisions = new AtomicInteger();
        private final AtomicInteger cleans = new AtomicInteger();
        private final AtomicInteger rollbacks = new AtomicInteger();
        private final AtomicInteger downloadAttempts = new AtomicInteger();
        private ManagedLocalAiSnapshot provisioned;
        private ManagedLocalAiActivationHistory.Activation rollbackCandidate;
        private ManagedLocalAiSnapshot rollbackResult;
        private boolean cleanComplete = true;
        private List<ManagedLocalAiSnapshot> progressSnapshots = List.of();
        private Exception failure;
        private boolean leaveRunning;
        private final CountDownLatch provisionStarted = new CountDownLatch(1);
        private ManagedLocalAiOperation lastOperation;
        private boolean allowDownloads = true;

        private FakeLifecycle(ManagedLocalAiSnapshot inspected) {
            this.inspected = inspected;
            this.reviewed = inspected;
            this.provisioned = inspected;
            this.rollbackResult = inspected;
        }

        @Override
        public ManagedLocalAiSnapshot inspect() {
            return inspected;
        }

        @Override
        public ManagedLocalAiSnapshot inspectReviewed() {
            return reviewed;
        }

        @Override
        public ManagedLocalAiOperation provision(Consumer<ManagedLocalAiSnapshot> progress, boolean allowDownloads) {
            provisions.incrementAndGet();
            this.allowDownloads = allowDownloads;
            ManagedLocalAiOperation operation = new ManagedLocalAiOperation(inspected);
            lastOperation = operation;
            provisionStarted.countDown();
            if (leaveRunning) return operation;
            if (!allowDownloads && provisioned.state() != ManagedLocalAiSnapshot.State.READY) {
                operation.fail(new IOException("offline setup cannot download artifacts"));
            } else if (failure instanceof CancellationException) {
                operation.cancelled();
            } else if (failure != null) {
                operation.fail(failure);
            } else {
                if (provisioned.state() != ManagedLocalAiSnapshot.State.READY) {
                    downloadAttempts.incrementAndGet();
                }
                progressSnapshots.forEach(progress);
                progress.accept(provisioned);
                operation.complete(provisioned);
            }
            return operation;
        }

        @Override
        public boolean clean() {
            cleans.incrementAndGet();
            return cleanComplete;
        }

        @Override
        public ManagedLocalAiActivationHistory.Activation rollbackCandidate() {
            return rollbackCandidate;
        }

        @Override
        public ManagedLocalAiSnapshot rollback(ManagedLocalAiActivationHistory.Activation expected) {
            assertEquals(rollbackCandidate, expected);
            rollbacks.incrementAndGet();
            return rollbackResult;
        }
    }
}
