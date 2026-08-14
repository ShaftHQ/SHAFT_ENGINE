package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InfrastructureSetupServiceTest {
    @Test
    void optionsDefaultToExternalAndNonMutating(@TempDir Path temp) {
        SetupOptions options = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp));

        assertEquals(SetupMode.EXTERNAL, options.mode());
        assertFalse(options.offline());
        assertFalse(options.autoStart());
        assertTrue(options.preferSystemTools());
        assertTrue(options.reuseOwnedProcesses());
        assertEquals(Duration.ofMinutes(2), options.startupTimeout());
        assertEquals(Duration.ofSeconds(30), options.shutdownTimeout());
        assertTrue(options.remoteEndpoint().isEmpty());
    }

    @Test
    void registryRejectsDuplicateAndMissingProviders() {
        SetupProvider provider = new FakeProvider();
        assertThrows(IllegalArgumentException.class, () -> new SetupProviderRegistry(List.of(provider, provider)));

        SetupProviderRegistry registry = new SetupProviderRegistry(List.of(provider));
        assertEquals(provider, registry.require(SetupProfile.REPORTING));
        assertThrows(IllegalArgumentException.class, () -> registry.require(SetupProfile.OCR));
        assertEquals(List.of(SetupProfile.REPORTING), registry.profiles());
    }

    @Test
    void coordinatorUsesReleasePlannerAndReportsProviderStatus(@TempDir Path temp) {
        SetupOptions options = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp))
                .withMode(SetupMode.MANAGED);
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);

        SetupPlan boundPlan = service.plan(options);
        assertEquals(SetupPlan.bind(ReportingSetupPlanner.plan(SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED), options.policyDigest()), service.plan(options));
        assertThrows(IllegalArgumentException.class, () -> SetupPlanJson.read(
                SetupPlanJson.write(boundPlan).replaceFirst(
                        "(?m)^  \"executionPolicyDigest\".*(?:\\R|$)", "")));
        SetupReport report = service.status(options);
        assertEquals(SetupProfile.REPORTING, report.profile());
        assertEquals(SetupReadiness.MISSING, report.readiness());
        assertEquals(List.of(SetupTarget.NODE, SetupTarget.ALLURE), report.targets().stream()
                .map(SetupStatus::target).toList());
    }

    @Test
    void builtInCoordinatorProvidesReadOnlyLighthouseStatusAndManagedPlan(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);

        SetupReport report = service.status(SetupOptions.defaults(SetupProfile.LIGHTHOUSE, paths));
        SetupPlan plan = service.plan(SetupOptions.defaults(SetupProfile.LIGHTHOUSE, paths)
                .withMode(SetupMode.MANAGED));

        assertEquals(SetupProfile.LIGHTHOUSE, report.profile());
        assertEquals(SetupReadiness.MISSING, report.readiness());
        assertEquals(List.of(SetupTarget.NODE, SetupTarget.LIGHTHOUSE), report.targets().stream()
                .map(SetupStatus::target).toList());
        assertEquals(SetupProfile.LIGHTHOUSE, plan.profile());
        assertEquals(List.of(SetupTarget.NODE, SetupTarget.LIGHTHOUSE), plan.actions().stream()
                .map(SetupAction::target).toList());
        SetupAction lighthouse = plan.actions().get(1);
        assertEquals(SetupActionKind.INSTALL, lighthouse.kind());
        assertEquals("13.4.1", lighthouse.version());
        assertEquals(URI.create("https://registry.npmjs.org/lighthouse/-/lighthouse-13.4.1.tgz"),
                lighthouse.source());
        assertEquals("sha256:110759ba9e863c024e214e9b08ed2b0344d89b492286227235d4dfb990dc3e54",
                lighthouse.checksum());
        assertEquals("sha256:5691359da63475578daef5b322a620311110ff6a09953cdb898bee314106c4dd",
                lighthouse.dependencyLockChecksum());
        assertTrue(lighthouse.requiredLicenses().isEmpty());
        assertTrue(Files.notExists(paths.cacheRoot()));
        assertTrue(Files.notExists(paths.dataRoot()));
    }

    @Test
    void builtInCoordinatorProvidesReadOnlyPlaywrightStatusAndExactManagedPlan(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupOptions external = SetupOptions.defaults(SetupProfile.PLAYWRIGHT, paths);
        SetupOptions managed = external.withMode(SetupMode.MANAGED);

        assertTrue(service.supports(SetupProfile.PLAYWRIGHT));
        SetupReport report = service.status(external);
        SetupPlan plan = service.plan(managed);

        assertEquals(SetupProfile.PLAYWRIGHT, report.profile());
        assertEquals(SetupReadiness.MISSING, report.readiness());
        assertEquals(List.of(SetupTarget.NODE, SetupTarget.PLAYWRIGHT_CHROMIUM,
                SetupTarget.PLAYWRIGHT_FIREFOX, SetupTarget.PLAYWRIGHT_WEBKIT, SetupTarget.FFMPEG),
                report.targets().stream().map(SetupStatus::target).toList());
        assertEquals(SetupProfile.PLAYWRIGHT, plan.profile());
        assertEquals(List.of(SetupTarget.NODE, SetupTarget.PLAYWRIGHT_CHROMIUM,
                SetupTarget.PLAYWRIGHT_FIREFOX, SetupTarget.PLAYWRIGHT_WEBKIT, SetupTarget.FFMPEG),
                plan.actions().stream().map(SetupAction::target).toList());
        assertTrue(plan.actions().stream().allMatch(action -> action.kind() == SetupActionKind.INSTALL));
        assertTrue(Files.notExists(paths.cacheRoot()));
        assertTrue(Files.notExists(paths.dataRoot()));
    }

    @Test
    void builtInCoordinatorProvidesCompleteAndroidPlanIncludingBuildToolsLicense(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_ANDROID, paths)
                .withMode(SetupMode.MANAGED);

        assertTrue(service.supports(SetupProfile.MOBILE_ANDROID));
        SetupPlan plan = service.plan(options);

        assertEquals(SetupProfile.MOBILE_ANDROID, plan.profile());
        assertEquals(List.of(SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_UIAUTOMATOR2_DRIVER,
                SetupTarget.ANDROID_SDK, SetupTarget.ANDROID_EMULATOR), plan.actions().stream()
                .map(SetupAction::target).toList());
        assertTrue(plan.actions().stream().allMatch(action -> action.kind() == SetupActionKind.INSTALL));
        SetupAction androidSdk = plan.actions().get(4);
        assertTrue(androidSdk.version().contains("build-tools;"),
                "The reviewed Android package set must bind build-tools so aapt2 is present.");
        assertTrue(androidSdk.requiredLicenses().contains("android-sdk-license"));
        assertTrue(Files.notExists(paths.cacheRoot()));
        assertTrue(Files.notExists(paths.dataRoot()));
    }

    @Test
    void bundledLighthouseManifestMatchesTheApprovedLock() throws Exception {
        byte[] packageJson;
        byte[] lock;
        try (var packageInput = InfrastructureSetupServiceTest.class.getResourceAsStream(
                "/com/shaft/infrastructure/lighthouse/package.json");
             var lockInput = InfrastructureSetupServiceTest.class.getResourceAsStream(
                     "/com/shaft/infrastructure/lighthouse/package-lock.json")) {
            packageJson = java.util.Objects.requireNonNull(packageInput).readAllBytes();
            lock = java.util.Objects.requireNonNull(lockInput).readAllBytes();
        }
        String canonical = new String(lock, StandardCharsets.UTF_8).replace("\r\n", "\n").replace('\r', '\n');

        assertTrue(new String(packageJson, StandardCharsets.UTF_8).contains("\"lighthouse\": \"13.4.1\""));
        assertTrue(canonical.contains("\"lighthouse\": \"13.4.1\""));
        var digest = java.security.MessageDigest.getInstance("SHA-256")
                .digest(canonical.getBytes(StandardCharsets.UTF_8));
        assertEquals(LighthouseSetupPlanner.LIGHTHOUSE_LOCK_SHA256, java.util.HexFormat.of().formatHex(digest));
    }

    @Test
    void remoteEndpointForcesDiagnosticPlanAndBlocksMutationBeforeProvider(@TempDir Path temp) {
        AtomicInteger mutations = new AtomicInteger();
        FakeProvider provider = new FakeProvider(mutations);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions options = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp))
                .withMode(SetupMode.MANAGED)
                .withRemoteEndpoint(URI.create("http://127.0.0.1:4444"));

        SetupPlan plan = service.plan(options);
        int callsAfterPlanning = provider.planCalls.get();
        assertEquals(SetupMode.EXTERNAL, plan.mode());
        assertTrue(plan.actions().stream().allMatch(action -> action.kind() == SetupActionKind.DIAGNOSE));
        assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertEquals(0, mutations.get());
        assertEquals(callsAfterPlanning, provider.planCalls.get());
    }

    @Test
    void staleApprovalAndMismatchedOptionsReachNoProviderMutation(@TempDir Path temp) {
        AtomicInteger mutations = new AtomicInteger();
        FakeProvider provider = new FakeProvider(mutations);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions managed = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp))
                .withMode(SetupMode.MANAGED);
        SetupPlan plan = service.plan(managed);
        int callsAfterPlanning = provider.planCalls.get();

        assertThrows(StaleSetupApprovalException.class, () -> service.install(plan,
                new SetupApproval("sha256:" + "0".repeat(64), Instant.EPOCH, Set.of()), managed));
        assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()),
                managed.withMode(SetupMode.HYBRID)));
        assertEquals(0, mutations.get());
        assertEquals(callsAfterPlanning, provider.planCalls.get());
    }

    @Test
    void approvalBindsDestinationRoots(@TempDir Path temp) {
        AtomicInteger mutations = new AtomicInteger();
        FakeProvider provider = new FakeProvider(mutations);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions rootA = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp.resolve("a")))
                .withMode(SetupMode.MANAGED);
        SetupOptions rootB = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp.resolve("b")))
                .withMode(SetupMode.MANAGED);
        SetupPlan plan = service.plan(rootA);

        assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), rootB));
        assertEquals(0, mutations.get());
    }

    @Test
    void approvalBindsEveryWritableChildPath(@TempDir Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        ShaftCachePaths pathsA = new ShaftCachePaths(cache, data, cache.resolve("downloads-a"),
                data.resolve("tools-a"), data.resolve("state-a"), data.resolve("receipts-a"));
        ShaftCachePaths pathsB = new ShaftCachePaths(cache, data, cache.resolve("downloads-b"),
                data.resolve("tools-b"), data.resolve("state-b"), data.resolve("receipts-b"));
        SetupOptions optionsA = SetupOptions.defaults(SetupProfile.REPORTING, pathsA).withMode(SetupMode.MANAGED);
        SetupOptions optionsB = SetupOptions.defaults(SetupProfile.REPORTING, pathsB).withMode(SetupMode.MANAGED);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(new FakeProvider())), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupPlan plan = service.plan(optionsA);

        assertFalse(optionsA.policyDigest().equals(optionsB.policyDigest()));
        assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), optionsB));
    }

    @Test
    void privilegedActionsAreRejectedBeforeProviderCallbacks(@TempDir Path temp) {
        AtomicInteger callbacks = new AtomicInteger();
        SetupProvider provider = new FakeProvider(callbacks) {
            @Override public SetupProfile profile() { return SetupProfile.MOBILE_WINDOWS; }
            @Override
            public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
                callbacks.incrementAndGet();
                throw new AssertionError("privileged denial must not call the provider");
            }
        };
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupAction action = new SetupAction(SetupTarget.WINAPPDRIVER, SetupActionKind.INSTALL, "1.2.1",
                URI.create("https://example.invalid/wad"), "sha256:" + "0".repeat(64), true, Set.of());
        SetupPlan plan = SetupPlan.bind(SetupPlan.create(SetupProfile.MOBILE_WINDOWS, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED, List.of(action)),
                SetupOptions.defaults(SetupProfile.MOBILE_WINDOWS, paths(temp)).withMode(SetupMode.MANAGED)
                        .policyDigest());
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_WINDOWS, paths(temp))
                .withMode(SetupMode.MANAGED);

        assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertEquals(0, callbacks.get());
    }

    @Test
    void providerOutputIdentityIsChecked(@TempDir Path temp) {
        SetupProvider provider = new FakeProvider() {
            @Override
            public SetupReport status(SetupOptions options, SetupPlatform platform,
                                      SetupArchitecture architecture) {
                return new SetupReport(1, SetupProfile.OCR, SetupReadiness.READY, List.of(), List.of());
            }
        };
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        assertThrows(IllegalStateException.class, () -> service.status(
                SetupOptions.defaults(SetupProfile.REPORTING, paths(temp))));
    }

    @Test
    void managedEnvironmentReleaseCanRetryAfterFailure() {
        AtomicInteger releases = new AtomicInteger();
        ManagedEnvironment environment = new ManagedEnvironment(SetupProfile.REPORTING,
                new SetupReceipt("sha256:" + "0".repeat(64), Instant.EPOCH, List.of()),
                java.util.Optional.empty(), java.util.Map.of(), () -> {
                    if (releases.incrementAndGet() == 1) throw new IllegalStateException("busy");
                });

        assertThrows(IllegalStateException.class, environment::close);
        assertFalse(environment.isClosed());
        environment.close();
        assertTrue(environment.isClosed());
        environment.close();
        assertEquals(2, releases.get());
    }

    @Test
    void rejectedManagedEnvironmentIsReleasedAndCleanupFailureIsSuppressed(@TempDir Path temp) {
        AtomicInteger releases = new AtomicInteger();
        SetupOptions options = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp))
                .withMode(SetupMode.MANAGED);
        SetupProvider provider = new FakeProvider() {
            @Override
            public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions ignored) {
                return new ManagedEnvironment(SetupProfile.OCR,
                        new SetupReceipt(plan.digest(), Instant.EPOCH, plan.actions()),
                        java.util.Optional.empty(), java.util.Map.of(), () -> {
                            releases.incrementAndGet();
                            throw new IllegalStateException("cleanup failed");
                        });
            }
        };
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupPlan plan = service.plan(options);

        IllegalStateException failure = assertThrows(IllegalStateException.class, () -> service.start(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertEquals(1, releases.get());
        assertEquals(1, failure.getSuppressed().length);
        assertEquals("cleanup failed", failure.getSuppressed()[0].getMessage());
    }

    @Test
    void partialStartedReceiptIsRejectedAndReleased(@TempDir Path temp) {
        AtomicInteger releases = new AtomicInteger();
        SetupOptions options = SetupOptions.defaults(SetupProfile.REPORTING, paths(temp))
                .withMode(SetupMode.MANAGED);
        SetupProvider provider = new FakeProvider() {
            @Override
            public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions ignored) {
                return new ManagedEnvironment(plan.profile(),
                        new SetupReceipt(plan.digest(), Instant.EPOCH, List.of()),
                        java.util.Optional.empty(), java.util.Map.of(), releases::incrementAndGet);
            }
        };
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupPlan plan = service.plan(options);

        assertThrows(IllegalStateException.class, () -> service.start(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertEquals(1, releases.get());
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static class FakeProvider implements SetupProvider {
        private final AtomicInteger mutations;
        private final AtomicInteger planCalls = new AtomicInteger();

        private FakeProvider() {
            this(new AtomicInteger());
        }

        private FakeProvider(AtomicInteger mutations) {
            this.mutations = mutations;
        }

        @Override
        public SetupProfile profile() {
            return SetupProfile.REPORTING;
        }

        @Override
        public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
            planCalls.incrementAndGet();
            return ReportingSetupPlanner.plan(platform, architecture, options.effectiveMode());
        }

        @Override
        public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
            return new SetupReport(1, profile(), SetupReadiness.MISSING, List.of(),
                    List.of("fake provider"));
        }

        @Override
        public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) {
            mutations.incrementAndGet();
            return new SetupReceipt(plan.digest(), Instant.EPOCH, plan.actions());
        }
    }
}
