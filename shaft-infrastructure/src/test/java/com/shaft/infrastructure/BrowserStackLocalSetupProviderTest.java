package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BrowserStackLocalSetupProviderTest {
    @Test
    void builtInCoordinatorProvidesBrowserStackLocalPlan(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions options = managed(temp);

        assertTrue(service.supports(SetupProfile.BROWSERSTACK_LOCAL));
        SetupPlan plan = service.plan(options);

        assertEquals(List.of(SetupTarget.BROWSERSTACK_LOCAL),
                plan.actions().stream().map(SetupAction::target).toList());
        assertEquals(SetupActionKind.INSTALL, plan.actions().getFirst().kind());
        assertEquals("8.9", plan.actions().getFirst().version());
        assertTrue(plan.actions().getFirst().source().toString().contains("/v8.9/BrowserStackLocal-linux-x64.zip"));
        assertEquals("sha256:6f10351faef2af198fb8188c5ce9b4ec449cf9ca5f119f0d999b5de2aa03166d",
                plan.actions().getFirst().checksum());
    }

    @Test
    void linuxArm64IsUnsupported() {
        assertThrows(IllegalArgumentException.class, () -> BrowserStackLocalSetupPlanner.plan(
                SetupPlatform.LINUX, SetupArchitecture.ARM64, SetupMode.MANAGED));
    }

    @Test
    void macosUsesDarwinX64EvenOnArm() {
        SetupPlan plan = BrowserStackLocalSetupPlanner.plan(
                SetupPlatform.MACOS, SetupArchitecture.ARM64, SetupMode.MANAGED);
        assertTrue(plan.actions().getFirst().source().toString().contains("darwin-x64"));
    }

    @Test
    void managedProviderInstallsTheExactReceipt(@TempDir Path temp) throws Exception {
        RecordingOperations operations = new RecordingOperations();
        InfrastructureSetupService service = coordinator(operations);
        SetupOptions options = managed(temp);
        SetupPlan plan = service.plan(options);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());

        assertEquals(SetupReadiness.MISSING, service.status(options).readiness());
        SetupReceipt receipt = service.install(plan, approval, options);

        assertEquals(plan.digest(), receipt.planDigest());
        assertEquals(SetupReadiness.READY, service.status(options).readiness());
        assertTrue(operations.events.contains("install:" + SetupTarget.BROWSERSTACK_LOCAL));
    }

    @Test
    void externalModeCannotInstall(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        SetupOptions options = SetupOptions.defaults(SetupProfile.BROWSERSTACK_LOCAL, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")));
        SetupPlan plan = service.plan(options);

        assertThrows(IllegalArgumentException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertFalse(Files.exists(options.paths().receipts().resolve("browserstack-local.json")));
    }

    private static InfrastructureSetupService coordinator(RecordingOperations operations) {
        return new InfrastructureSetupService(new SetupProviderRegistry(
                List.of(new BrowserStackLocalSetupProvider((paths, plan, offline) -> operations))),
                SetupPlatform.LINUX, SetupArchitecture.X64);
    }

    private static SetupOptions managed(Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return SetupOptions.defaults(SetupProfile.BROWSERSTACK_LOCAL, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")))
                .withMode(SetupMode.MANAGED);
    }

    private static final class RecordingOperations implements BrowserStackLocalToolchainOperations {
        private final List<String> events = new ArrayList<>();
        private boolean installed;

        @Override
        public void hostPreflight(List<SetupAction> actions) {
            events.add("host-preflight");
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean offline) {
            events.add("locked-preflight");
        }

        @Override
        public void install(SetupAction action) {
            events.add("install:" + action.target());
            installed = true;
        }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(),
                    installed ? SetupReadiness.READY : SetupReadiness.MISSING,
                    installed ? action.version() : "", installed ? "fixture" : "Binary is missing.");
        }
    }
}
