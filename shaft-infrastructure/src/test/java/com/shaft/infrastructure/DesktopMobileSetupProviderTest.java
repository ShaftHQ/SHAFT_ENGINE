package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesktopMobileSetupProviderTest {
    @ParameterizedTest
    @EnumSource(value = SetupProfile.class, names = {"MOBILE_IOS", "MOBILE_WINDOWS"})
    void managedProviderInstallsAndReportsTheExactReceipt(SetupProfile profile, @TempDir Path temp)
            throws Exception {
        RecordingOperations operations = new RecordingOperations();
        SetupProvider provider = profile == SetupProfile.MOBILE_IOS
                ? new IosSetupProvider((paths, plan, offline) -> operations)
                : new WindowsSetupProvider((paths, plan, offline) -> operations);
        SetupPlatform platform = profile == SetupProfile.MOBILE_IOS ? SetupPlatform.MACOS : SetupPlatform.WINDOWS;
        SetupArchitecture architecture = profile == SetupProfile.MOBILE_IOS
                ? SetupArchitecture.ARM64 : SetupArchitecture.X64;
        SetupSelection selection = profile == SetupProfile.MOBILE_IOS
                ? new SetupSelection(List.of("simulator_00000000_0000_0000_0000_000000000001", "port_4725"))
                : new SetupSelection(List.of("port_4725"));
        SetupOptions options = managed(profile, temp.resolve(profile.name().toLowerCase()));
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), platform, architecture);
        SetupPlan plan = service.plan(options, selection);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());

        assertEquals(SetupReadiness.DEGRADED,
                service.status(options, selection).readiness());
        SetupReceipt receipt = service.install(plan, approval, options);

        assertEquals(plan.digest(), receipt.planDigest());
        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(SetupReadiness.READY,
                service.status(options, selection).readiness());
        assertTrue(operations.events.contains("host-preflight"));
        assertTrue(operations.events.contains("locked-preflight"));
        assertTrue(operations.events.contains("install:" + plan.actions().getLast().target()));
    }

    @ParameterizedTest
    @EnumSource(value = SetupProfile.class, names = {"MOBILE_IOS", "MOBILE_WINDOWS"})
    void providerRejectsAPlanWhoseReleaseManifestWasChanged(SetupProfile profile, @TempDir Path temp) {
        RecordingOperations operations = new RecordingOperations();
        SetupProvider provider = profile == SetupProfile.MOBILE_IOS
                ? new IosSetupProvider((paths, plan, offline) -> operations)
                : new WindowsSetupProvider((paths, plan, offline) -> operations);
        SetupPlatform platform = profile == SetupProfile.MOBILE_IOS ? SetupPlatform.MACOS : SetupPlatform.WINDOWS;
        SetupArchitecture architecture = profile == SetupProfile.MOBILE_IOS
                ? SetupArchitecture.ARM64 : SetupArchitecture.X64;
        SetupSelection selection = profile == SetupProfile.MOBILE_IOS
                ? new SetupSelection(List.of("simulator_00000000_0000_0000_0000_000000000001"))
                : SetupSelection.defaults();
        SetupOptions options = managed(profile, temp.resolve(profile.name().toLowerCase()));
        SetupPlan original = provider.plan(options, selection, platform, architecture);
        List<SetupAction> changedActions = new ArrayList<>(original.actions());
        SetupAction originalServer = changedActions.get(1);
        changedActions.set(1, new SetupAction(originalServer.target(), originalServer.kind(), "3.6.1",
                originalServer.source(), originalServer.checksum(), originalServer.artifactBytes(),
                originalServer.dependencyLockChecksum(), originalServer.privileged(),
                originalServer.requiredLicenses()));
        SetupPlan changed = SetupPlan.bind(SetupPlan.create(profile, platform, architecture,
                SetupMode.MANAGED, changedActions), options.policyDigest());

        assertThrows(IllegalArgumentException.class, () -> provider.install(changed,
                new SetupApproval(changed.digest(), Instant.EPOCH, Set.of()), options));
        assertTrue(operations.events.isEmpty());
    }

    @Test
    void builtInCoordinatorProvidesIosAndWindowsPlans(@TempDir Path temp) {
        InfrastructureSetupService ios = InfrastructureSetupService.builtIn(
                SetupPlatform.MACOS, SetupArchitecture.ARM64);
        InfrastructureSetupService windows = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);

        assertTrue(ios.supports(SetupProfile.MOBILE_IOS));
        assertTrue(windows.supports(SetupProfile.MOBILE_WINDOWS));
        assertFalse(ios.supports(SetupProfile.MOBILE_WINDOWS));
        assertFalse(windows.supports(SetupProfile.MOBILE_IOS));

        SetupPlan iosPlan = ios.plan(managed(SetupProfile.MOBILE_IOS, temp.resolve("ios")),
                new SetupSelection(List.of("simulator_00000000_0000_0000_0000_000000000001")));
        SetupPlan windowsPlan = windows.plan(managed(SetupProfile.MOBILE_WINDOWS, temp.resolve("windows")));

        assertEquals(List.of(SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                        SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_XCUITEST_DRIVER,
                        SetupTarget.XCODE, SetupTarget.IOS_SIMULATOR),
                iosPlan.actions().stream().map(SetupAction::target).toList());
        assertEquals(List.of(SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                        SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_WINDOWS_DRIVER,
                        SetupTarget.WINAPPDRIVER),
                windowsPlan.actions().stream().map(SetupAction::target).toList());
    }

    @Test
    void selectedPortIsValidatedAndBoundIntoThePlan(@TempDir Path temp) {
        InfrastructureSetupService windows = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupOptions options = managed(SetupProfile.MOBILE_WINDOWS, temp.resolve("windows"));

        SetupPlan defaultPort = windows.plan(options, new SetupSelection(List.of("port_4723")));
        SetupPlan alternatePort = windows.plan(options, new SetupSelection(List.of("port_4725")));

        assertNotEquals(defaultPort.digest(), alternatePort.digest());
        assertThrows(IllegalArgumentException.class,
                () -> windows.plan(options, new SetupSelection(List.of("port_invalid"))));
    }

    @Test
    void coordinatorReconstructsExactIosSelectionFromAnApprovedPlan(@TempDir Path temp) {
        InfrastructureSetupService ios = InfrastructureSetupService.builtIn(
                SetupPlatform.MACOS, SetupArchitecture.ARM64);
        SetupSelection selection = new SetupSelection(List.of(
                "simulator_00000000_0000_0000_0000_000000000001", "port_4725"));
        SetupPlan plan = ios.plan(managed(SetupProfile.MOBILE_IOS, temp.resolve("ios")), selection);

        assertEquals(selection, ios.selectionFromPlan(plan));
    }

    private static SetupOptions managed(SetupProfile profile, Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return SetupOptions.defaults(profile, new ShaftCachePaths(cache, data, cache.resolve("downloads"),
                data.resolve("tools"), data.resolve("state"), data.resolve("receipts")))
                .withMode(SetupMode.MANAGED);
    }

    private static final class RecordingOperations implements DesktopMobileToolchainOperations {
        private final List<String> events = new ArrayList<>();

        @Override
        public void hostPreflight(List<SetupAction> actions) {
            events.add("host-preflight");
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean offline) {
            events.add("locked-preflight");
        }

        @Override
        public void install(SetupAction action) throws IOException {
            events.add("install:" + action.target());
        }

        @Override
        public SetupStatus status(SetupAction action) {
            events.add("status:" + action.target());
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }
    }
}
