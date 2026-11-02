package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class DesktopMobileHostAcceptanceTest {
    static final String IOS_GATE = "SHAFT_SETUP_IOS_ACCEPTANCE";
    static final String IOS_UDID = "SHAFT_SETUP_IOS_UDID";
    static final String WINDOWS_GATE = "SHAFT_SETUP_WINDOWS_ACCEPTANCE";

    @Test
    void iosGateRequiresExplicitTrue() {
        assertTrue(enabled(Map.of(IOS_GATE, "true"), IOS_GATE));
        assertTrue(enabled(Map.of(IOS_GATE, "TRUE"), IOS_GATE));
        assertFalse(enabled(Map.of(), IOS_GATE));
        assertFalse(enabled(Map.of(IOS_GATE, "1"), IOS_GATE));
        assertFalse(enabled(Map.of(IOS_GATE, "yes"), IOS_GATE));
    }

    @Test
    void windowsGateRequiresExplicitTrue() {
        assertTrue(enabled(Map.of(WINDOWS_GATE, "true"), WINDOWS_GATE));
        assertFalse(enabled(Map.of(), WINDOWS_GATE));
        assertFalse(enabled(Map.of(WINDOWS_GATE, "1"), WINDOWS_GATE));
    }

    @Test
    void iosStartsAppiumAndLeavesAPrebootedSimulator() throws Exception {
        assumeTrue(enabled(System.getenv(), IOS_GATE), "Set " + IOS_GATE + "=true to run iOS host acceptance.");
        assumeTrue(SetupPlatform.current() == SetupPlatform.MACOS, "iOS host acceptance runs on macOS only.");
        String udid = System.getenv(IOS_UDID);
        assumeTrue(udid != null && !udid.isBlank(), "Set " + IOS_UDID + " to an existing Simulator UDID.");
        InfrastructureSetupService service = InfrastructureSetupService.builtIn();
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_IOS, ShaftCachePaths.current())
                .withMode(SetupMode.MANAGED);
        SetupSelection selection = new SetupSelection(List.of(
                "simulator_" + udid.toLowerCase().replace('-', '_')));
        assumeReady(service, options, selection);
        SetupPlan plan = service.plan(options, selection);
        SystemDesktopMobileDeviceController devices =
                new SystemDesktopMobileDeviceController(plan, options.paths());
        assumeTrue(devices.simulatorState(udid) == DesktopMobileDeviceController.SimulatorState.BOOTED,
                "Pre-boot the selected Simulator; this test must not own boot/shutdown.");
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.now(), Set.of());
        try (ManagedEnvironment environment = service.start(plan, approval, options)) {
            assertTrue(environment.endpoint().isPresent());
        }
        assertTrue(devices.simulatorState(udid) == DesktopMobileDeviceController.SimulatorState.BOOTED);
    }

    @Test
    void windowsStartsAppiumOnlyWhenWinAppDriverAlreadyExists() throws Exception {
        assumeTrue(enabled(System.getenv(), WINDOWS_GATE),
                "Set " + WINDOWS_GATE + "=true to run Windows host acceptance.");
        assumeTrue(SetupPlatform.current() == SetupPlatform.WINDOWS,
                "Windows host acceptance runs on Windows only.");
        InfrastructureSetupService service = InfrastructureSetupService.builtIn();
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_WINDOWS, ShaftCachePaths.current())
                .withMode(SetupMode.MANAGED);
        SetupSelection selection = SetupSelection.defaults();
        assumeReady(service, options, selection);
        SetupPlan plan = service.plan(options, selection);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.now(), Set.of());
        try (ManagedEnvironment environment = service.start(plan, approval, options)) {
            assertTrue(environment.endpoint().isPresent());
        }
    }

    static boolean enabled(Map<String, String> env, String name) {
        return "true".equalsIgnoreCase(env.getOrDefault(name, ""));
    }

    private static void assumeReady(InfrastructureSetupService service, SetupOptions options,
                                    SetupSelection selection) {
        SetupReport report = service.status(options, selection);
        assumeTrue(report.readiness() == SetupReadiness.READY,
                "Install MOBILE_IOS/MOBILE_WINDOWS first; host acceptance does not download runtimes. "
                        + report.diagnostics());
    }
}
