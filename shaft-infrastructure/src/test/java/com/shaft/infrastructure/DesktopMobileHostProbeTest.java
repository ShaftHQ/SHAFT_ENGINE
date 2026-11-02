package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesktopMobileHostProbeTest {
    @Test
    void iosProbeRequiresFullXcodeAndTheExactAvailableSimulator(@TempDir Path temp) {
        String udid = "00000000-0000-0000-0000-000000000001";
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                command.getFirst().equals("xcodebuild")
                        ? result(0, "Xcode 16.4\nBuild version 16F6")
                        : result(0, "{\"devices\":{\"com.apple.CoreSimulator.SimRuntime.iOS-18-5\":["
                        + "{\"udid\":\"" + udid + "\",\"name\":\"iPhone 16\",\"isAvailable\":true}]}}");
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.MACOS, temp, runner);
        SetupPlan plan = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, new SetupSelection(List.of("simulator_"
                + udid.replace('-', '_'))));

        SetupStatus xcode = probe.status(plan.actions().get(4));
        SetupStatus simulator = probe.status(plan.actions().get(5));

        assertEquals(SetupReadiness.READY, xcode.readiness());
        assertEquals("16.4", xcode.detectedVersion());
        assertEquals(SetupReadiness.READY, simulator.readiness());
        assertEquals(udid, simulator.detectedVersion());
    }

    @Test
    void iosProbeMatchesHostUdidRegardlessOfHexCase(@TempDir Path temp) {
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                result(0, "{\"devices\":{\"com.apple.CoreSimulator.SimRuntime.iOS-18-5\":["
                        + "{\"udid\":\"00000000-0000-0000-0000-00000000000A\",\"name\":\"iPhone 16\","
                        + "\"isAvailable\":true}]}}");
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.MACOS, temp, runner);
        SetupAction simulator = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, new SetupSelection(List.of("simulator_00000000_0000_0000_0000_00000000000a")))
                .actions().getLast();

        SetupStatus status = probe.status(simulator);

        assertEquals(SetupReadiness.READY, status.readiness());
        assertEquals("00000000-0000-0000-0000-00000000000A", status.detectedVersion());
    }

    @Test
    void deviceControllerMatchesHostUdidRegardlessOfHexCase(@TempDir Path temp) throws Exception {
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                result(0, "{\"devices\":{\"r\":[{\"udid\":\"AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE\","
                        + "\"state\":\"Booted\"}]}}");
        SystemDesktopMobileDeviceController devices = new SystemDesktopMobileDeviceController(
                SetupPlatform.MACOS, temp, runner);

        assertEquals(DesktopMobileDeviceController.SimulatorState.BOOTED,
                devices.simulatorState("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"));
    }

    @Test
    void iosProbeRejectsASelectedSimulatorThatIsNotAvailable(@TempDir Path temp) {
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                result(0, "{\"devices\":{}}");
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.MACOS, temp, runner);
        SetupAction simulator = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, new SetupSelection(List.of(
                "simulator_00000000_0000_0000_0000_000000000001"))).actions().getLast();

        SetupStatus status = probe.status(simulator);

        assertEquals(SetupReadiness.MISSING, status.readiness());
        assertTrue(status.detail().contains("00000000-0000-0000-0000-000000000001"));
    }

    @Test
    void iosProbeRejectsXcodeBelowTheReviewedMinimum(@TempDir Path temp) {
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                result(0, "Xcode 14.2\nBuild version 14C18");
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.MACOS, temp, runner);
        SetupAction xcode = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, SetupSelection.defaults()).actions().get(4);

        SetupStatus status = probe.status(xcode);

        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertEquals("14.2", status.detectedVersion());
        assertTrue(status.detail().contains("14.3"));
    }

    @Test
    void iosProbeRejectsCommandLineToolsWithoutFullXcode(@TempDir Path temp) {
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                result(1, "xcode-select: error: tool 'xcodebuild' requires Xcode");
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.MACOS, temp, runner);
        SetupAction xcode = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, SetupSelection.defaults()).actions().get(4);

        SetupStatus status = probe.status(xcode);

        assertEquals(SetupReadiness.MISSING, status.readiness());
        assertTrue(status.detail().contains("Full Xcode"));
        assertTrue(status.detail().contains("Command Line Tools"));
    }

    @Test
    void windowsProbeChecksDeveloperModeBeforeWinAppDriver(@TempDir Path temp) {
        List<List<String>> commands = new ArrayList<>();
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) -> {
            commands.add(command);
            return result(0, "0");
        };
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.WINDOWS, temp, runner);
        SetupAction wad = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults()).actions().getLast();

        SetupStatus status = probe.status(wad);

        assertEquals(SetupReadiness.MISSING, status.readiness());
        assertTrue(status.detail().contains("Developer Mode"));
        assertEquals(1, commands.size());
    }

    @Test
    void windowsProbeRequiresTheReviewedWinAppDriverVersion(@TempDir Path temp) {
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                command.getLast().contains("AllowDevelopmentWithoutDevLicense")
                        ? result(0, "1")
                        : result(0, "C:\\Program Files (x86)\\Windows Application Driver\\WinAppDriver.exe|1.2.1.0");
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.WINDOWS, temp, runner);
        SetupAction wad = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults()).actions().getLast();

        SetupStatus status = probe.status(wad);

        assertEquals(SetupReadiness.READY, status.readiness());
        assertEquals("1.2.1", status.detectedVersion());
    }

    @Test
    void windowsProbeRejectsAnUnexpectedWinAppDriverBuild(@TempDir Path temp) {
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                command.getLast().contains("AllowDevelopmentWithoutDevLicense")
                        ? result(0, "1")
                        : result(0, "C:\\Program Files (x86)\\Windows Application Driver\\WinAppDriver.exe|1.2.1.9");
        SystemDesktopMobileHostProbe probe = new SystemDesktopMobileHostProbe(SetupPlatform.WINDOWS, temp, runner);
        SetupAction wad = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults()).actions().getLast();

        SetupStatus status = probe.status(wad);

        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertTrue(status.detail().contains("1.2.1.9"));
    }

    private static ReportingSetupService.ProcessResult result(int exitCode, String output) {
        return new ReportingSetupService.ProcessResult(exitCode, output);
    }
}
