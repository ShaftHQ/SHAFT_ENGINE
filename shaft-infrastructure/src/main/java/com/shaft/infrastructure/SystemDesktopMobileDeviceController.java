package com.shaft.infrastructure;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Map;
import java.util.Set;

/** Command-backed iOS Simulator boot/shutdown used only when SHAFT starts an exact UDID. */
final class SystemDesktopMobileDeviceController implements DesktopMobileDeviceController {
    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final SetupPlatform platform;
    private final Path workingDirectory;
    private final AndroidCommandRunner runner;

    SystemDesktopMobileDeviceController(SetupPlan plan, ShaftCachePaths paths) {
        this(plan.platform(), Path.of(System.getProperty("user.dir")).toAbsolutePath().normalize(),
                AndroidCommandRunner.system(paths, plan.platform(), plan.architecture()));
    }

    SystemDesktopMobileDeviceController(SetupPlatform platform, Path workingDirectory,
                                        AndroidCommandRunner runner) {
        this.platform = java.util.Objects.requireNonNull(platform, "platform");
        this.workingDirectory = java.util.Objects.requireNonNull(workingDirectory, "workingDirectory");
        this.runner = java.util.Objects.requireNonNull(runner, "runner");
    }

    @Override
    public SimulatorState simulatorState(String udid) throws IOException {
        if (platform != SetupPlatform.MACOS) {
            throw new IOException("iOS Simulator lifecycle is available only on macOS.");
        }
        ReportingSetupService.ProcessResult result = run(java.util.List.of("xcrun", "simctl", "list", "devices",
                "--json"));
        if (result.exitCode() != 0) {
            throw new IOException("xcrun simctl could not enumerate Simulator devices.");
        }
        JsonNode tree = JSON.readTree(result.output());
        final SimulatorState[] found = {SimulatorState.MISSING};
        tree.path("devices").forEach(runtime -> runtime.forEach(device -> {
            if (udid.equalsIgnoreCase(device.path("udid").asText())) {
                found[0] = "Booted".equalsIgnoreCase(device.path("state").asText())
                        ? SimulatorState.BOOTED : SimulatorState.SHUTDOWN;
            }
        }));
        return found[0];
    }

    @Override
    public void bootSimulator(String udid) throws IOException {
        requireSuccess(run(java.util.List.of("xcrun", "simctl", "boot", udid)),
                "Failed to boot iOS Simulator " + udid);
    }

    @Override
    public void shutdownSimulator(String udid) throws IOException {
        requireSuccess(run(java.util.List.of("xcrun", "simctl", "shutdown", udid)),
                "Failed to shut down SHAFT-booted iOS Simulator " + udid);
    }

    private ReportingSetupService.ProcessResult run(java.util.List<String> command) throws IOException {
        return runner.run(command, workingDirectory, Map.of(), Set.of(), null, null, Duration.ofSeconds(30));
    }

    private static void requireSuccess(ReportingSetupService.ProcessResult result, String message)
            throws IOException {
        if (result.exitCode() != 0) throw new IOException(message + ": " + result.output());
    }
}
