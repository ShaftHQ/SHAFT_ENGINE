package com.shaft.infrastructure;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Command-backed, read-only diagnosis of Xcode/Simulator and Windows desktop prerequisites. */
final class SystemDesktopMobileHostProbe implements DesktopMobileHostProbe {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final Pattern XCODE_VERSION = Pattern.compile("(?m)^Xcode\\s+([0-9]+(?:\\.[0-9]+)*)");
    private static final String DEVELOPER_MODE_COMMAND =
            "(Get-ItemPropertyValue -LiteralPath 'HKLM:\\\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\AppModelUnlock' "
                    + "-Name AllowDevelopmentWithoutDevLicense -ErrorAction Stop)";
    private static final String WAD_COMMAND =
            "$p=Join-Path ${env:ProgramFiles(x86)} 'Windows Application Driver\\WinAppDriver.exe';"
                    + "if (!(Test-Path -LiteralPath $p -PathType Leaf)) { exit 2 };"
                    + "$v=(Get-Item -LiteralPath $p).VersionInfo.ProductVersion; Write-Output ($p+'|'+$v)";

    private final SetupPlatform platform;
    private final Path workingDirectory;
    private final AndroidCommandRunner runner;

    SystemDesktopMobileHostProbe(SetupPlatform platform, Path workingDirectory, AndroidCommandRunner runner) {
        this.platform = java.util.Objects.requireNonNull(platform, "platform");
        this.workingDirectory = java.util.Objects.requireNonNull(workingDirectory, "workingDirectory");
        this.runner = java.util.Objects.requireNonNull(runner, "runner");
    }

    @Override
    public SetupStatus status(SetupAction action) {
        try {
            return switch (action.target()) {
                case XCODE -> xcodeStatus(action);
                case IOS_SIMULATOR -> simulatorStatus(action);
                case WINAPPDRIVER -> winAppDriverStatus(action);
                default -> new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Unexpected desktop-mobile host prerequisite.");
            };
        } catch (IOException | RuntimeException failure) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", safeMessage(failure));
        }
    }

    private SetupStatus xcodeStatus(SetupAction action) throws IOException {
        if (platform != SetupPlatform.MACOS) return missing(action, "Full Xcode is available only on macOS.");
        ReportingSetupService.ProcessResult result = run(List.of("xcodebuild", "-version"));
        if (result.exitCode() != 0) {
            return missing(action, "Full Xcode is required; Command Line Tools alone are insufficient.");
        }
        Matcher matcher = XCODE_VERSION.matcher(result.output());
        if (!matcher.find()) return degraded(action, "xcodebuild returned an unrecognized version.");
        String version = matcher.group(1);
        if (!isAtLeast(version, 14, 3)) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, version,
                    "Xcode 14.3 or newer is required.");
        }
        return ready(action, version, "Full Xcode is available.");
    }

    private SetupStatus simulatorStatus(SetupAction action) throws IOException {
        if (platform != SetupPlatform.MACOS) return missing(action, "iOS Simulator is available only on macOS.");
        ReportingSetupService.ProcessResult result = run(List.of("xcrun", "simctl", "list", "devices", "--json"));
        if (result.exitCode() != 0) return missing(action, "xcrun simctl could not enumerate Simulator devices.");
        JsonNode tree = JSON.readTree(result.output());
        List<String> available = new ArrayList<>();
        tree.path("devices").forEach(runtime -> runtime.forEach(device -> {
            if (device.path("isAvailable").asBoolean(false) && !device.path("udid").asText().isBlank()) {
                available.add(device.path("udid").asText());
            }
        }));
        String requested = metadata(action.version(), "udid");
        if (requested.equals("existing")) {
            if (available.isEmpty()) return missing(action, "No available iOS Simulator device exists.");
            return ready(action, available.getFirst(), "An existing iOS Simulator device is available.");
        }
        if (!available.contains(requested)) {
            return missing(action, "Selected iOS Simulator is unavailable: " + requested);
        }
        return ready(action, requested, "Selected iOS Simulator is available.");
    }

    private SetupStatus winAppDriverStatus(SetupAction action) throws IOException {
        if (platform != SetupPlatform.WINDOWS) return missing(action, "WinAppDriver is available only on Windows.");
        ReportingSetupService.ProcessResult developerMode = run(powershell(DEVELOPER_MODE_COMMAND));
        if (developerMode.exitCode() != 0 || !developerMode.output().strip().equals("1")) {
            return missing(action, "Windows Developer Mode must be enabled by the host administrator.");
        }
        ReportingSetupService.ProcessResult wad = run(powershell(WAD_COMMAND));
        if (wad.exitCode() != 0) {
            return missing(action, "WinAppDriver 1.2.1 must be installed separately on the host.");
        }
        String output = wad.output().strip();
        int separator = output.lastIndexOf('|');
        String version = separator < 0 ? "" : output.substring(separator + 1).strip();
        if (!(version.equals(DesktopMobileSetupPlanner.WINAPPDRIVER_VERSION)
                || version.equals(DesktopMobileSetupPlanner.WINAPPDRIVER_VERSION + ".0"))) {
            return degraded(action, "WinAppDriver version " + version + " does not match the required "
                    + DesktopMobileSetupPlanner.WINAPPDRIVER_VERSION + '.');
        }
        return ready(action, DesktopMobileSetupPlanner.WINAPPDRIVER_VERSION,
                "Developer Mode and WinAppDriver are ready.");
    }

    private ReportingSetupService.ProcessResult run(List<String> command) throws IOException {
        return runner.run(command, workingDirectory, Map.of(), Set.of(), null, null, Duration.ofSeconds(30));
    }

    private static List<String> powershell(String script) {
        return List.of("powershell.exe", "-NoProfile", "-NonInteractive", "-Command", script);
    }

    private static String metadata(String version, String key) {
        for (String entry : version.split(",")) {
            int separator = entry.indexOf('=');
            if (separator > 0 && entry.substring(0, separator).equals(key)) return entry.substring(separator + 1);
        }
        throw new IllegalArgumentException("Missing " + key + " in desktop-mobile plan metadata.");
    }

    private static SetupStatus ready(SetupAction action, String version, String detail) {
        return new SetupStatus(action.target(), SetupReadiness.READY, version, detail);
    }

    private static SetupStatus missing(SetupAction action, String detail) {
        return new SetupStatus(action.target(), SetupReadiness.MISSING, "", detail);
    }

    private static SetupStatus degraded(SetupAction action, String detail) {
        return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", detail);
    }

    private static String safeMessage(Throwable failure) {
        String message = failure.getMessage();
        return message == null || message.isBlank() ? failure.getClass().getSimpleName() : message;
    }

    private static boolean isAtLeast(String version, int requiredMajor, int requiredMinor) {
        String[] parts = version.split("\\.");
        try {
            int major = Integer.parseInt(parts[0]);
            int minor = parts.length > 1 ? Integer.parseInt(parts[1]) : 0;
            return major > requiredMajor || major == requiredMajor && minor >= requiredMinor;
        } catch (NumberFormatException malformed) {
            return false;
        }
    }
}
