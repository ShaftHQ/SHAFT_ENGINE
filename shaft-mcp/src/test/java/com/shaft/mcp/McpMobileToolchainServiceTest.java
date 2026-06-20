package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpMobileToolchainServiceTest {
    @TempDir
    Path temp;

    @Test
    void statusDiscoversAndroidDevicesCachedAvdsAndInspector() throws Exception {
        Path toolRoot = Files.createDirectories(temp.resolve("tools"));
        Path sdk = Files.createDirectories(temp.resolve("android-sdk"));
        Path avdHome = Files.createDirectories(temp.resolve("avd-home"));
        create(sdk.resolve("platform-tools"), "adb.exe");
        create(sdk.resolve("emulator"), "emulator.exe");
        create(sdk.resolve("cmdline-tools/latest/bin"), "sdkmanager.bat");
        create(sdk.resolve("cmdline-tools/latest/bin"), "avdmanager.bat");
        create(toolRoot.resolve("node/node-v24.17.0-win-x64"), "node.exe");
        create(toolRoot.resolve("node/node-v24.17.0-win-x64"), "npm.cmd");
        create(toolRoot.resolve("appium/node_modules/.bin"), "appium.cmd");
        Files.createDirectories(avdHome.resolve("Cached_Pixel.avd"));

        FakeRunner runner = new FakeRunner();
        runner.adbOutput = """
                List of devices attached
                emulator-5554 device product:sdk_gphone64_x86_64 model:Pixel_8 device:emu64
                R58M123 unauthorized
                """;
        runner.emulatorOutput = "Pixel_API_36\n";
        runner.appiumPluginOutput = "inspector\n";
        runner.appiumVersionOutput = "3.5.2\n";

        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("ANDROID_SDK_ROOT", sdk.toString(), "ANDROID_AVD_HOME", avdHome.toString(), "PATH", ""),
                toolRoot, "Windows 11", "amd64");

        McpMobileToolchainStatus status = service.status("Android");

        assertTrue(status.adbAvailable());
        assertTrue(status.appiumAvailable());
        assertTrue(status.appiumInspectorAvailable());
        assertEquals("3.5.2", status.appiumVersion());
        assertEquals(2, status.androidDevices().size());
        assertEquals("Pixel 8", status.androidDevices().getFirst().name());
        assertFalse(status.androidDevices().get(1).warnings().isEmpty());
        assertTrue(status.cachedAndroidEmulators().contains("Pixel_API_36"));
        assertTrue(status.cachedAndroidEmulators().contains("Cached_Pixel"));
        assertFalse(status.missingDependencies().contains("adb"));
    }

    @Test
    void defaultAndroidProposalUsesPortableCacheAndHostAbi() {
        Path toolRoot = temp.resolve("tools");
        McpMobileToolchainService service = new McpMobileToolchainService(new FakeRunner(),
                Map.of(), toolRoot, "Linux", "amd64");

        McpAndroidEmulatorProposal proposal = service.defaultAndroidProposal("", 0, "", "", "", 0, 0);

        assertEquals("pixel_8", proposal.deviceProfile());
        assertEquals(36, proposal.apiLevel());
        assertEquals("x86_64", proposal.abi());
        assertEquals(4096, proposal.ramMb());
        assertTrue(proposal.sdkRoot().toString().contains("android-sdk"));
        assertTrue(proposal.sdkPackages().contains("platforms;android-36"));
        assertTrue(proposal.commands().stream().anyMatch(command -> command.contains("sdkmanager")));
    }

    @Test
    void ensureAppiumInstallsPinnedPackagesWithCurrentCliSyntax() throws Exception {
        Path toolRoot = Files.createDirectories(temp.resolve("tools"));
        create(toolRoot.resolve("node/node-v24.17.0-win-x64"), "npm.cmd");
        create(toolRoot.resolve("appium/node_modules/.bin"), "appium.cmd");
        FakeRunner runner = new FakeRunner();
        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("PATH", ""), toolRoot, "Windows 11", "amd64");

        service.ensureAppium("Android");

        assertTrue(runner.commands.stream().anyMatch(command -> command.contains("appium@3.5.2")));
        assertTrue(runner.commands.stream().anyMatch(command -> command.equals(List.of(
                toolRoot.resolve("appium/node_modules/.bin/appium.cmd").toString(),
                "driver",
                "install",
                "--source=npm",
                "appium-uiautomator2-driver@7.6.2"))));
        assertTrue(runner.commands.stream().anyMatch(command -> command.equals(List.of(
                toolRoot.resolve("appium/node_modules/.bin/appium.cmd").toString(),
                "plugin",
                "install",
                "--source=npm",
                "appium-inspector-plugin@2026.5.1"))));
    }

    private static void create(Path directory, String fileName) throws Exception {
        Files.createDirectories(directory);
        Files.writeString(directory.resolve(fileName), "");
    }

    private static final class FakeRunner implements McpProcessRunner {
        private String adbOutput = "";
        private String emulatorOutput = "";
        private String appiumPluginOutput = "";
        private String appiumVersionOutput = "";
        private final List<List<String>> commands = new ArrayList<>();

        @Override
        public ProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                Duration timeout) {
            commands.add(command);
            String joined = String.join(" ", command);
            if (joined.contains("adb") && joined.contains("devices")) {
                return new ProcessResult(0, adbOutput, "", false);
            }
            if (joined.contains("emulator") && joined.contains("-list-avds")) {
                return new ProcessResult(0, emulatorOutput, "", false);
            }
            if (joined.contains("plugin") && joined.contains("list")) {
                return new ProcessResult(0, appiumPluginOutput, "", false);
            }
            if (joined.contains("appium") && joined.contains("--version")) {
                return new ProcessResult(0, appiumVersionOutput, "", false);
            }
            return new ProcessResult(0, "", "", false);
        }

        @Override
        public Process start(List<String> command, Path workingDirectory, Map<String, String> environment) {
            throw new UnsupportedOperationException("No process starts in unit tests.");
        }
    }
}
