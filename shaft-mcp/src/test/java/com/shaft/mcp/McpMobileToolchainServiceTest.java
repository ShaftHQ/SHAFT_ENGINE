package com.shaft.mcp;

import com.shaft.properties.internal.Internal;
import com.shaft.infrastructure.ManagedEnvironment;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupReceipt;
import org.aeonbits.owner.ConfigFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpMobileToolchainServiceTest {
    private static final Internal INTERNAL = ConfigFactory.create(Internal.class);

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
        create(toolRoot.resolve(windowsNodeArchive()), "node.exe");
        create(toolRoot.resolve(windowsNodeArchive()), "npm.cmd");
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
        runner.appiumVersionOutput = INTERNAL.appiumServerVersion() + "\n";

        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("ANDROID_SDK_ROOT", sdk.toString(), "ANDROID_AVD_HOME", avdHome.toString(), "PATH", ""),
                toolRoot, "Windows 11", "amd64");

        McpMobileToolchainStatus status = service.status("Android");

        assertTrue(status.adbAvailable());
        assertTrue(status.appiumAvailable());
        assertTrue(status.appiumInspectorAvailable());
        assertEquals(INTERNAL.appiumServerVersion(), status.appiumVersion());
        assertEquals(2, status.androidDevices().size());
        assertEquals("Pixel 8", status.androidDevices().getFirst().name());
        assertFalse(status.androidDevices().get(1).warnings().isEmpty());
        assertTrue(status.cachedAndroidEmulators().contains("Pixel_API_36"));
        assertTrue(status.cachedAndroidEmulators().contains("Cached_Pixel"));
        assertFalse(status.missingDependencies().contains("adb"));
        assertTrue(diagnostic(status, "appium").available());
        assertTrue(diagnostic(status, "appium").detectedPath().endsWith("appium.cmd"));
        assertEquals(INTERNAL.appiumServerVersion(), diagnostic(status, "appium").detectedVersion());
        assertTrue(diagnostic(status, "appium-inspector-plugin").available());
        assertEquals("", diagnostic(status, "appium-inspector-plugin").detectedVersion());
    }

    @Test
    void androidStatusDiscoversTheExactSharedInfrastructureLayout(@TempDir Path root) throws Exception {
        Path toolRoot = Files.createDirectories(root.resolve("tools"));
        Path sdk = Files.createDirectories(toolRoot.resolve("android-sdk/15859902-api36-x86_64"));
        Path avdHome = Files.createDirectories(toolRoot.resolve("android-avd"));
        create(sdk.resolve("platform-tools"), "adb.exe");
        create(sdk.resolve("emulator"), "emulator.exe");
        create(sdk.resolve("cmdline-tools/latest/bin"), "sdkmanager.bat");
        create(sdk.resolve("cmdline-tools/latest/bin"), "avdmanager.bat");
        Path node = toolRoot.resolve("node").resolve(INTERNAL.nodeLtsVersion()).resolve("windows-x64");
        create(node, "node.exe");
        create(node, "npm.cmd");
        Path appium = toolRoot.resolve("appium").resolve(INTERNAL.appiumServerVersion());
        create(appium.resolve("node_modules/.bin"), "appium.cmd");
        Files.createDirectories(appium.resolve("node_modules/appium-inspector-plugin"));
        Files.createDirectories(avdHome.resolve("Shared_Pixel.avd"));
        FakeRunner runner = new FakeRunner();
        runner.appiumPluginOutput = "inspector\n";
        runner.appiumVersionOutput = INTERNAL.appiumServerVersion() + "\n";
        McpMobileToolchainService.AndroidSetupOwner owner = new McpMobileToolchainService.AndroidSetupOwner() {
            @Override public Path sdkRoot(int apiLevel, String abi) { return sdk; }
            @Override public Path avdHome() { return avdHome; }
            @Override public void install(McpAndroidEmulatorProposal proposal) { }
            @Override public ManagedEnvironment start(McpAndroidEmulatorProposal proposal) {
                throw new UnsupportedOperationException();
            }
        };
        McpMobileToolchainService service = new McpMobileToolchainService(runner, Map.of("PATH", ""),
                toolRoot, "Windows 11", "amd64", owner);

        McpMobileToolchainStatus status = service.status("Android");

        assertEquals(sdk, status.androidSdkRoot());
        assertEquals(avdHome, status.androidAvdHome());
        assertEquals(appium, status.appiumRoot());
        assertTrue(status.nodeAvailable());
        assertTrue(status.npmAvailable());
        assertTrue(status.appiumAvailable());
        assertTrue(status.appiumInspectorAvailable());
        assertTrue(status.adbAvailable());
        assertTrue(status.emulatorAvailable());
        assertTrue(status.sdkManagerAvailable());
        assertTrue(status.avdManagerAvailable());
        assertTrue(status.cachedAndroidEmulators().contains("Shared_Pixel"));
    }

    @Test
    void statusReportsRepairableDiagnosticsForMissingAndroidToolchain() {
        Path toolRoot = temp.resolve("tools");
        McpMobileToolchainService service = new McpMobileToolchainService(new FakeRunner(),
                Map.of("PATH", "", "ANDROID_SDK_ROOT", temp.resolve("android-sdk").toString()),
                toolRoot, "Windows 11", "amd64");

        McpMobileToolchainStatus status = service.status("Android");

        McpMobileToolchainDiagnostic adb = diagnostic(status, "adb");
        assertFalse(adb.available());
        assertEquals("", adb.detectedPath());
        assertTrue(adb.failureCause().contains("adb was not found"));
        assertTrue(adb.repairGuidance().contains("platform-tools"));
        assertTrue(status.missingDependencies().contains("android platform-tools"));
        assertFalse(diagnostic(status, "node").available());
        assertFalse(diagnostic(status, "npm").available());
        assertFalse(diagnostic(status, "appium").available());
        assertFalse(diagnostic(status, "appium-inspector-plugin").available());

        McpMobileToolchainDiagnostic emulator = diagnostic(status, "android-emulator");
        assertFalse(emulator.available());
        assertTrue(emulator.failureCause().contains("Android emulator"));
        assertTrue(emulator.repairGuidance().contains("sdkmanager"));

        McpMobileToolchainDiagnostic sdkManager = diagnostic(status, "android-sdkmanager");
        assertFalse(sdkManager.available());
        assertTrue(sdkManager.repairGuidance().contains("cmdline-tools"));
        assertFalse(diagnostic(status, "android-avdmanager").available());
    }

    @Test
    void statusReportsIosHostConstraintOnNonMac() {
        McpMobileToolchainService service = new McpMobileToolchainService(new FakeRunner(),
                Map.of("PATH", ""), temp.resolve("tools"), "Windows 11", "amd64");

        McpMobileToolchainStatus status = service.status("iOS");

        McpMobileToolchainDiagnostic iosHost = diagnostic(status, "ios-host");
        assertFalse(iosHost.available());
        assertTrue(iosHost.failureCause().contains("requires macOS"));
        assertTrue(iosHost.repairGuidance().contains("Xcode"));
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
        assertTrue(proposal.sdkPackages().contains("build-tools;36.0.0"));
        assertTrue(proposal.commands().stream().anyMatch(command -> command.contains("sdkmanager")));
    }

    @Test
    void toolchainServiceInitializesPropertiesWhenConstructed() {
        Path toolRoot = temp.resolve("tools");
        FakeRunner runner = new FakeRunner();

        // Construct the service - should not throw even if SHAFT.Properties.internal is null
        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("PATH", ""), toolRoot, "Windows 11", "amd64");

        // Call status() to verify properties were initialized and accessor methods work
        McpMobileToolchainStatus status = service.status("Android");

        // If we reach here without NullPointerException, properties are initialized
        assertFalse(status.platformName().isBlank());
        assertEquals("Android", status.platformName());
    }

    @Test
    void toolchainStatusReturnsNormalResultWhenPropertiesInitialized() throws Exception {
        Path toolRoot = Files.createDirectories(temp.resolve("tools"));
        Path sdk = Files.createDirectories(temp.resolve("android-sdk"));
        Path avdHome = Files.createDirectories(temp.resolve("avd-home"));
        create(sdk.resolve("platform-tools"), "adb.exe");
        create(toolRoot.resolve(windowsNodeArchive()), "node.exe");

        FakeRunner runner = new FakeRunner();
        runner.adbOutput = "List of devices attached\nemulator-5554 device\n";

        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("ANDROID_SDK_ROOT", sdk.toString(), "ANDROID_AVD_HOME", avdHome.toString(), "PATH", ""),
                toolRoot, "Windows 11", "amd64");

        // This should not throw and should return valid diagnostics
        McpMobileToolchainStatus status = service.status("Android");

        assertFalse(status.diagnostics().isEmpty());
        assertTrue(status.diagnostics().stream().anyMatch(d -> d.dependencyId().equals("node")));
    }

    @Test
    void androidEnsureAppiumDoesNotOwnMutableNpmOrExtensionInstallation() throws Exception {
        Path toolRoot = Files.createDirectories(temp.resolve("tools"));
        create(toolRoot.resolve(windowsNodeArchive()), "npm.cmd");
        create(toolRoot.resolve("appium/node_modules/.bin"), "appium.cmd");
        FakeRunner runner = new FakeRunner();
        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("PATH", ""), toolRoot, "Windows 11", "amd64");

        service.ensureAppium("Android");

        assertTrue(runner.commands.isEmpty());
    }

    @Test
    void confirmedAndroidProposalDoesNotRunLegacySdkmanagerOrLicenseCommands() throws Exception {
        Path toolRoot = Files.createDirectories(temp.resolve("tools"));
        create(toolRoot.resolve("android-sdk/cmdline-tools/latest/bin"), "sdkmanager.bat");
        create(toolRoot.resolve("android-sdk/cmdline-tools/latest/bin"), "avdmanager.bat");
        FakeRunner runner = new FakeRunner();
        java.util.concurrent.atomic.AtomicInteger sharedInstalls = new java.util.concurrent.atomic.AtomicInteger();
        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("PATH", ""), toolRoot, "Windows 11", "amd64", new McpMobileToolchainService.AndroidSetupOwner() {
                    @Override public Path sdkRoot(int apiLevel, String abi) { return toolRoot.resolve("shared-sdk"); }
                    @Override public Path avdHome() { return toolRoot.resolve("shared-avd"); }
                    @Override public void install(McpAndroidEmulatorProposal proposal) { sharedInstalls.incrementAndGet(); }
                    @Override public ManagedEnvironment start(McpAndroidEmulatorProposal proposal) {
                        throw new UnsupportedOperationException();
                    }
                });
        McpAndroidEmulatorProposal proposal = service.defaultAndroidProposal("", 0, "", "", "", 0, 0);

        service.ensureAndroidEmulator(proposal);

        assertTrue(runner.commands.isEmpty());
        assertEquals(1, sharedInstalls.get());
    }

    @Test
    void androidRuntimeStartIsOwnedBySharedInfrastructureWithoutLegacyProcessLaunch() throws Exception {
        Path toolRoot = Files.createDirectories(temp.resolve("tools"));
        FakeRunner runner = new FakeRunner();
        ManagedEnvironment expected = new ManagedEnvironment(SetupProfile.MOBILE_ANDROID,
                new SetupReceipt("digest", Instant.EPOCH, List.of()),
                Optional.of(java.net.URI.create("http://127.0.0.1:4723/")),
                Map.of("ANDROID_SERIAL", "emulator-5554"), () -> { });
        McpMobileToolchainService service = new McpMobileToolchainService(runner,
                Map.of("PATH", ""), toolRoot, "Windows 11", "amd64", new McpMobileToolchainService.AndroidSetupOwner() {
                    @Override public Path sdkRoot(int apiLevel, String abi) { return toolRoot.resolve("shared-sdk"); }
                    @Override public Path avdHome() { return toolRoot.resolve("shared-avd"); }
                    @Override public void install(McpAndroidEmulatorProposal proposal) { }
                    @Override public ManagedEnvironment start(McpAndroidEmulatorProposal proposal) { return expected; }
                });
        McpAndroidEmulatorProposal proposal = service.defaultAndroidProposal("", 0, "", "", "", 0, 0);

        ManagedEnvironment actual = service.startAndroidRuntime(proposal);

        assertSame(expected, actual);
        assertTrue(runner.commands.isEmpty());
    }

    private static String windowsNodeArchive() {
        return "node/node-v" + INTERNAL.nodeLtsVersion() + "-win-x64";
    }

    private static void create(Path directory, String fileName) throws Exception {
        Files.createDirectories(directory);
        Files.writeString(directory.resolve(fileName), "");
    }

    private static McpMobileToolchainDiagnostic diagnostic(McpMobileToolchainStatus status, String dependencyId) {
        return status.diagnostics().stream()
                .filter(diagnostic -> dependencyId.equals(diagnostic.dependencyId()))
                .findFirst()
                .orElseThrow();
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
