package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.lang.reflect.Method;
import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpMobileInspectorRecordingServiceTest {
    @TempDir
    Path temp;

    @Test
    void prepareRequiresAppDetailsBeforeStart() {
        McpMobileInspectorRecordingService service = service();

        McpMobileInspectorPlan plan = service.prepare(
                "Android",
                "recordings/native.json",
                true,
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                0,
                "",
                "",
                "",
                0,
                0,
                true);

        assertFalse(plan.readyToStart());
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("appPackage")));
        assertTrue(plan.toolchainStatus().missingDependencies().contains("android platform-tools"));
        assertThrows(IllegalStateException.class,
                () -> service.start(plan.confirmationToken(), "", false));
    }

    @Test
    void prepareBuildsProvisioningProposalWhenNoAndroidDeviceExists() {
        String originalHome = System.getProperty("user.home");
        System.setProperty("user.home", temp.toString());
        try {
            McpMobileInspectorRecordingService service = service();

            McpMobileInspectorPlan plan = service.prepare(
                    "Android",
                    "recordings/native.json",
                    false,
                    "",
                    "com.example",
                    ".MainActivity",
                    "",
                    "",
                    "Pixel 8",
                    "",
                    "",
                    36,
                    "",
                    "",
                    "",
                    0,
                    0,
                    true);

            assertTrue(plan.readyToStart());
            assertTrue(plan.willProvisionAndroidEmulator());
            assertTrue(plan.confirmationRequired());
            assertTrue(plan.androidEmulatorProposal().sdkPackages().contains("platform-tools"));
            assertTrue(plan.appiumCapabilities().containsKey("appium:appPackage"));
            assertTrue(plan.nextSteps().stream().anyMatch(step -> step.contains("confirmation token")));
        } finally {
            System.setProperty("user.home", originalHome);
        }
    }

    @Test
    void prepareSurfacesToolchainDiagnosticWarnings() {
        McpMobileInspectorRecordingService service = service();

        McpMobileInspectorPlan plan = service.prepare(
                "Android",
                "recordings/native.json",
                false,
                "",
                "com.example",
                ".MainActivity",
                "",
                "",
                "Pixel 8",
                "",
                "",
                0,
                "",
                "",
                "",
                0,
                0,
                false);

        assertFalse(plan.readyToStart());
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("adb was not found")
                && warning.contains("platform-tools")));
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("Android emulator")
                && warning.contains("sdkmanager")));
    }

    @Test
    void prepareBlocksIosInspectorOnNonMacHost() {
        McpMobileInspectorRecordingService service = service();

        McpMobileInspectorPlan plan = service.prepare(
                "iOS",
                "recordings/native-ios.json",
                false,
                "",
                "",
                "",
                "com.example.ios",
                "",
                "iPhone 15",
                "",
                "",
                0,
                "",
                "",
                "",
                0,
                0,
                false);

        assertFalse(plan.readyToStart());
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("ios-host")
                && warning.contains("requires macOS")));
    }

    @Test
    void rejectsUnknownConfirmationToken() {
        McpMobileInspectorRecordingService service = service();

        assertThrows(IllegalArgumentException.class,
                () -> service.start("missing", "", false));
    }

    @Test
    void appiumInspectorWrapperUsesShaftUiTheme() throws Exception {
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, () -> false);
        McpAppiumInspectorProxy proxy = new McpAppiumInspectorProxy(
                URI.create("http://127.0.0.1:4723/"),
                commandRecorder,
                (action, checkpoint) -> new McpMobileInspectorRecordingStatus(
                        true, false, "Android", "", "", false, temp.resolve("recording.json"),
                        "http://127.0.0.1:3000", "http://127.0.0.1:4723", 3, List.of(), List.of()),
                "{\"platformName\":\"Android\"}");
        Method wrapperHtml = McpAppiumInspectorProxy.class.getDeclaredMethod("wrapperHtml");
        wrapperHtml.setAccessible(true);

        String html = (String) wrapperHtml.invoke(proxy);

        assertTrue(html.contains("--shaft-primary"), html);
        assertTrue(html.contains("status-chip"), html);
        assertTrue(html.contains("Recorder ready"), html);
    }

    private McpMobileInspectorRecordingService service() {
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        McpMobileToolchainService toolchain = new McpMobileToolchainService(new NoopRunner(),
                Map.of("PATH", "", "ANDROID_AVD_HOME", temp.resolve("avd").toString()),
                temp.resolve("tools"), "Windows 11", "amd64");
        return new McpMobileInspectorRecordingService(McpWorkspacePolicy.of(temp), recorder, toolchain);
    }

    private static final class NoopRunner implements McpProcessRunner {
        @Override
        public ProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                Duration timeout) {
            return new ProcessResult(0, "", "", false);
        }

        @Override
        public Process start(List<String> command, Path workingDirectory, Map<String, String> environment) {
            throw new UnsupportedOperationException("No process starts in unit tests.");
        }
    }
}
