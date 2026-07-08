package com.shaft.mcp;

import com.shaft.capture.proxy.CaptureCertificateAuthority;
import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class MobileServiceConfigurationTest {
    @TempDir
    Path temp;

    @AfterEach
    void cleanup() {
        SHAFT.Properties.clearForCurrentThread();
        new EngineService().quitDriver();
    }

    @Test
    void webEmulationSetupBuildsCustomDeviceWithoutStartingRealDriver() {
        EngineService engineService = mock(EngineService.class);
        MobileService service = new MobileService(engineService, McpWorkspacePolicy.of(temp));

        McpMobileSessionResult result = service.initializeWebEmulation(
                "",
                "edge",
                "",
                390,
                844,
                3.0,
                "agent\"mobile",
                true);

        assertEquals("web-emulation", result.mode());
        assertEquals("390x844", result.deviceName());
        assertEquals("EDGE", result.browserName());
        assertTrue(result.active());
        assertTrue(result.warnings().isEmpty());
        assertEquals("390x844", result.deviceProfile().get("viewport"));
        assertEquals("3.0", result.deviceProfile().get("deviceScaleFactor"));
        assertEquals("agent\"mobile", result.deviceProfile().get("userAgent"));
        assertEquals("true", result.deviceProfile().get("touch"));
        String code = result.codeBlocks().getFirst().code();
        assertTrue(code.contains(".targetBrowserName(\"EDGE\")"));
        assertTrue(code.contains(".mobileEmulationIsCustomDevice(true)"));
        assertTrue(code.contains(".mobileEmulationWidth(390)"));
        assertTrue(code.contains(".mobileEmulationUserAgent(\"agent\\\"mobile\")"));
        verify(engineService).quitDriver();
        verify(engineService).initializeConfiguredDriver("mobile web emulation");
    }

    @Test
    void webEmulationSetupUsesDefaultDeviceAndRejectsUnsupportedBrowser() {
        EngineService engineService = mock(EngineService.class);
        MobileService service = new MobileService(engineService, McpWorkspacePolicy.of(temp));

        McpMobileSessionResult result = service.initializeWebEmulation(
                "",
                null,
                " ",
                0,
                0,
                0,
                null,
                false);

        assertEquals("Pixel 5", result.deviceName());
        assertEquals("CHROME", result.browserName());
        assertFalse(result.warnings().isEmpty());
        assertEquals("393x851", result.deviceProfile().get("viewport"));
        assertEquals("2.75", result.deviceProfile().get("deviceScaleFactor"));
        assertTrue(result.deviceProfile().get("userAgent").contains("Pixel 5"));
        assertTrue(result.codeBlocks().getFirst().code().contains(".mobileEmulationDeviceName(\"Pixel 5\")"));
        assertThrows(IllegalArgumentException.class,
                () -> service.initializeWebEmulation("", "firefox", "", 0, 0, 0, "", false));
    }

    @Test
    void nativeSetupBuildsPlatformDefaultsAndWarningsWithoutStartingAppium() {
        EngineService androidEngine = mock(EngineService.class);
        MobileService android = new MobileService(androidEngine, McpWorkspacePolicy.of(temp));

        McpMobileSessionResult androidResult = android.initializeNative(
                " android ",
                "Pixel",
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                "");

        assertEquals("native", androidResult.mode());
        assertEquals("Android", androidResult.platformName());
        assertTrue(androidResult.warnings().stream().anyMatch(warning -> warning.contains("appPackage")));
        String androidCode = androidResult.codeBlocks().getFirst().code();
        assertTrue(androidCode.contains(".targetPlatform(\"Android\")"));
        assertTrue(androidCode.contains(".executionAddress(\"http://127.0.0.1:4723\")"));
        assertTrue(androidCode.contains(".automationName(\"UiAutomator2\")"));
        verify(androidEngine).initializeConfiguredDriver("mobile native Android");

        EngineService iosEngine = mock(EngineService.class);
        MobileService ios = new MobileService(iosEngine, McpWorkspacePolicy.of(temp));
        McpMobileSessionResult iosResult = ios.initializeNative(
                "ios",
                "iPhone",
                "http://appium.local",
                "",
                "18.0",
                "udid-1",
                "",
                "",
                "",
                "");

        assertEquals("iOS", iosResult.platformName());
        assertTrue(iosResult.warnings().stream().anyMatch(warning -> warning.contains("bundleId")));
        assertTrue(iosResult.codeBlocks().getFirst().code().contains(".automationName(\"XCUITest\")"));
        verify(iosEngine).initializeConfiguredDriver("mobile native iOS");
        assertThrows(IllegalArgumentException.class,
                () -> ios.initializeNative("windows", "", "", "", "", "", "", "", "", ""));
    }

    @Test
    void recordingToolsDelegateToRecorderAndGenerateReplayCode() {
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        MobileService service = new MobileService(mock(EngineService.class), recorder);
        Path recording = temp.resolve("journey.json");

        McpMobileRecordingStatus started = service.recordStart(recording.toString(), "native", true);
        recorder.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                "login",
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                false);
        McpMobileRecordingStatus active = service.recordStatus();
        McpMobileRecordingStatus stopped = service.recordStop(false);
        McpMobileReplayResult blocks = service.recordingCodeBlocks(recording.toString(), "mobileDriver");

        assertTrue(started.active());
        assertEquals("native", active.mode());
        assertEquals(1, active.actionCount());
        assertFalse(stopped.active());
        assertEquals(1, stopped.actionCount());
        assertTrue(blocks.codeBlocks().getFirst().code()
                .contains("mobileDriver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));"));
    }

    @Test
    void recordAtTargetDelegatesToRecorderWithWorkspaceTarget() throws Exception {
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        MobileService service = new MobileService(mock(EngineService.class), recorder,
                McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("target-recording.json");
        Path target = temp.resolve("LoginPage.java");
        Files.writeString(target, """
                public class LoginPage {
                    public LoginPage open() {
                        return this;
                    }
                }
                """);

        service.recordStart(recording.toString(), "native", true);
        recorder.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                "login",
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                false);
        service.recordStop(false);

        McpMobileReplayResult result = service.recordAtTargetCodeBlocks(
                recording.toString(),
                "driver",
                target.toString(),
                "open");

        assertTrue(result.codeBlocks().stream().anyMatch(block -> block.id().equals("mobile-target-action-snippet")));
    }

    @Test
    void mobileApiRecordToolsStartStatusAndStopACaptureSession() {
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp),
                new MobileApiCaptureController(() -> new CaptureCertificateAuthority(temp.resolve("capture-ca"))));

        MobileApiCaptureStatus started = service.mobileApiRecordStart(
                "Android", "emulator-5554", "recordings/mobile-api.json");
        MobileApiCaptureStatus active = service.mobileApiRecordStatus();
        MobileApiCaptureStatus stopped = service.mobileApiRecordStop(false);

        assertTrue(started.active(), "mobile API capture failed to start: " + started.warnings());
        assertTrue(started.proxyPort() > 0);
        assertTrue(started.caCertificatePem().contains("BEGIN CERTIFICATE"));
        assertEquals(started.sessionId(), active.sessionId());
        assertFalse(stopped.active());
        assertTrue(Files.exists(temp.resolve("recordings/mobile-api.json")));
    }

    @Test
    void mobileApiRecordStartGeneratesATimestampedPathWhenBlank() {
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp),
                new MobileApiCaptureController(() -> new CaptureCertificateAuthority(temp.resolve("capture-ca"))));

        MobileApiCaptureStatus started = service.mobileApiRecordStart("iOS", "iPhone-Simulator", "");
        service.mobileApiRecordStop(true);

        assertTrue(started.active(), "mobile API capture failed to start: " + started.warnings());
        assertTrue(started.sessionId().startsWith("mobile-api-"));
    }

    @Test
    void replayRecordingRejectsUnsupportedRecordedAction() {
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        MobileService service = new MobileService(mock(EngineService.class), recorder);
        Path recording = temp.resolve("unsupported.json");
        service.recordStart(recording.toString(), "native", true);
        recorder.record("unsupported", null, "", Map.of(), "driver.unsupported();", "driver.unsupported();", false);
        service.recordStop(false);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.replayRecording(recording.toString(), "driver"));

        assertTrue(failure.getMessage().contains("Unsupported mobile recording action"));
    }
}
