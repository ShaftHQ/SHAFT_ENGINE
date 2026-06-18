package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

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
                "driver.touch().tap(AppiumBy.accessibilityId(\"login\"));",
                "driver.touch().tap(AppiumBy.accessibilityId(\"login\"));",
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
                .contains("mobileDriver.touch().tap(AppiumBy.accessibilityId(\"login\"));"));
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
