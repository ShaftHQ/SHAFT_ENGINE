package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * The agent-performed codegen flow (capture_start_codegen, perform the described actions,
 * capture_stop, generate) requires element tools to drive the recorded browser: without the
 * bridge every element_* call failed with "No active browser session" during an active capture
 * session (issue #3429).
 */
class EngineServiceCaptureBridgeTest {
    @AfterEach
    void unregisterBridge() {
        EngineService.registerCaptureDriverBridge(null);
    }

    @Test
    void elementToolsFallBackToTheActiveCaptureSessionDriver() {
        SHAFT.GUI.WebDriver captureDriver = mock(SHAFT.GUI.WebDriver.class);
        EngineService.registerCaptureDriverBridge(() -> captureDriver);

        assertSame(captureDriver, EngineService.getDriver());
    }

    @Test
    void missingDriverAndCaptureSessionExplainsBothStartPaths() {
        EngineService.registerCaptureDriverBridge(() -> null);

        IllegalStateException failure = assertThrows(IllegalStateException.class, EngineService::getDriver);

        assertTrue(failure.getMessage().contains("driver_initialize"), failure.getMessage());
        assertTrue(failure.getMessage().contains("capture_start_codegen"), failure.getMessage());
    }

    @Test
    void throwingBridgeIsTreatedAsNoCaptureSession() {
        EngineService.registerCaptureDriverBridge(() -> {
            throw new IllegalStateException("capture manager unavailable");
        });

        assertThrows(IllegalStateException.class, EngineService::getDriver);
    }
}
