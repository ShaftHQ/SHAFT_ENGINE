package com.shaft.mcp;

import com.shaft.capture.runtime.CaptureManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Per-engine dispatch matrix for the unified {@code capture_start}/{@code capture_status}/
 * {@code capture_stop}/{@code capture_step_delete}/{@code capture_step_reorder} tools (design doc
 * amendment A3): asserts each tool reaches the correct underlying recorder for
 * {@link ActiveEngine#WEB}/{@link ActiveEngine#NONE} (SHAFT's own CDP-managed-browser Capture
 * session), {@link ActiveEngine#PLAYWRIGHT}, and {@link ActiveEngine#MOBILE_NATIVE}/
 * {@link ActiveEngine#MOBILE_WEB}, following the same {@code mockStatic}-free
 * {@code EngineService.setActiveEngine(...)} pattern as {@link BrowserServiceDispatchTest}
 * (capture dispatch never needs {@code EngineService.getDriver()}).
 */
class CaptureServiceDispatchTest {
    @TempDir
    Path temp;

    @AfterEach
    void resetActiveEngine() {
        EngineService.setActiveEngine(null);
    }

    private CaptureService service(PlaywrightService playwrightService, MobileService mobileService) {
        return new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService(),
                playwrightService,
                mobileService);
    }

    @Test
    void startDispatchesToWebCdpManagedBrowserWhenWebOrNoEngineIsActive() throws Exception {
        Path outside = Files.createTempFile("outside-capture-dispatch", ".json");
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        CaptureService service = service(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.WEB);

        // The CDP-managed-browser path validates the output path is inside the workspace before
        // ever launching a browser (McpWorkspacePolicy#output), so this proves capture_start
        // reached the WEB dispatch branch without needing a real browser launch.
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.start("https://example.test", "chrome", outside.toString(), true, ""));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void startDispatchesToPlaywrightRecordStartWhenPlaywrightEngineIsActive() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileRecordingStatus recorderStatus = new McpMobileRecordingStatus(true, null, "codegen", 0, false, List.of());
        when(playwrightService.recordStart("recording.json", "codegen", false)).thenReturn(recorderStatus);
        CaptureService service = service(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        McpCaptureUnionStatus result = service.start(null, null, "recording.json", null, "codegen");

        verify(playwrightService).recordStart("recording.json", "codegen", false);
        assertEquals(ActiveEngine.PLAYWRIGHT, result.engine());
        assertEquals(recorderStatus, result.playwrightStatus());
        assertNull(result.webStatus());
        assertNull(result.mobileStatus());
    }

    @Test
    void startDispatchesToMobileRecordStartWhenMobileNativeEngineIsActive() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileRecordingStatus recorderStatus = new McpMobileRecordingStatus(true, null, "journey", 0, false, List.of());
        when(mobileService.recordStart("recording.json", "journey", false)).thenReturn(recorderStatus);
        CaptureService service = service(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);

        McpCaptureUnionStatus result = service.start(null, null, "recording.json", null, "journey");

        verify(mobileService).recordStart("recording.json", "journey", false);
        assertEquals(ActiveEngine.MOBILE_NATIVE, result.engine());
        assertEquals(recorderStatus, result.mobileStatus());
        assertNull(result.webStatus());
        assertNull(result.playwrightStatus());
    }

    @Test
    void statusDispatchesPerActiveEngine() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileRecordingStatus pwStatus = new McpMobileRecordingStatus(true, null, "pw", 1, false, List.of());
        McpMobileRecordingStatus mobileStatus = new McpMobileRecordingStatus(true, null, "mobile", 2, false, List.of());
        when(playwrightService.recordStatus()).thenReturn(pwStatus);
        when(mobileService.recordStatus()).thenReturn(mobileStatus);
        CaptureService service = service(playwrightService, mobileService);

        EngineService.setActiveEngine(ActiveEngine.WEB);
        McpCaptureUnionStatus webResult = service.status();
        assertEquals(ActiveEngine.WEB, webResult.engine());
        assertTrue(webResult.webStatus() != null);
        assertNull(webResult.playwrightStatus());
        assertNull(webResult.mobileStatus());

        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);
        McpCaptureUnionStatus pwResult = service.status();
        assertEquals(ActiveEngine.PLAYWRIGHT, pwResult.engine());
        assertEquals(pwStatus, pwResult.playwrightStatus());
        assertNull(pwResult.webStatus());

        EngineService.setActiveEngine(ActiveEngine.MOBILE_WEB);
        McpCaptureUnionStatus mobileResult = service.status();
        assertEquals(ActiveEngine.MOBILE_WEB, mobileResult.engine());
        assertEquals(mobileStatus, mobileResult.mobileStatus());
        assertNull(mobileResult.webStatus());
    }

    @Test
    void stopDispatchesPerActiveEngine() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileRecordingStatus pwStatus = new McpMobileRecordingStatus(false, null, "pw", 1, false, List.of());
        McpMobileRecordingStatus mobileStatus = new McpMobileRecordingStatus(false, null, "mobile", 2, false, List.of());
        when(playwrightService.recordStop(true)).thenReturn(pwStatus);
        when(mobileService.recordStop(true)).thenReturn(mobileStatus);
        CaptureService service = service(playwrightService, mobileService);

        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);
        McpCaptureUnionStatus pwResult = service.stop(true);
        verify(playwrightService).recordStop(true);
        assertEquals(ActiveEngine.PLAYWRIGHT, pwResult.engine());
        assertEquals(pwStatus, pwResult.playwrightStatus());

        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);
        McpCaptureUnionStatus mobileResult = service.stop(true);
        verify(mobileService).recordStop(true);
        assertEquals(ActiveEngine.MOBILE_NATIVE, mobileResult.engine());
        assertEquals(mobileStatus, mobileResult.mobileStatus());

        EngineService.setActiveEngine(ActiveEngine.WEB);
        McpCaptureUnionStatus webResult = service.stop(false);
        assertEquals(ActiveEngine.WEB, webResult.engine());
        assertTrue(webResult.webStatus() != null);
    }

    @Test
    void stepDeleteDispatchesToPlaywrightOrMobileRecorderAndErrorsOnWebCdpSession() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileRecordingStatus pwStatus = new McpMobileRecordingStatus(true, null, "pw", 1, false, List.of());
        McpMobileRecordingStatus mobileStatus = new McpMobileRecordingStatus(true, null, "mobile", 1, false, List.of());
        when(playwrightService.stepDelete("m1")).thenReturn(pwStatus);
        when(mobileService.stepDelete("m1")).thenReturn(mobileStatus);
        CaptureService service = service(playwrightService, mobileService);

        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);
        assertEquals(pwStatus, service.stepDelete("m1"));
        verify(playwrightService).stepDelete("m1");

        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);
        assertEquals(mobileStatus, service.stepDelete("m1"));
        verify(mobileService).stepDelete("m1");

        EngineService.setActiveEngine(ActiveEngine.WEB);
        UnsupportedOperationException failure = assertThrows(UnsupportedOperationException.class,
                () -> service.stepDelete("m1"));
        assertTrue(failure.getMessage().contains("WEB"), failure.getMessage());
    }

    @Test
    void stepReorderDispatchesToPlaywrightOrMobileRecorderAndErrorsOnWebCdpSession() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileRecordingStatus pwStatus = new McpMobileRecordingStatus(true, null, "pw", 1, false, List.of());
        McpMobileRecordingStatus mobileStatus = new McpMobileRecordingStatus(true, null, "mobile", 1, false, List.of());
        when(playwrightService.stepReorder("m1", "up")).thenReturn(pwStatus);
        when(mobileService.stepReorder("m1", "up")).thenReturn(mobileStatus);
        CaptureService service = service(playwrightService, mobileService);

        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);
        assertEquals(pwStatus, service.stepReorder("m1", "up"));
        verify(playwrightService).stepReorder("m1", "up");

        EngineService.setActiveEngine(ActiveEngine.MOBILE_WEB);
        assertEquals(mobileStatus, service.stepReorder("m1", "up"));
        verify(mobileService).stepReorder("m1", "up");

        EngineService.setActiveEngine(ActiveEngine.NONE);
        UnsupportedOperationException failure = assertThrows(UnsupportedOperationException.class,
                () -> service.stepReorder("m1", "up"));
        assertTrue(failure.getMessage().contains("NONE"), failure.getMessage());
    }
}
