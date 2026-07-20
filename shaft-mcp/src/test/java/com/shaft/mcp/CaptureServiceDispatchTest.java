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

    @Test
    void startAcceptsOptionalCodegenOptionsAbsorbingCaptureStartCodegen() throws Exception {
        // capture_start_codegen's 28-field CaptureCodegenStartRequest becomes an optional nested
        // param on capture_start itself (design doc Decision 2): passing it selects the same
        // Playwright-codegen-shaped WEB launch capture_start_codegen used to provide directly.
        Path outside = Files.createTempFile("outside-capture-codegen-options", ".json");
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        CaptureService service = service(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.WEB);
        CaptureService.CaptureCodegenStartRequest options = new CaptureService.CaptureCodegenStartRequest();
        options.targetUrl = "https://example.test";
        options.outputPath = outside.toString();
        options.headless = true;

        // The workspace-boundary check on outputPath fires before any browser launch (same guard
        // proven for the flat-args path in startDispatchesToWebCdpManagedBrowserWhenWebOrNoEngineIsActive),
        // so this proves the nested codegenOptions object actually reached startWithOptions.
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.start(null, null, null, null, "", options));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void codeBlocksDispatchesToMobileRecorderWhenMobileEngineIsActiveOrBackendIsExplicitlyMobile() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileReplayResult mobileBlocks = new McpMobileReplayResult(
                Path.of("recording.json"), true, 0, List.of(), List.of());
        when(mobileService.recordingCodeBlocks("recording.json", "driver")).thenReturn(mobileBlocks);
        CaptureService service = service(playwrightService, mobileService);

        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);
        McpCaptureReplayResult inferred = service.codeBlocks("recording.json", null, null, null, null, "driver", null);
        verify(mobileService).recordingCodeBlocks("recording.json", "driver");
        assertTrue(inferred.successful());

        EngineService.setActiveEngine(ActiveEngine.WEB);
        service.codeBlocks("recording.json", null, null, null, null, "driver", "mobile");
        verify(mobileService, org.mockito.Mockito.times(2)).recordingCodeBlocks("recording.json", "driver");
    }

    @Test
    void recordAtTargetCodeBlocksDispatchesToMobileRecorderWhenMobileEngineIsActive() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileReplayResult mobileBlocks = new McpMobileReplayResult(
                Path.of("recording.json"), true, 0, List.of(), List.of());
        when(mobileService.recordAtTargetCodeBlocks("recording.json", "driver", "target.java", "anchor"))
                .thenReturn(mobileBlocks);
        CaptureService service = service(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_WEB);

        McpCaptureReplayResult result = service.recordAtTargetCodeBlocks(
                "recording.json", null, null, null, null, "target.java", "anchor", "driver", null);

        verify(mobileService).recordAtTargetCodeBlocks("recording.json", "driver", "target.java", "anchor");
        assertTrue(result.successful());
    }

    @Test
    void codeBlocksDispatchesToPlaywrightsOwnLiveRecorderWhenTheSessionFileIsStepRecordingFormat() throws Exception {
        // capture_start on the Playwright engine writes via playwrightService.recordStart, whose JSON
        // shape is the step-recorder format (top-level "actions"), never a Capture-JSON session (top-
        // level "events") -- capture_code_blocks/capture_generate_replay must sniff the session file
        // instead of assuming backend=playwright always means "render a Capture session as Playwright
        // code" (design doc Decision 2/3: "backend inferred from the session file / ActiveEngine").
        Path stepRecording = temp.resolve("playwright-step-recording.json");
        Files.writeString(stepRecording, "{\"schemaVersion\":1,\"mode\":\"playwright\",\"actions\":[]}");
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileReplayResult stepBlocks = new McpMobileReplayResult(
                stepRecording, true, 0, List.of(), List.of());
        when(playwrightService.recordingCodeBlocks(stepRecording.toString(), "driver")).thenReturn(stepBlocks);
        CaptureService service = service(playwrightService, mobileService);

        McpCaptureReplayResult result = service.codeBlocks(
                stepRecording.toString(), null, null, null, null, "driver", "playwright");

        verify(playwrightService).recordingCodeBlocks(stepRecording.toString(), "driver");
        assertTrue(result.successful());
    }

    @Test
    void apiStartStatusStopDispatchToMobileApiCaptureWhenMobileEngineIsActive() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        MobileApiCaptureStatus mobileStartStatus = new MobileApiCaptureStatus(
                true, "session-1", 8080, "", 0, List.of());
        MobileApiCaptureStatus mobileStatus = new MobileApiCaptureStatus(
                true, "session-1", 8080, "", 3, List.of());
        MobileApiCaptureStatus mobileStopStatus = new MobileApiCaptureStatus(
                false, "session-1", 0, "", 3, List.of());
        when(mobileService.mobileApiRecordStart("Android", "pixel", "recordings/api.json"))
                .thenReturn(mobileStartStatus);
        when(mobileService.mobileApiRecordStatus()).thenReturn(mobileStatus);
        when(mobileService.mobileApiRecordStop(false)).thenReturn(mobileStopStatus);
        CaptureService service = service(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);

        McpCaptureApiUnionStatus started = service.apiStart(
                null, null, null, null, "recordings/api.json", "Android", "pixel");
        McpCaptureApiUnionStatus status = service.apiStatus();
        McpCaptureApiUnionStatus stopped = service.apiStop(false);

        assertEquals(ActiveEngine.MOBILE_NATIVE, started.engine());
        assertEquals(mobileStartStatus, started.mobileStatus());
        assertNull(started.webStatus());
        assertEquals(mobileStatus, status.mobileStatus());
        assertEquals(mobileStopStatus, stopped.mobileStatus());
        verify(mobileService).mobileApiRecordStart("Android", "pixel", "recordings/api.json");
        verify(mobileService).mobileApiRecordStatus();
        verify(mobileService).mobileApiRecordStop(false);
    }

    @Test
    void generateReplayDispatchesToMobileReplayWhenMobileEngineIsActive() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        MobileService mobileService = mock(MobileService.class);
        McpMobileReplayResult mobileReplay = new McpMobileReplayResult(
                Path.of("recording.json"), true, 3, List.of(), List.of());
        when(mobileService.replayRecording("recording.json", "driver")).thenReturn(mobileReplay);
        CaptureService service = service(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);

        McpCaptureReplayResult result = service.generateReplay(
                "recording.json", null, null, null, false, true, false, false, false, "driver", null, null, null);

        verify(mobileService).replayRecording("recording.json", "driver");
        assertTrue(result.successful());
    }
}
