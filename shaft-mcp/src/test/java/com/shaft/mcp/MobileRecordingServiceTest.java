package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MobileRecordingServiceTest {
    @TempDir
    Path temp;

    @Test
    void recordingGeneratesRedactedReplayCodeInsideWorkspace() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("mobile.json");

        service.start(recording.toString(), "native", false);
        service.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                "login",
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                false);
        service.record(
                "type",
                locatorStrategy.ACCESSIBILITY_ID,
                "username",
                Map.of("value", "secret"),
                "driver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"secret\");",
                "driver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"<redacted>\");",
                true);

        McpMobileRecordingStatus stopped = service.stop(false);
        McpMobileReplayResult result = service.codeBlocks(recording.toString(), "mobileDriver");
        String code = result.codeBlocks().getFirst().code();

        assertEquals(2, stopped.actionCount());
        assertTrue(Files.isRegularFile(recording));
        assertTrue(code.contains("mobileDriver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));"));
        assertTrue(code.contains("mobileDriver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"<redacted>\");"));
        assertFalse(code.contains("\"secret\""));
        assertFalse(result.codeBlocks().getFirst().copyPasteReady());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
    }

    @Test
    void recordingCodeBlocksRejectPathOutsideWorkspace() throws Exception {
        Path outside = Files.createTempFile("mobile-recording", ".json");
        Files.writeString(outside, "{}");
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.codeBlocks(outside.toString(), "driver"));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void coordinateFallbackActionsCarryReplayWarning() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("coordinates.json");

        service.start(recording.toString(), "native", true);
        service.record(
                "tapCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.element().touch().tapByCoordinates(10, 20);",
                "driver.element().touch().tapByCoordinates(10, 20);",
                false);
        service.stop(false);

        McpMobileRecording stored = service.readRecording(recording.toString());
        McpMobileReplayResult replay = service.codeBlocks(recording.toString(), "driver");

        assertTrue(stored.actions().getFirst().warnings().stream()
                .anyMatch(warning -> warning.contains("probably fail")));
        assertTrue(replay.codeBlocks().getFirst().code().contains("probably fail"));
        assertTrue(replay.warnings().stream()
                .anyMatch(warning -> warning.contains("probably fail")));
    }
}
