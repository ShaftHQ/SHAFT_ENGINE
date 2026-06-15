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
                "driver.touch().tap(AppiumBy.accessibilityId(\"login\"));",
                "driver.touch().tap(AppiumBy.accessibilityId(\"login\"));",
                false);
        service.record(
                "type",
                locatorStrategy.ACCESSIBILITY_ID,
                "username",
                Map.of("value", "secret"),
                "driver.element().type(AppiumBy.accessibilityId(\"username\"), \"secret\");",
                "driver.element().type(AppiumBy.accessibilityId(\"username\"), \"<redacted>\");",
                true);

        McpMobileRecordingStatus stopped = service.stop(false);
        McpMobileReplayResult result = service.codeBlocks(recording.toString(), "mobileDriver");
        String code = result.codeBlocks().getFirst().code();

        assertEquals(2, stopped.actionCount());
        assertTrue(Files.isRegularFile(recording));
        assertTrue(code.contains("mobileDriver.touch().tap(AppiumBy.accessibilityId(\"login\"));"));
        assertTrue(code.contains("mobileDriver.element().type(AppiumBy.accessibilityId(\"username\"), \"<redacted>\");"));
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
}
