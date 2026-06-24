package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightServiceTest {
    @TempDir
    Path temp;

    @Test
    void recordingCodeBlocksGeneratePlaywrightReplayWithoutLeakingRedactedValues() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright.json");

        recorder.start(recording.toString(), "playwright", false);
        recorder.record(
                "type",
                locatorStrategy.ID,
                "email",
                Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.hasAnyTagName().hasId(\"email\").build(), \"alice@example.test\");",
                "driver.element().type(SHAFT.GUI.Locator.hasAnyTagName().hasId(\"email\").build(), \"<redacted>\");",
                true);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        assertTrue(Files.isRegularFile(recording));
        String code = result.codeBlocks().getFirst().code();
        assertTrue(code.contains("SHAFT.GUI.Playwright page"));
        assertTrue(code.contains("<redacted>"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
        assertFalse(code.contains("alice@example.test"));
    }

    @Test
    void semanticTypeRecordingCodeBlocksRedactTypedValues() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-semantic.json");

        recorder.start(recording.toString(), "playwright", false);
        recorder.record(
                "type_semantic",
                null,
                "Password",
                Map.of("value", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "<redacted>"),
                true);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        String code = result.codeBlocks().getFirst().code();
        assertTrue(result.codeBlocks().getFirst().imports().contains("com.shaft.gui.driver.ShaftLocator"));
        assertTrue(code.contains("ShaftLocator.xpath("));
        assertTrue(code.contains("Password"));
        assertTrue(code.contains("<redacted>"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
        assertFalse(code.contains("super-secret"));
    }
}
