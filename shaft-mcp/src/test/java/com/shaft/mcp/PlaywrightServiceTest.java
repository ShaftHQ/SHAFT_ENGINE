package com.shaft.mcp;

import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
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
    void recordingCodeBlocksGenerateCaptureArtifactsAndReportMetadata() throws Exception {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-capture.json");

        recorder.start(recording.toString(), "playwright", true);
        recorder.record(
                "navigate",
                null,
                "",
                Map.of("url", "https://example.test/login"),
                "driver.browser().navigateToURL(\"https://example.test/login\");",
                "driver.browser().navigateToURL(\"https://example.test/login\");",
                false);
        recorder.record(
                "type",
                locatorStrategy.ID,
                "email",
                Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"alice@example.test\");",
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"<redacted>\");",
                true);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        assertTrue(Files.isRegularFile(result.captureSessionPath()));
        assertTrue(Files.isRegularFile(result.sourcePath()));
        assertTrue(Files.isRegularFile(result.reportPath()));
        assertTrue(Files.isRegularFile(result.reviewPath()));
        assertTrue(result.codeBlocks().stream().anyMatch(block -> block.id().equals("capture-full-class")));
        assertTrue(result.report().warnings().stream()
                .anyMatch(warning -> warning.contains("No assertion")));
        CaptureSession session = new CaptureJsonCodec().read(result.captureSessionPath());
        assertTrue(session.events().getFirst() instanceof CaptureEvent.NavigationEvent);
        assertTrue(session.events().get(1) instanceof CaptureEvent.TypeEvent);
        assertTrue(Files.readString(result.sourcePath()).contains("SHAFT.GUI.Playwright"));
        assertTrue(Files.readString(result.sourcePath()).contains("driver.element().type("));
        assertFalse(Files.readString(result.sourcePath()).contains("alice@example.test"));
        assertTrue(Files.readString(result.testDataPath()).contains("alice@example.test"));
    }

    @Test
    void recordingCodeBlocksGeneratePlaywrightReplayWithoutLeakingRedactedValues() throws Exception {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright.json");

        recorder.start(recording.toString(), "playwright", false);
        recorder.record(
                "type",
                locatorStrategy.ID,
                "email",
                Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"alice@example.test\");",
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"<redacted>\");",
                true);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        assertTrue(Files.isRegularFile(recording));
        String code = result.codeBlocks().getFirst().code();
        assertTrue(code.contains("SHAFT.GUI.Playwright page"));
        assertTrue(code.contains("<redacted>"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
        assertFalse(code.contains("alice@example.test"));
        assertTrue(Files.isRegularFile(result.captureSessionPath()));
        assertTrue(Files.isRegularFile(result.sourcePath()));
        assertTrue(Files.isRegularFile(result.testDataPath()));
        assertTrue(Files.isRegularFile(result.reportPath()));
        assertArtifactDoesNotContain(result.captureSessionPath(), "alice@example.test");
        assertArtifactDoesNotContain(result.sourcePath(), "alice@example.test");
        assertArtifactDoesNotContain(result.testDataPath(), "alice@example.test");
        assertArtifactDoesNotContain(result.reportPath(), "alice@example.test");
        assertFalse(result.report().toString().contains("alice@example.test"));
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
        assertFalse(result.codeBlocks().getFirst().imports().contains("com.shaft.gui.driver.ShaftLocator"));
        assertTrue(code.contains("SHAFT.GUI.Locator.xpath("));
        assertTrue(code.contains("Password"));
        assertTrue(code.contains("<redacted>"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
        assertFalse(code.contains("super-secret"));
    }

    @Test
    void coordinateFallbackRecordingCarriesReplayWarning() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-coordinates.json");

        recorder.start(recording.toString(), "playwright", true);
        recorder.record(
                "clickCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.mouse().click(10, 20);",
                "driver.mouse().click(10, 20);",
                false);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        assertTrue(result.codeBlocks().getFirst().code().contains("probably fail"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("probably fail")));
    }

    private static void assertArtifactDoesNotContain(Path path, String rawValue) throws Exception {
        assertFalse(Files.readString(path).contains(rawValue));
    }
}
