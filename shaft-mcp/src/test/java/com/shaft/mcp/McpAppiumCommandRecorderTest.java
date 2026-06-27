package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpAppiumCommandRecorderTest {
    @TempDir
    Path temp;

    @Test
    void recordsElementClickAndRedactsTypedValueWhenConfigured() {
        Path output = temp.resolve("inspector.json");
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(output.toString(), "mobile-inspector-android", false);
        McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, () -> false);

        commandRecorder.capture("POST", "/session/abc/element",
                "{\"using\":\"accessibility id\",\"value\":\"login\"}",
                200,
                "{\"value\":{\"element-6066-11e4-a52e-4f735466cecf\":\"el-1\"}}");
        commandRecorder.capture("POST", "/session/abc/element/el-1/click", "{}", 200, "{\"value\":null}");
        commandRecorder.capture("POST", "/session/abc/element/el-1/value",
                "{\"text\":\"secret\"}", 200, "{\"value\":null}");

        McpMobileRecordingStatus stopped = recorder.stop(false);
        McpMobileReplayResult replay = recorder.codeBlocks(output.toString(), "mobileDriver");
        String code = replay.codeBlocks().getFirst().code();

        assertEquals(2, stopped.actionCount());
        assertTrue(code.contains("mobileDriver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));"));
        assertTrue(code.contains("mobileDriver.element().type(SHAFT.GUI.Locator.accessibilityId(\"login\"), \"<redacted>\");"));
        assertFalse(code.contains("secret"));
        assertTrue(replay.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
    }

    @Test
    void recordsPointerTapAndSwipeActionsAgainstLocatorsFromAccessibilityTree() {
        Path output = temp.resolve("gestures.json");
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(output.toString(), "mobile-inspector-android", true);
        McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, () -> false);

        commandRecorder.capture("GET", "/session/abc/source", "", 200, """
                {"value":"<hierarchy><android.widget.FrameLayout bounds=\\"[0,0][300,500]\\"><android.widget.Button content-desc=\\"login\\" bounds=\\"[10,20][110,70]\\"/><android.widget.ListView resource-id=\\"com.example:id/list\\" bounds=\\"[0,100][300,500]\\"/></android.widget.FrameLayout></hierarchy>"}""");
        commandRecorder.capture("POST", "/session/abc/actions", """
                {"actions":[{"type":"pointer","id":"finger","parameters":{"pointerType":"touch"},"actions":[
                {"type":"pointerMove","duration":0,"x":50,"y":40},
                {"type":"pointerDown","button":0},
                {"type":"pointerUp","button":0}
                ]}]}""", 200, "{\"value\":null}");
        commandRecorder.capture("POST", "/session/abc/actions", """
                {"actions":[{"type":"pointer","id":"finger","parameters":{"pointerType":"touch"},"actions":[
                {"type":"pointerMove","duration":0,"x":20,"y":120},
                {"type":"pointerDown","button":0},
                {"type":"pointerMove","duration":450,"x":20,"y":320},
                {"type":"pointerUp","button":0}
                ]}]}""", 200, "{\"value\":null}");

        recorder.stop(false);
        McpMobileRecording recording = recorder.readRecording(output.toString());

        assertEquals("tap", recording.actions().getFirst().action());
        assertEquals("ACCESSIBILITY_ID", recording.actions().getFirst().locatorStrategy());
        assertEquals("login", recording.actions().getFirst().locatorValue());
        assertEquals("swipeByOffset", recording.actions().get(1).action());
        assertEquals("ID", recording.actions().get(1).locatorStrategy());
        assertEquals("com.example:id/list", recording.actions().get(1).locatorValue());
        assertEquals("0", recording.actions().get(1).parameters().get("xOffset"));
        assertEquals("200", recording.actions().get(1).parameters().get("yOffset"));
    }

    @Test
    void warnsWhenPointerCoordinatesCannotBeMappedToLocator() {
        Path output = temp.resolve("coordinates.json");
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(output.toString(), "mobile-inspector-android", true);
        McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, () -> false);

        commandRecorder.capture("POST", "/session/abc/actions", """
                {"actions":[{"type":"pointer","id":"finger","parameters":{"pointerType":"touch"},"actions":[
                {"type":"pointerMove","duration":0,"x":10,"y":20},
                {"type":"pointerDown","button":0},
                {"type":"pointerUp","button":0}
                ]}]}""", 200, "{\"value\":null}");

        recorder.stop(false);
        McpMobileRecording recording = recorder.readRecording(output.toString());

        assertEquals("tapCoordinates", recording.actions().getFirst().action());
        assertTrue(recording.actions().getFirst().warnings().stream()
                .anyMatch(warning -> warning.contains("probably fail")));
    }

    @Test
    void ignoresInspectorCommandsWhilePaused() {
        Path output = temp.resolve("paused.json");
        AtomicBoolean paused = new AtomicBoolean(true);
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(output.toString(), "mobile-inspector-android", true);
        McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, paused::get);

        commandRecorder.capture("POST", "/session/abc/actions", """
                {"actions":[{"type":"pointer","parameters":{"pointerType":"touch"},"actions":[
                {"type":"pointerMove","x":1,"y":2},{"type":"pointerDown"},{"type":"pointerUp"}
                ]}]}""", 200, "{\"value\":null}");
        paused.set(false);
        commandRecorder.capture("POST", "/session/abc/actions", """
                {"actions":[{"type":"pointer","parameters":{"pointerType":"touch"},"actions":[
                {"type":"pointerMove","x":1,"y":2},{"type":"pointerDown"},{"type":"pointerUp"}
                ]}]}""", 200, "{\"value\":null}");

        McpMobileRecordingStatus stopped = recorder.stop(false);

        assertEquals(1, stopped.actionCount());
    }
}
