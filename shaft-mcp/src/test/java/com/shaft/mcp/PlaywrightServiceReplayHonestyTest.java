package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Regression test for the replay-status-honesty gap #4029/#4224 fixed for {@code CaptureGenerator}
 * (a skipped/unproven replay must never fold into bare {@code successful=true}), but which was
 * never extended to the mobile/Playwright step-recording branch of {@code capture_generate_replay}
 * (dispatched through {@link PlaywrightService#replayRecording}/{@link MobileService#replayRecording}).
 *
 * <p>{@code PlaywrightServiceDriverBackedTest#replayRecordingSilentlySkipsRedactedActionsWithoutRequiringAnActiveDriver}
 * already documents, as current intended behavior, that a recording whose every action is a
 * redacted sensitive value replays zero actions against a live driver yet still reports
 * {@code successful()==true}. That is the identical shape of dishonesty #4029 fixed for web codegen:
 * a "generation completed" outcome (nothing ran) is indistinguishable from a genuinely proven replay.
 */
class PlaywrightServiceReplayHonestyTest {
    @TempDir
    Path temp;

    @Test
    void replayThatExecutesNoActionsMustNotReportBareSuccess() {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/all-redacted.json");
        fixture.start(recording.toString(), "playwright", false);
        fixture.record("type", locatorStrategy.ID, "email", Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"alice@example.test\");",
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"<redacted>\");",
                true);
        fixture.record("type_semantic", null, "Password", Map.of("value", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "<redacted>"),
                true);
        fixture.stop(false);

        McpMobileReplayResult replay = service.replayRecording(recording.toString(), "driver");

        // Zero of the two recorded actions actually executed against a driver -- this is an
        // unconfirmed/skipped replay, not a proven one, and must not be reported as bare success.
        assertFalse(replay.successful(),
                "replayedActionCount=" + replay.replayedActionCount()
                        + " (0 of 2 recorded actions ran) but successful() reported true");
    }
}
