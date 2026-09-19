package com.shaft.intellij.ui;

import com.shaft.intellij.java.JavaTargetContext;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3661 / #5942: {@code RecordShaftFlowHereAction} used to copy a prefilled
 * {@code capture_record_at_target_code_blocks} request to the clipboard and open the tool window,
 * leaving a live browser recording several manual steps away. {@link
 * ShaftToolWindowPanel#startRecordingAtTarget} is the new direct entry point: it selects the
 * Automation live-record canvas and forwards the resolved Java caret target straight into {@link
 * RecorderToolPanel#startRecordingAtTarget}'s live {@code capture_start} call.
 */
class RecordShaftFlowHereActionRoutingTest {
    @Test
    void startRecordingAtTargetSelectsTheRecorderTabAndForwardsTheContext() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        settings.advancedUiEnabled = true;
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));
        JavaTargetContext context = new JavaTargetContext(
                "src/test/java/LoginTest.java", "tests", "LoginTest", "logsIn");

        toolWindow.startRecordingAtTarget(context);

        RecorderToolPanel recorder = toolWindow.recorderPanel();
        assertNotNull(recorder, "Recorder tab must be created when advancedUiEnabled is true");
        assertEquals("Record a SHAFT flow at logsIn in LoginTest",
                recorder.captureStartArguments().get("sessionGoal").getAsString());
        assertEquals(AutomationStagePanel.LIVE_RECORD_TAB, toolWindow.selectedSurfaceLabel());
    }

    @Test
    void startRecordingAtTargetWorksWhenAdvancedUiIsDisabled() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        settings.advancedUiEnabled = false;
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));
        JavaTargetContext context = new JavaTargetContext(
                "src/test/java/LoginTest.java", "tests", "LoginTest", "logsIn");

        toolWindow.startRecordingAtTarget(context);

        assertNotNull(toolWindow.recorderPanel(), "Recorder is part of the Automation stage, not expert-only");
        assertEquals(AutomationStagePanel.LIVE_RECORD_TAB, toolWindow.selectedSurfaceLabel());
    }
    @Test
    void readyPackIntentIsForwardedIntoCaptureStartSessionGoal() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        settings.advancedUiEnabled = false;
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));
        toolWindow.automationStagePanel().applyReadyPackPrefill(
                "https://shop.example/checkout", "valid payment places the order");
        JavaTargetContext context = new JavaTargetContext(
                "src/test/java/CheckoutTest.java", "tests", "CheckoutTest", "pays");

        toolWindow.startRecordingAtTarget(context);

        String goal = toolWindow.recorderPanel().captureStartArguments().get("sessionGoal").getAsString();
        assertTrue(goal.contains("valid payment places the order"), goal);
        assertTrue(goal.contains("pays"), goal);
        assertEquals("https://shop.example/checkout",
                toolWindow.recorderPanel().captureStartArguments().get("targetUrl").getAsString());
    }

}
