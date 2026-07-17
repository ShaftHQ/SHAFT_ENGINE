package com.shaft.intellij.ui;

import com.shaft.intellij.java.JavaTargetContext;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Covers issue #3661: {@code RecordShaftFlowHereAction}'s advanced mode used to copy a prefilled
 * {@code capture_record_at_target_code_blocks} request to the clipboard and open the tool window,
 * leaving a live browser recording several manual steps away. {@link
 * ShaftToolWindowPanel#startRecordingAtTarget} is the new direct entry point: it selects the
 * Recorder tab and forwards the resolved Java caret target straight into {@link
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
        assertEquals("Recorder", selectedWorkflowLabel(toolWindow));
    }

    @Test
    void startRecordingAtTargetIsANoOpWhenAdvancedUiIsDisabled() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        settings.advancedUiEnabled = false;
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));
        JavaTargetContext context = new JavaTargetContext(
                "src/test/java/LoginTest.java", "tests", "LoginTest", "logsIn");

        toolWindow.startRecordingAtTarget(context);

        assertNull(toolWindow.recorderPanel(), "No Recorder tab exists outside advanced UI mode");
    }

    private static String selectedWorkflowLabel(ShaftToolWindowPanel toolWindow) {
        Object selected = toolWindow.workflowSelector().getSelectedItem();
        return selected instanceof ShaftToolWindowPanel.WorkflowView view ? view.label() : "";
    }
}
