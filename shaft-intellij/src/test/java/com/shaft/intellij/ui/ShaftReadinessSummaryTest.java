package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Regression coverage for issue #3620: {@link ShaftReadinessSummary#dispose()} was dead code --
 * nothing ever called it, so every {@code ShaftToolWindowPanel.buildHealthChip()} rebuild (setup
 * completion, "Reset and reinstall", every automatic plugin-upgrade rerender via {@code
 * ShaftPluginResetService.rerenderOpenToolWindows()}) permanently leaked the previous instance's
 * static {@link ShaftRecordingActivity} listener registration for the lifetime of the IDE process,
 * per project window.
 */
class ShaftReadinessSummaryTest {
    // ShaftRecordingActivity's listener list is process-wide static; reset before AND after so
    // this test is hermetic regardless of run order with other classes in the same JVM (mirrors
    // ShaftRecordingActivityTest and ShaftToolWindowPanelTest's existing convention).
    @BeforeEach
    @AfterEach
    void resetState() {
        ShaftRecordingActivity.resetForTests();
    }

    @Test
    void listenerCountStaysFlatAcrossRepeatedMainViewRebuilds() throws ReflectiveOperationException {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";

        ShaftToolWindowPanel panel = new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));

        assertEquals(1, ShaftRecordingActivity.listenerCount(),
                "Constructing the panel (which builds the main view once, registering one "
                        + "ShaftReadinessSummary) must register exactly one listener");

        // buildHealthChip() is only reachable through the private showMainView(), which is exactly
        // the path re-run on every "Reset and reinstall" and every automatic plugin-upgrade
        // rerender (ShaftPluginResetService.rerenderOpenToolWindows()); reach it the same way
        // ShaftToolWindowPanelTest reaches GuidedWorkflowPanel's private scheduleNextStatusPoll(),
        // rather than widening ShaftToolWindowPanel's package-private surface for a test-only hook.
        Method showMainView = ShaftToolWindowPanel.class.getDeclaredMethod("showMainView");
        showMainView.setAccessible(true);
        for (int rebuild = 1; rebuild <= 5; rebuild++) {
            showMainView.invoke(panel);
            assertEquals(1, ShaftRecordingActivity.listenerCount(),
                    "Rebuild #" + rebuild + " must dispose the previous readiness summary's listener "
                            + "instead of leaking another one onto the static list (issue #3620)");
        }
    }
}
