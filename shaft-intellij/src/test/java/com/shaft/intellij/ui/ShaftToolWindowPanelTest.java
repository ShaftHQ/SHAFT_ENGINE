package com.shaft.intellij.ui;

import com.intellij.openapi.util.Disposer;
import com.intellij.util.Alarm;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression coverage for issue #3619: {@link ShaftToolWindowPanel} used to leave its child
 * panels (the Guided workflow tab's self-rescheduling status-poll {@link Alarm} in particular)
 * disposed only via internal view-switch bookkeeping ({@code showSetupView()}/{@code
 * showMainView()}), never via the platform's real content-teardown path. Closing the project (or
 * any real {@code Content} teardown) while the Guided tab was mid-recording left that Alarm
 * rescheduling itself forever, issuing MCP status calls into a dead context.
 */
class ShaftToolWindowPanelTest {
    @Test
    void disposingThePanelCascadesToTheGuidedWorkflowPanelsPollAlarm() throws ReflectiveOperationException {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        settings.advancedUiEnabled = true;

        ShaftToolWindowPanel panel = new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));

        GuidedWorkflowPanel guided = panel.guidedWorkflowPanel();
        assertNotNull(guided, "Guided workflow tab must be created when advancedUiEnabled is true");

        // Production code only creates the poll Alarm lazily once a live MCP invocation service
        // exists (startStatusPolling() -> scheduleNextStatusPoll()), and a headless unit test has
        // no live MCP connection to fake with project == null. Reach into the same private path
        // startStatusPolling() would take to reproduce the mid-recording state (a live Alarm)
        // this regression is about, without touching GuidedWorkflowPanel's production code.
        Method scheduleNextStatusPoll = GuidedWorkflowPanel.class.getDeclaredMethod("scheduleNextStatusPoll");
        scheduleNextStatusPoll.setAccessible(true);
        scheduleNextStatusPoll.invoke(guided);

        Field alarmField = GuidedWorkflowPanel.class.getDeclaredField("statusPollAlarm");
        alarmField.setAccessible(true);
        Alarm pollAlarm = (Alarm) alarmField.get(guided);
        assertNotNull(pollAlarm, "Test setup must produce a live poll Alarm before disposal");
        assertFalse(pollAlarm.isDisposed(), "Alarm must start out live, mirroring an in-progress recording");

        // The real-world teardown path this regression protects: project close / tool window
        // content rebuild disposes the top-level content Disposable exactly once, with no manual
        // call to disposeGuidedWorkflowPanel() ever happening.
        Disposer.dispose(panel);

        assertTrue(pollAlarm.isDisposed(),
                "Disposing the tool window panel must cascade to the Guided workflow panel's "
                        + "self-rescheduling status-poll Alarm, or it keeps polling a dead MCP "
                        + "connection forever after project close (issue #3619)");
    }
}
