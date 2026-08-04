package com.shaft.intellij.ui;

import com.intellij.openapi.util.Disposer;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.Timer;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression coverage for issue #4500: {@link ShaftAssistantPanel} owns two background lifecycles it
 * never used to end -- the in-flight local-agent {@link ShaftMcpInvocation} (a real Codex/Claude CLI
 * child process blocking one of {@code ShaftPluginExecutor}'s 4-8 bounded worker threads) and the
 * ~100ms one-shot {@code javax.swing.Timer} that flushes that run's coalesced output. Neither was
 * tied to the panel, so {@code ShaftToolWindowPanel}'s teardown (project close, tool-window content
 * rebuild, or a plain switch back to the setup view, all of which just drop the assistant panel)
 * left the CLI process running to its full timeout and the Timer pending in Swing's shared
 * {@code TimerQueue} -- the same defect shape {@code ShaftToolWindowPanel} already fixed for its
 * other child panels in issue #3619.
 */
class ShaftAssistantPanelDisposalTest {
    @Test
    void disposingTheAssistantPanelKillsItsInFlightLocalAgentRun() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        AtomicBoolean killed = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = new CompletableFuture<>();
        setField(panel, "currentInvocation", new ShaftMcpInvocation(future, () -> {
        }, () -> killed.set(true)));

        panel.dispose();

        assertAll(
                () -> assertTrue(killed.get(),
                        "Disposing the panel must kill its in-flight local-agent run, or the spawned "
                                + "CLI process keeps a bounded plugin worker thread until its own timeout"),
                () -> assertTrue(future.isCancelled(),
                        "The run's future must be cancelled so no completion callback fires into a dead panel"));
    }

    @Test
    void disposingTheAssistantPanelStopsThePendingLocalAgentFlushTimer() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        AtomicBoolean flushed = new AtomicBoolean();
        scheduleLocalAgentFlush(panel, () -> flushed.set(true));
        Timer flushTimer = (Timer) getField(panel, "localAgentFlushTimer");
        assertNotNull(flushTimer, "Precondition: scheduling a flush must produce a timer the panel owns");
        assertTrue(flushTimer.isRunning(), "Precondition: the scheduled flush timer must be pending");

        panel.dispose();

        // Well past the timer's own ~100ms delay: a stopped timer never fires, a merely forgotten one
        // does, and only the second leaves a live entry in Swing's shared TimerQueue.
        Thread.sleep(400);
        assertAll(
                () -> assertFalse(flushTimer.isRunning(),
                        "Disposing the panel must stop its pending output-flush timer instead of leaving "
                                + "it in Swing's shared TimerQueue holding a reference to the dead panel"),
                () -> assertFalse(flushed.get(), "A stopped flush timer must never run its flush"));
    }

    @Test
    void aTrailingFlushAfterDisposalSchedulesNoNewTimer() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        panel.dispose();

        // Killing a run does not silence its reader threads instantly: the process's already-buffered
        // trailing lines still reach the coalescer, which schedules a flush for them.
        AtomicBoolean flushed = new AtomicBoolean();
        scheduleLocalAgentFlush(panel, () -> flushed.set(true));

        assertAll(
                () -> assertNull(getField(panel, "localAgentFlushTimer"),
                        "A flush scheduled after disposal must start no timer, or a killed run's last "
                                + "line puts a fresh one-shot timer back into Swing's shared TimerQueue"),
                () -> assertFalse(flushed.get(), "Nothing must be scheduled to run against a dead panel"));
    }

    /**
     * The one test holding the production wiring in place. Every other panel in the module's test run
     * is torn down directly by {@code ShaftPanelBackgroundWorkExtension}, so if
     * {@code disposeActiveChildren()} ever stops disposing the assistant panel again, no leak check
     * anywhere will notice — only this test, which goes through the real
     * {@code Disposer.dispose(content)} path with no manual bookkeeping. Do not delete it without
     * replacing that coverage.
     */
    @Test
    void disposingTheToolWindowPanelCascadesToTheAssistantPanelsInFlightRun() throws ReflectiveOperationException {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(null, settings,
                (client, runtime) -> null, ShaftAssistantChatState.getInstance(null));
        ShaftAssistantPanel assistant = toolWindow.assistantPanel();
        assertNotNull(assistant, "Precondition: a completed MCP setup must show the assistant panel");
        AtomicBoolean killed = new AtomicBoolean();
        setField(assistant, "currentInvocation",
                new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
                }, () -> killed.set(true)));

        // The real teardown path: project close / tool window content rebuild disposes the content
        // Disposable exactly once, with no manual child bookkeeping call ever happening.
        Disposer.dispose(toolWindow);

        assertTrue(killed.get(),
                "Tool window teardown must cascade to the assistant panel's in-flight local-agent run "
                        + "the same way it already does for the API recording and Guided workflow panels");
    }

    @Test
    void disposeLivePanelsTearsDownEveryAssistantPanelStillLiveInThisJvm() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        AtomicBoolean killed = new AtomicBoolean();
        setField(panel, "currentInvocation", new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
        }, () -> killed.set(true)));

        ShaftAssistantPanel.disposeLivePanels();

        assertTrue(killed.get(),
                "disposeLivePanels() must reach a panel nobody else holds a disposal handle to -- it is "
                        + "what gives the headless test JVM the same deterministic teardown the Disposer "
                        + "tree gives production");
    }

    private static void scheduleLocalAgentFlush(ShaftAssistantPanel panel, Runnable flush)
            throws ReflectiveOperationException {
        Method scheduleLocalAgentFlush =
                ShaftAssistantPanel.class.getDeclaredMethod("scheduleLocalAgentFlush", Runnable.class);
        scheduleLocalAgentFlush.setAccessible(true); // NOPMD - reflective test invocation of a private scheduler, matching the established pattern in ShaftPanelSetupTest
        scheduleLocalAgentFlush.invoke(panel, flush);
    }

    private static ShaftSettingsState.Settings blankMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "";
        return settings;
    }

    private static void setField(Object target, String name, Object value) throws ReflectiveOperationException {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only field injection, matching the established pattern in ShaftPanelSetupTest
        field.set(target, value);
    }

    private static Object getField(Object target, String name) throws ReflectiveOperationException {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only field read, matching the established pattern in ShaftPanelSetupTest
        return field.get(target);
    }
}
