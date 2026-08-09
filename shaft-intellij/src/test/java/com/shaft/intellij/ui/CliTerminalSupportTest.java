package com.shaft.intellij.ui;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import org.junit.jupiter.api.Test;

import javax.swing.Timer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The additive raw-CLI terminal tab (issue #3959) reuses {@link ShaftTerminalCommands}'s existing
 * reflection + fallback discipline rather than inventing a new one: this class only adds the one
 * behavior that differs from the pre-type-and-wait flow {@code ShaftMcpSetupPanel} already uses --
 * actually submitting the command, since the whole point of the action is to launch an interactive
 * CLI session immediately, not to stage a command for the user to review first.
 */
class CliTerminalSupportTest {
    @Test
    void submitCommandAppendsCarriageReturnSoTheShellRunsItImmediately() {
        assertEquals("claude\r", CliTerminalSupport.submitCommand("claude"));
    }

    @Test
    void openInteractiveCliTerminalReturnsFalseWhenProjectIsNull() {
        boolean opened = CliTerminalSupport.openInteractiveCliTerminal(
                null, ".", "Claude Code", "claude", typed -> { });

        assertFalse(opened);
    }

    @Test
    void isExecutableOnPathReturnsFalseForAnObviouslyMissingCommand() {
        assertFalse(CliTerminalSupport.isExecutableOnPath("definitely-not-a-real-cli-xyz123"));
    }

    @Test
    void disposingTheTerminalOwnerStopsPendingCommandTypingRetries() {
        Disposable owner = Disposer.newDisposable();
        Timer retryTimer = ShaftTerminalCommands.scheduleCommandTyping(
                owner, new Object(), "shaft-mcp", typed -> { });
        assertTrue(retryTimer.isRunning(), "Precondition: terminal typing must own a pending retry timer");

        Disposer.dispose(owner);

        assertFalse(retryTimer.isRunning(),
                "Disposing the terminal project must stop its retry timer before it can outlive the tool window");
    }
}
