package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

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
}
