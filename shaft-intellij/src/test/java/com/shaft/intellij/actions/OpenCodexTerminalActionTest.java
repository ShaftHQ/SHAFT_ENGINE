package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Covers {@link OpenCodexTerminalAction} status-detail contract for terminal open outcomes (#6096).
 */
class OpenCodexTerminalActionTest {
    @Test
    void logStatusDetailPassesThroughStatusText() {
        assertEquals(
                "Opened Codex terminal tab.",
                OpenCodexTerminalAction.logStatusDetail("Opened Codex terminal tab."));
    }

    @Test
    void logStatusDetailTreatsNullAsEmpty() {
        assertEquals("", OpenCodexTerminalAction.logStatusDetail(null));
    }
}
