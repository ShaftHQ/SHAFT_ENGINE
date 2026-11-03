package com.shaft.intellij.ui.presenter;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftAssistantPresenterGateTest {
    @Test
    void assistantOptionalAfterMcpVerified() {
        assertFalse(ShaftAssistantPresenter.assistantChatAllowed(false));
        assertTrue(ShaftAssistantPresenter.assistantChatAllowed(true));
        assertTrue(ShaftAssistantPresenter.assistantGateMessage(false).contains("Verify SHAFT MCP"));
        assertTrue(ShaftAssistantPresenter.shortSlashList().stream().anyMatch(s -> s.startsWith("/heal")));
        assertTrue(ShaftAssistantPresenter.reattachPrompt("exit 1").contains("Reattach"));
    }
}
