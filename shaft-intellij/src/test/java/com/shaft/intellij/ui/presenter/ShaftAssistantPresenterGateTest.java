package com.shaft.intellij.ui.presenter;

import org.junit.jupiter.api.Test;

import javax.swing.JLabel;
import javax.swing.JPanel;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftAssistantPresenterGateTest {
    @Test
    void assistantOptionalAfterMcpVerified() {
        assertFalse(ShaftAssistantPresenter.assistantChatAllowed(false));
        assertTrue(ShaftAssistantPresenter.assistantChatAllowed(true));
        assertTrue(ShaftAssistantPresenter.assistantGateMessage(false).contains("Verify SHAFT MCP"));
        assertTrue(ShaftAssistantPresenter.assistantGateMessage(true).contains("optional"));
        assertTrue(ShaftAssistantPresenter.shortSlashList().stream().anyMatch(s -> s.startsWith("/heal")));
        assertTrue(ShaftAssistantPresenter.reattachPrompt("exit 1").contains("Reattach"));
        assertTrue(ShaftAssistantPresenter.plainLanguageAgentFailure("").contains("stopped"));
        assertTrue(ShaftAssistantPresenter.plainLanguageAgentFailure("oom").contains("oom"));
    }

    @Test
    void appliesFonts() {
        JPanel panel = new JPanel();
        JLabel label = new JLabel("section");
        ShaftAssistantPresenter.applyHeaderFont(panel);
        ShaftAssistantPresenter.applySectionFont(label);
        assertNotNull(ShaftAssistantPresenter.bodyFont());
        assertNotNull(panel.getFont());
        assertNotNull(label.getFont());
    }
}
