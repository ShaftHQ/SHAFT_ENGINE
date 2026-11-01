package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftAssistantPanelCoverageHelpersTest {
    @Test
    void fileNameReturnsLastPathSegment() {
        assertEquals("OrdersPage.java", ShaftAssistantPanel.fileName("/tmp/src/OrdersPage.java"));
        assertEquals("OrdersPage.java", ShaftAssistantPanel.fileName("OrdersPage.java"));
        assertEquals("current", ShaftAssistantPanel.fileName(""));
        assertEquals("current", ShaftAssistantPanel.fileName(null));
    }

    @Test
    void toolRunNarrationMentionsToolName() {
        String narration = ShaftAssistantPanel.toolRunNarration("capture_generate_replay");
        assertTrue(narration.contains("Generating your test now"));
        assertFalse(narration.isBlank());
        assertFalse(ShaftAssistantPanel.toolRunNarration("capture_code_blocks").isBlank());
        assertEquals("", ShaftAssistantPanel.toolRunNarration("other_tool"));
        assertEquals("", ShaftAssistantPanel.toolRunNarration(null));
    }

    @Test
    void requiresSourceEditApprovalBlocksUnsandboxedCustomAgentCommands() {
        assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "custom-agent --unsafe", "edit this test", ""));
        assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "", "edit this test", ""));
        assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, true, "custom-agent --unsafe", "edit this test", ""));
    }
}
