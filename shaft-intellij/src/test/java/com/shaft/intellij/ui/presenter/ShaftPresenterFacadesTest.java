package com.shaft.intellij.ui.presenter;

import com.shaft.intellij.ui.ShaftStatusPresentation;
import org.junit.jupiter.api.Test;

import javax.swing.JLabel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPresenterFacadesTest {
    @Test
    void mcpBadgesUseSharedStatusMarkers() {
        assertEquals(ShaftStatusPresentation.SUCCESS_ICON + " MCP", ShaftMcpSetupPresenter.verifiedBadgeText("MCP"));
        assertEquals(ShaftStatusPresentation.ERROR_ICON + " MCP", ShaftMcpSetupPresenter.failedBadgeText("MCP"));
        assertEquals(ShaftStatusPresentation.WARNING_ICON + " MCP", ShaftMcpSetupPresenter.warningBadgeText("MCP"));
        assertEquals(ShaftStatusPresentation.success(), ShaftMcpSetupPresenter.colorForSuccess(true));
        assertEquals(ShaftStatusPresentation.error(), ShaftMcpSetupPresenter.colorForSuccess(false));
    }

    @Test
    void assistantFailureIsPlainLanguageAndMentionsReattach() {
        String text = ShaftAssistantPresenter.plainLanguageAgentFailure("exit 1");
        assertTrue(text.contains("exit 1"));
        assertTrue(text.toLowerCase().contains("reattach"));
        String empty = ShaftAssistantPresenter.plainLanguageAgentFailure("  ");
        assertTrue(empty.toLowerCase().contains("reattach"));
        assertNotNull(ShaftAssistantPresenter.bodyFont());
        JLabel label = new JLabel("x");
        ShaftAssistantPresenter.applyHeaderFont(label);
        ShaftAssistantPresenter.applySectionFont(label);
        assertNotNull(label.getFont());
    }
}
