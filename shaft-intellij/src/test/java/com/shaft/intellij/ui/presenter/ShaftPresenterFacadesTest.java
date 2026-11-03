package com.shaft.intellij.ui.presenter;

import com.shaft.intellij.ui.ShaftStatusPresentation;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPresenterFacadesTest {
    @Test
    void mcpBadgesUseSharedStatusMarkers() {
        assertEquals(ShaftStatusPresentation.SUCCESS_ICON + " MCP", ShaftMcpSetupPresenter.verifiedBadgeText("MCP"));
        assertEquals(ShaftStatusPresentation.ERROR_ICON + " MCP", ShaftMcpSetupPresenter.failedBadgeText("MCP"));
    }

    @Test
    void assistantFailureIsPlainLanguageAndMentionsReattach() {
        String text = ShaftAssistantPresenter.plainLanguageAgentFailure("exit 1");
        assertTrue(text.contains("exit 1"));
        assertTrue(text.toLowerCase().contains("reattach"));
    }
}
