package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

class CliTerminalActionIconTest {
    @Test
    void constructorsInstallTerminalIcons() {
        assertNotNull(new OpenCodexTerminalAction().getTemplatePresentation().getIcon());
        assertNotNull(new OpenClaudeCodeTerminalAction().getTemplatePresentation().getIcon());
        assertNotNull(new OpenCopilotCliTerminalAction().getTemplatePresentation().getIcon());
    }
}
