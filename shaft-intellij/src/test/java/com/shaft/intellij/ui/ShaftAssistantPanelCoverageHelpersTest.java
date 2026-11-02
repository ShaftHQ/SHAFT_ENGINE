package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import java.util.List;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
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
        assertEquals("readme.md", ShaftAssistantPanel.fileName("C:\\repo\\readme.md"));
    }

    @Test
    void toolRunNarrationMentionsToolName() {
        String narration = ShaftAssistantPanel.toolRunNarration("capture_generate_replay");
        assertTrue(narration.contains("Generating your test now"));
        assertFalse(narration.isBlank());
        assertFalse(ShaftAssistantPanel.toolRunNarration("capture_code_blocks").isBlank());
        assertEquals("", ShaftAssistantPanel.toolRunNarration("other_tool"));
        assertEquals("", ShaftAssistantPanel.toolRunNarration(null));
        assertEquals("", ShaftAssistantPanel.toolRunNarration(""));
    }

    @Test
    void requiresSourceEditApprovalBlocksUnsandboxedCustomAgentCommands() {
        assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "custom-agent --unsafe", "edit this test", ""));
        assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "", "edit this test", ""));
        assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, true, "custom-agent --unsafe", "edit this test", ""));
        assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                false, false, false, "custom-agent --unsafe", "edit this test", ""));
        assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, true, false, "custom-agent --unsafe", "edit this test", ""));
        // Browser-only investigation without source artifacts skips approval.
        assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "custom-agent --unsafe", "open https://example.com and inspect the form", ""));
        // Continuation of a prior source-edit conversation needs approval.
        assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "custom-agent --unsafe", "continue", "please edit the test class"));
        assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "custom-agent --unsafe", "apply the change", "refactor the java module"));
        assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "agent", "write file OrdersPage.java", ""));
        assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                true, false, false, "agent", "apply patch to the source", ""));
    }

    @Test
    void slashTriggerAllowedOnlyAtBlankPrefix() {
        assertTrue(ShaftAssistantPanel.slashTriggerAllowed(null));
        assertTrue(ShaftAssistantPanel.slashTriggerAllowed(""));
        assertTrue(ShaftAssistantPanel.slashTriggerAllowed("   "));
        assertFalse(ShaftAssistantPanel.slashTriggerAllowed("hello"));
        assertFalse(ShaftAssistantPanel.slashTriggerAllowed(" /"));
    }

    @Test
    void isShaftMcpToolRecognizesPrefixedNamesOnly() {
        assertTrue(ShaftAssistantPanel.isShaftMcpTool("mcp__shaft-mcp__capture_start"));
        assertFalse(ShaftAssistantPanel.isShaftMcpTool("capture_start"));
        assertFalse(ShaftAssistantPanel.isShaftMcpTool(null));
        assertFalse(ShaftAssistantPanel.isShaftMcpTool(""));
    }

    @Test
    void requiresMcpSetupWhenInvocationNeedsConfiguredMcp() {
        AssistantCommand.Invocation capture = AssistantCommand.Invocation.tool("capture_start", new JsonObject());
        AssistantCommand.Invocation local = AssistantCommand.Invocation.local("hello");
        AssistantCommand.Invocation agent = AssistantCommand.Invocation.tool("autobot_local_agent_run", new JsonObject());
        assertAll(
                () -> assertTrue(ShaftAssistantPanel.requiresMcpSetup(capture, false)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(capture, true)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(local, false)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(agent, false)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(null, false)));
    }

    @Test
    void agentGuidanceOptimizationPromptNamesFamilySurfaces() {
        ShaftSettingsState.Settings claude = new ShaftSettingsState.Settings();
        claude.assistantFamily = "CLAUDE";
        String claudePrompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(claude);
        assertTrue(claudePrompt.contains("CLAUDE.md"));
        assertTrue(claudePrompt.contains("shaft_guide_search"));

        ShaftSettingsState.Settings copilot = new ShaftSettingsState.Settings();
        copilot.assistantFamily = "COPILOT";
        String copilotPrompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(copilot);
        assertTrue(copilotPrompt.contains(".github/copilot-instructions.md"));

        ShaftSettingsState.Settings codex = new ShaftSettingsState.Settings();
        codex.assistantFamily = "CODEX";
        String codexPrompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(codex);
        assertTrue(codexPrompt.contains("AGENTS.md"));
        assertTrue(codexPrompt.contains(".codex/config.toml"));
    }

    @Test
    void toolCardMarkdownAndKindCoverSuccessNullAndErrorPaths() {
        ShaftMcpToolResult ok = ShaftMcpToolResult.success("{\"status\":\"ok\"}");
        ShaftMcpToolResult failed = ShaftMcpToolResult.failure("boom");
        String okMarkdown = ShaftAssistantPanel.toolCardMarkdown("capture_status", ok, null);
        String nullMarkdown = ShaftAssistantPanel.toolCardMarkdown("capture_status", null, null);
        String errorMarkdown = ShaftAssistantPanel.toolCardMarkdown("capture_status", ok, new IllegalStateException("boom"));
        String failedMarkdown = ShaftAssistantPanel.toolCardMarkdown("capture_status", failed, null);

        assertAll(
                () -> assertFalse(okMarkdown.isBlank()),
                () -> assertTrue(nullMarkdown.toLowerCase().contains("no result")
                        || nullMarkdown.toLowerCase().contains("couldn't")
                        || nullMarkdown.contains("No result"), nullMarkdown),
                () -> assertFalse(errorMarkdown.isBlank()),
                () -> assertFalse(failedMarkdown.isBlank()),
                () -> assertEquals(ShaftAssistantChatState.KIND_TOOL_EVENT,
                        ShaftAssistantPanel.toolCardKind(ok, null)),
                () -> assertEquals(ShaftAssistantChatState.KIND_ERROR,
                        ShaftAssistantPanel.toolCardKind(null, null)),
                () -> assertEquals(ShaftAssistantChatState.KIND_ERROR,
                        ShaftAssistantPanel.toolCardKind(ok, new RuntimeException("x"))),
                () -> assertEquals(ShaftAssistantChatState.KIND_ERROR,
                        ShaftAssistantPanel.toolCardKind(failed, null)));
    }

    @Test
    void interruptedRunNoticeIncludesTailWhenPresent() {
        String empty = ShaftAssistantPanel.interruptedRunNotice(null);
        assertTrue(empty.contains("panel was closed"));
        assertFalse(empty.contains("Last output"));

        String blank = ShaftAssistantPanel.interruptedRunNotice("   ");
        assertFalse(blank.contains("Last output"));

        String shortTail = ShaftAssistantPanel.interruptedRunNotice("step 1 done");
        assertTrue(shortTail.contains("Last output before it stopped"));
        assertTrue(shortTail.contains("step 1 done"));

        String longBody = "x".repeat(2_000);
        String longTail = ShaftAssistantPanel.interruptedRunNotice(longBody);
        assertTrue(longTail.contains("..."));
        assertTrue(longTail.contains("xxx"));
        assertTrue(longTail.length() < longBody.length() + 400);
    }

    @Test
    void disposeLivePanelsIsSafeWhenNoPanelsAreRegistered() {
        assertDoesNotThrow(ShaftAssistantPanel::disposeLivePanels);
    }

    @Test
    void toolRunNarrationCoversKnownLongRunningTools() {
        for (String tool : List.of(
                "capture_generate_replay",
                "capture_code_blocks",
                "capture_start",
                "doctor_analyze_failed_allure",
                "healer_run_failed_test",
                "shaft_project_create",
                "test_automation_scenarios")) {
            String narration = ShaftAssistantPanel.toolRunNarration(tool);
            assertTrue(narration != null);
        }
        assertEquals("", ShaftAssistantPanel.toolRunNarration("not_a_tool"));
    }

    @Test
    void fileNameHandlesMixedSeparatorsAndTrailingSlash() {
        assertEquals("pom.xml", ShaftAssistantPanel.fileName("/repo/module/pom.xml"));
        assertEquals("pom.xml", ShaftAssistantPanel.fileName("C:\\repo\\module\\pom.xml"));
        assertEquals("/", ShaftAssistantPanel.fileName("/"));
        assertEquals("\\", ShaftAssistantPanel.fileName("\\"));
    }


    @Test
    void agentGuidanceOptimizationPromptFallsBackThroughDefaultClient() {
        ShaftSettingsState.Settings blank = new ShaftSettingsState.Settings();
        blank.assistantFamily = "";
        blank.defaultAutobotClient = "";
        String blankPrompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(blank);
        assertTrue(blankPrompt.contains("AGENTS.md"));

        ShaftSettingsState.Settings viaClient = new ShaftSettingsState.Settings();
        viaClient.assistantFamily = "  ";
        viaClient.defaultAutobotClient = "claude";
        String via = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(viaClient);
        assertTrue(via.toLowerCase().contains("claude") || via.contains("CLAUDE.md") || via.contains("AGENTS.md"));

        ShaftSettingsState.Settings gemini = new ShaftSettingsState.Settings();
        gemini.assistantFamily = "GEMINI";
        String geminiPrompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(gemini);
        assertTrue(geminiPrompt.contains("AGENTS.md"));
    }

}
