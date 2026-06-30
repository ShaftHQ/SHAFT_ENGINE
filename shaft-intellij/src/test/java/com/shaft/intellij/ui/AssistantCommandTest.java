package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantCommandTest {
    @Test
    void normalPromptBuildsLocalAgentRequestWithoutMutationByDefault() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Plan a resilient login test",
                "CODEX",
                "AGENT",
                "C:/work/project",
                "",
                false);

        assertFalse(invocation.isLocal());
        assertEquals("autobot_local_agent_run", invocation.toolName());
        assertEquals("CODEX", invocation.arguments().get("client").getAsString());
        assertEquals("AGENT", invocation.arguments().get("mode").getAsString());
        assertTrue(invocation.arguments().get("prompt").getAsString().endsWith("Plan a resilient login test"));
        assertFalse(invocation.arguments().get("allowSourceMutation").getAsBoolean());
    }

    @Test
    void agentMutationRequiresAgentModeAndApproval() {
        AssistantCommand.Invocation askInvocation = AssistantCommand.fromPrompt(
                "Can I edit?",
                "CODEX",
                "ASK",
                ".",
                "",
                true);
        AssistantCommand.Invocation agentInvocation = AssistantCommand.fromPrompt(
                "Edit this",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);

        assertFalse(askInvocation.arguments().get("allowSourceMutation").getAsBoolean());
        assertTrue(agentInvocation.arguments().get("allowSourceMutation").getAsBoolean());
    }

    @Test
    void localAgentPromptsIncludeGenericShaftMcpUsageHint() {
        AssistantCommand.Invocation duckDuckGo = AssistantCommand.fromPrompt(
                "open duckduckgo and search for SHAFT Engine",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);
        AssistantCommand.Invocation alreadyExplicit = AssistantCommand.fromPrompt(
                "use shaft-mcp to open mobile app",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);
        AssistantCommand.Invocation plain = AssistantCommand.fromPrompt(
                "Explain the current test failure",
                "CODEX",
                "ASK",
                ".",
                "",
                false);

        assertEquals("""
                If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.
                For WebDriver browser tasks, call driver_initialize before browser_* tools; do not use Playwright unless requested.

                open duckduckgo and search for SHAFT Engine""",
                duckDuckGo.arguments().get("prompt").getAsString());
        assertEquals("use shaft-mcp to open mobile app", alreadyExplicit.arguments().get("prompt").getAsString());
        assertEquals("""
                If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.
                For WebDriver browser tasks, call driver_initialize before browser_* tools; do not use Playwright unless requested.

                Explain the current test failure""", plain.arguments().get("prompt").getAsString());
    }

    @Test
    void directLocalAgentRunnerUsesSelectedCliDefaults() {
        AssistantCommand.Invocation codexAsk = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "", false);
        AssistantCommand.Invocation codexAgent = AssistantCommand.fromPrompt(
                "Implement the fix", "CODEX", "AGENT", ".", "", true);
        AssistantCommand.Invocation claudePlan = AssistantCommand.fromPrompt(
                "Plan the fix", "CLAUDE_CODE", "PLAN", ".", "", false);
        AssistantCommand.Invocation custom = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "custom-agent --safe", false);

        assertEquals(List.of("codex", "exec", "--sandbox", "read-only", "-"),
                AssistantLocalAgentRunner.commandFor(codexAsk.arguments()));
        assertEquals(List.of(
                        "codex", "exec",
                        "--sandbox", "workspace-write",
                        "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                        "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                        "-"),
                AssistantLocalAgentRunner.commandFor(codexAgent.arguments()));
        assertEquals(List.of("claude", "--print", "--permission-mode", "plan"),
                AssistantLocalAgentRunner.commandFor(claudePlan.arguments()));
        assertEquals(List.of("custom-agent", "--safe"), AssistantLocalAgentRunner.commandFor(custom.arguments()));
    }

    @Test
    void directLocalAgentRunnerOnlyHandlesNormalLocalCliPrompts() {
        AssistantCommand.Invocation normal = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "", false);
        AssistantCommand.Invocation slash = AssistantCommand.fromPrompt(
                "/guide locators", "CODEX", "ASK", ".", "", false);
        AssistantCommand.Invocation cloud = AssistantCommand.fromPrompt(
                "Explain this failure",
                AssistantCommand.Selection.cloud("openai", "gpt-4.1"),
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation pluginRuntime = AssistantCommand.fromPrompt(
                "Explain this failure",
                AssistantCommand.Selection.local("COPILOT", "IDE_PLUGIN"),
                "ASK",
                ".",
                "",
                false);

        assertTrue(AssistantLocalAgentRunner.supports(normal));
        assertFalse(AssistantLocalAgentRunner.supports(slash));
        assertFalse(AssistantLocalAgentRunner.supports(cloud));
        assertFalse(AssistantLocalAgentRunner.supports(pluginRuntime));
    }

    @Test
    void directLocalAgentOutputPrefersStdoutOnSuccessAndIncludesStderrOnFailure() {
        assertEquals("## Done", AssistantLocalAgentRunner.agentOutput(true, "## Done\n", "progress", ""));
        assertEquals("warning", AssistantLocalAgentRunner.agentOutput(true, "", "warning\n", ""));
        assertEquals("partial\n\nerror", AssistantLocalAgentRunner.agentOutput(false, "partial", "error", ""));
        assertEquals("Timed out", AssistantLocalAgentRunner.agentOutput(false, "", "", "Timed out"));
    }

    @Test
    void cloudPromptBuildsProviderChatRequestWithoutMutation() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Review the checkout test plan",
                AssistantCommand.Selection.cloud("github", "openai/gpt-4.1"),
                "PLAN",
                "C:/work/project",
                "",
                true);

        assertFalse(invocation.isLocal());
        assertEquals("autobot_provider_chat", invocation.toolName());
        assertEquals("github", invocation.arguments().get("provider").getAsString());
        assertEquals("openai/gpt-4.1", invocation.arguments().get("model").getAsString());
        assertEquals("PLAN", invocation.arguments().get("mode").getAsString());
        assertEquals("Review the checkout test plan", invocation.arguments().get("prompt").getAsString());
        assertFalse(invocation.arguments().get("allowSourceMutation").getAsBoolean());
    }

    @Test
    void appAndPluginRuntimePromptsStayLocalWithHandoffMessage() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Open this in Copilot",
                AssistantCommand.Selection.local("COPILOT", "IDE_PLUGIN"),
                "ASK",
                "C:/work/project",
                "",
                false);

        assertTrue(invocation.isLocal());
        assertTrue(invocation.localResponse().contains("GitHub Copilot plugin"));
    }

    @Test
    void slashCommandsMapToCuratedTools() {
        assertEquals("shaft_guide_search", command("/guide locators").toolName());
        assertEquals("locators", command("/guide locators").arguments().get("query").getAsString());
        assertEquals("test_automation_scenarios", command("/scenarios checkout").toolName());
        assertEquals("checkout", command("/scenarios checkout").arguments().get("intent").getAsString());
        assertEquals("capture_start", command("/record").toolName());
        assertEquals("Chrome", command("/record").arguments().get("browser").getAsString());
        assertEquals("playwright_record_start", command("/record playwright").toolName());
        assertEquals("browser_open_intent", command("/inspect").toolName());
        assertEquals("browser_open_intent", command("/locator").toolName());
        assertEquals("", command("/inspect").arguments().get("targetUrl").getAsString());
        assertEquals("https://example.com", command("/inspect https://example.com sign in").arguments().get("targetUrl").getAsString());
        assertEquals("sign in", command("/inspect https://example.com sign in").arguments().get("userIntent").getAsString());
        assertEquals("test_code_guardrails_check", command("/guardrails driver.element().click(locator);").toolName());
        assertEquals("java", command("/guardrails code").arguments().get("language").getAsString());
        assertEquals("autobot_local_agent_clients", command("/clients").toolName());
        assertEquals("test_automation_scenarios", command("/generatetest login").toolName());
        assertEquals("capture_code_blocks", command("/generatetest recordings/capture-session.json").toolName());
        assertEquals("playwright_recording_code_blocks", command("/generatetest recordings/playwright-session.json").toolName());
    }

    @Test
    void triageAndFixTestFailureRouteToDoctorWithWorkingDirectory() {
        String workingDirectory = "C:/work/project";
        AssistantCommand.Invocation triage = command("/triage", workingDirectory);
        AssistantCommand.Invocation fix = command("/fixTestFailure", workingDirectory);

        assertEquals("doctor_analyze_failed_allure", triage.toolName());
        assertEquals("doctor_analyze_failed_allure", fix.toolName());
        assertEquals("allure-results", triage.arguments().get("allureResultPaths").getAsJsonArray().get(0).getAsString());
        assertEquals(workingDirectory, triage.arguments().get("repositoryRoot").getAsString());
        assertEquals(workingDirectory, fix.arguments().get("repositoryRoot").getAsString());
    }

    @Test
    void helpAndUnknownCommandsStayLocal() {
        assertTrue(command("/help").isLocal());
        assertTrue(command("/help").localResponse().contains("/record"));
        assertTrue(command("/missing").isLocal());
        assertTrue(command("   ").isLocal());
    }

    private static AssistantCommand.Invocation command(String prompt) {
        return command(prompt, ".");
    }

    private static AssistantCommand.Invocation command(String prompt, String workingDirectory) {
        return AssistantCommand.fromPrompt(prompt, "CODEX", "ASK", workingDirectory, "", false);
    }
}
