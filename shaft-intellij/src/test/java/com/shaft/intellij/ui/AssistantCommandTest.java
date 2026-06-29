package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

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
        assertEquals("Plan a resilient login test", invocation.arguments().get("prompt").getAsString());
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
