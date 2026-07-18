package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
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
        assertTrue(invocation.arguments().get("prompt").getAsString().contains("Plan a resilient login test"));
        assertTrue(invocation.arguments().get("prompt").getAsString()
                .contains("Source edits are not enabled for this session"));
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
        AssistantCommand.Invocation wikipedia = AssistantCommand.fromPrompt(
                "open wikipedia and search for SHAFT Engine",
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

        String wikipediaPrompt = wikipedia.arguments().get("prompt").getAsString();
        String alreadyExplicitPrompt = alreadyExplicit.arguments().get("prompt").getAsString();
        String plainPrompt = plain.arguments().get("prompt").getAsString();

        assertAll(
                // Verify Wikipedia-based example is present
                () -> assertTrue(wikipediaPrompt.contains("Wikipedia"), wikipediaPrompt),
                () -> assertTrue(wikipediaPrompt.contains("searchInput"), wikipediaPrompt),
                () -> assertTrue(wikipediaPrompt.contains("mw-search-result-heading"), wikipediaPrompt),
                // Verify stale DuckDuckGo/data-testid references are gone
                () -> assertFalse(wikipediaPrompt.toLowerCase().contains("duckduckgo"), wikipediaPrompt),
                () -> assertFalse(wikipediaPrompt.contains("data-testid='result'"), wikipediaPrompt),
                // Verify the core hint is present
                () -> assertTrue(wikipediaPrompt.contains("If this request requires interacting with a browser"), wikipediaPrompt),
                () -> assertTrue(wikipediaPrompt.contains("open wikipedia and search for SHAFT Engine"), wikipediaPrompt),
                () -> assertTrue(wikipediaPrompt.contains("Source edits are approved for this session"), wikipediaPrompt),
                // The user already mentioning shaft-mcp themselves skips the redundant "use shaft-mcp
                // for browser tasks" boilerplate, but must NOT also drop the unrelated shaft-options
                // hint -- that silently lost every clarifying-question chip UI for any turn that
                // happened to mention shaft-mcp (a real user report: agent questions never rendered
                // as clickable options).
                () -> assertFalse(alreadyExplicitPrompt.contains("If this request requires interacting with a browser"),
                        alreadyExplicitPrompt),
                () -> assertTrue(alreadyExplicitPrompt.contains("tagged shaft-options"), alreadyExplicitPrompt),
                () -> assertFalse(alreadyExplicitPrompt.toLowerCase().contains("duckduckgo"), alreadyExplicitPrompt),
                () -> assertFalse(alreadyExplicitPrompt.contains("data-testid='result'"), alreadyExplicitPrompt),
                // plain must not have duckduckgo reference
                () -> assertFalse(plainPrompt.toLowerCase().contains("duckduckgo"), plainPrompt),
                () -> assertFalse(plainPrompt.contains("data-testid='result'"), plainPrompt),
                () -> assertTrue(plainPrompt.contains("Wikipedia"), plainPrompt),
                // Structured question/options protocol (#3719): the hint now prefers the single
                // trailing JSON line and keeps the fence as documented fallback -- in every prompt
                // variant, including the shaft-mcp-mentioning one that keeps the hint after the
                // boilerplate-omission fix.
                () -> assertTrue(wikipediaPrompt.contains("\"shaft-question\""), wikipediaPrompt),
                () -> assertTrue(alreadyExplicitPrompt.contains("\"shaft-question\""), alreadyExplicitPrompt),
                () -> assertTrue(plainPrompt.contains("\"shaft-question\""), plainPrompt));
    }

    @Test
    void localAndCloudPromptsIncludeConversationContextWhenProvided() {
        String context = "User: Previous target URL was https://duckduckgo.com";
        AssistantCommand.Invocation local = AssistantCommand.fromPrompt(
                "continue with the first result assertion",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true,
                AssistantCommand.OpenFileContext.empty(),
                context);
        AssistantCommand.Invocation cloud = AssistantCommand.fromPrompt(
                "summarize next step",
                AssistantCommand.Selection.cloud("openai", "gpt-5"),
                "ASK",
                "C:/work/project",
                "",
                false,
                AssistantCommand.OpenFileContext.empty(),
                context);

        assertAll(
                () -> assertTrue(local.arguments().get("prompt").getAsString().contains("Conversation context:")),
                () -> assertTrue(local.arguments().get("prompt").getAsString().contains(context)),
                () -> assertTrue(local.arguments().get("prompt").getAsString()
                        .contains("Current request:\nIf this request requires interacting")),
                () -> assertTrue(cloud.arguments().get("prompt").getAsString().contains("Conversation context:")),
                () -> assertTrue(cloud.arguments().get("prompt").getAsString().contains(context)),
                () -> assertTrue(cloud.arguments().get("prompt").getAsString()
                        .contains("Current request:\nIf you need to ask the user a genuine clarifying question")),
                () -> assertTrue(cloud.arguments().get("prompt").getAsString().contains("summarize next step")));
    }

    @Test
    void cloudSelectionDefaultsBlankProviderToGemini() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Summarize this test",
                AssistantCommand.Selection.cloud("", ""),
                "ASK",
                ".",
                "",
                true);

        assertEquals("autobot_provider_chat", invocation.toolName());
        assertEquals("gemini", invocation.arguments().get("provider").getAsString());
        assertEquals("", invocation.arguments().get("model").getAsString());
        assertEquals("ASK", invocation.arguments().get("mode").getAsString());
        assertFalse(invocation.arguments().get("allowSourceMutation").getAsBoolean());
    }

    @Test
    void askOrPlanMcpNaturalLanguagePromptsRequireAgentModeButSlashCommandsDoNot() {
        AssistantCommand.Invocation natural = AssistantCommand.fromPrompt(
                "open duckduckgo and search for SHAFT Engine",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "PLAN",
                ".",
                "",
                false);
        AssistantCommand.Invocation slash = AssistantCommand.fromPrompt(
                "/guide locators",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "PLAN",
                ".",
                "",
                false);

        assertAll(
                () -> assertTrue(AssistantCommand.requiresAgentModeForMcp(
                        "open duckduckgo and search for SHAFT Engine", "PLAN", natural)),
                () -> assertFalse(AssistantCommand.requiresAgentModeForMcp("/guide locators", "PLAN", slash)),
                () -> assertFalse(AssistantCommand.requiresAgentModeForMcp(
                        "open duckduckgo and search for SHAFT Engine", "AGENT", natural)));
    }

    @Test
    void localAgentCodeGenerationPromptsNameShaftMcpPracticeToolsBeforeGeneration() {
        AssistantCommand.Invocation generatedTest = AssistantCommand.fromPrompt(
                "generate a Java login test with a page object",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);
        AssistantCommand.Invocation explicitMcp = AssistantCommand.fromPrompt(
                "use shaft-mcp to generate Java code for this login flow",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);

        String generatedPrompt = generatedTest.arguments().get("prompt").getAsString();
        String explicitPrompt = explicitMcp.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertTrue(generatedPrompt.contains("This is a code-generation request. Before returning Java:"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Call shaft_guide_search"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Call test_automation_scenarios"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("call shaft_coding_partner_plan"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Reuse existing tests, page objects"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("MUST follow the Page Object Model"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains(
                        "do not put driver.element()/locator calls directly in a @Test method body"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("fluent design and action chaining"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("start a fresh session with capture_start_codegen"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("ask the user to confirm the exact target URL"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Do not infer canonical URLs"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Never substitute a different URL, domain, or example page"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("do not call live browser tools"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Do not publish locators as verified"), generatedPrompt),
                () -> assertFalse(generatedPrompt.contains("Live browser verification was explicitly requested"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Never generate SHAFT.GUI.Locator.xpath"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Call test_code_guardrails_check"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Do not print full repository files"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Put every code snippet in fenced code blocks"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Return only SHAFT-syntax Java"), generatedPrompt),
                () -> assertTrue(explicitPrompt.contains("Call shaft_guide_search"), explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("ask the user to confirm the exact target URL"),
                        explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("do not call live browser tools"), explicitPrompt),
                () -> assertFalse(explicitPrompt.contains("browser_open_intent"), explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("Call test_code_guardrails_check"), explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("Do not print full repository files"), explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("Put every code snippet in fenced code blocks"),
                        explicitPrompt));
    }

    @Test
    void slashCodegenWithLiveInstructionsRoutesToReviewOnlyAgentCodegenPrompt() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "/codegen navigate to https://duckduckgo.com, search for shaft_engine, open the first result and assert that the url is correct.",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                ".",
                "",
                true);

        String prompt = invocation.arguments().has("prompt")
                ? invocation.arguments().get("prompt").getAsString()
                : "";

        assertAll(
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertTrue(prompt.contains("This is a code-generation request. Before returning Java:"), prompt),
                () -> assertTrue(prompt.contains("start a fresh session with capture_start_codegen"), prompt),
                // Natural-language codegen must end in the replay-proving generator, never the
                // generate-only draft tool, so the returned locators are validated live.
                () -> assertTrue(prompt.contains("capture_generate_replay"), prompt),
                () -> assertFalse(prompt.contains("with capture_code_blocks"), prompt),
                () -> assertFalse(prompt.contains("Live browser verification was explicitly requested"), prompt),
                () -> assertTrue(prompt.contains("navigate to https://duckduckgo.com"), prompt),
                () -> assertFalse(prompt.contains("test_automation_scenarios OK")));
    }

    @Test
    void codeOnlyBrowserCodegenPromptDoesNotRouteToLiveBrowserSequence() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "write code only for https://duckduckgo.com search Page Object; do not run browser",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                ".",
                "",
                true);

        String prompt = invocation.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertFalse(invocation.isSequence()),
                () -> assertTrue(prompt.contains("do not call live browser tools"), prompt),
                () -> assertFalse(prompt.contains("Live browser verification was explicitly requested"), prompt));
    }

    @Test
    void localAgentCodeRequestsStayScopedToCurrentOpenFileAndMode() {
        AssistantCommand.OpenFileContext openFile = new AssistantCommand.OpenFileContext(
                "src/test/java/example/LoginTest.java",
                """
                        driver.get("https://example.com/login");
                        driver.findElement(By.id("username")).sendKeys("standard_user");
                        """);
        AssistantCommand.Invocation ask = AssistantCommand.fromPrompt(
                "convert this Selenium code to SHAFT syntax",
                "CODEX",
                "ASK",
                ".",
                "",
                false,
                openFile);
        AssistantCommand.Invocation agent = AssistantCommand.fromPrompt(
                "convert this Selenium code to SHAFT syntax",
                "CODEX",
                "AGENT",
                ".",
                "",
                true,
                openFile);
        AssistantCommand.Invocation cloud = AssistantCommand.fromPrompt(
                "convert this Selenium code to SHAFT syntax",
                AssistantCommand.Selection.cloud("github", "openai/gpt-4.1"),
                "ASK",
                ".",
                "",
                false,
                openFile);

        String askPrompt = ask.arguments().get("prompt").getAsString();
        String agentPrompt = agent.arguments().get("prompt").getAsString();
        String cloudPrompt = cloud.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertTrue(askPrompt.contains("Use only the currently open file"), askPrompt),
                () -> assertTrue(askPrompt.contains("src/test/java/example/LoginTest.java"), askPrompt),
                () -> assertTrue(askPrompt.contains("driver.findElement(By.id(\"username\"))"), askPrompt),
                () -> assertTrue(askPrompt.contains("Ask mode: recommend migrated SHAFT-syntax code"), askPrompt),
                () -> assertFalse(askPrompt.contains("suggest code changes directly inside the IDE"), askPrompt),
                () -> assertTrue(agentPrompt.contains("Agent mode: suggest code changes directly inside the IDE"),
                        agentPrompt),
                () -> assertTrue(agentPrompt.contains("current open class only"), agentPrompt),
                () -> assertEquals("autobot_provider_chat", cloud.toolName()),
                () -> assertTrue(cloudPrompt.contains("Use only the currently open file"), cloudPrompt),
                () -> assertTrue(cloudPrompt.contains("start a fresh session with capture_start_codegen"), cloudPrompt),
                () -> assertTrue(cloudPrompt.contains("Do not publish locators as verified"), cloudPrompt));
    }

    @Test
    void localAgentLoginCodeGenerationRequiresUrlConfirmationBeforeLiveLocatorProof() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "generate code to login to saucedemo",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);

        String prompt = invocation.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertTrue(prompt.contains("generate code to login to saucedemo"), prompt),
                () -> assertTrue(prompt.contains("ask the user to confirm the exact target URL"), prompt),
                () -> assertTrue(prompt.contains("Do not infer canonical URLs"), prompt),
                () -> assertTrue(prompt.contains("Never substitute a different URL, domain, or example page"), prompt),
                () -> assertTrue(prompt.contains("start a fresh session with capture_start_codegen"), prompt),
                () -> assertFalse(prompt.contains("Live browser verification was explicitly requested"), prompt),
                () -> assertTrue(prompt.contains("Do not publish locators as verified"), prompt),
                () -> assertTrue(prompt.contains("Return only SHAFT-syntax Java"), prompt));
    }

    @Test
    void localAgentCodeGenerationCanRequestExplicitLiveBrowserVerification() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "generate code to login to https://example.com and verify locators live",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);

        String prompt = invocation.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertTrue(prompt.contains("Live browser verification was explicitly requested"), prompt),
                () -> assertTrue(prompt.contains("Open a real browser session"), prompt),
                () -> assertTrue(prompt.contains("browser_open_intent"), prompt),
                () -> assertTrue(prompt.contains("element_type, element_click, or natural_act"), prompt),
                () -> assertTrue(prompt.contains("Return only SHAFT-syntax Java"), prompt));
    }

    @Test
    void agentModeWithoutSourceEditsAddsSourceMutationGuardInPrompt() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Implement this login flow in Java",
                "CODEX",
                "AGENT",
                ".",
                "",
                false);

        assertTrue(invocation.arguments().get("prompt").getAsString().contains("Source edits are not enabled for this session"));
        assertTrue(invocation.arguments().get("prompt").getAsString().contains("Do not apply patches"));
    }

    @Test
    void agentModeWithSourceEditsAddsAffirmativeSourceApprovalInPrompt() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Implement this login flow in Java",
                "CODEX",
                "AGENT",
                ".",
                "",
                true);

        String prompt = invocation.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertTrue(prompt.contains("Source edits are approved for this session"), prompt),
                () -> assertFalse(prompt.contains("Source edits are not enabled"), prompt));
    }

    @Test
    void nonAgentModesOmitSourceMutationConstantsFromPrompt() {
        AssistantCommand.Invocation askInvocation = AssistantCommand.fromPrompt(
                "Explain this login flow", "CODEX", "ASK", ".", "", true);
        AssistantCommand.Invocation planInvocation = AssistantCommand.fromPrompt(
                "Plan this login flow", "CODEX", "PLAN", ".", "", true);

        assertAll(
                () -> assertFalse(askInvocation.arguments().get("prompt").getAsString()
                        .contains("Source edits are not enabled")),
                () -> assertFalse(askInvocation.arguments().get("prompt").getAsString()
                        .contains("Source edits are approved")),
                () -> assertFalse(planInvocation.arguments().get("prompt").getAsString()
                        .contains("Source edits are not enabled")),
                () -> assertFalse(planInvocation.arguments().get("prompt").getAsString()
                        .contains("Source edits are approved")));
    }

    @Test
    void directLocalAgentRunnerUsesSelectedCliDefaults() {
        AssistantCommand.Invocation codexAsk = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "", false);
        AssistantCommand.Invocation codexAgent = AssistantCommand.fromPrompt(
                "Implement the fix", "CODEX", "AGENT", ".", "", true);
        AssistantCommand.Invocation codexAgentNoSource = AssistantCommand.fromPrompt(
                "Implement the fix", "CODEX", "AGENT", ".", "", false);
        AssistantCommand.Invocation claudePlan = AssistantCommand.fromPrompt(
                "Plan the fix", "CLAUDE_CODE", "PLAN", ".", "", false);
        AssistantCommand.Invocation custom = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "custom-agent --safe", false);

        // Deliberate flag change (R3-T1): codex exec now always adds the experimental --json flag,
        // and claude always adds --output-format stream-json --verbose, so the runner receives
        // incremental NDJSON events instead of a single buffered blob. Custom commands are untouched.
        assertEquals(List.of("codex", "exec", "--sandbox", "read-only", "--json", "-"),
                AssistantLocalAgentRunner.commandFor(codexAsk.arguments()));
        assertEquals(List.of(
                "codex", "exec",
                "--sandbox", "workspace-write",
                "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                "--json",
                "-"),
                AssistantLocalAgentRunner.commandFor(codexAgent.arguments()));
        // T2-runner-approval-bridge: the auto-approve config flag is only emitted when the approval
        // store grants it (allowSourceMutation is false here, so it must be absent) instead of being
        // hard-coded unconditionally as it was before.
        assertEquals(List.of(
                "codex", "exec",
                "--sandbox", "read-only",
                "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                "--json",
                "-"),
                AssistantLocalAgentRunner.commandFor(codexAgentNoSource.arguments()));
        assertEquals(List.of("claude", "--print", "--permission-mode", "plan",
                        "--output-format", "stream-json", "--verbose"),
                AssistantLocalAgentRunner.commandFor(claudePlan.arguments()));
        assertEquals(List.of("custom-agent", "--safe"), AssistantLocalAgentRunner.commandFor(custom.arguments()));
    }

    @Test
    void directLocalAgentRunnerAppliesSelectedModelAndEffort() {
        AssistantCommand.Invocation codex = AssistantCommand.fromPrompt(
                "Explain this failure",
                AssistantCommand.Selection.local("CODEX", "CLI", "gpt-5.2-codex", "HIGH"),
                "ASK", ".", "", false);
        AssistantCommand.Invocation claude = AssistantCommand.fromPrompt(
                "Explain this failure",
                AssistantCommand.Selection.local("CLAUDE", "CLI", "claude-sonnet-5", "LOW"),
                "ASK", ".", "", false);
        AssistantCommand.Invocation copilot = AssistantCommand.fromPrompt(
                "Explain this failure",
                AssistantCommand.Selection.local("COPILOT", "CLI", "gpt-5.2", "DEFAULT"),
                "ASK", ".", "", false);

        assertAll(
                () -> assertEquals(List.of(
                                "codex", "exec",
                                "--model", "gpt-5.2-codex",
                                "-c", "model_reasoning_effort=\"high\"",
                                "--sandbox", "read-only", "--json", "-"),
                        AssistantLocalAgentRunner.commandFor(codex.arguments())),
                // Codex receives the effort as a CLI config flag, so its prompt stays clean.
                () -> assertFalse(codex.arguments().get("prompt").getAsString()
                        .contains("reasoning effort")),
                () -> assertEquals(List.of(
                                "claude", "--print",
                                "--model", "claude-sonnet-5",
                                "--output-format", "stream-json", "--verbose"),
                        AssistantLocalAgentRunner.commandFor(claude.arguments())),
                // Claude Code has no effort flag, so the prompt carries the preference instead.
                () -> assertTrue(claude.arguments().get("prompt").getAsString()
                        .startsWith("Use low reasoning effort for this request.")),
                () -> assertEquals(List.of("copilot", "ask", "--model", "gpt-5.2"),
                        AssistantLocalAgentRunner.commandFor(copilot.arguments())),
                // Default effort adds neither a flag nor a prompt hint.
                () -> assertFalse(copilot.arguments().get("prompt").getAsString()
                        .contains("reasoning effort")));
    }

    @Test
    void cloudInvocationCarriesSelectedModelAndEffortHint() {
        AssistantCommand.Invocation withEffort = AssistantCommand.fromPrompt(
                "Explain this failure",
                AssistantCommand.Selection.cloud("gemini", "gemini-3.5-pro", "MEDIUM"),
                "ASK", ".", "", false);
        AssistantCommand.Invocation defaultEffort = AssistantCommand.fromPrompt(
                "Explain this failure",
                AssistantCommand.Selection.cloud("gemini", "", ""),
                "ASK", ".", "", false);

        assertAll(
                () -> assertEquals("autobot_provider_chat", withEffort.toolName()),
                () -> assertEquals("gemini-3.5-pro", withEffort.arguments().get("model").getAsString()),
                () -> assertTrue(withEffort.arguments().get("prompt").getAsString()
                        .startsWith("Use medium reasoning effort for this request.")),
                () -> assertFalse(defaultEffort.arguments().get("prompt").getAsString()
                        .contains("reasoning effort")));
    }

    @Test
    void directCustomAgentCommandRequiresSourceEditApprovalBecauseItCannotBeSandboxed() throws Exception {
        AssistantCommand.Invocation custom = AssistantCommand.fromPrompt(
                "Inspect the browser",
                "CODEX",
                "AGENT",
                ".",
                "custom-agent --unsafe",
                false);

        ShaftMcpToolResult result = AssistantLocalAgentRunner.start(custom).future().get(5, TimeUnit.SECONDS);

        assertFalse(result.success());
        assertTrue(result.output().contains("Custom Agent commands require Allow source edits"));
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
