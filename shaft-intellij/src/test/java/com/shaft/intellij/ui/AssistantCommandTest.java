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
                Never start an interactive user-driven recording (capture_start, playwright_record_start, mobile_record_start): your MCP session ends with this turn and would kill the recording seconds after it starts. Tell the user to ask the SHAFT panel to record instead.
                A scripted capture_start_codegen session that you drive and capture_stop within this same turn is allowed.
                Generated Java code must use SHAFT syntax only: SHAFT.GUI.WebDriver, driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator.
                Never generate SHAFT.GUI.Locator.xpath(...); use smart locators, the SHAFT locator builder, or By.xpath only as a last fallback.
                Never generate raw Selenium code such as WebDriver, ChromeDriver, driver.get(...), driver.findElement(...), or direct WebElement actions.
                For repeated search-result anchors, scope the locator to the first result container; for DuckDuckGo use `(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']`.
                If you need to ask the user a genuine clarifying question with a short list of concrete choices (2-6 short options), end your answer with a fenced code block tagged shaft-options containing a JSON array of the option labels (for example: a fence tagged shaft-options wrapping ["Use the sample page", "I'll give you a URL"]); omit this block for ordinary narrative answers.

                open duckduckgo and search for SHAFT Engine

                Source edits are approved for this session.
                You may apply patches, write files, and run filesystem commands needed to make the requested source changes.""",
                duckDuckGo.arguments().get("prompt").getAsString());
        assertEquals("""
                use shaft-mcp to open mobile app

                Source edits are approved for this session.
                You may apply patches, write files, and run filesystem commands needed to make the requested source changes.""",
                alreadyExplicit.arguments().get("prompt").getAsString());
        assertEquals("""
                If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.
                For WebDriver browser tasks, call driver_initialize before browser_* tools; do not use Playwright unless requested.
                Never start an interactive user-driven recording (capture_start, playwright_record_start, mobile_record_start): your MCP session ends with this turn and would kill the recording seconds after it starts. Tell the user to ask the SHAFT panel to record instead.
                A scripted capture_start_codegen session that you drive and capture_stop within this same turn is allowed.
                Generated Java code must use SHAFT syntax only: SHAFT.GUI.WebDriver, driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator.
                Never generate SHAFT.GUI.Locator.xpath(...); use smart locators, the SHAFT locator builder, or By.xpath only as a last fallback.
                Never generate raw Selenium code such as WebDriver, ChromeDriver, driver.get(...), driver.findElement(...), or direct WebElement actions.
                For repeated search-result anchors, scope the locator to the first result container; for DuckDuckGo use `(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']`.
                If you need to ask the user a genuine clarifying question with a short list of concrete choices (2-6 short options), end your answer with a fenced code block tagged shaft-options containing a JSON array of the option labels (for example: a fence tagged shaft-options wrapping ["Use the sample page", "I'll give you a URL"]); omit this block for ordinary narrative answers.

                Explain the current test failure""", plain.arguments().get("prompt").getAsString());
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
        assertEquals("""
                If you need to ask the user a genuine clarifying question with a short list of concrete choices (2-6 short options), end your answer with a fenced code block tagged shaft-options containing a JSON array of the option labels (for example: a fence tagged shaft-options wrapping ["Use the sample page", "I'll give you a URL"]); omit this block for ordinary narrative answers.

                Review the checkout test plan""",
                invocation.arguments().get("prompt").getAsString());
        assertFalse(invocation.arguments().get("allowSourceMutation").getAsBoolean());
    }

    @Test
    void cloudPromptsIncludeShaftOptionsHintSoClarifyingQuestionsRenderChips() {
        // Issue #3690: the shaft-options fence instruction (added to SHAFT_MCP_USAGE_HINT by #3674)
        // must also reach cloud-provider chat prompts (autobot_provider_chat), not just local-agent
        // ones, so a clarifying question asked by a cloud model still renders selectable chips.
        AssistantCommand.Invocation nonCodegen = AssistantCommand.fromPrompt(
                "Which login flow should I test?",
                AssistantCommand.Selection.cloud("openai", "gpt-5"),
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation codegen = AssistantCommand.fromPrompt(
                "generate test for the login page",
                AssistantCommand.Selection.cloud("openai", "gpt-5"),
                "ASK",
                ".",
                "",
                false);

        String nonCodegenPrompt = nonCodegen.arguments().get("prompt").getAsString();
        String codegenPrompt = codegen.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertTrue(nonCodegenPrompt.contains("tagged shaft-options"), nonCodegenPrompt),
                () -> assertTrue(nonCodegenPrompt.contains("Which login flow should I test?"), nonCodegenPrompt),
                () -> assertTrue(codegenPrompt.contains("tagged shaft-options"), codegenPrompt));
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
    void recordingIntentsRouteToPluginRecorderNotLocalAgent() {
        // A recording started by a one-shot local agent turn dies with that turn's MCP child
        // process (issue #3429), so every natural recording phrasing — including the Assistant's
        // own suggestion prefills — must resolve to a plugin-run recorder tool instead of the agent.
        AssistantCommand.Invocation webPrefill =
                command("Record my browser actions on https://duckduckgo.com");
        AssistantCommand.Invocation mobilePrefill =
                command("Record my mobile actions on the Android emulator");

        assertAll(
                () -> assertEquals("capture_start", webPrefill.toolName()),
                () -> assertEquals("https://duckduckgo.com",
                        webPrefill.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start",
                        command("record a web flow on https://example.com").toolName()),
                () -> assertEquals("capture_start",
                        command("Record my actions on https://example.com/login").toolName()),
                () -> assertEquals("mobile_record_start", mobilePrefill.toolName()),
                () -> assertEquals("mobile_record_start",
                        command("record my app on the emulator").toolName()),
                // Non-recording "record"-adjacent asks still go to the agent untouched.
                () -> assertEquals("autobot_local_agent_run",
                        command("record a video of the sprint demo").toolName()));
    }

    @Test
    void mobileRecordingOutweighsBrowserRecordingWithoutCrossPredicateExclusion() {
        // NATURAL_INTENTS used to resolve this by having isBrowserRecordingIntent explicitly call
        // isMobileRecordingStartIntent and bail out (the issue #3429 patch). That cross-predicate
        // exclusion is gone now: directIntent scores every matching intent and mobile-recording's
        // weight (WEIGHT_MOBILE_RECORDING) is set above browser-recording's (WEIGHT_BROWSER_RECORDING),
        // so the same phrasing still starts the mobile recorder.
        AssistantCommand.Invocation mobileWordPrefill =
                command("Record my mobile actions on the Android emulator");
        // Genuinely ambiguous chosen example: dropping "mobile" still leaves "my actions on" (a
        // capture_start keyword) AND "android"/"emulator" (mobile_record_start keywords), so both
        // isBrowserRecordingIntent and isMobileRecordingStartIntent match this exact phrase under
        // their raw keyword lists -- only the weight difference decides the winner.
        AssistantCommand.Invocation ambiguousPhrase =
                command("record my actions on the android emulator");

        assertAll(
                () -> assertEquals("mobile_record_start", mobileWordPrefill.toolName()),
                () -> assertEquals("mobile_record_start", ambiguousPhrase.toolName()));
    }

    @Test
    void naturalIntentTiesResolveByDeclarationOrder() {
        // browser-control and mobile-control share WEIGHT_TOPIC_CONTROL: both are equally broad
        // "topic + action-word" recognizers, so a phrase satisfying both predicates' raw conditions
        // must resolve by declaration order (browser-control is declared first in NATURAL_INTENTS),
        // exactly like the old first-match linear scan. "open mobile inspector http://..." mentions
        // an action word + URL (browser-control) and "mobile" + "inspector" (mobile-control).
        for (int attempt = 0; attempt < 5; attempt++) {
            AssistantCommand.Invocation invocation = command("open mobile inspector http://example.com");

            assertAll("attempt " + attempt,
                    () -> assertTrue(invocation.isSequence()),
                    () -> assertEquals("driver_initialize", invocation.toolCalls().get(0).toolName()),
                    () -> assertEquals("browser_open_intent", invocation.toolCalls().get(1).toolName()));
        }
    }

    @Test
    void slashCommandsMapToCuratedTools() {
        assertEquals("shaft_guide_search", command("/guide locators").toolName());
        assertEquals("locators", command("/guide locators").arguments().get("query").getAsString());
        assertEquals("shaft_guide_search", command("/docs locators").toolName());
        assertEquals("test_automation_scenarios", command("/scenarios checkout").toolName());
        assertEquals("checkout", command("/scenarios checkout").arguments().get("intent").getAsString());
        assertEquals("capture_start", command("/record-web https://example.com").toolName());
        assertEquals("https://example.com", command("/record-web https://example.com")
                .arguments().get("targetUrl").getAsString());
        assertEquals("capture_start", command("/record").toolName());
        assertEquals("Chrome", command("/record").arguments().get("browser").getAsString());
        assertEquals("playwright_record_start", command("/record playwright").toolName());
        assertTrue(command("/inspect").isLocal());
        assertTrue(command("/locator").isLocal());
        assertTrue(command("/inspect https://example.com sign in").isSequence());
        assertEquals(List.of("driver_initialize", "browser_open_intent"),
                command("/inspect https://example.com sign in").toolCalls().stream()
                        .map(AssistantCommand.ToolCall::toolName).toList());
        assertEquals("https://example.com", command("/inspect https://example.com sign in")
                .toolCalls().get(1).arguments().get("targetUrl").getAsString());
        assertEquals("sign in", command("/inspect https://example.com sign in")
                .toolCalls().get(1).arguments().get("userIntent").getAsString());
        assertEquals("test_code_guardrails_check", command("/guardrails driver.element().click(locator);").toolName());
        assertEquals("java", command("/guardrails code").arguments().get("language").getAsString());
        assertEquals("autobot_local_agent_clients", command("/assistant").toolName());
        assertEquals("autobot_local_agent_clients", command("/agent").toolName());
        assertEquals("autobot_local_agent_clients", command("/ask").toolName());
        assertEquals("autobot_local_agent_clients", command("/plan").toolName());
        assertEquals("autobot_local_agent_clients", command("/clients").toolName());
        assertEquals("test_automation_scenarios", command("/generatetest login").toolName());
        // Explicit codegen against a Capture recording re-executes it (generate + compile +
        // headless replay), per issue #3409; Playwright/mobile recordings replay through their
        // schema-native replay tools so their returned code blocks are proven live too.
        assertEquals("capture_generate_replay", command("/generatetest recordings/capture-session.json").toolName());
        assertTrue(command("/generatetest recordings/capture-session.json").arguments().get("replay").getAsBoolean());
        assertFalse(command("/generatetest recordings/capture-session.json").arguments().get("useAi").getAsBoolean());
        assertTrue(command("/generatetest recordings/playwright-session.json").isSequence());
        assertEquals(List.of("playwright_initialize", "playwright_replay_recording"),
                command("/generatetest recordings/playwright-session.json").toolCalls().stream()
                        .map(AssistantCommand.ToolCall::toolName).toList());
        assertEquals("recordings/playwright-session.json",
                command("/generatetest recordings/playwright-session.json")
                        .toolCalls().get(1).arguments().get("recordingPath").getAsString());
        assertEquals("capture_generate_replay", command("/codegen recordings/capture-session.json").toolName());
        assertEquals("mobile_replay_recording", command("/codegen mobile recordings/mobile-session.json").toolName());
        assertEquals("recordings/mobile-session.json",
                command("/codegen mobile recordings/mobile-session.json")
                        .arguments().get("recordingPath").getAsString());
        assertEquals("mobile_inspector_record_prepare",
                command("/record-mobile inspector Android recordings/inspector.json").toolName());
    }

    @Test
    void slashUpgradeShowsCopyableAutomatedUpgradeScript() {
        AssistantCommand.Invocation upgrade = command("/upgrade .");
        String response = upgrade.localResponse();

        assertAll(
                () -> assertTrue(upgrade.isLocal()),
                // Outside Agent mode the command explains exactly how to authorize the agent-run
                // upgrade instead of silently pasting a recipe (issue #3426 B6)...
                () -> assertTrue(response.contains("Allow source edits"), response),
                () -> assertTrue(response.contains("**Agent**"), response),
                // ...while still offering the manual command for users who prefer to run it.
                () -> assertTrue(response.contains("```shell\npython -c \"import runpy,sys,urllib.request as u;")),
                () -> assertTrue(response.contains("u.urlretrieve('https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/shaft-upgrader/upgrade_to_modular_shaft.py',p)")),
                () -> assertTrue(response.contains("sys.argv=[p,'--project','.'];runpy.run_path(p,run_name='__main__')")),
                () -> assertTrue(response.contains("\n```")));
    }

    @Test
    void slashUpgradeInAgentModeWithSourceEditsRunsTheUpgradeThroughTheLocalAgent() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "/upgrade .",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true);
        String prompt = invocation.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertTrue(invocation.arguments().get("allowSourceMutation").getAsBoolean()),
                // The agent performs the upgrade itself, non-interactively, and must narrate.
                () -> assertTrue(prompt.contains("Perform the upgrade yourself"), prompt),
                () -> assertTrue(prompt.contains("shaft_project_upgrade"), prompt),
                () -> assertTrue(prompt.contains("'--yes'"), prompt),
                () -> assertTrue(prompt.contains("mvn -B -q test-compile"), prompt),
                () -> assertTrue(prompt.contains("Never reply with a bare confirmation like \"Done\""), prompt));
    }

    @Test
    void slashUpgradeOnCloudRouteKeepsTheManualCommand() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "/upgrade .",
                AssistantCommand.Selection.cloud("gemini", "gemini-2.5-flash"),
                "AGENT",
                ".",
                "",
                true);

        assertAll(
                () -> assertTrue(invocation.isLocal()),
                () -> assertTrue(invocation.localResponse().contains("upgrade_to_modular_shaft.py")));
    }

    @Test
    void naturalUpgradePhraseStillReachesAgentPerformedUpgradePath() {
        // isProjectUpgradeIntent used to also match "upgrade shaft project"/"upgrade this shaft
        // project", but both branches were dead: isNaturalUpgradeIntent (checked in fromPrompt
        // before directIntent -- and this predicate -- is ever consulted) already intercepts this
        // phrasing and routes it to the agent-performed upgrade (issue #3426 B6). Removing the dead
        // branches from the predicate must not change that routing.
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "upgrade shaft project .",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true);
        String prompt = invocation.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertFalse(invocation.isLocal()),
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertTrue(invocation.arguments().get("allowSourceMutation").getAsBoolean()),
                () -> assertTrue(prompt.contains("Perform the upgrade yourself"), prompt),
                () -> assertTrue(prompt.contains("shaft_project_upgrade"), prompt));
    }

    @Test
    void commandRegistryExposesCoreByDefaultAndExpertCommandsInExpertMode() {
        List<String> coreHints = AssistantCommand.commandHints(false).stream()
                .map(AssistantCommand.CommandHint::canonical).toList();
        List<String> allHints = AssistantCommand.commandHints(true).stream()
                .map(AssistantCommand.CommandHint::canonical).toList();
        String coreTooltip = AssistantCommand.commandTooltip(false);
        String expertTooltip = AssistantCommand.commandTooltip(true);

        assertAll(
                // default composer shows only the five core entry points
                () -> assertEquals(List.of("/record", "/record-mobile", "/codegen", "/doctor", "/upgrade"),
                        coreHints),
                // Expert mode reveals the rest, including the two new commands
                () -> assertTrue(allHints.containsAll(List.of("/record", "/record-mobile", "/codegen", "/doctor",
                        "/upgrade", "/partner", "/guide", "/guardrails", "/browser", "/mobile", "/project", "/verify",
                        "/skills"))),
                () -> assertEquals(AssistantCommand.commandHints().size(), allHints.size()),
                () -> assertTrue(coreTooltip.contains("/record-web")),
                () -> assertFalse(coreTooltip.contains("/partner")),
                () -> assertFalse(coreTooltip.contains("/verify")),
                () -> assertFalse(coreTooltip.contains("/skills")),
                () -> assertTrue(expertTooltip.contains("/partner")),
                () -> assertTrue(expertTooltip.contains("/verify")),
                () -> assertTrue(expertTooltip.contains("/skills")),
                () -> assertFalse(expertTooltip.contains("/commands")),
                () -> assertFalse(expertTooltip.contains("/assistant")),
                () -> assertFalse(expertTooltip.contains("/mobile-record")));
    }

    @Test
    void commandHelpShowsCoreCommandsAndPointsToExpertMode() {
        AssistantCommand.Invocation help = command("/commands");
        String response = help.localResponse();

        assertAll(
                () -> assertTrue(help.isLocal()),
                () -> assertTrue(response.contains("**/codegen**")),
                () -> assertTrue(response.contains("**/record**")),
                () -> assertTrue(response.contains("**/record-mobile**")),
                () -> assertTrue(response.contains("**/doctor**")),
                () -> assertTrue(response.contains("**/upgrade**")),
                // expert commands are not shown as sections in the default help
                () -> assertFalse(response.contains("**/guide**")),
                () -> assertFalse(response.contains("**/partner**")),
                // but the Expert-mode footer names them
                () -> assertTrue(response.contains("Expert mode")),
                () -> assertTrue(response.contains("/verify")),
                () -> assertTrue(response.contains("/skills")),
                () -> assertTrue(response.contains("```text\n/codegen recordings/intellij-capture.json\n```")),
                () -> assertTrue(response.contains("```text\n/record https://example.com\n```")),
                () -> assertTrue(response.contains("```text\n/upgrade .\n```")),
                () -> assertFalse(response.contains("/commands -")),
                () -> assertFalse(response.contains("/assistant -")));
    }

    @Test
    void verifyCommandRoutesToGuardedFocusedVerification() {
        assertEquals("verify_run_focused", command("/verify mvn -q test-compile").toolName());
        assertEquals("verify_run_focused", command("/verify").toolName());
        assertEquals("verify_run_focused", command("/check").toolName());
    }

    @Test
    void skillsCommandListsAuthoringSkills() {
        AssistantCommand.Invocation invocation = command("/skills");
        assertAll(
                () -> assertTrue(invocation.isLocal()),
                () -> assertTrue(invocation.localResponse().contains("$writing-shaft-tests")),
                () -> assertTrue(invocation.localResponse().contains("$choosing-shaft-locators")),
                () -> assertTrue(invocation.localResponse().contains("$verifying-and-applying-shaft-changes")));
    }

    @Test
    void setupGateOnlyAppliesToDirectMcpFeatureInvocations() {
        AssistantCommand.Invocation broadLocal = AssistantCommand.fromPrompt(
                "Plan how to use mobile recording in this project",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation broadCloud = AssistantCommand.fromPrompt(
                "Plan how to use mobile recording in this project",
                AssistantCommand.Selection.cloud("github", "openai/gpt-4.1"),
                "PLAN",
                ".",
                "",
                false);
        AssistantCommand.Invocation explicitFeature = command("start mobile recording");
        AssistantCommand.Invocation slashFeature = command("/guide locators");
        AssistantCommand.Invocation localHelp = command("/help");

        assertAll(
                () -> assertEquals("autobot_local_agent_run", broadLocal.toolName()),
                () -> assertFalse(broadLocal.requiresMcpConfiguration()),
                () -> assertEquals("autobot_provider_chat", broadCloud.toolName()),
                () -> assertFalse(broadCloud.requiresMcpConfiguration()),
                () -> assertTrue(explicitFeature.requiresMcpConfiguration()),
                () -> assertTrue(slashFeature.requiresMcpConfiguration()),
                () -> assertFalse(localHelp.requiresMcpConfiguration()));
    }

    @Test
    void browserCommandsCreateOrderedToolSequencesWhenSessionSetupIsRequired() {
        AssistantCommand.Invocation webdriver = command("/browser open https://example.com sign in");
        AssistantCommand.Invocation inspectAlias = command("/locator https://example.com sign in");
        AssistantCommand.Invocation natural = command("open https://example.com and inspect the sign in link");
        AssistantCommand.Invocation playwright = command("/web playwright open https://example.com");

        assertAll(
                () -> assertTrue(webdriver.isSequence()),
                () -> assertEquals(List.of("driver_initialize", "browser_open_intent"),
                        webdriver.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertEquals("CHROME", webdriver.toolCalls().get(0).arguments().get("targetBrowser").getAsString()),
                () -> assertEquals("https://example.com",
                        webdriver.toolCalls().get(1).arguments().get("targetUrl").getAsString()),
                () -> assertEquals("sign in", webdriver.toolCalls().get(1).arguments().get("userIntent").getAsString()),
                () -> assertEquals(List.of("driver_initialize", "browser_open_intent"),
                        inspectAlias.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertEquals(List.of("driver_initialize", "browser_open_intent"),
                        natural.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertTrue(playwright.isSequence()),
                () -> assertEquals(List.of("playwright_initialize", "playwright_browser_navigate"),
                        playwright.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertEquals("chrome", playwright.toolCalls().get(0).arguments().get("browser").getAsString()),
                () -> assertFalse(playwright.toolCalls().get(0).arguments().get("headless").getAsBoolean()),
                () -> assertEquals("https://example.com",
                        playwright.toolCalls().get(1).arguments().get("targetUrl").getAsString()));
    }

    @Test
    void browserCommandsExposeCommonDirectControls() {
        AssistantCommand.Invocation screenshot = command("/browser screenshot target/shaft-browser/home.png");

        assertAll(
                () -> assertEquals("browser_take_screenshot", screenshot.toolName()),
                () -> assertEquals("target/shaft-browser/home.png",
                        screenshot.arguments().get("outputPath").getAsString()),
                () -> assertFalse(screenshot.arguments().get("includeBase64").getAsBoolean()),
                () -> assertEquals("browser_get_page_dom", command("/browser dom").toolName()),
                () -> assertEquals("browser_get_title", command("/browser title").toolName()),
                () -> assertEquals("browser_refresh", command("/browser refresh").toolName()),
                () -> assertEquals("browser_navigate_back", command("/browser back").toolName()),
                () -> assertEquals("browser_navigate_forward", command("/browser forward").toolName()),
                () -> assertEquals("browser_maximize_window", command("/browser maximize").toolName()),
                () -> assertEquals("browser_fullscreen_window", command("/browser fullscreen").toolName()),
                () -> assertEquals("driver_quit", command("/browser quit").toolName()));
    }

    @Test
    void mobileCommandsMapToControlRecordingCodegenAndInspectorTools() {
        AssistantCommand.Invocation nativeSession = command("/mobile native Android Pixel_6");
        AssistantCommand.Invocation webSession = command("/mobile web https://example.com");
        AssistantCommand.Invocation inspector = command("/mobile-record inspector Android recordings/inspector.json");
        AssistantCommand.Invocation unknown = command("/mobile calibrate");

        assertAll(
                () -> assertEquals("mobile_toolchain_status", command("/mobile doctor Android").toolName()),
                () -> assertEquals("Android",
                        command("/mobile status Android").arguments().get("platformName").getAsString()),
                () -> assertEquals("mobile_initialize_native", nativeSession.toolName()),
                () -> assertEquals("Android", nativeSession.arguments().get("platformName").getAsString()),
                () -> assertEquals("Pixel_6", nativeSession.arguments().get("deviceName").getAsString()),
                () -> assertEquals("mobile_initialize_web_emulation", webSession.toolName()),
                () -> assertEquals("https://example.com", webSession.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("CHROME", webSession.arguments().get("browser").getAsString()),
                () -> assertFalse(webSession.arguments().get("headless").getAsBoolean()),
                () -> assertEquals("mobile_get_accessibility_tree", command("/mobile tree").toolName()),
                () -> assertEquals("mobile_take_screenshot", command("/mobile screenshot target/mobile.png").toolName()),
                () -> assertFalse(command("/mobile screenshot target/mobile.png").arguments().get("includeBase64").getAsBoolean()),
                () -> assertEquals("mobile_get_contexts", command("/mobile contexts").toolName()),
                () -> assertEquals("mobile_switch_context", command("/mobile switch WEBVIEW_chrome").toolName()),
                () -> assertEquals("WEBVIEW_chrome", command("/mobile switch WEBVIEW_chrome").arguments().get("contextName").getAsString()),
                () -> assertEquals("mobile_switch_context", command("/mobile context NATIVE_APP").toolName()),
                () -> assertEquals("mobile_record_start", command("/mobile-record start recordings/mobile.json").toolName()),
                () -> assertEquals("recordings/mobile.json",
                        command("/mobile-record start recordings/mobile.json").arguments().get("outputPath").getAsString()),
                () -> assertEquals("mobile_record_stop", command("/app-record stop").toolName()),
                () -> assertEquals("mobile_inspector_record_prepare", inspector.toolName()),
                () -> assertEquals("mobile_inspector_record_prepare", command("/inspector-record Android recordings/inspector.json").toolName()),
                () -> assertEquals("Android", inspector.arguments().get("platformName").getAsString()),
                () -> assertEquals("recordings/inspector.json", inspector.arguments().get("outputPath").getAsString()),
                () -> assertEquals("mobile_recording_code_blocks",
                        command("/mobile-codegen recordings/mobile.json").toolName()),
                () -> assertEquals("mobile_replay_recording",
                        command("/mobile-replay recordings/mobile.json").toolName()),
                () -> assertEquals("driver_quit", command("/mobile quit").toolName()),
                () -> assertTrue(unknown.isLocal()),
                () -> assertTrue(unknown.localResponse().contains("Unknown mobile command")));
    }

    @Test
    void browserCommandsExposeSessionActionTools() {
        AssistantCommand.Invocation size = command("/browser size 1920x1080");
        AssistantCommand.Invocation resizeDefault = command("/browser resize");
        AssistantCommand.Invocation deleteCookie = command("/browser delete cookie sessionId");
        AssistantCommand.Invocation network = command("/browser network requests example.com");
        AssistantCommand.Invocation saveSession = command("/browser save session recordings/state.json");
        AssistantCommand.Invocation loadSessionDefault = command("/browser load session");

        assertAll(
                () -> assertEquals("browser_set_window_size", size.toolName()),
                () -> assertEquals(1920, size.arguments().get("width").getAsInt()),
                () -> assertEquals(1080, size.arguments().get("height").getAsInt()),
                () -> assertEquals(1280, resizeDefault.arguments().get("width").getAsInt()),
                () -> assertEquals(800, resizeDefault.arguments().get("height").getAsInt()),
                () -> assertEquals("browser_delete_all_cookies", command("/browser clear cookies").toolName()),
                () -> assertEquals("browser_delete_all_cookies", command("/browser delete all cookies").toolName()),
                () -> assertEquals("browser_delete_cookie", deleteCookie.toolName()),
                () -> assertEquals("sessionId", deleteCookie.arguments().get("cookieName").getAsString()),
                () -> assertTrue(command("/browser delete cookie").isLocal()),
                () -> assertEquals("browser_aria_snapshot", command("/browser aria").toolName()),
                () -> assertEquals("", command("/browser aria").arguments().get("locatorValue").getAsString()),
                () -> assertFalse(command("/browser aria").arguments().has("locatorStrategy")),
                () -> assertEquals("browser_accessibility_audit", command("/browser audit").toolName()),
                () -> assertEquals(0, command("/browser audit").arguments().getAsJsonArray("wcagTags").size()),
                () -> assertEquals(2, command("/browser audit wcag2a wcag21aa").arguments()
                        .getAsJsonArray("wcagTags").size()),
                () -> assertEquals("browser_network_requests", network.toolName()),
                () -> assertEquals("example.com", network.arguments().get("urlFilter").getAsString()),
                () -> assertEquals(50, network.arguments().get("limit").getAsInt()),
                () -> assertEquals("browser_storage_state_save", saveSession.toolName()),
                () -> assertEquals("recordings/state.json", saveSession.arguments().get("filePath").getAsString()),
                () -> assertEquals("browser_storage_state_load",
                        command("/browser load session recordings/state.json").toolName()),
                () -> assertEquals("recordings/state.json", command("/browser load session recordings/state.json")
                        .arguments().get("filePath").getAsString()),
                () -> assertTrue(loadSessionDefault.isLocal()));
    }

    @Test
    void browserSessionActionsAreReachableFromNaturalLanguageWhenTheyNameTheBrowser() {
        assertAll(
                () -> assertEquals("browser_set_window_size",
                        command("resize the browser window to 1920x1080").toolName()),
                () -> assertEquals(1920, command("resize the browser window to 1920x1080")
                        .arguments().get("width").getAsInt()),
                () -> assertEquals("browser_delete_all_cookies",
                        command("clear cookies on the browser").toolName()),
                () -> assertEquals("browser_delete_cookie",
                        command("delete cookie sessionId from the browser").toolName()),
                () -> assertEquals("sessionId", command("delete cookie sessionId from the browser")
                        .arguments().get("cookieName").getAsString()),
                () -> assertEquals("browser_aria_snapshot",
                        command("aria snapshot of the current browser page").toolName()),
                () -> assertEquals("browser_accessibility_audit",
                        command("run an accessibility audit on the browser").toolName()),
                () -> assertEquals("browser_network_requests",
                        command("list network requests from the browser").toolName()),
                // "clear cookies" with no mention of "browser" is deliberately left ambiguous (could
                // be about a different app or a generic question) and falls through to the agent.
                () -> assertEquals("autobot_local_agent_run", command("clear cookies").toolName()));
    }

    @Test
    void playwrightBrowserCommandsExposeNavigationAndWindowTools() {
        assertAll(
                () -> assertEquals("playwright_browser_refresh", command("/browser playwright refresh").toolName()),
                () -> assertEquals("playwright_browser_navigate_back", command("/browser playwright back").toolName()),
                () -> assertEquals("playwright_browser_navigate_forward", command("/browser playwright forward").toolName()),
                () -> assertEquals("playwright_browser_set_window_size", command("/browser playwright size 1024x768").toolName()),
                () -> assertEquals(1024, command("/browser playwright size 1024x768").arguments().get("width").getAsInt()),
                () -> assertEquals(768, command("/browser playwright size 1024x768").arguments().get("height").getAsInt()),
                () -> assertEquals("playwright_browser_new_window", command("/browser playwright newtab https://example.com").toolName()),
                () -> assertEquals("TAB", command("/browser playwright newtab https://example.com").arguments().get("windowType").getAsString()),
                () -> assertEquals("https://example.com", command("/browser playwright newtab https://example.com").arguments().get("targetUrl").getAsString()),
                () -> assertEquals("playwright_browser_new_window", command("/browser playwright newwindow").toolName()),
                () -> assertEquals("WINDOW", command("/browser playwright newwindow").arguments().get("windowType").getAsString()));
    }

    @Test
    void mobileCommandsExposeTouchAndAppLifecycleTools() {
        assertAll(
                () -> assertEquals("mobile_rotate", command("/mobile rotate landscape").toolName()),
                () -> assertEquals("LANDSCAPE", command("/mobile rotate landscape").arguments().get("orientation").getAsString()),
                () -> assertEquals("PORTRAIT", command("/mobile rotate").arguments().get("orientation").getAsString()),
                () -> assertEquals("mobile_hide_keyboard", command("/mobile hide keyboard").toolName()),
                () -> assertEquals("mobile_background_app", command("/mobile background 10").toolName()),
                () -> assertEquals(10, command("/mobile background 10").arguments().get("seconds").getAsInt()),
                () -> assertEquals(5, command("/mobile background").arguments().get("seconds").getAsInt()),
                () -> assertEquals("mobile_activate_app", command("/mobile activate com.example.app").toolName()),
                () -> assertEquals("com.example.app",
                        command("/mobile activate com.example.app").arguments().get("appId").getAsString()),
                () -> assertTrue(command("/mobile activate").isLocal()),
                () -> assertEquals("mobile_rotate", command("rotate the mobile device to landscape").toolName()),
                () -> assertEquals("LANDSCAPE",
                        command("rotate the mobile device to landscape").arguments().get("orientation").getAsString()),
                () -> assertEquals("mobile_hide_keyboard", command("on my mobile, hide keyboard").toolName()),
                () -> assertEquals("mobile_background_app",
                        command("on my mobile, background the app").toolName()),
                () -> assertEquals("mobile_activate_app",
                        command("activate app com.example.app on mobile").toolName()),
                () -> assertEquals("com.example.app", command("activate app com.example.app on mobile")
                        .arguments().get("appId").getAsString()));
    }

    @Test
    void captureStatusIsReachableFromNaturalLanguage() {
        assertAll(
                () -> assertEquals("capture_status", command("recording status").toolName()),
                () -> assertEquals("capture_status", command("capture status").toolName()),
                () -> assertEquals("capture_status", command("is a recording active").toolName()));
    }

    @Test
    void doctorCommandsMapToAllureTraceAndFixRecommendationTools() {
        AssistantCommand.Invocation analyze = command("/doctor target/allure-results", "C:/work/project");
        AssistantCommand.Invocation playwrightAnalyze = command("/doctor playwright target/allure-results");
        AssistantCommand.Invocation suggest = command("/doctor fix target/shaft-doctor/doctor-report.json");
        AssistantCommand.Invocation trace = command("/doctor trace target/shaft-traces");
        AssistantCommand.Invocation triageAlias = command("/triage target/custom-results", "C:/work/project");
        AssistantCommand.Invocation fixAlias = command("/fixTestFailure target/custom-results", "C:/work/project");

        assertAll(
                () -> assertEquals("doctor_analyze_failed_allure", analyze.toolName()),
                () -> assertEquals("target/allure-results",
                        analyze.arguments().getAsJsonArray("allureResultPaths").get(0).getAsString()),
                () -> assertEquals("C:/work/project", analyze.arguments().get("repositoryRoot").getAsString()),
                () -> assertEquals("playwright_doctor_analyze_failed_allure", playwrightAnalyze.toolName()),
                () -> assertEquals("doctor_suggest_fix", suggest.toolName()),
                () -> assertEquals("target/shaft-doctor/doctor-report.json",
                        suggest.arguments().get("jsonReportPath").getAsString()),
                () -> assertEquals("doctor_analyze_trace", trace.toolName()),
                () -> assertEquals("target/shaft-traces", trace.arguments().get("tracePath").getAsString()),
                () -> assertEquals("webdriver", trace.arguments().get("backend").getAsString()),
                () -> assertEquals("doctor_analyze_failed_allure", command("/allure target/allure-results").toolName()),
                () -> assertEquals("target/custom-results",
                        triageAlias.arguments().getAsJsonArray("allureResultPaths").get(0).getAsString()),
                () -> assertEquals("target/custom-results",
                        fixAlias.arguments().getAsJsonArray("allureResultPaths").get(0).getAsString()));
    }

    @Test
    void rawMcpCommandAcceptsExplicitToolAndJsonArguments() {
        AssistantCommand.Invocation raw = command("/mcp browser_take_screenshot {\"outputPath\":\"target/x.png\",\"includeBase64\":false}");

        assertAll(
                () -> assertEquals("browser_get_title", command("/tool browser_get_title {}").toolName()),
                () -> assertEquals("browser_take_screenshot", raw.toolName()),
                () -> assertEquals("target/x.png", raw.arguments().get("outputPath").getAsString()),
                () -> assertTrue(command("/mcp browser_take_screenshot {bad json}").isLocal()));
    }

    @Test
    void partnerCommandRoutesRepositoryIntentToCodingPartnerPlan() {
        AssistantCommand.Invocation slash = command(
                "/partner Log in with valid credentials then verify account menu",
                "C:/work/project");
        AssistantCommand.Invocation natural = command(
                "plan coding partner work for checkout happy path",
                "C:/work/project");

        assertAll(
                () -> assertEquals("shaft_coding_partner_plan", slash.toolName()),
                () -> assertEquals("C:/work/project", slash.arguments().get("repositoryPath").getAsString()),
                () -> assertEquals("Log in with valid credentials then verify account menu",
                        slash.arguments().get("intent").getAsString()),
                () -> assertEquals("WebDriver", slash.arguments().get("backend").getAsString()),
                () -> assertEquals("", slash.arguments().get("currentSourcePath").getAsString()),
                () -> assertEquals("", slash.arguments().get("selectedText").getAsString()),
                () -> assertEquals(0, slash.arguments().getAsJsonArray("artifactPaths").size()),
                () -> assertEquals(10, slash.arguments().get("maxResults").getAsInt()),
                () -> assertEquals("shaft_coding_partner_plan", natural.toolName()),
                () -> assertEquals("checkout happy path", natural.arguments().get("intent").getAsString()));
    }

    @Test
    void partnerCommandIncludesOpenFileContextWhenAvailable() {
        AssistantCommand.OpenFileContext openFile = new AssistantCommand.OpenFileContext(
                "src/test/java/pages/LoginPage.java",
                "class LoginPage { void login() {} }",
                "driver.findElement(By.id(\"email\")).sendKeys(username);");
        AssistantCommand.Invocation slash = AssistantCommand.fromPrompt(
                "/partner Convert this login step to SHAFT",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true,
                openFile);
        AssistantCommand.Invocation natural = AssistantCommand.fromPrompt(
                "plan coding partner work for mobile login",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true,
                openFile);

        assertAll(
                () -> assertEquals("src/test/java/pages/LoginPage.java",
                        slash.arguments().get("currentSourcePath").getAsString()),
                () -> assertEquals("driver.findElement(By.id(\"email\")).sendKeys(username);",
                        slash.arguments().get("selectedText").getAsString()),
                () -> assertEquals("Mobile", natural.arguments().get("backend").getAsString()),
                () -> assertEquals("src/test/java/pages/LoginPage.java",
                        natural.arguments().get("currentSourcePath").getAsString()));
    }

    @Test
    void naturalIntentRoutesObviousMcpFeatureRequests() {
        AssistantCommand.Invocation projectUpgrade = command("preview shaft upgrade .", "C:/work/project");
        AssistantCommand.Invocation projectCreate = command("create SHAFT project demo-web");

        assertAll(
                () -> assertEquals("shaft_guide_search", command("search SHAFT docs locators").toolName()),
                () -> assertEquals("locators", command("search SHAFT docs locators").arguments().get("query").getAsString()),
                () -> assertEquals("test_automation_scenarios", command("find automation scenarios checkout").toolName()),
                () -> assertEquals("checkout", command("find automation scenarios checkout").arguments().get("intent").getAsString()),
                () -> assertEquals("test_code_guardrails_check",
                        command("check generated Java code driver.element().click(locator);").toolName()),
                () -> assertEquals("driver.element().click(locator);",
                        command("check generated Java code driver.element().click(locator);").arguments().get("code").getAsString()),
                () -> assertTrue(projectCreate.isLocal()),
                () -> assertTrue(projectCreate.localResponse().contains("writes files")),
                () -> assertTrue(projectCreate.localResponse().contains("demo-web")),
                () -> assertEquals("shaft_project_upgrade", projectUpgrade.toolName()),
                () -> assertTrue(projectUpgrade.arguments().get("dryRun").getAsBoolean()),
                () -> assertFalse(projectUpgrade.arguments().get("approve").getAsBoolean()),
                () -> assertEquals(".", projectUpgrade.arguments().get("projectRoot").getAsString()),
                () -> assertEquals("mobile_record_start", command("start mobile recording").toolName()),
                () -> assertEquals("capture_start", command("start a browser recording").toolName()),
                () -> assertEquals("doctor_analyze_failed_allure",
                        command("run doctor on target/allure-results").toolName()),
                () -> assertEquals("mobile_recording_code_blocks",
                        command("generate mobile code from recordings/mobile.json").toolName()),
                () -> assertEquals("mobile_toolchain_status",
                        command("check my Android mobile toolchain").toolName()),
                () -> assertEquals("Android",
                        command("check my Android mobile toolchain").arguments().get("platformName").getAsString()),
                () -> assertEquals("mobile_get_accessibility_tree",
                        command("inspect the current mobile screen").toolName()),
                () -> assertEquals(8000,
                        command("inspect the current mobile screen").arguments().get("maxCharacters").getAsInt()),
                () -> assertEquals("mobile_get_contexts",
                        command("show mobile contexts").toolName()),
                () -> assertEquals("mobile_take_screenshot",
                        command("take a mobile screenshot target/shaft-mobile/home.png").toolName()),
                () -> assertFalse(command("take a mobile screenshot target/shaft-mobile/home.png")
                        .arguments().get("includeBase64").getAsBoolean()),
                () -> assertTrue(command("what commands can I use for mobile recording?").isLocal()),
                () -> assertTrue(command("what commands can I use for mobile recording?")
                        .localResponse().contains("/record-mobile")));
    }

    @Test
    void naturalTriagePhrasingsRouteToAutoDiscoveringDoctor() {
        // No path in the text -> empty allureResultPaths -> the server auto-discovers the newest
        // allure-results directory or single-file AllureReport.html in the workspace.
        for (String phrase : List.of(
                "diagnose my last run",
                "diagnose the latest run",
                "analyze the latest report",
                "why did my test fail?",
                "why did my tests fail")) {
            AssistantCommand.Invocation triage = command(phrase, "C:/work/project");
            assertAll(phrase,
                    () -> assertEquals("doctor_analyze_failed_allure", triage.toolName()),
                    () -> assertEquals(0, triage.arguments().get("allureResultPaths").getAsJsonArray().size()),
                    () -> assertEquals("C:/work/project", triage.arguments().get("repositoryRoot").getAsString()));
        }
    }

    @Test
    void requiresShaftProjectGatesOnlyMutatingOrShaftReportingSpecificTools() {
        AssistantCommand.Invocation projectUpgrade = command("preview shaft upgrade .", "C:/work/project");
        AssistantCommand.Invocation verify = command("/verify mvn -q test-compile");
        AssistantCommand.Invocation doctor = command("run doctor on target/allure-results");
        AssistantCommand.Invocation guideSearch = command("search SHAFT docs locators");
        AssistantCommand.Invocation codeGuardrails = command("check generated Java code driver.element().click(locator);");
        AssistantCommand.Invocation localAgentRun = command("Plan a resilient login test");
        AssistantCommand.Invocation local = command("/help");

        assertAll(
                () -> assertTrue(AssistantCommand.requiresShaftProject(projectUpgrade),
                        "shaft_project_upgrade mutates the project and must be gated"),
                () -> assertTrue(AssistantCommand.requiresShaftProject(verify),
                        "verify_run_focused runs a real Maven build and must be gated"),
                () -> assertTrue(AssistantCommand.requiresShaftProject(doctor),
                        "doctor_analyze_failed_allure only makes sense against SHAFT Allure evidence"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(guideSearch),
                        "shaft_guide_search is read-only and useful while adopting SHAFT"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(codeGuardrails),
                        "test_code_guardrails_check is read-only"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(localAgentRun),
                        "autobot_local_agent_run is not SHAFT-project-specific"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(local),
                        "local invocations never dispatch a tool"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(null)));
    }

    @Test
    void shaftProjectRequiredNudgeIsALocalOnboardingMessage() {
        AssistantCommand.Invocation nudge = AssistantCommand.shaftProjectRequiredNudge("verify_run_focused");

        assertAll(
                () -> assertTrue(nudge.isLocal()),
                () -> assertTrue(nudge.localResponse().contains("verify_run_focused")),
                () -> assertTrue(nudge.localResponse().contains("doesn't look like a SHAFT project")),
                () -> assertTrue(nudge.localResponse().contains("Create SHAFT Project")));
    }

    @Test
    void naturalRecordingCommandsMapToSafeRecorderWorkflow() {
        AssistantCommand.Invocation simpleStart = command("start recording");
        AssistantCommand.Invocation articleStart = command("start a recording");
        AssistantCommand.Invocation start = command("start recording https://duckduckgo.com/");
        AssistantCommand.Invocation stop = command("stop");
        AssistantCommand.Invocation review = command("review recording recordings/demo.json");
        AssistantCommand.Invocation defaultReview = command("review recording");

        assertAll(
                () -> assertEquals("", simpleStart.arguments().get("targetUrl").getAsString(),
                        "no target parsed from bare 'start recording' should stay blank so capture_start "
                                + "resolves it to an empty browser downstream, not a hardcoded default site"),
                () -> assertEquals("", articleStart.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", start.toolName()),
                () -> assertEquals("https://duckduckgo.com/",
                        start.arguments().get("targetUrl").getAsString()),
                () -> assertTrue(start.arguments().get("outputPath").getAsString()
                        .startsWith(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH_PREFIX)),
                () -> assertTrue(start.arguments().get("outputPath").getAsString().endsWith(".json")),
                () -> assertFalse(start.arguments().get("headless").getAsBoolean()),
                () -> assertEquals("capture_stop", stop.toolName()),
                () -> assertFalse(stop.arguments().get("discard").getAsBoolean()),
                () -> assertEquals("capture_code_blocks", review.toolName()),
                () -> assertEquals("recordings/demo.json", review.arguments().get("sessionPath").getAsString()),
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_REVIEW_DIRECTORY,
                        review.arguments().get("outputDirectory").getAsString()),
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH,
                        defaultReview.arguments().get("sessionPath").getAsString()));
    }

    @Test
    void recordScenarioPhrasesRouteDeterministicallyToBlankCaptureStart() {
        // Issue #3673: common recording phrases ("record a scenario", "start recording", ...)
        // must resolve deterministically to capture_start with no target rather than falling
        // through to the LLM agent. No target parsed means an empty/blank-page browser opens.
        AssistantCommand.Invocation recordAScenario = command("record a scenario");
        AssistantCommand.Invocation recordANewScenario = command("record a new scenario");
        AssistantCommand.Invocation recordScenario = command("record scenario");
        AssistantCommand.Invocation recordNewScenario = command("record new scenario");
        // Regression guard: the narrow new phrases must not disturb the existing mobile-recording
        // routing for phrasing that mentions "record" alongside mobile keywords.
        AssistantCommand.Invocation mobileRecording = command("record my actions on the Android emulator");

        assertAll(
                () -> assertEquals("capture_start", recordAScenario.toolName()),
                () -> assertEquals("", recordAScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", recordANewScenario.toolName()),
                () -> assertEquals("", recordANewScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", recordScenario.toolName()),
                () -> assertEquals("", recordScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", recordNewScenario.toolName()),
                () -> assertEquals("", recordNewScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("mobile_record_start", mobileRecording.toolName(),
                        "the narrow record-a-scenario predicate must not collide with mobile recording intent"));
    }

    @Test
    void proseScenarioDescriptionRoutesToAgentWithFullOrchestrationGuidanceInsteadOfBareRecording() {
        // Issue #3692 item 3: the onboarding "Record a sample flow" quick action sends this prose
        // scenario description as a chat prompt (see ShaftAssistantPanel's empty-state chip), not a
        // bare "start recording"/"record a scenario" trigger. Before this fix, isBrowserRecordingIntent
        // matched "web flow" in the description and deterministically short-circuited it into a bare
        // capture_start with no target, silently dropping "add one assertion" and "generate a
        // reviewed test." It must instead reach the agent with the full record -> act -> stop ->
        // codegen -> self-heal guidance.
        AssistantCommand.Invocation invocation = command(
                "Record a sample web flow on a practice page, add one assertion, and generate a reviewed test.");

        assertAll(
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertFalse(invocation.arguments().has("targetUrl"),
                        "must not deterministically dispatch a bare capture_start"));

        String prompt = invocation.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertTrue(prompt.contains("This is a code-generation request. Before returning Java:"), prompt),
                () -> assertTrue(prompt.contains("start a fresh session with capture_start_codegen"), prompt),
                () -> assertTrue(prompt.contains("capture_generate_replay"), prompt),
                () -> assertTrue(prompt.contains("healer_run_failed_test"), prompt),
                () -> assertTrue(prompt.contains("Page Object Model"), prompt),
                () -> assertTrue(prompt.contains(
                        "Record a sample web flow on a practice page, add one assertion, and generate a reviewed test."),
                        prompt));
    }

    @Test
    void bareRecordingTriggersStillRouteDeterministicallyDespiteScenarioDescriptionCarveOut() {
        // Regression guard for the isScenarioDescriptionIntent carve-out added alongside the test
        // above: it must not swallow the narrow, exact-match deterministic triggers (issue #3673)
        // that other tests already pin down.
        AssistantCommand.Invocation startRecording = command("start recording");
        AssistantCommand.Invocation recordAScenario = command("record a scenario");

        assertAll(
                () -> assertEquals("capture_start", startRecording.toolName()),
                () -> assertEquals("capture_start", recordAScenario.toolName()));
    }

    @Test
    void naturalRecorderCommandsCanDiscardAnActiveCaptureSession() {
        AssistantCommand.Invocation discard = command("discard recording");
        AssistantCommand.Invocation discardAlias = command("discard capture session");

        assertAll(
                () -> assertEquals("capture_stop", discard.toolName()),
                () -> assertEquals("capture_stop", discardAlias.toolName()),
                () -> assertTrue(discard.arguments().get("discard").getAsBoolean()),
                () -> assertTrue(discardAlias.arguments().get("discard").getAsBoolean()));
    }

    @Test
    void reRecordStopsAndRestartsCaptureInASequence() {
        AssistantCommand.Invocation reRecord = command("re-record");
        AssistantCommand.Invocation reRecordWithUrl = command("re-record https://duckduckgo.com/");

        assertAll(
                () -> assertTrue(reRecord.isSequence()),
                () -> assertEquals("capture_stop", reRecord.toolCalls().get(0).toolName()),
                () -> assertEquals("capture_start", reRecord.toolCalls().get(1).toolName()),
                () -> assertTrue(reRecord.toolCalls().get(0).arguments().get("discard").getAsBoolean()),
                () -> assertEquals("capture_stop", reRecordWithUrl.toolCalls().get(0).toolName()),
                () -> assertEquals("capture_start", reRecordWithUrl.toolCalls().get(1).toolName()),
                () -> assertTrue(reRecordWithUrl.toolCalls().get(0).arguments().get("discard").getAsBoolean()),
                () -> assertEquals("https://duckduckgo.com/",
                        reRecordWithUrl.toolCalls().get(1).arguments().get("targetUrl").getAsString()),
                () -> assertTrue(reRecordWithUrl.toolCalls().get(1).arguments().get("outputPath")
                        .getAsString().startsWith(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH_PREFIX)),
                () -> assertEquals("chrome", reRecordWithUrl.toolCalls().get(1).arguments().get("browser").getAsString().toLowerCase(Locale.ROOT)),
                () -> assertFalse(reRecordWithUrl.toolCalls().get(1).arguments().get("headless").getAsBoolean()));
    }

    @Test
    void captureReviewUsesIgnoredReviewDirectoryAndApprovalCreatesAgentRequest() {
        assertTrue(AssistantCommand.isCaptureApproval("okay"));
        assertTrue(AssistantCommand.isCaptureApproval("generate"));
        assertTrue(AssistantCommand.isCaptureApproval("apply it"));
        assertFalse(AssistantCommand.isCaptureApproval("yes"));
        assertFalse(AssistantCommand.isCaptureApproval("go ahead"));
        assertFalse(AssistantCommand.isCaptureApproval("looks good"));
        assertFalse(AssistantCommand.isCaptureApproval("create files"));
        assertFalse(AssistantCommand.isCaptureApproval("generate a login test"));

        var review = AssistantCommand.captureCodeReview("recordings/demo.json");
        assertAll(
                () -> assertEquals("recordings/demo.json", review.get("sessionPath").getAsString()),
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_REVIEW_DIRECTORY,
                        review.get("outputDirectory").getAsString()),
                () -> assertTrue(review.get("overwrite").getAsBoolean()));

        AssistantCommand.Invocation apply = AssistantCommand.approvedCaptureIntegration(
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "C:/work/project",
                "",
                "```java\nclass Generated {}\n```",
                "{\"codeBlocks\":[]}");

        assertAll(
                () -> assertEquals("autobot_local_agent_run", apply.toolName()),
                () -> assertEquals("AGENT", apply.arguments().get("mode").getAsString()),
                () -> assertTrue(apply.arguments().get("allowSourceMutation").getAsBoolean()),
                () -> assertTrue(apply.arguments().get("prompt").getAsString().contains("Page Object Model")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString().contains("Do not start a new recording")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString()
                        .contains("Preserve the recorded browser journey")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString()
                        .contains("open the first result")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString()
                        .contains("assert the final page title")));
    }

    @Test
    void triageAndFixTestFailureRouteToDoctorWithWorkingDirectory() {
        String workingDirectory = "C:/work/project";
        AssistantCommand.Invocation triage = command("/triage", workingDirectory);
        AssistantCommand.Invocation fix = command("/fixTestFailure", workingDirectory);

        assertEquals("doctor_analyze_failed_allure", triage.toolName());
        assertEquals("doctor_analyze_failed_allure", fix.toolName());
        // Empty allureResultPaths tells the MCP server to auto-discover the most recent results.
        assertTrue(triage.arguments().get("allureResultPaths").getAsJsonArray().isEmpty());
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

    @Test
    void verboseStreamingDeliversEachStubbedProcessLineIncrementallyBeforeExit() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        FakeProcess process = FakeProcess.withDelayedStdoutLines(
                List.of("line one", "line two", "line three"), 40);
        List<String> delivered = new CopyOnWriteArrayList<>();

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, delivered::add, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(3, delivered.size()),
                () -> assertEquals("line one", delivered.get(0)),
                () -> assertEquals("line two", delivered.get(1)),
                () -> assertEquals("line three", delivered.get(2)));
    }

    @Test
    void structuredClaudeStreamProducesHumanReadableProgressAndParsesTerminalUsage() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CLAUDE_CODE", "ASK", ".", "", false);
        String toolUseEvent = """
                {"type":"assistant","message":{"content":[{"type":"tool_use","id":"tool_1","name":"shaft_guide_search","input":{}}]}}""";
        String assistantTextEvent = """
                {"type":"assistant","message":{"content":[{"type":"text","text":"Looking into the failure now."}]}}""";
        String terminalEvent = """
                {"type":"result","subtype":"success","result":"The failure was a stale locator.","usage":{"input_tokens":123,"output_tokens":45}}""";
        FakeProcess process = FakeProcess.withStdout(
                String.join("\n", toolUseEvent, assistantTextEvent, terminalEvent) + "\n", 0);
        List<String> delivered = new CopyOnWriteArrayList<>();

        // requireCommandAvailable=false: this exercises the real claude default command shape without
        // depending on the claude CLI actually being installed on the test machine's PATH.
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, delivered::add, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        AssistantLocalAgentRunner.TokenUsage usage = AssistantLocalAgentRunner.parseTokenUsage(result.output());

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(result.output().contains("The failure was a stale locator.")),
                () -> assertFalse(delivered.isEmpty()),
                () -> assertTrue(delivered.stream().anyMatch(line -> line.contains("shaft_guide_search")),
                        "Expected a human-readable tool_use line: " + delivered),
                () -> assertTrue(delivered.stream().anyMatch(line -> line.contains("Looking into the failure now.")),
                        "Expected the assistant text to be delivered: " + delivered),
                () -> assertTrue(delivered.stream().noneMatch(line -> line.startsWith("{")),
                        "Consumer lines should be human-readable, not raw JSON: " + delivered),
                () -> assertEquals(123, usage.inputTokens()),
                () -> assertEquals(45, usage.outputTokens()),
                () -> assertFalse(usage.estimated()));
    }

    @Test
    void plainProseThroughCustomCommandNeverInventsUsageMetadata() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        String prose = "Here is the explanation.\nIt spans several lines.\nNo structured data anywhere.";
        FakeProcess process = FakeProcess.withStdout(prose + "\n", 0);

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(prose, result.output()),
                () -> assertNull(AssistantLocalAgentRunner.parseTokenUsage(result.output())));
    }

    @Test
    void bufferedCopilotInvocationSendsOneNoticeThenForwardsRawOutputAsIs() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "COPILOT_CLI", "ASK", ".", "", false);
        String bufferedResponse = "The full Copilot answer, delivered only once the process exits.";
        FakeProcess process = FakeProcess.withStdout(bufferedResponse + "\n", 0);
        List<String> delivered = new CopyOnWriteArrayList<>();

        // requireCommandAvailable=false: this exercises the real "copilot ask" default command shape
        // without depending on the copilot CLI actually being installed on the test machine's PATH.
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, delivered::add, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(2, delivered.size(),
                        "Expected the one-time notice followed by the raw line: " + delivered),
                () -> assertTrue(delivered.get(0).toLowerCase(Locale.ROOT).contains("raw output"),
                        "Notice should explain the raw as-is stream: " + delivered.get(0)),
                () -> assertEquals(bufferedResponse, delivered.get(1),
                        "Verbose mode must share the CLI's output as-is instead of swallowing it"),
                () -> assertTrue(result.output().contains(bufferedResponse)));
    }

    @Test
    void stripTrailingUsageMetadataRemovesUsageLineButKeepsProseAndAnswerText() {
        String withUsage = "The answer text.\n\n{\"usage\":{\"input_tokens\":123,\"output_tokens\":45}}";
        String plainProse = "Just a plain multi-line\nanswer with no usage metadata.";

        assertAll(
                () -> assertEquals("The answer text.", AssistantLocalAgentRunner.stripTrailingUsageMetadata(withUsage)),
                () -> assertEquals(plainProse, AssistantLocalAgentRunner.stripTrailingUsageMetadata(plainProse)));
    }

    @Test
    void tokenUsageParsingPrefersReportedFieldsAndFallsBackToNullForEstimate() {
        String claudeJsonResult = """
                {"type":"result","subtype":"success","result":"Done","usage":{"input_tokens":123,"output_tokens":45}}
                """.strip();

        AssistantLocalAgentRunner.TokenUsage reported = AssistantLocalAgentRunner.parseTokenUsage(claudeJsonResult);
        AssistantLocalAgentRunner.TokenUsage absent = AssistantLocalAgentRunner.parseTokenUsage("Plain markdown response with no usage metadata.");

        assertAll(
                () -> assertEquals(123, reported.inputTokens()),
                () -> assertEquals(45, reported.outputTokens()),
                () -> assertFalse(reported.estimated()),
                () -> assertEquals(168, reported.totalTokens()),
                () -> assertNull(absent));
    }

    @Test
    void modelSelectorPopulatesFromStubbedCliWithNoHardcodedModelNames() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CLAUDE_CODE", "ASK", ".", "", false);
        FakeProcess process = FakeProcess.withStdout("stub-model-alpha\nstub-model-beta\n", 0);

        List<String> models = AssistantLocalAgentRunner.listModels(
                invocation.arguments(), (command, workingDirectory, environment) -> process);

        assertAll(
                () -> assertEquals(2, models.size()),
                () -> assertTrue(models.contains("stub-model-alpha")),
                () -> assertTrue(models.contains("stub-model-beta")),
                () -> assertFalse(models.contains("claude-opus-4-8")),
                () -> assertFalse(models.contains("claude-sonnet-5")),
                () -> assertFalse(models.contains("gpt-4.1")));
    }

    @Test
    void autoCompactIssuesCompactCommandBeforeUserPromptInvocation() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CLAUDE_CODE", "ASK", ".", "stub-agent --print", false);
        List<List<String>> launchedCommands = new CopyOnWriteArrayList<>();
        AtomicInteger launchCount = new AtomicInteger();

        AssistantLocalAgentRunner.ProcessLauncher launcher = (command, workingDirectory, environment) -> {
            launchedCommands.add(command);
            int index = launchCount.getAndIncrement();
            return index == 0
                    ? FakeProcess.withStdout("Compaction complete.", 0)
                    : FakeProcess.withStdout("Explanation of the failure.", 0);
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, true, output -> { }, launcher);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(2, launchedCommands.size()),
                () -> assertTrue(launchedCommands.get(0).contains("/compact"),
                        "Compact command should be issued first: " + launchedCommands),
                () -> assertEquals(AssistantLocalAgentRunner.commandFor(invocation.arguments()),
                        launchedCommands.get(1)));
    }

    @Test
    void autoCompactSkipsGracefullyWithoutFailingRequestWhenCliDoesNotSupportCompaction() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "COPILOT_CLI", "ASK", ".", "stub-agent ask", false);
        List<List<String>> launchedCommands = new CopyOnWriteArrayList<>();
        AssistantLocalAgentRunner.ProcessLauncher launcher = (command, workingDirectory, environment) -> {
            launchedCommands.add(command);
            return FakeProcess.withStdout("ask response", 0);
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, true, output -> { }, launcher);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(1, launchedCommands.size(), "Unsupported CLI should skip compaction, not fail: "
                        + launchedCommands),
                () -> assertEquals(AssistantLocalAgentRunner.commandFor(invocation.arguments()),
                        launchedCommands.get(0)));
    }

    private static AssistantCommand.Invocation command(String prompt) {
        return command(prompt, ".");
    }

    private static AssistantCommand.Invocation command(String prompt, String workingDirectory) {
        return AssistantCommand.fromPrompt(prompt, "CODEX", "ASK", workingDirectory, "", false);
    }

    /**
     * Minimal stub {@link Process} for driving {@link AssistantLocalAgentRunner} without spawning a
     * real CLI executable.
     */
    private static final class FakeProcess extends Process {
        private final InputStream stdout;
        private final InputStream stderr;
        private final OutputStream stdin = new ByteArrayOutputStream();
        private final int exitValue;

        private FakeProcess(InputStream stdout, InputStream stderr, int exitValue) {
            this.stdout = stdout;
            this.stderr = stderr;
            this.exitValue = exitValue;
        }

        static FakeProcess withStdout(String stdout, int exitValue) {
            return new FakeProcess(
                    new ByteArrayInputStream(stdout.getBytes(StandardCharsets.UTF_8)),
                    new ByteArrayInputStream(new byte[0]),
                    exitValue);
        }

        static FakeProcess withDelayedStdoutLines(List<String> lines, long delayMillisPerLine) {
            return new FakeProcess(new DelayedLinesInputStream(lines, delayMillisPerLine),
                    new ByteArrayInputStream(new byte[0]), 0);
        }

        @Override
        public OutputStream getOutputStream() {
            return stdin;
        }

        @Override
        public InputStream getInputStream() {
            return stdout;
        }

        @Override
        public InputStream getErrorStream() {
            return stderr;
        }

        @Override
        public int waitFor() {
            return exitValue;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) {
            return true;
        }

        @Override
        public int exitValue() {
            return exitValue;
        }

        @Override
        public void destroy() {
            // No underlying OS process to terminate.
        }

        @Override
        public Process destroyForcibly() {
            return this;
        }

        @Override
        public boolean isAlive() {
            return false;
        }
    }

    /**
     * Feeds newline-delimited lines to a reader with a delay between each line, simulating a slow
     * streaming CLI process for verbose-output tests.
     */
    private static final class DelayedLinesInputStream extends InputStream {
        private final List<byte[]> chunks = new ArrayList<>();
        private final long delayMillisPerLine;
        private int chunkIndex;
        private int positionInChunk;
        private boolean delayedForCurrentChunk;

        DelayedLinesInputStream(List<String> lines, long delayMillisPerLine) {
            this.delayMillisPerLine = delayMillisPerLine;
            for (String line : lines) {
                chunks.add((line + "\n").getBytes(StandardCharsets.UTF_8));
            }
        }

        @Override
        public synchronized int read() throws IOException {
            while (chunkIndex < chunks.size() && positionInChunk >= chunks.get(chunkIndex).length) {
                chunkIndex++;
                positionInChunk = 0;
                delayedForCurrentChunk = false;
            }
            if (chunkIndex >= chunks.size()) {
                return -1;
            }
            if (!delayedForCurrentChunk) {
                delayedForCurrentChunk = true;
                try {
                    Thread.sleep(delayMillisPerLine);
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while simulating delayed output", exception);
                }
            }
            return chunks.get(chunkIndex)[positionInChunk++] & 0xFF;
        }
    }
}
