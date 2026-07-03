package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertAll;
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
                Generated Java code must use SHAFT syntax only: SHAFT.GUI.WebDriver, driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator.
                Never generate raw Selenium code such as WebDriver, ChromeDriver, driver.get(...), driver.findElement(...), or direct WebElement actions.
                For repeated search-result anchors, scope the locator to the first result container; for DuckDuckGo use `(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']`.

                open duckduckgo and search for SHAFT Engine""",
                duckDuckGo.arguments().get("prompt").getAsString());
        assertEquals("use shaft-mcp to open mobile app", alreadyExplicit.arguments().get("prompt").getAsString());
        assertEquals("""
                If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.
                For WebDriver browser tasks, call driver_initialize before browser_* tools; do not use Playwright unless requested.
                Generated Java code must use SHAFT syntax only: SHAFT.GUI.WebDriver, driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator.
                Never generate raw Selenium code such as WebDriver, ChromeDriver, driver.get(...), driver.findElement(...), or direct WebElement actions.
                For repeated search-result anchors, scope the locator to the first result container; for DuckDuckGo use `(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']`.

                Explain the current test failure""", plain.arguments().get("prompt").getAsString());
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
                () -> assertTrue(generatedPrompt.contains("ask the user to confirm the exact target URL"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Do not infer canonical URLs"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Never substitute a different URL, domain, or example page"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Open a real browser session"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("call driver_initialize and browser_open_intent"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("element_type, element_click, or natural_act"),
                        generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Do not publish unverified locators"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Call test_code_guardrails_check"), generatedPrompt),
                () -> assertTrue(generatedPrompt.contains("Return only SHAFT-syntax Java"), generatedPrompt),
                () -> assertTrue(explicitPrompt.contains("Call shaft_guide_search"), explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("ask the user to confirm the exact target URL"),
                        explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("browser_open_intent"), explicitPrompt),
                () -> assertTrue(explicitPrompt.contains("Call test_code_guardrails_check"), explicitPrompt));
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
                () -> assertTrue(cloudPrompt.contains("Open a real browser session"), cloudPrompt),
                () -> assertTrue(cloudPrompt.contains("Do not publish unverified locators"), cloudPrompt));
    }

    @Test
    void localAgentLoginCodeGenerationRequiresUrlConfirmationAndLiveLocatorProof() {
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
                () -> assertTrue(prompt.contains("Open a real browser session"), prompt),
                () -> assertTrue(prompt.contains("browser_open_intent"), prompt),
                () -> assertTrue(prompt.contains("element_type, element_click, or natural_act"), prompt),
                () -> assertTrue(prompt.contains("Do not publish unverified locators"), prompt),
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

        assertEquals(List.of("codex", "exec", "--sandbox", "read-only", "-"),
                AssistantLocalAgentRunner.commandFor(codexAsk.arguments()));
        assertEquals(List.of(
                "codex", "exec",
                "--sandbox", "workspace-write",
                "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                "-"),
                AssistantLocalAgentRunner.commandFor(codexAgent.arguments()));
        assertEquals(List.of(
                "codex", "exec",
                "--sandbox", "read-only",
                "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                "-"),
                AssistantLocalAgentRunner.commandFor(codexAgentNoSource.arguments()));
        assertEquals(List.of("claude", "--print", "--permission-mode", "plan"),
                AssistantLocalAgentRunner.commandFor(claudePlan.arguments()));
        assertEquals(List.of("custom-agent", "--safe"), AssistantLocalAgentRunner.commandFor(custom.arguments()));
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
        assertEquals("capture_code_blocks", command("/generatetest recordings/capture-session.json").toolName());
        assertEquals("playwright_recording_code_blocks", command("/generatetest recordings/playwright-session.json").toolName());
        assertEquals("capture_code_blocks", command("/codegen recordings/capture-session.json").toolName());
        assertEquals("mobile_recording_code_blocks", command("/codegen mobile recordings/mobile-session.json").toolName());
        assertEquals("mobile_inspector_record_prepare",
                command("/record-mobile inspector Android recordings/inspector.json").toolName());
    }

    @Test
    void commandRegistryShowsOnlyTestedValidCommandFamilies() {
        List<AssistantCommand.CommandHint> hints = AssistantCommand.commandHints();
        String tooltip = AssistantCommand.commandTooltip();

        assertAll(
                () -> assertEquals(List.of(
                                "/codegen",
                                "/record-web",
                                "/record-mobile",
                                "/doctor",
                                "/guide",
                                "/guardrails",
                                "/browser",
                                "/mobile",
                                "/project"),
                        hints.stream().map(AssistantCommand.CommandHint::canonical).toList()),
                () -> assertTrue(tooltip.contains("/codegen")),
                () -> assertTrue(tooltip.contains("/record-web")),
                () -> assertTrue(tooltip.contains("/record-mobile")),
                () -> assertTrue(tooltip.contains("/doctor")),
                () -> assertTrue(tooltip.contains("/guide")),
                () -> assertTrue(tooltip.contains("/guardrails")),
                () -> assertTrue(tooltip.contains("/browser")),
                () -> assertTrue(tooltip.contains("/mobile")),
                () -> assertTrue(tooltip.contains("/project")),
                () -> assertFalse(tooltip.contains("/commands")),
                () -> assertFalse(tooltip.contains("/assistant")),
                () -> assertFalse(tooltip.contains("/mobile-record")),
                () -> assertFalse(tooltip.contains("Slash commands:")));
    }

    @Test
    void commandHelpShowsOnlyTestedValidCommandFamilies() {
        AssistantCommand.Invocation help = command("/commands");

        assertAll(
                () -> assertTrue(help.isLocal()),
                () -> assertTrue(help.localResponse().contains("/codegen")),
                () -> assertTrue(help.localResponse().contains("/record-web")),
                () -> assertTrue(help.localResponse().contains("/record-mobile")),
                () -> assertTrue(help.localResponse().contains("/doctor")),
                () -> assertTrue(help.localResponse().contains("/guide")),
                () -> assertTrue(help.localResponse().contains("/guardrails")),
                () -> assertTrue(help.localResponse().contains("/browser")),
                () -> assertTrue(help.localResponse().contains("/mobile")),
                () -> assertTrue(help.localResponse().contains("/project")),
                () -> assertFalse(help.localResponse().contains("/commands")),
                () -> assertFalse(help.localResponse().contains("/assistant")));
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
    void naturalRecordingCommandsMapToSafeRecorderWorkflow() {
        AssistantCommand.Invocation simpleStart = command("start recording");
        AssistantCommand.Invocation articleStart = command("start a recording");
        AssistantCommand.Invocation start = command("start recording https://duckduckgo.com/");
        AssistantCommand.Invocation stop = command("stop");

        assertAll(
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_TARGET_URL,
                        simpleStart.arguments().get("targetUrl").getAsString()),
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_TARGET_URL,
                        articleStart.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", start.toolName()),
                () -> assertEquals("https://duckduckgo.com/",
                        start.arguments().get("targetUrl").getAsString()),
                () -> assertTrue(start.arguments().get("outputPath").getAsString()
                        .startsWith(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH_PREFIX)),
                () -> assertTrue(start.arguments().get("outputPath").getAsString().endsWith(".json")),
                () -> assertFalse(start.arguments().get("headless").getAsBoolean()),
                () -> assertEquals("capture_stop", stop.toolName()),
                () -> assertFalse(stop.arguments().get("discard").getAsBoolean()));
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
