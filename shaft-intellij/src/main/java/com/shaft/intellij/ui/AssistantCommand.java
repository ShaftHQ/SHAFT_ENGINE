package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.intellij.mcp.ShaftCommandLine;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Builds SHAFT Assistant MCP invocations from prompt text.
 */
final class AssistantCommand {
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    private static final int DEFAULT_BROWSER_MAX_CHARACTERS = 12_000;
    private static final int DEFAULT_MOBILE_MAX_CHARACTERS = 8_000;
    private static final int DEFAULT_BROWSER_MAX_ELEMENTS = 10;
    static final String DEFAULT_CAPTURE_TARGET_URL = "https://duckduckgo.com/";
    static final String DEFAULT_CAPTURE_RECORDING_PATH = "recordings/intellij-capture.json";
    static final String DEFAULT_CAPTURE_RECORDING_PATH_PREFIX = "recordings/intellij-capture-";
    static final String DEFAULT_PLAYWRIGHT_RECORDING_PATH = "recordings/playwright-recording.json";
    static final String DEFAULT_CAPTURE_REVIEW_DIRECTORY = "target/shaft-capture/assistant-review";
    private static final String DEFAULT_MOBILE_RECORDING_PATH = "recordings/mobile-recording.json";
    private static final String DEFAULT_MOBILE_INSPECTOR_RECORDING_PATH = "recordings/mobile-inspector.json";
    private static final String DEFAULT_DOCTOR_REPORT_PATH = "target/shaft-doctor/doctor-report.json";
    private static final String AGENT_SOURCE_GUARD =
            """
                    Source edits are not enabled for this session.
                    Return source-change suggestions in concise markdown and fenced code blocks only.
                    Do not apply patches, write files, or run filesystem commands that would mutate source.
                    The user should decide whether and how to apply any suggested edits.
                    """.stripIndent().trim();
    private static final String AGENT_SOURCE_APPROVAL =
            """
                    Source edits are approved for this session.
                    You may apply patches, write files, and run filesystem commands needed to make the requested source changes.
                    """.stripIndent().trim();
    private static final String SHAFT_MCP_USAGE_HINT =
            """
                    If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.
                    For WebDriver browser tasks, call driver_initialize before browser_* tools; do not use Playwright unless requested.
                    Generated Java code must use SHAFT syntax only: SHAFT.GUI.WebDriver, driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator.
                    Never generate SHAFT.GUI.Locator.xpath(...); use smart locators, the SHAFT locator builder, or By.xpath only as a last fallback.
                    Never generate raw Selenium code such as WebDriver, ChromeDriver, driver.get(...), driver.findElement(...), or direct WebElement actions.
                    For repeated search-result anchors, scope the locator to the first result container; for DuckDuckGo use `(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']`.
                    """.stripIndent().trim();
    private static final String SHAFT_CODEGEN_TOOL_GUIDANCE =
            """
                    This is a code-generation request. Before returning Java:
                    - Call shaft_guide_search for the relevant SHAFT guide examples and cite the official guide URLs.
                    - Call test_automation_scenarios for broad test/page-object design to learn the matching SHAFT coding pattern.
                    - Inspect existing project code and call shaft_coding_partner_plan before creating tests, page objects, locators, or actions.
                    - Reuse existing tests, page objects, locator fields, and action methods first.
                    - If required actions or locators are missing, ask before starting live browser work; otherwise return review-only Java and mark locator assumptions as unverified.
                    - If the user names a site, product, or environment without an explicit URL, ask the user to confirm the exact target URL before navigating or writing code. Do not infer canonical URLs.
                    - For any requested site/action, preserve the user's requested target. Never substitute a different URL, domain, or example page in code or screenshot evidence.
                    - If the user asks for code only, a draft, or says not to run/open a browser, do not call live browser tools.
                    - Do not publish locators as verified unless a live browser session actually checked them.
                    - Never generate SHAFT.GUI.Locator.xpath(...); prefer smart locators and the locator builder, then By.xpath only as a last fallback.
                    - Use native Playwright locators only as a last fallback in SHAFT Playwright-specific code.
                    - Call test_code_guardrails_check on the final Java snippet.
                    - Do not print full repository files or broad file dumps. Cite inspected files by path and include only the generated or changed code needed for the answer.
                    - Put every code snippet in fenced code blocks with the correct language.
                    - Return only SHAFT-syntax Java; never return raw Selenium code.
                    """.stripIndent().trim();
    private static final String SHAFT_LIVE_CODEGEN_TOOL_GUIDANCE =
            """
                    Live browser verification was explicitly requested:
                    - Open a real browser session; call driver_initialize and browser_open_intent with the confirmed targetUrl and userIntent to inspect the live page and locator candidates.
                    - Verify each selected locator by performing the actual action with shaft-mcp element_type, element_click, or natural_act before returning it.
                    """.stripIndent().trim();
    private static final CommandDefinition COMMAND_HELP = new CommandDefinition("/commands", "Show command help",
            List.of("/help", "/mcp-help", "/shaft-help"),
            "/commands", (command, rest, workingDirectory) -> Invocation.local(commandHelp(false)));
    private static final CommandDefinition CODEGEN_COMMAND = new CommandDefinition("/codegen",
            "Generate code from recordings",
            List.of(),
            "/codegen recordings/intellij-capture.json",
            (command, rest, workingDirectory) -> generateTest(rest));
    private static final CommandDefinition PARTNER_COMMAND = new CommandDefinition("/partner",
            "Plan coding partner work",
            List.of("/coding-partner", "/reuse"),
            "/partner log in then verify account menu",
            (command, rest, workingDirectory) -> Invocation.tool(
                    "shaft_coding_partner_plan",
                    codingPartnerPlan(rest, workingDirectory)));
    private static final CommandDefinition RECORD_WEB_COMMAND = new CommandDefinition("/record-web",
            "Record web actions",
            List.of(),
            "/record-web https://example.com",
            (command, rest, workingDirectory) -> record(rest));
    private static final CommandDefinition RECORD_MOBILE_COMMAND = new CommandDefinition("/record-mobile",
            "Record mobile actions",
            List.of(),
            "/record-mobile inspector Android recordings/inspector.json",
            (command, rest, workingDirectory) -> mobileRecord(rest));
    private static final CommandDefinition DOCTOR_COMMAND = new CommandDefinition("/doctor",
            "Analyze failures and recommend fixes",
            List.of(),
            "/doctor target/allure-results",
            (command, rest, workingDirectory) -> doctor(rest, workingDirectory));
    private static final CommandDefinition GUIDE_COMMAND = new CommandDefinition("/guide", "Search the SHAFT guide",
            List.of("/docs", "/manual"),
            "/guide locators", (command, rest, workingDirectory) -> Invocation.tool("shaft_guide_search", guide(rest)));
    private static final CommandDefinition BROWSER_COMMAND = new CommandDefinition("/browser", "Inspect browser state",
            List.of("/web", "/browse", "/page", "/inspect", "/locator"),
            "/browser open https://example.com sign in", (command, rest, workingDirectory) -> browser(rest));
    private static final CommandDefinition MOBILE_COMMAND = new CommandDefinition("/mobile", "Inspect mobile state",
            List.of("/appium", "/device", "/phone", "/emulator"),
            "/mobile native Android Pixel_6", (command, rest, workingDirectory) -> mobile(rest));
    private static final CommandDefinition GUARDRAILS_COMMAND = new CommandDefinition("/guardrails",
            "Check generated Java code",
            List.of("/checkcode"),
            "/guardrails driver.element().click(locator);",
            (command, rest, workingDirectory) -> Invocation.tool("test_code_guardrails_check", guardrails(rest)));
    private static final CommandDefinition UPGRADE_COMMAND = new CommandDefinition("/upgrade",
            "Have the agent upgrade this project to the latest SHAFT",
            List.of(),
            "/upgrade .", (command, rest, workingDirectory) -> upgradeScript(rest));
    private static final CommandDefinition PROJECT_COMMAND = new CommandDefinition("/project",
            "Create or upgrade SHAFT projects",
            List.of("/newshaft"),
            "/project upgrade .", AssistantCommand::project);
    private static final CommandDefinition VERIFY_COMMAND = new CommandDefinition("/verify",
            "Run a focused verification command",
            List.of("/validate", "/check"),
            "/verify mvn -q test-compile",
            (command, rest, workingDirectory) -> verify(rest, workingDirectory));
    private static final CommandDefinition SKILLS_COMMAND = new CommandDefinition("/skills",
            "List SHAFT authoring skills",
            List.of("/skill"),
            "/skills",
            (command, rest, workingDirectory) -> Invocation.local(skillsHelp()));
    // Default composer shows only the core entry points; the rest are revealed by Expert mode.
    private static final List<CommandDefinition> CORE_COMMANDS = List.of(
            RECORD_WEB_COMMAND,
            RECORD_MOBILE_COMMAND,
            CODEGEN_COMMAND,
            DOCTOR_COMMAND,
            UPGRADE_COMMAND);
    private static final List<CommandDefinition> EXPERT_COMMANDS = List.of(
            PARTNER_COMMAND,
            GUIDE_COMMAND,
            GUARDRAILS_COMMAND,
            BROWSER_COMMAND,
            MOBILE_COMMAND,
            PROJECT_COMMAND,
            VERIFY_COMMAND,
            SKILLS_COMMAND);
    private static final List<CommandDefinition> VISIBLE_COMMANDS = concat(CORE_COMMANDS, EXPERT_COMMANDS);
    private static final List<CommandDefinition> COMMANDS = List.of(
            COMMAND_HELP,
            CODEGEN_COMMAND,
            PARTNER_COMMAND,
            RECORD_WEB_COMMAND,
            RECORD_MOBILE_COMMAND,
            DOCTOR_COMMAND,
            GUIDE_COMMAND,
            new CommandDefinition("/scenarios", "Find automation scenarios",
                    List.of("/scenario", "/ideas"),
                    "/scenarios checkout", (command, rest, workingDirectory) -> Invocation.tool("test_automation_scenarios", scenarios(rest))),
            BROWSER_COMMAND,
            new CommandDefinition("/record", "Record browser actions",
                    List.of("/rec", "/capture"),
                    "/record playwright", (command, rest, workingDirectory) -> record(rest)),
            MOBILE_COMMAND,
            new CommandDefinition("/mobile-record", "Record mobile actions",
                    List.of("/app-record", "/inspector-record"),
                    "/mobile-record inspector Android recordings/inspector.json",
                    (command, rest, workingDirectory) -> mobileRecord("/inspector-record".equals(command) ? "inspector " + rest : rest)),
            new CommandDefinition("/mobile-codegen", "Generate mobile replay snippets",
                    List.of("/app-codegen"),
                    "/mobile-codegen recordings/mobile.json", (command, rest, workingDirectory) -> mobileCodegen(rest)),
            new CommandDefinition("/mobile-replay", "Replay mobile recordings",
                    List.of("/app-replay"),
                    "/mobile-replay recordings/mobile.json", (command, rest, workingDirectory) -> mobileReplay(rest)),
            new CommandDefinition("/allure", "Analyze failed Allure evidence",
                    List.of("/triage", "/fixTestFailure", "/failure", "/fix"),
                    "/allure target/allure-results", (command, rest, workingDirectory) -> doctor(rest, workingDirectory)),
            GUARDRAILS_COMMAND,
            new CommandDefinition("/assistant", "List assistant clients",
                    List.of("/agent", "/ask", "/plan", "/clients", "/client"),
                    "/assistant", (command, rest, workingDirectory) -> Invocation.tool("autobot_local_agent_clients", new JsonObject())),
            UPGRADE_COMMAND,
            PROJECT_COMMAND,
            new CommandDefinition("/mcp", "Run an explicit MCP tool",
                    List.of("/tool", "/call"),
                    "/mcp browser_get_title {}", (command, rest, workingDirectory) -> rawMcp(rest)),
            new CommandDefinition("/generate", "Generate code from recordings",
                    List.of("/gen", "/generateTest"),
                    "/generate recordings/intellij-capture.json",
                    (command, rest, workingDirectory) -> generateTest(rest)),
            VERIFY_COMMAND,
            SKILLS_COMMAND);
    private static final List<NaturalIntent> NATURAL_INTENTS = List.of(
            new NaturalIntent(AssistantCommand::isCommandHelpIntent,
                    (text, workingDirectory) -> Invocation.local(commandHelp(false))),
            new NaturalIntent(AssistantCommand::isCodingPartnerIntent,
                    (text, workingDirectory) -> Invocation.tool(
                            "shaft_coding_partner_plan",
                            codingPartnerPlan(naturalCodingPartnerIntent(text), workingDirectory))),
            new NaturalIntent(AssistantCommand::isGuideSearchIntent,
                    (text, workingDirectory) -> Invocation.tool("shaft_guide_search", guide(naturalQuery(text)))),
            new NaturalIntent(AssistantCommand::isScenarioSearchIntent,
                    (text, workingDirectory) -> Invocation.tool("test_automation_scenarios", scenarios(naturalQuery(text)))),
            new NaturalIntent(AssistantCommand::isGuardrailsCheckIntent,
                    (text, workingDirectory) -> Invocation.tool("test_code_guardrails_check", guardrails(naturalCode(text)))),
            new NaturalIntent(AssistantCommand::isProjectCreateIntent,
                    (text, workingDirectory) -> projectCreateReview(naturalProjectName(text))),
            new NaturalIntent(AssistantCommand::isProjectUpgradeIntent,
                    (text, workingDirectory) -> Invocation.tool("shaft_project_upgrade", projectUpgrade(naturalProjectRoot(text, workingDirectory)))),
            new NaturalIntent(AssistantCommand::isBrowserControlIntent,
                    (text, workingDirectory) -> browser(text)),
            new NaturalIntent(AssistantCommand::isBrowserRecordingIntent,
                    (text, workingDirectory) -> record(text)),
            new NaturalIntent(AssistantCommand::isMobileControlIntent,
                    (text, workingDirectory) -> mobile(naturalMobileCommand(text))),
            new NaturalIntent(AssistantCommand::isMobileRecordingStartIntent,
                    (text, workingDirectory) -> mobileRecord("start " + firstJsonLikePath(text))),
            new NaturalIntent(AssistantCommand::isMobileRecordingStopIntent,
                    (text, workingDirectory) -> mobileRecord("stop")),
            new NaturalIntent(AssistantCommand::isMobileCodegenIntent,
                    (text, workingDirectory) -> mobileCodegen(firstJsonLikePath(text))),
            new NaturalIntent(AssistantCommand::isDoctorIntent,
                    (text, workingDirectory) -> {
                        String path = firstPathLike(text);
                        return doctor(path.isBlank() ? "" : path, workingDirectory);
                    }));
    private static final List<CommandHint> CORE_HINTS =
            CORE_COMMANDS.stream().map(CommandDefinition::hint).toList();
    private static final List<CommandHint> EXPERT_HINTS =
            EXPERT_COMMANDS.stream().map(CommandDefinition::hint).toList();
    private static final List<CommandHint> ALL_HINTS = concat(CORE_HINTS, EXPERT_HINTS);

    private AssistantCommand() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns every command hint. Used for looking up metadata about a command the user already typed,
     * regardless of Expert mode.
     */
    static List<CommandHint> commandHints() {
        return ALL_HINTS;
    }

    /**
     * Returns the command hints that should be visible in menus/autocomplete: core-only by default, or every
     * command when Expert mode is enabled.
     */
    static List<CommandHint> commandHints(boolean expertEnabled) {
        return expertEnabled ? ALL_HINTS : CORE_HINTS;
    }

    static String commandTooltip(boolean expertEnabled) {
        StringBuilder tooltip = new StringBuilder("<html><b>SHAFT commands</b>");
        for (CommandHint hint : commandHints(expertEnabled)) {
            tooltip.append("<br><code>")
                    .append(hint.canonical())
                    .append("</code> - ")
                    .append(hint.summary());
            if (!hint.synonyms().isEmpty()) {
                tooltip.append("<br>&nbsp;&nbsp;Aliases: ")
                        .append(String.join(", ", hint.synonyms()));
            }
            tooltip.append("<br>&nbsp;&nbsp;Example: ")
                    .append(hint.example());
        }
        return tooltip.append("</html>").toString();
    }

    static String commandHelp(boolean expertEnabled) {
        StringBuilder help = new StringBuilder("SHAFT Assistant commands:");
        for (CommandDefinition definition : (expertEnabled ? VISIBLE_COMMANDS : CORE_COMMANDS)) {
            help.append("\n\n**")
                    .append(definition.canonical())
                    .append("** - ")
                    .append(definition.summary());
            if (!definition.aliases().isEmpty()) {
                help.append("\n  Aliases: ")
                        .append(String.join(", ", definition.aliases()));
            }
            help.append("\n  Example:\n```text\n")
                    .append(definition.example())
                    .append("\n```");
        }
        if (!expertEnabled) {
            help.append("\n\n_Enable Expert mode in SHAFT settings to reveal advanced commands: ")
                    .append(EXPERT_COMMANDS.stream().map(CommandDefinition::canonical)
                            .collect(java.util.stream.Collectors.joining(", ")))
                    .append("._");
        }
        return help.toString();
    }

    private static <T> List<T> concat(List<T> first, List<T> second) {
        List<T> combined = new ArrayList<>(first);
        combined.addAll(second);
        return List.copyOf(combined);
    }

    private static Invocation verify(String rest, String workingDirectory) {
        JsonObject arguments = new JsonObject();
        String repositoryPath = text(workingDirectory);
        arguments.addProperty("repositoryRoot", repositoryPath.isBlank() ? "." : repositoryPath);
        JsonArray commandArray = new JsonArray();
        String trimmed = text(rest);
        List<String> tokens = trimmed.isBlank()
                ? List.of("mvn", "-q", "test-compile")
                : ShaftCommandLine.parse(trimmed);
        if (tokens.isEmpty()) {
            tokens = List.of("mvn", "-q", "test-compile");
        }
        for (String token : tokens) {
            commandArray.add(token);
        }
        arguments.add("command", commandArray);
        arguments.addProperty("networkValidationApproved", false);
        return Invocation.tool("verify_run_focused", arguments);
    }

    private static String skillsHelp() {
        return """
                SHAFT authoring skills (use `$<name>` in a chat with an installed agent):

                **$writing-shaft-tests** - write, review, or repair SHAFT Java tests, page objects, API/mobile tests.
                **$choosing-shaft-locators** - pick stable SHAFT smart/semantic locators over brittle XPath.
                **$recording-shaft-tests-with-mcp** - record web/mobile flows and turn them into reusable SHAFT code.
                **$analyzing-shaft-failures** - triage failed Allure/trace evidence and route repairs safely.
                **$verifying-and-applying-shaft-changes** - review, diff, apply, guardrail, and verify generated code.

                Each skill grounds generated code in the official SHAFT guide and the coding-partner plan first.""";
    }

    record OpenFileContext(String path, String text, String selectedText) {
        OpenFileContext(String path, String text) {
            this(path, text, "");
        }

        static OpenFileContext empty() {
            return new OpenFileContext("", "", "");
        }

        boolean present() {
            return path != null && !path.isBlank() && text != null && !text.isBlank();
        }

        String selectedOrEmpty() {
            return selectedText == null ? "" : selectedText.trim();
        }
    }

    static Invocation fromPrompt(
            String prompt,
            String client,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation) {
        return fromPrompt(prompt, Selection.fromClient(client), mode, workingDirectory, customCommand, allowSourceMutation,
                OpenFileContext.empty());
    }

    static Invocation fromPrompt(
            String prompt,
            String client,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation,
            OpenFileContext openFileContext) {
        return fromPrompt(prompt, Selection.fromClient(client), mode, workingDirectory, customCommand, allowSourceMutation,
                openFileContext, "");
    }

    static Invocation fromPrompt(
            String prompt,
            Selection selection,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation) {
        return fromPrompt(prompt, selection, mode, workingDirectory, customCommand, allowSourceMutation,
                OpenFileContext.empty());
    }

    static Invocation fromPrompt(
            String prompt,
            Selection selection,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation,
            OpenFileContext openFileContext) {
        return fromPrompt(prompt, selection, mode, workingDirectory, customCommand, allowSourceMutation,
                openFileContext, "");
    }

    static Invocation fromPrompt(
            String prompt,
            Selection selection,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation,
            OpenFileContext openFileContext,
            String conversationContext) {
        String text = prompt == null ? "" : prompt.trim();
        if (text.isEmpty()) {
            return Invocation.local("Enter a prompt or slash command.");
        }
        String liveCodegen = liveCodegenSlashPrompt(text);
        boolean liveCodegenRequest = !liveCodegen.isBlank();
        boolean upgradeAgentRequest = false;
        if (!liveCodegen.isBlank()) {
            text = liveCodegen;
        } else if (isUpgradeSlashCommand(text)) {
            // /upgrade is an action, not a recipe: the local agent performs the upgrade itself
            // (with source-edit approval), repairs what breaks, and reports what it changed —
            // instead of pasting a command back for the user to run manually (issue #3426 B6).
            Invocation blocked = upgradeAgentGate(selection, mode, allowSourceMutation, afterFirstWord(text.trim()));
            if (blocked != null) {
                return blocked;
            }
            text = upgradeAgentPrompt(afterFirstWord(text.trim()));
            upgradeAgentRequest = true;
        } else if (text.startsWith("/")) {
            return slash(text, workingDirectory, openFileContext);
        }
        boolean codeGenerationRequest = !upgradeAgentRequest && isCodeGenerationRequest(text);
        if (!liveCodegenRequest && !upgradeAgentRequest) {
            if (isCodingPartnerIntent(text)) {
                return Invocation.tool(
                        "shaft_coding_partner_plan",
                        codingPartnerPlan(naturalCodingPartnerIntent(text), workingDirectory, openFileContext));
            }
            if (isGuardrailsCheckIntent(text)) {
                return Invocation.tool("test_code_guardrails_check", guardrails(naturalCode(text)));
            }
        }
        if (!liveCodegenRequest && !upgradeAgentRequest && !codeGenerationRequest) {
            Invocation recording = recording(text);
            if (recording != null) {
                return recording;
            }
            Invocation directIntent = directIntent(text, workingDirectory);
            if (directIntent != null) {
                return directIntent;
            }
        }
        if (selection.cloud()) {
            return cloud(text, selection, mode, workingDirectory, openFileContext, conversationContext);
        }
        if (!"CLI".equals(selection.runtime())) {
            return Invocation.local("SHAFT is configured for " + selection.displayName()
                    + ". Configure SHAFT MCP for that runtime, then send this prompt from its native chat panel.");
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", selection.client());
        arguments.addProperty("mode", mode);
        arguments.addProperty("model", selection.localModel());
        arguments.addProperty("effort", selection.effort());
        // Codex receives the effort as a real CLI config flag in AssistantLocalAgentRunner; the
        // other CLIs have no effort flag, so their prompt carries the preference instead.
        String localPrompt = localAgentPrompt(text, mode, allowSourceMutation, openFileContext,
                conversationContext);
        arguments.addProperty("prompt", "CODEX".equals(selection.family())
                ? localPrompt
                : withEffortHint(localPrompt, selection.effort()));
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.add("command", commandArray(customCommand));
        arguments.add("environment", new JsonObject());
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", "AGENT".equals(mode) && allowSourceMutation);
        return Invocation.tool("autobot_local_agent_run", arguments);
    }

    private static Invocation cloud(
            String text,
            Selection selection,
            String mode,
            String workingDirectory,
            OpenFileContext openFileContext,
            String conversationContext) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("provider", selection.cloudProvider());
        arguments.addProperty("model", selection.cloudModel());
        arguments.addProperty("mode", mode);
        arguments.addProperty("prompt",
                withEffortHint(cloudPrompt(text, mode, openFileContext, conversationContext), selection.effort()));
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", false);
        return Invocation.tool("autobot_provider_chat", arguments);
    }

    private static String cloudPrompt(String text, String mode, OpenFileContext openFileContext,
                                      String conversationContext) {
        if (!isCodeGenerationRequest(text)) {
            return withConversationContext(text, conversationContext);
        }
        return withConversationContext(SHAFT_MCP_USAGE_HINT
                + "\n" + codeGenerationGuidance(text)
                + "\n" + codeRequestScope(Selection.normalize(mode, "ASK"), openFileContext)
                + "\n\n" + text, conversationContext);
    }

    private static Invocation slash(String text, String workingDirectory) {
        return slash(text, workingDirectory, OpenFileContext.empty());
    }

    private static Invocation slash(String text, String workingDirectory, OpenFileContext openFileContext) {
        String[] parts = text.split("\\s+", 2);
        String command = parts[0].toLowerCase(Locale.ROOT);
        String rest = parts.length == 2 ? parts[1].trim() : "";
        if (PARTNER_COMMAND.matches(command)) {
            return Invocation.tool("shaft_coding_partner_plan", codingPartnerPlan(rest, workingDirectory, openFileContext));
        }
        for (CommandDefinition definition : COMMANDS) {
            if (definition.matches(command)) {
                return definition.invoke(command, rest, workingDirectory);
            }
        }
        return Invocation.local("Unknown command. Use /commands for available SHAFT Assistant commands.");
    }

    private static String liveCodegenSlashPrompt(String text) {
        String trimmed = text(text);
        if (!"/codegen".equals(firstWord(trimmed).toLowerCase(Locale.ROOT))) {
            return "";
        }
        String rest = afterFirstWord(trimmed);
        if (rest.isBlank() || !firstJsonLikePath(rest).isBlank()) {
            return "";
        }
        return "Generate SHAFT Java code for this browser flow: " + rest;
    }

    private static JsonObject guide(String query) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("query", query.isBlank() ? "page objects locators" : query);
        arguments.addProperty("maxResults", 5);
        return arguments;
    }

    private static JsonObject scenarios(String intent) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("area", "all");
        arguments.addProperty("intent", intent);
        arguments.addProperty("maxResults", 20);
        return arguments;
    }

    private static JsonObject codingPartnerPlan(String intent, String workingDirectory) {
        return codingPartnerPlan(intent, workingDirectory, OpenFileContext.empty());
    }

    private static JsonObject codingPartnerPlan(String intent, String workingDirectory, OpenFileContext openFileContext) {
        JsonObject arguments = new JsonObject();
        String repositoryPath = text(workingDirectory);
        String normalizedIntent = text(intent);
        arguments.addProperty("repositoryPath", repositoryPath.isBlank() ? "." : repositoryPath);
        arguments.addProperty("intent", normalizedIntent);
        arguments.addProperty("backend", codingPartnerBackend(normalizedIntent, openFileContext));
        arguments.addProperty("currentSourcePath", openFileContext == null ? "" : text(openFileContext.path()));
        arguments.addProperty("selectedText", openFileContext == null ? "" : openFileContext.selectedOrEmpty());
        arguments.add("artifactPaths", new JsonArray());
        arguments.addProperty("maxResults", 10);
        return arguments;
    }

    private static String codingPartnerBackend(String intent, OpenFileContext openFileContext) {
        String combined = (text(intent) + " "
                + (openFileContext == null ? "" : text(openFileContext.text()) + " " + openFileContext.selectedOrEmpty()))
                .toLowerCase(Locale.ROOT);
        if (containsAny(combined, "mobile", "appium", "android", "ios")) {
            return "Mobile";
        }
        return combined.contains("playwright") ? "Playwright" : "WebDriver";
    }

    private static JsonObject inspect(String rest) {
        String[] targetAndIntent = splitTargetUrlAndIntent(rest);
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", targetAndIntent[0]);
        arguments.addProperty("userIntent", targetAndIntent[1]);
        arguments.addProperty("maxCharacters", DEFAULT_BROWSER_MAX_CHARACTERS);
        arguments.addProperty("maxElements", DEFAULT_BROWSER_MAX_ELEMENTS);
        return arguments;
    }

    private static JsonObject guardrails(String code) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("language", "java");
        arguments.addProperty("code", code);
        return arguments;
    }

    private static Invocation browser(String rest) {
        String trimmed = text(rest);
        if (trimmed.isBlank()) {
            return Invocation.local("Use `/browser open <url> [intent]`, `/browser dom`, `/browser screenshot`, or `/browser quit`.");
        }
        String action = firstWord(trimmed).toLowerCase(Locale.ROOT);
        String remainder = afterFirstWord(trimmed);
        if (commandIs(action, "playwright", "pw")) {
            return playwrightBrowser(remainder);
        }
        if (commandIs(action, "inspect", "locator", "locate")) {
            return webdriverOpen(remainder);
        }
        if (commandIs(action, "open", "navigate", "visit", "go") || isUrl(firstWord(trimmed))) {
            String target = commandIs(action, "open", "navigate", "visit", "go") ? remainder : trimmed;
            if (firstWord(target).isBlank()) {
                return Invocation.local("Provide a URL for WebDriver browser navigation.");
            }
            return webdriverOpen(target);
        }
        if (commandIs(action, "screenshot", "shot", "capture")) {
            return Invocation.tool("browser_take_screenshot",
                    screenshot(firstTokenOrDefault(remainder, "target/shaft-browser/screenshot.png")));
        }
        if (commandIs(action, "dom", "source", "tree")) {
            JsonObject arguments = new JsonObject();
            arguments.addProperty("maxCharacters", DEFAULT_BROWSER_MAX_CHARACTERS);
            return Invocation.tool("browser_get_page_dom", arguments);
        }
        if (commandIs(action, "title")) {
            return Invocation.tool("browser_get_title", new JsonObject());
        }
        if (commandIs(action, "url")) {
            return Invocation.tool("browser_get_current_url", new JsonObject());
        }
        if (commandIs(action, "refresh", "reload")) {
            return Invocation.tool("browser_refresh", new JsonObject());
        }
        if (commandIs(action, "back")) {
            return Invocation.tool("browser_navigate_back", new JsonObject());
        }
        if (commandIs(action, "forward")) {
            return Invocation.tool("browser_navigate_forward", new JsonObject());
        }
        if (commandIs(action, "maximize")) {
            return Invocation.tool("browser_maximize_window", new JsonObject());
        }
        if (commandIs(action, "fullscreen")) {
            return Invocation.tool("browser_fullscreen_window", new JsonObject());
        }
        if (commandIs(action, "quit", "close", "stop")) {
            return Invocation.tool("driver_quit", new JsonObject());
        }
        return webdriverOpen(trimmed);
    }

    private static Invocation playwrightBrowser(String rest) {
        String trimmed = text(rest);
        if (trimmed.isBlank()) {
            return Invocation.local("Use `/browser playwright open <url>`, `/browser playwright dom`, or `/browser playwright screenshot`.");
        }
        String action = firstWord(trimmed).toLowerCase(Locale.ROOT);
        String remainder = afterFirstWord(trimmed);
        if (commandIs(action, "open", "navigate", "visit", "go") || isUrl(firstWord(trimmed))) {
            String targetUrl = commandIs(action, "open", "navigate", "visit", "go") ? firstTokenOrDefault(remainder, "") : firstWord(trimmed);
            if (targetUrl.isBlank()) {
                return Invocation.local("Provide a URL for Playwright browser navigation.");
            }
            return Invocation.sequence(List.of(
                    new ToolCall("playwright_initialize", playwrightInitialize()),
                    new ToolCall("playwright_browser_navigate", targetUrl("targetUrl", targetUrl))));
        }
        if (commandIs(action, "screenshot", "shot", "capture")) {
            return Invocation.tool("playwright_browser_take_screenshot",
                    screenshot(firstTokenOrDefault(remainder, "target/shaft-playwright/screenshot.png")));
        }
        if (commandIs(action, "dom", "source", "tree")) {
            JsonObject arguments = new JsonObject();
            arguments.addProperty("maxCharacters", DEFAULT_BROWSER_MAX_CHARACTERS);
            return Invocation.tool("playwright_browser_get_page_dom", arguments);
        }
        if (commandIs(action, "title")) {
            return Invocation.tool("playwright_browser_get_title", new JsonObject());
        }
        if (commandIs(action, "url")) {
            return Invocation.tool("playwright_browser_get_current_url", new JsonObject());
        }
        if (commandIs(action, "quit", "close", "stop")) {
            return Invocation.tool("playwright_quit", new JsonObject());
        }
        return Invocation.local("Unknown Playwright browser command. Use `/browser playwright open <url>`.");
    }

    private static Invocation webdriverOpen(String rest) {
        return Invocation.sequence(List.of(
                new ToolCall("driver_initialize", driverInitialize()),
                new ToolCall("browser_open_intent", inspect(rest))));
    }

    private static JsonObject driverInitialize() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetBrowser", "CHROME");
        return arguments;
    }

    private static JsonObject playwrightInitialize() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("browser", "chrome");
        arguments.addProperty("headless", false);
        return arguments;
    }

    private static Invocation mobile(String rest) {
        String trimmed = text(rest);
        String action = firstWord(trimmed).toLowerCase(Locale.ROOT);
        String remainder = afterFirstWord(trimmed);
        if (trimmed.isBlank() || commandIs(action, "status", "doctor", "toolchain", "check")) {
            return Invocation.tool("mobile_toolchain_status", platform(commandIs(action, "status", "doctor", "toolchain", "check") ? remainder : trimmed));
        }
        if (commandIs(action, "native", "app", "init", "initialize")) {
            return Invocation.tool("mobile_initialize_native", mobileNative(remainder));
        }
        if (commandIs(action, "web", "emulation", "emulate")) {
            return Invocation.tool("mobile_initialize_web_emulation", mobileWeb(remainder));
        }
        if (commandIs(action, "tree", "source", "dom", "accessibility", "inspect")) {
            JsonObject arguments = new JsonObject();
            arguments.addProperty("maxCharacters", DEFAULT_MOBILE_MAX_CHARACTERS);
            return Invocation.tool("mobile_get_accessibility_tree", arguments);
        }
        if (commandIs(action, "contexts", "context")) {
            String contextName = firstWord(remainder);
            if (!contextName.isBlank() && !commandIs(contextName.toLowerCase(Locale.ROOT), "list", "show", "get")) {
                JsonObject arguments = new JsonObject();
                arguments.addProperty("contextName", contextName);
                return Invocation.tool("mobile_switch_context", arguments);
            }
            JsonObject arguments = new JsonObject();
            arguments.addProperty("maxCharacters", DEFAULT_MOBILE_MAX_CHARACTERS);
            return Invocation.tool("mobile_get_contexts", arguments);
        }
        if (commandIs(action, "switch")) {
            String contextName = firstTokenOrDefault(remainder, "");
            if (contextName.isBlank()) {
                return Invocation.local("Provide an Appium context name, for example `/mobile switch WEBVIEW_chrome`.");
            }
            JsonObject arguments = new JsonObject();
            arguments.addProperty("contextName", contextName);
            return Invocation.tool("mobile_switch_context", arguments);
        }
        if (commandIs(action, "screenshot", "shot", "capture")) {
            return Invocation.tool("mobile_take_screenshot",
                    mobileScreenshot(firstTokenOrDefault(remainder, "target/shaft-mobile/screenshot.png")));
        }
        if (commandIs(action, "record", "recording")) {
            return mobileRecord(remainder);
        }
        if (commandIs(action, "codegen", "generate")) {
            return mobileCodegen(remainder);
        }
        if (commandIs(action, "replay")) {
            return mobileReplay(remainder);
        }
        if (commandIs(action, "quit", "close", "stop")) {
            return Invocation.tool("driver_quit", new JsonObject());
        }
        return Invocation.local("Unknown mobile command. Use `/mobile status`, `/mobile native Android <device>`, `/mobile web <url>`, `/mobile tree`, `/mobile contexts`, `/mobile switch <context>`, `/mobile screenshot <path>`, or `/mobile quit`.");
    }

    private static JsonObject mobileScreenshot(String outputPath) {
        JsonObject arguments = screenshot(outputPath);
        arguments.addProperty("includeBase64", false);
        return arguments;
    }

    private static JsonObject mobileNative(String rest) {
        String trimmed = text(rest);
        String platformName = platformName(firstWord(trimmed));
        String deviceName = platformName.equalsIgnoreCase(firstWord(trimmed)) ? afterFirstWord(trimmed) : trimmed;
        JsonObject arguments = new JsonObject();
        arguments.addProperty("platformName", platformName);
        arguments.addProperty("deviceName", deviceName);
        arguments.addProperty("appiumServerUrl", "");
        arguments.addProperty("automationName", "");
        arguments.addProperty("platformVersion", "");
        arguments.addProperty("udid", "");
        arguments.addProperty("app", "");
        arguments.addProperty("appPackage", "");
        arguments.addProperty("appActivity", "");
        arguments.addProperty("bundleId", "");
        return arguments;
    }

    private static JsonObject mobileWeb(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", parseLeadingUrl(rest));
        arguments.addProperty("browser", "CHROME");
        arguments.addProperty("deviceName", "Pixel 5");
        arguments.addProperty("width", 0);
        arguments.addProperty("height", 0);
        arguments.addProperty("pixelRatio", 1.0);
        arguments.addProperty("userAgent", "");
        arguments.addProperty("headless", recorderHeadlessPreference());
        return arguments;
    }

    /**
     * Recorder browser visibility shared with the Guided workflow panel's Headless toggle
     * ({@code ShaftSettingsState.Settings#recorderHeadless}). Defaults to a visible browser when
     * the IntelliJ application is unavailable (plain unit tests).
     */
    private static boolean recorderHeadlessPreference() {
        try {
            if (com.intellij.openapi.application.ApplicationManager.getApplication() == null) {
                return false;
            }
            var settings = com.shaft.intellij.settings.ShaftSettingsState.getInstance().getState();
            return settings != null && settings.recorderHeadless;
        } catch (RuntimeException | Error headlessTestEnvironment) {
            return false;
        }
    }

    private static Invocation mobileRecord(String rest) {
        String trimmed = text(rest);
        String action = firstWord(trimmed).toLowerCase(Locale.ROOT);
        String remainder = afterFirstWord(trimmed);
        if (trimmed.isBlank() || commandIs(action, "start", "begin", "record")) {
            return Invocation.tool("mobile_record_start",
                    mobileRecordingStart(firstJsonLikePath(trimmed).isBlank()
                            ? DEFAULT_MOBILE_RECORDING_PATH
                            : firstJsonLikePath(trimmed)));
        }
        if (commandIs(action, "status")) {
            return Invocation.tool("mobile_record_status", new JsonObject());
        }
        if (commandIs(action, "stop", "finish", "end")) {
            return Invocation.tool("mobile_record_stop", discard(remainder));
        }
        if (commandIs(action, "inspector", "inspect", "appium")) {
            return Invocation.tool("mobile_inspector_record_prepare", mobileInspectorRecordPrepare(remainder));
        }
        if (commandIs(action, "inspector-status")) {
            return Invocation.tool("mobile_inspector_record_status", new JsonObject());
        }
        return Invocation.local("Use `/record-mobile start <path>`, `/record-mobile stop`, or `/record-mobile inspector Android <path>`.");
    }

    private static JsonObject mobileRecordingStart(String outputPath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", outputPath);
        arguments.addProperty("mode", "default");
        arguments.addProperty("includeSensitiveValues", false);
        return arguments;
    }

    private static JsonObject mobileInspectorRecordPrepare(String rest) {
        String platformName = platformName(firstWord(rest));
        String outputPath = firstJsonLikePath(rest);
        JsonObject arguments = new JsonObject();
        arguments.addProperty("platformName", platformName);
        arguments.addProperty("outputPath", outputPath.isBlank() ? DEFAULT_MOBILE_INSPECTOR_RECORDING_PATH : outputPath);
        arguments.addProperty("includeSensitiveValues", false);
        arguments.addProperty("app", "");
        arguments.addProperty("appPackage", "");
        arguments.addProperty("appActivity", "");
        arguments.addProperty("bundleId", "");
        arguments.addProperty("udid", "");
        arguments.addProperty("deviceName", "");
        arguments.addProperty("platformVersion", "");
        arguments.addProperty("selectedAndroidAvdName", "");
        arguments.addProperty("androidApiLevel", 0);
        arguments.addProperty("androidDeviceProfile", "pixel_6");
        arguments.addProperty("androidImageTag", "google_apis");
        arguments.addProperty("androidAbi", "x86_64");
        arguments.addProperty("androidRamMb", 2048);
        arguments.addProperty("androidCores", 2);
        arguments.addProperty("provisionAndroidEmulator", false);
        return arguments;
    }

    private static Invocation mobileCodegen(String rest) {
        return Invocation.tool("mobile_recording_code_blocks", mobileRecordingPath(rest));
    }

    private static Invocation mobileReplay(String rest) {
        return Invocation.tool("mobile_replay_recording", mobileRecordingPath(rest));
    }

    private static JsonObject mobileRecordingPath(String rest) {
        JsonObject arguments = new JsonObject();
        String path = firstJsonLikePath(rest);
        arguments.addProperty("recordingPath", path.isBlank() ? DEFAULT_MOBILE_RECORDING_PATH : path);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static Invocation doctor(String rest, String workingDirectory) {
        String trimmed = text(rest);
        boolean playwright = "playwright".equalsIgnoreCase(firstWord(trimmed));
        if (playwright) {
            trimmed = afterFirstWord(trimmed);
        }
        String action = firstWord(trimmed).toLowerCase(Locale.ROOT);
        String remainder = afterFirstWord(trimmed);
        if (commandIs(action, "fix", "suggest", "recommend", "repair")) {
            return Invocation.tool(playwright ? "playwright_doctor_suggest_fix" : "doctor_suggest_fix",
                    doctorSuggestFix(firstTokenOrDefault(remainder, DEFAULT_DOCTOR_REPORT_PATH), workingDirectory));
        }
        if (commandIs(action, "trace", "traces")) {
            return Invocation.tool("doctor_analyze_trace",
                    doctorTrace(firstTokenOrDefault(remainder, "target/shaft-traces"), playwright ? "playwright" : "webdriver"));
        }
        String allurePath = trimmed.isBlank() ? "allure-results" : firstTokenOrDefault(trimmed, "allure-results");
        return Invocation.tool(playwright ? "playwright_doctor_analyze_failed_allure" : "doctor_analyze_failed_allure",
                triage(workingDirectory, allurePath));
    }

    private static JsonObject doctorSuggestFix(String reportPath, String workingDirectory) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("jsonReportPath", reportPath);
        arguments.addProperty("repositoryRoot", workingDirectory == null ? "" : workingDirectory);
        arguments.add("allowedSourcePaths", new JsonArray());
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static JsonObject doctorTrace(String tracePath, String backend) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("tracePath", tracePath);
        arguments.addProperty("backend", backend);
        return arguments;
    }

    private static Invocation project(String command, String rest, String workingDirectory) {
        String action = "/newshaft".equals(command) ? "create"
                : "/upgrade".equals(command) ? "upgrade"
                : firstWord(rest).toLowerCase(Locale.ROOT);
        String remainder = "/project".equals(command) ? afterFirstWord(rest) : rest;
        if (commandIs(action, "upgrade", "migrate")) {
            return Invocation.tool("shaft_project_upgrade",
                    projectUpgrade(firstTokenOrDefault(remainder, workingDirectory == null || workingDirectory.isBlank() ? "." : workingDirectory)));
        }
        return projectCreateReview(firstTokenOrDefault(remainder, "shaft-demo"));
    }

    private static Invocation projectCreateReview(String outputDirectory) {
        String target = outputDirectory == null || outputDirectory.isBlank() ? "shaft-demo" : outputDirectory.trim();
        return Invocation.local("""
                Creating a SHAFT project writes files. Open the Projects workflow, choose Create SHAFT Project, set `outputDirectory` to `%s`, then press Run and confirm.
                """.formatted(target).strip());
    }

    /**
     * Tool names that mutate the project, run real Maven builds, or only make sense against SHAFT's
     * own reporting shape (Allure results, healer/doctor triage). Gated behind
     * {@link ShaftProjectDetector} so they never run unattended in an ordinary non-SHAFT project.
     * Read-only/advisory tools (guide search, coding-partner planning, agent chat/codegen, browser and
     * mobile control) are deliberately not in this set: they are legitimately useful precisely when a
     * user is adopting SHAFT into a project that does not use it yet.
     */
    private static final Set<String> SHAFT_PROJECT_TOOLS = Set.of(
            "shaft_project_upgrade",
            "verify_run_focused",
            "healer_run_failed_test",
            "playwright_healer_run_failed_test",
            "doctor_analyze_failed_allure",
            "playwright_doctor_analyze_failed_allure",
            "doctor_suggest_fix",
            "playwright_doctor_suggest_fix",
            "doctor_analyze_trace");

    /**
     * Returns whether the given invocation would dispatch at least one SHAFT-project-only tool.
     *
     * @param invocation the invocation about to be dispatched
     * @return {@code true} when the invocation is not local and calls a SHAFT-project-only tool
     */
    static boolean requiresShaftProject(Invocation invocation) {
        return invocation != null && !invocation.isLocal()
                && invocation.toolCalls().stream().anyMatch(call -> SHAFT_PROJECT_TOOLS.contains(call.toolName()));
    }

    /**
     * Builds the local chat message shown instead of dispatching a SHAFT-project-only tool in a
     * project that does not depend on SHAFT.
     *
     * @param toolName the tool that was about to run
     * @return a local invocation carrying the onboarding nudge
     */
    static Invocation shaftProjectRequiredNudge(String toolName) {
        return Invocation.local("""
                This doesn't look like a SHAFT project yet, so `%s` can't run here -- it needs SHAFT's setup \
                (a SHAFT dependency in your `pom.xml`/`build.gradle`, Maven layout, Allure results). To get \
                started, open the Projects workflow and choose **Create SHAFT Project**, or ask me to \
                "search SHAFT docs for getting started." Once the build depends on SHAFT, send this again.
                """.formatted(toolName).strip());
    }

    private static Invocation upgradeScript(String projectRoot) {
        return Invocation.local("""
                Run this from the project you want to upgrade:

                ```shell
                %s
                ```
                """.formatted(upgradeScriptCommand(firstTokenOrDefault(projectRoot, "."))).strip());
    }

    private static String upgradeScriptCommand(String projectRoot) {
        String target = pythonSingleQuoted(projectRoot == null || projectRoot.isBlank() ? "." : projectRoot.trim());
        return "python -c \"import runpy,sys,urllib.request as u;p='upgrade_to_modular_shaft.py';"
                + "u.urlretrieve('https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/shaft-upgrader/upgrade_to_modular_shaft.py',p);"
                + "sys.argv=[p,'--project','" + target + "'];runpy.run_path(p,run_name='__main__')\"";
    }

    private static String pythonSingleQuoted(String value) {
        return value.replace("\\", "\\\\").replace("'", "\\'");
    }

    private static boolean isUpgradeSlashCommand(String text) {
        return UPGRADE_COMMAND.matches(firstWord(text(text)).toLowerCase(Locale.ROOT));
    }

    /**
     * Pre-flight for the agent-performed {@code /upgrade}: cloud and non-CLI routes cannot edit
     * local files, so they keep the copyable manual command, and a local CLI route must have Agent
     * mode plus explicit source-edit approval before SHAFT lets an agent rewrite the project's
     * build. Returns {@code null} when the agent run may proceed.
     */
    private static Invocation upgradeAgentGate(
            Selection selection, String mode, boolean allowSourceMutation, String projectRoot) {
        if (selection.cloud() || !"CLI".equals(selection.runtime())) {
            return upgradeScript(projectRoot);
        }
        if (!"AGENT".equals(mode) || !allowSourceMutation) {
            return Invocation.local("""
                    **`/upgrade` changes your `pom.xml` (and possibly imports), so it needs your explicit permission first.**

                    1. Switch the mode selector to **Agent**.
                    2. Tick **Allow source edits**.
                    3. Send `/upgrade` again. The agent will then preview the upgrade, apply it, verify the project still compiles, and report every change it made and why.

                    Prefer to run the upgrade yourself instead? Use this from the project root:

                    ```shell
                    %s
                    ```
                    """.formatted(upgradeScriptCommand(firstTokenOrDefault(projectRoot, "."))).strip());
        }
        return null;
    }

    /**
     * The agent prompt behind {@code /upgrade}: the agent executes the official upgrader itself,
     * repairs or works around anything that breaks, verifies the build, and reports the full story.
     */
    private static String upgradeAgentPrompt(String projectRoot) {
        String root = firstTokenOrDefault(projectRoot, ".");
        return """
                Upgrade the project at `%s` to the latest modular SHAFT Engine now. Perform the upgrade yourself; do not hand me commands to run.

                Follow these steps in order and narrate each one:
                1. Read the project's pom.xml and state the current SHAFT setup (artifact and version), or state clearly that the project does not use SHAFT yet.
                2. Preview safely first: call the shaft-mcp tool `shaft_project_upgrade` with projectRoot "%s", upgradeType "basic", and dryRun true, then summarize the proposed changes in plain language.
                3. Apply the upgrade by running the official upgrader non-interactively from the project root:
                   %s
                   If the script fails, quote its exact error, explain the cause, and either fix the cause and retry or apply the equivalent pom.xml edits yourself.
                4. Verify by running `mvn -B -q test-compile` in the project root. If compilation fails because of the upgrade, repair the affected files using SHAFT syntax and re-verify.
                5. Report: previous version -> new version, every file you touched and why, the verification outcome, and anything you could not complete. Never reply with a bare confirmation like "Done".
                """.formatted(root, root, upgradeApplyCommand(root)).strip();
    }

    /**
     * Non-interactive upgrader command for agent execution: local agent CLIs close stdin
     * immediately, so the script must never wait for an interactive confirmation.
     */
    private static String upgradeApplyCommand(String projectRoot) {
        String target = pythonSingleQuoted(projectRoot == null || projectRoot.isBlank() ? "." : projectRoot.trim());
        return "python -c \"import runpy,sys,urllib.request as u;p='upgrade_to_modular_shaft.py';"
                + "u.urlretrieve('https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/shaft-upgrader/upgrade_to_modular_shaft.py',p);"
                + "sys.argv=[p,'--project','" + target + "','--yes'];runpy.run_path(p,run_name='__main__')\"";
    }

    private static JsonObject projectUpgrade(String projectRoot) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("projectRoot", projectRoot);
        arguments.addProperty("upgradeType", "basic");
        arguments.addProperty("dryRun", true);
        arguments.addProperty("approve", false);
        arguments.addProperty("shaftVersion", "");
        arguments.addProperty("compileCommand", "");
        arguments.addProperty("compileTimeout", 900);
        arguments.addProperty("skipBaselineCompile", false);
        arguments.addProperty("allowAiRepair", false);
        return arguments;
    }

    private static Invocation rawMcp(String rest) {
        String toolName = firstWord(rest);
        if (toolName.isBlank()) {
            return Invocation.local("Use `/mcp <toolName> [json]`.");
        }
        String rawJson = afterFirstWord(rest);
        if (rawJson.isBlank()) {
            return Invocation.tool(toolName, new JsonObject());
        }
        try {
            JsonElement parsed = JsonParser.parseString(rawJson);
            if (!parsed.isJsonObject()) {
                return Invocation.local("Raw MCP arguments must be a JSON object.");
            }
            return Invocation.tool(toolName, parsed.getAsJsonObject());
        } catch (JsonParseException exception) {
            return Invocation.local("Raw MCP arguments are not valid JSON: " + exception.getMessage());
        }
    }

    private static Invocation record(String rest) {
        boolean playwright = rest.toLowerCase(Locale.ROOT).contains("playwright");
        return Invocation.tool(playwright ? "playwright_record_start" : "capture_start",
                playwright ? playwrightRecordStart() : captureStart(rest));
    }

    private static Invocation recording(String text) {
        if (isReRecord(text)) {
            return reRecord(text);
        }
        if (isStartRecording(text)) {
            return record(text.replaceFirst("(?i)^start\\s+(a\\s+)?(web(driver)?\\s+)?(capture|recording|recorder)\\b", "").trim());
        }
        if (isReviewRecording(text)) {
            return reviewRecording(text);
        }
        if (isStopRecording(text)) {
            return text.toLowerCase(Locale.ROOT).contains("playwright")
                    ? Invocation.tool("playwright_record_stop", stopRecording())
                    : Invocation.tool("capture_stop", stopRecording());
        }
        if (isDiscardRecording(text)) {
            return Invocation.tool("capture_stop", stopRecording(true));
        }
        return null;
    }

    private static Invocation reviewRecording(String text) {
        String recordingPath = firstJsonLikePath(text);
        String lower = text.toLowerCase(Locale.ROOT);
        if (lower.contains("playwright") || recordingPath.toLowerCase(Locale.ROOT).contains("playwright")) {
            return Invocation.tool("playwright_recording_code_blocks", playwrightCodeReview(recordingPath));
        }
        return Invocation.tool("capture_code_blocks", captureCodeReview(recordingPath));
    }

    private static Invocation reRecord(String text) {
        boolean playwright = text.toLowerCase(Locale.ROOT).contains("playwright");
        return Invocation.sequence(List.of(
                new ToolCall(playwright ? "playwright_record_stop" : "capture_stop", stopRecording(true)),
                new ToolCall(playwright ? "playwright_record_start" : "capture_start",
                        playwright ? playwrightRecordStart() : captureStart(text))
        ));
    }

    private static Invocation directIntent(String text, String workingDirectory) {
        for (NaturalIntent intent : NATURAL_INTENTS) {
            if (intent.matches(text)) {
                return intent.invoke(text, workingDirectory);
            }
        }
        return null;
    }

    private static boolean isCommandHelpIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.contains("command")
                && (normalized.startsWith("what ")
                || normalized.startsWith("which ")
                || normalized.startsWith("show ")
                || normalized.startsWith("list ")
                || normalized.contains(" can i use"));
    }

    private static boolean isCodingPartnerIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return (normalized.startsWith("plan coding partner work for ")
                || normalized.startsWith("plan partner work for ")
                || normalized.startsWith("coding partner plan for ")
                || normalized.startsWith("partner plan for ")
                || normalized.startsWith("find reuse for "))
                && !naturalCodingPartnerIntent(text).isBlank();
    }

    private static String naturalCodingPartnerIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        String[] prefixes = {
                "plan coding partner work for ",
                "plan partner work for ",
                "coding partner plan for ",
                "partner plan for ",
                "find reuse for "
        };
        for (String prefix : prefixes) {
            if (normalized.startsWith(prefix)) {
                return normalized.substring(prefix.length()).trim();
            }
        }
        return text(text);
    }

    private static boolean isGuideSearchIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return (normalized.startsWith("search shaft docs ")
                || normalized.startsWith("search the shaft docs ")
                || normalized.startsWith("search shaft guide ")
                || normalized.startsWith("search the shaft guide ")
                || normalized.startsWith("find shaft docs ")
                || normalized.startsWith("find shaft guide ")
                || normalized.startsWith("guide me on ")
                || normalized.startsWith("docs for "))
                && !naturalQuery(text).isBlank();
    }

    private static boolean isScenarioSearchIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return (normalized.startsWith("find scenarios ")
                || normalized.startsWith("find automation scenarios ")
                || normalized.startsWith("show scenarios ")
                || normalized.startsWith("automation scenarios for ")
                || normalized.startsWith("scenario ideas for "))
                && !naturalQuery(text).isBlank();
    }

    private static boolean isGuardrailsCheckIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return (normalized.startsWith("check generated java code ")
                || normalized.startsWith("check this java code ")
                || normalized.startsWith("run guardrails ")
                || normalized.startsWith("guardrails check "))
                && !naturalCode(text).isBlank();
    }

    private static boolean isProjectCreateIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.startsWith("create shaft project ")
                || normalized.startsWith("create a shaft project ")
                || normalized.startsWith("new shaft project ")
                || normalized.startsWith("scaffold shaft project ");
    }

    private static boolean isProjectUpgradeIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.startsWith("preview shaft upgrade")
                || normalized.startsWith("preview upgrade")
                || normalized.startsWith("dry run shaft upgrade")
                || normalized.startsWith("upgrade shaft project")
                || normalized.startsWith("upgrade this shaft project");
    }

    private static boolean containsAny(String text, String... needles) {
        if (text == null || needles == null) {
            return false;
        }
        for (String needle : needles) {
            if (needle != null && !needle.isBlank() && text.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isBrowserControlIntent(String text) {
        String lower = text == null ? "" : text.toLowerCase(Locale.ROOT);
        if (isBrowserRecordingIntent(lower) || isCommandHelpIntent(lower) || isCodeOnlyRequest(lower)) {
            return false;
        }
        boolean mentionsBrowserAction = containsAny(lower,
                "open ", "navigate", "visit", "refresh", "reload", "back", "forward",
                "maximize", "fullscreen", "quit", "close browser", "screenshot", "dom", "page source",
                "title", "current url", "inspect", "locator");
        boolean mentionsWebTarget = containsAny(lower, "http://", "https://", "file:");
        return mentionsBrowserAction && mentionsWebTarget;
    }

    private static boolean isBrowserRecordingIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("start browser recording")
                || normalized.equals("start a browser recording")
                || normalized.equals("start webdriver recording")
                || normalized.equals("start a webdriver recording")
                || normalized.startsWith("start browser recording ")
                || normalized.startsWith("start a browser recording ")
                || normalized.startsWith("start webdriver recording ")
                || normalized.startsWith("start a webdriver recording ");
    }

    private static boolean isMobileControlIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.contains("mobile")
                && (containsAny(normalized, "toolchain", "appium", "adb", "emulator", "sdk", "inspector")
                || containsAny(normalized, "accessibility tree", "current mobile screen", "mobile screen", "contexts",
                "context switch", "switch context", "screenshot", "quit mobile", "close mobile"));
    }

    private static String naturalMobileCommand(String text) {
        String normalized = normalizeNaturalCommand(text);
        String path = firstImageLikePath(text);
        if (containsAny(normalized, "screenshot", "screen shot")) {
            return "screenshot " + (path.isBlank() ? "target/shaft-mobile/screenshot.png" : path);
        }
        if (containsAny(normalized, "contexts", "context list")) {
            return "contexts";
        }
        if (containsAny(normalized, "switch context", "context switch")) {
            String contextName = lastAllCapsToken(text);
            return contextName.isBlank() ? "switch" : "switch " + contextName;
        }
        if (containsAny(normalized, "accessibility tree", "current mobile screen", "mobile screen", "inspect")) {
            return "tree";
        }
        if (containsAny(normalized, "quit mobile", "close mobile", "stop mobile session")) {
            return "quit";
        }
        return "status " + (normalized.contains("ios") ? "iOS" : "Android");
    }

    private static boolean isMobileRecordingStartIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("start mobile recording")
                || normalized.equals("start app recording")
                || normalized.startsWith("start mobile recording ")
                || normalized.startsWith("start app recording ");
    }

    private static boolean isMobileRecordingStopIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("stop mobile recording") || normalized.equals("stop app recording");
    }

    private static boolean isMobileCodegenIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return (normalized.startsWith("generate mobile code")
                || normalized.startsWith("generate appium code")
                || normalized.startsWith("create mobile code"))
                && !firstJsonLikePath(text).isBlank();
    }

    private static boolean isDoctorIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.startsWith("run doctor")
                || normalized.startsWith("analyze allure")
                || normalized.startsWith("analyse allure")
                || normalized.startsWith("doctor ");
    }

    private static String naturalQuery(String text) {
        String trimmed = text(text);
        String[] prefixes = {
                "search the shaft docs", "search shaft docs", "search the shaft guide", "search shaft guide",
                "find shaft docs", "find shaft guide", "guide me on", "docs for",
                "find automation scenarios", "find scenarios", "show scenarios", "automation scenarios for",
                "scenario ideas for", "run guardrails", "guardrails check"
        };
        String lower = trimmed.toLowerCase(Locale.ROOT);
        for (String prefix : prefixes) {
            if (lower.startsWith(prefix)) {
                return trimmed.substring(prefix.length()).trim();
            }
        }
        return trimmed;
    }

    private static String naturalCode(String text) {
        String query = naturalQuery(text);
        String lower = query.toLowerCase(Locale.ROOT);
        String[] prefixes = {"check generated java code", "check this java code"};
        for (String prefix : prefixes) {
            if (lower.startsWith(prefix)) {
                return query.substring(prefix.length()).trim();
            }
        }
        return query;
    }

    private static String naturalProjectName(String text) {
        String normalized = normalizeNaturalCommand(text);
        String[] prefixes = {"create a shaft project", "create shaft project", "new shaft project", "scaffold shaft project"};
        for (String prefix : prefixes) {
            if (normalized.startsWith(prefix)) {
                String name = text(text).substring(Math.min(prefix.length(), text(text).length())).trim();
                return firstTokenOrDefault(name, "shaft-demo");
            }
        }
        return "shaft-demo";
    }

    private static String naturalProjectRoot(String text, String workingDirectory) {
        String trimmed = text(text);
        String lower = trimmed.toLowerCase(Locale.ROOT);
        String[] prefixes = {"preview shaft upgrade", "preview upgrade", "dry run shaft upgrade",
                "upgrade this shaft project", "upgrade shaft project"};
        for (String prefix : prefixes) {
            if (lower.startsWith(prefix)) {
                String root = firstTokenOrDefault(trimmed.substring(prefix.length()).trim(), "");
                if (!root.isBlank()) {
                    return root;
                }
            }
        }
        String path = firstPathLike(text);
        if (!path.isBlank()) {
            return path;
        }
        return workingDirectory == null || workingDirectory.isBlank() ? "." : workingDirectory;
    }

    private static JsonObject triage(String workingDirectory) {
        return triage(workingDirectory, "allure-results");
    }

    private static JsonObject triage(String workingDirectory, String allurePath) {
        JsonObject arguments = new JsonObject();
        JsonArray allureResultPaths = new JsonArray();
        allureResultPaths.add(allurePath == null || allurePath.isBlank() ? "allure-results" : allurePath);
        arguments.add("allureResultPaths", allureResultPaths);
        arguments.add("historicalBundlePaths", new JsonArray());
        arguments.addProperty("outputDirectory", "target/shaft-doctor");
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", true);
        arguments.addProperty("minimumAllureResults", 1);
        arguments.addProperty("repositoryRoot", workingDirectory == null ? "" : workingDirectory);
        arguments.add("allowedSourcePaths", new JsonArray());
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static Invocation generateTest(String rest) {
        String recordingPath = firstJsonLikePath(rest);
        if (recordingPath.isBlank()) {
            return Invocation.tool("test_automation_scenarios", scenarios(rest));
        }
        String lower = text(rest).toLowerCase(Locale.ROOT);
        if (lower.contains("mobile") || lower.contains("appium")) {
            return mobileCodegen(recordingPath);
        }
        if (lower.contains("playwright") || recordingPath.toLowerCase(Locale.ROOT).contains("playwright")) {
            // Playwright recorder sessions use their own recording schema (recordingPath), which
            // the Capture-session replay tools cannot consume; they keep the generate-only tool.
            return Invocation.tool("playwright_recording_code_blocks", generatePlaywrightCodeFromRecording(recordingPath));
        }
        // Explicit /codegen re-executes the recording (issue #3409): the generated test is
        // compiled and replayed headless so the returned code blocks are verified against the
        // live flow, not just statically generated. The automatic post-stop review keeps the
        // faster generate-only tool.
        return Invocation.tool("capture_generate_replay", generateReplayFromRecording(recordingPath));
    }

    private static JsonObject captureStart(String rest) {
        JsonObject arguments = new JsonObject();
        String targetUrl = parseLeadingUrl(rest);
        arguments.addProperty("targetUrl", targetUrl.isBlank() ? DEFAULT_CAPTURE_TARGET_URL : targetUrl);
        arguments.addProperty("browser", "Chrome");
        arguments.addProperty("outputPath", defaultCaptureRecordingPath());
        arguments.addProperty("headless", recorderHeadlessPreference());
        return arguments;
    }

    static JsonObject captureCodeReview(String sessionPath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath",
                sessionPath == null || sessionPath.isBlank() ? DEFAULT_CAPTURE_RECORDING_PATH : sessionPath);
        arguments.addProperty("outputDirectory", DEFAULT_CAPTURE_REVIEW_DIRECTORY);
        arguments.addProperty("packageName", "tests.generated");
        arguments.addProperty("className", "RecordedFlowTest");
        arguments.addProperty("overwrite", true);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    static JsonObject playwrightCodeReview(String recordingPath) {
        return generatePlaywrightCodeFromRecording(
                recordingPath == null || recordingPath.isBlank() ? DEFAULT_PLAYWRIGHT_RECORDING_PATH : recordingPath);
    }

    static Invocation stopPlaywrightRecording() {
        return Invocation.tool("playwright_record_stop", stopRecording());
    }

    private static String defaultCaptureRecordingPath() {
        return DEFAULT_CAPTURE_RECORDING_PATH_PREFIX + System.currentTimeMillis() + ".json";
    }

    static Invocation approvedCaptureIntegration(
            Selection selection,
            String workingDirectory,
            String customCommand,
            String reviewMarkdown,
            String rawCodegenResult) {
        if (selection.cloud() || !"CLI".equals(selection.runtime())) {
            return Invocation.local("Switch to a Local / CLI route before approving generated Capture code.");
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", selection.client());
        arguments.addProperty("mode", "AGENT");
        arguments.addProperty("prompt", captureIntegrationPrompt(reviewMarkdown, rawCodegenResult));
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.add("command", commandArray(customCommand));
        arguments.add("environment", new JsonObject());
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", true);
        return Invocation.tool("autobot_local_agent_run", arguments);
    }

    static boolean isStopRecording(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("stop")
                || normalized.equals("stop recording")
                || normalized.equals("stop playwright recording")
                || normalized.equals("stop playwright recorder")
                || normalized.equals("stop recorder")
                || normalized.equals("stop capture")
                || normalized.equals("finish recording")
                || normalized.equals("end recording");
    }

    private static boolean isDiscardRecording(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("discard")
                || normalized.equals("discard it")
                || normalized.equals("discard recording")
                || normalized.equals("discard capture")
                || normalized.equals("discard current recording")
                || normalized.equals("discard current capture")
                || normalized.equals("discard recording session")
                || normalized.equals("discard capture session")
                || normalized.startsWith("discard recording ")
                || normalized.startsWith("discard capture ");
    }

    private static boolean isReRecord(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("re-record")
                || normalized.equals("re record")
                || normalized.equals("re-record recording")
                || normalized.equals("capture again")
                || normalized.equals("record again")
                || normalized.startsWith("re-record ")
                || normalized.startsWith("re record ")
                || normalized.startsWith("capture again")
                || normalized.startsWith("record again ")
                || normalized.startsWith("restart recording")
                || normalized.startsWith("restart capture")
                || normalized.startsWith("start recording again")
                || normalized.startsWith("start capture again");
    }

    static boolean isCaptureApproval(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("generate")
                || normalized.equals("okay")
                || normalized.equals("ok")
                || normalized.equals("approve")
                || normalized.equals("approved")
                || normalized.equals("apply")
                || normalized.equals("apply it")
                || normalized.equals("apply capture");
    }

    private static boolean isStartRecording(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("start recording")
                || normalized.equals("start a recording")
                || normalized.equals("start recorder")
                || normalized.equals("start capture")
                || normalized.startsWith("start recording ")
                || normalized.startsWith("start a recording ")
                || normalized.startsWith("start recorder ")
                || normalized.startsWith("start capture ");
    }

    private static boolean isReviewRecording(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("review recording")
                || normalized.equals("review capture")
                || normalized.equals("review recorded code")
                || normalized.startsWith("review recording ")
                || normalized.startsWith("review capture ")
                || normalized.startsWith("review code from recording ")
                || normalized.startsWith("generate reviewed code from recording ");
    }

    private static JsonObject stopRecording() {
        return stopRecording(false);
    }

    private static JsonObject stopRecording(boolean discard) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("discard", discard);
        return arguments;
    }

    private static JsonObject playwrightRecordStart() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", DEFAULT_PLAYWRIGHT_RECORDING_PATH);
        arguments.addProperty("mode", "default");
        arguments.addProperty("includeSensitiveValues", false);
        return arguments;
    }

    private static JsonObject generateCaptureCodeFromRecording(String recordingPath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", recordingPath);
        arguments.addProperty("outputDirectory", ".");
        arguments.addProperty("packageName", "tests.generated");
        arguments.addProperty("className", "RecordedFlowTest");
        // The generated class name and path are deterministic, so re-running /codegen on the same
        // (or another) recording must regenerate rather than fail with "already exists".
        arguments.addProperty("overwrite", true);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    /**
     * Arguments for the generate-compile-replay codegen tools. Replay re-executes the generated
     * test headless (enforced server-side) against the recorded flow; AI enrichment stays off
     * unless the user explicitly opts in elsewhere.
     */
    private static JsonObject generateReplayFromRecording(String recordingPath) {
        JsonObject arguments = generateCaptureCodeFromRecording(recordingPath);
        arguments.addProperty("replay", true);
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        return arguments;
    }

    private static String captureIntegrationPrompt(String reviewMarkdown, String rawCodegenResult) {
        return """
                The user approved the reviewed SHAFT Capture code. Create the actual Java test files now.

                Tool availability (important):
                - Create and edit files with your file-editing tools only; do not write files through shell commands.
                - Optional helpers (shaft_coding_partner_plan, shaft_guide_search, test_code_guardrails_check, shell/Maven commands) may be unavailable or denied in this session. Try each at most once; if it is denied or missing, continue WITHOUT it immediately instead of retrying or asking again.
                - The raw codegen result below already contains everything needed to create the files, so a denied helper is never a blocker.

                Requirements:
                - Use the Page Object Model where practical.
                - Inspect the current project structure before editing.
                - Move stable locators into page object classes.
                - Move replay actions into intent-named page methods.
                - Do not use SHAFT.GUI.Locator.xpath; use smart locators, the locator builder, or By.xpath only as a last fallback.
                - Keep the TestNG test focused on scenario orchestration and final assertions.
                - Do not duplicate existing locators, page actions, tests, or classes.
                - Preserve the recorded browser journey; do not collapse it to only opening the start page or a generic title check.
                - When the capture searched results, open the first result with a scoped first-result locator before asserting.
                - assert the final page title or final page-specific text from the recorded destination.
                - Preserve existing repository style and the smallest compiling source edit.
                - Do not start a new recording or drive the browser again.
                - Run the smallest relevant compile or test check only if a shell tool is available and approved; skip it otherwise.

                Final answer format (mandatory):
                - List every file you created or modified with its repository-relative path.
                - Include the full content of each created test and page object class in fenced ```java blocks.
                - If you could not create any file, say exactly which tool failed or was denied and why; never answer with a bare confirmation like "Done".

                Reviewed Capture output:
                %s

                Raw codegen result:
                ```json
                %s
                ```
                """.formatted(clip(reviewMarkdown, 12_000), clip(rawCodegenResult, 24_000)).strip();
    }

    private static JsonObject generatePlaywrightCodeFromRecording(String recordingPath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("recordingPath", recordingPath);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static JsonArray commandArray(String value) {
        JsonArray array = new JsonArray();
        if (value == null || value.isBlank()) {
            return array;
        }
        for (String token : ShaftCommandLine.parse(value)) {
            array.add(token);
        }
        return array;
    }

    private static String localAgentPrompt(
            String text,
            String mode,
            boolean allowSourceMutation,
            OpenFileContext openFileContext,
            String conversationContext) {
        String lower = text.toLowerCase(Locale.ROOT);
        String normalizedMode = Selection.normalize(mode, "ASK");
        boolean agentMode = "AGENT".equals(normalizedMode);
        boolean codeGenerationRequest = isCodeGenerationRequest(text);
        String hint = SHAFT_MCP_USAGE_HINT
                + (codeGenerationRequest
                ? "\n" + codeGenerationGuidance(text) + "\n" + codeRequestScope(normalizedMode, openFileContext)
                : "");
        String withHint = lower.contains("shaft-mcp")
                ? (codeGenerationRequest
                ? codeGenerationGuidance(text) + "\n" + codeRequestScope(normalizedMode, openFileContext)
                + "\n\n" + text
                : text)
                : hint + "\n\n" + text;
        withHint = withConversationContext(withHint, conversationContext);
        if (!agentMode) {
            return withHint;
        }
        return allowSourceMutation
                ? withHint + "\n\n" + AGENT_SOURCE_APPROVAL
                : withHint + "\n\n" + AGENT_SOURCE_GUARD;
    }

    static boolean requiresAgentModeForMcp(String prompt, String mode, Invocation invocation) {
        if ("AGENT".equals(Selection.normalize(mode, "ASK")) || invocation == null || invocation.isLocal()) {
            return false;
        }
        if (!Invocation.LOCAL_AGENT_TOOL.equals(invocation.toolName())) {
            return false;
        }
        return promptNeedsMcpToolCalls(prompt);
    }

    private static boolean promptNeedsMcpToolCalls(String prompt) {
        String normalized = normalizeNaturalCommand(prompt);
        return isCodeGenerationRequest(prompt)
                || containsAny(normalized,
                "shaft mcp",
                "shaft-mcp",
                "browser",
                "open browser",
                "open page",
                "open url",
                "navigate",
                "visit http",
                "click",
                "type into",
                "search for",
                "locator",
                "inspect element",
                "mobile app",
                "record flow",
                "take screenshot",
                "duckduckgo");
    }

    private static String withEffortHint(String prompt, String effort) {
        if (!AssistantModelCatalog.isExplicitEffort(effort)) {
            return prompt;
        }
        return "Use " + effort.toLowerCase(Locale.ROOT) + " reasoning effort for this request.\n\n" + prompt;
    }

    private static String withConversationContext(String prompt, String conversationContext) {
        String context = text(conversationContext);
        if (context.isBlank()) {
            return prompt;
        }
        return """
                Conversation context:
                %s

                Current request:
                %s
                """.formatted(context, prompt).stripIndent().trim();
    }

    private static String codeRequestScope(String mode, OpenFileContext openFileContext) {
        String modeGuidance = switch (mode) {
            case "AGENT" -> "Agent mode: suggest code changes directly inside the IDE for the current open class only.";
            case "ASK" -> "Ask mode: recommend migrated SHAFT-syntax code in markdown; do not apply IDE edits.";
            default -> "Plan mode: outline the SHAFT migration steps before recommending code.";
        };
        if (openFileContext != null && openFileContext.present()) {
            return """
                    %s
                    Use only the currently open file as project source context. Do not read, scan, or summarize other project files unless the user explicitly asks.
                    Current open file: %s
                    ```java
                    %s
                    ```
                    """.formatted(modeGuidance, openFileContext.path(), clip(openFileContext.text(), 8_000))
                    .stripIndent().trim();
        }
        return """
                %s
                Use only the currently open file for code writing or conversion. No current open file content was provided, so ask the user to open the target file or paste the code. Do not read all project files to find context.
                """.formatted(modeGuidance).stripIndent().trim();
    }

    private static boolean isCodeGenerationRequest(String text) {
        String normalized = normalizeNaturalCommand(text);
        return containsAny(normalized,
                "generate code",
                "generate java",
                "generate test",
                "generate tests",
                "write code",
                "write java",
                "create test",
                "create tests",
                "write test",
                "write tests",
                "implement test",
                "add test",
                "page object",
                "pom",
                "codegen",
                "code gen",
                "convert code",
                "convert this code",
                "convert this selenium code",
                "convert to shaft",
                "selenium code to shaft",
                "to shaft syntax",
                "migrate code",
                "migrate to shaft",
                "rewrite code",
                "fix code",
                "fix this code",
                "java code",
                "code only",
                "code-only",
                "draft code",
                "generated code");
    }

    private static String codeGenerationGuidance(String text) {
        return shouldUseLiveCodegenTools(text)
                ? SHAFT_CODEGEN_TOOL_GUIDANCE + "\n" + SHAFT_LIVE_CODEGEN_TOOL_GUIDANCE
                : SHAFT_CODEGEN_TOOL_GUIDANCE;
    }

    private static boolean shouldUseLiveCodegenTools(String text) {
        String normalized = normalizeNaturalCommand(text);
        return !isCodeOnlyRequest(normalized)
                && containsAny(normalized,
                "live browser",
                "live locator",
                "verify locator",
                "verify locators",
                "inspect live",
                "run browser",
                "open browser",
                "record flow",
                "record the flow",
                "capture flow",
                "capture the flow");
    }

    private static boolean isCodeOnlyRequest(String text) {
        String normalized = normalizeNaturalCommand(text);
        return containsAny(normalized,
                "code only",
                "code-only",
                "draft only",
                "draft code",
                "write code only",
                "do not run browser",
                "dont run browser",
                "don't run browser",
                "without running browser",
                "do not open browser",
                "dont open browser",
                "don't open browser",
                "no browser");
    }

    private static String[] splitTargetUrlAndIntent(String rest) {
        String trimmed = rest == null ? "" : rest.trim();
        if (trimmed.isBlank()) {
            return new String[]{"", ""};
        }
        int firstSpace = trimmed.indexOf(' ');
        if (firstSpace < 0) {
            if (isUrl(trimmed)) {
                return new String[]{trimmed, ""};
            }
            return new String[]{"", trimmed};
        }
        String firstToken = trimmed.substring(0, firstSpace);
        String restIntent = trimmed.substring(firstSpace + 1).trim();
        if (isUrl(firstToken)) {
            return new String[]{firstToken, restIntent};
        }
        return new String[]{"", trimmed};
    }

    private static String firstJsonLikePath(String rest) {
        if (rest == null || rest.isBlank()) {
            return "";
        }
        for (String token : rest.split("\\s+")) {
            if (token.toLowerCase(Locale.ROOT).endsWith(".json")) {
                return token;
            }
        }
        return "";
    }

    private static String firstImageLikePath(String rest) {
        if (rest == null || rest.isBlank()) {
            return "";
        }
        for (String token : rest.split("\\s+")) {
            String lower = token.toLowerCase(Locale.ROOT);
            if (lower.endsWith(".png") || lower.endsWith(".jpg") || lower.endsWith(".jpeg")) {
                return token;
            }
        }
        return "";
    }

    private static String lastAllCapsToken(String rest) {
        if (rest == null || rest.isBlank()) {
            return "";
        }
        String match = "";
        for (String token : rest.split("\\s+")) {
            if (token.matches("[A-Z][A-Z0-9_*.-]*")) {
                match = token;
            }
        }
        return match;
    }

    private static String firstPathLike(String rest) {
        if (rest == null || rest.isBlank()) {
            return "";
        }
        for (String token : rest.split("\\s+")) {
            String lower = token.toLowerCase(Locale.ROOT);
            if (lower.contains("allure-results")
                    || lower.contains("shaft-doctor")
                    || lower.contains("shaft-traces")
                    || lower.endsWith(".json")) {
                return token;
            }
        }
        return "";
    }

    private static String parseLeadingUrl(String rest) {
        String[] parts = (rest == null ? "" : rest).trim().split("\\s+");
        for (String part : parts) {
            if (isUrl(part)) {
                return part;
            }
        }
        return "";
    }

    private static JsonObject screenshot(String outputPath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", outputPath);
        arguments.addProperty("includeBase64", false);
        return arguments;
    }

    private static JsonObject targetUrl(String key, String value) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty(key, value);
        return arguments;
    }

    private static JsonObject platform(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("platformName", platformName(firstWord(rest)));
        return arguments;
    }

    private static JsonObject discard(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("discard", text(rest).toLowerCase(Locale.ROOT).contains("discard"));
        return arguments;
    }

    private static String platformName(String candidate) {
        String value = text(candidate);
        if ("ios".equalsIgnoreCase(value)) {
            return "iOS";
        }
        return value.isBlank() || "android".equalsIgnoreCase(value) ? "Android" : value;
    }

    private static String firstTokenOrDefault(String value, String fallback) {
        String token = firstWord(value);
        return token.isBlank() ? fallback : token;
    }

    private static String firstWord(String value) {
        String trimmed = text(value);
        if (trimmed.isBlank()) {
            return "";
        }
        int firstSpace = trimmed.indexOf(' ');
        return firstSpace < 0 ? trimmed : trimmed.substring(0, firstSpace);
    }

    private static String afterFirstWord(String value) {
        String trimmed = text(value);
        int firstSpace = trimmed.indexOf(' ');
        return firstSpace < 0 ? "" : trimmed.substring(firstSpace + 1).trim();
    }

    private static boolean commandIs(String action, String... candidates) {
        for (String candidate : candidates) {
            if (action.equals(candidate)) {
                return true;
            }
        }
        return false;
    }

    private static String normalizeNaturalCommand(String text) {
        return (text == null ? "" : text)
                .trim()
                .toLowerCase(Locale.ROOT)
                .replaceAll("\\s+", " ")
                .replaceAll("[.!?]+$", "");
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static String clip(String text, int maxCharacters) {
        if (text == null || text.length() <= maxCharacters) {
            return text == null ? "" : text;
        }
        return text.substring(0, maxCharacters) + "\n... truncated ...";
    }

    private static boolean isUrl(String value) {
        String candidate = value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
        return candidate.startsWith("http://") || candidate.startsWith("https://") || candidate.startsWith("file:");
    }

    record ToolCall(String toolName, JsonObject arguments) {
    }

    record CommandHint(String canonical, String summary, List<String> synonyms, String example) {
    }

    private record CommandDefinition(
            String canonical,
            String summary,
            List<String> aliases,
            String example,
            CommandBuilder builder) {
        boolean matches(String command) {
            String normalized = command == null ? "" : command.toLowerCase(Locale.ROOT);
            if (canonical.toLowerCase(Locale.ROOT).equals(normalized)) {
                return true;
            }
            for (String alias : aliases) {
                if (alias.toLowerCase(Locale.ROOT).equals(normalized)) {
                    return true;
                }
            }
            return false;
        }

        Invocation invoke(String command, String rest, String workingDirectory) {
            return builder.build(command, rest, workingDirectory);
        }

        CommandHint hint() {
            return new CommandHint(canonical, summary, aliases, example);
        }
    }

    @FunctionalInterface
    private interface CommandBuilder {
        Invocation build(String command, String rest, String workingDirectory);
    }

    private record NaturalIntent(NaturalIntentMatcher matcher, NaturalIntentBuilder builder) {
        boolean matches(String text) {
            return matcher.matches(text);
        }

        Invocation invoke(String text, String workingDirectory) {
            return builder.build(text, workingDirectory);
        }
    }

    @FunctionalInterface
    private interface NaturalIntentMatcher {
        boolean matches(String text);
    }

    @FunctionalInterface
    private interface NaturalIntentBuilder {
        Invocation build(String text, String workingDirectory);
    }

    record Invocation(List<ToolCall> toolCalls, String localResponse) {
        private static final String LOCAL_AGENT_TOOL = "autobot_local_agent_run";
        private static final String PROVIDER_CHAT_TOOL = "autobot_provider_chat";

        static Invocation tool(String toolName, JsonObject arguments) {
            return new Invocation(List.of(new ToolCall(toolName, arguments == null ? new JsonObject() : arguments)), null);
        }

        static Invocation sequence(List<ToolCall> toolCalls) {
            if (toolCalls == null || toolCalls.isEmpty()) {
                return local("No MCP tools were selected.");
            }
            return new Invocation(List.copyOf(toolCalls), null);
        }

        static Invocation local(String response) {
            return new Invocation(List.of(), response);
        }

        boolean isLocal() {
            return localResponse != null;
        }

        boolean isSequence() {
            return toolCalls.size() > 1;
        }

        boolean requiresMcpConfiguration() {
            return !isLocal() && toolCalls.stream().anyMatch(Invocation::isDirectMcpFeatureTool);
        }

        private static boolean isDirectMcpFeatureTool(ToolCall call) {
            return call != null
                    && !LOCAL_AGENT_TOOL.equals(call.toolName())
                    && !PROVIDER_CHAT_TOOL.equals(call.toolName());
        }

        String toolName() {
            return toolCalls.isEmpty() ? "" : toolCalls.get(0).toolName();
        }

        JsonObject arguments() {
            return toolCalls.isEmpty() ? new JsonObject() : toolCalls.get(0).arguments();
        }
    }

    record Selection(boolean cloud, String family, String runtime, String cloudProvider, String cloudModel,
                     String localModel, String effort) {
        static Selection local(String family, String runtime) {
            return local(family, runtime, "", "");
        }

        static Selection local(String family, String runtime, String model, String effort) {
            return new Selection(false, normalize(family, "CODEX"), normalize(runtime, "CLI"), "", "",
                    model == null ? "" : model.trim(), normalizeEffort(effort));
        }

        static Selection cloud(String provider, String model) {
            return cloud(provider, model, "");
        }

        static Selection cloud(String provider, String model, String effort) {
            return new Selection(true, "", "", normalize(provider, "gemini").toLowerCase(Locale.ROOT),
                    model == null ? "" : model.trim(), "", normalizeEffort(effort));
        }

        private static String normalizeEffort(String effort) {
            String normalized = normalize(effort, AssistantModelCatalog.DEFAULT_EFFORT);
            return AssistantModelCatalog.isExplicitEffort(normalized)
                    ? normalized
                    : AssistantModelCatalog.DEFAULT_EFFORT;
        }

        static Selection fromClient(String client) {
            return switch (normalize(client, "CODEX")) {
                case "CLAUDE_CODE" -> local("CLAUDE", "CLI");
                case "COPILOT_CLI" -> local("COPILOT", "CLI");
                default -> local("CODEX", "CLI");
            };
        }

        String client() {
            return switch (family) {
                case "CLAUDE" -> "CLAUDE_CODE";
                case "COPILOT" -> "COPILOT_CLI";
                default -> "CODEX";
            };
        }

        String displayName() {
            String familyName = switch (family) {
                case "CLAUDE" -> "Claude";
                case "COPILOT" -> "GitHub Copilot";
                default -> "Codex";
            };
            String runtimeName = switch (runtime) {
                case "IDE_PLUGIN" -> "plugin";
                case "DESKTOP_APP" -> "desktop app";
                default -> "CLI";
            };
            return familyName + " " + runtimeName;
        }

        private static String normalize(String value, String fallback) {
            String normalized = value == null || value.isBlank() ? fallback : value.trim();
            return normalized.toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
        }
    }
}
