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
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Builds SHAFT Assistant MCP invocations from prompt text.
 */
final class AssistantCommand {
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    private static final int DEFAULT_BROWSER_MAX_CHARACTERS = 12_000;
    private static final int DEFAULT_MOBILE_MAX_CHARACTERS = 8_000;
    private static final int DEFAULT_BROWSER_MAX_ELEMENTS = 10;
    static final String DEFAULT_CAPTURE_RECORDING_PATH = "recordings/intellij-capture.json";
    static final String DEFAULT_CAPTURE_RECORDING_PATH_PREFIX = "recordings/intellij-capture-";
    static final String DEFAULT_PLAYWRIGHT_RECORDING_PATH = "recordings/playwright-recording.json";
    static final String DEFAULT_CAPTURE_REVIEW_DIRECTORY = "target/shaft-capture/assistant-review";
    private static final String DEFAULT_MOBILE_RECORDING_PATH = "recordings/mobile-recording.json";
    private static final String DEFAULT_MOBILE_INSPECTOR_RECORDING_PATH = "recordings/mobile-inspector.json";
    private static final String DEFAULT_DOCTOR_REPORT_PATH = "target/shaft-doctor/doctor-report.json";
    private static final String DEFAULT_STORAGE_STATE_PATH = "target/shaft-browser/storage-state.json";
    private static final String DEFAULT_WINDOW_SIZE = "1280x800";

    private record NaturalSessionCommandRule(Predicate<String> matchesNormalized, Function<String, String> commandFor) {
    }

    private static final List<NaturalSessionCommandRule> BROWSER_SESSION_COMMAND_RULES = List.of(
            new NaturalSessionCommandRule(
                    normalized -> containsAny(normalized, "resize", "window size"),
                    text -> "size " + firstSizeLikeToken(text)),
            new NaturalSessionCommandRule(
                    normalized -> containsAny(normalized, "delete all cookies", "clear cookies", "clear all cookies"),
                    text -> "clearcookies"),
            new NaturalSessionCommandRule(
                    normalized -> normalized.contains("delete cookie"),
                    text -> "deletecookie " + firstTokenAfterWord(text, "cookie")),
            new NaturalSessionCommandRule(
                    normalized -> normalized.contains("aria snapshot"),
                    text -> "aria"),
            new NaturalSessionCommandRule(
                    normalized -> normalized.contains("accessibility audit"),
                    text -> "audit"),
            new NaturalSessionCommandRule(
                    normalized -> containsAny(normalized, "network requests", "network transactions"),
                    text -> "network"),
            new NaturalSessionCommandRule(
                    normalized -> containsAny(normalized, "save session", "save storage state"),
                    text -> "save " + firstJsonLikePath(text)),
            new NaturalSessionCommandRule(
                    normalized -> containsAny(normalized, "load session", "load storage state"),
                    text -> "load " + firstJsonLikePath(text)));
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
    // Split out so cloudPrompt's non-code-generation branch (which has no other reason to mention
    // shaft-mcp -- cloud providers don't have it) can still ask cloud models to emit the structured
    // line (or, failing that, the fence) that AssistantQuestion looks for, without pulling in the
    // rest of the shaft-mcp-specific hint. Issue #3719: the single-line JSON form is preferred --
    // AssistantLocalAgentRunner's StructuredStreamParser recognizes it at the terminal-event
    // boundary for local Claude Code/Codex runs (immune to activity-footer/token-usage lines getting
    // appended after it), and AssistantQuestion.detectStructuredLine recognizes it directly in the
    // displayed markdown for paths with no runner envelope (cloud chat). The fence remains the
    // documented fallback for every CLI (Copilot, custom commands, or a model that just can't
    // produce a clean single JSON line), unchanged from before this issue.
    private static final String SHAFT_OPTIONS_HINT =
            """
                    If you need to ask the user a genuine clarifying question with a short list of concrete choices (2-6 short options), prefer ending your final answer with a single line containing only a JSON object shaped {"shaft-question": "<question text>", "shaft-options": ["<option 1>", "<option 2>"]} (for example: {"shaft-question": "Want me to actually run through a recording now?", "shaft-options": ["Use the sample page", "I'll give you a URL"]}); if you cannot produce that exact single-line JSON object, fall back to ending your answer with a fenced code block tagged shaft-options containing a JSON array of the option labels instead (for example: a fence tagged shaft-options wrapping ["Use the sample page", "I'll give you a URL"]); omit both forms for ordinary narrative answers.
                    """.stripIndent().trim();
    private static final String SHAFT_MCP_USAGE_HINT =
            """
                    If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.
                    For WebDriver browser tasks, call driver_initialize before browser_* tools; do not use Playwright unless requested.
                    Never start an interactive user-driven recording (capture_start, playwright_record_start, mobile_record_start): your MCP session ends with this turn and would kill the recording seconds after it starts. Tell the user to ask the SHAFT panel to record instead.
                    A scripted capture_start_codegen session that you drive and capture_stop within this same turn is allowed.
                    Generated Java code must use SHAFT syntax only: SHAFT.GUI.WebDriver, driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator.
                    Never generate SHAFT.GUI.Locator.xpath(...); use smart locators, the SHAFT locator builder, or By.xpath only as a last fallback.
                    Never generate raw Selenium code such as WebDriver, ChromeDriver, driver.get(...), driver.findElement(...), or direct WebElement actions.
                    For repeated search-result anchors, scope the locator to the first result container; for Wikipedia use By.id("searchInput") for the search box and `(//div[contains(@class,'mw-search-result-heading')])[1]//a` for the first result.
                    """.stripIndent().trim() + "\n" + SHAFT_OPTIONS_HINT;
    private static final String SHAFT_CODEGEN_TOOL_GUIDANCE =
            """
                    This is a code-generation request. Before returning Java:
                    - Call shaft_guide_search for the relevant SHAFT guide examples and cite the official guide URLs.
                    - Call test_automation_scenarios for broad test/page-object design to learn the matching SHAFT coding pattern.
                    - Inspect existing project code and call shaft_coding_partner_plan before creating tests, page objects, locators, or actions.
                    - Reuse existing tests, page objects, locator fields, and action methods first.
                    - If the user provided a recording JSON path, generate from it with capture_generate_replay (replay true, useAi false); it re-executes the recording headless, compiles the generated test, and proves every locator live. No active capture session is required and none should be demanded.
                    - If the user described the scenario without a recording, do not stall and do not return unverified locator guesses: start a fresh session with capture_start_codegen, perform the described actions live (element_type, element_click, natural_act), call capture_stop, then pass the persisted recording to capture_generate_replay (replay true, useAi false) so the returned code ships with replay-proven locators.
                    - Choose the recording surface that matches the description: capture_start_codegen for a WebDriver browser flow, or the mobile_record_start / mobile_tap / mobile_type / mobile_record_stop sequence for a native or hybrid mobile flow.
                    - If the user asks for an API/HTTP test (SHAFT.API / RestActions) rather than a UI flow, record with capture_api_start (networkOptions capturing request/response bodies), browse the flow live, call capture_api_stop, then capture_api_generate (style SCENARIO by default) to return SHAFT.API code; do not force capture_start_codegen's UI-locator tools onto an API-only request.
                    - If capture_generate_replay (or its mobile/Playwright equivalents) reports a broken or unhealed locator, call healer_run_failed_test (or playwright_healer_run_failed_test) to self-heal it before returning code, instead of returning a known-broken locator.
                    - MUST follow the Page Object Model: one Page Object Model class per described flow, holding all locators and action methods, matching this project's existing package/module layout and SHAFT syntax; do not put driver.element()/locator calls directly in a @Test method body -- extract them into a page class. Keep the returned code minimal and project-structure-compliant; do not repeat this guidance text back to the user or paste unrelated boilerplate.
                    - Write in SHAFT's fluent design and action chaining style: chain calls on the same element/actions object (for example driver.element().click(locator).type(otherLocator, "value")) instead of a separate statement per action; only break the chain when the next step needs a different receiver.
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
    private static final CommandDefinition RECORD_WEB_COMMAND = new CommandDefinition("/record",
            "Record web actions",
            List.of("/record-web", "/rec", "/capture"),
            "/record https://example.com",
            (command, rest, workingDirectory) -> record(rest));
    private static final CommandDefinition RECORD_MOBILE_COMMAND = new CommandDefinition("/record-mobile",
            "Record mobile actions",
            List.of(),
            "/record-mobile inspector Android recordings/inspector.json",
            (command, rest, workingDirectory) -> mobileRecord(rest));
    private static final CommandDefinition RECORD_API_COMMAND = new CommandDefinition("/record-api",
            "Record API traffic",
            List.of("/api-record", "/capture-api"),
            "/record-api https://example.com/api",
            (command, rest, workingDirectory) -> recordApi(rest));
    private static final CommandDefinition DOCTOR_COMMAND = new CommandDefinition("/doctor",
            "Analyze failures and recommend fixes",
            List.of(),
            "/doctor allure-report/AllureReport.html",
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
            RECORD_API_COMMAND,
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
            RECORD_API_COMMAND,
            DOCTOR_COMMAND,
            GUIDE_COMMAND,
            new CommandDefinition("/scenarios", "Find automation scenarios",
                    List.of("/scenario", "/ideas"),
                    "/scenarios checkout", (command, rest, workingDirectory) -> Invocation.tool("test_automation_scenarios", scenarios(rest))),
            BROWSER_COMMAND,
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
    // Specificity weights for NATURAL_INTENTS: directIntent (below) scores every predicate that
    // matches the prompt and dispatches the highest-weight match instead of stopping at the first
    // one, so ties fall back to declaration order in the list below. Magnitudes are otherwise
    // arbitrary; only the relative ordering matters. Two orderings are load-bearing:
    //  - WEIGHT_MOBILE_RECORDING > WEIGHT_BROWSER_RECORDING so a "record ..." phrase that satisfies
    //    both recognizers' keyword lists (e.g. "record my actions on the Android emulator") still
    //    starts the mobile recorder, without isBrowserRecordingIntent hand-excluding
    //    isMobileRecordingStartIntent the way the issue #3429 patch used to.
    //  - WEIGHT_TOPIC_CONTROL is shared by browser-control and mobile-control: both are equally
    //    broad "topic + action-word" recognizers, so a phrase satisfying both (e.g. "open mobile
    //    inspector http://...") resolves by declaration order below, exactly like the old
    //    first-match linear scan did (browser-control is declared first).
    //  - WEIGHT_API_RECORDING > WEIGHT_MOBILE_RECORDING > WEIGHT_BROWSER_RECORDING so a "record
    //    api ..." phrase that happens to also satisfy the browser/mobile recognizers' broad keyword
    //    lists still starts the API-capture recorder (issue #3726). isApiRecordingIntent excludes
    //    "without a browser" phrasing up front so RecordApiMobileAction's no-browser proxy prefill
    //    is left to isMobileApiRecordingIntent instead (issue #3738): WEIGHT_MOBILE_API_RECORDING
    //    sits between WEIGHT_API_RECORDING and WEIGHT_MOBILE_RECORDING so that prefill starts
    //    mobile_api_record_start deterministically, outscoring both isMobileRecordingStartIntent's
    //    broad "android"/"mobile" keywords and isBrowserRecordingIntent's "browser" keyword (which
    //    the literal words "without a browser" also happen to satisfy).
    private static final int WEIGHT_COMMAND_HELP = 100;
    private static final int WEIGHT_NAMED_TOOL_REQUEST = 90;
    private static final int WEIGHT_PROJECT_UPGRADE = 80;
    private static final int WEIGHT_API_RECORDING = 70;
    private static final int WEIGHT_MOBILE_API_RECORDING = 68;
    private static final int WEIGHT_MOBILE_RECORDING = 65;
    private static final int WEIGHT_BROWSER_RECORDING = 60;
    private static final int WEIGHT_TOPIC_CONTROL = 55;
    private static final int WEIGHT_MOBILE_CODEGEN = 40;
    private static final int WEIGHT_DOCTOR = 30;
    private static final List<NaturalIntent> NATURAL_INTENTS = List.of(
            new NaturalIntent(WEIGHT_COMMAND_HELP, AssistantCommand::isCommandHelpIntent,
                    (text, workingDirectory) -> Invocation.local(commandHelp(false))),
            new NaturalIntent(WEIGHT_NAMED_TOOL_REQUEST, AssistantCommand::isCodingPartnerIntent,
                    (text, workingDirectory) -> Invocation.tool(
                            "shaft_coding_partner_plan",
                            codingPartnerPlan(naturalCodingPartnerIntent(text), workingDirectory))),
            new NaturalIntent(WEIGHT_NAMED_TOOL_REQUEST, AssistantCommand::isGuideSearchIntent,
                    (text, workingDirectory) -> Invocation.tool("shaft_guide_search", guide(naturalQuery(text)))),
            new NaturalIntent(WEIGHT_NAMED_TOOL_REQUEST, AssistantCommand::isScenarioSearchIntent,
                    (text, workingDirectory) -> Invocation.tool("test_automation_scenarios", scenarios(naturalQuery(text)))),
            new NaturalIntent(WEIGHT_NAMED_TOOL_REQUEST, AssistantCommand::isGuardrailsCheckIntent,
                    (text, workingDirectory) -> Invocation.tool("test_code_guardrails_check", guardrails(naturalCode(text)))),
            new NaturalIntent(WEIGHT_NAMED_TOOL_REQUEST, AssistantCommand::isProjectCreateIntent,
                    (text, workingDirectory) -> projectCreateReview(naturalProjectName(text))),
            new NaturalIntent(WEIGHT_PROJECT_UPGRADE, AssistantCommand::isProjectUpgradeIntent,
                    (text, workingDirectory) -> Invocation.tool("shaft_project_upgrade", projectUpgrade(naturalProjectRoot(text, workingDirectory)))),
            new NaturalIntent(WEIGHT_TOPIC_CONTROL, AssistantCommand::isBrowserControlIntent,
                    (text, workingDirectory) -> browser(text)),
            new NaturalIntent(WEIGHT_API_RECORDING, AssistantCommand::isApiRecordingIntent,
                    (text, workingDirectory) -> recordApi(text)),
            new NaturalIntent(WEIGHT_MOBILE_API_RECORDING, AssistantCommand::isMobileApiRecordingIntent,
                    (text, workingDirectory) -> recordApiMobile(text)),
            new NaturalIntent(WEIGHT_BROWSER_RECORDING, AssistantCommand::isBrowserRecordingIntent,
                    (text, workingDirectory) -> record(text)),
            new NaturalIntent(WEIGHT_MOBILE_RECORDING, AssistantCommand::isMobileRecordingStartIntent,
                    (text, workingDirectory) -> mobileRecord("start " + firstJsonLikePath(text))),
            new NaturalIntent(WEIGHT_MOBILE_RECORDING, AssistantCommand::isMobileRecordingStopIntent,
                    (text, workingDirectory) -> mobileRecord("stop")),
            new NaturalIntent(WEIGHT_TOPIC_CONTROL, AssistantCommand::isMobileControlIntent,
                    (text, workingDirectory) -> mobile(naturalMobileCommand(text))),
            new NaturalIntent(WEIGHT_MOBILE_CODEGEN, AssistantCommand::isMobileCodegenIntent,
                    (text, workingDirectory) -> mobileCodegen(firstJsonLikePath(text))),
            new NaturalIntent(WEIGHT_DOCTOR, AssistantCommand::isDoctorIntent,
                    (text, workingDirectory) -> {
                        String path = firstPathLike(text);
                        return doctor(path.isBlank() ? "" : path, workingDirectory);
                    }));
    private static final List<CommandHint> CORE_HINTS =
            CORE_COMMANDS.stream().map(CommandDefinition::hint).toList();
    private static final List<CommandHint> EXPERT_HINTS =
            EXPERT_COMMANDS.stream().map(CommandDefinition::hint).toList();
    private static final List<CommandHint> ALL_HINTS = concat(CORE_HINTS, EXPERT_HINTS);
    // intent-keyword table (gated by tests/scripts/test_mcp_tool_catalog_sync.py)
    private static final Map<String, List<String>> INTENT_KEYWORDS = Map.ofEntries(
            Map.entry("shaft_coding_partner_plan", List.of(
                    "plan coding partner work for ",
                    "plan partner work for ",
                    "coding partner plan for ",
                    "partner plan for ",
                    "find reuse for ")),
            Map.entry("shaft_guide_search", List.of(
                    "search shaft docs ",
                    "search the shaft docs ",
                    "search shaft guide ",
                    "search the shaft guide ",
                    "find shaft docs ",
                    "find shaft guide ",
                    "guide me on ",
                    "docs for ")),
            Map.entry("test_automation_scenarios", List.of(
                    "find scenarios ",
                    "find automation scenarios ",
                    "show scenarios ",
                    "automation scenarios for ",
                    "scenario ideas for ")),
            Map.entry("test_code_guardrails_check", List.of(
                    "check generated java code ",
                    "check this java code ",
                    "run guardrails ",
                    "guardrails check ")),
            Map.entry("shaft_project_upgrade", List.of(
                    "preview shaft upgrade",
                    "preview upgrade",
                    "dry run shaft upgrade")),
            Map.entry("capture_start", List.of(
                    "browser",
                    "web flow",
                    "web journey",
                    "webdriver",
                    "http://",
                    "https://",
                    "my actions on",
                    "browser actions",
                    "actions on the site",
                    "actions on the page")),
            Map.entry("mobile_record_start", List.of(
                    "mobile",
                    "android",
                    "emulator",
                    "appium",
                    " app ")),
            Map.entry("mobile_record_stop", List.of(
                    "stop mobile recording",
                    "stop app recording")),
            Map.entry("mobile_recording_code_blocks", List.of(
                    "generate mobile code",
                    "generate appium code",
                    "create mobile code")),
            // Despite the map key, this list is really "any recognized mobile-topic action word":
            // isMobileControlIntent gates on it for every mobile() sub-action, not just toolchain
            // status, and naturalMobileCommand resolves the specific action afterward.
            Map.entry("mobile_toolchain_status", List.of(
                    "toolchain",
                    "appium",
                    "adb",
                    "emulator",
                    "sdk",
                    "inspector",
                    "accessibility tree",
                    "current mobile screen",
                    "mobile screen",
                    "contexts",
                    "context switch",
                    "switch context",
                    "screenshot",
                    "quit mobile",
                    "close mobile",
                    "rotate",
                    "landscape",
                    "portrait",
                    "hide keyboard",
                    "dismiss keyboard",
                    "background the app",
                    "send app to background",
                    "background app",
                    "activate app",
                    "foreground app",
                    "bring app to foreground")),
            Map.entry("doctor_analyze_failed_allure", List.of(
                    "run doctor",
                    "analyze allure",
                    "analyse allure",
                    "doctor ",
                    "diagnose my last run",
                    "diagnose the last run",
                    "diagnose the latest run",
                    "analyze the latest report",
                    "analyze latest report",
                    "why did my test fail",
                    "why did my tests fail")),
            // Session-scoped browser actions that need no URL to be unambiguous (issue #3678): each
            // phrase maps 1:1 to exactly one browser_* tool via naturalBrowserSessionCommand, so
            // isBrowserSessionActionIntent can skip isBrowserControlIntent's mentionsWebTarget gate.
            Map.entry("browser_session_action", List.of(
                    "resize",
                    "window size",
                    "clear cookies",
                    "clear all cookies",
                    "delete all cookies",
                    "delete cookie",
                    "aria snapshot",
                    "accessibility audit",
                    "network requests",
                    "network transactions",
                    "save session",
                    "save storage state",
                    "load session",
                    "load storage state")));
    // "start <mode> recording" phrasings match as a whole prefix (equal to the phrase, or the
    // phrase followed by more words) via matchesWholeWordPrefix, rather than the anywhere-in-text
    // keywords above -- kept out of the map above because they need different match semantics.
    private static final List<String> CAPTURE_START_PHRASES = List.of(
            "start browser recording",
            "start a browser recording",
            "start webdriver recording",
            "start a webdriver recording");
    private static final List<String> MOBILE_RECORD_START_PHRASES = List.of(
            "start mobile recording",
            "start app recording");
    private static final List<String> API_CAPTURE_START_PHRASES = List.of(
            "start api recording",
            "start an api recording",
            "start api capture",
            "start an api capture");


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
        return fromPrompt(prompt, selection, mode, workingDirectory, customCommand, allowSourceMutation,
                openFileContext, conversationContext, "");
    }

    /**
     * Real entry point for building an Assistant invocation from prompt text (issue #3727 adds
     * {@code attachmentsContext}: the pre-formatted {@link AssistantAttachments#outboundBlock} for
     * whatever the user attached via the panel's attach affordances -- current file, all open
     * files, a picked file, or an image path note). Folded into the outbound prompt text alongside
     * {@code conversationContext} by {@link #localAgentPrompt} and {@link #cloudPrompt}, the only
     * two call sites that carry a free-text "prompt" argument; direct MCP tool invocations (slash
     * commands, natural-language tool routing) carry no such field and attachments are not applied
     * to them (see {@link AssistantAttachments#outboundBlock} javadoc for the transport evidence).
     */
    static Invocation fromPrompt(
            String prompt,
            Selection selection,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation,
            OpenFileContext openFileContext,
            String conversationContext,
            String attachmentsContext) {
        String text = prompt == null ? "" : prompt.trim();
        if (text.isEmpty()) {
            return Invocation.local("Describe what you need in plain language — record a journey, "
                    + "generate a test, diagnose a failed run, or upgrade this project to SHAFT.");
        }
        String liveCodegen = liveCodegenSlashPrompt(text);
        boolean liveCodegenRequest = !liveCodegen.isBlank();
        boolean upgradeAgentRequest = false;
        if (!liveCodegen.isBlank()) {
            text = liveCodegen;
        } else if (isUpgradeSlashCommand(text) || isNaturalUpgradeIntent(text)) {
            // Upgrading is an action, not a recipe: the local agent performs the upgrade itself
            // (with source-edit approval), repairs what breaks, and reports what it changed —
            // instead of pasting a command back for the user to run manually (issue #3426 B6).
            String projectRoot = isUpgradeSlashCommand(text) ? afterFirstWord(text.trim()) : "";
            Invocation blocked = upgradeAgentGate(selection, mode, allowSourceMutation, projectRoot);
            if (blocked != null) {
                return blocked;
            }
            text = upgradeAgentPrompt(projectRoot);
            upgradeAgentRequest = true;
        } else if (text.startsWith("/")) {
            return slash(text, workingDirectory, openFileContext);
        }
        boolean codeGenerationRequest = !upgradeAgentRequest && isCodeGenerationRequest(text);
        // A code-generation request that already names a recording JSON converts deterministically:
        // the recording file is the source of truth and needs no live session and no agent CLI.
        if (codeGenerationRequest && !liveCodegenRequest && !firstJsonLikePath(text).isBlank()) {
            return generateTest(text);
        }
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
            return cloud(text, selection, mode, workingDirectory, openFileContext, conversationContext,
                    attachmentsContext);
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
                conversationContext, attachmentsContext);
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
            String conversationContext,
            String attachmentsContext) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("provider", selection.cloudProvider());
        arguments.addProperty("model", selection.cloudModel());
        arguments.addProperty("mode", mode);
        arguments.addProperty("prompt",
                withEffortHint(cloudPrompt(text, mode, openFileContext, conversationContext, attachmentsContext),
                        selection.effort()));
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", false);
        return Invocation.tool("autobot_provider_chat", arguments);
    }

    private static String cloudPrompt(String text, String mode, OpenFileContext openFileContext,
                                      String conversationContext, String attachmentsContext) {
        return withAttachments(cloudPrompt(text, mode, openFileContext, conversationContext), attachmentsContext);
    }

    private static String cloudPrompt(String text, String mode, OpenFileContext openFileContext,
                                      String conversationContext) {
        if (!isCodeGenerationRequest(text)) {
            return withConversationContext(SHAFT_OPTIONS_HINT + "\n\n" + text, conversationContext);
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
        return Invocation.local("Unknown command. Just describe what you need in plain language — "
                + "record a journey, generate a test, diagnose a failed run, or upgrade this project.");
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
        if (commandIs(action, "size", "resize", "windowsize")) {
            return Invocation.tool("browser_set_window_size", windowSize(remainder));
        }
        if (commandIs(action, "clearcookies")) {
            return Invocation.tool("browser_delete_all_cookies", new JsonObject());
        }
        if (commandIs(action, "deletecookie")) {
            return remainder.isBlank()
                    ? Invocation.local("Provide a cookie name, for example `/browser delete cookie sessionId`.")
                    : Invocation.tool("browser_delete_cookie", cookieName(remainder));
        }
        if (commandIs(action, "aria", "ariasnapshot")) {
            return Invocation.tool("browser_aria_snapshot", ariaSnapshotWholePage());
        }
        if (commandIs(action, "audit")) {
            return Invocation.tool("browser_accessibility_audit", wcagTags(remainder));
        }
        if (commandIs(action, "network")) {
            // "requests"/"transactions" is a label after the bare `/browser network` action word, not
            // part of the URL filter -- `/browser network requests example.com` must still filter by
            // "example.com", not treat the literal word "requests" as the filter.
            String filter = stripLeadingWord(stripLeadingWord(remainder, "requests"), "transactions");
            return Invocation.tool("browser_network_requests", networkRequestsFilter(filter));
        }
        if (commandIs(action, "save")) {
            // Accepts both `/browser save <path>` and the more conversational `/browser save
            // session <path>`; a leading "session" token is a label, not part of the path.
            return Invocation.tool("browser_storage_state_save",
                    storageStatePath(stripLeadingWord(remainder, "session"), DEFAULT_STORAGE_STATE_PATH));
        }
        if (commandIs(action, "load")) {
            String path = stripLeadingWord(remainder, "session");
            return path.isBlank()
                    ? Invocation.local("Provide a storage-state JSON path, for example `/browser load session " + DEFAULT_STORAGE_STATE_PATH + "`.")
                    : Invocation.tool("browser_storage_state_load", storageStatePath(path, DEFAULT_STORAGE_STATE_PATH));
        }
        // Phrase-form session actions ("clear cookies", "delete cookie x", "window size 1280x800",
        // "save session", ...) don't front-load one of the single-word action tokens above, so they
        // fall through here: rewrite to the canonical verb-first form and recurse once. Placed last
        // so it never pre-empts the open/navigate/screenshot branches above it, which is what keeps
        // a URL containing a coincidental substring like "resize" (matched by
        // isBrowserSessionActionPhrase) from being misrouted -- open/navigate/etc already returned
        // by the time this runs.
        if (isBrowserSessionActionPhrase(trimmed)) {
            String canonical = naturalBrowserSessionCommand(trimmed);
            if (!canonical.equalsIgnoreCase(trimmed)) {
                return browser(canonical);
            }
        }
        return webdriverOpen(trimmed);
    }

    /**
     * True for browser session actions (window size, cookies, aria snapshot, accessibility audit,
     * network transactions, storage state) that -- unlike open/navigate/screenshot -- need no URL to
     * be unambiguous. Lets {@link #isBrowserControlIntent} route these without its usual
     * mentionsWebTarget requirement (issue #3678). Requires the word "browser" so a bare "clear
     * cookies" with no browser context stays with the agent.
     */
    private static boolean isBrowserSessionActionIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.contains("browser") && isBrowserSessionActionPhrase(normalized);
    }

    /**
     * The keyword half of {@link #isBrowserSessionActionIntent}, without the "browser" requirement --
     * used inside {@link #browser} itself, where the /browser or "browser" context is already
     * established by the caller, to recognize phrase-form input like "clear cookies" or "delete
     * cookie sessionId" that {@link #commandIs}'s single-word action vocabulary cannot match directly.
     */
    private static boolean isBrowserSessionActionPhrase(String text) {
        return containsAny(normalizeNaturalCommand(text), INTENT_KEYWORDS.get("browser_session_action"));
    }

    /**
     * Rewrites a free-language browser-session sentence into the verb-first form {@link #browser}
     * expects (mirrors {@link #naturalMobileCommand} for the mobile topic). Both call sites
     * ({@link #browser}'s phrase fallback and the top-level {@link #isBrowserSessionActionIntent}
     * dispatch) only invoke this after {@link #isBrowserSessionActionPhrase} has already matched one
     * of the same keywords branched on below, so the final fallback return is unreachable in
     * practice.
     */
    private static String naturalBrowserSessionCommand(String text) {
        String normalized = normalizeNaturalCommand(text);
        for (NaturalSessionCommandRule rule : BROWSER_SESSION_COMMAND_RULES) {
            if (rule.matchesNormalized().test(normalized)) {
                return rule.commandFor().apply(text);
            }
        }
        return text(text);
    }

    private static JsonObject windowSize(String rest) {
        int[] widthHeight = parseWidthHeight(rest);
        JsonObject arguments = new JsonObject();
        arguments.addProperty("width", widthHeight[0]);
        arguments.addProperty("height", widthHeight[1]);
        return arguments;
    }

    // Returns already-validated ints (never re-parsed by the caller) so windowSize cannot hit an
    // uncaught NumberFormatException: both the parsed and the DEFAULT_WINDOW_SIZE fallback paths
    // resolve to ints before returning.
    private static int[] parseWidthHeight(String rest) {
        // Scans every token rather than just the first: natural phrasing like "resize the browser
        // window to 1920x1080" puts the WxH token well after the action word.
        String[] parts = firstSizeLikeToken(rest).toLowerCase(Locale.ROOT).split("x");
        if (parts.length == 2) {
            try {
                return new int[]{Integer.parseInt(parts[0].trim()), Integer.parseInt(parts[1].trim())};
            } catch (NumberFormatException notNumeric) {
                // fall through to the default size below
            }
        }
        String[] fallback = DEFAULT_WINDOW_SIZE.split("x");
        if (fallback.length == 2) {
            try {
                return new int[]{Integer.parseInt(fallback[0]), Integer.parseInt(fallback[1])};
            } catch (NumberFormatException notNumeric) {
                // fall through to the hardcoded literal below
            }
        }
        // Only reachable if DEFAULT_WINDOW_SIZE itself were ever edited to a malformed literal.
        return new int[]{1280, 800};
    }

    private static JsonObject cookieName(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("cookieName", firstTokenOrDefault(rest, ""));
        return arguments;
    }

    /**
     * browser_aria_snapshot's locatorStrategy is intentionally left unset: the tool's own contract
     * treats a missing locatorStrategy plus a blank locatorValue as "snapshot the whole page", which
     * is the only case reachable from phrasing alone (a specific element still needs a resolved
     * locator, like the element_* tools -- see issue #3678 classification).
     */
    private static JsonObject ariaSnapshotWholePage() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("locatorValue", "");
        return arguments;
    }

    private static JsonObject wcagTags(String rest) {
        JsonObject arguments = new JsonObject();
        JsonArray tags = new JsonArray();
        String trimmed = text(rest);
        if (!trimmed.isBlank()) {
            for (String token : trimmed.split("\\s+")) {
                tags.add(token);
            }
        }
        arguments.add("wcagTags", tags);
        return arguments;
    }

    private static JsonObject networkRequestsFilter(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("urlFilter", text(rest));
        arguments.addProperty("limit", 50);
        return arguments;
    }

    private static JsonObject storageStatePath(String rest, String fallback) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("filePath", firstTokenOrDefault(rest, fallback));
        return arguments;
    }

    private static String firstSizeLikeToken(String rest) {
        if (rest == null || rest.isBlank()) {
            return DEFAULT_WINDOW_SIZE;
        }
        for (String token : rest.split("\\s+")) {
            if (token.toLowerCase(Locale.ROOT).matches("\\d+x\\d+")) {
                return token;
            }
        }
        return DEFAULT_WINDOW_SIZE;
    }

    private static String firstTokenAfterWord(String rest, String word) {
        if (rest == null || rest.isBlank()) {
            return "";
        }
        String[] tokens = rest.trim().split("\\s+");
        for (int i = 0; i < tokens.length - 1; i++) {
            if (tokens[i].equalsIgnoreCase(word)) {
                return tokens[i + 1];
            }
        }
        return "";
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
        if (commandIs(action, "refresh", "reload")) {
            return Invocation.tool("playwright_browser_refresh", new JsonObject());
        }
        if (commandIs(action, "back")) {
            return Invocation.tool("playwright_browser_navigate_back", new JsonObject());
        }
        if (commandIs(action, "forward")) {
            return Invocation.tool("playwright_browser_navigate_forward", new JsonObject());
        }
        if (commandIs(action, "size", "resize", "windowsize")) {
            return Invocation.tool("playwright_browser_set_window_size", windowSize(remainder));
        }
        if (commandIs(action, "newtab")) {
            return Invocation.tool("playwright_browser_new_window", newWindow(remainder, "TAB"));
        }
        if (commandIs(action, "newwindow")) {
            return Invocation.tool("playwright_browser_new_window", newWindow(remainder, "WINDOW"));
        }
        return Invocation.local("Unknown Playwright browser command. Use `/browser playwright open <url>`.");
    }

    private static JsonObject newWindow(String rest, String windowType) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", text(rest));
        arguments.addProperty("windowType", windowType);
        return arguments;
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
        if (commandIs(action, "rotate")) {
            return Invocation.tool("mobile_rotate", orientation(remainder));
        }
        if (commandIs(action, "hide") && commandIs(firstWord(remainder).toLowerCase(Locale.ROOT), "keyboard")) {
            return Invocation.tool("mobile_hide_keyboard", new JsonObject());
        }
        if (commandIs(action, "background")) {
            return Invocation.tool("mobile_background_app", backgroundSeconds(remainder));
        }
        if (commandIs(action, "activate")) {
            return remainder.isBlank()
                    ? Invocation.local("Provide an app id, for example `/mobile activate com.example.app`.")
                    : Invocation.tool("mobile_activate_app", appId(remainder));
        }
        if (commandIs(action, "quit", "close", "stop")) {
            return Invocation.tool("driver_quit", new JsonObject());
        }
        return Invocation.local("Unknown mobile command. Use `/mobile status`, `/mobile native Android <device>`, `/mobile web <url>`, `/mobile tree`, `/mobile contexts`, `/mobile switch <context>`, `/mobile screenshot <path>`, or `/mobile quit`.");
    }

    private static JsonObject orientation(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("orientation", "LANDSCAPE".equalsIgnoreCase(firstWord(rest)) ? "LANDSCAPE" : "PORTRAIT");
        return arguments;
    }

    private static JsonObject backgroundSeconds(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("seconds", parseIntOrDefault(firstWord(rest), 5));
        return arguments;
    }

    private static int parseIntOrDefault(String value, int fallback) {
        try {
            return Integer.parseInt(text(value));
        } catch (NumberFormatException notNumeric) {
            return fallback;
        }
    }

    private static JsonObject appId(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("appId", firstTokenOrDefault(rest, ""));
        return arguments;
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
        return Invocation.local("Tell me to start or stop a mobile recording, or to open the mobile "
                + "inspector on the Android emulator — for example: \"Record my mobile actions on the "
                + "Android emulator\".");
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
        String allurePath = trimmed.isBlank() ? "" : firstTokenOrDefault(trimmed, "");
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
     * Plain-language upgrade requests ("upgrade this project to the latest SHAFT") get the same
     * agent-performed upgrade as the legacy slash form: the user asks, the agent does it.
     */
    private static boolean isNaturalUpgradeIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return (normalized.startsWith("upgrade ")
                && containsAny(normalized, "shaft", "this project", "my project", "the project"))
                || containsAny(normalized,
                        "upgrade this project", "upgrade my project", "upgrade the project",
                        "upgrade to shaft", "upgrade shaft", "migrate to shaft");
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
                    **Upgrading changes your `pom.xml` (and possibly imports), so it needs your explicit permission first.**

                    1. Switch the mode selector to **Agent**.
                    2. Tick **Allow source edits**.
                    3. Send the upgrade request again. The agent will then preview the upgrade, apply it, verify the project still compiles, and report every change it made and why.

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

    /**
     * Starts a browser-driven API/network-traffic recording via {@code capture_api_start}
     * (issue #3726), the same MCP tool {@code RecordApiWebAction}'s dedicated API Recording tab
     * drives, so the chat-panel command and the toolbar action produce the same recorded session
     * shape and can both feed {@code capture_api_generate} for SHAFT.API/RestActions code.
     */
    private static Invocation recordApi(String rest) {
        return Invocation.tool("capture_api_start", apiCaptureStart(rest));
    }

    /**
     * Starts RecordApiMobileAction's no-browser API/network-traffic recording (issue #3738) via
     * {@code mobile_api_record_start}, the loopback MITM proxy that captures native mobile API
     * traffic without launching a browser. Mirrors the argument shape RecordApiMobileAction's
     * Advanced-UI path builds directly: platform, plus blank deviceLabel/outputPath left for the
     * user to fill in via the API Recording tab.
     */
    private static Invocation recordApiMobile(String text) {
        return Invocation.tool("mobile_api_record_start", mobileApiRecordStart(text));
    }

    private static JsonObject mobileApiRecordStart(String text) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("platform", platformName(mobileApiRecordingPlatform(text)));
        arguments.addProperty("deviceLabel", "");
        arguments.addProperty("outputPath", "");
        return arguments;
    }

    /**
     * Extracts the platform word following " on " in RecordApiMobileAction's prefill (for example
     * "Record API traffic without a browser on Android"). Returns blank when no such trailing
     * platform is present, letting {@link #platformName(String)} default it to Android.
     */
    private static String mobileApiRecordingPlatform(String text) {
        String trimmed = text(text);
        int index = trimmed.toLowerCase(Locale.ROOT).lastIndexOf(" on ");
        return index < 0 ? "" : firstWord(trimmed.substring(index + 4));
    }

    private static JsonObject apiCaptureStart(String rest) {
        JsonObject arguments = new JsonObject();
        String targetUrl = parseLeadingUrl(rest);
        // A blank targetUrl is intentionally passed through, matching captureStart: no target
        // parsed means capture_api_start resolves it to an empty/blank-page browser downstream.
        arguments.addProperty("targetUrl", targetUrl);
        arguments.addProperty("browser", "Chrome");
        arguments.addProperty("headless", recorderHeadlessPreference());
        arguments.add("networkOptions", apiNetworkCaptureOptions());
        return arguments;
    }

    /**
     * Field names must match {@code com.shaft.capture.runtime.NetworkCaptureOptions} exactly --
     * mirrors {@code RecordApiWebAction.NETWORK_CAPTURE_OPTIONS_FIELDS}' warning that the MCP
     * binder silently drops an unrecognized key instead of rejecting it. Request/response body
     * capture (the entire point of an API recording) defaults to false unless enabled here.
     */
    private static JsonObject apiNetworkCaptureOptions() {
        JsonObject networkOptions = new JsonObject();
        networkOptions.addProperty("enabled", true);
        networkOptions.addProperty("excludeAssets", true);
        networkOptions.addProperty("captureRequestBodies", true);
        networkOptions.addProperty("captureResponseBodies", true);
        return networkOptions;
    }

    private static Invocation recording(String text) {
        if (isReRecord(text)) {
            return reRecord(text);
        }
        if (isStartRecording(text)) {
            return record(text.replaceFirst("(?i)^start\\s+(a\\s+)?(web(driver)?\\s+)?(capture|recording|recorder)\\b", "").trim());
        }
        if (isRecordScenarioIntent(text)) {
            return record("");
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
        if (isCaptureStatusIntent(text)) {
            return Invocation.tool("capture_status", new JsonObject());
        }
        return null;
    }

    private static boolean isCaptureStatusIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("recording status")
                || normalized.equals("capture status")
                || normalized.equals("check recording status")
                || normalized.equals("what is the recording status")
                || normalized.equals("is a recording active")
                || normalized.equals("is recording active");
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

    /**
     * Scores every natural-language intent that matches {@code text} and dispatches the
     * highest-weight match (see the {@code WEIGHT_*} constants above {@link #NATURAL_INTENTS}).
     * The loop only replaces {@code best} on a strictly greater weight, so ties resolve to
     * whichever matching intent is declared first in {@link #NATURAL_INTENTS} -- the same
     * first-match order the old linear scan used.
     */
    private static Invocation directIntent(String text, String workingDirectory) {
        NaturalIntent best = null;
        for (NaturalIntent intent : NATURAL_INTENTS) {
            if (intent.matches(text) && (best == null || intent.weight() > best.weight())) {
                best = intent;
            }
        }
        return best == null ? null : best.invoke(text, workingDirectory);
    }


    /**
     * Intentionally broad: "command" plus a leading question word is a low-cost signal (command
     * help never mutates anything or calls an MCP tool), so a rare false-positive match is cheaper
     * than missing a real "what commands can I use" style question. Left as-is rather than
     * narrowed.
     */
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
        return startsWithAny(normalized, INTENT_KEYWORDS.get("shaft_coding_partner_plan"))
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
        return startsWithAny(normalized, INTENT_KEYWORDS.get("shaft_guide_search"))
                && !naturalQuery(text).isBlank();
    }

    private static boolean isScenarioSearchIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return startsWithAny(normalized, INTENT_KEYWORDS.get("test_automation_scenarios"))
                && !naturalQuery(text).isBlank();
    }

    private static boolean isGuardrailsCheckIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return startsWithAny(normalized, INTENT_KEYWORDS.get("test_code_guardrails_check"))
                && !naturalCode(text).isBlank();
    }

    private static boolean isProjectCreateIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.startsWith("create shaft project ")
                || normalized.startsWith("create a shaft project ")
                || normalized.startsWith("new shaft project ")
                || normalized.startsWith("scaffold shaft project ");
    }

    /**
     * "upgrade shaft project"/"upgrade this shaft project" used to be handled here too, but both
     * are dead: {@code isNaturalUpgradeIntent} in {@code fromPrompt} intercepts that phrasing
     * before {@code directIntent} (and this predicate) is ever consulted, routing it to the
     * agent-performed upgrade instead (issue #3426 B6). Removed rather than left as unreachable
     * branches.
     */
    private static boolean isProjectUpgradeIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return startsWithAny(normalized, INTENT_KEYWORDS.get("shaft_project_upgrade"));
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

    private static boolean containsAny(String text, List<String> needles) {
        return needles != null && containsAny(text, needles.toArray(new String[0]));
    }

    private static boolean startsWithAny(String text, List<String> prefixes) {
        if (text == null || prefixes == null) {
            return false;
        }
        for (String prefix : prefixes) {
            if (prefix != null && text.startsWith(prefix)) {
                return true;
            }
        }
        return false;
    }

    /**
     * True when {@code normalized} equals one of {@code phrases}, or starts with one of them
     * followed by a word boundary (a space). Used for "start &lt;mode&gt; recording" phrasings,
     * where a plain {@code startsWith} would also match unrelated longer words glued onto the
     * phrase (e.g. "start browser recordingroom").
     */
    private static boolean matchesWholeWordPrefix(String normalized, List<String> phrases) {
        if (normalized == null || phrases == null) {
            return false;
        }
        for (String phrase : phrases) {
            if (normalized.equals(phrase) || normalized.startsWith(phrase + " ")) {
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
        // Session actions (resize, cookies, aria snapshot, accessibility audit, network requests,
        // storage state) target the current session, not a URL, so they skip mentionsWebTarget
        // below entirely -- see isBrowserSessionActionIntent (issue #3678).
        if (isBrowserSessionActionIntent(lower)) {
            return true;
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
        if (matchesWholeWordPrefix(normalized, CAPTURE_START_PHRASES)) {
            return true;
        }
        // Plain-language "record ..." requests must run on the plugin's long-lived MCP process:
        // a recording started by a one-shot local agent turn dies with that turn's MCP child
        // process seconds later (issue #3429). This must cover at least the Assistant's own
        // suggestion prefill ("Record my browser actions on https://").
        if (!normalized.startsWith("record ")) {
            return false;
        }
        return containsAny(normalized, INTENT_KEYWORDS.get("capture_start"));
    }

    /**
     * True for requests to start a browser-driven API/network-traffic recording (issue #3726),
     * for example RecordApiWebAction's exact Assistant prefill ("Record API traffic on
     * https://..."). Deliberately excludes "without a browser" phrasing up front: that marks
     * RecordApiMobileAction's no-browser proxy prefill, which drives a different MCP tool
     * ({@code mobile_api_record_start}, via {@link #isMobileApiRecordingIntent}) that this
     * command does not.
     */
    private static boolean isApiRecordingIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        if (normalized.contains("without a browser")) {
            return false;
        }
        if (matchesWholeWordPrefix(normalized, API_CAPTURE_START_PHRASES)) {
            return true;
        }
        return normalized.startsWith("record ") && normalized.contains("api");
    }

    private static boolean isMobileControlIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.contains("mobile")
                && containsAny(normalized, INTENT_KEYWORDS.get("mobile_toolchain_status"));
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
        if (containsAny(normalized, "rotate", "landscape", "portrait")) {
            return "rotate " + (normalized.contains("landscape") ? "LANDSCAPE" : "PORTRAIT");
        }
        if (containsAny(normalized, "hide keyboard", "dismiss keyboard")) {
            return "hide keyboard";
        }
        if (containsAny(normalized, "background the app", "send app to background", "background app")) {
            return "background";
        }
        if (containsAny(normalized, "activate app", "foreground app", "bring app to foreground")) {
            String appId = firstAppIdLikeToken(text);
            // No package/bundle id found in the sentence: fall through to status rather than guess
            // one, since mobile_activate_app has a real side effect (issue #3678).
            return appId.isBlank() ? "status " + (normalized.contains("ios") ? "iOS" : "Android") : "activate " + appId;
        }
        return "status " + (normalized.contains("ios") ? "iOS" : "Android");
    }

    /**
     * Scans for an Android package or iOS bundle id shape (dot-separated identifier segments), for
     * example {@code com.example.app}. Used only to resolve mobile_activate_app from natural
     * language; returns blank when no such token is present.
     */
    private static String firstAppIdLikeToken(String rest) {
        if (rest == null || rest.isBlank()) {
            return "";
        }
        for (String token : rest.split("\\s+")) {
            if (token.matches("[A-Za-z][A-Za-z0-9_]*(\\.[A-Za-z0-9_]+)+")) {
                return token;
            }
        }
        return "";
    }

    /**
     * True for RecordApiMobileAction's no-browser proxy prefill (issue #3738), for example
     * "Record API traffic without a browser on Android" -- the loopback MITM proxy captures
     * native mobile API traffic directly, so this must resolve to {@code mobile_api_record_start}
     * rather than {@code isApiRecordingIntent}'s browser-driven {@code capture_api_start} or
     * {@code isMobileRecordingStartIntent}'s UI-driven {@code mobile_record_start}. Requires both
     * "without a browser" and "api" wording so plain browser-API phrases (no "without a browser")
     * and plain mobile-recording phrases (no "api") still resolve to their own intents.
     */
    private static boolean isMobileApiRecordingIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.startsWith("record ")
                && normalized.contains("api")
                && normalized.contains("without a browser");
    }

    private static boolean isMobileRecordingStartIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        if (matchesWholeWordPrefix(normalized, MOBILE_RECORD_START_PHRASES)) {
            return true;
        }
        // Same one-shot-agent hazard as browser recordings (issue #3429): plain-language mobile
        // recording requests — including the Assistant's own "Record my mobile actions on the
        // Android emulator" prefill — must run on the plugin's long-lived MCP process. Mobile
        // recording outweighs browser recording in NATURAL_INTENTS (WEIGHT_MOBILE_RECORDING >
        // WEIGHT_BROWSER_RECORDING), so phrases mentioning both, e.g. "record my actions on the
        // Android emulator", resolve here without isBrowserRecordingIntent needing a
        // cross-predicate exclusion.
        return normalized.startsWith("record ")
                && containsAny(normalized, INTENT_KEYWORDS.get("mobile_record_start"));
    }

    private static boolean isMobileRecordingStopIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return INTENT_KEYWORDS.get("mobile_record_stop").contains(normalized);
    }

    private static boolean isMobileCodegenIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return startsWithAny(normalized, INTENT_KEYWORDS.get("mobile_recording_code_blocks"))
                && !firstJsonLikePath(text).isBlank();
    }

    // Report-triage phrasings route to the same auto-discovering analyzer: with no path in the
    // text, the server analyzes the newest allure-results or single-file AllureReport.html in the
    // workspace.
    private static boolean isDoctorIntent(String text) {
        String normalized = normalizeNaturalCommand(text);
        return startsWithAny(normalized, INTENT_KEYWORDS.get("doctor_analyze_failed_allure"));
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
        return triage(workingDirectory, "");
    }

    private static JsonObject triage(String workingDirectory, String allurePath) {
        JsonObject arguments = new JsonObject();
        // An empty path list makes the MCP server analyze the most recent populated
        // allure-results directory in the workspace, so "diagnose my last run" needs no path.
        JsonArray allureResultPaths = new JsonArray();
        if (allurePath != null && !allurePath.isBlank()) {
            allureResultPaths.add(allurePath);
        }
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
            // Replay proves the recorded locators against the active mobile session before the code
            // blocks are returned; the server rejects redacted recordings with a clear error.
            return Invocation.tool("mobile_replay_recording", mobileRecordingPath(recordingPath));
        }
        if (lower.contains("playwright") || recordingPath.toLowerCase(Locale.ROOT).contains("playwright")) {
            // Playwright recorder sessions use their own recording schema (recordingPath), which
            // the Capture-session replay tools cannot consume; replay through the Playwright-native
            // replay tool (after initializing its driver) so returned locators are proven live.
            return Invocation.sequence(List.of(
                    new ToolCall("playwright_initialize", playwrightInitialize()),
                    new ToolCall("playwright_replay_recording", generatePlaywrightCodeFromRecording(recordingPath))));
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
        // A blank targetUrl is intentionally passed through: no target parsed means capture_start
        // resolves it to an empty/blank-page browser downstream, not a hardcoded default site.
        arguments.addProperty("targetUrl", targetUrl);
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

    private static boolean isRecordScenarioIntent(String text) {
        // Issue #3673: deliberately an exact-match set, not a "record "-prefix or startsWith
        // match -- a broader match would risk misrouting other "record "-adjacent phrases (e.g.
        // "record a video of the sprint demo", which must keep going to the LLM agent).
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("record a scenario")
                || normalized.equals("record a new scenario")
                || normalized.equals("record scenario")
                || normalized.equals("record new scenario");
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
            String conversationContext,
            String attachmentsContext) {
        String lower = text.toLowerCase(Locale.ROOT);
        String normalizedMode = Selection.normalize(mode, "ASK");
        boolean agentMode = "AGENT".equals(normalizedMode);
        boolean codeGenerationRequest = isCodeGenerationRequest(text);
        String hint = SHAFT_MCP_USAGE_HINT
                + (codeGenerationRequest
                ? "\n" + codeGenerationGuidance(text) + "\n" + codeRequestScope(normalizedMode, openFileContext)
                : "");
        // The user already mentioning "shaft-mcp" themselves makes the "use shaft-mcp for browser
        // tasks" boilerplate redundant, but the shaft-options hint is unrelated to that -- it must
        // always be sent, or every clarifying question on a turn that happens to mention shaft-mcp
        // silently loses its clickable-options chip UI (a real user report).
        String withHint = lower.contains("shaft-mcp")
                ? (codeGenerationRequest
                ? codeGenerationGuidance(text) + "\n" + codeRequestScope(normalizedMode, openFileContext)
                + "\n" + SHAFT_OPTIONS_HINT + "\n\n" + text
                : SHAFT_OPTIONS_HINT + "\n\n" + text)
                : hint + "\n\n" + text;
        withHint = withConversationContext(withHint, conversationContext);
        withHint = withAttachments(withHint, attachmentsContext);
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

    /**
     * Appends the {@link AssistantAttachments#outboundBlock} (issue #3727) to an already-assembled
     * prompt, mirroring {@link #withConversationContext}'s no-op-when-blank shape. Applied after
     * conversation context so attachment content always reads as the most recent addition.
     */
    private static String withAttachments(String prompt, String attachmentsContext) {
        String block = text(attachmentsContext);
        if (block.isBlank()) {
            return prompt;
        }
        return prompt + "\n\n" + block;
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

    /**
     * Recognizes a scenario described in prose -- e.g. the onboarding quick action's "Record a
     * sample web flow on a practice page, add one assertion, and generate a reviewed test." -- as
     * distinct from a bare "start recording"/"record a scenario" trigger (issue #3692 item 3).
     *
     * <p>Without this carve-out, such prose is misrouted: {@link #isBrowserRecordingIntent}'s
     * keyword list (which includes "web flow", "browser", "http://", ...) matches these
     * descriptions too, since a scenario naturally mentions a URL or "web flow", so it was being
     * deterministically short-circuited into a bare {@code capture_start} that opens a browser and
     * silently drops the rest of the description (the assertion, the generated test). Routing this
     * intent through {@link #isCodeGenerationRequest} instead sends it to the underlying agent with
     * {@link #SHAFT_CODEGEN_TOOL_GUIDANCE}'s full record -&gt; act -&gt; stop -&gt; codegen -&gt;
     * self-heal pipeline, matching what the description actually asked for.
     *
     * <p>{@link #isStartRecording}/{@link #isRecordScenarioIntent}/{@link #isReRecord} are
     * excluded so the narrow, deliberately exact-match deterministic triggers they own (issue
     * #3673) are unaffected.
     *
     * <p>Package-private (not {@code private}) so {@code ShaftAssistantPanel} can reuse this exact
     * detection as the trigger for its pre-run orchestration-stage announcement (issue #3704),
     * without reimplementing it.
     *
     * @param text the natural-language prompt
     * @return whether the text describes a scenario to record and turn into code, not just a bare
     *         recording trigger
     */
    static boolean isScenarioDescriptionIntent(String text) {
        if (isStartRecording(text) || isRecordScenarioIntent(text) || isReRecord(text)) {
            return false;
        }
        if (!firstJsonLikePath(text).isBlank()) {
            // A recording (or other JSON) path is already named: the existing deterministic routes
            // (isMobileCodegenIntent, the "recording file is the source of truth" branch in
            // fromPrompt, ...) already handle that correctly. This predicate is only for scenarios
            // described without one yet -- and must not match on "recordings/x.json" substrings.
            return false;
        }
        String normalized = normalizeNaturalCommand(text);
        boolean mentionsRecordingVerb = normalized.startsWith("record ") || normalized.contains(" record ")
                || normalized.startsWith("recording ") || normalized.contains(" recording ")
                || normalized.startsWith("capture ") || normalized.contains(" capture ");
        if (!mentionsRecordingVerb) {
            return false;
        }
        return containsAny(normalized,
                "assert", "assertion", "generate", "generated", "codegen", "code gen",
                "reviewed test", "page object");
    }

    private static boolean isCodeGenerationRequest(String text) {
        String normalized = normalizeNaturalCommand(text);
        if (isScenarioDescriptionIntent(text)) {
            return true;
        }
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

    private static String stripLeadingWord(String rest, String word) {
        String trimmedRest = text(rest);
        return word.equalsIgnoreCase(firstWord(trimmedRest)) ? afterFirstWord(trimmedRest) : trimmedRest;
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

    private record NaturalIntent(int weight, NaturalIntentMatcher matcher, NaturalIntentBuilder builder) {
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
