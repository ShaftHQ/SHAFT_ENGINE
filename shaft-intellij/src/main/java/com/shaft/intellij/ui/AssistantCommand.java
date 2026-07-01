package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.intellij.mcp.ShaftCommandLine;

import java.util.List;
import java.util.Locale;

/**
 * Builds SHAFT Assistant MCP invocations from prompt text.
 */
final class AssistantCommand {
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    private static final int DEFAULT_BROWSER_MAX_CHARACTERS = 12_000;
    private static final int DEFAULT_BROWSER_MAX_ELEMENTS = 10;
    static final String DEFAULT_CAPTURE_TARGET_URL = "https://duckduckgo.com/";
    static final String DEFAULT_CAPTURE_RECORDING_PATH = "recordings/intellij-capture.json";
    static final String DEFAULT_CAPTURE_RECORDING_PATH_PREFIX = "recordings/intellij-capture-";
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
    private static final String SHAFT_MCP_USAGE_HINT =
            """
                    If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.
                    For WebDriver browser tasks, call driver_initialize before browser_* tools; do not use Playwright unless requested.
                    For repeated search-result anchors, scope the locator to the first result container; for DuckDuckGo use `(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']`.
                    """.stripIndent().trim();
    private static final String HELP = """
            Slash commands:
            /guide <query> - Search the SHAFT guide.
            /scenarios <intent> - Find automation scenarios.
            /browser <open|dom|screenshot|title|quit> - Control WebDriver browser tools.
            /record [playwright] - Start a WebDriver or Playwright recording session.
            /codegen [webdriver|playwright|mobile] <recording.json> - Generate replay snippets.
            /mobile <status|native|web|tree|screenshot> - Control mobile/Appium tools.
            /mobile-record <start|stop|inspector> - Record mobile actions or prepare Appium Inspector.
            /mobile-codegen <recording.json> - Generate mobile replay snippets.
            /doctor [playwright] [allurePath] - Analyze failures and recommend fixes.
            /inspect [url] [intent] - Open intent-driven inspection context.
            /locator [url] [intent] - Alias for /inspect.
            /guardrails <code> - Check generated Java code.
            /triage - Analyze failed Allure evidence.
            /fixTestFailure - Analyze failed Allure evidence and propose repair options.
            /clients - List local assistant clients.
            /generateTest [intent|recordingPath] - Suggest scenarios, or generate code from recording.
            /mcp <toolName> [json] - Run a raw MCP tool call.
            /help - Show this help.
            """.stripIndent().trim();
    private static final List<CommandHint> COMMAND_HINTS = List.of(
            new CommandHint("/browser", "Control browser sessions",
                    List.of("/web", "/browse", "/page"),
                    "/browser open https://example.com sign in"),
            new CommandHint("/record", "Record browser actions",
                    List.of("/rec", "/capture"),
                    "/record playwright"),
            new CommandHint("/codegen", "Generate code from recordings",
                    List.of("/generate", "/gen", "/generateTest"),
                    "/codegen mobile recordings/mobile.json"),
            new CommandHint("/mobile", "Control Appium and mobile web",
                    List.of("/appium", "/device", "/phone", "/emulator"),
                    "/mobile native Android Pixel_6"),
            new CommandHint("/mobile-record", "Record mobile actions",
                    List.of("/app-record", "/inspector-record"),
                    "/mobile-record inspector Android recordings/inspector.json"),
            new CommandHint("/doctor", "Analyze failure evidence",
                    List.of("/allure", "/triage", "/fixTestFailure"),
                    "/doctor target/allure-results"),
            new CommandHint("/project", "Create or upgrade SHAFT projects",
                    List.of("/newshaft", "/upgrade"),
                    "/project upgrade ."),
            new CommandHint("/mcp", "Run an explicit MCP tool",
                    List.of("/tool", "/call"),
                    "/mcp browser_get_title {}"));

    private AssistantCommand() {
        throw new IllegalStateException("Utility class");
    }

    static List<CommandHint> commandHints() {
        return COMMAND_HINTS;
    }

    static String commandTooltip() {
        StringBuilder tooltip = new StringBuilder("<html><b>SHAFT commands</b>");
        for (CommandHint hint : COMMAND_HINTS) {
            tooltip.append("<br><code>")
                    .append(hint.canonical())
                    .append("</code> - ")
                    .append(hint.summary())
                    .append("<br>&nbsp;&nbsp;Aliases: ")
                    .append(String.join(", ", hint.synonyms()))
                    .append("<br>&nbsp;&nbsp;Example: ")
                    .append(hint.example());
        }
        return tooltip.append("</html>").toString();
    }

    static Invocation fromPrompt(
            String prompt,
            String client,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation) {
        return fromPrompt(prompt, Selection.fromClient(client), mode, workingDirectory, customCommand, allowSourceMutation);
    }

    static Invocation fromPrompt(
            String prompt,
            Selection selection,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation) {
        String text = prompt == null ? "" : prompt.trim();
        if (text.isEmpty()) {
            return Invocation.local("Enter a prompt or slash command.");
        }
        if (text.startsWith("/")) {
            return slash(text, workingDirectory);
        }
        Invocation recording = recording(text);
        if (recording != null) {
            return recording;
        }
        Invocation directIntent = directIntent(text, workingDirectory);
        if (directIntent != null) {
            return directIntent;
        }
        if (selection.cloud()) {
            return cloud(text, selection, mode, workingDirectory);
        }
        if (!"CLI".equals(selection.runtime())) {
            return Invocation.local("SHAFT is configured for " + selection.displayName()
                    + ". Configure SHAFT MCP for that runtime, then send this prompt from its native chat panel.");
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", selection.client());
        arguments.addProperty("mode", mode);
        arguments.addProperty("prompt", localAgentPrompt(text, "AGENT".equals(mode), allowSourceMutation));
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.add("command", commandArray(customCommand));
        arguments.add("environment", new JsonObject());
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", "AGENT".equals(mode) && allowSourceMutation);
        return Invocation.tool("autobot_local_agent_run", arguments);
    }

    private static Invocation cloud(String text, Selection selection, String mode, String workingDirectory) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("provider", selection.cloudProvider());
        arguments.addProperty("model", selection.cloudModel());
        arguments.addProperty("mode", mode);
        arguments.addProperty("prompt", text);
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", false);
        return Invocation.tool("autobot_provider_chat", arguments);
    }

    private static Invocation slash(String text, String workingDirectory) {
        String[] parts = text.split("\\s+", 2);
        String command = parts[0].toLowerCase(Locale.ROOT);
        String rest = parts.length == 2 ? parts[1].trim() : "";
        return switch (command) {
            case "/guide", "/docs", "/manual" -> Invocation.tool("shaft_guide_search", guide(rest));
            case "/scenarios", "/scenario", "/ideas" -> Invocation.tool("test_automation_scenarios", scenarios(rest));
            case "/browser", "/web", "/browse", "/page" -> browser(rest);
            case "/record", "/rec", "/capture" -> record(rest);
            case "/codegen", "/generate", "/gen", "/generatetest" -> generateTest(rest);
            case "/mobile", "/appium", "/device", "/phone", "/emulator" -> mobile(rest);
            case "/mobile-record", "/app-record", "/inspector-record" -> mobileRecord(rest);
            case "/mobile-codegen", "/app-codegen" -> mobileCodegen(rest);
            case "/mobile-replay", "/app-replay" -> mobileReplay(rest);
            case "/doctor", "/allure", "/failure", "/fix" -> doctor(rest, workingDirectory);
            case "/inspect", "/locator" -> Invocation.tool("browser_open_intent", inspect(rest));
            case "/guardrails", "/checkcode" -> Invocation.tool("test_code_guardrails_check", guardrails(rest));
            case "/triage", "/fixtestfailure" -> Invocation.tool(
                    "doctor_analyze_failed_allure", triage(workingDirectory));
            case "/clients", "/client" -> Invocation.tool("autobot_local_agent_clients", new JsonObject());
            case "/project", "/newshaft", "/upgrade" -> project(command, rest, workingDirectory);
            case "/mcp", "/tool", "/call" -> rawMcp(rest);
            case "/help", "/commands", "/mcp-help", "/shaft-help" -> Invocation.local(HELP);
            default -> Invocation.local("Unknown command. Use /help for available SHAFT Assistant commands.");
        };
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
        if (commandIs(action, "open", "navigate", "visit", "go") || isUrl(firstWord(trimmed))) {
            return webdriverOpen(commandIs(action, "open", "navigate", "visit", "go") ? remainder : trimmed);
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
        if (commandIs(action, "tree", "source", "dom", "accessibility")) {
            JsonObject arguments = new JsonObject();
            arguments.addProperty("maxCharacters", DEFAULT_BROWSER_MAX_CHARACTERS);
            return Invocation.tool("mobile_get_accessibility_tree", arguments);
        }
        if (commandIs(action, "contexts", "context")) {
            JsonObject arguments = new JsonObject();
            arguments.addProperty("maxCharacters", DEFAULT_BROWSER_MAX_CHARACTERS);
            return Invocation.tool("mobile_get_contexts", arguments);
        }
        if (commandIs(action, "screenshot", "shot", "capture")) {
            return Invocation.tool("mobile_take_screenshot",
                    screenshot(firstTokenOrDefault(remainder, "target/shaft-mobile/screenshot.png")));
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
        return Invocation.local("Unknown mobile command. Use `/mobile status`, `/mobile native Android <device>`, or `/mobile tree`.");
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
        arguments.addProperty("headless", false);
        return arguments;
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
        return Invocation.local("Use `/mobile-record start <path>`, `/mobile-record stop`, or `/mobile-record inspector Android <path>`.");
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
        return Invocation.tool("shaft_project_create", projectCreate(firstTokenOrDefault(remainder, "shaft-demo")));
    }

    private static JsonObject projectCreate(String outputDirectory) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputDirectory", outputDirectory);
        arguments.addProperty("runner", "TestNG");
        arguments.addProperty("platform", "web");
        arguments.addProperty("groupId", "io.github.yourUsername");
        arguments.addProperty("artifactId", outputDirectory);
        arguments.addProperty("version", "1.0.0");
        arguments.add("optionalModules", new JsonArray());
        arguments.addProperty("includeGithubActions", true);
        arguments.addProperty("includeDependabot", true);
        arguments.addProperty("overwrite", false);
        return arguments;
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
        if (isStartRecording(text)) {
            return record(text.replaceFirst("(?i)^start\\s+(a\\s+)?(web(driver)?\\s+)?(capture|recording|recorder)\\b", "").trim());
        }
        if (isStopRecording(text)) {
            return Invocation.tool("capture_stop", stopRecording());
        }
        return null;
    }

    private static Invocation directIntent(String text, String workingDirectory) {
        String normalized = normalizeNaturalCommand(text);
        if (normalized.equals("start mobile recording")
                || normalized.equals("start app recording")
                || normalized.startsWith("start mobile recording ")
                || normalized.startsWith("start app recording ")) {
            return mobileRecord("start " + firstJsonLikePath(text));
        }
        if (normalized.equals("stop mobile recording") || normalized.equals("stop app recording")) {
            return mobileRecord("stop");
        }
        if ((normalized.startsWith("generate mobile code")
                || normalized.startsWith("generate appium code")
                || normalized.startsWith("create mobile code"))
                && !firstJsonLikePath(text).isBlank()) {
            return mobileCodegen(firstJsonLikePath(text));
        }
        if (normalized.startsWith("run doctor")
                || normalized.startsWith("analyze allure")
                || normalized.startsWith("analyse allure")
                || normalized.startsWith("doctor ")) {
            String path = firstPathLike(text);
            return doctor(path.isBlank() ? "" : path, workingDirectory);
        }
        return null;
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
            return Invocation.tool("playwright_recording_code_blocks", generatePlaywrightCodeFromRecording(recordingPath));
        }
        return Invocation.tool("capture_code_blocks", generateCaptureCodeFromRecording(recordingPath));
    }

    private static JsonObject captureStart(String rest) {
        JsonObject arguments = new JsonObject();
        String targetUrl = parseLeadingUrl(rest);
        arguments.addProperty("targetUrl", targetUrl.isBlank() ? DEFAULT_CAPTURE_TARGET_URL : targetUrl);
        arguments.addProperty("browser", "Chrome");
        arguments.addProperty("outputPath", defaultCaptureRecordingPath());
        arguments.addProperty("headless", false);
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
                || normalized.equals("stop recorder")
                || normalized.equals("stop capture")
                || normalized.equals("finish recording")
                || normalized.equals("end recording");
    }

    static boolean isCaptureApproval(String text) {
        String normalized = normalizeNaturalCommand(text);
        return normalized.equals("generate")
                || normalized.equals("okay")
                || normalized.equals("ok")
                || normalized.equals("approve")
                || normalized.equals("approved")
                || normalized.equals("yes")
                || normalized.equals("go ahead")
                || normalized.equals("looks good")
                || normalized.equals("create files")
                || normalized.equals("write files")
                || normalized.equals("apply it");
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

    private static JsonObject stopRecording() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("discard", false);
        return arguments;
    }

    private static JsonObject playwrightRecordStart() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", "recordings/playwright-recording.json");
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
        arguments.addProperty("overwrite", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static String captureIntegrationPrompt(String reviewMarkdown, String rawCodegenResult) {
        return """
                The user approved the reviewed SHAFT Capture code. Create the actual Java test files now.

                Requirements:
                - Use the Page Object Model where practical.
                - Inspect the current project structure before editing.
                - Move stable locators into page object classes.
                - Move replay actions into intent-named page methods.
                - Keep the TestNG test focused on scenario orchestration and final assertions.
                - Preserve existing repository style and the smallest compiling source edit.
                - Do not start a new recording or drive the browser again.
                - Run the smallest relevant compile or test check if the project can do so locally.

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

    private static String localAgentPrompt(String text, boolean agentMode, boolean allowSourceMutation) {
        String lower = text.toLowerCase(Locale.ROOT);
        String withHint = lower.contains("shaft-mcp") ? text : SHAFT_MCP_USAGE_HINT + "\n\n" + text;
        return agentMode && !allowSourceMutation ? withHint + "\n\n" + AGENT_SOURCE_GUARD : withHint;
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

    record Invocation(List<ToolCall> toolCalls, String localResponse) {
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

        String toolName() {
            return toolCalls.isEmpty() ? "" : toolCalls.get(0).toolName();
        }

        JsonObject arguments() {
            return toolCalls.isEmpty() ? new JsonObject() : toolCalls.get(0).arguments();
        }
    }

    record Selection(boolean cloud, String family, String runtime, String cloudProvider, String cloudModel) {
        static Selection local(String family, String runtime) {
            return new Selection(false, normalize(family, "CODEX"), normalize(runtime, "CLI"), "", "");
        }

        static Selection cloud(String provider, String model) {
            return new Selection(true, "", "", normalize(provider, "openai").toLowerCase(Locale.ROOT),
                    model == null ? "" : model.trim());
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
