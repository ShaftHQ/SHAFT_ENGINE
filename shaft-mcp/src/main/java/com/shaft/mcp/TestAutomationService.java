package com.shaft.mcp;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * MCP tools that describe SHAFT test-automation agent workflows and validate generated code guardrails.
 */
@Service
public class TestAutomationService {
    private static final int MAX_RESULTS = 50;
    private static final Pattern TOKEN_SPLIT = Pattern.compile("[^a-z0-9]+");
    private static final Pattern THREAD_SLEEP = Pattern.compile("\\bThread\\s*\\.\\s*sleep\\s*\\(");
    private static final Pattern BY_XPATH = Pattern.compile("\\bBy\\s*\\.\\s*xpath\\s*\\(\\s*\"((?:\\\\.|[^\"\\\\])*)\"");
    private static final Pattern PAGE_FACTORY = Pattern.compile("(?:@FindBy\\b|\\bPageFactory\\b)");
    private static final Pattern IMPLICIT_WAIT = Pattern.compile(
            "\\.\\s*manage\\s*\\(\\s*\\)\\s*\\.\\s*timeouts\\s*\\(\\s*\\)\\s*\\.\\s*implicitlyWait\\s*\\(");
    private static final Pattern RAW_FIND_ELEMENT = Pattern.compile("\\bdriver\\s*\\.\\s*findElements?\\s*\\(");
    private static final Pattern HEADED_BROWSER = Pattern.compile(
            "(?:\\.\\s*setHeadless\\s*\\(\\s*false\\s*\\)|--headed\\b|--headless\\s*=\\s*false|\\bheadless\\s*=\\s*false\\b)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern DIRECT_SYSTEM_PROPERTY = Pattern.compile("\\bSystem\\s*\\.\\s*getProperty\\s*\\(");
    private static final Pattern HARDCODED_SECRET_ASSIGNMENT = Pattern.compile(
            "\\b\\w*(?:password|passwd|pwd|secret|token|apikey|api_key|authorization|clientsecret|client_secret"
                    + "|accesstoken|access_token)\\w*\\s*=\\s*\"((?:\\\\.|[^\"\\\\]){8,})\"",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern HARDCODED_SECRET_HEADER = Pattern.compile(
            "\\b(?:put|header|setHeader|addHeader)\\s*\\(\\s*\"[^\"]*(?:authorization|token|password|secret"
                    + "|api[-_ ]?key)[^\"]*\"\\s*,\\s*\"((?:\\\\.|[^\"\\\\]){8,})\"",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern HARDCODED_SECRET_SETTER = Pattern.compile(
            "\\b(?:setPassword|setToken|setSecret|setApiKey|setAuthorization|bearerToken|basicAuth)\\s*\\("
                    + "\\s*\"((?:\\\\.|[^\"\\\\]){8,})\"",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern STRING_LITERAL = Pattern.compile("\"(?:\\\\.|[^\"\\\\])*\"");
    private static final String NO_SLEEP = "Do not generate Thread.sleep; use SHAFT waits/actions/assertions.";
    private static final String NO_ABSOLUTE_XPATH = "Do not generate absolute XPath; prefer smart locators,"
            + " SHAFT.GUI.Locator builders, or stable By/AppiumBy locators.";
    private static final String NO_IMPLICIT_WAIT = "Avoid Selenium implicit waits; use SHAFT waits/actions/assertions.";
    private static final String NO_RAW_FIND_ELEMENT = "Avoid direct driver.findElement/findElements calls in generated"
            + " SHAFT tests; route actions through SHAFT facades or page objects.";
    private static final String NO_HEADED_BROWSER = "Do not hard-code headed browser setup; keep generated tests"
            + " headless-configurable.";
    private static final String NO_DIRECT_SYSTEM_PROPERTY = "Avoid direct System.getProperty() in SHAFT-like snippets;"
            + " use SHAFT properties or injected configuration.";
    private static final String NO_HARDCODED_SECRET = "Do not hard-code obvious header, token, or password secrets.";
    private static final List<String> GUIDANCE_RULES = List.of(
            "Call shaft_guide_search before writing SHAFT API, GUI, mobile, CLI, DB, or troubleshooting code.",
            "Keep MCP as guidance and inspection; the calling agent writes repository files and runs validation.",
            "For OpenAPI work, group by tag/resource and place reusable request builders and response validators"
                    + " under src/main/java.",
            "Place executable TestNG scenarios under src/test/java and reuse the main-package API or page objects.",
            "For GUI work, use Page Object Model plus fluent SHAFT actions by default.",
            "Use WebDriver MCP tools by default; use playwright_* tools when the project or user asks for SHAFT Playwright.",
            "Prefer SHAFT smart/semantic locators, ARIA locators, and SHAFT.GUI.Locator builders before raw By objects.",
            NO_SLEEP,
            NO_ABSOLUTE_XPATH,
            NO_IMPLICIT_WAIT,
            NO_RAW_FIND_ELEMENT,
            NO_HEADED_BROWSER,
            NO_DIRECT_SYSTEM_PROPERTY,
            "Do not generate hard-coded secrets in headers, tokens, passwords, or API keys.");
    private static final List<McpTestAutomationScenario> CATALOG = scenarios();

    /**
     * Returns SHAFT MCP test automation use cases and the actions a coding agent should take.
     *
     * @param area automation area such as all, api, web, mobile, capture, doctor, reporting, ci, cli, or db
     * @param intent optional user intent used to narrow the catalog
     * @param maxResults maximum scenarios to return; 0 returns every match
     * @return scenario catalog result
     */
    @Tool(name = "test_automation_scenarios",
            description = "returns SHAFT MCP usage scenarios, agent actions, design patterns, guardrails,"
                    + " and completion criteria for API, GUI, mobile, capture, Doctor, Heal, reporting, and CI work")
    public McpScenarioCatalogResult testAutomationScenarios(String area, String intent, int maxResults) {
        String effectiveArea = normalizeArea(area);
        String effectiveIntent = text(intent);
        List<McpTestAutomationScenario> areaMatches = CATALOG.stream()
                .filter(scenario -> "all".equals(effectiveArea) || scenario.areas().contains(effectiveArea))
                .toList();
        List<McpTestAutomationScenario> matches = areaMatches.stream()
                .filter(scenario -> matchesIntent(scenario, effectiveIntent))
                .toList();
        List<String> warnings = new ArrayList<>();
        if (matches.isEmpty() && !areaMatches.isEmpty() && !effectiveIntent.isBlank()) {
            warnings.add("No exact intent match. Returning all scenarios for area: " + effectiveArea + ".");
            matches = areaMatches;
        }
        if (matches.isEmpty()) {
            warnings.add("No scenarios matched area: " + effectiveArea + ".");
        }
        return new McpScenarioCatalogResult(
                McpScenarioCatalogResult.CURRENT_SCHEMA_VERSION,
                effectiveArea,
                effectiveIntent,
                limit(matches, maxResults),
                GUIDANCE_RULES,
                warnings);
    }

    /**
     * Checks generated or agent-authored code for SHAFT MCP guardrails.
     *
     * @param language source language label, usually java
     * @param code source code to check
     * @return code guardrail result
     */
    @Tool(name = "test_code_guardrails_check",
            description = "checks generated SHAFT test code for lexical anti-patterns such as sleeps, brittle locators,"
                    + " raw driver calls, headed setup, direct system properties, and obvious secrets")
    public McpCodeGuardrailResult checkGeneratedCode(String language, String code) {
        String source = code == null ? "" : code;
        List<McpCodeGuardrailViolation> violations = new ArrayList<>();
        addThreadSleepViolations(source, violations);
        addAbsoluteXpathViolations(source, violations);
        addPageFactoryWarnings(source, violations);
        addImplicitWaitWarnings(source, violations);
        addRawFindElementWarnings(source, violations);
        addHeadedBrowserWarnings(source, violations);
        addDirectSystemPropertyWarnings(source, violations);
        addHardcodedSecretViolations(source, violations);
        List<String> warnings = source.isBlank()
                ? List.of("No code was supplied.")
                : List.of("Lexical guardrail check only; compile/runtime validation is still required.");
        boolean passed = violations.stream().noneMatch(violation -> "ERROR".equals(violation.severity()));
        return new McpCodeGuardrailResult(
                McpCodeGuardrailResult.CURRENT_SCHEMA_VERSION,
                language,
                passed,
                violations,
                warnings);
    }

    private static void addThreadSleepViolations(String source, List<McpCodeGuardrailViolation> violations) {
        addPatternViolations(source, violations, THREAD_SLEEP, "THREAD_SLEEP", "ERROR", NO_SLEEP);
    }

    private static void addAbsoluteXpathViolations(String source, List<McpCodeGuardrailViolation> violations) {
        Matcher matcher = BY_XPATH.matcher(source);
        while (matcher.find()) {
            if (isCommentOnlyLine(source, matcher.start())) {
                continue;
            }
            String xpath = matcher.group(1).replace("\\\"", "\"").trim();
            if (isAbsoluteXpath(xpath)) {
                violations.add(violation(
                        "ABSOLUTE_XPATH",
                        "ERROR",
                        NO_ABSOLUTE_XPATH,
                        source,
                        matcher.start()));
            }
        }
    }

    private static void addPageFactoryWarnings(String source, List<McpCodeGuardrailViolation> violations) {
        addPatternViolations(source, violations, PAGE_FACTORY, "PAGE_FACTORY", "WARNING",
                "Prefer Selenium By objects and SHAFT.GUI.Locator instead of @FindBy or PageFactory.");
    }

    private static void addImplicitWaitWarnings(String source, List<McpCodeGuardrailViolation> violations) {
        addPatternViolations(source, violations, IMPLICIT_WAIT, "IMPLICIT_WAIT", "WARNING", NO_IMPLICIT_WAIT);
    }

    private static void addRawFindElementWarnings(String source, List<McpCodeGuardrailViolation> violations) {
        addPatternViolations(source, violations, RAW_FIND_ELEMENT, "RAW_FIND_ELEMENT", "WARNING", NO_RAW_FIND_ELEMENT);
    }

    private static void addHeadedBrowserWarnings(String source, List<McpCodeGuardrailViolation> violations) {
        addPatternViolations(source, violations, HEADED_BROWSER, "HEADED_BROWSER", "WARNING", NO_HEADED_BROWSER);
    }

    private static void addDirectSystemPropertyWarnings(String source, List<McpCodeGuardrailViolation> violations) {
        addPatternViolations(source, violations, DIRECT_SYSTEM_PROPERTY, "DIRECT_SYSTEM_PROPERTY", "WARNING",
                NO_DIRECT_SYSTEM_PROPERTY);
    }

    private static void addHardcodedSecretViolations(String source, List<McpCodeGuardrailViolation> violations) {
        addSecretViolations(source, violations, HARDCODED_SECRET_ASSIGNMENT);
        addSecretViolations(source, violations, HARDCODED_SECRET_HEADER);
        addSecretViolations(source, violations, HARDCODED_SECRET_SETTER);
    }

    private static void addPatternViolations(
            String source,
            List<McpCodeGuardrailViolation> violations,
            Pattern pattern,
            String kind,
            String severity,
            String message) {
        Matcher matcher = pattern.matcher(source);
        while (matcher.find()) {
            if (isCommentOnlyLine(source, matcher.start())) {
                continue;
            }
            violations.add(violation(kind, severity, message, source, matcher.start()));
        }
    }

    private static void addSecretViolations(
            String source,
            List<McpCodeGuardrailViolation> violations,
            Pattern pattern) {
        Matcher matcher = pattern.matcher(source);
        while (matcher.find()) {
            if (isCommentOnlyLine(source, matcher.start()) || !isSuspiciousSecretLiteral(matcher.group(1))) {
                continue;
            }
            violations.add(secretViolation(source, matcher.start()));
        }
    }

    private static boolean isAbsoluteXpath(String xpath) {
        return (xpath.startsWith("/") && !xpath.startsWith("//")) || xpath.startsWith("(/");
    }

    private static boolean isSuspiciousSecretLiteral(String literal) {
        String value = literal == null ? "" : literal.trim();
        if (value.length() < 8 || value.contains("${")) {
            return false;
        }
        String lower = value.toLowerCase(Locale.ROOT);
        if (lower.contains("example") || lower.contains("sample") || lower.contains("dummy")
                || lower.contains("placeholder") || lower.contains("redacted") || lower.contains("changeme")
                || lower.contains("change-me") || lower.contains("your_") || lower.contains("your-")) {
            return false;
        }
        String compact = lower.replaceAll("[^a-z0-9]+", "");
        return switch (compact) {
            case "authorization", "apikey", "token", "accesstoken", "password", "passwd", "pwd", "secret",
                    "clientsecret" -> false;
            default -> true;
        };
    }

    private static boolean isCommentOnlyLine(String source, int offset) {
        int lineStart = source.lastIndexOf('\n', Math.max(0, offset - 1)) + 1;
        String prefix = source.substring(lineStart, Math.min(offset, source.length())).trim();
        return prefix.startsWith("//") || prefix.startsWith("/*") || prefix.startsWith("*");
    }

    private static McpCodeGuardrailViolation violation(
            String kind,
            String severity,
            String message,
            String source,
            int offset) {
        return new McpCodeGuardrailViolation(kind, severity, message, lineNumber(source, offset), lineSnippet(source, offset));
    }

    private static McpCodeGuardrailViolation secretViolation(String source, int offset) {
        return new McpCodeGuardrailViolation(
                "HARDCODED_SECRET",
                "ERROR",
                NO_HARDCODED_SECRET,
                lineNumber(source, offset),
                STRING_LITERAL.matcher(lineSnippet(source, offset)).replaceAll("\"[REDACTED]\""));
    }

    private static int lineNumber(String source, int offset) {
        int line = 1;
        int length = Math.min(offset, source.length());
        for (int index = 0; index < length; index++) {
            if (source.charAt(index) == '\n') {
                line++;
            }
        }
        return line;
    }

    private static String lineSnippet(String source, int offset) {
        int start = source.lastIndexOf('\n', Math.max(0, offset - 1)) + 1;
        int end = source.indexOf('\n', offset);
        if (end < 0) {
            end = source.length();
        }
        String snippet = source.substring(start, end).trim();
        return snippet.length() > 160 ? snippet.substring(0, 157) + "..." : snippet;
    }

    private static boolean matchesIntent(McpTestAutomationScenario scenario, String intent) {
        if (intent.isBlank()) {
            return true;
        }
        String searchable = String.join(" ",
                scenario.id(),
                scenario.title(),
                String.join(" ", scenario.areas()),
                String.join(" ", scenario.userPrompts()),
                String.join(" ", scenario.agentActions()),
                String.join(" ", scenario.repoPattern())).toLowerCase(Locale.ROOT);
        String query = intent.toLowerCase(Locale.ROOT);
        if (searchable.contains(query)) {
            return true;
        }
        for (String token : TOKEN_SPLIT.split(query)) {
            if (token.length() > 2 && searchable.contains(token)) {
                return true;
            }
        }
        return false;
    }

    private static List<McpTestAutomationScenario> limit(List<McpTestAutomationScenario> values, int maxResults) {
        if (maxResults < 1 || values.size() <= maxResults) {
            return values;
        }
        return values.subList(0, Math.min(maxResults, MAX_RESULTS));
    }

    private static String normalizeArea(String area) {
        String value = text(area).toLowerCase(Locale.ROOT);
        return value.isBlank() ? "all" : value;
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static List<McpTestAutomationScenario> scenarios() {
        return List.of(
                s("setup-connect-agent", "Connect an MCP client to SHAFT", a("setup", "mcp"),
                        "Install shaft-mcp for Codex or Claude and verify available tools.",
                        t("shaft_guide_search"),
                        t("Run the installer or configured server", "Verify tool discovery in the client"),
                        t("No repository code required"),
                        t("Do not expose model or cloud credentials"),
                        t("Client can call shaft_guide_search and reports the configured shaft-mcp version")),
                s("guide-before-code", "Search official SHAFT guidance before coding", a("setup", "docs"),
                        "Before writing this SHAFT test, find the official pattern to follow.",
                        t("shaft_guide_search"),
                        t("Search task-shaped guide terms", "Cite returned guide URLs in the implementation summary"),
                        t("No repository code required"),
                        t("Do not invent SHAFT APIs when guide results are missing"),
                        t("Agent has cited guide evidence or states that guidance was unavailable")),
                s("api-openapi-contract-suite", "Generate OpenAPI-backed SHAFT API tests", a("api", "openapi"),
                        "Create API tests that cover this Swagger contract with reusable request builders and validators.",
                        t("shaft_guide_search", "test_code_guardrails_check"),
                        t("Read the OpenAPI contract", "Group operations by tag or resource",
                                "Write request builders and response validators under src/main/java",
                                "Write TestNG scenario classes under src/test/java"),
                        t("src/main/java/<basePackage>/api for reusable API objects",
                                "src/test/java/<basePackage>/tests/api for executable tests"),
                        t(NO_SLEEP, "Enable swagger.validation.enabled with swagger.validation.url", "No secrets in generated code"),
                        t("Focused API tests compile, run, and validate against the configured OpenAPI contract")),
                s("api-auth-and-session", "Create API authentication and session flows", a("api"),
                        "Add SHAFT API tests for login, token reuse, and protected endpoints.",
                        t("shaft_guide_search", "test_code_guardrails_check"),
                        t("Model login/token setup once", "Reuse headers or cookies through SHAFT.API session",
                                "Cover valid and invalid authentication"),
                        t("Authentication helper in src/main/java API package; tests call it explicitly"),
                        t("No hard-coded credentials", "Use test data or environment-backed placeholders"),
                        t("Protected happy path and invalid auth assertions pass")),
                s("api-negative-boundary-tests", "Cover API negative and boundary behavior", a("api"),
                        "Add negative SHAFT API scenarios for required fields, bad IDs, and forbidden requests.",
                        t("shaft_guide_search", "test_code_guardrails_check"),
                        t("Read status codes from the contract or existing tests",
                                "Use setTargetStatusCode for expected non-200 responses",
                                "Assert response body errors with response validators"),
                        t("Negative validators live beside positive validators"),
                        t("Do not disable status validation unless the assertion requires that status"),
                        t("Negative tests assert status, error body, and contract behavior")),
                s("api-graphql-suite", "Create GraphQL SHAFT API tests", a("api", "graphql"),
                        "Write SHAFT tests for these GraphQL queries and mutations.",
                        t("shaft_guide_search", "test_code_guardrails_check"),
                        t("Use SHAFT.API.sendGraphQlRequest", "Keep query strings readable",
                                "Assert data and errors through response validators"),
                        t("GraphQL request methods under src/main/java API package"),
                        t("No secrets in headers", "Keep variables explicit test data"),
                        t("Query, mutation, variable, and auth scenarios pass")),
                s("web-pom-fluent-test", "Create web GUI tests with POM and fluent SHAFT actions", a("web", "gui"),
                        "Use SHAFT MCP to inspect this page and write maintainable web UI tests.",
                        t("shaft_guide_search", "driver_initialize", "browser_navigate", "browser_get_page_dom",
                                "browser_take_screenshot", "test_code_guardrails_check"),
                        t("Inspect DOM and screenshot", "Create or extend page objects",
                                "Write fluent SHAFT actions and assertions in tests"),
                        t("Page objects under src/main/java/<basePackage>/pages; tests under src/test/java"),
                        t(NO_SLEEP, NO_ABSOLUTE_XPATH, "No @FindBy or PageFactory"),
                        t("Headless focused GUI test passes and page object methods stay reusable")),
                s("web-playwright-pom-fluent-test", "Create web GUI tests with SHAFT Playwright", a("web", "gui", "playwright"),
                        "Use SHAFT Playwright MCP tools to inspect this page and write maintainable UI tests.",
                        t("shaft_guide_search", "playwright_initialize", "playwright_browser_navigate",
                                "playwright_browser_get_page_dom", "playwright_browser_take_screenshot",
                                "playwright_element_click", "playwright_element_type", "test_code_guardrails_check"),
                        t("Inspect DOM and screenshot with Playwright", "Create or extend page objects",
                                "Use SHAFT.GUI.Playwright actions and assertions in tests"),
                        t("Page objects under src/main/java/<basePackage>/pages; tests under src/test/java"),
                        t(NO_SLEEP, NO_ABSOLUTE_XPATH, "Do not mix WebDriver-only waits into Playwright tests"),
                        t("Headless focused Playwright GUI test passes and page object methods stay reusable")),
                s("web-locator-refactor", "Refactor brittle web locators", a("web", "gui", "locator"),
                        "Replace these brittle locators with SHAFT smart locators or locator builders.",
                        t("shaft_guide_search", "browser_get_page_dom", "browser_take_screenshot",
                                "test_code_guardrails_check"),
                        t("Find stable labels, roles, data attributes, ids, or text",
                                "Prefer smart and semantic locator builders", "Run the affected UI test"),
                        t("Only locator fields/methods change unless behavior is wrong"),
                        t(NO_ABSOLUTE_XPATH, "No PageFactory migration"),
                        t("Affected tests pass with clearer locator definitions")),
                s("web-capture-to-pom", "Convert browser capture into page objects", a("web", "gui", "capture"),
                        "Record this flow and integrate it into our existing page object tests.",
                        t("capture_start", "capture_status", "capture_stop", "capture_code_blocks",
                                "test_code_guardrails_check"),
                        t("Record the user flow", "Generate replay snippets",
                                "Move stable locators/actions into page objects instead of pasting a raw class"),
                        t("Generated capture code is integrated into existing page/test structure"),
                        t(NO_SLEEP, NO_ABSOLUTE_XPATH),
                        t("Replay-derived test compiles and passes, with no duplicate generic test class")),
                s("web-playwright-record-replay", "Record and replay SHAFT Playwright actions", a("web", "gui", "capture", "playwright"),
                        "Record this Playwright journey and turn it into reusable SHAFT Playwright test code.",
                        t("playwright_initialize", "playwright_record_start", "playwright_record_stop",
                                "playwright_recording_code_blocks", "playwright_replay_recording",
                                "playwright_capture_code_blocks", "test_code_guardrails_check"),
                        t("Record actions through MCP Playwright tools", "Generate Playwright replay snippets",
                                "Move stable locators/actions into page objects instead of pasting a raw class"),
                        t("Recording JSON remains an artifact; reusable code enters page/test classes"),
                        t(NO_SLEEP, NO_ABSOLUTE_XPATH, "Do not include sensitive typed values unless explicitly allowed"),
                        t("Replay code is reviewed, guarded, and inserted into the right Playwright classes")),
                s("web-visual-accessibility", "Add visual or accessibility checks", a("web", "gui", "visual"),
                        "Add visual or accessibility assertions for this critical page state.",
                        t("shaft_guide_search", "driver_initialize", "browser_navigate", "browser_take_screenshot"),
                        t("Use visual/accessibility checks only for requested states",
                                "Store needed reference assets under test resources"),
                        t("Assertions live in the page or focused test that owns the state"),
                        t("Do not add screenshots for non-visual behavior"),
                        t("Visual/accessibility evidence is attached to the relevant report")),
                s("mobile-web-emulation", "Run mobile web emulation checks", a("mobile", "web", "gui"),
                        "Test this responsive flow with SHAFT mobile web emulation.",
                        t("mobile_initialize_web_emulation", "mobile_take_screenshot", "mobile_get_contexts",
                                "test_code_guardrails_check"),
                        t("Initialize mobile web emulation", "Inspect viewport", "Write reusable page object flow"),
                        t("Reuse web page objects when behavior matches desktop"),
                        t(NO_SLEEP, NO_ABSOLUTE_XPATH),
                        t("Mobile web scenario passes in headless-compatible validation")),
                s("mobile-native-appium", "Create native Appium SHAFT tests", a("mobile", "gui"),
                        "Inspect this native app screen and write SHAFT Appium tests.",
                        t("mobile_initialize_native", "mobile_get_accessibility_tree", "mobile_take_screenshot",
                                "mobile_tap", "mobile_type", "test_code_guardrails_check"),
                        t("Inspect contexts and accessibility tree", "Prefer accessibility ids and stable Appium locators",
                                "Create mobile page objects and TestNG tests"),
                        t("Mobile page objects under src/main/java; tests under src/test/java"),
                        t(NO_SLEEP, "Use AppiumBy.accessibilityId before XPath"),
                        t("Native mobile test compiles; real-device/cloud run is reported if unavailable")),
                s("mobile-record-replay", "Record and replay mobile actions", a("mobile", "capture"),
                        "Record this mobile journey and turn it into reusable SHAFT test code.",
                        t("mobile_record_start", "mobile_record_stop", "mobile_recording_code_blocks",
                                "mobile_replay_recording", "test_code_guardrails_check"),
                        t("Record actions through MCP mobile tools", "Generate code blocks",
                                "Move actions into mobile page methods"),
                        t("Recording JSON remains an artifact; reusable code enters page/test classes"),
                        t(NO_SLEEP, "Do not include sensitive typed values unless explicitly allowed"),
                        t("Replay code is reviewed, guarded, and inserted into the right classes")),
                s("cli-test-generation", "Create SHAFT CLI and file tests", a("cli"),
                        "Write SHAFT tests for this command-line workflow and generated files.",
                        t("shaft_guide_search", "test_code_guardrails_check"),
                        t("Model command inputs and expected files", "Use SHAFT CLI/file actions",
                                "Assert outputs without shell-specific brittle parsing"),
                        t("CLI helper objects under src/main/java only when reused"),
                        t("No destructive commands unless user explicitly approves"),
                        t("Command test passes and generated files are cleaned or isolated")),
                s("db-test-generation", "Create SHAFT database checks", a("db"),
                        "Write database verification tests for this backend flow.",
                        t("shaft_guide_search", "test_code_guardrails_check"),
                        t("Read existing DB configuration", "Run setup/query/assertions through SHAFT DB patterns"),
                        t("Reusable DB helpers under src/main/java only when shared"),
                        t("No real production connection strings or destructive queries"),
                        t("DB test uses test data and reports query assertions")),
                s("failure-doctor-analysis", "Analyze failed SHAFT, Selenium, or Playwright tests", a("doctor", "failure"),
                        "Analyze these Allure results and tell me the first actionable failure.",
                        t("doctor_analyze_failed_allure", "doctor_suggest_fix",
                                "playwright_doctor_analyze_failed_allure", "playwright_doctor_suggest_fix",
                                "shaft_guide_search"),
                        t("Analyze Allure evidence", "Separate product defect, test defect, and infrastructure",
                                "Return review-only code blocks when applicable"),
                        t("No source edit unless the calling agent applies a reviewed fix"),
                        t("Do not trust empty Allure result files"),
                        t("Agent reports first actionable failure and next validation step")),
                s("failure-healer-loop", "Run guarded healer loop", a("doctor", "healer", "failure"),
                        "Rerun this failing Selenium or Playwright test and propose a safe fix.",
                        t("healer_run_failed_test", "playwright_healer_run_failed_test",
                                "doctor_analyze_failed_allure", "playwright_doctor_analyze_failed_allure",
                                "test_code_guardrails_check"),
                        t("Run allowed headless Maven command", "Analyze fresh evidence", "Propose review-only fix"),
                        t("Agent applies only approved source edits"),
                        t("No publish/deploy goals; no worktree escape paths"),
                        t("Failure is fixed by validation or reported as blocked/product defect")),
                s("report-generate-and-summarize", "Generate and summarize test evidence", a("reporting"),
                        "Generate the SHAFT report and summarize what passed and failed.",
                        t("generate_test_report", "doctor_analyze_failed_allure"),
                        t("Generate report from current session", "Summarize Allure/SHAFT evidence and failures"),
                        t("No source code changes"),
                        t("Do not delete the allure-results root"),
                        t("Report path and high-signal result summary are returned")),
                s("ci-local-validation", "Choose the smallest useful validation", a("ci"),
                        "Validate the SHAFT changes locally without running unnecessary suites.",
                        t("shaft_guide_search"),
                        t("Load validation gotchas", "Run affected tests first", "Escalate to compile/package when needed"),
                        t("Validation command is chosen from changed files and risk"),
                        t("Avoid external/cloud suites unless required and configured"),
                        t("Agent reports exact commands, result, and remaining risk")),
                s("migration-or-upgrade-support", "Support SHAFT migration or upgrade work", a("setup", "migration"),
                        "Help migrate this project to current SHAFT patterns.",
                        t("shaft_guide_search", "doctor_analyze_failed_allure"),
                        t("Inspect existing POMs and SHAFT usage", "Apply minimal supported replacements",
                                "Validate compile before broader tests"),
                        t("Preserve existing project layout and public tests"),
                        t("No publish/deploy/history rewrite operations"),
                        t("Migration compiles or blockers are reported with exact files")));
    }

    private static McpTestAutomationScenario s(
            String id,
            String title,
            List<String> areas,
            String prompt,
            List<String> tools,
            List<String> actions,
            List<String> pattern,
            List<String> guardrails,
            List<String> done) {
        return new McpTestAutomationScenario(id, title, areas, List.of(prompt), tools, actions, pattern, guardrails, done);
    }

    private static List<String> a(String... values) {
        return List.of(values);
    }

    private static List<String> t(String... values) {
        return List.of(values);
    }
}
