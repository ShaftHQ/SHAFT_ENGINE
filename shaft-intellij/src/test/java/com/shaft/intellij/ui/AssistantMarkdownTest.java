package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.concurrent.CompletionException;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantMarkdownTest {
    @Test
    void captureReplayResultTellsTheFullStoryInsteadOfBareCodeBlocks() {
        // Issue #3426 B4: /codegen results must explain what was generated where, whether it
        // compiled, why a browser window opened, and what to do next — never a bare "Done".
        String markdown = AssistantMarkdown.fromMcpOutput("capture_generate_replay", """
                {
                  "sourcePath": "target/shaft-capture-generated/src/test/java/RecordedFlowTest.java",
                  "testDataPath": "target/shaft-capture-generated/capture-data.json",
                  "reportPath": "target/shaft-capture-generated/report.json",
                  "reviewPath": "target/shaft-capture-generated/review.md",
                  "successful": true,
                  "codeBlocks": [{"id": "full-class", "language": "java",
                    "code": "public class RecordedFlowTest { SHAFT.GUI.WebDriver driver; }"}],
                  "report": {
                    "status": "SUCCESS",
                    "compilation": {"status": "PASSED", "diagnostics": [], "allureResultCount": 0},
                    "replay": {"status": "PASSED", "diagnostics": [], "allureResultCount": 2},
                    "warnings": []
                  },
                  "warnings": []
                }
                """);

        assertAll(
                () -> assertTrue(markdown.contains("Test generated, compiled, and verified"), markdown),
                () -> assertTrue(markdown.contains("What happened"), markdown),
                () -> assertTrue(markdown.contains("RecordedFlowTest.java"), markdown),
                () -> assertTrue(markdown.contains("browser window you may have seen open"), markdown),
                () -> assertTrue(markdown.contains("Generated code"), markdown),
                () -> assertTrue(markdown.contains("```java"), markdown),
                () -> assertTrue(markdown.contains("report.json"), markdown));
    }

    @Test
    void captureReplayFailureExplainsTheFailingPhaseAndKeepsNothingHidden() {
        String markdown = AssistantMarkdown.fromMcpOutput("capture_generate_replay", """
                {
                  "sourcePath": "target/generated/RecordedFlowTest.java",
                  "successful": false,
                  "codeBlocks": [],
                  "report": {
                    "status": "FAILED",
                    "compilation": {"status": "PASSED", "diagnostics": [], "allureResultCount": 0},
                    "replay": {"status": "FAILED",
                      "diagnostics": ["Replay step 3 failed: element not found: searchbox_input"],
                      "allureResultCount": 1},
                    "warnings": []
                  },
                  "warnings": ["Replay ran against a changed page"]
                }
                """);

        assertAll(
                () -> assertTrue(markdown.contains("finished with problems"), markdown),
                () -> assertTrue(markdown.contains("The replay FAILED"), markdown),
                () -> assertTrue(markdown.contains("Replay step 3 failed: element not found: searchbox_input"),
                        markdown),
                () -> assertTrue(markdown.contains("about:blank"), markdown),
                () -> assertTrue(markdown.contains("No usable code was produced"), markdown),
                () -> assertTrue(markdown.contains("Replay ran against a changed page"), markdown));
    }

    @Test
    void unwrapsNestedMcpTextAndFencesJavaCode() {
        String code = """
                public class LoginTest {
                    @Test void logsIn() {
                    }
                }
                """.stripIndent().trim();

        String markdown = AssistantMarkdown.fromMcpOutput(mcpText(mcpText(code)));

        assertAll(
                () -> assertTrue(markdown.startsWith("```java")),
                () -> assertTrue(markdown.contains("public class LoginTest")),
                () -> assertFalse(markdown.contains("\"content\"")));
    }

    @Test
    void unwrapsJsonRpcResultContentEnvelope() {
        JsonObject envelope = new JsonObject();
        envelope.addProperty("jsonrpc", "2.0");
        envelope.addProperty("id", 1);
        envelope.add("result", JsonParser.parseString(mcpText("Run the focused check.")));

        String markdown = AssistantMarkdown.fromMcpOutput("shaft_guide_search", envelope.toString());

        assertAll(
                () -> assertTrue(markdown.contains("Run the focused check.")),
                () -> assertFalse(markdown.contains("\"jsonrpc\"")),
                () -> assertFalse(markdown.contains("\"content\"")));
    }

    @Test
    void formatsJsonPayloadsAsMarkdownFencedBlocks() {
        String markdown = AssistantMarkdown.fromMcpOutput(mcpText("{\"client\":\"CODEX\",\"displayName\":\"Codex CLI\"}"));

        assertAll(
                () -> assertTrue(markdown.startsWith("```json")),
                () -> assertTrue(markdown.contains("\"displayName\": \"Codex CLI\"")));
    }

    @Test
    void formatsLocalAgentClientsAsMarkdownTable() {
        String markdown = AssistantMarkdown.fromMcpOutput("autobot_local_agent_clients", mcpText("""
                [
                  {"id":"CODEX","displayName":"Codex CLI","executableName":"codex","requiresCloudApiKey":false},
                  {"id":"CLAUDE_CODE","displayName":"Claude Code","executableName":"claude","requiresCloudApiKey":false}
                ]
                """));

        assertAll(
                () -> assertTrue(markdown.contains("| Client | Command | SHAFT API key |")),
                () -> assertTrue(markdown.contains("| Codex CLI | `codex` | Not required |")),
                () -> assertFalse(markdown.contains("\"displayName\"")));
    }

    @Test
    void formatsLocalAgentRunAroundStdoutAndWarnings() {
        String markdown = AssistantMarkdown.fromMcpOutput("autobot_local_agent_run", """
                {
                  "status": "FAILED",
                  "client": "CODEX",
                  "mode": "ASK",
                  "command": ["codex", "exec"],
                  "exitCode": 1,
                  "stdout": "Use `mvn test` for the focused check.",
                  "stderr": "WARN noisy diagnostic",
                  "timedOut": false,
                  "duration": "PT1S",
                  "requiresCloudApiKey": false,
                  "warnings": ["Codex returned a non-zero exit code."]
                }
                """);

        assertAll(
                () -> assertTrue(markdown.contains("Use `mvn test` for the focused check.")),
                () -> assertTrue(markdown.contains("**⚠️ Warnings**")),
                () -> assertTrue(markdown.contains("Codex returned a non-zero exit code.")),
                () -> assertTrue(markdown.contains("```text")),
                () -> assertFalse(markdown.contains("\"stdout\"")));
    }

    @Test
    void formatsProviderChatAnswerAndRejectedWarnings() {
        String success = AssistantMarkdown.fromMcpOutput("autobot_provider_chat", """
                {
                  "status": "SUCCESS",
                  "provider": "openai",
                  "model": "gpt-4.1",
                  "mode": "ASK",
                  "answer": "Run the focused IntelliJ plugin tests.",
                  "warnings": [],
                  "duration": "PT2S",
                  "fallbackReason": ""
                }
                """);
        String rejected = AssistantMarkdown.fromMcpOutput("autobot_provider_chat", """
                {
                  "status": "REJECTED",
                  "provider": "github",
                  "model": "openai/gpt-4.1",
                  "mode": "AGENT",
                  "answer": "",
                  "warnings": ["Cloud provider chat supports Ask and Plan only."],
                  "duration": "PT0S",
                  "fallbackReason": ""
                }
                """);

        assertAll(
                () -> assertTrue(success.contains("Run the focused IntelliJ plugin tests.")),
                () -> assertFalse(success.contains("\"answer\"")),
                () -> assertTrue(rejected.contains("Cloud provider chat supports Ask and Plan only.")),
                () -> assertTrue(rejected.contains("**Status:** REJECTED")));
    }

    @Test
    void formatsCaptureStatusWithoutRawJson() {
        String markdown = AssistantMarkdown.fromMcpOutput("capture_status", mcpText("""
                {
                  "state": "INCOMPLETE",
                  "sessionId": "session",
                  "browser": "chrome",
                  "currentUrl": "https://example.test/",
                  "eventCount": 2,
                  "readiness": "BLOCKED",
                  "warnings": ["The recorder process is no longer reachable."],
                  "outputPath": "recordings/intellij-capture.json",
                  "aiEnabled": false,
                  "processId": 123,
                  "startedAt": "2026-06-30T19:00:00Z"
                }
                """));

        assertAll(
                () -> assertTrue(markdown.contains("**State:** INCOMPLETE")),
                () -> assertTrue(markdown.contains("⛔ BLOCKED")),
                () -> assertTrue(markdown.contains("**Output:** `recordings/intellij-capture.json`")),
                () -> assertTrue(markdown.contains("The recorder process is no longer reachable.")),
                () -> assertTrue(markdown.contains("nothing was lost")),
                () -> assertFalse(markdown.contains("\"processId\"")));
    }

    @Test
    void formatsStoppedCaptureWithCodegenHint() {
        String markdown = AssistantMarkdown.fromMcpOutput("capture_stop", mcpText("""
                {
                  "state": "COMPLETED",
                  "sessionId": "session",
                  "browser": "chrome",
                  "currentUrl": "https://example.test/checkout",
                  "eventCount": 4,
                  "readiness": "READY",
                  "warnings": [],
                  "outputPath": "recordings/intellij-capture.json",
                  "aiEnabled": false,
                  "processId": 123,
                  "startedAt": "2026-06-30T19:00:00Z"
                }
                """));

        assertAll(
                () -> assertTrue(markdown.contains("**State:** COMPLETED")),
                () -> assertTrue(markdown.contains("✅ READY")),
                () -> assertFalse(markdown.contains("nothing was lost")),
                () -> assertTrue(markdown.contains("**Output:** `recordings/intellij-capture.json`")),
                () -> assertTrue(markdown.contains("Generate code next — send:")),
                () -> assertTrue(markdown.contains(
                        "```text\nGenerate a SHAFT test from recordings/intellij-capture.json\n```")));
    }


    @Test
    void formatsMobileToolchainSessionInspectionAndScreenshotResponses() {
        String toolchain = AssistantMarkdown.fromMcpOutput("mobile_toolchain_status", mcpText("""
                {
                  "platformName": "Android",
                  "nodeAvailable": true,
                  "npmAvailable": true,
                  "appiumAvailable": false,
                  "appiumInspectorAvailable": true,
                  "adbAvailable": true,
                  "emulatorAvailable": false,
                  "sdkManagerAvailable": true,
                  "avdManagerAvailable": true,
                  "appiumVersion": "",
                  "appiumInspectorPluginVersion": "2025.3.1",
                  "androidDevices": [{"id":"emulator-5554","state":"device","type":"emulator"}],
                  "cachedAndroidEmulators": ["Pixel_6"],
                  "missingDependencies": ["appium"],
                  "warnings": ["Install the managed Appium toolchain."],
                  "diagnostics": []
                }
                """));
        String session = AssistantMarkdown.fromMcpOutput("mobile_initialize_web_emulation", mcpText("""
                {
                  "mode": "web-emulation",
                  "platformName": "",
                  "deviceName": "Pixel 5",
                  "browserName": "CHROME",
                  "active": true,
                  "codeBlocks": [],
                  "warnings": [],
                  "deviceProfile": {"width":"393","height":"851"}
                }
                """));
        String tree = AssistantMarkdown.fromMcpOutput("mobile_get_accessibility_tree", mcpText("""
                {
                  "currentContext": "NATIVE_APP",
                  "source": "<hierarchy><node text='Sign in'/></hierarchy>",
                  "characterCount": 44,
                  "truncated": false,
                  "warnings": []
                }
                """));
        String contexts = AssistantMarkdown.fromMcpOutput("mobile_get_contexts", mcpText("""
                {
                  "currentContext": "WEBVIEW_chrome",
                  "contexts": ["NATIVE_APP", "WEBVIEW_chrome"],
                  "pageSource": "<html><body>Home</body></html>",
                  "sourceCharacterCount": 30,
                  "truncated": false,
                  "warnings": []
                }
                """));
        String screenshot = AssistantMarkdown.fromMcpOutput("mobile_take_screenshot", mcpText("""
                {
                  "mediaType": "image/png",
                  "byteLength": 128,
                  "base64": null,
                  "outputPath": "target/shaft-mobile/home.png",
                  "warnings": ["Base64 omitted; set includeBase64=true to return inline PNG bytes."]
                }
                """));

        assertAll(
                () -> assertTrue(toolchain.contains("**Platform:** Android")),
                () -> assertTrue(toolchain.contains("**Missing dependencies**")),
                () -> assertTrue(toolchain.contains("| `emulator-5554` | device | emulator |")),
                () -> assertFalse(toolchain.contains("\"nodeAvailable\"")),
                () -> assertTrue(session.contains("**Mode:** web-emulation")),
                () -> assertTrue(session.contains("**Device profile**")),
                () -> assertTrue(tree.contains("**Accessibility tree**")),
                () -> assertTrue(tree.contains("<node text='Sign in'/>")),
                () -> assertTrue(contexts.contains("**Contexts**")),
                () -> assertTrue(contexts.contains("WEBVIEW_chrome")),
                () -> assertTrue(screenshot.contains("**Screenshot:** image/png")),
                () -> assertTrue(screenshot.contains("target/shaft-mobile/home.png")),
                () -> assertFalse(screenshot.contains("\"base64\"")));
    }

    @Test
    void formatsMobileRecordingAndInspectorResponsesWithoutRawJson() {
        String recording = AssistantMarkdown.fromMcpOutput("mobile_record_start", mcpText("""
                {
                  "active": true,
                  "outputPath": "recordings/mobile.json",
                  "mode": "default",
                  "actionCount": 0,
                  "includeSensitiveValues": false,
                  "warnings": []
                }
                """));
        String plan = AssistantMarkdown.fromMcpOutput("mobile_inspector_record_prepare", mcpText("""
                {
                  "confirmationToken": "confirm-123",
                  "platformName": "Android",
                  "readyToStart": true,
                  "confirmationRequired": true,
                  "willProvisionAndroidEmulator": false,
                  "realDeviceAvailable": true,
                  "selectedDeviceId": "emulator-5554",
                  "selectedAndroidAvdName": "Pixel_6",
                  "outputPath": "recordings/inspector.json",
                  "includeSensitiveValues": false,
                  "appiumCapabilities": {},
                  "codeBlocks": [],
                  "nextSteps": ["Review the plan, then call mobile_inspector_record_start with the confirmation token."],
                  "warnings": ["Inspector is not opened until start is explicitly confirmed."]
                }
                """));
        String status = AssistantMarkdown.fromMcpOutput("mobile_inspector_record_status", mcpText("""
                {
                  "active": true,
                  "paused": false,
                  "platformName": "Android",
                  "deviceId": "emulator-5554",
                  "androidAvdName": "Pixel_6",
                  "managedEmulator": false,
                  "outputPath": "recordings/inspector.json",
                  "inspectorUrl": "http://127.0.0.1:4723/inspector",
                  "appiumServerUrl": "http://127.0.0.1:4723",
                  "actionCount": 2,
                  "codeBlocks": [],
                  "warnings": []
                }
                """));

        assertAll(
                () -> assertTrue(recording.contains("**Recording:** active")),
                () -> assertTrue(recording.contains("**Sensitive values:** excluded")),
                () -> assertTrue(recording.contains("`recordings/mobile.json`")),
                () -> assertFalse(recording.contains("\"includeSensitiveValues\"")),
                () -> assertTrue(plan.contains("**Inspector plan:** ready")),
                () -> assertTrue(plan.contains("**Confirmation token:** `confirm-123`")),
                () -> assertTrue(plan.contains("Inspector is not opened until start is explicitly confirmed.")),
                () -> assertFalse(plan.contains("\"appiumCapabilities\"")),
                () -> assertTrue(status.contains("**Inspector recording:** active")),
                () -> assertTrue(status.contains("http://127.0.0.1:4723/inspector")),
                () -> assertFalse(status.contains("\"managedEmulator\"")));
    }

    @Test
    void mobileRecordingStatusSpeaksWebCaptureGlossaryWithReviewCta() {
        String active = AssistantMarkdown.fromMcpOutput("mobile_record_status", mcpText("""
                {
                  "active": true,
                  "outputPath": "recordings/mobile.json",
                  "mode": "native",
                  "actionCount": 3,
                  "includeSensitiveValues": false,
                  "warnings": ["Ignored: recording is not active — call mobile_record_start to capture this step."],
                  "readiness": "RISKY"
                }
                """));
        String stopped = AssistantMarkdown.fromMcpOutput("mobile_record_stop", mcpText("""
                {
                  "active": false,
                  "outputPath": "recordings/mobile.json",
                  "mode": "native",
                  "actionCount": 3,
                  "includeSensitiveValues": false,
                  "warnings": [],
                  "readiness": "READY"
                }
                """));

        assertAll(
                // Glossary parity: "Steps", not "Actions"; readiness pill present.
                () -> assertTrue(active.contains("**Steps:** 3")),
                () -> assertFalse(active.contains("**Actions:**")),
                () -> assertTrue(active.contains("RISKY")),
                () -> assertTrue(active.contains("Ignored: recording is not active")),
                // Review-code CTA fires once stopped with steps (mobile emits no web "state").
                () -> assertFalse(active.contains("Generate a SHAFT test from")),
                () -> assertTrue(stopped.contains("READY")),
                () -> assertTrue(stopped.contains("Review code next")),
                () -> assertTrue(stopped.contains("Generate a SHAFT test from recordings/mobile.json")));
    }

    @Test
    void playwrightRecordingStatusInheritsTheSharedGlossaryRendering() {
        // The Playwright recorder returns the same McpMobileRecordingStatus DTO, and markdown
        // dispatch is structural (not tool-name based), so playwright_record_stop must render the
        // same Steps / Readiness / Review-code treatment as mobile without a dedicated branch.
        String stopped = AssistantMarkdown.fromMcpOutput("playwright_record_stop", mcpText("""
                {
                  "active": false,
                  "outputPath": "recordings/playwright.json",
                  "mode": "playwright",
                  "actionCount": 4,
                  "includeSensitiveValues": false,
                  "warnings": [],
                  "readiness": "READY"
                }
                """));

        assertAll(
                () -> assertTrue(stopped.contains("**Steps:** 4")),
                () -> assertFalse(stopped.contains("**Actions:**")),
                () -> assertTrue(stopped.contains("READY")),
                () -> assertTrue(stopped.contains("Review code next")),
                () -> assertTrue(stopped.contains("Generate a SHAFT test from recordings/playwright.json")));
    }

    @Test
    void formatsDoctorAnalysisReportWithActionsSnippetsAndReportPaths() {
        String markdown = AssistantMarkdown.fromMcpOutput("doctor_analyze_failed_allure", mcpText("""
                {
                  "schemaVersion": "1.0",
                  "status": "DETERMINISTIC",
                  "bundleId": "bundle-123",
                  "primaryCause": "LOCATOR",
                  "confidence": "HIGH",
                  "summary": "Button locator no longer matches the page.",
                  "actions": [
                    {"title":"Update locator","action":"Replace the stale CSS selector.","status":"SUGGESTED"}
                  ],
                  "codeBlocks": [
                    {"title":"Locator fix","language":"java","code":"driver.element().click(SHAFT.GUI.Locator.id(\\"login\\"));","copyPasteReady":true}
                  ],
                  "providerFallback": {"used":false,"reason":"AI advisory disabled by default."},
                  "bundlePath": "target/shaft-doctor/evidence-bundle.json",
                  "jsonReportPath": "target/shaft-doctor/doctor-report.json",
                  "markdownReportPath": "target/shaft-doctor/doctor-report.md",
                  "warnings": []
                }
                """));

        assertAll(
                () -> assertTrue(markdown.contains("**Doctor:** DETERMINISTIC")),
                () -> assertTrue(markdown.contains("**Primary cause:** LOCATOR")),
                () -> assertTrue(markdown.contains("Button locator no longer matches the page.")),
                () -> assertTrue(markdown.contains("**Recommended actions**")),
                () -> assertTrue(markdown.contains("Replace the stale CSS selector.")),
                () -> assertTrue(markdown.contains("**Fix snippets**")),
                () -> assertTrue(markdown.contains("```java")),
                () -> assertTrue(markdown.contains("driver.element().click(SHAFT.GUI.Locator.id")),
                () -> assertFalse(markdown.contains("driver.findElement")),
                () -> assertTrue(markdown.contains("target/shaft-doctor/doctor-report.json")),
                () -> assertTrue(markdown.contains("AI advisory disabled by default.")),
                () -> assertFalse(markdown.contains("\"schemaVersion\"")));
    }

    @Test
    void rejectsNativeSeleniumGeneratedJavaSnippetsWithRegenerationTools() {
        String markdown = AssistantMarkdown.fromMcpOutput("capture_code_blocks", mcpText("""
                {
                  "codeBlocks": [
                    {"language":"java","code":"driver.get(\\"https://example.com\\");\\ndriver.findElement(By.id(\\"login\\")).click();"}
                  ]
                }
                """));

        assertAll(
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertTrue(markdown.contains("Ask the agent to regenerate")),
                () -> assertTrue(markdown.contains("`shaft_guide_search`")),
                () -> assertTrue(markdown.contains("`test_automation_scenarios`")),
                () -> assertTrue(markdown.contains("`test_code_guardrails_check`")),
                () -> assertTrue(markdown.contains("SHAFT-only Java")),
                () -> assertFalse(markdown.contains("driver.get")),
                () -> assertFalse(markdown.contains("driver.findElement")));
    }

    @Test
    void rejectsShaftLocatorXpathGeneratedJavaSnippets() {
        String markdown = AssistantMarkdown.fromMcpOutput("capture_code_blocks", mcpText("""
                {
                  "codeBlocks": [
                    {"language":"java","code":"driver.element().click(SHAFT.GUI.Locator.xpath(\\"//button[@type='submit']\\"));"}
                  ]
                }
                """));

        assertAll(
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertTrue(markdown.contains("SHAFT.GUI.Locator.xpath")),
                () -> assertFalse(markdown.contains("driver.element().click")));
    }

    @Test
    void formatsAutomationScenariosWithoutRawJson() {
        String markdown = AssistantMarkdown.fromMcpOutput("test_automation_scenarios", mcpText("""
                {
                  "schemaVersion": "1.0",
                  "area": "all",
                  "intent": "navigate to duckduckgo, search for shaft_engine, open the first result",
                  "scenarios": [
                    {
                      "id": "web-search-result",
                      "title": "Search and open the first result",
                      "areas": ["web"],
                      "summary": "Initialize a browser, search, open the first result, then assert the URL.",
                      "tools": ["driver_initialize", "browser_open_intent", "test_code_guardrails_check"]
                    }
                  ]
                }
                """));

        assertAll(
                () -> assertTrue(markdown.contains("**Automation scenarios**")),
                () -> assertTrue(markdown.contains("Search and open the first result")),
                () -> assertTrue(markdown.contains("Initialize a browser")),
                () -> assertTrue(markdown.contains("`driver_initialize`")),
                () -> assertFalse(markdown.contains("\"schemaVersion\"")),
                () -> assertFalse(markdown.contains("\"scenarios\"")));
    }

    @Test
    void formatsCodingPartnerPlanWithoutRawJson() {
        String markdown = AssistantMarkdown.fromMcpOutput("shaft_coding_partner_plan", mcpText("""
                {
                  "schemaVersion": "1.2",
                  "workingSetSummary": "Intent: Log in; backend: WebDriver; current source: src/test/java/pages/LoginPage.java; reuse candidates: 1; evidence paths: 1",
                  "backend": "WebDriver",
                  "reuseMatches": [
                    {
                      "sourcePath": "src/test/java/pages/LoginPage.java",
                      "packageName": "pages",
                      "className": "LoginPage",
                      "driverVariableName": "browser",
                      "insertionAnchors": ["loginAs"],
                      "score": 176,
                      "locatorSummaries": ["emailInput = SHAFT.GUI.Locator.inputField(\\"Email\\")"],
                      "actionSummaries": ["loginAs"]
                    }
                  ],
                  "stepPlan": [
                    {
                      "index": 1,
                      "instruction": "Log in with valid credentials",
                      "reuseHint": "Reuse src/test/java/pages/LoginPage.java after loginAs before creating duplicate locators, actions, or tests.",
                      "proofTool": "browser_open_intent"
                    }
                  ],
                  "recommendedTargetSourcePath": "src/test/java/pages/LoginPage.java",
                  "recommendedInsertionAnchor": "loginAs",
                  "missingCodeItems": ["Replace raw Selenium selected text with SHAFT syntax."],
                  "suggestedMcpCalls": ["capture_target_candidates", "test_code_guardrails_check"],
                  "nextActions": [
                    {
                      "label": "Open the target URL and rank locators",
                      "toolName": "browser_open_intent",
                      "arguments": {"targetUrl": "", "userIntent": "Log in"},
                      "requiresConfirmation": true,
                      "rationale": ["The user must confirm the URL before browser automation runs."]
                    }
                  ],
                  "verificationCommand": "mvn -q -DskipTests test-compile",
                  "evidencePaths": ["target/shaft-traces/latest"],
                  "warnings": ["Preview only; do not edit source before approval."]
                }
                """));

        assertAll(
                () -> assertTrue(markdown.contains("**Coding partner plan:** 1.2")),
                () -> assertTrue(markdown.contains("**Recommended target:** `src/test/java/pages/LoginPage.java`")),
                () -> assertTrue(markdown.contains("**Insertion anchor:** `loginAs`")),
                () -> assertTrue(markdown.contains("**Plan steps**")),
                () -> assertTrue(markdown.contains("Reuse src/test/java/pages/LoginPage.java")),
                () -> assertTrue(markdown.contains("Proof: `browser_open_intent`")),
                () -> assertTrue(markdown.contains("**Reuse matches**")),
                () -> assertTrue(markdown.contains("`emailInput = SHAFT.GUI.Locator.inputField(")),
                () -> assertTrue(markdown.contains("**Suggested MCP calls**")),
                () -> assertTrue(markdown.contains("**Next actions**")),
                () -> assertTrue(markdown.contains("Open the target URL and rank locators (`browser_open_intent`)")),
                () -> assertTrue(markdown.contains("confirm context first")),
                () -> assertTrue(markdown.contains("`mvn -q -DskipTests test-compile`")),
                () -> assertTrue(markdown.contains("Preview only; do not edit source before approval.")),
                () -> assertFalse(markdown.contains("\"reuseMatches\"")),
                () -> assertFalse(markdown.contains("\"stepPlan\"")),
                () -> assertFalse(markdown.contains("\"nextActions\"")));
    }

    @Test
    void unknownJsonCanUseAgentFormatterButKnownResponsesDoNot() {
        assertAll(
                () -> assertTrue(AssistantMarkdown.shouldFormatWithAgent("browser_unknown", "{\"unexpected\":true}")),
                () -> assertTrue(AssistantMarkdown.shouldFormatWithAgent("browser_unknown", "{\"status\":\"MYSTERY\"}")),
                () -> assertFalse(AssistantMarkdown.shouldFormatWithAgent("autobot_local_agent_clients", "[]")),
                () -> assertFalse(AssistantMarkdown.shouldFormatWithAgent("unknown", "{\"warnings\":[\"safe\"]}")));
    }

    @Test
    void formatsProviderChatStructuredCodegen() {
        String json = """
                {"status":"SUCCESS","provider":"gemini","model":"gemini-3.5-flash","mode":"PLAN",
                 "answer":"Added a sign-in test","summary":"signs in and asserts the dashboard",
                 "codeBlocks":[{"language":"java","path":"src/test/java/tests/SignInTest.java",
                   "insertionAnchor":"loginAs","code":"driver.element().click(signIn);"}],
                 "citedGuideUrls":["https://shafthq.github.io/docs/testing/web"],
                 "locatorAssumptions":["signIn assumed by label"],"guardrailStatus":"PASSED",
                 "warnings":[],"fallbackReason":""}
                """;

        String markdown = AssistantMarkdown.fromMcpOutput("autobot_provider_chat", mcpText(json));

        assertAll(
                () -> assertTrue(markdown.contains("Summary:")),
                () -> assertTrue(markdown.contains("SignInTest.java")),
                () -> assertTrue(markdown.contains("after `loginAs`")),
                () -> assertTrue(markdown.contains("driver.element().click(signIn);")),
                () -> assertTrue(markdown.contains("Cited SHAFT guides")),
                () -> assertTrue(markdown.contains("Unverified locator assumptions")),
                () -> assertTrue(markdown.contains("**Guardrails:** PASSED")));
    }

    @Test
    void formatsProviderStatusReadiness() {
        String json = """
                {"schemaVersion":"1.0","provider":"gemini","model":"gemini-3.5-flash","apiKeyPresent":true,
                 "apiKeyEnvironmentVariable":"GEMINI_API_KEY","structuredOutputSupported":true,
                 "supportedModes":"ASK, PLAN","warnings":[]}
                """;

        String markdown = AssistantMarkdown.fromMcpOutput("autobot_provider_status", mcpText(json));

        assertAll(
                () -> assertTrue(markdown.contains("**Provider:** gemini")),
                () -> assertTrue(markdown.contains("**API key:** present")),
                () -> assertTrue(markdown.contains("**Structured output:** yes")),
                () -> assertFalse(markdown.contains("GEMINI_API_KEY=")));
    }

    @Test
    void formatsCodingPartnerDiffPreview() {
        String json = """
                {"schemaVersion":"1.0","targetSourcePath":"src/test/java/pages/LoginPage.java",
                 "insertionAnchor":"loginAs","targetExists":true,"insertedLineCount":4,
                 "unifiedDiff":"--- a/x\\n+++ b/x\\n@@ -1,1 +1,2 @@\\n class X {}\\n+// added",
                 "warnings":["Diff is preview-only; apply changes in IntelliJ under explicit user approval."]}
                """;

        String markdown = AssistantMarkdown.fromMcpOutput("shaft_coding_partner_diff", mcpText(json));

        assertAll(
                () -> assertTrue(markdown.contains("Patch preview")),
                () -> assertTrue(markdown.contains("Anchor")),
                () -> assertTrue(markdown.contains("```diff")),
                () -> assertTrue(markdown.contains("preview-only")),
                () -> assertTrue(markdown.contains("/verify")));
    }

    @Test
    void formatsVerificationResult() {
        String pass = """
                {"schemaVersion":"1.0","status":"PASSED","exitCode":0,"timedOut":false,
                 "command":["mvn","-q","test-compile","--offline"],"outputSummary":"BUILD SUCCESS","warnings":[]}
                """;
        String fail = """
                {"schemaVersion":"1.0","status":"FAILED","exitCode":1,"timedOut":false,
                 "command":["mvn","test"],"outputSummary":"BUILD FAILURE","warnings":[]}
                """;

        String passMarkdown = AssistantMarkdown.fromMcpOutput("verify_run_focused", mcpText(pass));
        String failMarkdown = AssistantMarkdown.fromMcpOutput("verify_run_focused", mcpText(fail));

        assertAll(
                () -> assertTrue(passMarkdown.contains("PASSED")),
                () -> assertTrue(passMarkdown.contains("mvn -q test-compile --offline")),
                () -> assertTrue(passMarkdown.contains("BUILD SUCCESS")),
                () -> assertTrue(failMarkdown.contains("FAILED")),
                () -> assertTrue(failMarkdown.contains("BUILD FAILURE")));
    }

    @Test
    void humanizeErrorStripsFullyQualifiedExceptionClassNames() {
        String humanized = AssistantMarkdown.humanizeError(
                new IllegalStateException("java.util.NoSuchElementException: Index 5 out of bounds for length 3"));

        assertEquals("Index 5 out of bounds for length 3", humanized);
    }

    @Test
    void humanizeErrorLeavesAlreadyPlainMessagesUnchanged() {
        String humanized = AssistantMarkdown.humanizeError(
                new IOException("Timed out waiting for SHAFT MCP response."));

        assertEquals("Timed out waiting for SHAFT MCP response.", humanized);
    }

    @Test
    void humanizeErrorUnwrapsCompletionExceptionToTheRealCause() {
        String humanized = AssistantMarkdown.humanizeError(
                new CompletionException(new IOException("Connection refused")));

        assertEquals("Connection refused", humanized);
    }

    @Test
    void humanizeErrorNeverReturnsNullOrBlankEvenWithoutAMessage() {
        assertAll(
                () -> assertEquals("", AssistantMarkdown.humanizeError(null)),
                () -> assertTrue(AssistantMarkdown.humanizeError(new NullPointerException())
                        .contains("NullPointerException")),
                () -> assertFalse(AssistantMarkdown.humanizeError(new NullPointerException()).isBlank()));
    }

    private static String mcpText(String text) {
        JsonObject item = new JsonObject();
        item.addProperty("type", "text");
        item.addProperty("text", text);
        JsonArray content = new JsonArray();
        content.add(item);
        JsonObject result = new JsonObject();
        result.add("content", content);
        result.addProperty("isError", false);
        return result.toString();
    }
}
