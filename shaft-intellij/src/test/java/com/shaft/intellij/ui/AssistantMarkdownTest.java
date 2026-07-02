package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantMarkdownTest {
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
                () -> assertTrue(markdown.contains("**Warnings**")),
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
                () -> assertTrue(markdown.contains("**Output:** `recordings/intellij-capture.json`")),
                () -> assertTrue(markdown.contains("The recorder process is no longer reachable.")),
                () -> assertFalse(markdown.contains("\"processId\"")));
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
    void unknownJsonCanUseAgentFormatterButKnownResponsesDoNot() {
        assertAll(
                () -> assertTrue(AssistantMarkdown.shouldFormatWithAgent("browser_unknown", "{\"unexpected\":true}")),
                () -> assertTrue(AssistantMarkdown.shouldFormatWithAgent("browser_unknown", "{\"status\":\"MYSTERY\"}")),
                () -> assertFalse(AssistantMarkdown.shouldFormatWithAgent("autobot_local_agent_clients", "[]")),
                () -> assertFalse(AssistantMarkdown.shouldFormatWithAgent("unknown", "{\"warnings\":[\"safe\"]}")));
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
