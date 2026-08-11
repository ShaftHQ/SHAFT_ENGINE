package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.intellij.mcp.ShaftCommandLine;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class AssistantGeminiLiveE2ETest {
    @Test
    void livePluginMcpGeminiPromptReturnsExpectedProviderResponse() throws Exception {
        Assumptions.assumeTrue(Boolean.getBoolean("shaft.intellij.liveGemini"),
                "Set -Dshaft.intellij.liveGemini=true to run the live Gemini IntelliJ MCP test.");
        String apiKey = System.getenv("GEMINI_API_KEY");
        Assumptions.assumeTrue(apiKey != null && !apiKey.isBlank(),
                "Set GEMINI_API_KEY to run the live Gemini IntelliJ MCP test.");
        String commandLine = System.getProperty("shaft.intellij.liveMcpCommand", "").trim();
        Assumptions.assumeTrue(!commandLine.isBlank(),
                "Set -Dshaft.intellij.liveMcpCommand to a SHAFT MCP stdio command.");
        // The default flash model is periodically load-shed by Google with 503 "high demand"
        // responses; override the model when live verification needs a less contended one.
        String model = System.getProperty("shaft.intellij.liveGeminiModel", "gemini-3.5-flash").trim();

        Path workspace = Path.of(System.getProperty("shaft.intellij.workspaceRoot", ".."))
                .toAbsolutePath()
                .normalize();
        // ShaftMcpStdioClient spawns the MCP server with `workspace` as its ProcessBuilder working
        // directory (ShaftMcpStdioClient.java:79-80); CreateProcess/exec fails with a raw IOException
        // when that directory does not already exist -- reproduced empirically (nightly run
        // 30146701546: "java.io.IOException at AssistantGeminiLiveE2ETest.java:139"). CI passes a
        // fresh, never-created "${GITHUB_WORKSPACE}/build/live-tools/gemini" here, so it must be
        // created before use, the same way every other live-tool-E2E test's workspace already is
        // (e.g. ShaftAssistantPanelLiveDiagnosticsToolE2ETest.LiveContext.assumeConfigured()).
        Files.createDirectories(workspace);
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "gemini";
        settings.cloudModel = model;
        settings.passProviderApiKeysToMcp = false;
        Map<String, String> environment = new HashMap<>(mcpEnvironment(settings));
        environment.put("GEMINI_API_KEY", apiKey);
        environment.put("SHAFT_MCP_WORKSPACE_ROOT", workspace.toString());

        String options = environment.getOrDefault("JAVA_TOOL_OPTIONS", "");
        assertTrue(options.contains("-Dpilot.ai.provider=gemini"));
        assertTrue(options.contains("-Dpilot.ai.gemini.model=" + model));

        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Reply with the exact token SHAFT_GEMINI_LIVE_OK.",
                AssistantCommand.Selection.cloud("", ""),
                "ASK",
                workspace.toString(),
                "",
                false);

        assertEquals("autobot_provider_chat", invocation.toolName());
        assertEquals("gemini", invocation.arguments().get("provider").getAsString());
        assertEquals("ASK", invocation.arguments().get("mode").getAsString());

        JsonElement result = callTool(
                ShaftCommandLine.parse(commandLine),
                workspace,
                environment,
                invocation.toolName(),
                invocation.arguments(),
                Duration.ofSeconds(150));
        String responseText = mcpText(result);
        JsonObject response;
        try {
            response = JsonParser.parseString(responseText).getAsJsonObject();
        } catch (com.google.gson.JsonSyntaxException exception) {
            fail("MCP text response was not JSON. Preview: " + preview(responseText), exception);
            return;
        }
        String answer = response.get("answer").getAsString();

        assertEquals("SUCCESS", response.get("status").getAsString(), response.toString());
        assertEquals("gemini", response.get("provider").getAsString());
        assertTrue(response.get("model").getAsString().startsWith(model), response.get("model").getAsString());
        assertEquals("ASK", response.get("mode").getAsString());
        assertTrue(answer.contains("SHAFT_GEMINI_LIVE_OK"), answer);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, String> mcpEnvironment(ShaftSettingsState.Settings settings) throws Exception {
        Class<?> environmentClass = Class.forName("com.shaft.intellij.mcp.ShaftMcpEnvironment");
        Method forSettings = environmentClass.getDeclaredMethod("forSettings", ShaftSettingsState.Settings.class);
        forSettings.setAccessible(true);
        return (Map<String, String>) forSettings.invoke(null, settings);
    }

    private static JsonElement callTool(
            List<String> command,
            Path workspace,
            Map<String, String> environment,
            String toolName,
            JsonObject arguments,
            Duration timeout) throws Exception {
        Class<?> clientClass = Class.forName("com.shaft.intellij.mcp.ShaftMcpStdioClient");
        Constructor<?> constructor = clientClass.getDeclaredConstructor(List.class, Path.class, Map.class);
        constructor.setAccessible(true);
        Object client = constructor.newInstance(command, workspace, environment);
        try {
            Method callTool = clientClass.getDeclaredMethod("callTool", String.class, JsonObject.class, Duration.class);
            callTool.setAccessible(true);
            return (JsonElement) callTool.invoke(client, toolName, arguments, timeout);
        } catch (InvocationTargetException exception) {
            Throwable cause = exception.getCause();
            if (cause instanceof IOException ioException) {
                throw ioException;
            }
            if (cause instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            throw exception;
        } finally {
            Method close = clientClass.getDeclaredMethod("close");
            close.setAccessible(true);
            close.invoke(client);
        }
    }

    private static String mcpText(JsonElement result) {
        JsonElement current = result;
        for (int depth = 0; depth < 3; depth++) {
            if (current == null || !current.isJsonObject()) {
                break;
            }
            JsonObject object = current.getAsJsonObject();
            if (!object.has("content")) {
                break;
            }
            JsonObject item = object.getAsJsonArray("content").get(0).getAsJsonObject();
            String text = item.get("text").getAsString();
            JsonElement nested;
            try {
                nested = JsonParser.parseString(text);
            } catch (com.google.gson.JsonSyntaxException exception) {
                return text;
            }
            if (!nested.isJsonObject()) {
                return text;
            }
            current = nested;
        }
        return current.toString();
    }

    private static String preview(String text) {
        String normalized = text == null ? "" : text.replaceAll("\\s+", " ").trim();
        return normalized.length() <= 500 ? normalized : normalized.substring(0, 500);
    }
}
