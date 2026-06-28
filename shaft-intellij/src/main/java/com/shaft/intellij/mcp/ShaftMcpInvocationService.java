package com.shaft.intellij.mcp;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftCredentialService;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Project service that invokes SHAFT MCP tools through the configured stdio command.
 */
public final class ShaftMcpInvocationService {
    private static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(90);

    private final Project project;

    /**
     * Creates the project-scoped invocation service.
     *
     * @param project IntelliJ project
     */
    public ShaftMcpInvocationService(@NotNull Project project) {
        this.project = project;
    }

    /**
     * Returns the project-level invocation service.
     *
     * @param project IntelliJ project
     * @return invocation service
     */
    public static ShaftMcpInvocationService getInstance(Project project) {
        return project.getService(ShaftMcpInvocationService.class);
    }

    /**
     * Invokes a SHAFT MCP tool in a pooled background task.
     *
     * @param toolName MCP tool name
     * @param arguments JSON object arguments
     * @return future result
     */
    public CompletableFuture<ShaftMcpToolResult> invokeTool(String toolName, JsonObject arguments) {
        ShaftSettingsState.Settings settings = ShaftSettingsState.getInstance().getState();
        if (settings.mcpCommand == null || settings.mcpCommand.isBlank()) {
            return CompletableFuture.completedFuture(ShaftMcpToolResult.failure(
                    "Configure the SHAFT MCP stdio command in Settings | SHAFT."));
        }
        List<String> command = ShaftCommandLine.parse(settings.mcpCommand);
        if (command.isEmpty()) {
            return CompletableFuture.completedFuture(ShaftMcpToolResult.failure(
                    "Configure the SHAFT MCP stdio command in Settings | SHAFT."));
        }
        return CompletableFuture.supplyAsync(() -> invoke(command, toolName, arguments, settings));
    }

    private ShaftMcpToolResult invoke(
            List<String> command,
            String toolName,
            JsonObject arguments,
            ShaftSettingsState.Settings settings) {
        Path workingDirectory = project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
        Map<String, String> environment = providerEnvironment(settings);
        try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, workingDirectory, environment)) {
            JsonObject result = client.callTool(toolName, arguments == null ? new JsonObject() : arguments,
                    DEFAULT_TIMEOUT);
            return ShaftMcpToolResult.success(result.toString());
        } catch (Exception exception) {
            return ShaftMcpToolResult.failure(exception.getMessage());
        }
    }

    private static Map<String, String> providerEnvironment(ShaftSettingsState.Settings settings) {
        if (!settings.passProviderApiKeysToMcp) {
            return Map.of();
        }
        ShaftCredentialService credentials = ShaftCredentialService.getInstance();
        Map<String, String> environment = new LinkedHashMap<>();
        putIfPresent(environment, "OPENAI_API_KEY", credentials.apiKey("OPENAI_API_KEY"));
        putIfPresent(environment, "ANTHROPIC_API_KEY", credentials.apiKey("ANTHROPIC_API_KEY"));
        putIfPresent(environment, "GITHUB_TOKEN", credentials.apiKey("GITHUB_TOKEN"));
        return environment;
    }

    private static void putIfPresent(Map<String, String> environment, String key, String value) {
        if (value != null && !value.isBlank()) {
            environment.put(key, value);
        }
    }
}
