package com.shaft.intellij.mcp;

import com.google.gson.JsonElement;
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
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

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
        return startTool(toolName, arguments).future();
    }

    /**
     * Returns the latest list of tools from SHAFT MCP.
     *
     * @return future MCP result
     */
    public CompletableFuture<ShaftMcpToolResult> listTools() {
        return startListTools().future();
    }

    /**
     * Starts a cancellable SHAFT MCP tools/list request.
     *
     * @return cancellable invocation
     */
    public ShaftMcpInvocation startListTools() {
        ShaftSettingsState.Settings settings = ShaftSettingsState.getInstance().getState();
        List<String> command = command(settings);
        if (command.isEmpty()) {
            return completed(CONFIGURE_MESSAGE);
        }
        AtomicReference<ShaftMcpStdioClient> clientReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> listTools(command, settings, clientReference, cancellationRequested));
        return new ShaftMcpInvocation(future, () -> cancel(clientReference, cancellationRequested));
    }

    /**
     * Starts a cancellable SHAFT MCP tool invocation.
     *
     * @param toolName MCP tool name
     * @param arguments JSON object arguments
     * @return cancellable invocation
     */
    public ShaftMcpInvocation startTool(String toolName, JsonObject arguments) {
        ShaftSettingsState.Settings settings = ShaftSettingsState.getInstance().getState();
        List<String> command = command(settings);
        if (command.isEmpty()) {
            return completed(CONFIGURE_MESSAGE);
        }
        AtomicReference<ShaftMcpStdioClient> clientReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> invoke(command, toolName, arguments, settings, clientReference, cancellationRequested));
        return new ShaftMcpInvocation(future, () -> cancel(clientReference, cancellationRequested));
    }

    /**
     * Tests the configured SHAFT MCP command by running the initialization handshake.
     *
     * @return cancellable invocation
     */
    public ShaftMcpInvocation testConnection() {
        ShaftSettingsState.Settings settings = ShaftSettingsState.getInstance().getState();
        List<String> command = command(settings);
        if (command.isEmpty()) {
            return completed(CONFIGURE_MESSAGE);
        }
        AtomicReference<ShaftMcpStdioClient> clientReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> initialize(command, settings, clientReference, cancellationRequested));
        return new ShaftMcpInvocation(future, () -> cancel(clientReference, cancellationRequested));
    }

    private ShaftMcpToolResult invoke(
            List<String> command,
            String toolName,
            JsonObject arguments,
            ShaftSettingsState.Settings settings,
            AtomicReference<ShaftMcpStdioClient> clientReference,
            AtomicBoolean cancellationRequested) {
        Path workingDirectory = project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
        Map<String, String> environment = providerEnvironment(settings);
        try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, workingDirectory, environment)) {
            clientReference.set(client);
            if (cancellationRequested.get()) {
                throw new CancellationException("Operation cancelled");
            }
            JsonElement result = client.callTool(toolName, arguments == null ? new JsonObject() : arguments,
                    DEFAULT_TIMEOUT);
            return ShaftMcpToolResult.success(result.toString());
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            return ShaftMcpToolResult.failure(exception.getMessage());
        } finally {
            clientReference.set(null);
        }
    }

    private ShaftMcpToolResult initialize(
            List<String> command,
            ShaftSettingsState.Settings settings,
            AtomicReference<ShaftMcpStdioClient> clientReference,
            AtomicBoolean cancellationRequested) {
        Path workingDirectory = project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
        Map<String, String> environment = providerEnvironment(settings);
        try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, workingDirectory, environment)) {
            clientReference.set(client);
            if (cancellationRequested.get()) {
                throw new CancellationException("Operation cancelled");
            }
            return ShaftMcpToolResult.success(client.initializeOnly(DEFAULT_TIMEOUT));
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            return ShaftMcpToolResult.failure(exception.getMessage());
        } finally {
            clientReference.set(null);
        }
    }

    private ShaftMcpToolResult listTools(
            List<String> command,
            ShaftSettingsState.Settings settings,
            AtomicReference<ShaftMcpStdioClient> clientReference,
            AtomicBoolean cancellationRequested) {
        Path workingDirectory = project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
        Map<String, String> environment = providerEnvironment(settings);
        try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, workingDirectory, environment)) {
            clientReference.set(client);
            if (cancellationRequested.get()) {
                throw new CancellationException("Operation cancelled");
            }
            JsonElement result = client.listTools(DEFAULT_TIMEOUT);
            return ShaftMcpToolResult.success(result.toString());
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            return ShaftMcpToolResult.failure(exception.getMessage());
        } finally {
            clientReference.set(null);
        }
    }

    private static ShaftMcpInvocation completed(String message) {
        return new ShaftMcpInvocation(CompletableFuture.completedFuture(ShaftMcpToolResult.failure(message)), () -> {
        });
    }

    private static void cancel(AtomicReference<ShaftMcpStdioClient> clientReference,
                              AtomicBoolean cancellationRequested) {
        cancellationRequested.set(true);
        ShaftMcpStdioClient client = clientReference.getAndSet(null);
        if (client != null) {
            client.cancel();
        }
    }

    private static List<String> command(ShaftSettingsState.Settings settings) {
        if (settings == null || settings.mcpCommand == null || settings.mcpCommand.isBlank()) {
            return List.of();
        }
        return ShaftCommandLine.parse(settings.mcpCommand);
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

    private static final String CONFIGURE_MESSAGE = "Configure the SHAFT MCP stdio command in Settings | SHAFT.";
}
