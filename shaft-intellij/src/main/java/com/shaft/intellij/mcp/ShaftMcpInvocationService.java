package com.shaft.intellij.mcp;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.time.Duration;
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
    private static final String DISCONNECTED_MESSAGE = "MCP connection is disconnected. Click 'Reconnect' to restore service.";

    private final Project project;
    private final ShaftMcpConnectionState connectionState;

    /**
     * Creates the project-scoped invocation service.
     *
     * @param project IntelliJ project
     */
    public ShaftMcpInvocationService(@NotNull Project project) {
        this.project = project;
        this.connectionState = project.getService(ShaftMcpConnectionState.class);
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
        List<String> command = verifiedCommand(settings);
        if (command.isEmpty()) {
            return completed(CONFIGURE_MESSAGE);
        }
        AtomicReference<ShaftMcpStdioClient> clientReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> listTools(command, settings, clientReference, cancellationRequested));
        return new ShaftMcpInvocation(
                future,
                () -> cancel(clientReference, cancellationRequested, false),
                () -> cancel(clientReference, cancellationRequested, true));
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
        List<String> command = verifiedCommand(settings);
        if (command.isEmpty()) {
            return completed(CONFIGURE_MESSAGE);
        }
        if (connectionState != null && !connectionState.isConnected()) {
            return completed(DISCONNECTED_MESSAGE);
        }
        AtomicReference<ShaftMcpStdioClient> clientReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> invoke(command, toolName, arguments, settings, clientReference, cancellationRequested));
        return new ShaftMcpInvocation(
                future,
                () -> cancel(clientReference, cancellationRequested, false),
                () -> cancel(clientReference, cancellationRequested, true));
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
        return new ShaftMcpInvocation(
                future,
                () -> cancel(clientReference, cancellationRequested, false),
                () -> cancel(clientReference, cancellationRequested, true));
    }

    private ShaftMcpToolResult invoke(
            List<String> command,
            String toolName,
            JsonObject arguments,
            ShaftSettingsState.Settings settings,
            AtomicReference<ShaftMcpStdioClient> clientReference,
            AtomicBoolean cancellationRequested) {
        Path workingDirectory = project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
        try {
            Map<String, String> environment = ShaftMcpProjectScope.environmentForProject(
                    ShaftMcpEnvironment.forSettings(settings), workingDirectory);
            List<String> scopedCommand = ShaftMcpProjectScope.commandForProject(command, workingDirectory);
            try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(scopedCommand, workingDirectory, environment)) {
                clientReference.set(client);
                if (cancellationRequested.get()) {
                    throw new CancellationException("Operation cancelled");
                }
                JsonElement result = client.callTool(toolName, arguments == null ? new JsonObject() : arguments,
                        DEFAULT_TIMEOUT);
                return ShaftMcpToolResult.success(result.toString());
            }
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            McpInvocationError category = McpInvocationError.categorize(exception);
            return ShaftMcpToolResult.failure(category.message(), category, category.recoveryAction());
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
        try {
            Map<String, String> environment = ShaftMcpProjectScope.environmentForProject(
                    ShaftMcpEnvironment.forSettings(settings), workingDirectory);
            List<String> scopedCommand = ShaftMcpProjectScope.commandForProject(command, workingDirectory);
            try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(scopedCommand, workingDirectory, environment)) {
                clientReference.set(client);
                if (cancellationRequested.get()) {
                    throw new CancellationException("Operation cancelled");
                }
                return ShaftMcpToolResult.success(scopedMessage(client.initializeOnly(DEFAULT_TIMEOUT), workingDirectory));
            }
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            McpInvocationError category = McpInvocationError.categorize(exception);
            return ShaftMcpToolResult.failure(category.message(), category, category.recoveryAction());
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
        try {
            Map<String, String> environment = ShaftMcpProjectScope.environmentForProject(
                    ShaftMcpEnvironment.forSettings(settings), workingDirectory);
            List<String> scopedCommand = ShaftMcpProjectScope.commandForProject(command, workingDirectory);
            try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(scopedCommand, workingDirectory, environment)) {
                clientReference.set(client);
                if (cancellationRequested.get()) {
                    throw new CancellationException("Operation cancelled");
                }
                JsonElement result = client.listTools(DEFAULT_TIMEOUT);
                return ShaftMcpToolResult.success(result.toString());
            }
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            McpInvocationError category = McpInvocationError.categorize(exception);
            return ShaftMcpToolResult.failure(category.message(), category, category.recoveryAction());
        } finally {
            clientReference.set(null);
        }
    }

    private static ShaftMcpInvocation completed(String message) {
        return new ShaftMcpInvocation(CompletableFuture.completedFuture(ShaftMcpToolResult.failure(message)), () -> {
        });
    }

    private static void cancel(AtomicReference<ShaftMcpStdioClient> clientReference,
                               AtomicBoolean cancellationRequested,
                               boolean force) {
        cancellationRequested.set(true);
        ShaftMcpStdioClient client = force ? clientReference.getAndSet(null) : clientReference.get();
        if (client != null) {
            if (force) {
                client.kill();
            } else {
                client.cancel();
            }
        }
    }

    private static List<String> command(ShaftSettingsState.Settings settings) {
        if (settings == null || settings.mcpCommand == null || settings.mcpCommand.isBlank()) {
            return List.of();
        }
        return ShaftCommandLine.parse(settings.mcpCommand);
    }

    private static List<String> verifiedCommand(ShaftSettingsState.Settings settings) {
        return settings != null && settings.mcpReady() ? command(settings) : List.of();
    }

    private static String scopedMessage(String message, Path workingDirectory) {
        String root = workingDirectory.toAbsolutePath().normalize().toString();
        return message + "\n\nMCP workspace: " + root
                + "\nuser.dir: " + root
                + "\nshaft.mcp.workspaceRoot: " + root
                + "\nSHAFT_MCP_WORKSPACE_ROOT: " + root;
    }

    private static final String CONFIGURE_MESSAGE = "Configure the SHAFT MCP stdio command in Settings | SHAFT.";
}
