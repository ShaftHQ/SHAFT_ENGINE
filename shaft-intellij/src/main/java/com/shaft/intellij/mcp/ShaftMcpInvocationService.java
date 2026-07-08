package com.shaft.intellij.mcp;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
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
 *
 * <p>Tool calls share one long-lived MCP server process per project. Session-starting tools
 * (SHAFT Capture recordings, live browser/mobile drivers) keep their state inside that process,
 * so it must survive across tool calls: a recording started by {@code capture_start} stays
 * collecting until {@code capture_stop} arrives on the same process. The shared process is
 * respawned transparently when it dies or when the configured command changes, and is shut down
 * gracefully (stdin end-of-stream first) so live sessions can release their browsers.</p>
 */
public final class ShaftMcpInvocationService implements Disposable {
    private static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(90);
    private static final String DISCONNECTED_MESSAGE = "MCP connection is disconnected. Click 'Reconnect' to restore service.";

    private final Project project;
    private final ShaftMcpConnectionState connectionState;
    private final ShaftMcpClientFactory clientFactory;
    private final Object clientLock = new Object();
    private ShaftMcpStdioClient sharedClient;
    private List<String> sharedCommand = List.of();
    private Map<String, String> sharedEnvironment = Map.of();
    private Path sharedWorkingDirectory;

    /**
     * Creates the project-scoped invocation service.
     *
     * @param project IntelliJ project
     */
    public ShaftMcpInvocationService(@NotNull Project project) {
        this(project, ShaftMcpStdioClient::new);
    }

    /**
     * Test seam: tolerates a {@code null} project so unit tests can exercise the shared-client pool
     * without the IntelliJ platform. Not public API.
     *
     * @param project       IntelliJ project, or {@code null} in tests
     * @param clientFactory factory used to spawn the shared MCP server process
     */
    ShaftMcpInvocationService(Project project, ShaftMcpClientFactory clientFactory) {
        this.project = project;
        this.connectionState = project == null ? null : project.getService(ShaftMcpConnectionState.class);
        this.clientFactory = clientFactory;
    }

    /**
     * Spawns the shared MCP server process; overridable in tests to avoid real process creation.
     */
    @FunctionalInterface
    interface ShaftMcpClientFactory {
        ShaftMcpStdioClient create(List<String> command, Path workingDirectory, Map<String, String> environment)
                throws IOException;
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
                () -> call(command, settings, clientReference, cancellationRequested,
                        client -> client.listTools(DEFAULT_TIMEOUT)));
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
                () -> call(command, settings, clientReference, cancellationRequested,
                        client -> client.callTool(toolName,
                                arguments == null ? new JsonObject() : arguments, DEFAULT_TIMEOUT)));
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

    @Override
    public void dispose() {
        synchronized (clientLock) {
            closeSharedClientLocked();
        }
    }

    private interface McpRequest {
        JsonElement send(ShaftMcpStdioClient client) throws IOException;
    }

    private ShaftMcpToolResult call(
            List<String> command,
            ShaftSettingsState.Settings settings,
            AtomicReference<ShaftMcpStdioClient> clientReference,
            AtomicBoolean cancellationRequested,
            McpRequest request) {
        Path workingDirectory = project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
        ShaftMcpStdioClient client = null;
        try {
            Map<String, String> environment = ShaftMcpProjectScope.environmentForProject(
                    ShaftMcpEnvironment.forSettings(settings), workingDirectory);
            List<String> scopedCommand = ShaftMcpProjectScope.commandForProject(command, workingDirectory);
            client = acquireClient(scopedCommand, workingDirectory, environment);
            clientReference.set(client);
            if (cancellationRequested.get()) {
                throw new CancellationException("Operation cancelled");
            }
            JsonElement result = request.send(client);
            return ShaftMcpToolResult.success(result.toString());
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            dropClientIfDead(client);
            McpInvocationError category = McpInvocationError.categorize(exception);
            return ShaftMcpToolResult.failure(category.message(), category, category.recoveryAction());
        } finally {
            clientReference.set(null);
        }
    }

    /**
     * Returns the shared MCP server client, spawning a fresh process when none is alive or the
     * effective command, environment, or working directory changed since the last call.
     */
    ShaftMcpStdioClient acquireClient(
            List<String> scopedCommand,
            Path workingDirectory,
            Map<String, String> environment) throws IOException {
        synchronized (clientLock) {
            if (sharedClient != null
                    && sharedClient.isAlive()
                    && sharedCommand.equals(scopedCommand)
                    && sharedEnvironment.equals(environment)
                    && workingDirectory.equals(sharedWorkingDirectory)) {
                return sharedClient;
            }
            closeSharedClientLocked();
            sharedClient = clientFactory.create(scopedCommand, workingDirectory, environment);
            sharedCommand = List.copyOf(scopedCommand);
            sharedEnvironment = Map.copyOf(environment);
            sharedWorkingDirectory = workingDirectory;
            return sharedClient;
        }
    }

    private void dropClientIfDead(ShaftMcpStdioClient client) {
        if (client == null || client.isAlive()) {
            return;
        }
        synchronized (clientLock) {
            if (sharedClient == client) {
                closeSharedClientLocked();
            }
        }
    }

    private void closeSharedClientLocked() {
        if (sharedClient != null) {
            sharedClient.close();
            sharedClient = null;
            sharedCommand = List.of();
            sharedEnvironment = Map.of();
            sharedWorkingDirectory = null;
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

    private static ShaftMcpInvocation completed(String message) {
        return new ShaftMcpInvocation(CompletableFuture.completedFuture(ShaftMcpToolResult.failure(message)), () -> {
        });
    }

    private void cancel(AtomicReference<ShaftMcpStdioClient> clientReference,
                        AtomicBoolean cancellationRequested,
                        boolean force) {
        cancellationRequested.set(true);
        ShaftMcpStdioClient client = force ? clientReference.getAndSet(null) : clientReference.get();
        if (client == null) {
            return;
        }
        if (force) {
            // Force-kill terminates the shared server process; live sessions in it are lost and
            // the next tool call spawns a fresh process.
            synchronized (clientLock) {
                if (sharedClient == client) {
                    sharedClient = null;
                    sharedCommand = List.of();
                    sharedEnvironment = Map.of();
                    sharedWorkingDirectory = null;
                }
            }
            client.kill();
        } else {
            // Graceful cancel abandons in-flight requests but keeps the shared server process
            // (and any live capture or driver session inside it) running.
            client.cancel();
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
