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
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

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
     * Memoized {@code tools/list} result for {@link #sharedClient}. Guarded by {@link #clientLock}
     * and keyed to the exact client instance it was fetched from, so a respawned process (new
     * command/environment/working directory, or a dead-client replacement) can never serve a stale
     * catalog: {@link #isCacheValidLocked()} compares identity against the live {@code sharedClient}.
     */
    private ToolsCache toolsCache;

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
     * Returns the latest list of tools from SHAFT MCP, serving the memoized catalog when the shared
     * process is still alive.
     *
     * @return future MCP result
     */
    public CompletableFuture<ShaftMcpToolResult> listTools() {
        return startListTools().future();
    }

    /**
     * Returns the list of tools from SHAFT MCP.
     *
     * @param forceRefresh when {@code true}, bypasses the cache and issues a fresh {@code tools/list}
     * @return future MCP result
     */
    public CompletableFuture<ShaftMcpToolResult> listTools(boolean forceRefresh) {
        return startListTools(forceRefresh).future();
    }

    /**
     * Starts a cancellable SHAFT MCP tools/list request, serving the memoized catalog when the
     * shared process is still alive instead of round-tripping again.
     *
     * @return cancellable invocation
     */
    public ShaftMcpInvocation startListTools() {
        return startListTools(false);
    }

    /**
     * Forces a fresh {@code tools/list} round-trip, bypassing the cache. Used by explicit
     * "Refresh tools" actions where a stale catalog would be misleading.
     *
     * @return cancellable invocation
     */
    public ShaftMcpInvocation refreshToolsList() {
        return startListTools(true);
    }

    /**
     * Starts a cancellable SHAFT MCP tools/list request.
     *
     * @param forceRefresh when {@code true}, bypasses the cache and issues a fresh {@code tools/list}
     * @return cancellable invocation
     */
    public ShaftMcpInvocation startListTools(boolean forceRefresh) {
        return startListTools(forceRefresh, ShaftSettingsState.getInstance().getState());
    }

    /**
     * Test seam: same logic as {@link #startListTools(boolean)} with explicit settings, so unit
     * tests can exercise the cache without depending on {@code ApplicationManager}. Not public API.
     *
     * @param forceRefresh when {@code true}, bypasses the cache and issues a fresh {@code tools/list}
     * @param settings     explicit settings, bypassing {@code ShaftSettingsState.getInstance()}
     * @return cancellable invocation
     */
    ShaftMcpInvocation startListTools(boolean forceRefresh, ShaftSettingsState.Settings settings) {
        if (!forceRefresh) {
            Optional<String> cached = cachedToolsList();
            if (cached.isPresent()) {
                return completedSuccess(cached.get());
            }
        }
        List<String> command = verifiedCommand(settings);
        if (command.isEmpty()) {
            return completed(CONFIGURE_MESSAGE);
        }
        AtomicReference<ShaftMcpStdioClient> clientReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> call(command, settings, clientReference, cancellationRequested,
                        client -> {
                            JsonElement result = client.listTools(DEFAULT_TIMEOUT);
                            updateToolsCache(client, result);
                            return result;
                        }));
        return new ShaftMcpInvocation(
                future,
                () -> cancel(clientReference, cancellationRequested, false),
                () -> cancel(clientReference, cancellationRequested, true));
    }

    /**
     * Returns the tool names known from the last successful {@code tools/list} response for the
     * currently live shared process, or empty when no catalog has been fetched yet (or the process
     * that fetched it is gone). Callers use this to fail fast on an unknown tool name instead of
     * dispatching a doomed request; when empty, the server remains the source of truth.
     *
     * @return known tool names, or empty when the cache is not populated for the live process
     */
    public Optional<Set<String>> knownToolNames() {
        synchronized (clientLock) {
            return isCacheValidLocked() ? Optional.of(toolsCache.toolNames()) : Optional.empty();
        }
    }

    /**
     * Returns the raw {@code tools/list} payload memoized for the currently live shared process, or
     * empty when it has not been fetched yet.
     *
     * @return the raw payload, or empty when the cache is not populated for the live process
     */
    public Optional<String> cachedToolsList() {
        synchronized (clientLock) {
            return isCacheValidLocked() ? Optional.of(toolsCache.rawPayload()) : Optional.empty();
        }
    }

    private boolean isCacheValidLocked() {
        return toolsCache != null && sharedClient != null && toolsCache.client() == sharedClient
                && sharedClient.isAlive();
    }

    private void updateToolsCache(ShaftMcpStdioClient client, JsonElement result) {
        Set<String> names = parseToolNames(result);
        String rawPayload = result == null ? "{}" : result.toString();
        synchronized (clientLock) {
            toolsCache = new ToolsCache(client, rawPayload, names);
        }
    }

    private static Set<String> parseToolNames(JsonElement result) {
        Set<String> names = new LinkedHashSet<>();
        if (result != null && result.isJsonObject()) {
            JsonElement tools = result.getAsJsonObject().get("tools");
            if (tools != null && tools.isJsonArray()) {
                for (JsonElement entry : tools.getAsJsonArray()) {
                    if (!entry.isJsonObject()) {
                        continue;
                    }
                    JsonElement name = entry.getAsJsonObject().get("name");
                    if (name != null && name.isJsonPrimitive() && name.getAsJsonPrimitive().isString()) {
                        names.add(name.getAsString());
                    }
                }
            }
        }
        return names;
    }

    /**
     * Identity-keyed memo of a successful {@code tools/list} response.
     *
     * @param client     the shared client instance the payload was fetched from
     * @param rawPayload the raw JSON-RPC result, ready to hand back verbatim
     * @param toolNames  the parsed tool names for fast local lookups
     */
    private record ToolsCache(ShaftMcpStdioClient client, String rawPayload, Set<String> toolNames) {
    }

    /**
     * Starts a cancellable SHAFT MCP tool invocation.
     *
     * @param toolName MCP tool name
     * @param arguments JSON object arguments
     * @return cancellable invocation
     */
    public ShaftMcpInvocation startTool(String toolName, JsonObject arguments) {
        return startTool(toolName, arguments, ShaftSettingsState.getInstance().getState(), null);
    }

    /**
     * Starts a cancellable SHAFT MCP tool invocation, streaming {@code notifications/progress}
     * milestones to {@code onProgress} while the call is in flight (issue #3546). The server only
     * emits progress for tools that opted in (currently {@code capture_generate_replay}); calling
     * this for any other tool is harmless and simply never invokes {@code onProgress}.
     *
     * @param toolName   MCP tool name
     * @param arguments  JSON object arguments
     * @param onProgress callback for streamed progress updates, invoked on the caller's background
     *                   thread — callers touching UI state must marshal to the UI thread themselves
     * @return cancellable invocation
     */
    public ShaftMcpInvocation startTool(String toolName, JsonObject arguments, Consumer<ShaftMcpProgress> onProgress) {
        return startTool(toolName, arguments, ShaftSettingsState.getInstance().getState(), onProgress);
    }

    /**
     * Test seam: same logic as {@link #startTool(String, JsonObject)} with explicit settings, so
     * unit tests can exercise the fail-fast unknown-tool check without depending on
     * {@code ApplicationManager}. Not public API.
     *
     * @param toolName  MCP tool name
     * @param arguments JSON object arguments
     * @param settings  explicit settings, bypassing {@code ShaftSettingsState.getInstance()}
     * @return cancellable invocation
     */
    ShaftMcpInvocation startTool(String toolName, JsonObject arguments, ShaftSettingsState.Settings settings) {
        return startTool(toolName, arguments, settings, null);
    }

    /**
     * Test seam: same logic as {@link #startTool(String, JsonObject, Consumer)} with explicit
     * settings. Not public API.
     *
     * @param toolName   MCP tool name
     * @param arguments  JSON object arguments
     * @param settings   explicit settings, bypassing {@code ShaftSettingsState.getInstance()}
     * @param onProgress callback for streamed progress updates, or {@code null} to opt out
     * @return cancellable invocation
     */
    ShaftMcpInvocation startTool(String toolName, JsonObject arguments, ShaftSettingsState.Settings settings,
            Consumer<ShaftMcpProgress> onProgress) {
        List<String> command = verifiedCommand(settings);
        if (command.isEmpty()) {
            return completed(CONFIGURE_MESSAGE);
        }
        if (connectionState != null && !connectionState.isConnected()) {
            return completed(DISCONNECTED_MESSAGE);
        }
        Optional<Set<String>> known = knownToolNames();
        if (known.isPresent() && !known.get().contains(toolName)) {
            return completed(unknownToolMessage(toolName, known.get()));
        }
        AtomicReference<ShaftMcpStdioClient> clientReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> call(command, settings, clientReference, cancellationRequested,
                        client -> client.callTool(toolName,
                                arguments == null ? new JsonObject() : arguments, DEFAULT_TIMEOUT, onProgress)));
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
            return toolResult(result);
        } catch (Exception exception) {
            if (cancellationRequested.get() || exception instanceof CancellationException) {
                throw new CancellationException("Operation cancelled");
            }
            dropClientIfWedged(client);
            McpInvocationError category = McpInvocationError.categorize(exception);
            return ShaftMcpToolResult.failure(
                    McpInvocationError.detail(exception, category), category, category.recoveryAction());
        } finally {
            clientReference.set(null);
        }
    }

    /**
     * Converts a successful JSON-RPC {@code result} into a tool result, treating an MCP
     * {@code isError: true} tool-call response as a failure. Spring AI's MCP server reports a
     * failing {@code @Tool} method this way inside a normal JSON-RPC success envelope, so without
     * this check every failing tool call (missing SHAFT setup, bad arguments, etc.) was reported to
     * the user as a success.
     *
     * @param result raw JSON-RPC result payload
     * @return success, or failure with the tool's own error text
     */
    static ShaftMcpToolResult toolResult(JsonElement result) {
        if (result != null && result.isJsonObject() && isToolError(result.getAsJsonObject())) {
            return ShaftMcpToolResult.failure(
                    toolErrorText(result.getAsJsonObject()), McpInvocationError.TOOL_ERROR,
                    McpInvocationError.TOOL_ERROR.recoveryAction());
        }
        return ShaftMcpToolResult.success(result == null ? "{}" : result.toString());
    }

    private static boolean isToolError(JsonObject object) {
        JsonElement isError = object.get("isError");
        return isError != null && isError.isJsonPrimitive() && isError.getAsJsonPrimitive().isBoolean()
                && isError.getAsBoolean();
    }

    private static String toolErrorText(JsonObject object) {
        JsonElement content = object.get("content");
        if (content != null && content.isJsonArray()) {
            StringBuilder text = new StringBuilder();
            for (JsonElement entry : content.getAsJsonArray()) {
                JsonElement value = entry.isJsonObject() ? entry.getAsJsonObject().get("text") : null;
                if (value != null && value.isJsonPrimitive()) {
                    if (!text.isEmpty()) {
                        text.append('\n');
                    }
                    text.append(value.getAsString());
                }
            }
            if (!text.isEmpty()) {
                return text.toString();
            }
        }
        return object.toString();
    }

    /**
     * Reports the shared long-lived MCP client's liveness without any side effects: the client is
     * never created, replaced, or closed here (respawn churn kills in-progress capture/driver
     * sessions). Returns empty when no shared client exists yet — callers such as
     * {@link ShaftMcpHeartbeat} then decide whether a fresh spawn-probe is worth its cost.
     *
     * @return the shared client's liveness, or empty when no shared client exists
     */
    public java.util.Optional<Boolean> peekSharedClientAlive() {
        synchronized (clientLock) {
            return sharedClient == null
                    ? java.util.Optional.empty()
                    : java.util.Optional.of(sharedClient.isAlive());
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

    /**
     * Drops the shared client after a failed dispatch when it is either dead or was never
     * initialized (an alive process whose {@code initialize} handshake hung or was rejected):
     * either way it can never serve another call, so the next action must respawn a fresh one.
     * An alive, initialized client is left in place — a single tool-call failure (a bad
     * argument, a server-side error) must not tear down a live capture/driver session.
     */
    private void dropClientIfWedged(ShaftMcpStdioClient client) {
        if (client == null || !shouldDropAfterFailure(client.isAlive(), client.isInitialized())) {
            return;
        }
        synchronized (clientLock) {
            if (sharedClient == client) {
                closeSharedClientLocked();
            }
        }
    }

    /**
     * Pure decision of whether a client that just failed a dispatch should be dropped as the
     * shared client (issue #3591): dead clients always go, and so does an alive client that never
     * completed the {@code initialize} handshake, since it will only keep re-failing the same
     * handshake forever otherwise. An alive, initialized client is kept.
     */
    static boolean shouldDropAfterFailure(boolean alive, boolean initialized) {
        return !alive || !initialized;
    }

    private void closeSharedClientLocked() {
        if (sharedClient != null) {
            sharedClient.close();
            sharedClient = null;
            sharedCommand = List.of();
            sharedEnvironment = Map.of();
            sharedWorkingDirectory = null;
            toolsCache = null;
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
            return ShaftMcpToolResult.failure(
                    McpInvocationError.detail(exception, category), category, category.recoveryAction());
        } finally {
            clientReference.set(null);
        }
    }

    private static ShaftMcpInvocation completed(String message) {
        return new ShaftMcpInvocation(CompletableFuture.completedFuture(ShaftMcpToolResult.failure(message)), () -> {
        });
    }

    private static ShaftMcpInvocation completedSuccess(String rawPayload) {
        return new ShaftMcpInvocation(CompletableFuture.completedFuture(ShaftMcpToolResult.success(rawPayload)), () -> {
        });
    }

    /**
     * Builds the fail-fast message for a tool name absent from the cached catalog, suggesting up to
     * three known names that look related (substring/prefix match or a small edit distance) so a
     * typo or a renamed tool points the caller at the right name instead of a bare "not found".
     */
    private static String unknownToolMessage(String toolName, Set<String> knownNames) {
        List<String> suggestions = suggestSimilar(toolName, knownNames);
        StringBuilder message = new StringBuilder("Unknown MCP tool '").append(toolName)
                .append("'. It is not in the cached tool catalog.");
        if (!suggestions.isEmpty()) {
            message.append(" Did you mean: ").append(String.join(", ", suggestions)).append('?');
        }
        return message.toString();
    }

    private static List<String> suggestSimilar(String toolName, Set<String> knownNames) {
        String needle = toolName.toLowerCase(Locale.ROOT);
        return knownNames.stream()
                .filter(name -> isRelated(needle, name.toLowerCase(Locale.ROOT)))
                .sorted(Comparator.comparingInt(name -> levenshtein(needle, name.toLowerCase(Locale.ROOT))))
                .limit(3)
                .toList();
    }

    private static boolean isRelated(String needle, String candidate) {
        if (needle.isEmpty() || candidate.isEmpty()) {
            return false;
        }
        if (candidate.contains(needle) || needle.contains(candidate)) {
            return true;
        }
        int prefixLength = Math.min(3, Math.min(needle.length(), candidate.length()));
        if (needle.substring(0, prefixLength).equals(candidate.substring(0, prefixLength))) {
            return true;
        }
        return levenshtein(needle, candidate) <= 2;
    }

    /**
     * Classic Levenshtein edit distance ("Levenshtein-lite": no transposition handling, just
     * insert/delete/substitute) used to rank suggested tool names by similarity.
     */
    private static int levenshtein(String a, String b) {
        int[] previous = new int[b.length() + 1];
        int[] current = new int[b.length() + 1];
        for (int j = 0; j <= b.length(); j++) {
            previous[j] = j;
        }
        for (int i = 1; i <= a.length(); i++) {
            current[0] = i;
            for (int j = 1; j <= b.length(); j++) {
                int cost = a.charAt(i - 1) == b.charAt(j - 1) ? 0 : 1;
                current[j] = Math.min(Math.min(current[j - 1] + 1, previous[j] + 1), previous[j - 1] + cost);
            }
            int[] swap = previous;
            previous = current;
            current = swap;
        }
        return previous[b.length()];
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
                    toolsCache = null;
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
