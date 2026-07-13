package com.shaft.intellij.mcp;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletionException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises {@link ShaftMcpInvocationService} lifecycle and dispatch paths not pinned by
 * {@link ShaftMcpInvocationServiceCacheTest} or {@link ShaftMcpInvocationServiceTest}: the
 * public/test-seam constructor, the "not configured" and "disconnected" fail-fast branches of
 * {@code startListTools}/{@code startTool}, {@code dispose()}, {@code peekSharedClientAlive()},
 * a live server dying mid-dispatch (error categorization and shared-client eviction), and the
 * cancel/kill actions on a {@link ShaftMcpInvocation} returned by {@code startListTools}/
 * {@code startTool}.
 */
class ShaftMcpInvocationServiceLifecycleTest {
    private final List<ShaftMcpStdioClient> spawned = new ArrayList<>();

    @AfterEach
    void closeSpawnedClients() {
        spawned.forEach(ShaftMcpStdioClient::close);
        spawned.clear();
    }

    @Test
    void publicConstructorDelegatesToTheRealClientFactoryAndDisposesCleanly() {
        // Uses the public single-argument constructor (the real entry point IntelliJ calls), not
        // the package-private test seam used elsewhere in this package. No tool is ever invoked, so
        // no process is spawned; this only pins that construction and disposal do not throw.
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject());

        assertTrue(service.peekSharedClientAlive().isEmpty(), "no shared client exists yet");
        service.dispose();
    }

    @Test
    void startListToolsReportsConfigureMessageWhenCommandIsNotConfigured() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());

        ShaftMcpToolResult result = service.startListTools(false, notReadySettings()).future().join();

        assertFalse(result.success());
        assertTrue(result.output().contains("Configure the SHAFT MCP stdio command"), result.output());
    }

    @Test
    void startToolReportsConfigureMessageWhenCommandIsNotConfigured() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());

        ShaftMcpToolResult result = service.startTool("any_tool", new JsonObject(), notReadySettings()).future().join();

        assertFalse(result.success());
        assertTrue(result.output().contains("Configure the SHAFT MCP stdio command"), result.output());
    }

    @Test
    void startToolReportsDisconnectedWhenConnectionStateIsDisconnected() {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        connectionState.setConnected(false);
        ShaftMcpInvocationService service =
                new ShaftMcpInvocationService(fakeProjectWithConnectionState(connectionState), new RecordingFactory());

        ShaftMcpToolResult result =
                service.startTool("any_tool", new JsonObject(), settingsForMode("toolsList")).future().join();

        assertFalse(result.success());
        assertTrue(result.output().contains("disconnected"), result.output());
    }

    @Test
    void peekSharedClientAliveReflectsTheSharedClientLifecycle() throws IOException {
        RecordingFactory factory = new RecordingFactory();
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), factory);

        assertTrue(service.peekSharedClientAlive().isEmpty(), "no shared client acquired yet");

        ShaftMcpStdioClient client = service.acquireClient(fakeServerCommand("toolsList"), Path.of("."), Map.of());
        assertEquals(Optional.of(true), service.peekSharedClientAlive());

        client.kill();
        assertEquals(Optional.of(false), service.peekSharedClientAlive(),
                "peek must observe the process death without side effects (no respawn, no reset)");
    }

    @Test
    void disposeClosesAndForgetsTheSharedClient() throws IOException {
        RecordingFactory factory = new RecordingFactory();
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), factory);
        ShaftMcpStdioClient client = service.acquireClient(fakeServerCommand("toolsList"), Path.of("."), Map.of());
        assertTrue(client.isAlive());

        service.dispose();

        assertFalse(client.isAlive(), "dispose() must close the pooled shared client");
        assertTrue(service.peekSharedClientAlive().isEmpty());
        assertTrue(service.knownToolNames().isEmpty(), "dispose() must forget the memoized tools/list cache too");
    }

    @Test
    void deadServerDuringDispatchIsCategorizedAsProcessExitedAndEvicted() {
        RecordingFactory factory = new RecordingFactory();
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), factory);
        ShaftSettingsState.Settings settings = settingsForMode("stderrAndExit");

        ShaftMcpToolResult result = service.startListTools(false, settings).future().join();

        assertFalse(result.success());
        assertEquals(McpInvocationError.PROCESS_EXITED, result.errorCategory());
        assertTrue(result.output().contains("process exit code"), result.output());
        assertTrue(service.peekSharedClientAlive().isEmpty(),
                "a dead client encountered mid-dispatch must be evicted, not kept as the shared client");
    }

    @Test
    void startToolWithNullArgumentsDispatchesWithAnEmptyObjectAndSucceeds() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());

        ShaftMcpToolResult result = service.startTool("fake_tool", null, settingsForMode("toolsList")).future().join();

        assertTrue(result.success(), result.output());
    }

    @Test
    void startToolIsErrorResponseIsReportedAsFailureWithTheToolsOwnText() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());

        ShaftMcpToolResult result = service.startTool("fake_tool", new JsonObject(), settingsForMode("toolCallIsError"))
                .future().join();

        assertFalse(result.success());
        assertEquals("boom", result.output());
        assertEquals(McpInvocationError.TOOL_ERROR, result.errorCategory());
    }

    @Test
    void startListToolsGracefulCancelAbandonsTheRequestButKeepsTheProcessAliveThenKillTerminatesIt() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = settingsForMode("silentToolCalls");

        ShaftMcpInvocation gracefulInvocation = service.startListTools(false, settings);
        cancelUntilDone(gracefulInvocation::cancel, gracefulInvocation.future());
        CompletionException gracefulFailure =
                assertThrows(CompletionException.class, () -> gracefulInvocation.future().join());
        assertInstanceOf(CancellationException.class, gracefulFailure.getCause());
        assertEquals(Optional.of(true), service.peekSharedClientAlive(),
                "a graceful cancel must not terminate the shared server process");

        ShaftMcpInvocation killedInvocation = service.startListTools(false, settings);
        cancelUntilDone(killedInvocation::kill, killedInvocation.future());
        assertTrue(killedInvocation.future().isCompletedExceptionally() || killedInvocation.future().isCancelled());
        assertTrue(service.peekSharedClientAlive().isEmpty(),
                "a force-kill must terminate the shared server process");
    }

    @Test
    void startToolGracefulCancelAbandonsTheRequestButKeepsTheProcessAliveThenKillTerminatesIt() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = settingsForMode("silentToolCalls");

        ShaftMcpInvocation gracefulInvocation =
                service.startTool("never_answered", new JsonObject(), settings);
        cancelUntilDone(gracefulInvocation::cancel, gracefulInvocation.future());
        CompletionException gracefulFailure =
                assertThrows(CompletionException.class, () -> gracefulInvocation.future().join());
        assertInstanceOf(CancellationException.class, gracefulFailure.getCause());
        assertEquals(Optional.of(true), service.peekSharedClientAlive(),
                "a graceful cancel must not terminate the shared server process");

        ShaftMcpInvocation killedInvocation =
                service.startTool("never_answered", new JsonObject(), settings);
        cancelUntilDone(killedInvocation::kill, killedInvocation.future());
        assertTrue(killedInvocation.future().isCompletedExceptionally() || killedInvocation.future().isCancelled());
        assertTrue(service.peekSharedClientAlive().isEmpty(),
                "a force-kill must terminate the shared server process");
    }

    /**
     * The invocation's async task acquires the shared client and dispatches its request on a pool
     * thread, not synchronously on this one. {@code cancel()}/{@code kill()} are "sticky": calling
     * either before the client has been assigned still aborts the call (the background task's
     * {@code cancellationRequested.get()} check short-circuits before ever dispatching), but that
     * path never touches the real client, so a premature kill would never actually terminate the
     * server process. Sleeping first gives the background task a realistic chance to reach its
     * blocking read before the cancel/kill action is exercised, then keeps retrying (mirroring the
     * polling idiom used by
     * {@code ShaftMcpStdioClientTest.cancelAbandonsInFlightRequestWithoutKillingTheServerProcess})
     * in case one more retry is needed.
     */
    private static void cancelUntilDone(Runnable action, java.util.concurrent.CompletableFuture<?> future) {
        sleepQuietly(300);
        for (int attempt = 0; attempt < 100 && !future.isDone(); attempt++) {
            action.run();
            sleepQuietly(50);
        }
    }

    private static void sleepQuietly(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }

    private static List<String> fakeServerCommand(String mode) {
        return List.of(javaExecutable(), "-cp", System.getProperty("java.class.path"),
                FakeMcpServer.class.getName(), mode);
    }

    private static ShaftSettingsState.Settings settingsForMode(String mode) {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = quote(javaExecutable()) + " -cp " + quote(System.getProperty("java.class.path"))
                + " " + FakeMcpServer.class.getName() + " " + mode;
        return settings;
    }

    private static ShaftSettingsState.Settings notReadySettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = false;
        settings.mcpCommand = "";
        return settings;
    }

    private static String javaExecutable() {
        String javaHome = System.getProperty("java.home");
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win");
        return java.nio.file.Paths.get(javaHome, "bin", windows ? "java.exe" : "java").toString();
    }

    private static String quote(String value) {
        return "\"" + value.replace("\"", "\\\"") + "\"";
    }

    /**
     * Minimal {@link Project} stub: only {@code getBasePath()} and {@code getService()} (returning
     * {@code null}, keeping the connected-state gate inert) need real answers, matching the pattern
     * used across this package's tests.
     */
    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "getBasePath" -> ".";
                    case "getName" -> "shaft-mcp-lifecycle-test-project";
                    default -> defaultValue(method.getReturnType());
                });
    }

    /**
     * Same stub as {@link #fakeProject()}, except {@code getService(ShaftMcpConnectionState.class)}
     * answers with the given (already constructed) connection-state instance, so tests can exercise
     * {@link ShaftMcpInvocationService#startTool}'s disconnected fail-fast branch, which only
     * triggers when the constructor was given a non-null project.
     */
    private static Project fakeProjectWithConnectionState(ShaftMcpConnectionState connectionState) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "getBasePath" -> ".";
                    case "getName" -> "shaft-mcp-lifecycle-test-project";
                    case "getService" -> arguments != null && arguments.length == 1
                            && arguments[0] == ShaftMcpConnectionState.class ? connectionState : null;
                    default -> defaultValue(method.getReturnType());
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive()) {
            return null;
        }
        if (returnType == boolean.class) {
            return false;
        }
        return 0;
    }

    private final class RecordingFactory implements ShaftMcpInvocationService.ShaftMcpClientFactory {
        @Override
        public ShaftMcpStdioClient create(List<String> command, Path workingDirectory, Map<String, String> environment)
                throws IOException {
            ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, workingDirectory, environment);
            spawned.add(client);
            return client;
        }
    }
}
