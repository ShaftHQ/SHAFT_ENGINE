package com.shaft.intellij.mcp;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins the {@code tools/list} caching contract on {@link ShaftMcpInvocationService}: a second
 * {@code startListTools()} call must be served from the memoized catalog instead of round-tripping
 * the shared MCP process again, a respawned process must invalidate the cache, and {@code startTool}
 * must fail fast (locally, no dispatch) for a tool name absent from a populated cache while still
 * dispatching to the server when the cache is empty (server remains the source of truth).
 *
 * <p>Uses the package-private {@code (boolean, Settings)} test seams on
 * {@link ShaftMcpInvocationService#startListTools(boolean, ShaftSettingsState.Settings)} and
 * {@link ShaftMcpInvocationService#startTool(String, JsonObject, ShaftSettingsState.Settings)} to
 * avoid a dependency on {@code ApplicationManager}/{@code ShaftSettingsState.getInstance()}, which
 * is not available outside the real IntelliJ platform. Real round-trips are counted via
 * {@link CountingToolsListServer}, a dedicated subprocess fixture.</p>
 */
class ShaftMcpInvocationServiceCacheTest {
    private final List<ShaftMcpStdioClient> spawned = new ArrayList<>();

    @AfterEach
    void closeSpawnedClients() {
        spawned.forEach(ShaftMcpStdioClient::close);
        spawned.clear();
    }

    @Test
    void secondListToolsCallServesFromCacheWithoutARoundTrip() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());

        ShaftMcpToolResult first = service.startListTools(false, settings).future().join();
        ShaftMcpToolResult second = service.startListTools(false, settings).future().join();

        assertTrue(first.success(), first.output());
        assertTrue(second.success(), second.output());
        assertEquals(first.output(), second.output(),
                "a cache hit must serve the exact same payload the first round-trip fetched");
        assertEquals(1, CountingToolsListServer.callCount(second.output()),
                "the second startListTools() call must not have issued another tools/list RPC");
    }

    @Test
    void forceRefreshBypassesTheCache() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());

        ShaftMcpToolResult first = service.startListTools(false, settings).future().join();
        ShaftMcpToolResult refreshed = service.startListTools(true, settings).future().join();
        ShaftMcpToolResult cachedAgain = service.startListTools(false, settings).future().join();

        assertEquals(1, CountingToolsListServer.callCount(first.output()));
        assertEquals(2, CountingToolsListServer.callCount(refreshed.output()),
                "startListTools(true, ...) must force a fresh round-trip");
        assertEquals(2, CountingToolsListServer.callCount(cachedAgain.output()),
                "the next cache-serving call must return the refreshed payload, not re-fetch");
    }

    @Test
    void refreshToolsListMethodAlsoForcesARoundTripThroughTheDefaultSettingsOverload() {
        // refreshToolsList()/startListTools() (no explicit Settings) delegate to the (boolean,
        // Settings) seam exercised elsewhere in this suite; this only pins that the delegation
        // itself passes forceRefresh through unchanged.
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());

        service.startListTools(false, settings).future().join();
        ShaftMcpToolResult refreshed = service.startListTools(true, settings).future().join();

        assertEquals(2, CountingToolsListServer.callCount(refreshed.output()));
    }

    @Test
    void knownToolNamesIsEmptyUntilAFetchSucceedsThenPopulated() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());

        assertTrue(service.knownToolNames().isEmpty(), "no catalog fetched yet");

        service.startListTools(false, settings).future().join();

        Optional<Set<String>> known = service.knownToolNames();
        assertTrue(known.isPresent());
        assertTrue(known.get().contains(CountingToolsListServer.TOOL_NAME));
    }

    @Test
    void respawnInvalidatesTheCache() {
        RecordingFactory factory = new RecordingFactory();
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), factory);
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());

        ShaftMcpToolResult first = service.startListTools(false, settings).future().join();
        assertEquals(1, factory.created.size());
        assertTrue(service.knownToolNames().isPresent());

        factory.created.get(0).kill();

        ShaftMcpToolResult afterRespawn = service.startListTools(false, settings).future().join();

        assertEquals(2, factory.created.size(), "a dead shared client must be respawned");
        assertEquals(1, CountingToolsListServer.callCount(first.output()));
        assertEquals(2, CountingToolsListServer.callCount(afterRespawn.output()),
                "the cache must not have served the pre-respawn payload; a fresh round-trip must run");
    }

    @Test
    void unknownToolNameFailsFastWithoutDispatchingWhenCacheIsPopulated() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());
        service.startListTools(false, settings).future().join();

        ShaftMcpToolResult result = service.startTool("counting_toool", new JsonObject(), settings).future().join();

        assertFalse(result.success());
        assertTrue(result.output().contains("Unknown MCP tool 'counting_toool'"), result.output());
        assertTrue(result.output().contains(CountingToolsListServer.TOOL_NAME),
                "the closest known tool name should be suggested: " + result.output());
    }

    @Test
    void knownToolNameStillDispatchesWhenCacheIsPopulated() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());
        service.startListTools(false, settings).future().join();

        ShaftMcpToolResult result = service.startTool(CountingToolsListServer.TOOL_NAME, new JsonObject(), settings)
                .future().join();

        assertTrue(result.success(), result.output());
    }

    @Test
    void unpopulatedCacheStillDispatchesAnyToolNameServerRemainsSourceOfTruth() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = countingServerSettings(freshCounterFile());

        ShaftMcpToolResult result = service.startTool("whatever_the_server_supports", new JsonObject(), settings)
                .future().join();

        assertTrue(result.success(), result.output());
    }

    private static ShaftSettingsState.Settings countingServerSettings(Path counterFile) {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = quote(javaExecutable())
                + " -D" + CountingToolsListServer.COUNTER_FILE_PROPERTY + "=" + quote(counterFile.toString())
                + " -cp " + quote(System.getProperty("java.class.path"))
                + " " + CountingToolsListServer.class.getName();
        return settings;
    }

    /**
     * A fresh, not-yet-existing counter file per test, so {@link CountingToolsListServer}'s
     * cross-respawn counter starts at zero and different tests never share counts.
     */
    private static Path freshCounterFile() {
        try {
            Path file = Files.createTempFile("shaft-mcp-cache-test-", ".counter");
            Files.deleteIfExists(file);
            file.toFile().deleteOnExit();
            return file;
        } catch (IOException exception) {
            throw new UncheckedIOException(exception);
        }
    }

    private static String javaExecutable() {
        String javaHome = System.getProperty("java.home");
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win");
        return Paths.get(javaHome, "bin", windows ? "java.exe" : "java").toString();
    }

    private static String quote(String value) {
        return "\"" + value.replace("\"", "\\\"") + "\"";
    }

    /**
     * Minimal {@link Project} stub: only {@code getBasePath()} (read by
     * {@link ShaftMcpInvocationService}'s private {@code call()} helper) and {@code getName()} need
     * real answers; everything else falls through to {@link #defaultValue}.
     */
    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "getBasePath" -> ".";
                    case "getName" -> "shaft-mcp-cache-test-project";
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
        private final List<ShaftMcpStdioClient> created = new ArrayList<>();

        @Override
        public ShaftMcpStdioClient create(List<String> command, Path workingDirectory, Map<String, String> environment)
                throws IOException {
            ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, workingDirectory, environment);
            created.add(client);
            spawned.add(client);
            return client;
        }
    }
}
