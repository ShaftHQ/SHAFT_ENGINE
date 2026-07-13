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
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises {@link ShaftMcpInvocationService}'s unknown-tool-name suggestion machinery
 * ({@code unknownToolMessage}/{@code suggestSimilar}/{@code isRelated}/{@code levenshtein}) and the
 * {@code parseToolNames} defensive skip of non-object {@code tools/list} entries, using
 * {@link MultiToolListServer} (a four-tool catalog) instead of {@link CountingToolsListServer}'s
 * single-tool one: with only one known name, the suggestion ranking's {@code sorted(...)} never
 * actually compares two candidates, so {@code levenshtein} is never invoked by that suite.
 */
class ShaftMcpInvocationServiceSuggestionsTest {
    private final List<ShaftMcpStdioClient> spawned = new ArrayList<>();

    @AfterEach
    void closeSpawnedClients() {
        spawned.forEach(ShaftMcpStdioClient::close);
        spawned.clear();
    }

    @Test
    void parseToolNamesSkipsTheNonObjectEntryAndKeepsTheRealTools() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());

        service.startListTools(false, multiToolListSettings()).future().join();

        Optional<Set<String>> known = service.knownToolNames();
        assertTrue(known.isPresent());
        assertEquals(Set.of(MultiToolListServer.TOOL_NAMES), known.get(),
                "the leading non-object '42' entry must be skipped, not crash parsing or leak in as a name");
    }

    @Test
    void unknownToolNameSuggestsAllRelatedCandidatesRankedByEditDistance() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = multiToolListSettings();
        service.startListTools(false, settings).future().join();

        ShaftMcpToolResult result = service.startTool("captur", new JsonObject(), settings).future().join();

        assertFalse(result.success());
        assertTrue(result.output().contains("Did you mean"), result.output());
        assertTrue(result.output().contains("capture_start"), result.output());
        assertTrue(result.output().contains("capture_stop"), result.output());
        assertTrue(result.output().contains("capture_status"), result.output());
        assertFalse(result.output().contains("driver_initialize"),
                "an unrelated tool name (no substring/prefix/edit-distance match) must not be suggested: "
                        + result.output());
    }

    @Test
    void emptyToolNameHasNoRelatedCandidatesSoNoSuggestionIsOffered() {
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(fakeProject(), new RecordingFactory());
        ShaftSettingsState.Settings settings = multiToolListSettings();
        service.startListTools(false, settings).future().join();

        ShaftMcpToolResult result = service.startTool("", new JsonObject(), settings).future().join();

        assertFalse(result.success());
        assertTrue(result.output().contains("Unknown MCP tool ''"), result.output());
        assertFalse(result.output().contains("Did you mean"),
                "an empty tool name is unrelated to every known name, so no suggestion should be offered: "
                        + result.output());
    }

    private static ShaftSettingsState.Settings multiToolListSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = quote(javaExecutable()) + " -cp " + quote(System.getProperty("java.class.path"))
                + " " + MultiToolListServer.class.getName();
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

    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "getBasePath" -> ".";
                    case "getName" -> "shaft-mcp-suggestions-test-project";
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
