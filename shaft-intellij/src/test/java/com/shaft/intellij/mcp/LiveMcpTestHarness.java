package com.shaft.intellij.mcp;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.shaft.intellij.settings.ShaftSettingsState;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;

/**
 * Test-only bridge that exposes the plugin's package-private MCP stdio client and project
 * scoping to live end-to-end tests in other packages, so those tests exercise the exact
 * client/scoping code paths the plugin uses without reflection.
 */
public final class LiveMcpTestHarness implements AutoCloseable {
    private final ShaftMcpStdioClient client;

    /**
     * Spawns one long-lived SHAFT MCP server process through the plugin's stdio client.
     *
     * @param command scoped MCP launch command
     * @param workingDirectory MCP working directory (the workspace root)
     * @param environment scoped process environment
     * @throws IOException when the process cannot be started
     */
    public LiveMcpTestHarness(List<String> command, Path workingDirectory, Map<String, String> environment)
            throws IOException {
        client = new ShaftMcpStdioClient(command, workingDirectory, environment);
    }

    /**
     * Applies the plugin's project scoping to an MCP launch command.
     *
     * @param command configured MCP command
     * @param workspace project/workspace root
     * @return scoped command
     * @throws IOException when a scoped argfile cannot be materialized
     */
    public static List<String> scopedCommand(List<String> command, Path workspace) throws IOException {
        return ShaftMcpProjectScope.commandForProject(command, workspace);
    }

    /**
     * Builds the plugin's scoped MCP process environment for the given settings and workspace.
     *
     * @param settings plugin settings
     * @param workspace project/workspace root
     * @return scoped environment
     */
    public static Map<String, String> scopedEnvironment(ShaftSettingsState.Settings settings, Path workspace) {
        return ShaftMcpProjectScope.environmentForProject(ShaftMcpEnvironment.forSettings(settings), workspace);
    }

    /**
     * Calls an MCP tool on the shared server process.
     *
     * @param toolName MCP tool name
     * @param arguments JSON arguments
     * @param timeout per-call timeout
     * @return raw JSON-RPC result payload
     * @throws IOException on transport failure
     */
    public JsonElement callTool(String toolName, JsonObject arguments, Duration timeout) throws IOException {
        return client.callTool(toolName, arguments, timeout);
    }

    @Override
    public void close() {
        client.close();
    }
}
