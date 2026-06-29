package com.shaft.intellij.mcp;

import com.shaft.intellij.settings.ShaftSettingsState;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Probes a SHAFT MCP stdio command from settings.
 */
public final class ShaftMcpConnectionProbe {
    private static final Duration TIMEOUT = Duration.ofSeconds(30);

    private ShaftMcpConnectionProbe() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Tests a SHAFT MCP stdio command by running the initialization handshake.
     *
     * @param commandLine command line
     * @return async test result
     */
    public static CompletableFuture<ShaftMcpToolResult> test(String commandLine) {
        return test(commandLine, false);
    }

    /**
     * Tests a SHAFT MCP stdio command by running the initialization handshake.
     *
     * @param commandLine command line
     * @param passProviderKeys whether to pass stored provider keys in process environment
     * @return async test result
     */
    public static CompletableFuture<ShaftMcpToolResult> test(String commandLine, boolean passProviderKeys) {
        return test(commandLine, ShaftMcpEnvironment.providerKeys(passProviderKeys));
    }

    /**
     * Tests a SHAFT MCP stdio command by running the initialization handshake.
     *
     * @param commandLine command line
     * @param settings plugin settings used to build the MCP process environment
     * @return async test result
     */
    public static CompletableFuture<ShaftMcpToolResult> test(String commandLine, ShaftSettingsState.Settings settings) {
        return test(commandLine, ShaftMcpEnvironment.forSettings(settings));
    }

    /**
     * Tests a SHAFT MCP stdio command with the current IntelliJ project as workspace.
     *
     * @param commandLine command line
     * @param settings plugin settings used to build the MCP process environment
     * @param projectRoot current IntelliJ project root
     * @return async test result
     */
    public static CompletableFuture<ShaftMcpToolResult> test(
            String commandLine,
            ShaftSettingsState.Settings settings,
            Path projectRoot) {
        return test(commandLine, ShaftMcpEnvironment.forSettings(settings), projectRoot);
    }

    private static CompletableFuture<ShaftMcpToolResult> test(String commandLine, Map<String, String> environment) {
        return test(commandLine, environment, Path.of("."));
    }

    private static CompletableFuture<ShaftMcpToolResult> test(
            String commandLine,
            Map<String, String> environment,
            Path projectRoot) {
        List<String> command = ShaftCommandLine.parse(commandLine == null ? "" : commandLine);
        if (command.isEmpty()) {
            return CompletableFuture.completedFuture(ShaftMcpToolResult.failure(
                    "Enter a SHAFT MCP stdio command before testing."));
        }
        return CompletableFuture.supplyAsync(() -> {
            Path workspace = projectRoot == null ? Path.of(".") : projectRoot;
            try {
                List<String> scopedCommand = ShaftMcpProjectScope.commandForProject(command, workspace);
                Map<String, String> scopedEnvironment = ShaftMcpProjectScope.environmentForProject(environment, workspace);
                try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(scopedCommand, workspace, scopedEnvironment)) {
                    return ShaftMcpToolResult.success(client.initializeOnly(TIMEOUT));
                }
            } catch (Exception exception) {
                return ShaftMcpToolResult.failure(exception.getMessage());
            }
        });
    }
}
