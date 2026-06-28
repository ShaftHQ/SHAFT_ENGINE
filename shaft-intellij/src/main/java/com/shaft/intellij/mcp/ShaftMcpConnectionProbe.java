package com.shaft.intellij.mcp;

import com.shaft.intellij.settings.ShaftCredentialService;

import java.nio.file.Path;
import java.time.Duration;
import java.util.LinkedHashMap;
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
        List<String> command = ShaftCommandLine.parse(commandLine == null ? "" : commandLine);
        if (command.isEmpty()) {
            return CompletableFuture.completedFuture(ShaftMcpToolResult.failure(
                    "Enter a SHAFT MCP stdio command before testing."));
        }
        return CompletableFuture.supplyAsync(() -> {
            try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, Path.of("."),
                    providerEnvironment(passProviderKeys))) {
                return ShaftMcpToolResult.success(client.initializeOnly(TIMEOUT));
            } catch (Exception exception) {
                return ShaftMcpToolResult.failure(exception.getMessage());
            }
        });
    }

    private static Map<String, String> providerEnvironment(boolean passProviderKeys) {
        if (!passProviderKeys) {
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
