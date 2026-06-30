package com.shaft.intellij.mcp;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Starts the public shaft-mcp installer from the IntelliJ plugin.
 */
public final class ShaftMcpInstaller {
    private static final String DEFAULT_REF = "main";
    private static final String REF_ENV = "SHAFT_MCP_INSTALLER_REF";
    private static final long TIMEOUT_MINUTES = 20;

    private ShaftMcpInstaller() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Installs or updates shaft-mcp for this plugin and returns the stdio command.
     *
     * @return async installer result
     */
    public static CompletableFuture<ShaftMcpInstallResult> installForPlugin() {
        return CompletableFuture.supplyAsync(() -> run(installCommand("intellij-plugin", true), true));
    }

    /**
     * Installs or updates shaft-mcp for this plugin and configures the selected assistant client.
     *
     * @param client installer client target
     * @return async installer result
     */
    public static CompletableFuture<ShaftMcpInstallResult> installForPluginAndClient(String client) {
        String target = client == null || client.isBlank() ? "intellij-plugin" : client;
        return CompletableFuture.supplyAsync(() -> run(installCommand(target, true), true));
    }

    /**
     * Installs or updates shaft-mcp and configures GitHub Copilot for IntelliJ IDEA.
     *
     * @return async installer result
     */
    public static CompletableFuture<ShaftMcpInstallResult> configureCopilotIntellij() {
        return configureClient("copilot-intellij");
    }

    /**
     * Installs or updates shaft-mcp and configures one supported MCP client.
     *
     * @param client installer client target
     * @return async installer result
     */
    public static CompletableFuture<ShaftMcpInstallResult> configureClient(String client) {
        return CompletableFuture.supplyAsync(() -> run(installCommand(client, false), false));
    }

    static List<String> installCommand(String client, boolean json) {
        if (isWindows()) {
            String arguments = json ? " -Arguments @('--json')" : "";
            return List.of("powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-Command",
                    "irm " + installerUrl("install-shaft-mcp.ps1") + " | iex; Install-ShaftMcp -Client "
                            + client + arguments);
        }
        List<String> flags = new ArrayList<>();
        flags.add("--" + client);
        if (json) {
            flags.add("--json");
        }
        return List.of("sh", "-c", "curl -fsSL " + installerUrl("install-shaft-mcp.sh")
                + " | sh -s -- " + String.join(" ", flags));
    }

    static String installerUrl(String scriptName) {
        return installerUrl(scriptName, installerRef());
    }

    static String installerUrl(String scriptName, String ref) {
        return "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/"
                + sanitizeInstallerRef(ref) + "/scripts/mcp/" + scriptName;
    }

    static ShaftMcpInstallResult run(List<String> command, boolean parseJson) {
        Process process = null;
        try {
            process = new ProcessBuilder(command).redirectErrorStream(true).start();
            Process activeProcess = process;
            CompletableFuture<String> output = CompletableFuture.supplyAsync(() -> readOutput(activeProcess));
            if (!process.waitFor(TIMEOUT_MINUTES, TimeUnit.MINUTES)) {
                process.destroyForcibly();
                return ShaftMcpInstallResult.failure("Timed out while installing SHAFT MCP.");
            }
            String text = output.get(5, TimeUnit.SECONDS).trim();
            if (process.exitValue() != 0) {
                return ShaftMcpInstallResult.failure(text);
            }
            return parseJson
                    ? ShaftMcpInstallResult.success(commandLineFromJson(text), text)
                    : ShaftMcpInstallResult.success("", text);
        } catch (Exception exception) {
            if (process != null) {
                process.destroyForcibly();
            }
            return ShaftMcpInstallResult.failure(exception.getMessage());
        }
    }

    static String commandLineFromJson(String output) {
        JsonObject root = JsonParser.parseString(lastJsonLine(output)).getAsJsonObject();
        String command = root.get("command").getAsString();
        JsonArray args = root.getAsJsonArray("args");
        List<String> parts = new ArrayList<>();
        parts.add(quote(command));
        for (int index = 0; index < args.size(); index++) {
            parts.add(quote(args.get(index).getAsString()));
        }
        return String.join(" ", parts);
    }

    private static String lastJsonLine(String output) {
        String[] lines = output == null ? new String[0] : output.split("\\R");
        for (int index = lines.length - 1; index >= 0; index--) {
            String line = lines[index].trim();
            if (line.startsWith("{") && line.endsWith("}")) {
                return line;
            }
        }
        throw new IllegalArgumentException("Installer did not return JSON output.");
    }

    private static String quote(String value) {
        return "\"" + value.replace('\\', '/').replace("\"", "\\\"") + "\"";
    }

    private static String readOutput(Process process) {
        try {
            return new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            return exception.getMessage();
        }
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
    }

    private static String installerRef() {
        String configured = System.getenv(REF_ENV);
        return sanitizeInstallerRef(configured);
    }

    static String sanitizeInstallerRef(String ref) {
        if (ref == null || ref.isBlank()) {
            return DEFAULT_REF;
        }
        String trimmed = ref.trim();
        return trimmed.matches("[A-Za-z0-9._/-]+") ? trimmed : DEFAULT_REF;
    }
}
