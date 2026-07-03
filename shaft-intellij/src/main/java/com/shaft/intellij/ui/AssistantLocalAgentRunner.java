package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * Runs selected local assistant CLIs directly so their markdown stays user-facing.
 */
final class AssistantLocalAgentRunner {
    static final String LOCAL_AGENT_TOOL = "autobot_local_agent_run";
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    private static final String CUSTOM_AGENT_APPROVAL_WARNING =
            "Custom Agent commands require Allow source edits because SHAFT cannot enforce read-only execution.";

    private AssistantLocalAgentRunner() {
        throw new IllegalStateException("Utility class");
    }

    static boolean supports(AssistantCommand.Invocation invocation) {
        return invocation != null && LOCAL_AGENT_TOOL.equals(invocation.toolName());
    }

    static ShaftMcpInvocation start(AssistantCommand.Invocation invocation) {
        return start(invocation, null);
    }

    static ShaftMcpInvocation start(AssistantCommand.Invocation invocation, Consumer<String> outputConsumer) {
        JsonObject arguments = invocation.arguments();
        AtomicReference<Process> processReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> run(arguments, processReference, cancellationRequested, outputConsumer));
        return new ShaftMcpInvocation(
                future,
                () -> cancel(processReference, cancellationRequested, false),
                () -> cancel(processReference, cancellationRequested, true));
    }

    static ShaftMcpToolResult readiness(String client, String runtime) {
        if (!"CLI".equals(normalize(runtime))) {
            return ShaftMcpToolResult.success("Selected agent runtime is configured by SHAFT MCP.");
        }
        String executable = switch (normalize(client)) {
            case "CLAUDE_CODE" -> "claude";
            case "COPILOT_CLI" -> "copilot";
            default -> "codex";
        };
        String displayName = displayName(client);
        return isCommandAvailable(executable)
                ? ShaftMcpToolResult.success(displayName + " executable is available on PATH.")
                : ShaftMcpToolResult.failure(displayName + " executable is not available on PATH.");
    }

    static List<String> commandFor(JsonObject arguments) {
        List<String> custom = customCommand(arguments);
        if (!custom.isEmpty()) {
            return custom;
        }
        boolean allowSourceMutation = allowSourceMutation(arguments);
        String mode = normalize(string(arguments, "mode", "ASK"));
        return switch (normalize(string(arguments, "client", "CODEX"))) {
            case "CLAUDE_CODE" -> claudeCommand(mode, allowSourceMutation);
            case "COPILOT_CLI" -> copilotCommand(mode, allowSourceMutation);
            default -> codexCommand(mode, allowSourceMutation);
        };
    }

    private static ShaftMcpToolResult run(
            JsonObject arguments,
            AtomicReference<Process> processReference,
            AtomicBoolean cancellationRequested,
            Consumer<String> outputConsumer) {
        List<String> command = commandFor(arguments);
        if (command.isEmpty()) {
            return ShaftMcpToolResult.failure("No local assistant command was configured.");
        }
        if (!defaultCommand(arguments)
                && "AGENT".equals(normalize(string(arguments, "mode", "ASK")))
                && !allowSourceMutation(arguments)) {
            return ShaftMcpToolResult.failure(CUSTOM_AGENT_APPROVAL_WARNING);
        }
        if (defaultCommand(arguments) && !isCommandAvailable(command.get(0))) {
            return ShaftMcpToolResult.failure(displayName(string(arguments, "client", "CODEX"))
                    + " executable is not available on PATH.");
        }
        Duration timeout = Duration.ofSeconds(intValue(arguments, "timeoutSeconds", DEFAULT_TIMEOUT_SECONDS));
        String stdin = string(arguments, "prompt", "");
        Path workingDirectory = workingDirectory(arguments);
        try {
            ProcessBuilder builder = new ProcessBuilder(command);
            builder.directory(workingDirectory.toFile());
            builder.environment().putAll(environment(arguments));
            Process process = builder.start();
            processReference.set(process);
            InputStream stdoutStream = process.getInputStream();
            InputStream stderrStream = process.getErrorStream();
            CompletableFuture<String> stdout = readAsync(stdoutStream, outputConsumer);
            CompletableFuture<String> stderr = readAsync(stderrStream, outputConsumer);
            process.getOutputStream().write(stdin.getBytes(StandardCharsets.UTF_8));
            process.getOutputStream().close();

            if (cancellationRequested.get()) {
                throw new CancellationException("Operation cancelled");
            }
            if (!process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS)) {
                process.destroyForcibly();
                return ShaftMcpToolResult.failure(agentOutput(false, stdoutNow(stdout), stderrNow(stderr),
                        "Timed out after " + timeout.toSeconds() + " seconds."));
            }
            if (cancellationRequested.get()) {
                closeQuietly(stdoutStream);
                closeQuietly(stderrStream);
                stdoutNow(stdout);
                stderrNow(stderr);
                throw new CancellationException("Operation cancelled");
            }
            String output = agentOutput(process.exitValue() == 0, stdoutNow(stdout), stderrNow(stderr), "");
            return process.exitValue() == 0
                    ? ShaftMcpToolResult.success(output)
                    : ShaftMcpToolResult.failure(output);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            if (cancellationRequested.get()) {
                throw new CancellationException("Operation cancelled");
            }
            return ShaftMcpToolResult.failure("Interrupted while running local assistant.");
        } catch (IOException | RuntimeException exception) {
            if (cancellationRequested.get()) {
                throw new CancellationException("Operation cancelled");
            }
            return ShaftMcpToolResult.failure(exception.getMessage());
        } finally {
            processReference.set(null);
        }
    }

    private static void cancel(
            AtomicReference<Process> processReference,
            AtomicBoolean cancellationRequested,
            boolean force) {
        cancellationRequested.set(true);
        Process process = force ? processReference.getAndSet(null) : processReference.get();
        if (process != null) {
            if (force) {
                process.destroyForcibly();
            } else {
                process.destroy();
            }
        }
    }

    private static List<String> codexCommand(String mode, boolean allowSourceMutation) {
        return switch (mode) {
            case "AGENT" -> List.of(
                    "codex", "exec",
                    "--sandbox", allowSourceMutation ? "workspace-write" : "read-only",
                    "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                    "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                    "-");
            default -> List.of("codex", "exec", "--sandbox", "read-only", "-");
        };
    }

    private static List<String> claudeCommand(String mode, boolean allowSourceMutation) {
        return switch (mode) {
            case "PLAN" -> List.of("claude", "--print", "--permission-mode", "plan");
            case "AGENT" -> List.of("claude", "--print", "--permission-mode", allowSourceMutation ? "acceptEdits" : "plan");
            default -> List.of("claude", "--print");
        };
    }

    private static List<String> copilotCommand(String mode, boolean allowSourceMutation) {
        return switch (mode) {
            case "PLAN" -> List.of("copilot", "plan");
            case "AGENT" -> List.of("copilot", allowSourceMutation ? "agent" : "ask");
            default -> List.of("copilot", "ask");
        };
    }

    private static boolean allowSourceMutation(JsonObject arguments) {
        return booleanValue(arguments, "allowSourceMutation");
    }

    private static boolean defaultCommand(JsonObject arguments) {
        return customCommand(arguments).isEmpty();
    }

    private static List<String> customCommand(JsonObject arguments) {
        JsonElement command = arguments == null ? null : arguments.get("command");
        if (command == null || !command.isJsonArray()) {
            return List.of();
        }
        List<String> values = new ArrayList<>();
        for (JsonElement item : command.getAsJsonArray()) {
            if (item != null && item.isJsonPrimitive()) {
                String value = item.getAsString();
                if (value != null && !value.isBlank()) {
                    values.add(value);
                }
            }
        }
        return List.copyOf(values);
    }

    private static Map<String, String> environment(JsonObject arguments) {
        JsonElement environment = arguments == null ? null : arguments.get("environment");
        if (environment == null || !environment.isJsonObject()) {
            return Map.of();
        }
        java.util.HashMap<String, String> values = new java.util.HashMap<>();
        for (Map.Entry<String, JsonElement> entry : environment.getAsJsonObject().entrySet()) {
            JsonElement value = entry.getValue();
            if (value != null && value.isJsonPrimitive()) {
                values.put(entry.getKey(), value.getAsString());
            }
        }
        return values;
    }

    private static Path workingDirectory(JsonObject arguments) {
        String configured = string(arguments, "workingDirectory", "");
        return configured.isBlank() ? Path.of(".") : Path.of(configured);
    }

    static String agentOutput(boolean success, String stdout, String stderr, String fallback) {
        if (success && stdout != null && !stdout.isBlank()) {
            return stdout.strip();
        }
        List<String> sections = new ArrayList<>();
        if (stdout != null && !stdout.isBlank()) {
            sections.add(stdout.strip());
        }
        if (stderr != null && !stderr.isBlank()) {
            sections.add(stderr.strip());
        }
        if (sections.isEmpty() && fallback != null && !fallback.isBlank()) {
            sections.add(fallback);
        }
        return String.join("\n\n", sections).trim();
    }

    private static CompletableFuture<String> readAsync(InputStream stream, Consumer<String> outputConsumer) {
        return CompletableFuture.supplyAsync(() -> {
            StringBuilder output = new StringBuilder();
            try {
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        output.append(line).append('\n');
                        if (outputConsumer != null) {
                            outputConsumer.accept(line);
                        }
                    }
                }
            } catch (IOException exception) {
                return "";
            }
            return output.toString();
        });
    }

    private static String stdoutNow(CompletableFuture<String> future) {
        try {
            return future.get(2, TimeUnit.SECONDS);
        } catch (Exception exception) {
            return "";
        }
    }

    private static String stderrNow(CompletableFuture<String> future) {
        return stdoutNow(future);
    }

    private static void closeQuietly(InputStream stream) {
        try {
            stream.close();
        } catch (IOException ignored) {
            // Stream readers treat closed process streams as empty output.
        }
    }

    private static boolean isCommandAvailable(String command) {
        String path = System.getenv("PATH");
        if (path == null || path.isBlank()) {
            return false;
        }
        for (String directory : path.split(File.pathSeparator)) {
            if (isExecutable(Path.of(directory, command))) {
                return true;
            }
            if (isWindows()) {
                for (String extension : windowsExecutableExtensions()) {
                    if (isExecutable(Path.of(directory, command + extension))) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static boolean isExecutable(Path candidate) {
        return Files.isRegularFile(candidate) && Files.isExecutable(candidate);
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
    }

    private static List<String> windowsExecutableExtensions() {
        String pathext = System.getenv("PATHEXT");
        if (pathext == null || pathext.isBlank()) {
            return List.of(".exe", ".cmd", ".bat");
        }
        return Arrays.stream(pathext.split(";"))
                .filter(value -> !value.isBlank())
                .map(value -> value.startsWith(".") ? value : "." + value)
                .toList();
    }

    private static String displayName(String client) {
        return switch (normalize(client)) {
            case "CLAUDE_CODE" -> "Claude Code";
            case "COPILOT_CLI" -> "GitHub Copilot CLI";
            default -> "Codex CLI";
        };
    }

    private static int intValue(JsonObject object, String key, int fallback) {
        JsonElement value = object == null ? null : object.get(key);
        if (value == null || !value.isJsonPrimitive()) {
            return fallback;
        }
        try {
            return Math.max(1, value.getAsInt());
        } catch (RuntimeException exception) {
            return fallback;
        }
    }

    private static String string(JsonObject object, String key, String fallback) {
        JsonElement value = object == null ? null : object.get(key);
        return value == null || !value.isJsonPrimitive() ? fallback : value.getAsString();
    }

    private static boolean booleanValue(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        return value != null && value.isJsonPrimitive() && value.getAsBoolean();
    }

    private static String normalize(String value) {
        return (value == null || value.isBlank() ? "" : value.trim())
                .toUpperCase(Locale.ROOT)
                .replace('-', '_')
                .replace(' ', '_');
    }
}
