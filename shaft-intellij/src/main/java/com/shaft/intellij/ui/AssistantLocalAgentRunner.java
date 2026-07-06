package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Runs selected local assistant CLIs directly so their markdown stays user-facing.
 */
final class AssistantLocalAgentRunner {
    static final String LOCAL_AGENT_TOOL = "autobot_local_agent_run";
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    private static final int MODEL_LIST_TIMEOUT_SECONDS = 20;
    private static final int COMPACT_TIMEOUT_SECONDS = 30;
    private static final String CUSTOM_AGENT_APPROVAL_WARNING =
            "Custom Agent commands require Allow source edits because SHAFT cannot enforce read-only execution.";
    private static final Pattern MODEL_LINE_PATTERN = Pattern.compile("(claude-[a-z0-9.-]+|gpt-[a-z0-9.-]+|o[0-9][a-z0-9.-]*)",
            Pattern.CASE_INSENSITIVE);

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
        return start(invocation, outputConsumer, AssistantLocalAgentRunner::launchProcess);
    }

    /**
     * Package-private overload that accepts a custom process launcher so tests can drive a stub
     * process instead of spawning a real CLI.
     */
    static ShaftMcpInvocation start(
            AssistantCommand.Invocation invocation,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher) {
        JsonObject arguments = invocation.arguments();
        AtomicReference<Process> processReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(
                () -> run(arguments, processReference, cancellationRequested, outputConsumer, processLauncher));
        return new ShaftMcpInvocation(
                future,
                () -> cancel(processReference, cancellationRequested, false),
                () -> cancel(processReference, cancellationRequested, true));
    }

    /**
     * Starts a local agent invocation, first sending the agent CLI's compact/compress command as a
     * preamble when {@code autoCompactEnabled} is true. The compact call always completes (or is
     * skipped) before the user prompt invocation is dispatched. Compaction failures or an
     * unsupported CLI never block the user's request — they are logged into the transcript of the
     * eventual result via the {@code outputConsumer}, and the main invocation proceeds regardless.
     */
    static ShaftMcpInvocation startWithOptionalCompact(
            AssistantCommand.Invocation invocation,
            boolean autoCompactEnabled,
            Consumer<String> outputConsumer) {
        if (autoCompactEnabled) {
            ShaftMcpToolResult compactResult = runCompactPreamble(invocation.arguments());
            if (outputConsumer != null && compactResult.output() != null && !compactResult.output().isBlank()) {
                outputConsumer.accept(compactResult.output());
            }
        }
        return start(invocation, outputConsumer);
    }

    /**
     * Package-private overload used by tests to drive a stub process for both the compact preamble
     * and the main invocation, and to assert their relative ordering.
     */
    static ShaftMcpInvocation startWithOptionalCompact(
            AssistantCommand.Invocation invocation,
            boolean autoCompactEnabled,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher) {
        if (autoCompactEnabled) {
            JsonObject arguments = invocation.arguments();
            ShaftMcpToolResult compactResult = runCompactPreamble(arguments, processLauncher);
            if (outputConsumer != null && compactResult.output() != null && !compactResult.output().isBlank()) {
                outputConsumer.accept(compactResult.output());
            }
        }
        return start(invocation, outputConsumer, processLauncher);
    }

    /**
     * Launches a local agent process. Extracted so tests can substitute a stub process without
     * spawning a real CLI executable.
     */
    @FunctionalInterface
    interface ProcessLauncher {
        Process launch(List<String> command, Path workingDirectory, Map<String, String> environment) throws IOException;
    }

    private static Process launchProcess(List<String> command, Path workingDirectory, Map<String, String> environment)
            throws IOException {
        ProcessBuilder builder = new ProcessBuilder(command);
        builder.directory(workingDirectory.toFile());
        builder.environment().putAll(environment);
        return builder.start();
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
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher) {
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
            Process process = processLauncher.launch(command, workingDirectory, environment(arguments));
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

    /**
     * Returns the command used to list the models supported by the selected agent CLI, or an
     * empty list when the CLI has no known model-listing capability.
     */
    static List<String> modelsCommandFor(JsonObject arguments) {
        return switch (normalize(string(arguments, "client", "CODEX"))) {
            case "CLAUDE_CODE" -> List.of("claude", "config", "list-models");
            case "COPILOT_CLI" -> List.of("copilot", "models");
            default -> List.of("codex", "models");
        };
    }

    /**
     * Queries the connected agent CLI for its supported model list. Returns an empty list when the
     * CLI is unavailable, unsupported, or the query fails or times out — callers should fall back to
     * their own default model set in that case.
     */
    static List<String> listModels(JsonObject arguments) {
        List<String> command = modelsCommandFor(arguments);
        if (command.isEmpty() || !isCommandAvailable(command.get(0))) {
            return List.of();
        }
        return listModels(arguments, AssistantLocalAgentRunner::launchProcess);
    }

    /**
     * Package-private overload used by tests to drive a stub process directly, bypassing the PATH
     * availability check that gates the real CLI in {@link #listModels(JsonObject)}.
     */
    static List<String> listModels(JsonObject arguments, ProcessLauncher processLauncher) {
        List<String> command = modelsCommandFor(arguments);
        if (command.isEmpty()) {
            return List.of();
        }
        Path workingDirectory = workingDirectory(arguments);
        try {
            Process process = processLauncher.launch(command, workingDirectory, Map.of());
            process.getOutputStream().close();
            CompletableFuture<String> stdout = readAsync(process.getInputStream(), null);
            CompletableFuture<String> stderr = readAsync(process.getErrorStream(), null);
            boolean finished = process.waitFor(MODEL_LIST_TIMEOUT_SECONDS, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return List.of();
            }
            String output = process.exitValue() == 0 ? stdoutNow(stdout) : "";
            if (output.isBlank()) {
                stderrNow(stderr);
                return List.of();
            }
            return parseModelNames(output);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return List.of();
        } catch (IOException | RuntimeException exception) {
            return List.of();
        }
    }

    /**
     * Parses model names out of a CLI's model-listing output. Accepts either a JSON array of
     * strings/objects (with a {@code name} or {@code id} field) or plain newline-delimited text.
     */
    static List<String> parseModelNames(String output) {
        String trimmed = output == null ? "" : output.strip();
        if (trimmed.isBlank()) {
            return List.of();
        }
        Set<String> models = new LinkedHashSet<>();
        if (trimmed.startsWith("[") || trimmed.startsWith("{")) {
            try {
                JsonElement parsed = JsonParser.parseString(trimmed);
                collectModelNames(parsed, models);
                if (!models.isEmpty()) {
                    return List.copyOf(models);
                }
            } catch (JsonParseException ignored) {
                // Fall through to line-based parsing below.
            }
        }
        for (String line : trimmed.split("\\r?\\n")) {
            String candidate = line.strip();
            if (candidate.isEmpty()) {
                continue;
            }
            candidate = candidate.replaceFirst("^[-*]\\s+", "").strip();
            Matcher matcher = MODEL_LINE_PATTERN.matcher(candidate);
            if (matcher.find()) {
                models.add(matcher.group(1));
            } else if (!candidate.contains(" ") && candidate.length() < 64) {
                models.add(candidate);
            }
        }
        return List.copyOf(models);
    }

    private static void collectModelNames(JsonElement element, Set<String> models) {
        if (element == null || element.isJsonNull()) {
            return;
        }
        if (element.isJsonArray()) {
            for (JsonElement item : element.getAsJsonArray()) {
                collectModelNames(item, models);
            }
            return;
        }
        if (element.isJsonPrimitive() && element.getAsJsonPrimitive().isString()) {
            models.add(element.getAsString());
            return;
        }
        if (element.isJsonObject()) {
            JsonObject object = element.getAsJsonObject();
            for (String key : List.of("id", "name", "model")) {
                JsonElement value = object.get(key);
                if (value != null && value.isJsonPrimitive()) {
                    models.add(value.getAsString());
                    return;
                }
            }
        }
    }

    /**
     * Returns the agent CLI's compact/compress command preamble, or an empty list when the
     * selected client has no known compaction command.
     */
    static List<String> compactCommandFor(JsonObject arguments) {
        return switch (normalize(string(arguments, "client", "CODEX"))) {
            case "CLAUDE_CODE" -> List.of("claude", "--print", "/compact");
            case "COPILOT_CLI" -> List.of();
            default -> List.of();
        };
    }

    static boolean supportsCompact(JsonObject arguments) {
        return !compactCommandFor(arguments).isEmpty();
    }

    /**
     * Sends the agent CLI's compact/compress command as a preamble before a new user request, when
     * the client supports one. Returns a successful no-op result immediately when the CLI does not
     * support compaction so callers can skip it gracefully without failing the request.
     */
    static ShaftMcpToolResult runCompactPreamble(JsonObject arguments) {
        List<String> command = compactCommandFor(arguments);
        if (command.isEmpty()) {
            return ShaftMcpToolResult.success("Compaction is not supported by the selected agent CLI.");
        }
        if (!isCommandAvailable(command.get(0))) {
            return ShaftMcpToolResult.success("Compaction skipped: agent executable is not available on PATH.");
        }
        return runCompactPreamble(arguments, AssistantLocalAgentRunner::launchProcess);
    }

    /**
     * Package-private overload used by tests to drive a stub process directly, bypassing the PATH
     * availability check that gates the real CLI in {@link #runCompactPreamble(JsonObject)}.
     */
    static ShaftMcpToolResult runCompactPreamble(JsonObject arguments, ProcessLauncher processLauncher) {
        List<String> command = compactCommandFor(arguments);
        if (command.isEmpty()) {
            return ShaftMcpToolResult.success("Compaction is not supported by the selected agent CLI.");
        }
        Path workingDirectory = workingDirectory(arguments);
        try {
            Process process = processLauncher.launch(command, workingDirectory, Map.of());
            process.getOutputStream().close();
            CompletableFuture<String> stdout = readAsync(process.getInputStream(), null);
            CompletableFuture<String> stderr = readAsync(process.getErrorStream(), null);
            boolean finished = process.waitFor(COMPACT_TIMEOUT_SECONDS, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return ShaftMcpToolResult.success("Compaction timed out; continuing without it.");
            }
            String output = agentOutput(process.exitValue() == 0, stdoutNow(stdout), stderrNow(stderr), "");
            return ShaftMcpToolResult.success(output.isBlank() ? "Compaction complete." : output);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return ShaftMcpToolResult.success("Compaction interrupted; continuing without it.");
        } catch (IOException | RuntimeException exception) {
            return ShaftMcpToolResult.success("Compaction skipped: " + exception.getMessage());
        }
    }

    /**
     * Token usage parsed from an agent's structured result metadata (for example, the
     * {@code usage} object in the Claude CLI's {@code --output-format json} result), or an
     * estimated fallback when the agent reports no usage fields.
     *
     * @param inputTokens reported or estimated input token count
     * @param outputTokens reported or estimated output token count
     * @param estimated true when the counts are an estimate rather than agent-reported values
     */
    record TokenUsage(int inputTokens, int outputTokens, boolean estimated) {
        static TokenUsage estimate(int inputTokens, int outputTokens) {
            return new TokenUsage(Math.max(0, inputTokens), Math.max(0, outputTokens), true);
        }

        static TokenUsage reported(int inputTokens, int outputTokens) {
            return new TokenUsage(Math.max(0, inputTokens), Math.max(0, outputTokens), false);
        }

        int totalTokens() {
            return inputTokens + outputTokens;
        }
    }

    /**
     * Parses token usage from an agent's raw output. Looks for a JSON object anywhere in the text
     * containing a {@code usage} field with {@code input_tokens}/{@code output_tokens} (the Claude
     * CLI {@code --output-format json} shape), falling back to top-level
     * {@code input_tokens}/{@code output_tokens} fields. Returns {@code null} when no structured
     * usage metadata is present, so callers can fall back to an estimate and label it accordingly.
     */
    static TokenUsage parseTokenUsage(String output) {
        String trimmed = output == null ? "" : output.strip();
        if (trimmed.isBlank()) {
            return null;
        }
        JsonObject usageHolder = extractJsonObject(trimmed);
        if (usageHolder == null) {
            return null;
        }
        JsonObject usage = usageHolder.has("usage") && usageHolder.get("usage").isJsonObject()
                ? usageHolder.getAsJsonObject("usage")
                : usageHolder;
        Integer inputTokens = intField(usage, "input_tokens");
        Integer outputTokens = intField(usage, "output_tokens");
        if (inputTokens == null && outputTokens == null) {
            return null;
        }
        return TokenUsage.reported(inputTokens == null ? 0 : inputTokens, outputTokens == null ? 0 : outputTokens);
    }

    private static Integer intField(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        if (value == null || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isNumber()) {
            return null;
        }
        try {
            return value.getAsInt();
        } catch (RuntimeException exception) {
            return null;
        }
    }

    private static JsonObject extractJsonObject(String text) {
        int start = text.indexOf('{');
        if (start < 0) {
            return null;
        }
        int end = text.lastIndexOf('}');
        if (end <= start) {
            return null;
        }
        String candidate = text.substring(start, end + 1);
        try {
            JsonElement parsed = JsonParser.parseString(candidate);
            return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (JsonParseException exception) {
            return null;
        }
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
