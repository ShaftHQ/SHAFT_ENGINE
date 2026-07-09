package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.shaft.intellij.approval.AgentApprovalCapability;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
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
    private static final String BUFFERED_MODE_NOTICE =
            "Live output is unavailable for this CLI; the full response will appear once it completes.";
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
        return start(invocation, outputConsumer, processLauncher, true);
    }

    /**
     * Package-private overload used by tests to drive a stub process for a default-command
     * invocation (Claude/Codex/Copilot) whose real executable is not guaranteed to be installed on
     * the test machine's PATH, bypassing the {@code isCommandAvailable} gate that protects the real
     * CLI path in {@link #start(AssistantCommand.Invocation, Consumer, ProcessLauncher)}. Mirrors the
     * existing bypass overloads for {@link #runCompactPreamble} and {@link #listModels(JsonObject,
     * ProcessLauncher)}.
     */
    static ShaftMcpInvocation start(
            AssistantCommand.Invocation invocation,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher,
            boolean requireCommandAvailable) {
        JsonObject arguments = invocation.arguments();
        AtomicReference<Process> processReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(() -> run(
                arguments, processReference, cancellationRequested, outputConsumer, processLauncher,
                requireCommandAvailable));
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
        String model = string(arguments, "model", "").trim();
        String effort = normalize(string(arguments, "effort", ""));
        return switch (normalize(string(arguments, "client", "CODEX"))) {
            case "CLAUDE_CODE" -> claudeCommand(mode, allowSourceMutation, model);
            case "COPILOT_CLI" -> copilotCommand(mode, allowSourceMutation, model);
            default -> codexCommand(mode, allowSourceMutation, model, effort);
        };
    }

    private static ShaftMcpToolResult run(
            JsonObject arguments,
            AtomicReference<Process> processReference,
            AtomicBoolean cancellationRequested,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher) {
        return run(arguments, processReference, cancellationRequested, outputConsumer, processLauncher, true);
    }

    private static ShaftMcpToolResult run(
            JsonObject arguments,
            AtomicReference<Process> processReference,
            AtomicBoolean cancellationRequested,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher,
            boolean requireCommandAvailable) {
        List<String> command = commandFor(arguments);
        if (command.isEmpty()) {
            return ShaftMcpToolResult.failure("No local assistant command was configured.");
        }
        if (!defaultCommand(arguments)
                && "AGENT".equals(normalize(string(arguments, "mode", "ASK")))
                && !allowSourceMutation(arguments)) {
            return ShaftMcpToolResult.failure(CUSTOM_AGENT_APPROVAL_WARNING);
        }
        if (requireCommandAvailable && defaultCommand(arguments) && !isCommandAvailable(command.get(0))) {
            return ShaftMcpToolResult.failure(displayName(string(arguments, "client", "CODEX"))
                    + " executable is not available on PATH.");
        }
        Duration timeout = Duration.ofSeconds(intValue(arguments, "timeoutSeconds", DEFAULT_TIMEOUT_SECONDS));
        String stdin = string(arguments, "prompt", "");
        Path workingDirectory = workingDirectory(arguments);
        boolean isDefaultCommand = defaultCommand(arguments);
        StructuredStreamParser streamParser = isDefaultCommand ? structuredStreamParser(command) : null;
        Consumer<String> effectiveConsumer =
                effectiveConsumer(outputConsumer, streamParser, isDefaultCommand);
        OutputStream stdinStream = null;
        try {
            Process process = processLauncher.launch(command, workingDirectory, environment(arguments));
            processReference.set(process);
            InputStream stdoutStream = process.getInputStream();
            InputStream stderrStream = process.getErrorStream();
            stdinStream = process.getOutputStream();
            CompletableFuture<String> stdout = readAsync(stdoutStream, effectiveConsumer);
            CompletableFuture<String> stderr = readAsync(stderrStream, effectiveConsumer);
            stdinStream.write(stdin.getBytes(StandardCharsets.UTF_8));
            // Every local CLI is launched with plain text input (no CLI here supports a stream-json
            // *input* protocol yet), so each one reads its prompt from stdin until EOF before doing
            // anything at all. Closing stdin immediately is therefore required for every command, not
            // just an optimization: holding it open past this point guarantees a hang until the
            // timeout below fires, no matter what the prompt says.
            stdinStream.close();

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
            boolean success = process.exitValue() == 0;
            String rawStdout = stdoutNow(stdout);
            String rawStderr = stderrNow(stderr);
            String output = success && streamParser != null && streamParser.hasTerminalEvent()
                    ? streamParser.finalOutput()
                    : agentOutput(success, rawStdout, rawStderr, "");
            return success
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
            closeQuietly(stdinStream);
            processReference.set(null);
        }
    }

    /**
     * Returns a stream-event parser when {@code command} is a structured-stream default command
     * (Claude's {@code stream-json} output or Codex's experimental {@code --json} output), or
     * {@code null} for buffered-output commands (Copilot, custom commands) so callers know to fall
     * back to today's buffered {@link #agentOutput} handling untouched.
     */
    private static StructuredStreamParser structuredStreamParser(List<String> command) {
        if (command.isEmpty()) {
            return null;
        }
        String executable = command.get(0);
        if ("claude".equals(executable) && command.contains("stream-json")) {
            return new StructuredStreamParser(StructuredStreamParser.Format.CLAUDE);
        }
        if ("codex".equals(executable) && command.contains("--json")) {
            return new StructuredStreamParser(StructuredStreamParser.Format.CODEX);
        }
        return null;
    }

    /**
     * Wraps the caller's {@code outputConsumer} so that structured-stream default commands receive
     * human-readable progress lines translated from NDJSON, while buffered-output <em>default</em>
     * commands (Copilot, whose CLI has no known streaming flag) receive a single honest notice that
     * live output is unavailable, followed by silence until the buffered response returns. Custom
     * commands are never wrapped — SHAFT cannot know a hand-typed command's output shape, so its raw
     * lines are forwarded unchanged exactly as before, the same pass-through behavior a slow custom
     * CLI already relied on. A {@code null} caller consumer is left as {@code null} in every case
     * since there is nothing to forward lines to.
     */
    private static Consumer<String> effectiveConsumer(
            Consumer<String> outputConsumer, StructuredStreamParser streamParser, boolean isDefaultCommand) {
        if (outputConsumer == null) {
            return null;
        }
        if (streamParser != null) {
            return line -> streamParser.accept(line, outputConsumer);
        }
        if (!isDefaultCommand) {
            return outputConsumer;
        }
        AtomicBoolean notified = new AtomicBoolean();
        return line -> {
            if (notified.compareAndSet(false, true)) {
                outputConsumer.accept(BUFFERED_MODE_NOTICE);
            }
        };
    }

    /**
     * Translates a structured NDJSON stream (Claude {@code stream-json} or Codex experimental
     * {@code --json} events) into human-readable progress lines delivered through the caller's
     * {@code outputConsumer}, while separately capturing the terminal event's final answer text and
     * usage object. Both CLIs' event schemas are experimental/subject to drift, so unrecognized event
     * types and missing fields are skipped rather than treated as parse failures — only a
     * well-formed terminal event upgrades the result away from today's buffered {@link #agentOutput}
     * fallback. The Verbose toggle exists precisely so the user can watch what the wrapped CLI is
     * doing, so this is deliberately generous rather than sparse: non-JSON output (banners, warnings)
     * passes straight through, and internal chain-of-thought (Claude's {@code thinking} blocks,
     * Codex's {@code reasoning} items) is surfaced with a clear label instead of hidden.
     *
     * <p>Not thread-safe: each invocation of {@link #run} creates its own instance, but stdout and
     * stderr are each read on their own thread via {@link #readAsync}, so access to the mutable
     * terminal-event fields is synchronized.
     */
    private static final class StructuredStreamParser {
        enum Format { CLAUDE, CODEX }

        private final Format format;
        private final Map<String, String> toolNamesByUseId = new HashMap<>();
        private String answer;
        private Integer inputTokens;
        private Integer outputTokens;

        StructuredStreamParser(Format format) {
            this.format = format;
        }

        synchronized void accept(String line, Consumer<String> outputConsumer) {
            String trimmed = line == null ? "" : line.strip();
            if (trimmed.isEmpty()) {
                return;
            }
            JsonObject event = parseObject(trimmed);
            if (event == null) {
                // Non-JSON output (banners, warnings) is not part of the structured contract but is
                // still useful progress information, so it passes through unchanged.
                outputConsumer.accept(line);
                return;
            }
            String humanReadableLine = format == Format.CLAUDE ? describeClaudeEvent(event) : describeCodexEvent(event);
            if (humanReadableLine != null) {
                outputConsumer.accept(humanReadableLine);
            }
        }

        synchronized boolean hasTerminalEvent() {
            return answer != null;
        }

        synchronized String finalOutput() {
            JsonObject usage = new JsonObject();
            usage.addProperty("input_tokens", orZero(inputTokens));
            usage.addProperty("output_tokens", orZero(outputTokens));
            JsonObject usageHolder = new JsonObject();
            usageHolder.add("usage", usage);
            return answer.strip() + "\n\n" + usageHolder;
        }

        /**
         * Describes an "assistant" event as one or more human-readable lines (joined with newlines),
         * covering every recognized block in the message rather than stopping at the first match: an
         * extended-thinking block followed by a tool call and some text all become separate lines, so
         * Verbose mode shows the CLI's full train of thought instead of only a fragment of it.
         */
        private String describeClaudeEvent(JsonObject event) {
            String type = stringField(event, "type");
            if ("assistant".equals(type)) {
                JsonObject message = objectField(event, "message");
                JsonElement content = message == null ? null : message.get("content");
                if (content != null && content.isJsonArray()) {
                    List<String> lines = new ArrayList<>();
                    for (JsonElement blockElement : content.getAsJsonArray()) {
                        if (!blockElement.isJsonObject()) {
                            continue;
                        }
                        JsonObject block = blockElement.getAsJsonObject();
                        String blockType = stringField(block, "type");
                        if ("thinking".equals(blockType)) {
                            String thinking = stringField(block, "thinking");
                            if (thinking != null && !thinking.isBlank()) {
                                lines.add("Thinking: " + thinking);
                            }
                        } else if ("redacted_thinking".equals(blockType)) {
                            lines.add("Thinking: (redacted by Claude for safety)");
                        } else if ("tool_use".equals(blockType)) {
                            String toolName = stringField(block, "name");
                            String toolUseId = stringField(block, "id");
                            if (toolUseId != null && toolName != null) {
                                toolNamesByUseId.put(toolUseId, toolName);
                            }
                            String label = toolName == null ? "(unknown)" : toolName;
                            String summary = toolInputSummary(objectField(block, "input"));
                            lines.add(summary == null
                                    ? "Calling tool " + label + "..."
                                    : "Calling tool " + label + " (" + summary + ")...");
                        } else if ("text".equals(blockType)) {
                            String text = stringField(block, "text");
                            if (text != null && !text.isBlank()) {
                                lines.add(text);
                            }
                        }
                    }
                    return lines.isEmpty() ? null : String.join("\n", lines);
                }
                return null;
            }
            if ("user".equals(type)) {
                return describeClaudeToolResults(event);
            }
            if ("result".equals(type)) {
                String resultText = stringField(event, "result");
                answer = resultText == null ? "" : resultText;
                JsonObject usage = objectField(event, "usage");
                inputTokens = intField(usage, "input_tokens");
                outputTokens = intField(usage, "output_tokens");
                return null;
            }
            return null;
        }

        /**
         * Describes a "user" event's {@code tool_result} blocks (Claude's stream-json protocol
         * delivers a completed tool call's outcome as a synthetic user-role message, not as part of
         * the assistant event that requested it) as one line per result, correlated back to the
         * requesting call's tool name via {@link #toolNamesByUseId}. Without this, Verbose mode shows
         * "Calling tool X..." and then nothing — the exact "generic thinking, not what it's actually
         * doing" gap the Verbose toggle exists to close.
         */
        private String describeClaudeToolResults(JsonObject event) {
            JsonObject message = objectField(event, "message");
            JsonElement content = message == null ? null : message.get("content");
            if (content == null || !content.isJsonArray()) {
                return null;
            }
            List<String> lines = new ArrayList<>();
            for (JsonElement blockElement : content.getAsJsonArray()) {
                if (!blockElement.isJsonObject()) {
                    continue;
                }
                JsonObject block = blockElement.getAsJsonObject();
                if (!"tool_result".equals(stringField(block, "type"))) {
                    continue;
                }
                String toolName = toolNamesByUseId.get(stringField(block, "tool_use_id"));
                String label = toolName == null ? "tool" : toolName;
                String text = toolResultText(block.get("content"));
                String summary = text == null || text.isBlank() ? "(no output)" : truncate(firstLine(text.strip()), 160);
                lines.add((booleanField(block, "is_error") ? "Tool failed (" : "Tool result (") + label + "): " + summary);
            }
            return lines.isEmpty() ? null : String.join("\n", lines);
        }

        private String describeCodexEvent(JsonObject event) {
            String type = stringField(event, "type");
            if ("item.completed".equals(type) || "item.updated".equals(type)) {
                JsonObject item = objectField(event, "item");
                String itemType = stringField(item, "type");
                if ("reasoning".equals(itemType)) {
                    String reasoning = firstNonBlank(stringField(item, "text"), stringField(item, "summary"));
                    return reasoning == null ? null : "Reasoning: " + reasoning;
                }
                if ("tool_call".equals(itemType) || "command_execution".equals(itemType) || "mcp_tool_call".equals(itemType)) {
                    String toolName = firstNonBlank(stringField(item, "name"), stringField(item, "tool"), stringField(item, "command"));
                    String label = toolName == null ? "(unknown)" : toolName;
                    String summary = toolInputSummary(item);
                    List<String> lines = new ArrayList<>();
                    lines.add(summary == null || summary.equals(label)
                            ? "Calling tool " + label + "..."
                            : "Calling tool " + label + " (" + summary + ")...");
                    if ("item.completed".equals(type)) {
                        String output = firstNonBlank(
                                stringField(item, "aggregated_output"), stringField(item, "output"), stringField(item, "result"));
                        if (output != null && !output.isBlank()) {
                            Integer exitCode = intField(item, "exit_code");
                            String outputSummary = truncate(firstLine(output.strip()), 160);
                            lines.add(exitCode != null && exitCode != 0
                                    ? "Tool failed (" + label + ", exit " + exitCode + "): " + outputSummary
                                    : "Tool result (" + label + "): " + outputSummary);
                        }
                    }
                    return String.join("\n", lines);
                }
                if ("agent_message".equals(itemType)) {
                    String text = stringField(item, "text");
                    if (text != null && !text.isBlank()) {
                        return text;
                    }
                }
                return null;
            }
            if ("turn.completed".equals(type) || "turn.failed".equals(type)) {
                JsonObject usage = objectField(event, "usage");
                inputTokens = firstNonNull(intField(usage, "input_tokens"), inputTokens);
                outputTokens = firstNonNull(intField(usage, "output_tokens"), outputTokens);
                if ("turn.completed".equals(type)) {
                    String lastAgentMessage = stringField(event, "last_agent_message");
                    answer = lastAgentMessage != null ? lastAgentMessage : (answer == null ? "" : answer);
                }
                return null;
            }
            return null;
        }

        private static String firstNonBlank(String... candidates) {
            for (String candidate : candidates) {
                if (candidate != null && !candidate.isBlank()) {
                    return candidate;
                }
            }
            return null;
        }

        private static Integer firstNonNull(Integer preferred, Integer fallback) {
            return preferred != null ? preferred : fallback;
        }

        private static JsonObject parseObject(String candidate) {
            if (!candidate.startsWith("{")) {
                return null;
            }
            try {
                JsonElement parsed = JsonParser.parseString(candidate);
                return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
            } catch (JsonParseException exception) {
                return null;
            }
        }

        private static String stringField(JsonObject object, String key) {
            JsonElement value = object == null ? null : object.get(key);
            return value != null && value.isJsonPrimitive() && value.getAsJsonPrimitive().isString()
                    ? value.getAsString()
                    : null;
        }

        private static JsonObject objectField(JsonObject object, String key) {
            JsonElement value = object == null ? null : object.get(key);
            return value != null && value.isJsonObject() ? value.getAsJsonObject() : null;
        }

        private static boolean booleanField(JsonObject object, String key) {
            JsonElement value = object == null ? null : object.get(key);
            return value != null && value.isJsonPrimitive() && value.getAsJsonPrimitive().isBoolean()
                    && value.getAsBoolean();
        }

        /**
         * Picks the first present, non-blank value among the input keys most likely to tell the user
         * what a tool call is actually doing (the command run, the file touched, the pattern searched
         * for, ...), collapsed to a single line and truncated so a large payload never floods the
         * transcript. Returns {@code null} when none of the known keys are present rather than
         * guessing at unfamiliar tool schemas.
         */
        private static String toolInputSummary(JsonObject input) {
            if (input == null) {
                return null;
            }
            for (String key : List.of(
                    "command", "file_path", "path", "pattern", "url", "query", "description", "prompt")) {
                String value = stringField(input, key);
                if (value != null && !value.isBlank()) {
                    return truncate(value.strip().replaceAll("\\s+", " "), 80);
                }
            }
            return null;
        }

        /**
         * Extracts the human-readable text of a {@code tool_result} block's {@code content}, which per
         * Claude's stream-json protocol is either a plain string or an array of content blocks (text
         * blocks are joined; non-text blocks such as images are skipped since they have nothing to
         * show in a text transcript).
         */
        private static String toolResultText(JsonElement content) {
            if (content == null || content.isJsonNull()) {
                return null;
            }
            if (content.isJsonPrimitive() && content.getAsJsonPrimitive().isString()) {
                return content.getAsString();
            }
            if (content.isJsonArray()) {
                List<String> parts = new ArrayList<>();
                for (JsonElement element : content.getAsJsonArray()) {
                    if (element.isJsonObject()) {
                        String text = stringField(element.getAsJsonObject(), "text");
                        if (text != null && !text.isBlank()) {
                            parts.add(text);
                        }
                    }
                }
                return parts.isEmpty() ? null : String.join("\n", parts);
            }
            return null;
        }

        private static String firstLine(String text) {
            int newlineIndex = text.indexOf('\n');
            return newlineIndex < 0 ? text : text.substring(0, newlineIndex);
        }

        private static String truncate(String value, int maxLength) {
            return value.length() > maxLength ? value.substring(0, maxLength) + "..." : value;
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

    /**
     * {@code --json} is an experimental Codex CLI flag (documented alongside its
     * {@code --experimental-json} alias) that switches {@code codex exec} to emit newline-delimited
     * JSON events (thread/turn/item lifecycle plus a final {@code turn.completed} usage summary)
     * instead of a single buffered blob. Because it is experimental, its event schema is subject to
     * drift between Codex CLI releases, so the NDJSON parser in {@link #run} is deliberately
     * tolerant: unrecognized event types or fields are skipped rather than treated as errors.
     *
     * <p>{@code mcp_servers.shaft-mcp.default_tools_approval_mode="approve"} is a launch-time
     * pre-approval flag — see {@link AgentApprovalCapability#CODEX}, which has no interactive
     * approval scopes, so this is the only approval mechanism available to Codex today. It is only
     * added when {@link AgentApprovalCapability#isAutoApproveGranted(boolean)} grants it; otherwise
     * Codex runs in AGENT mode with its default (deny-by-default) tool approval behavior, matching
     * the read-only sandbox it also falls back to.
     */
    private static List<String> codexCommand(String mode, boolean allowSourceMutation, String model, String effort) {
        List<String> command = new ArrayList<>(List.of("codex", "exec"));
        if (!model.isBlank()) {
            command.add("--model");
            command.add(model);
        }
        if (AssistantModelCatalog.isExplicitEffort(effort)) {
            command.add("-c");
            command.add("model_reasoning_effort=\"" + effort.toLowerCase(Locale.ROOT) + "\"");
        }
        command.add("--sandbox");
        command.add("AGENT".equals(mode) && allowSourceMutation ? "workspace-write" : "read-only");
        if ("AGENT".equals(mode)) {
            if (AgentApprovalCapability.CODEX.isAutoApproveGranted(allowSourceMutation)) {
                command.add("-c");
                command.add("mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"");
            }
            command.add("-c");
            command.add("mcp_servers.shaft-mcp.tool_timeout_sec=600");
        }
        command.add("--json");
        command.add("-");
        return List.copyOf(command);
    }

    /**
     * {@code --output-format stream-json} requires {@code --verbose} whenever it is paired with
     * {@code --print} (the CLI rejects the combination otherwise), so every mode below carries both
     * flags together to receive incremental NDJSON events instead of a single buffered blob. The
     * AGENT-mode permission mode is the Claude equivalent of an auto-approve flag: {@code
     * acceptEdits} is only selected when {@link AgentApprovalCapability#isAutoApproveGranted(boolean)}
     * grants it, otherwise AGENT mode falls back to {@code plan} exactly as before.
     */
    private static List<String> claudeCommand(String mode, boolean allowSourceMutation, String model) {
        List<String> command = new ArrayList<>(List.of("claude", "--print"));
        if (!model.isBlank()) {
            command.add("--model");
            command.add(model);
        }
        if ("PLAN".equals(mode) || "AGENT".equals(mode)) {
            command.add("--permission-mode");
            command.add("AGENT".equals(mode)
                    && AgentApprovalCapability.CLAUDE_CODE.isAutoApproveGranted(allowSourceMutation)
                    ? "acceptEdits" : "plan");
        }
        command.add("--output-format");
        command.add("stream-json");
        command.add("--verbose");
        return List.copyOf(command);
    }

    private static List<String> copilotCommand(String mode, boolean allowSourceMutation, String model) {
        List<String> command = new ArrayList<>();
        command.add("copilot");
        command.add(switch (mode) {
            case "PLAN" -> "plan";
            case "AGENT" -> AgentApprovalCapability.COPILOT_CLI.isAutoApproveGranted(allowSourceMutation)
                    ? "agent" : "ask";
            default -> "ask";
        });
        if (!model.isBlank()) {
            command.add("--model");
            command.add(model);
        }
        return List.copyOf(command);
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
     * Parses token usage from an agent's raw output. First tries the output as NDJSON: each line is
     * parsed independently as a JSON object carrying (or holding a nested {@code usage} object with)
     * {@code input_tokens}/{@code output_tokens}, and the <em>last</em> line that yields usage fields
     * wins — matching a structured-stream terminal event that may be followed by later
     * housekeeping/log lines. When no line parses to usage, falls back to the original
     * first-brace/last-brace whole-text extraction (the Claude CLI {@code --output-format json}
     * shape). Returns {@code null} when no structured usage metadata is present in either form, so
     * callers can fall back to an estimate and label it accordingly.
     */
    static TokenUsage parseTokenUsage(String output) {
        String trimmed = output == null ? "" : output.strip();
        if (trimmed.isBlank()) {
            return null;
        }
        TokenUsage fromLines = parseTokenUsageFromLines(trimmed);
        if (fromLines != null) {
            return fromLines;
        }
        JsonObject usageHolder = extractJsonObject(trimmed);
        if (usageHolder == null) {
            return null;
        }
        return usageFromHolder(usageHolder);
    }

    /**
     * Scans {@code text} line by line, parsing each non-blank line as a standalone JSON object and
     * keeping the usage parsed from the last line that has one. Lines that are not valid JSON (plain
     * prose, partial output) are skipped rather than treated as errors, since NDJSON output is
     * expected to interleave structured events with the occasional non-JSON banner line.
     */
    private static TokenUsage parseTokenUsageFromLines(String text) {
        TokenUsage lastMatch = null;
        for (String line : text.split("\\r?\\n")) {
            String candidate = line.strip();
            if (candidate.length() < 2 || !candidate.startsWith("{") || !candidate.endsWith("}")) {
                continue;
            }
            JsonObject parsed;
            try {
                JsonElement element = JsonParser.parseString(candidate);
                if (!element.isJsonObject()) {
                    continue;
                }
                parsed = element.getAsJsonObject();
            } catch (JsonParseException exception) {
                continue;
            }
            TokenUsage usage = usageFromHolder(parsed);
            if (usage != null) {
                lastMatch = usage;
            }
        }
        return lastMatch;
    }

    private static TokenUsage usageFromHolder(JsonObject usageHolder) {
        JsonObject usage = resolveUsageObject(usageHolder);
        Integer inputTokens = intField(usage, "input_tokens");
        Integer outputTokens = intField(usage, "output_tokens");
        if (inputTokens == null && outputTokens == null) {
            return null;
        }
        return TokenUsage.reported(orZero(inputTokens), orZero(outputTokens));
    }

    /**
     * Returns {@code output} with its trailing usage-metadata JSON line removed, leaving only the
     * human-facing answer text. This is the display-side counterpart to {@link #parseTokenUsage}:
     * both inspect the same trailing line, so a change to one's line-detection rules must be mirrored
     * in the other. When the last non-blank line is not a parseable usage object, {@code output} is
     * returned unchanged (stripped of surrounding whitespace) so plain prose responses are never
     * altered.
     */
    static String stripTrailingUsageMetadata(String output) {
        String trimmed = output == null ? "" : output.strip();
        if (trimmed.isBlank()) {
            return trimmed;
        }
        String[] lines = trimmed.split("\\r?\\n");
        int lastLineIndex = lines.length - 1;
        String lastLine = lines[lastLineIndex].strip();
        if (lastLine.length() < 2 || !lastLine.startsWith("{") || !lastLine.endsWith("}")) {
            return trimmed;
        }
        JsonObject parsed;
        try {
            JsonElement element = JsonParser.parseString(lastLine);
            if (!element.isJsonObject()) {
                return trimmed;
            }
            parsed = element.getAsJsonObject();
        } catch (JsonParseException exception) {
            return trimmed;
        }
        if (usageFromHolder(parsed) == null) {
            return trimmed;
        }
        StringBuilder remainder = new StringBuilder();
        for (int index = 0; index < lastLineIndex; index++) {
            remainder.append(lines[index]).append('\n');
        }
        return remainder.toString().strip();
    }

    private static JsonObject resolveUsageObject(JsonObject usageHolder) {
        JsonElement usage = usageHolder.get("usage");
        if (usage != null && usage.isJsonObject()) {
            return usage.getAsJsonObject();
        }
        return usageHolder;
    }

    private static int orZero(Integer value) {
        return value == null ? 0 : value;
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

    private static void closeQuietly(OutputStream stream) {
        if (stream == null) {
            return;
        }
        try {
            stream.close();
        } catch (IOException ignored) {
            // The process may have already exited and closed its own end of the pipe.
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
