package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.shaft.intellij.approval.AgentApprovalCapability;
import com.shaft.intellij.approval.LocalAgentApprovalBridge;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.mcp.ShaftPluginExecutor;

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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.LongSupplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Runs selected local assistant CLIs directly so their markdown stays user-facing.
 */
final class AssistantLocalAgentRunner {
    static final String LOCAL_AGENT_TOOL = "autobot_local_agent_run";
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    /**
     * Cap on how much a run's deadline can be extended by accumulated approval-deliberation time
     * (see {@link #awaitProcessWithApprovalExtension}) -- deliberation up to this cap never kills a
     * run, but abandonment past it still gets cleaned up, per issue #3633.
     */
    static final int MAX_APPROVAL_EXTENSION_SECONDS = 600;
    private static final long DEADLINE_POLL_INTERVAL_MILLIS = 200;
    private static final int MODEL_LIST_TIMEOUT_SECONDS = 20;
    private static final int COMPACT_TIMEOUT_SECONDS = 30;
    private static final int MCP_ACCESS_TIMEOUT_SECONDS = 45;
    private static final String CUSTOM_AGENT_APPROVAL_WARNING =
            "Custom Agent commands require Allow source edits because SHAFT cannot enforce read-only execution.";
    private static final String BUFFERED_MODE_NOTICE =
            "This CLI has no structured live stream; its raw output is shown as-is below until it completes.";
    private static final Pattern MODEL_LINE_PATTERN = Pattern.compile("(claude-[a-z0-9.-]+|gpt-[a-z0-9.-]+|o[0-9][a-z0-9.-]*)",
            Pattern.CASE_INSENSITIVE);
    /**
     * Invisible prefix tagging a line delivered to the live output consumer as genuinely raw, non-JSON
     * CLI stdout (a banner/warning outside the structured NDJSON contract, see {@link
     * StructuredStreamParser#accept}) as opposed to a SHAFT-translated milestone line ("Calling tool
     * ...", "Thinking: ...", ...). Both travel through the same {@code Consumer<String>}, so
     * ShaftAssistantPanel needs this tag to tell them apart without guessing from content: issue #3918
     * gates non-verbose mode's compact milestone bubbles to translated content only, while Verbose mode
     * must still see the raw line unchanged (the marker is stripped before any display). Never sent for
     * buffered-default (Copilot) or custom-command output -- see {@link #hasStructuredStream}.
     */
    static final String RAW_STDOUT_MARKER = "\u0000raw-stdout\u0000";

    private AssistantLocalAgentRunner() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Whether {@code arguments} launches a command whose live stream can ever contain a SHAFT-
     * translated milestone line -- true only for Claude Code/Codex default commands, which get a
     * {@link StructuredStreamParser} (see {@link #structuredStreamParser}); false for Copilot's default
     * command (buffered, zero translation -- see {@link #effectiveConsumer}) and for any custom
     * command (forwarded raw and unwrapped). Issue #3918: ShaftAssistantPanel uses this once per run to
     * decide whether its non-verbose compact milestone bubbles have anything legitimate to show, since
     * a buffered/custom run's entire live stream is raw CLI passthrough with no SHAFT authorship at
     * all.
     */
    static boolean hasStructuredStream(JsonObject arguments) {
        return defaultCommand(arguments) && !"COPILOT_CLI".equals(normalize(string(arguments, "client", "CODEX")));
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
        return start(invocation, outputConsumer, processLauncher, requireCommandAvailable, null,
                LocalAgentApprovalBridge::start);
    }

    /**
     * Package-private overload used by tests to drive the {@code verbose} parameter (see {@link
     * #agentOutput(boolean, String, String, String, boolean)}) directly without an approval handler.
     */
    static ShaftMcpInvocation start(
            AssistantCommand.Invocation invocation,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher,
            boolean requireCommandAvailable,
            boolean verbose) {
        return start(invocation, outputConsumer, processLauncher, requireCommandAvailable, null,
                LocalAgentApprovalBridge::start, verbose);
    }

    /**
     * Package-private overload used by {@link com.shaft.intellij.ui.ShaftAssistantPanel} to supply a
     * real interactive-approval callback. Tests that don't exercise the approval bridge should keep
     * using the shorter overloads above rather than passing a handler.
     */
    static ShaftMcpInvocation start(
            AssistantCommand.Invocation invocation,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher,
            boolean requireCommandAvailable,
            LocalAgentApprovalBridge.ApprovalRequestHandler approvalHandler) {
        return start(invocation, outputConsumer, processLauncher, requireCommandAvailable, approvalHandler,
                LocalAgentApprovalBridge::start);
    }

    /**
     * Package-private overload used by tests to drive a fake {@link ApprovalBridgeLauncher} instead
     * of spawning a real HTTP server, so bridge-wiring tests stay fast and deterministic. Defaults
     * {@code verbose} to {@code true} (today's unconditional behavior, unchanged) -- see {@link
     * #start(AssistantCommand.Invocation, Consumer, ProcessLauncher, boolean,
     * LocalAgentApprovalBridge.ApprovalRequestHandler, ApprovalBridgeLauncher, boolean)} for the
     * verbose-aware entry point.
     */
    static ShaftMcpInvocation start(
            AssistantCommand.Invocation invocation,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher,
            boolean requireCommandAvailable,
            LocalAgentApprovalBridge.ApprovalRequestHandler approvalHandler,
            ApprovalBridgeLauncher bridgeLauncher) {
        return start(invocation, outputConsumer, processLauncher, requireCommandAvailable, approvalHandler,
                bridgeLauncher, true);
    }

    /**
     * Canonical overload: every shorter {@code start} overload above ultimately delegates here.
     * {@code verbose} gates whether {@link #run} folds raw stderr into the compact terminal answer
     * on a buffered/failed run (issue #3965) -- {@code true} preserves the CLI's raw stderr in that
     * answer exactly as before, {@code false} withholds it (the content remains reachable via the
     * live {@code outputConsumer} stream, which this parameter never affects).
     */
    static ShaftMcpInvocation start(
            AssistantCommand.Invocation invocation,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher,
            boolean requireCommandAvailable,
            LocalAgentApprovalBridge.ApprovalRequestHandler approvalHandler,
            ApprovalBridgeLauncher bridgeLauncher,
            boolean verbose) {
        JsonObject arguments = invocation.arguments();
        AtomicReference<Process> processReference = new AtomicReference<>();
        AtomicBoolean cancellationRequested = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(() -> run(
                arguments, processReference, cancellationRequested, outputConsumer, processLauncher,
                requireCommandAvailable, approvalHandler, bridgeLauncher, verbose),
                ShaftPluginExecutor.getInstance().executor());
        return new ShaftMcpInvocation(
                future,
                () -> cancel(processReference, cancellationRequested, false),
                () -> cancel(processReference, cancellationRequested, true));
    }

    /**
     * Launches a {@link LocalAgentApprovalBridge}. Extracted so tests can substitute a fake bridge
     * instead of spawning a real HTTP server.
     */
    @FunctionalInterface
    interface ApprovalBridgeLauncher {
        LocalAgentApprovalBridge launch(LocalAgentApprovalBridge.ApprovalRequestHandler handler, Duration decisionTimeout)
                throws IOException;
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
     * Starts a local agent invocation with a real interactive-approval callback (see {@link
     * #startWithOptionalCompact(AssistantCommand.Invocation, boolean, Consumer)} for the compact
     * preamble behavior, unchanged here).
     */
    static ShaftMcpInvocation startWithOptionalCompact(
            AssistantCommand.Invocation invocation,
            boolean autoCompactEnabled,
            Consumer<String> outputConsumer,
            LocalAgentApprovalBridge.ApprovalRequestHandler approvalHandler) {
        if (autoCompactEnabled) {
            ShaftMcpToolResult compactResult = runCompactPreamble(invocation.arguments());
            if (outputConsumer != null && compactResult.output() != null && !compactResult.output().isBlank()) {
                outputConsumer.accept(compactResult.output());
            }
        }
        return start(invocation, outputConsumer, AssistantLocalAgentRunner::launchProcess, true, approvalHandler);
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

    /**
     * Deep readiness for the setup screen's "Check now" button: beyond {@link #readiness}'s PATH
     * check, asks the selected CLI itself whether it can access shaft-mcp, so a user who just
     * reinstalled shaft-mcp gets a real "your agent CLI can now use it" verdict instead of a bare
     * executable check. Claude's {@code claude mcp get shaft-mcp} actually spawns and connects to
     * the server (empirically ~11s, exit code 0 even for an unknown server, so the OUTPUT is the
     * contract); Codex's {@code codex mcp get shaft-mcp} is a fast config read. Never called at
     * panel-construction time — only from an explicit user-triggered check on a background thread.
     */
    static ShaftMcpToolResult connectionReadiness(String client, String runtime) {
        ShaftMcpToolResult basic = readiness(client, runtime);
        if (!basic.success() || !"CLI".equals(normalize(runtime))) {
            return basic;
        }
        return mcpAccessReadiness(client, AssistantLocalAgentRunner::launchProcess);
    }

    /**
     * Returns the CLI command that reports shaft-mcp access for the client, or an empty list when
     * the client has no known MCP inspection command (Copilot CLI today).
     */
    static List<String> mcpAccessCommandFor(String client) {
        return switch (normalize(client)) {
            case "CLAUDE_CODE" -> List.of("claude", "mcp", "get", "shaft-mcp");
            case "COPILOT_CLI" -> List.of();
            default -> List.of("codex", "mcp", "get", "shaft-mcp");
        };
    }

    /**
     * Package-private overload used by tests to drive a stub process. Unknown output shapes and
     * probe launch failures fail OPEN (PATH-level success) so an older CLI without an {@code mcp
     * get} subcommand never blocks setup; only explicit negative markers fail the check.
     */
    static ShaftMcpToolResult mcpAccessReadiness(String client, ProcessLauncher processLauncher) {
        List<String> command = mcpAccessCommandFor(client);
        String displayName = displayName(client);
        if (command.isEmpty()) {
            return ShaftMcpToolResult.success(displayName + " executable is available on PATH.");
        }
        try {
            Process process = processLauncher.launch(command, Path.of("."), Map.of());
            process.getOutputStream().close();
            CompletableFuture<String> stdout = readAsync(process.getInputStream(), null);
            CompletableFuture<String> stderr = readAsync(process.getErrorStream(), null);
            if (!process.waitFor(MCP_ACCESS_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
                process.destroyForcibly();
                return ShaftMcpToolResult.success(displayName
                        + " is on PATH; the shaft-mcp access check timed out and was skipped.");
            }
            String output = (stdoutNow(stdout) + "\n" + stderrNow(stderr)).strip();
            return interpretMcpAccessOutput(client, process.exitValue(), output);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return ShaftMcpToolResult.success(displayName
                    + " is on PATH; the shaft-mcp access check was interrupted and skipped.");
        } catch (IOException | RuntimeException exception) {
            return ShaftMcpToolResult.success(displayName
                    + " is on PATH; the shaft-mcp access check could not run (" + exception.getMessage() + ").");
        }
    }

    private static ShaftMcpToolResult interpretMcpAccessOutput(String client, int exitValue, String output) {
        return "CLAUDE_CODE".equals(normalize(client))
                ? interpretClaudeMcpAccessOutput(displayName(client), exitValue, output)
                : interpretCodexMcpAccessOutput(displayName(client), exitValue, output);
    }

    private static ShaftMcpToolResult interpretClaudeMcpAccessOutput(
            String displayName, int exitValue, String output) {
        String lower = output.toLowerCase(Locale.ROOT);
        if (lower.contains("no mcp server named")) {
            return ShaftMcpToolResult.failure("shaft-mcp is not registered with " + displayName
                    + ". Run the SHAFT MCP installer command, then check again.");
        }
        if (lower.contains("failed to connect") || output.contains("✘")) {
            return ShaftMcpToolResult.failure(displayName + " sees shaft-mcp but could not connect to it."
                    + " If you just installed or updated shaft-mcp, restart " + displayName
                    + " with the restart command below, then check again.\n\n" + output);
        }
        if (lower.contains("connected")) {
            return ShaftMcpToolResult.success(displayName + " can access shaft-mcp (status: connected).");
        }
        if (exitValue != 0) {
            return ShaftMcpToolResult.failure(displayName + " could not verify shaft-mcp access:\n" + output);
        }
        return undeterminedMcpAccess(displayName);
    }

    /**
     * Codex: {@code codex mcp get} is a config read; enabled registration is the positive marker.
     */
    private static ShaftMcpToolResult interpretCodexMcpAccessOutput(
            String displayName, int exitValue, String output) {
        String lower = output.toLowerCase(Locale.ROOT);
        if (lower.contains("shaft-mcp") && lower.contains("enabled: true")) {
            return ShaftMcpToolResult.success(displayName + " has shaft-mcp registered and enabled.");
        }
        if (exitValue != 0 || lower.contains("not found") || lower.contains("no mcp server")
                || lower.contains("unknown server")) {
            return ShaftMcpToolResult.failure("shaft-mcp is not registered with " + displayName
                    + ". Run the SHAFT MCP installer command, then check again.");
        }
        return undeterminedMcpAccess(displayName);
    }

    private static ShaftMcpToolResult undeterminedMcpAccess(String displayName) {
        return ShaftMcpToolResult.success(displayName
                + " is on PATH; the shaft-mcp access status could not be determined from its output.");
    }

    static List<String> commandFor(JsonObject arguments) {
        return commandFor(arguments, null);
    }

    private static List<String> commandFor(JsonObject arguments, LocalAgentApprovalBridge bridge) {
        List<String> custom = customCommand(arguments);
        if (!custom.isEmpty()) {
            return custom;
        }
        boolean allowSourceMutation = allowSourceMutation(arguments);
        String mode = normalize(string(arguments, "mode", "ASK"));
        String model = string(arguments, "model", "").trim();
        String effort = normalize(string(arguments, "effort", ""));
        return switch (normalize(string(arguments, "client", "CODEX"))) {
            case "CLAUDE_CODE" -> claudeCommand(mode, allowSourceMutation, model, bridge);
            case "COPILOT_CLI" -> copilotCommand(mode, allowSourceMutation, model);
            default -> codexCommand(mode, allowSourceMutation, model, effort);
        };
    }

    /**
     * Returns whether an interactive mid-run approval bridge should be launched for this invocation:
     * every default (non-custom) Claude Code AGENT-mode run whose caller supplied a real {@code
     * approvalHandler}. Granted runs ("Allow source edits" checked) need the bridge too: {@code
     * --permission-mode acceptEdits} only auto-approves file edits, so without a bridge every Bash
     * and MCP tool call is auto-DENIED in {@code --print} mode — reproduced live as 16 denials in a
     * single capture-integration run that burned ~28 turns retrying declined tools before answering
     * with a terse no-op. Other combinations (a {@code null} handler, PLAN/ASK mode, a different
     * client, a custom command) are unchanged: {@code plan} remains the fallback when no bridge is
     * available (headless/tests/legacy call sites).
     */
    private static boolean needsInteractiveApproval(
            JsonObject arguments, LocalAgentApprovalBridge.ApprovalRequestHandler approvalHandler) {
        if (approvalHandler == null || !defaultCommand(arguments)) {
            return false;
        }
        if (!"CLAUDE_CODE".equals(normalize(string(arguments, "client", "CODEX")))) {
            return false;
        }
        return "AGENT".equals(normalize(string(arguments, "mode", "ASK")));
    }

    /**
     * Wraps {@code handler} so the transcript shows a status line while a tool-approval decision is
     * pending and another when it resolves, satisfying issue #3633's "status line reflects the
     * paused timer during a pending approval" acceptance criterion via the existing narration
     * (output-consumer) hooks -- no new UI plumbing needed. Returns {@code handler} unchanged when
     * either it or {@code outputConsumer} is {@code null} (nothing to narrate, or nowhere to narrate
     * it to).
     */
    static LocalAgentApprovalBridge.ApprovalRequestHandler narrateApproval(
            LocalAgentApprovalBridge.ApprovalRequestHandler handler, Consumer<String> outputConsumer) {
        if (handler == null || outputConsumer == null) {
            return handler;
        }
        return (toolName, toolInput) -> {
            outputConsumer.accept("Waiting for your approval on " + toolName + " -- run timer paused.");
            CompletableFuture<LocalAgentApprovalBridge.Decision> future = handler.requestApproval(toolName, toolInput);
            future.whenComplete((decision, throwable) ->
                    outputConsumer.accept("Approval resolved -- run timer resumed."));
            return future;
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
        return run(arguments, processReference, cancellationRequested, outputConsumer, processLauncher,
                requireCommandAvailable, null, LocalAgentApprovalBridge::start, true);
    }

    private static ShaftMcpToolResult run(
            JsonObject arguments,
            AtomicReference<Process> processReference,
            AtomicBoolean cancellationRequested,
            Consumer<String> outputConsumer,
            ProcessLauncher processLauncher,
            boolean requireCommandAvailable,
            LocalAgentApprovalBridge.ApprovalRequestHandler approvalHandler,
            ApprovalBridgeLauncher bridgeLauncher,
            boolean verbose) {
        Duration timeout = Duration.ofSeconds(intValue(arguments, "timeoutSeconds", DEFAULT_TIMEOUT_SECONDS));
        LocalAgentApprovalBridge.ApprovalRequestHandler narratingApprovalHandler =
                narrateApproval(approvalHandler, outputConsumer);
        LocalAgentApprovalBridge bridge = null;
        if (needsInteractiveApproval(arguments, approvalHandler)) {
            try {
                // The bridge's own decision timeout is deliberately a little longer than the process
                // timeout below: if the user never answers, the process timeout fires first and this
                // run fails on its own; the bridge timeout is a defensive backstop for the case where
                // close() somehow isn't reached promptly, not the primary mechanism.
                bridge = bridgeLauncher.launch(narratingApprovalHandler, timeout.plusSeconds(5));
            } catch (IOException exception) {
                if (outputConsumer != null) {
                    outputConsumer.accept("Interactive approval is unavailable (" + exception.getMessage()
                            + "); "
                            + (AgentApprovalCapability.forClient(string(arguments, "client", "CODEX"))
                            .isAutoApproveGranted(allowSourceMutation(arguments))
                            ? "file edits stay auto-approved, but other tool calls will be denied."
                            : "running in propose-only plan mode."));
                }
            }
        }
        try {
            List<String> command = commandFor(arguments, bridge);
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
                // timeout below fires, no matter what the prompt says. The approval bridge above is a
                // separate side channel (its own HTTP port) that the CLI talks to independently of this
                // stdin/stdout pipe, so it needs none of this.
                stdinStream.close();

                if (cancellationRequested.get()) {
                    throw new CancellationException("Operation cancelled");
                }
                if (!awaitProcessWithApprovalExtension(process, timeout, bridge)) {
                    process.destroyForcibly();
                    return ShaftMcpToolResult.failure(agentOutput(false, stdoutNow(stdout), stderrNow(stderr),
                            "Timed out after " + timeout.toSeconds() + " seconds.", verbose));
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
                String output;
                if (success) {
                    output = streamParser != null && streamParser.hasTerminalEvent()
                            ? streamParser.finalOutput()
                            : agentOutput(true, rawStdout, rawStderr, "", verbose);
                } else if (streamParser != null) {
                    // A structured (stream-json / --json) CLI writes machine NDJSON to stdout, so on a
                    // failed run the raw stream must never reach the transcript verbatim -- rerun/codegen
                    // failures used to dump lines like {"type":"system","subtype":"thinking_tokens",...}.
                    // Render the parser's cleaned answer, or a plain-language reason plus stderr when the
                    // CLI died before emitting a terminal event -- gated on live Verbose state (issue
                    // #3965) exactly like agentOutput below, since that stderr is already surfaced live
                    // through outputConsumer while the run is in progress.
                    output = streamParser.failureOutput(process.exitValue(), rawStderr, verbose);
                } else {
                    output = agentOutput(false, rawStdout, rawStderr, "", verbose);
                }
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
        } finally {
            if (bridge != null) {
                bridge.close();
            }
        }
    }

    /**
     * Waits for the process to finish, extending the nominal {@code timeout} by however long the
     * user has spent deliberating on interactive approval prompts (per {@code bridge}), up to
     * {@link #MAX_APPROVAL_EXTENSION_SECONDS} -- so a user genuinely reading a diff at the approval
     * gate doesn't lose the whole run to the same deadline that governs unattended abandonment. When
     * no bridge is in play (no interactive approval for this run), behavior is unchanged: a single
     * plain wait on the nominal timeout.
     */
    private static boolean awaitProcessWithApprovalExtension(
            Process process, Duration timeout, LocalAgentApprovalBridge bridge) throws InterruptedException {
        if (bridge == null) {
            return process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS);
        }
        return awaitProcessWithDeadlineExtension(
                process,
                timeout,
                TimeUnit.SECONDS.toMillis(MAX_APPROVAL_EXTENSION_SECONDS),
                bridge::accumulatedPendingMillis);
    }

    /**
     * Polls {@code process} in short slices (instead of one long blocking wait) so the effective
     * deadline -- {@code timeout + min(accumulatedPendingMillis, maxExtensionMillis)} -- can be
     * recomputed on the fly as {@code accumulatedPendingMillisSupplier} grows during a pending
     * approval. Package-private (not {@code private}) so tests can drive it directly with a fake
     * supplier and a tiny cap, without waiting on real approval-bridge wall-clock time or the real
     * 10-minute cap.
     */
    static boolean awaitProcessWithDeadlineExtension(
            Process process, Duration timeout, long maxExtensionMillis, LongSupplier accumulatedPendingMillisSupplier)
            throws InterruptedException {
        long startNanos = System.nanoTime();
        long baseMillis = timeout.toMillis();
        while (true) {
            long extensionMillis = Math.min(accumulatedPendingMillisSupplier.getAsLong(), maxExtensionMillis);
            long effectiveDeadlineMillis = baseMillis + extensionMillis;
            long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
            long remainingMillis = effectiveDeadlineMillis - elapsedMillis;
            if (remainingMillis <= 0) {
                return false;
            }
            long sliceMillis = Math.min(remainingMillis, DEADLINE_POLL_INTERVAL_MILLIS);
            if (process.waitFor(sliceMillis, TimeUnit.MILLISECONDS)) {
                return true;
            }
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
     * human-readable progress lines translated from NDJSON (with unmapped events passed through raw
     * — Verbose mode must never hide information), while buffered-output <em>default</em> commands
     * (Copilot, whose CLI has no known streaming flag) receive a one-time notice followed by their
     * raw lines as-is. Custom commands are never wrapped — SHAFT cannot know a hand-typed command's
     * output shape, so its raw lines are forwarded unchanged exactly as before. A {@code null}
     * caller consumer is left as {@code null} in every case since there is nothing to forward lines
     * to.
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
            outputConsumer.accept(line);
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

        /**
         * Sentinel returned by the describe methods for events that ARE fully consumed elsewhere
         * (terminal result/usage events that become the final answer) and therefore must be neither
         * described nor passed through raw. Distinct from {@code null}, which means "no
         * human-readable mapping exists" and triggers the raw as-is passthrough in
         * {@link #accept}.
         */
        private static final String CONSUMED = "";

        private final Format format;
        private final Map<String, String> toolNamesByUseId = new HashMap<>();
        private final LinkedHashSet<String> filesTouched = new LinkedHashSet<>();
        private final Map<String, Integer> permissionDenialsByTool = new LinkedHashMap<>();
        private String answer;
        private Integer inputTokens;
        private Integer outputTokens;
        // A short, human-readable reason captured from a non-success terminal event (Claude's error
        // subtype, Codex's turn.failed detail), used to explain a failure whose answer text is empty.
        private String terminalDetail;
        // The plan text from a Claude Code ExitPlanMode tool_use call (see the "tool_use" branch of
        // describeClaudeEvent), if any. Carried through composeOutput's trailing metadata line so
        // ShaftAssistantPanel can recover it via parsePlanProposal and render the terminal "Plan
        // proposed" card (issue #3680), without changing the ShaftMcpToolResult contract.
        private String planProposal;
        // The structured question/options (issue #3719) detected in the terminal answer text via
        // AssistantQuestion.detectStructuredLine, if any -- detected and stripped out of `answer`
        // the moment it is captured below (before activitySummary/usageHolder append anything
        // after it), then carried through composeOutput's trailing metadata line so
        // AssistantLocalAgentRunner.parseQuestion can recover it, mirroring planProposal above.
        private AssistantQuestion structuredQuestion;

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
                // still useful progress information, so it passes through unchanged (content-wise) --
                // tagged with RAW_STDOUT_MARKER (issue #3918) so ShaftAssistantPanel can withhold it
                // from non-verbose mode's compact milestone bubbles without guessing from content.
                outputConsumer.accept(RAW_STDOUT_MARKER + line);
                return;
            }
            String humanReadableLine = format == Format.CLAUDE ? describeClaudeEvent(event) : describeCodexEvent(event);
            if (humanReadableLine == null) {
                // No human-readable mapping for this event: share it as-is (raw JSON) so Verbose
                // mode never hides information the CLI sent. Non-verbose runs only ever show the
                // parsed final answer, so this raw line is invisible unless the user opted in.
                outputConsumer.accept(line);
            } else if (!CONSUMED.equals(humanReadableLine)) {
                outputConsumer.accept(humanReadableLine);
            }
        }

        synchronized boolean hasTerminalEvent() {
            return answer != null;
        }

        synchronized String finalOutput() {
            return composeOutput(answer.strip());
        }

        /**
         * Cleaned output for a failed structured run. Prefers the terminal answer text; when the CLI
         * died before returning one, synthesizes a plain-language reason (from {@link #terminalDetail}
         * or the exit code) and, when {@code verbose} is {@code true}, appends stderr as a fenced block.
         * Never returns the raw NDJSON stdout, so native stream events such as
         * {@code {"type":"system","subtype":"thinking_tokens",...}} can no longer leak verbatim into
         * the transcript. Issue #3965: a non-verbose run withholds the stderr fence from this compact
         * answer -- it never needed to duplicate it here, since the same raw stderr already reached the
         * live {@code outputConsumer} stream (Verbose-gated there, at the panel) while the process ran.
         */
        synchronized String failureOutput(int exitCode, String stderr, boolean verbose) {
            String core = answer == null ? "" : answer.strip();
            if (core.isBlank()) {
                core = terminalDetail != null && !terminalDetail.isBlank()
                        ? "The local assistant stopped before finishing: " + terminalDetail + "."
                        : "The local assistant exited with code " + exitCode + " before returning an answer.";
            }
            String stderrText = stderr == null ? "" : stderr.strip();
            if (verbose && !stderrText.isBlank()) {
                core = core + "\n\n```\n" + stderrText + "\n```";
            }
            return composeOutput(core);
        }

        private String composeOutput(String core) {
            StringBuilder output = new StringBuilder(core);
            String activity = activitySummary();
            if (!activity.isBlank()) {
                output.append("\n\n").append(activity);
            }
            return output + "\n\n" + usageHolder();
        }

        /**
         * Detects the structured question protocol (issue #3719) in a just-captured terminal
         * answer, via {@link AssistantQuestion#detectStructuredLine}, stashing it in {@link
         * #structuredQuestion} for {@link #usageHolder()} and returning the answer text with the
         * marker line removed. Detecting here -- at the moment the terminal event's raw text is
         * captured, before {@link #activitySummary()} or the usage/plan metadata line are appended
         * after it -- is what keeps this reliable: those later append their own trailing lines,
         * which would otherwise bury a model-authored trailing marker mid-text by the time any
         * later, UI-layer pass over the fully composed output could look for it. When no valid
         * structured line is found, {@code answer} is returned unchanged and {@link
         * #structuredQuestion} stays {@code null}, so the fence remains the fallback exactly as
         * before this issue.
         */
        private String captureStructuredQuestion(String answer) {
            AssistantQuestion detected = AssistantQuestion.detectStructuredLine(answer);
            if (detected == null) {
                return answer;
            }
            structuredQuestion = detected;
            return detected.promptMarkdown();
        }

        private JsonObject usageHolder() {
            JsonObject usage = new JsonObject();
            usage.addProperty("input_tokens", orZero(inputTokens));
            usage.addProperty("output_tokens", orZero(outputTokens));
            JsonObject usageHolder = new JsonObject();
            usageHolder.add("usage", usage);
            if (planProposal != null && !planProposal.isBlank()) {
                usageHolder.addProperty("plan", planProposal);
            }
            if (structuredQuestion != null) {
                JsonObject question = new JsonObject();
                question.addProperty("text", structuredQuestion.promptMarkdown());
                JsonArray options = new JsonArray();
                for (String option : structuredQuestion.options()) {
                    options.add(option);
                }
                question.add("options", options);
                usageHolder.add("question", question);
            }
            return usageHolder;
        }

        /**
         * Compact factual footer for the final transcript bubble: which files the CLI actually
         * created or edited, and which tool calls were denied. This renders even with Verbose off,
         * so a run that silently lost most of its tool calls to permission denials can never end as
         * an unexplained bare confirmation with no visible trace of what happened.
         */
        private String activitySummary() {
            if (filesTouched.isEmpty() && permissionDenialsByTool.isEmpty()) {
                return "";
            }
            List<String> lines = new ArrayList<>();
            lines.add("---");
            lines.add("**Local agent activity**");
            if (filesTouched.isEmpty()) {
                lines.add("- No files were created or edited by this run.");
            } else {
                lines.add("- Files created or edited: "
                        + filesTouched.stream().map(path -> "`" + path + "`")
                        .collect(Collectors.joining(", ")));
            }
            if (!permissionDenialsByTool.isEmpty()) {
                // Codex's --json schema (status "failed" / a non-zero exit_code) cannot distinguish a
                // sandbox/approval denial from an ordinary execution failure the way Claude's
                // permission_denials array can, so Codex failures are merged into this same bucket
                // under an honest "failed or denied" label instead of Claude's more precise "denied"
                // wording -- an explicit, documented tradeoff (issue #3679) rather than a silent no-op.
                // Codex also has no interactive approval prompt to "approve" at (see
                // AgentApprovalCapability#CODEX), so its hint points at the Allow source edits toggle
                // instead of Claude's "approve them when prompted".
                lines.add(format == Format.CODEX
                        ? "- Failed or denied tool calls: "
                        + permissionDenialsByTool.entrySet().stream()
                        .map(entry -> entry.getKey() + (entry.getValue() > 1 ? " ×" + entry.getValue() : ""))
                        .collect(Collectors.joining(", "))
                        + " — re-run with Allow source edits enabled, since Codex has no interactive"
                        + " approval prompt."
                        : "- Denied tool calls: "
                        + permissionDenialsByTool.entrySet().stream()
                        .map(entry -> entry.getKey() + (entry.getValue() > 1 ? " ×" + entry.getValue() : ""))
                        .collect(Collectors.joining(", "))
                        + " — approve them when prompted, or re-run after adjusting permissions.");
            }
            return String.join("\n", lines);
        }

        synchronized boolean touchedFiles() {
            return !filesTouched.isEmpty();
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
                            recordFileMutation(toolName, objectField(block, "input"));
                            String plan = "ExitPlanMode".equals(toolName)
                                    ? stringField(objectField(block, "input"), "plan")
                                    : null;
                            if (plan != null && !plan.isBlank()) {
                                // Claude Code's own built-in "propose this plan, ask to proceed" tool sends
                                // its proposal in a "plan" input key that toolInputSummary does not
                                // recognize (issue #3680): without this branch it would fall through to the
                                // generic bare "Calling tool ExitPlanMode..." line below, hiding the one
                                // piece of information -- the plan itself -- the user actually needs.
                                planProposal = plan;
                                lines.add("**Plan proposed:**\n\n" + plan);
                            } else {
                                String label = toolName == null ? "(unknown)" : toolName;
                                String summary = toolInputSummary(objectField(block, "input"));
                                lines.add(summary == null
                                        ? "Calling tool " + label + "..."
                                        : "Calling tool " + label + " (" + summary + ")...");
                            }
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
            if ("system".equals(type)) {
                return describeClaudeSystemEvent(event);
            }
            if ("result".equals(type)) {
                String resultText = stringField(event, "result");
                answer = resultText == null ? "" : resultText;
                answer = captureStructuredQuestion(answer);
                JsonObject usage = objectField(event, "usage");
                inputTokens = intField(usage, "input_tokens");
                outputTokens = intField(usage, "output_tokens");
                recordPermissionDenials(event.get("permission_denials"));
                String subtype = stringField(event, "subtype");
                if (booleanField(event, "is_error") || (subtype != null && subtype.startsWith("error"))) {
                    terminalDetail = humanizeSubtype(subtype);
                }
                // Fully consumed: this event becomes the final answer, so it is mapped, not hidden.
                return CONSUMED;
            }
            return null;
        }

        /**
         * Describes a Claude {@code system} event (session lifecycle/control-plane notices, distinct
         * from the assistant/user/result conversation events above). Official subtypes per the
         * documented stream-json schema: {@code init}, {@code api_retry}, {@code plugin_install},
         * {@code hook_started}/{@code hook_progress}/{@code hook_response}, and {@code
         * compact_boundary}. Only the two most user-visible subtypes get a compact rendering here
         * (issue #3922); the rest fall through to {@code null} -- raw-JSON passthrough in Verbose mode,
         * same as any other unrecognized event, so nothing is silently hidden even though it isn't
         * translated yet.
         */
        private static String describeClaudeSystemEvent(JsonObject event) {
            String subtype = stringField(event, "subtype");
            if ("init".equals(subtype)) {
                List<String> parts = new ArrayList<>();
                String model = stringField(event, "model");
                String sessionId = stringField(event, "session_id");
                if (model != null && !model.isBlank()) {
                    parts.add("model " + model);
                }
                if (sessionId != null && !sessionId.isBlank()) {
                    parts.add("session " + sessionId);
                }
                return parts.isEmpty() ? "Session started." : "Session started (" + String.join(", ", parts) + ").";
            }
            if ("compact_boundary".equals(subtype)) {
                return "Conversation history was compacted to save context.";
            }
            return null;
        }

        /**
         * Remembers the target path of a file-mutating built-in tool call (Claude's Write/Edit
         * family) so the final bubble can list what was actually created or edited.
         */
        private void recordFileMutation(String toolName, JsonObject input) {
            if (toolName == null || input == null) {
                return;
            }
            if (!"Write".equals(toolName) && !"Edit".equals(toolName)
                    && !"MultiEdit".equals(toolName) && !"NotebookEdit".equals(toolName)) {
                return;
            }
            String path = firstNonBlank(stringField(input, "file_path"), stringField(input, "path"));
            if (path != null) {
                filesTouched.add(path);
            }
        }

        /**
         * Aggregates the terminal event's {@code permission_denials} array (Claude stream-json)
         * into per-tool counts for the activity footer.
         */
        private void recordPermissionDenials(JsonElement denials) {
            if (denials == null || !denials.isJsonArray()) {
                return;
            }
            for (JsonElement element : denials.getAsJsonArray()) {
                if (!element.isJsonObject()) {
                    continue;
                }
                String toolName = stringField(element.getAsJsonObject(), "tool_name");
                if (toolName != null && !toolName.isBlank()) {
                    permissionDenialsByTool.merge(toolName, 1, Integer::sum);
                }
            }
        }

        /**
         * Remembers the file(s) touched by a completed Codex {@code file_change} item (Codex's
         * {@code --json} counterpart to Claude's Write/Edit tool calls) so the final bubble can list
         * what was actually created or edited. Codex's {@code file_change} item reports a real
         * {@code status} ("completed" when the patch was applied, "failed" when it was not), so unlike
         * {@link #recordFileMutation}'s request-time guess, this only credits paths whose patch
         * actually succeeded; a failed patch is instead counted as a failed/denied tool call via
         * {@link #recordCodexToolFailure} so it never claims a file was edited when it was not.
         */
        private void recordCodexFileMutation(JsonObject item) {
            JsonElement changes = item == null ? null : item.get("changes");
            if (changes == null || !changes.isJsonArray()) {
                return;
            }
            if ("failed".equals(stringField(item, "status"))) {
                permissionDenialsByTool.merge("file_change", 1, Integer::sum);
                return;
            }
            for (JsonElement element : changes.getAsJsonArray()) {
                if (!element.isJsonObject()) {
                    continue;
                }
                String path = stringField(element.getAsJsonObject(), "path");
                if (path != null && !path.isBlank()) {
                    filesTouched.add(path);
                }
            }
        }

        /**
         * Aggregates a completed Codex tool item's failure into the same per-tool bucket Claude's
         * {@code permission_denials} array populates, using the completed item's {@code status}
         * field (including {@code "failed"} and {@code "declined"} -- {@code CommandExecutionStatus}
         * per {@code exec_events.rs} -- the latter being Codex's approval/sandbox-rejection outcome,
         * which carries neither a non-zero exit code nor an error object of its own), a non-zero
         * {@code exit_code} (shell commands), or a present {@code error} object (MCP tool calls) as
         * the failure signal. Codex's {@code --json} schema has no field that distinguishes a
         * sandbox/approval denial from an ordinary execution failure (see issue #3679), so both are
         * merged here; {@link #activitySummary()} labels this bucket "Failed or denied tool calls" for
         * Codex runs rather than reusing Claude's more precise "Denied tool calls" wording, so the
         * footer never overclaims what the CLI's own output can actually prove.
         */
        private void recordCodexToolFailure(String toolName, JsonObject item) {
            if (toolName == null || toolName.isBlank() || item == null) {
                return;
            }
            Integer exitCode = intField(item, "exit_code");
            String status = stringField(item, "status");
            boolean failed = "failed".equals(status)
                    || "declined".equals(status)
                    || (exitCode != null && exitCode != 0)
                    || objectField(item, "error") != null;
            if (failed) {
                permissionDenialsByTool.merge(toolName, 1, Integer::sum);
            }
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
                String summary = text == null || text.isBlank() ? "(no output)" : text.strip();
                lines.add((booleanField(block, "is_error") ? "Tool failed (" : "Tool result (") + label + "): " + summary);
            }
            return lines.isEmpty() ? null : String.join("\n", lines);
        }

        private String describeCodexEvent(JsonObject event) {
            String type = stringField(event, "type");
            if ("item.completed".equals(type) || "item.updated".equals(type)) {
                return describeCodexItemEvent(type, event);
            }
            if ("thread.started".equals(type)) {
                // ThreadStartedEvent per exec_events.rs carries only { thread_id }; a compact fixed
                // notice is enough to mark the session boundary without overclaiming detail.
                return "Codex session started.";
            }
            if ("turn.started".equals(type)) {
                // TurnStartedEvent per exec_events.rs is field-free ({}).
                return "Codex turn started.";
            }
            if ("turn.completed".equals(type) || "turn.failed".equals(type)) {
                JsonObject usage = objectField(event, "usage");
                inputTokens = firstNonNull(intField(usage, "input_tokens"), inputTokens);
                outputTokens = firstNonNull(intField(usage, "output_tokens"), outputTokens);
                if ("turn.completed".equals(type)) {
                    String lastAgentMessage = stringField(event, "last_agent_message");
                    answer = lastAgentMessage != null ? lastAgentMessage : (answer == null ? "" : answer);
                    answer = captureStructuredQuestion(answer);
                } else {
                    JsonObject error = objectField(event, "error");
                    terminalDetail = firstNonBlank(stringField(event, "error"),
                            stringField(error, "message"), stringField(error, "type"));
                }
                // Fully consumed: this event becomes the final answer/usage, so it is mapped, not hidden.
                return CONSUMED;
            }
            if ("error".equals(type)) {
                // ThreadEvent::Error per exec_events.rs -- a FATAL top-level error, distinct from
                // turn.failed's nested error above. Feeds terminalDetail so a run that dies here gets a
                // real reason in failureOutput() instead of the generic "exited with code N" fallback.
                String message = stringField(event, "message");
                if (message != null && !message.isBlank()) {
                    terminalDetail = message;
                    return "Error: " + message;
                }
                return null;
            }
            return null;
        }

        /**
         * Describes an {@code item.completed}/{@code item.updated} event's {@code item.type}. Real
         * Codex {@code --json} item variants per {@code codex-rs/exec/src/exec_events.rs}'s {@code
         * ThreadItemDetails} enum: {@code agent_message}, {@code reasoning}, {@code command_execution},
         * {@code file_change}, {@code mcp_tool_call}, {@code collab_tool_call}, {@code web_search},
         * {@code todo_list}, {@code error}. Note there is no {@code "tool_call"} variant -- an earlier
         * revision of this switch matched it under a stale assumption, but it never appears in live
         * Codex output (issue #3922); only the three real tool-call-shaped variants are handled below.
         */
        private String describeCodexItemEvent(String type, JsonObject event) {
            JsonObject item = objectField(event, "item");
            String itemType = stringField(item, "type");
            if ("reasoning".equals(itemType)) {
                String reasoning = firstNonBlank(stringField(item, "text"), stringField(item, "summary"));
                return reasoning == null ? null : "Reasoning: " + reasoning;
            }
            if ("command_execution".equals(itemType) || "mcp_tool_call".equals(itemType)
                    || "collab_tool_call".equals(itemType)) {
                String toolName = firstNonBlank(stringField(item, "name"), stringField(item, "tool"), stringField(item, "command"));
                String label = toolName == null ? "(unknown)" : toolName;
                String summary = toolInputSummary(item);
                List<String> lines = new ArrayList<>();
                lines.add(summary == null || summary.equals(label)
                        ? "Calling tool " + label + "..."
                        : "Calling tool " + label + " (" + summary + ")...");
                if ("item.completed".equals(type)) {
                    recordCodexToolFailure(label, item);
                    String output = firstNonBlank(
                            stringField(item, "aggregated_output"), stringField(item, "output"), stringField(item, "result"));
                    if (output != null && !output.isBlank()) {
                        Integer exitCode = intField(item, "exit_code");
                        String outputSummary = output.strip();
                        lines.add(exitCode != null && exitCode != 0
                                ? "Tool failed (" + label + ", exit " + exitCode + "): " + outputSummary
                                : "Tool result (" + label + "): " + outputSummary);
                    }
                }
                return String.join("\n", lines);
            }
            if ("file_change".equals(itemType)) {
                if ("item.completed".equals(type)) {
                    recordCodexFileMutation(item);
                }
                return null;
            }
            if ("agent_message".equals(itemType)) {
                String text = stringField(item, "text");
                return text != null && !text.isBlank() ? text : null;
            }
            if ("web_search".equals(itemType)) {
                String query = stringField(item, "query");
                return query != null && !query.isBlank() ? "Web search: " + query : null;
            }
            if ("todo_list".equals(itemType)) {
                return describeCodexTodoList(item);
            }
            if ("error".equals(itemType)) {
                // ErrorItem per exec_events.rs -- item-level, non-fatal (e.g. "command output
                // truncated"), distinct from the top-level ThreadEvent::Error handled above.
                String message = stringField(item, "message");
                return message != null && !message.isBlank() ? "Error: " + message : null;
            }
            return null;
        }

        /**
         * Renders a completed Codex {@code todo_list} item ({@code items: [{ text, completed }] }) as
         * a Markdown checklist, one line per entry.
         */
        private static String describeCodexTodoList(JsonObject item) {
            JsonElement itemsElement = item == null ? null : item.get("items");
            if (itemsElement == null || !itemsElement.isJsonArray()) {
                return null;
            }
            List<String> lines = new ArrayList<>();
            lines.add("Todo list:");
            for (JsonElement element : itemsElement.getAsJsonArray()) {
                if (!element.isJsonObject()) {
                    continue;
                }
                JsonObject todo = element.getAsJsonObject();
                String text = stringField(todo, "text");
                if (text == null || text.isBlank()) {
                    continue;
                }
                lines.add((booleanField(todo, "completed") ? "- [x] " : "- [ ] ") + text);
            }
            return lines.size() <= 1 ? null : String.join("\n", lines);
        }

        private static String firstNonBlank(String... candidates) {
            for (String candidate : candidates) {
                if (candidate != null && !candidate.isBlank()) {
                    return candidate;
                }
            }
            return null;
        }

        /**
         * Turns a machine error subtype (e.g. {@code error_max_turns}) into a short prose reason
         * (e.g. {@code max turns}) for the failure headline.
         */
        private static String humanizeSubtype(String subtype) {
            if (subtype == null || subtype.isBlank()) {
                return "the run failed";
            }
            String cleaned = subtype.replace("error_", "").replace('_', ' ').trim();
            return cleaned.isBlank() ? "the run failed" : cleaned;
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
         * for, ...), collapsed to a single line (but not otherwise truncated -- the user asked to see
         * full messages). Returns {@code null} when none of the known keys are present rather than
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
                    return value.strip().replaceAll("\\s+", " ");
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
     * grants it. When it does not, an ungranted AGENT run wires up {@code bridge} (when the caller
     * supplied one -- see {@link #needsInteractiveApproval}) so the CLI can ask SHAFT for a real,
     * interactive per-tool decision via {@code --permission-prompt-tool} instead of falling all the
     * way back to {@code plan} (propose-only, no real actions), which remains the fallback when no
     * bridge is available. Validated empirically against the real {@code claude} CLI: no {@code
     * --input-format stream-json} or held-open stdin is needed for this -- the bridge is a separate
     * HTTP side channel the CLI talks to independently of the stdout stream already being read for
     * display, and the CLI's own built-in safety classifier still auto-allows obviously-safe tool
     * calls without ever reaching the bridge, matching what an interactive user would see.
     */
    private static List<String> claudeCommand(
            String mode, boolean allowSourceMutation, String model, LocalAgentApprovalBridge bridge) {
        List<String> command = new ArrayList<>(List.of("claude", "--print"));
        if (!model.isBlank()) {
            command.add("--model");
            command.add(model);
        }
        if ("PLAN".equals(mode) || "AGENT".equals(mode)) {
            boolean granted = "AGENT".equals(mode)
                    && AgentApprovalCapability.CLAUDE_CODE.isAutoApproveGranted(allowSourceMutation);
            if (granted) {
                // acceptEdits only auto-approves file edits; Bash and MCP tool calls are still
                // permission-gated and would be auto-denied in --print mode, so granted runs keep
                // the interactive bridge for everything beyond edits whenever one is available.
                command.add("--permission-mode");
                command.add("acceptEdits");
                if (bridge != null) {
                    command.add("--mcp-config");
                    command.add(bridge.mcpConfigArgument());
                    command.add("--permission-prompt-tool");
                    command.add(bridge.permissionPromptToolName());
                }
                addShaftMcpAllowedTools(command);
            } else if ("AGENT".equals(mode) && bridge != null) {
                command.add("--permission-mode");
                command.add("default");
                command.add("--mcp-config");
                command.add(bridge.mcpConfigArgument());
                command.add("--permission-prompt-tool");
                command.add(bridge.permissionPromptToolName());
                addShaftMcpAllowedTools(command);
            } else {
                command.add("--permission-mode");
                command.add("plan");
            }
        }
        command.add("--output-format");
        command.add("stream-json");
        command.add("--verbose");
        return List.copyOf(command);
    }

    /**
     * Pre-approves every SHAFT MCP tool for Claude Code AGENT runs. SHAFT's own tools are
     * first-party capabilities of the Assistant (Codex runs already pre-approve them via
     * {@code mcp_servers.shaft-mcp.default_tools_approval_mode="approve"}), so they never need a
     * per-call approval prompt; the bridge remains for shell commands and third-party MCP tools.
     * The server-level rule {@code mcp__shaft-mcp} allows every tool on that server — validated
     * empirically against claude v2.1.203: a tools/call on an {@code mcp__shaft-mcp__*} tool
     * executes with zero permission denials and no permission-prompt round trip.
     */
    private static void addShaftMcpAllowedTools(List<String> command) {
        command.add("--allowedTools");
        command.add("mcp__shaft-mcp");
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
        JsonObject parsed = parseTrailingJsonLine(lines[lastLineIndex].strip());
        if (parsed == null || usageFromHolder(parsed) == null) {
            return trimmed;
        }
        StringBuilder remainder = new StringBuilder();
        for (int index = 0; index < lastLineIndex; index++) {
            remainder.append(lines[index]).append('\n');
        }
        return remainder.toString().strip();
    }

    /**
     * Parses {@code lastLine} as a trailing metadata JSON object (the shape {@link
     * #stripTrailingUsageMetadata}, {@link #parsePlanProposal}, and {@link #parseQuestion} all look
     * for), or {@code null} when it isn't brace-delimited or isn't valid/object JSON. Shared so no
     * caller re-implements the same guard-clause parse and to keep each caller's own NPath
     * complexity low.
     */
    private static JsonObject parseTrailingJsonLine(String lastLine) {
        if (lastLine.length() < 2 || !lastLine.startsWith("{") || !lastLine.endsWith("}")) {
            return null;
        }
        try {
            JsonElement element = JsonParser.parseString(lastLine);
            return element.isJsonObject() ? element.getAsJsonObject() : null;
        } catch (JsonParseException exception) {
            return null;
        }
    }

    /**
     * Extracts the {@code ExitPlanMode} plan text embedded in the trailing usage-metadata JSON line
     * (see {@code StructuredStreamParser#usageHolder}), or {@code null} when the run never called
     * {@code ExitPlanMode} (or the output has no trailing metadata line at all). Used by {@code
     * ShaftAssistantPanel} to render the terminal "Plan proposed" structured card (issue #3680)
     * independently of whether Claude's own final answer text happened to restate the plan. Mirrors
     * {@link #stripTrailingUsageMetadata}'s trailing-line detection: both inspect the same last line,
     * so a change to one's line-detection rules must be mirrored in the other.
     */
    static String parsePlanProposal(String output) {
        String trimmed = output == null ? "" : output.strip();
        if (trimmed.isBlank()) {
            return null;
        }
        String[] lines = trimmed.split("\\r?\\n");
        JsonObject parsed = parseTrailingJsonLine(lines[lines.length - 1].strip());
        if (parsed == null) {
            return null;
        }
        JsonElement planElement = parsed.get("plan");
        if (planElement == null || !planElement.isJsonPrimitive() || !planElement.getAsJsonPrimitive().isString()) {
            return null;
        }
        String plan = planElement.getAsString();
        return plan.isBlank() ? null : plan;
    }

    /**
     * Extracts the runner-recognized structured question (issue #3719) from the trailing
     * usage-metadata JSON line's {@code question} field -- the preferred, protocol-level
     * counterpart to {@link AssistantQuestion#detect}'s fence-in-markdown fallback. Returns {@code
     * null} when {@code output} has no trailing metadata line, or the field is absent (the
     * overwhelming majority of runs, which never attempted the protocol), so callers fall back to
     * fence detection exactly as before this issue. Mirrors {@link #parsePlanProposal}'s
     * trailing-line extraction. Enforces the same validation as {@link AssistantQuestion#detect}:
     * blanks are filtered from options, and the final count must be 2-6 (issue #3719).
     */
    static AssistantQuestion parseQuestion(String output) {
        String trimmed = output == null ? "" : output.strip();
        if (trimmed.isBlank()) {
            return null;
        }
        String[] lines = trimmed.split("\\r?\\n");
        JsonObject parsed = parseTrailingJsonLine(lines[lines.length - 1].strip());
        if (parsed == null) {
            return null;
        }

        JsonObject question = questionObject(parsed);
        if (question == null) {
            return null;
        }

        String text = questionText(question);
        if (text == null) {
            return null;
        }

        List<String> options = questionOptions(question);
        if (options == null) {
            return null;
        }

        return new AssistantQuestion(text, options);
    }

    /**
     * Extracts and validates the question object from the parsed JSON. Returns {@code null} if
     * the field is missing or not a JSON object.
     */
    private static JsonObject questionObject(JsonObject parsed) {
        JsonElement questionElement = parsed.get("question");
        if (questionElement == null || !questionElement.isJsonObject()) {
            return null;
        }
        return questionElement.getAsJsonObject();
    }

    /**
     * Extracts the question text from the question object. Returns {@code null} if the text
     * field is missing, not a string, or empty after trimming.
     */
    private static String questionText(JsonObject question) {
        JsonElement textElement = question.get("text");
        if (textElement == null || !textElement.isJsonPrimitive()
                || !textElement.getAsJsonPrimitive().isString()) {
            return null;
        }
        String text = textElement.getAsString().strip();
        if (text.isEmpty()) {
            return null;
        }
        return text;
    }

    /**
     * Extracts and validates the options array from the question object. Returns {@code null} if
     * the options field is missing, not an array, or the final count falls outside MIN/MAX bounds.
     * Reuses AssistantQuestion's validation (issue #3719).
     */
    private static List<String> questionOptions(JsonObject question) {
        JsonElement optionsElement = question.get("options");
        if (optionsElement == null || !optionsElement.isJsonArray()) {
            return null;
        }
        List<String> options = AssistantQuestion.parseOptionsArray(optionsElement.getAsJsonArray());
        if (options.isEmpty()) {
            return null;
        }
        return options;
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

    /**
     * Unconditional overload retained for callers with no live Verbose context of their own (for
     * example {@link #runCompactPreamble}, whose result never reaches the interactive transcript's
     * live stream) -- always folds {@code stderr} in, matching the behavior of every caller before
     * issue #3965. See {@link #agentOutput(boolean, String, String, String, boolean)} for the
     * verbose-gated version {@link #run} uses for the compact terminal answer.
     */
    static String agentOutput(boolean success, String stdout, String stderr, String fallback) {
        return agentOutput(success, stdout, stderr, fallback, true);
    }

    /**
     * Composes the buffered/non-structured fallback answer (used for Copilot's default command, any
     * custom command, and a timed-out run) from the CLI's raw stdout/stderr. Issue #3965: {@code
     * stderr} is folded into the compact terminal answer only when {@code verbose} is {@code true} --
     * a non-verbose run withholds it here, but never loses it entirely, since the same raw stderr
     * already reached the live {@code outputConsumer} stream (Verbose-gated there, at the panel) while
     * the process was running.
     */
    static String agentOutput(boolean success, String stdout, String stderr, String fallback, boolean verbose) {
        if (success && stdout != null && !stdout.isBlank()) {
            return stdout.strip();
        }
        List<String> sections = new ArrayList<>();
        if (stdout != null && !stdout.isBlank()) {
            sections.add(stdout.strip());
        }
        if (verbose && stderr != null && !stderr.isBlank()) {
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
        }, ShaftPluginExecutor.getInstance().executor());
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

    static boolean isCommandAvailable(String command) {
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
