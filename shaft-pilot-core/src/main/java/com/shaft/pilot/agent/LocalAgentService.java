package com.shaft.pilot.agent;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.function.Predicate;

/**
 * Routes SHAFT Autobot requests to local agent CLIs.
 */
public final class LocalAgentService {
    /**
     * @deprecated Agent mode can run without source mutation approval; custom Agent commands still require it.
     */
    @Deprecated(since = "2026.6", forRemoval = false)
    public static final String AGENT_MODE_APPROVAL_WARNING =
            "Agent mode can run without source-mutation approval; custom Agent commands still require it.";
    private static final String CUSTOM_AGENT_APPROVAL_WARNING =
            "Custom Agent commands require source-mutation approval because SHAFT cannot enforce read-only execution.";

    private final Predicate<String> commandAvailable;
    private final LocalAgentProcessRunner runner;

    /**
     * Creates a service backed by the local JDK process runner.
     */
    public LocalAgentService() {
        this(LocalAgentService::isCommandAvailable, new JdkLocalAgentProcessRunner());
    }

    /**
     * Creates a service with injectable command discovery and process execution.
     *
     * @param commandAvailable executable availability check
     * @param runner process runner
     */
    public LocalAgentService(Predicate<String> commandAvailable, LocalAgentProcessRunner runner) {
        this.commandAvailable = Objects.requireNonNull(commandAvailable, "commandAvailable");
        this.runner = Objects.requireNonNull(runner, "runner");
    }

    /**
     * Executes a local agent request.
     *
     * @param request request
     * @return safe response
     */
    public LocalAgentResponse execute(LocalAgentRequest request) {
        Objects.requireNonNull(request, "request");
        boolean customCommand = !request.command().isEmpty();
        if (customCommand && request.mode() == LocalAgentMode.AGENT && !request.allowSourceMutation()) {
            return response(request, LocalAgentStatus.REJECTED, request.command(), -1, "", "", false,
                    Duration.ZERO, List.of(CUSTOM_AGENT_APPROVAL_WARNING));
        }
        List<String> command = customCommand ? request.command() : defaultCommand(request);
        if (!customCommand && !commandAvailable.test(command.getFirst())) {
            return response(request, LocalAgentStatus.UNAVAILABLE, command, -1, "", "", false, Duration.ZERO,
                    List.of(request.client().displayName() + " executable is not available on PATH."));
        }

        LocalAgentProcessResult result = runner.run(command, request.workingDirectory(), request.environment(),
                request.prompt(), request.timeout());
        LocalAgentStatus status = status(result);
        return response(request, status, command, result.exitCode(), result.stdout(), result.stderr(), result.timedOut(),
                result.duration(), List.of());
    }

    private static LocalAgentStatus status(LocalAgentProcessResult result) {
        if (result.timedOut()) {
            return LocalAgentStatus.TIMEOUT;
        }
        return result.exitCode() == 0 ? LocalAgentStatus.SUCCESS : LocalAgentStatus.FAILED;
    }

    private static LocalAgentResponse response(
            LocalAgentRequest request,
            LocalAgentStatus status,
            List<String> command,
            int exitCode,
            String stdout,
            String stderr,
            boolean timedOut,
            Duration duration,
            List<String> warnings) {
        return new LocalAgentResponse(status, request.client(), request.mode(), command, exitCode, stdout, stderr,
                timedOut, duration, false, warnings);
    }

    private static List<String> defaultCommand(LocalAgentRequest request) {
        return switch (request.client()) {
            case CODEX -> codexCommand(request.mode(), request.allowSourceMutation());
            case CLAUDE_CODE -> claudeCommand(request.mode(), request.allowSourceMutation());
            case COPILOT_CLI -> copilotCommand(request.mode(), request.allowSourceMutation());
        };
    }

    private static List<String> codexCommand(LocalAgentMode mode, boolean allowSourceMutation) {
        return switch (mode) {
            case ASK, PLAN -> List.of("codex", "exec", "--sandbox", "read-only", "-");
            case AGENT -> List.of(
                    "codex", "exec",
                    "--sandbox", allowSourceMutation ? "workspace-write" : "read-only",
                    "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                    "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                    "-");
        };
    }

    private static List<String> claudeCommand(LocalAgentMode mode, boolean allowSourceMutation) {
        return switch (mode) {
            case ASK -> List.of("claude", "--print");
            case PLAN -> List.of("claude", "--print", "--permission-mode", "plan");
            case AGENT -> List.of("claude", "--print", "--permission-mode", allowSourceMutation ? "acceptEdits" : "plan");
        };
    }

    private static List<String> copilotCommand(LocalAgentMode mode, boolean allowSourceMutation) {
        return switch (mode) {
            case ASK -> List.of("copilot", "ask");
            case PLAN -> List.of("copilot", "plan");
            case AGENT -> List.of("copilot", allowSourceMutation ? "agent" : "ask");
        };
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
        return java.util.Arrays.stream(pathext.split(";"))
                .filter(value -> !value.isBlank())
                .map(value -> value.startsWith(".") ? value : "." + value)
                .toList();
    }
}
