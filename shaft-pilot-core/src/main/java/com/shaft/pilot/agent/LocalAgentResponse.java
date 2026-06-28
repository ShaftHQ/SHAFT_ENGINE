package com.shaft.pilot.agent;

import java.time.Duration;
import java.util.List;
import java.util.Objects;

/**
 * Safe local agent execution response.
 *
 * @param status normalized execution status
 * @param client local agent client
 * @param mode Autobot mode
 * @param command command executed, without environment variables
 * @param exitCode process exit code
 * @param stdout standard output
 * @param stderr standard error
 * @param timedOut whether execution timed out
 * @param duration elapsed duration
 * @param requiresCloudApiKey whether this route requires a SHAFT-managed cloud API key
 * @param warnings safe warnings for the caller
 */
public record LocalAgentResponse(
        LocalAgentStatus status,
        LocalAgentClient client,
        LocalAgentMode mode,
        List<String> command,
        int exitCode,
        String stdout,
        String stderr,
        boolean timedOut,
        Duration duration,
        boolean requiresCloudApiKey,
        List<String> warnings) {
    /**
     * Creates and defensively copies a response.
     */
    public LocalAgentResponse {
        status = Objects.requireNonNull(status, "status");
        client = Objects.requireNonNull(client, "client");
        mode = Objects.requireNonNull(mode, "mode");
        command = command == null ? List.of() : List.copyOf(command);
        stdout = Objects.requireNonNullElse(stdout, "");
        stderr = Objects.requireNonNullElse(stderr, "");
        duration = duration == null ? Duration.ZERO : duration;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
