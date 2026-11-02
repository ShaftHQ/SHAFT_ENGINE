package com.shaft.pilot.agent;

import java.time.Duration;
import java.util.Objects;

/**
 * Raw process execution result from a local agent CLI.
 *
 * @param exitCode process exit code, or -1 when the process could not finish normally
 * @param stdout standard output decoded as UTF-8
 * @param stderr standard error decoded as UTF-8
 * @param timedOut whether the process exceeded the requested timeout
 * @param duration elapsed execution duration
 */
public record LocalAgentProcessResult(
        int exitCode,
        String stdout,
        String stderr,
        boolean timedOut,
        Duration duration) {
    /**
     * Creates an immutable process result.
     */
    public LocalAgentProcessResult {
        stdout = Objects.requireNonNullElse(stdout, "");
        stderr = Objects.requireNonNullElse(stderr, "");
        duration = duration == null ? Duration.ZERO : duration;
    }
}
