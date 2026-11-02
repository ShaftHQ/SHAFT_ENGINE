package com.shaft.cli;

import java.util.Objects;

/**
 * Structured result of a remote SSH command executed through {@link TerminalActions}.
 *
 * @param command        the exact command text that was executed
 * @param stdout         standard output captured from the remote command
 * @param stderr         standard error captured from the remote command
 * @param exitCode       remote process exit code
 * @param durationMillis elapsed execution time in milliseconds
 */
public record SshCommandResult(
        String command,
        String stdout,
        String stderr,
        int exitCode,
        long durationMillis) {

    /**
     * Creates an immutable SSH command result with non-null stream values.
     */
    public SshCommandResult {
        command = Objects.requireNonNullElse(command, "");
        stdout = Objects.requireNonNullElse(stdout, "");
        stderr = Objects.requireNonNullElse(stderr, "");
    }

    /**
     * @return {@code true} when the remote command exited with code {@code 0}
     */
    public boolean succeeded() {
        return exitCode == 0;
    }

    /**
     * @return stdout and stderr merged in execution order for callers that expect one combined log
     */
    public String combinedOutput() {
        if (stdout.isEmpty()) {
            return stderr;
        }
        if (stderr.isEmpty()) {
            return stdout;
        }
        return stdout + System.lineSeparator() + stderr;
    }
}
