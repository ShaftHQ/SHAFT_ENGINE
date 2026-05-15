package com.shaft.cli;

/**
 * Result of a remote {@code exec} channel: separate stdout/stderr plus exit status
 * ({@code -1} if not yet available from JSch).
 */
public record SshCommandResult(String standardOutput, String standardError, int exitStatus) {

    /**
     * stdout then stderr, matching the merged logging order used by {@link TerminalActions} for remote runs.
     */
    public String mergedOutput() {
        if (standardOutput == null || standardOutput.isEmpty()) {
            return standardError == null ? "" : standardError;
        }
        if (standardError == null || standardError.isEmpty()) {
            return standardOutput;
        }
        return standardOutput + System.lineSeparator() + standardError;
    }
}
