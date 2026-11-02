package com.shaft.pilot.agent;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;

/**
 * Executes a local agent command.
 */
public interface LocalAgentProcessRunner {
    /**
     * Runs the command with stdin and scoped environment variables.
     *
     * @param command command and arguments
     * @param workingDirectory working directory for the process
     * @param environment additional environment variables
     * @param stdin prompt written to standard input
     * @param timeout maximum execution time
     * @return process result
     */
    LocalAgentProcessResult run(
            List<String> command,
            Path workingDirectory,
            Map<String, String> environment,
            String stdin,
            Duration timeout);
}
