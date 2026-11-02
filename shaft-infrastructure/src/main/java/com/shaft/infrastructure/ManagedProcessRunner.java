package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

/** Bounded, argument-list child-process boundary shared by managed setup consumers. */
public final class ManagedProcessRunner {
    private ManagedProcessRunner() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Runs a child without a shell, bounds output/time, and reaps its owned process tree.
     *
     * @param command exact executable and argument list
     * @param workingDirectory existing working directory for the child
     * @param timeout total execution and output-drain timeout
     * @return immutable exit code and bounded combined output
     * @throws IOException when the child cannot start, exceeds a bound, or cannot be reaped
     */
    public static Result run(List<String> command, Path workingDirectory, Duration timeout) throws IOException {
        Path directory = java.util.Objects.requireNonNull(workingDirectory, "workingDirectory")
                .toAbsolutePath().normalize();
        if (!java.nio.file.Files.isDirectory(directory)) {
            throw new IOException("Managed process working directory does not exist: " + directory);
        }
        ReportingSetupService.ProcessResult result = ReportingSetupService.runProcess(
                List.copyOf(command), null, timeout, directory, directory, directory);
        return new Result(result.exitCode(), result.output());
    }

    /**
     * Immutable exit code and bounded combined output.
     *
     * @param exitCode child exit code
     * @param output bounded combined standard output and error
     */
    public record Result(int exitCode, String output) { }
}
