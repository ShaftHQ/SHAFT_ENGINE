package com.shaft.mcp;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Thin process boundary for MCP toolchain commands.
 */
interface McpProcessRunner {
    ProcessResult run(List<String> command, Path workingDirectory, Map<String, String> environment, Duration timeout);

    default ProcessResult runWithInput(
            List<String> command,
            Path workingDirectory,
            Map<String, String> environment,
            Duration timeout,
            String input) {
        return run(command, workingDirectory, environment, timeout);
    }

    Process start(List<String> command, Path workingDirectory, Map<String, String> environment);

    /**
     * Completed process result.
     */
    record ProcessResult(int exitCode, String stdout, String stderr, boolean timedOut) {
        public ProcessResult {
            stdout = stdout == null ? "" : stdout;
            stderr = stderr == null ? "" : stderr;
        }
    }

    /**
     * Returns the real local process runner.
     */
    static McpProcessRunner system() {
        return new SystemProcessRunner();
    }

    final class SystemProcessRunner implements McpProcessRunner {
        @Override
        public ProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                Duration timeout) {
            Process process = start(command, workingDirectory, environment);
            closeInput(process, "");
            return waitFor(process, timeout);
        }

        @Override
        public ProcessResult runWithInput(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                Duration timeout,
                String input) {
            Process process = start(command, workingDirectory, environment);
            closeInput(process, input);
            return waitFor(process, timeout);
        }

        private ProcessResult waitFor(Process process, Duration timeout) {
            CompletableFuture<String> stdout = read(process.getInputStream());
            CompletableFuture<String> stderr = read(process.getErrorStream());
            try {
                boolean finished = process.waitFor(Math.max(timeout.toMillis(), 1), TimeUnit.MILLISECONDS);
                if (!finished) {
                    process.destroyForcibly();
                    return new ProcessResult(-1, stdout.join(), stderr.join(), true);
                }
                return new ProcessResult(process.exitValue(), stdout.join(), stderr.join(), false);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                process.destroyForcibly();
                return new ProcessResult(-1, stdout.join(), stderr.join(), true);
            }
        }

        private static void closeInput(Process process, String input) {
            try (var output = process.getOutputStream()) {
                if (input != null && !input.isBlank()) {
                    output.write(input.getBytes(StandardCharsets.UTF_8));
                    output.flush();
                }
            } catch (IOException ignored) {
                // Some commands close stdin immediately.
            }
        }

        @Override
        public Process start(List<String> command, Path workingDirectory, Map<String, String> environment) {
            if (command == null || command.isEmpty()) {
                throw new IllegalArgumentException("Process command is required.");
            }
            ProcessBuilder builder = new ProcessBuilder(command);
            if (workingDirectory != null) {
                builder.directory(workingDirectory.toFile());
            }
            if (environment != null) {
                builder.environment().putAll(environment);
            }
            try {
                return builder.start();
            } catch (IOException exception) {
                throw new IllegalStateException("Process could not be started: " + String.join(" ", command),
                        exception);
            }
        }

        private static CompletableFuture<String> read(InputStream stream) {
            return CompletableFuture.supplyAsync(() -> {
                try (stream) {
                    return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
                } catch (IOException exception) {
                    return "";
                }
            });
        }
    }
}
