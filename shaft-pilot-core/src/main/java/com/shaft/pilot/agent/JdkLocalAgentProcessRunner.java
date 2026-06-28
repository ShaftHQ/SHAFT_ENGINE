package com.shaft.pilot.agent;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

final class JdkLocalAgentProcessRunner implements LocalAgentProcessRunner {
    @Override
    public LocalAgentProcessResult run(
            List<String> command,
            Path workingDirectory,
            Map<String, String> environment,
            String stdin,
            Duration timeout) {
        Instant started = Instant.now();
        try {
            ProcessBuilder builder = new ProcessBuilder(command);
            builder.directory(workingDirectory.toFile());
            builder.environment().putAll(environment);
            Process process = builder.start();
            CompletableFuture<String> stdout = readAsync(process.getInputStream());
            CompletableFuture<String> stderr = readAsync(process.getErrorStream());
            process.getOutputStream().write(stdin.getBytes(StandardCharsets.UTF_8));
            process.getOutputStream().close();

            boolean finished = process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                return new LocalAgentProcessResult(-1, stdoutNow(stdout), stdoutNow(stderr), true,
                        Duration.between(started, Instant.now()));
            }
            return new LocalAgentProcessResult(process.exitValue(), stdoutNow(stdout), stdoutNow(stderr), false,
                    Duration.between(started, Instant.now()));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return failed("Interrupted while running local agent command.", started);
        } catch (IOException | RuntimeException e) {
            return failed(e.getMessage(), started);
        }
    }

    private static CompletableFuture<String> readAsync(java.io.InputStream stream) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            } catch (IOException e) {
                return "";
            }
        });
    }

    private static String stdoutNow(CompletableFuture<String> future) {
        try {
            return future.get(2, TimeUnit.SECONDS);
        } catch (Exception e) {
            return "";
        }
    }

    private static LocalAgentProcessResult failed(String reason, Instant started) {
        return new LocalAgentProcessResult(-1, "", Objects.requireNonNullElse(reason, "Local agent command failed."),
                false, Duration.between(started, Instant.now()));
    }
}
