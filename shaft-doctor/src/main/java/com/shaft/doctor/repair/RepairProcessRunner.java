package com.shaft.doctor.repair;

import com.shaft.doctor.internal.DoctorRedactor;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.TimeUnit;

class RepairProcessRunner {
    private static final int MAX_OUTPUT_CHARS = 24_000;

    ProcessResult run(List<String> command, Path directory, Duration timeout) {
        Path output = null;
        try {
            output = Files.createTempFile("shaft-doctor-command-", ".log");
            Process process = new ProcessBuilder(command)
                    .directory(directory.toAbsolutePath().normalize().toFile())
                    .redirectErrorStream(true)
                    .redirectOutput(output.toFile())
                    .start();
            boolean finished = process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                process.waitFor(5, TimeUnit.SECONDS);
            }
            String diagnostics = Files.readString(output, StandardCharsets.UTF_8);
            diagnostics = new DoctorRedactor().redact(diagnostics);
            if (diagnostics.length() > MAX_OUTPUT_CHARS) {
                diagnostics = diagnostics.substring(diagnostics.length() - MAX_OUTPUT_CHARS);
            }
            return new ProcessResult(finished ? process.exitValue() : -1, !finished, diagnostics);
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor repair command could not be launched.", exception);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Doctor repair command was interrupted.", exception);
        } finally {
            if (output != null) {
                try {
                    Files.deleteIfExists(output);
                } catch (IOException ignored) {
                    // Best-effort cleanup of bounded process output.
                }
            }
        }
    }

    record ProcessResult(int exitCode, boolean timedOut, String output) {
        boolean successful() {
            return !timedOut && exitCode == 0;
        }
    }
}
