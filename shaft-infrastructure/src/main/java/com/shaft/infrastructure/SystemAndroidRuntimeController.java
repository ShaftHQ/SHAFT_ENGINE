package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;

final class SystemAndroidRuntimeController implements AndroidRuntimeController {
    @Override
    public AndroidOwnedProcess start(String role, List<String> command, Path workingDirectory,
                                     Map<String, String> environment, Set<String> removedEnvironment,
                                     Path log) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(workingDirectory);
        VerifiedArtifactStore.requireUnlinkedAncestors(log);
        Files.createDirectories(log.getParent());
        ProcessBuilder builder = new ProcessBuilder(command).directory(workingDirectory.toFile())
                .redirectErrorStream(true).redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile()));
        removedEnvironment.forEach(builder.environment()::remove);
        builder.environment().putAll(environment);
        Process process = builder.start();
        return new SystemOwnedProcess(process.toHandle(), identity(process.toHandle(), command.getFirst()));
    }

    @Override
    public Optional<AndroidOwnedProcess> find(long pid, Instant startInstant, String commandIdentity)
            throws IOException {
        Optional<ProcessHandle> found = ProcessHandle.of(pid);
        if (found.isEmpty() || !found.orElseThrow().isAlive()) return Optional.empty();
        ProcessHandle handle = found.orElseThrow();
        Instant actualStart = handle.info().startInstant().orElseThrow(() ->
                new IOException("Owned process has no start-instant identity: " + pid));
        String actualCommand = identity(handle, "");
        if (!actualStart.equals(startInstant) || !actualCommand.equals(commandIdentity)) {
            throw new IOException("Live process identity does not match the SHAFT runtime lease: " + pid);
        }
        return Optional.of(new SystemOwnedProcess(handle, actualCommand));
    }

    private static String identity(ProcessHandle handle, String fallback) {
        return handle.info().command().map(path -> Path.of(path).toAbsolutePath().normalize().toString())
                .orElseGet(() -> Path.of(fallback).toAbsolutePath().normalize().toString());
    }

    private record SystemOwnedProcess(ProcessHandle handle, String commandIdentity) implements AndroidOwnedProcess {
        @Override public long pid() { return handle.pid(); }
        @Override public Instant startInstant() { return handle.info().startInstant().orElse(Instant.EPOCH); }
        @Override public boolean isAlive() { return handle.isAlive(); }

        @Override
        public void stop(Duration timeout) throws IOException {
            Instant deadline = Instant.now().plus(timeout);
            List<ProcessHandle> descendants = handle.descendants()
                    .sorted(Comparator.comparingInt(SystemAndroidRuntimeController::depth).reversed()).toList();
            descendants.forEach(ProcessHandle::destroy);
            handle.destroy();
            for (ProcessHandle process : descendants) awaitOrForce(process, deadline);
            awaitOrForce(handle, deadline);
        }

        private static void awaitOrForce(ProcessHandle process, Instant deadline) throws IOException {
            if (!process.isAlive()) return;
            long millis = Math.max(1, Duration.between(Instant.now(), deadline).toMillis());
            try {
                process.onExit().get(millis, TimeUnit.MILLISECONDS);
            } catch (java.util.concurrent.TimeoutException timeout) {
                process.destroyForcibly();
                millis = Math.max(1, Duration.between(Instant.now(), deadline).toMillis());
                try {
                    process.onExit().get(millis, TimeUnit.MILLISECONDS);
                } catch (java.util.concurrent.TimeoutException stillAlive) {
                    throw new IOException("Owned process tree did not terminate before the shutdown deadline: "
                            + process.pid(), stillAlive);
                } catch (java.util.concurrent.ExecutionException failure) {
                    throw new IOException("Failed while awaiting owned process termination.", failure);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while stopping the owned Android process tree.", interrupted);
                }
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while stopping the owned Android process tree.", interrupted);
            } catch (java.util.concurrent.ExecutionException failure) {
                throw new IOException("Failed while awaiting owned process termination.", failure);
            }
        }
    }

    private static int depth(ProcessHandle handle) {
        int depth = 0;
        for (Optional<ProcessHandle> parent = handle.parent(); parent.isPresent(); parent = parent.get().parent()) {
            depth++;
        }
        return depth;
    }
}
