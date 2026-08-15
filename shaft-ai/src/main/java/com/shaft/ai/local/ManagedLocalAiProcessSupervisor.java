package com.shaft.ai.local;

import java.time.Duration;
import java.time.Instant;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/** Process boundary that prevents a managed native runtime from outliving the SHAFT JVM. */
public final class ManagedLocalAiProcessSupervisor {
    private ManagedLocalAiProcessSupervisor() { }

    /** Internal process entrypoint. */
    public static void main(String[] arguments) throws Exception {
        if (arguments.length < 4) {
            System.exit(2);
        }
        ParentIdentity identity = ParentIdentity.parse(arguments);
        ProcessHandle parent = findParent(identity.parentPid(), identity.parentStartedAt());
        if (parent != null) {
            Invocation invocation = Invocation.parse(arguments);
            new Ownership(parent, invocation.workingDirectory(), invocation.command()).run();
        }
    }

    private static ProcessHandle findParent(long parentPid, Instant parentStartedAt) {
        return ProcessHandle.of(parentPid)
                .filter(ProcessHandle::isAlive)
                .filter(candidate -> candidate.info().startInstant()
                        .map(parentStartedAt::equals)
                        .orElse(false))
                .orElse(null);
    }

    private record ParentIdentity(long parentPid, Instant parentStartedAt) {
        private static ParentIdentity parse(String[] arguments) {
            try {
                return new ParentIdentity(Long.parseLong(arguments[0]), Instant.parse(arguments[1]));
            } catch (NumberFormatException invalidPid) {
                throw new IllegalArgumentException("Managed local AI parent PID is invalid.", invalidPid);
            }
        }
    }

    private record Invocation(Path workingDirectory, List<String> command) {
        private static Invocation parse(String[] arguments) throws java.io.IOException {
            Path workingDirectory = validatedWorkingDirectory(arguments[2], arguments[3]);
            List<String> command = List.copyOf(new ArrayList<>(List.of(arguments).subList(3, arguments.length)));
            return new Invocation(workingDirectory, command);
        }
    }

    private static final class Ownership {
        private final ProcessHandle parent;
        private final Path workingDirectory;
        private final List<String> command;
        private final Object gate = new Object();
        private final AtomicBoolean parentExited = new AtomicBoolean();
        private final AtomicBoolean launchResolved = new AtomicBoolean();
        private final AtomicReference<Process> child = new AtomicReference<>();
        private final Set<ProcessHandle> retainedDescendants =
                java.util.concurrent.ConcurrentHashMap.newKeySet();
        private Thread parentMonitor;
        private Thread controlMonitor;
        private Thread treeMonitor;

        private Ownership(ProcessHandle parent, Path workingDirectory, List<String> command) {
            this.parent = parent;
            this.workingDirectory = workingDirectory;
            this.command = command;
        }

        private void run() throws Exception {
            startMonitors();
            if (!launch()) {
                return;
            }
            awaitChild();
        }

        private void startMonitors() {
            parentMonitor = Thread.ofVirtual().name("shaft-local-ai-parent-monitor").start(() -> {
                parent.onExit().join();
                ownerLost();
            });
            controlMonitor = Thread.ofVirtual().name("shaft-local-ai-control-monitor").start(() -> {
                awaitControlPipeClosure();
                ownerLost();
            });
            treeMonitor = Thread.ofVirtual().name("shaft-local-ai-tree-monitor").start(this::monitorTree);
        }

        private boolean launch() throws java.io.IOException {
            synchronized (gate) {
                if (parentExited.get() || !parent.isAlive()) {
                    launchResolved.set(true);
                    return false;
                }
                ProcessBuilder builder = new ProcessBuilder(command).directory(workingDirectory.toFile())
                        .redirectErrorStream(true);
                builder.environment().remove("CLASSPATH");
                builder.inheritIO();
                try {
                    child.set(builder.start());
                    return true;
                } finally {
                    launchResolved.set(true);
                }
            }
        }

        private void awaitChild() throws InterruptedException {
            try {
                child.get().waitFor();
            } finally {
                parentMonitor.interrupt();
                controlMonitor.interrupt();
                synchronized (gate) {
                    terminate(child.get(), Duration.ofSeconds(2), retainedDescendants);
                }
                treeMonitor.interrupt();
            }
        }

        private void awaitControlPipeClosure() {
            try {
                while (System.in.read() != -1) {
                    // The private control pipe carries no data; EOF revokes launch ownership.
                }
            } catch (java.io.IOException ignored) {
                // A broken control pipe has the same meaning as EOF.
            }
        }

        private void monitorTree() {
            while (true) {
                Process current = child.get();
                if (current != null) {
                    captureDescendants(current, retainedDescendants);
                    if (!current.isAlive()) {
                        return;
                    }
                } else if (launchResolved.get()) {
                    return;
                }
                if (!pauseTreeMonitor()) {
                    return;
                }
            }
        }

        private boolean pauseTreeMonitor() {
            try {
                Thread.sleep(1);
                return true;
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                return false;
            }
        }

        private void ownerLost() {
            terminateOnOwnerLoss(gate, parentExited, child, retainedDescendants);
        }
    }

    private static Path validatedWorkingDirectory(String requestedDirectory, String requestedExecutable)
            throws java.io.IOException {
        Path directory = Path.of(requestedDirectory);
        Path executable = Path.of(requestedExecutable);
        if (!directory.isAbsolute() || !executable.isAbsolute()) {
            throw new IllegalArgumentException("Managed local AI child paths must be absolute.");
        }
        directory = directory.normalize();
        executable = executable.normalize();
        if (!directory.equals(executable.getParent())) {
            throw new IllegalArgumentException("Managed local AI child working directory is not executable-owned.");
        }
        BasicFileAttributes directoryAttributes = Files.readAttributes(directory,
                BasicFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
        BasicFileAttributes executableAttributes = Files.readAttributes(executable,
                BasicFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
        if (!directoryAttributes.isDirectory() || directoryAttributes.isSymbolicLink()
                || directoryAttributes.isOther() || !executableAttributes.isRegularFile()
                || executableAttributes.isSymbolicLink() || executableAttributes.isOther()) {
            throw new IllegalArgumentException("Managed local AI child path is not ordinary.");
        }
        return directory;
    }

    private static void terminateOnOwnerLoss(Object gate, AtomicBoolean parentExited,
                                             AtomicReference<Process> child,
                                             Set<ProcessHandle> retainedDescendants) {
        synchronized (gate) {
            parentExited.set(true);
            terminate(child.get(), Duration.ofSeconds(2), retainedDescendants);
        }
    }

    private static void terminate(Process process, Duration timeout, Set<ProcessHandle> descendants) {
        if (process == null) {
            return;
        }
        long deadline = System.nanoTime() + timeout.toNanos();
        try {
            while (process.isAlive() && System.nanoTime() < deadline) {
                captureDescendants(process, descendants);
                descendants.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroy);
                process.destroy();
                Thread.sleep(1);
            }
            captureDescendants(process, descendants);
            descendants.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
            if (process.isAlive()) {
                process.destroyForcibly();
                process.waitFor(Math.max(0, deadline - System.nanoTime()), TimeUnit.NANOSECONDS);
            }
            while (descendants.stream().anyMatch(ProcessHandle::isAlive) && System.nanoTime() < deadline) {
                Thread.sleep(10);
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            process.toHandle().descendants().filter(ProcessHandle::isAlive).forEach(descendants::add);
            descendants.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
            if (process.isAlive()) {
                process.destroyForcibly();
            }
        }
        while (descendants.stream().anyMatch(ProcessHandle::isAlive)) {
            descendants.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
            try {
                Thread.sleep(100);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                return;
            }
        }
    }

    private static void captureDescendants(Process process, Set<ProcessHandle> descendants) {
        try {
            process.toHandle().descendants().filter(ProcessHandle::isAlive).forEach(descendants::add);
        } catch (RuntimeException ignored) {
            // Retained handles from earlier samples remain authoritative.
        }
    }
}
