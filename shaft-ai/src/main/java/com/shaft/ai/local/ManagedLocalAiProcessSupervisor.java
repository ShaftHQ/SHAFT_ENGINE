package com.shaft.ai.local;

import java.time.Duration;
import java.time.Instant;
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
        if (arguments.length < 3) {
            System.exit(2);
        }
        long parentPid = Long.parseLong(arguments[0]);
        Instant parentStartedAt = Instant.parse(arguments[1]);
        ProcessHandle parent = ProcessHandle.of(parentPid)
                .filter(ProcessHandle::isAlive)
                .filter(candidate -> candidate.info().startInstant()
                        .map(parentStartedAt::equals)
                        .orElse(false))
                .orElse(null);
        if (parent == null) {
            return;
        }
        List<String> command = new ArrayList<>(List.of(arguments).subList(2, arguments.length));
        Object gate = new Object();
        AtomicBoolean parentExited = new AtomicBoolean();
        AtomicBoolean launchResolved = new AtomicBoolean();
        AtomicReference<Process> child = new AtomicReference<>();
        Set<ProcessHandle> retainedDescendants = java.util.concurrent.ConcurrentHashMap.newKeySet();
        Thread monitor = Thread.ofVirtual().name("shaft-local-ai-parent-monitor").start(() -> {
            parent.onExit().join();
            terminateOnOwnerLoss(gate, parentExited, child, retainedDescendants);
        });
        Thread control = Thread.ofVirtual().name("shaft-local-ai-control-monitor").start(() -> {
            try {
                while (System.in.read() != -1) {
                    // The private control pipe carries no data; EOF revokes launch ownership.
                }
            } catch (java.io.IOException ignored) {
                // A broken control pipe has the same meaning as EOF.
            }
            terminateOnOwnerLoss(gate, parentExited, child, retainedDescendants);
        });
        Thread treeMonitor = Thread.ofVirtual().name("shaft-local-ai-tree-monitor").start(() -> {
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
                try {
                    Thread.sleep(1);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        });
        synchronized (gate) {
            if (parentExited.get() || !parent.isAlive()) {
                launchResolved.set(true);
                return;
            }
            ProcessBuilder builder = new ProcessBuilder(command).redirectErrorStream(true);
            builder.environment().remove("CLASSPATH");
            builder.inheritIO();
            try {
                child.set(builder.start());
            } finally {
                launchResolved.set(true);
            }
        }
        try {
            child.get().waitFor();
        } finally {
            monitor.interrupt();
            control.interrupt();
            synchronized (gate) {
                terminate(child.get(), Duration.ofSeconds(2), retainedDescendants);
            }
            treeMonitor.interrupt();
        }
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
