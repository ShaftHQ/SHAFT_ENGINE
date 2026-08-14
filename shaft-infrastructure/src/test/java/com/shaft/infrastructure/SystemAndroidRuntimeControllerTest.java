package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SystemAndroidRuntimeControllerTest {
    @Test
    @EnabledOnOs(OS.LINUX)
    void execTransitionUsesPostLaunchCommandIdentityForLeaseReuse(@TempDir Path temp) throws Exception {
        SystemAndroidRuntimeController controller = new SystemAndroidRuntimeController();
        AndroidOwnedProcess process = controller.start("exec-child", List.of("/bin/sh", "-c", "exec sleep 30"),
                temp, Map.of(), Set.of(), temp.resolve("exec-child.log"));
        try {
            awaitCommandChange(process.pid(), "/bin/sh", Duration.ofSeconds(5));

            assertTrue(controller.find(process.pid(), process.startInstant(), process.commandIdentity()).isPresent());
        } finally {
            process.stop(Duration.ofSeconds(5));
        }
    }

    @Test
    void mismatchedStartInstantOrCommandIsNeverAdoptedOrKilled(@TempDir Path temp) throws Exception {
        Path java = javaExecutable();
        Path log = temp.resolve("child.log");
        SystemAndroidRuntimeController controller = new SystemAndroidRuntimeController();
        AndroidOwnedProcess process = controller.start("child", List.of(java.toString(), "-Xmx32m",
                        "-XX:+UseSerialGC", "-cp", System.getProperty("java.class.path"), SleepingChild.class.getName()),
                temp, Map.of(), Set.of(), log);
        try {
            Instant wrongStart = process.startInstant().plusMillis(1);

            assertThrows(java.io.IOException.class,
                    () -> controller.find(process.pid(), wrongStart, process.commandIdentity()));
            assertTrue(process.isAlive());
            assertThrows(java.io.IOException.class,
                    () -> controller.find(process.pid(), process.startInstant(), process.commandIdentity() + ".other"));
            assertTrue(process.isAlive());
        } finally {
            process.stop(Duration.ofSeconds(5));
            awaitDelete(log, Duration.ofSeconds(2));
        }
    }

    @Test
    void stopTerminatesRealDescendantBeforeParent(@TempDir Path temp) throws Exception {
        Path java = javaExecutable();
        Path childPidFile = temp.resolve("child.pid");
        List<String> command = List.of(java.toString(), "-Xmx32m", "-XX:+UseSerialGC", "-cp",
                System.getProperty("java.class.path"), DescendantParent.class.getName(), childPidFile.toString());
        SystemAndroidRuntimeController controller = new SystemAndroidRuntimeController();
        AndroidOwnedProcess parent = controller.start("parent", command, temp, Map.of(), Set.of(),
                temp.resolve("parent.log"));
        long childPid = -1;
        try {
            childPid = awaitPid(childPidFile, Duration.ofSeconds(5));

            parent.stop(Duration.ofSeconds(5));

            assertTrue(ProcessHandle.of(childPid).map(handle -> !handle.isAlive()).orElse(true));
            assertTrue(ProcessHandle.of(parent.pid()).map(handle -> !handle.isAlive()).orElse(true));
        } finally {
            if (childPid > 0) ProcessHandle.of(childPid).filter(ProcessHandle::isAlive)
                    .ifPresent(ProcessHandle::destroyForcibly);
            ProcessHandle.of(parent.pid()).filter(ProcessHandle::isAlive).ifPresent(ProcessHandle::destroyForcibly);
        }
    }

    private static long awaitPid(Path pidFile, Duration timeout) throws Exception {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (System.nanoTime() < deadline) {
            if (Files.isRegularFile(pidFile) && Files.size(pidFile) > 0) {
                return Long.parseLong(Files.readString(pidFile));
            }
            Thread.sleep(Duration.ofMillis(10));
        }
        throw new AssertionError("Timed out waiting for descendant PID.");
    }

    private static void awaitDelete(Path file, Duration timeout) throws Exception {
        long deadline = System.nanoTime() + timeout.toNanos();
        Exception last = null;
        while (System.nanoTime() < deadline) {
            try {
                Files.deleteIfExists(file);
                return;
            } catch (java.io.IOException locked) {
                last = locked;
                Thread.sleep(Duration.ofMillis(10));
            }
        }
        throw new java.io.IOException("Timed out waiting for child process log handle to close: " + file, last);
    }

    private static void awaitCommandChange(long pid, String launchCommand, Duration timeout) throws Exception {
        String normalizedLaunch = Path.of(launchCommand).toAbsolutePath().normalize().toString();
        long deadline = System.nanoTime() + timeout.toNanos();
        while (System.nanoTime() < deadline) {
            String command = ProcessHandle.of(pid).flatMap(handle -> handle.info().command())
                    .map(value -> Path.of(value).toAbsolutePath().normalize().toString()).orElse("");
            if (!command.isBlank() && !command.equals(normalizedLaunch)) return;
            Thread.sleep(Duration.ofMillis(10));
        }
        throw new AssertionError("Timed out waiting for the child process exec transition.");
    }

    private static Path javaExecutable() {
        return Path.of(System.getProperty("java.home"), "bin", SetupPlatform.current() == SetupPlatform.WINDOWS
                ? "java.exe" : "java").toAbsolutePath();
    }

    public static final class DescendantParent {
        public static void main(String[] args) throws Exception {
            Path java = Path.of(System.getProperty("java.home"), "bin", SetupPlatform.current() == SetupPlatform.WINDOWS
                    ? "java.exe" : "java");
            Process child = new ProcessBuilder(java.toString(), "-Xmx32m", "-XX:+UseSerialGC", "-cp",
                    System.getProperty("java.class.path"), SleepingChild.class.getName()).start();
            Files.writeString(Path.of(args[0]), Long.toString(child.pid()));
            Thread.sleep(Duration.ofSeconds(30));
        }
    }

    public static final class SleepingChild {
        public static void main(String[] args) throws Exception {
            Thread.sleep(Duration.ofSeconds(30));
        }
    }
}
