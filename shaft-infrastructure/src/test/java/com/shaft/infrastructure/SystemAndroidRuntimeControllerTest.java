package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

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
    void mismatchedStartInstantOrCommandIsNeverAdoptedOrKilled(@TempDir Path temp) throws Exception {
        Path java = javaExecutable();
        SystemAndroidRuntimeController controller = new SystemAndroidRuntimeController();
        AndroidOwnedProcess process = controller.start("child", List.of(java.toString(), "-Xmx32m",
                        "-XX:+UseSerialGC", "-cp", System.getProperty("java.class.path"), SleepingChild.class.getName()),
                temp, Map.of(), Set.of(), temp.resolve("child.log"));
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
