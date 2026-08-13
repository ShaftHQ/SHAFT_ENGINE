package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiProcessTest {
    @TempDir
    Path temp;

    @Test
    void commandIsLoopbackAuthenticatedAndEnvironmentIsAllowlistOnly() throws Exception {
        Path executable = Files.writeString(temp.resolve("llama-server.exe"), "binary");
        Path model = Files.writeString(temp.resolve("model.gguf"), "weights");
        Path keyFile = Files.writeString(temp.resolve("api-key.txt"), "key-123");
        Map<String, String> environment = ManagedLocalAiProcess.runtimeEnvironment(Map.of(
                "Path", "bin", "SystemRoot", "windows", "TEMP", "tmp", "LD_LIBRARY_PATH", "loader",
                "AWS_SECRET_ACCESS_KEY", "secret", "AZURE_CLIENT_SECRET", "secret", "DATABASE_URL", "secret"));
        List<String> command = ManagedLocalAiProcess.command(executable, model, 19191, "alias-123", keyFile, 4);

        assertEquals("bin", environment.get("Path"));
        assertEquals("windows", environment.get("SystemRoot"));
        assertEquals("loader", environment.get("LD_LIBRARY_PATH"));
        assertFalse(environment.containsValue("secret"));
        assertTrue(command.containsAll(List.of("--host", "127.0.0.1", "--port", "19191",
                "--api-key-file", keyFile.toAbsolutePath().toString(), "--alias", "alias-123", "--threads", "4")));
        assertFalse(command.contains("key-123"));
        assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiProcess.command(executable, model, 0, "alias", keyFile, 1));
    }

    @Test
    void launchRetriesPortTheftAndRequiresAuthenticatedAliasIdentity() throws Exception {
        AtomicInteger starts = new AtomicInteger();
        FakeProcess stolen = new FakeProcess(1);
        FakeProcess owned = new FakeProcess(null);
        Path cache = temp.resolve("cache");
        Path runtimeStage = readyStage(cache, "runtime", "server", "binary");
        Path modelStage = readyStage(cache, "model", "model.gguf", "weights");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime", runtimeStage));
        ManagedLocalAiCache.Installation modelInstall = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model", modelStage));
        Path executable = runtime.root().resolve("server");
        Path model = modelInstall.root().resolve("model.gguf");
        Path logPath = cache.resolve("staging/logs/server.log");
        ManagedLocalAiProcess.Session session = ManagedLocalAiProcess.launch(
                cache, executable, model, logPath, "expected-alias",
                2, Duration.ofSeconds(1), () -> starts.get() == 0 ? 18181 : 18182,
                (command, environment, log) -> starts.getAndIncrement() == 0 ? stolen : owned,
                (process, port, key, alias, timeout) -> {
                    if (port == 18181) {
                        throw new IllegalStateException("foreign loopback responder");
                    }
                    assertEquals("expected-alias", alias);
                    assertFalse(key.isBlank());
                });

        assertEquals(2, starts.get());
        assertEquals(18182, session.port());
        assertEquals(owned, session.process());
        assertTrue(stolen.destroyed);
        session.close();
        assertTrue(owned.destroyed);
    }

    @Test
    void identityRejectsPublicHealthSpoofWrongBearerAndWrongAlias() throws Exception {
        FakeProcess process = new FakeProcess(null);
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.requireIdentity(process, 1234,
                "secret", "expected", Duration.ofMillis(50),
                (uri, bearer) -> Map.of("data", List.of(Map.of("id", "foreign")))));
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.requireIdentity(process, 1234,
                "secret", "expected", Duration.ofMillis(50),
                (uri, bearer) -> { throw new SecurityException("wrong bearer"); }));
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.requireIdentity(process, 1234,
                "secret", "expected", Duration.ofMillis(50),
                (uri, bearer) -> Map.of("data", List.of(Map.of("id", "expected"), Map.of("id", "foreign")))));
    }

    @Test
    void terminationIsBoundedKillsDescendantsAndDoesNotMaskPrimaryFailure() throws Exception {
        FakeProcess process = new FakeProcess(null);
        FakeHandle descendant = new FakeHandle();
        process.descendant = descendant;
        RuntimeException primary = new RuntimeException("inference failed");
        ManagedLocalAiProcess.terminate(process, Duration.ZERO, primary);

        assertTrue(process.destroyed);
        assertTrue(process.forciblyDestroyed);
        assertTrue(descendant.destroyed);
        assertEquals(0, primary.getSuppressed().length);
    }

    @Test
    void nativeLogIsBoundedAndRedactsAuthenticationMaterial() throws Exception {
        String secret = "local-secret-value";
        byte[] noisy = ("Authorization: Bearer " + secret + "\n--api-key " + secret + "\n"
                + "x".repeat(2048)).getBytes(java.nio.charset.StandardCharsets.UTF_8);
        Path log = temp.resolve("staging/logs/sanitized.log");
        ManagedLocalAiProcess.writeSanitizedLog(temp, new ByteArrayInputStream(noisy), log, Set.of(secret), 512);
        String value = Files.readString(log);

        assertFalse(value.contains(secret));
        assertFalse(value.toLowerCase(java.util.Locale.ROOT).contains("bearer "));
        assertTrue(Files.size(log) <= 512);
    }

    @Test
    void logLeafLinkIsRejectedWithoutChangingExternalContent() throws Exception {
        Path outside = Files.writeString(temp.resolve("outside.log"), "user-owned");
        Path logs = temp.resolve("staging/logs");
        Files.createDirectories(logs);
        Path link = logs.resolve("server.log");
        try {
            Files.createSymbolicLink(link, outside);
        } catch (UnsupportedOperationException | java.nio.file.FileSystemException unavailable) {
            org.junit.jupiter.api.Assumptions.abort("symbolic links are unavailable");
        }
        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiProcess.writeSanitizedLog(temp,
                new ByteArrayInputStream("runtime".getBytes()), link, Set.of(), 128));
        assertEquals("user-owned", Files.readString(outside));
    }

    private static Path readyStage(Path cache, String prefix, String fileName, String content) throws Exception {
        Path stage = cache.resolve("staging/" + prefix + ".extract-test");
        Files.createDirectories(stage);
        Files.writeString(stage.resolve(fileName), content);
        Files.writeString(stage.resolve(".shaft-ready"), "");
        return stage;
    }

    private static final class FakeProcess extends Process {
        private Integer exit;
        private boolean destroyed;
        private boolean forciblyDestroyed;
        private FakeHandle descendant;

        private FakeProcess(Integer exit) { this.exit = exit; }
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException {
            while (exit == null) {
                Thread.sleep(5);
            }
            return exit;
        }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) { return exit != null; }
        @Override public int exitValue() { if (exit == null) throw new IllegalThreadStateException(); return exit; }
        @Override public void destroy() { destroyed = true; }
        @Override public Process destroyForcibly() { forciblyDestroyed = true; exit = -1; return this; }
        @Override public boolean isAlive() { return exit == null; }
        @Override public ProcessHandle toHandle() { return descendant == null ? super.toHandle() : descendant; }
    }

    private static final class FakeHandle implements ProcessHandle {
        private boolean destroyed;
        @Override public long pid() { return 42; }
        @Override public java.util.Optional<ProcessHandle> parent() { return java.util.Optional.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> children() { return java.util.stream.Stream.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> descendants() { return java.util.stream.Stream.of(this); }
        @Override public Info info() { throw new UnsupportedOperationException(); }
        @Override public java.util.concurrent.CompletableFuture<ProcessHandle> onExit() { return new java.util.concurrent.CompletableFuture<>(); }
        @Override public boolean supportsNormalTermination() { return true; }
        @Override public boolean destroy() { destroyed = true; return true; }
        @Override public boolean destroyForcibly() { destroyed = true; return true; }
        @Override public boolean isAlive() { return !destroyed; }
        @Override public int compareTo(ProcessHandle other) { return Long.compare(pid(), other.pid()); }
    }
}
