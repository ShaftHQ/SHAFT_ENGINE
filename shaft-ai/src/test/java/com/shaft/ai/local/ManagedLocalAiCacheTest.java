package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class ManagedLocalAiCacheTest {
    @TempDir
    Path temp;

    @Test
    void adoptsAndReusesOnlyAnImmutableOwnedInstallation() throws Exception {
        Path cache = temp.resolve("cache");
        Path stage = readyStage(cache, "runtime", "binary");

        ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "runtime-b10400-windows-x86_64", stage));

        assertTrue(Files.isRegularFile(installed.root().resolve("bin/llama-server")));
        assertEquals(installed, ManagedLocalAiCache.verify(cache, installed.id()));
        assertTrue(Files.isRegularFile(cache.resolve("owner-manifest.json")));
    }

    @Test
    void changedOrUnownedPathsAreNeverClaimedOrDeleted() throws Exception {
        Path cache = temp.resolve("cache");
        ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "runtime-b10400-linux-x86_64",
                        readyStage(cache, "runtime", "binary")));
        Path changed = installed.root().resolve("bin/llama-server");
        Files.writeString(changed, "changed", StandardCharsets.UTF_8);
        Path unknown = installed.root().resolve("user-note.txt");
        Files.writeString(unknown, "mine", StandardCharsets.UTF_8);

        assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.verify(cache, installed.id()));
        ManagedLocalAiCache.CleanResult result = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.clean(cache));

        assertTrue(Files.exists(changed));
        assertTrue(Files.exists(unknown));
        assertEquals(1, result.conflicts().size());
        assertEquals(0, result.deletedFiles());

        Path empty = installed.root().resolve("unknown-empty");
        Files.createDirectory(empty);
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.verify(cache, installed.id()));
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () -> ManagedLocalAiCache.clean(cache));
        assertTrue(Files.isDirectory(empty));
    }

    @Test
    void cleanDeletesOnlyExactOwnedFilesAndIsIdempotent() throws Exception {
        Path cache = temp.resolve("cache");
        ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "model-qwen",
                        readyStage(cache, "model", "weights")));
        Path unrelated = cache.resolve("unowned.txt");
        Files.writeString(unrelated, "mine");

        ManagedLocalAiCache.CleanResult first = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.clean(cache));
        ManagedLocalAiCache.CleanResult second = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.clean(cache));

        assertTrue(first.deletedFiles() >= 2);
        assertFalse(Files.exists(installed.root()));
        assertTrue(Files.exists(unrelated));
        assertEquals(0, second.deletedFiles());
    }

    @Test
    void failedCleanRollbackRestoresTheInstallationAndItsOwnerRecord() throws Exception {
        Path cache = temp.resolve("rollback-cache");
        ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "model-rollback",
                        readyStage(cache, "rollback-model", "weights")));
        Path trash = cache.resolve("trash").resolve("model-rollback-test");
        Files.createDirectories(trash.getParent());
        Files.move(installed.root(), trash);
        Files.writeString(cache.resolve("owner-manifest.json"), "{\"schemaVersion\":1,\"installations\":[]}");

        ManagedLocalAiCache.rollbackClean(cache, installed, trash, new IOException("simulated delete failure"));

        assertEquals(installed, ManagedLocalAiCache.verify(cache, installed.id()));
        assertFalse(Files.exists(trash));
    }

    @Test
    void crossThreadContentionTimesOutWithoutMutation() throws Exception {
        Path cache = temp.resolve("cache");
        CountDownLatch locked = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
            var holder = executor.submit(() -> ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(2), () -> {
                locked.countDown();
                release.await();
                return null;
            }));
            locked.await();

            assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.withLock(cache,
                    Duration.ofMillis(100), () -> ManagedLocalAiCache.adopt(cache, "must-not-install",
                            readyStage(cache, "blocked", "bad"))));
            release.countDown();
            holder.get();
        }
        assertFalse(Files.exists(cache.resolve("installations")));
    }

    @Test
    void rejectsTheStagingRootAndDuplicateOwnerPaths() throws Exception {
        Path cache = temp.resolve("cache");
        Path stage = readyStage(cache, "one", "binary");
        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "bad", cache.resolve("staging"))));
        assertTrue(Files.exists(stage));

        String duplicate = """
                {"schemaVersion":1,"installations":[{"id":"duplicate","rootPath":"installations/x",
                "files":[{"path":"installations/x/a","size":1,"sha256":"%s"},
                {"path":"installations/x/a","size":1,"sha256":"%s"}]}]}
                """.formatted("0".repeat(64), "0".repeat(64));
        Files.createDirectories(cache);
        Files.writeString(cache.resolve("owner-manifest.json"), duplicate);
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.verify(cache, "duplicate"));

        String overlapping = """
                {"schemaVersion":1,"installations":[
                {"id":"one","rootPath":"installations/x","files":[
                {"path":"installations/x/a","size":1,"sha256":"%s"}]},
                {"id":"two","rootPath":"installations/x","files":[
                {"path":"installations/x/a","size":1,"sha256":"%s"}]}]}
                """.formatted("0".repeat(64), "0".repeat(64));
        Files.writeString(cache.resolve("owner-manifest.json"), overlapping);
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.verify(cache, "one"));
    }

    @Test
    void subprocessContentionTimesOutWithoutMutation() throws Exception {
        Path cache = temp.resolve("process-cache");
        Path ready = temp.resolve("process-ready");
        Path release = temp.resolve("process-release");
        String java = Path.of(System.getProperty("java.home"), "bin",
                System.getProperty("os.name").startsWith("Windows") ? "java.exe" : "java").toString();
        String classpath = System.getProperty("surefire.test.class.path", System.getProperty("java.class.path"));
        Process process = new ProcessBuilder(java, "-cp", classpath, LockProbe.class.getName(),
                cache.toString(), ready.toString(), release.toString())
                .redirectErrorStream(true).redirectOutput(ProcessBuilder.Redirect.DISCARD).start();
        try {
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
            while (!Files.exists(ready) && process.isAlive() && System.nanoTime() < deadline) {
                Thread.sleep(20);
            }
            assertTrue(Files.exists(ready), "subprocess did not acquire the cache lock");
            assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.withLock(cache,
                    Duration.ofMillis(150), () -> null));
            assertFalse(Files.exists(cache.resolve("installations")));
        } finally {
            Files.writeString(release, "release");
            assertTrue(process.waitFor(10, TimeUnit.SECONDS));
            if (process.isAlive()) {
                process.destroyForcibly();
            }
        }
        assertEquals(0, process.exitValue());
    }

    @Test
    void windowsJunctionCannotEscapeCacheOwnership() throws Exception {
        assumeTrue(System.getProperty("os.name").startsWith("Windows"));
        Path cache = temp.resolve("junction-cache");
        Path stage = readyStage(cache, "junction", "binary");
        Path outside = temp.resolve("outside");
        Files.createDirectories(outside);
        Path victim = outside.resolve("victim.txt");
        Files.writeString(victim, "user-owned");
        Path junction = stage.resolve("linked");
        Process creator = new ProcessBuilder("cmd.exe", "/d", "/c", "mklink", "/J",
                junction.toString(), outside.toString())
                .redirectErrorStream(true).redirectOutput(ProcessBuilder.Redirect.DISCARD).start();
        assertTrue(creator.waitFor(10, TimeUnit.SECONDS));
        assertEquals(0, creator.exitValue());

        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "junction", stage)));
        assertTrue(Files.exists(victim));
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () -> ManagedLocalAiCache.clean(cache));
        assertTrue(Files.exists(victim));
    }

    @Test
    void recoversInterruptedAdoptAndCleanTransactions() throws Exception {
        Path cache = temp.resolve("cache");
        Path stage = readyStage(cache, "recover", "binary");
        Path destination = cache.resolve("installations/recover-orphan");
        Files.createDirectories(destination.getParent());
        Files.move(stage, destination);
        Files.writeString(cache.resolve("transaction.json"), """
                {"schemaVersion":1,"operation":"ADOPT","id":"recover","source":"%s","target":"%s","files":[]}
                """.formatted(relative(cache, stage), relative(cache, destination)));

        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () -> {
            ManagedLocalAiCache.recover(cache);
            return null;
        });
        assertTrue(Files.isDirectory(stage));
        assertFalse(Files.exists(destination));
        assertFalse(Files.exists(cache.resolve("transaction.json")));

        ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "recover", stage));
        Path trash = cache.resolve("trash/recover-interrupted");
        Files.createDirectories(trash.getParent());
        Files.move(installed.root(), trash);
        String files = installed.files().stream().map(file ->
                "{\"path\":\"%s\",\"size\":%d,\"sha256\":\"%s\"}".formatted(
                        file.path(), file.size(), file.sha256())).collect(java.util.stream.Collectors.joining(","));
        Files.writeString(cache.resolve("transaction.json"), """
                {"schemaVersion":1,"operation":"CLEAN","id":"recover","source":"%s","target":"%s","files":[%s]}
                """.formatted(relative(cache, installed.root()), relative(cache, trash), files));
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () -> {
            ManagedLocalAiCache.recover(cache);
            return null;
        });
        assertTrue(Files.isDirectory(installed.root()));
        assertFalse(Files.exists(trash));
    }

    @Test
    void forgedOrConflictedRecoveryNeverDeletesUnknownContentOrClearsEvidence() throws Exception {
        Path cache = temp.resolve("cache");
        Path victim = cache.resolve("user-data/keep.txt");
        Files.createDirectories(victim.getParent());
        Files.writeString(victim, "mine");
        Files.writeString(cache.resolve("transaction.json"), """
                {"schemaVersion":1,"operation":"CLEAN","id":"not-owned","source":"installations/not-owned-x",
                "target":"user-data","files":[{"path":"installations/not-owned-x/a","size":1,"sha256":"%s"}]}
                """.formatted("0".repeat(64)));
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.recover(cache));
        assertTrue(Files.exists(victim));
        assertTrue(Files.exists(cache.resolve("transaction.json")));

        Files.delete(cache.resolve("transaction.json"));
        ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.withLock(cache,
                Duration.ofSeconds(1), () -> ManagedLocalAiCache.adopt(cache, "conflict",
                        readyStage(cache, "conflict", "binary")));
        Path trash = cache.resolve("trash/conflict-copy");
        copyTree(installed.root(), trash);
        String files = installed.files().stream().map(file ->
                "{\"path\":\"%s\",\"size\":%d,\"sha256\":\"%s\"}".formatted(
                        file.path(), file.size(), file.sha256())).collect(java.util.stream.Collectors.joining(","));
        Files.writeString(cache.resolve("transaction.json"), """
                {"schemaVersion":1,"operation":"CLEAN","id":"conflict","source":"%s",
                "target":"%s","files":[%s]}
                """.formatted(relative(cache, installed.root()), relative(cache, trash), files));
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiCache.recover(cache));
        assertTrue(Files.exists(cache.resolve("transaction.json")));
        assertTrue(Files.exists(installed.root()));
        assertTrue(Files.exists(trash));
    }

    private static Path readyStage(Path cache, String prefix, String content) throws Exception {
        Path stage = cache.resolve("staging").resolve(prefix + ".extract-test");
        Files.createDirectories(stage.resolve("bin"));
        Files.writeString(stage.resolve("bin/llama-server"), content);
        Files.writeString(stage.resolve(".shaft-ready"), "");
        return stage;
    }

    public static final class LockProbe {
        private LockProbe() {
        }

        public static void main(String[] args) throws Exception {
            Path cache = Path.of(args[0]);
            Path ready = Path.of(args[1]);
            Path release = Path.of(args[2]);
            ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(10), () -> {
                Files.writeString(ready, "ready");
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
                while (!Files.exists(release) && System.nanoTime() < deadline) {
                    Thread.sleep(20);
                }
                return null;
            });
        }
    }

    private static String relative(Path cache, Path path) {
        return cache.toAbsolutePath().normalize().relativize(path.toAbsolutePath().normalize())
                .toString().replace('\\', '/');
    }

    private static void copyTree(Path source, Path target) throws Exception {
        try (var paths = Files.walk(source)) {
            for (Path path : paths.toList()) {
                Path destination = target.resolve(source.relativize(path));
                if (Files.isDirectory(path)) {
                    Files.createDirectories(destination);
                } else {
                    Files.copy(path, destination);
                }
            }
        }
    }
}
