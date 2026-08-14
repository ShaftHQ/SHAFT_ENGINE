package com.shaft.ai.local;

import com.shaft.driver.SHAFT;
import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.node.JsonNodeFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.math.BigDecimal;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@org.junit.jupiter.api.parallel.Execution(org.junit.jupiter.api.parallel.ExecutionMode.SAME_THREAD)
@org.junit.jupiter.api.parallel.Isolated
class ManagedLocalAiProcessTest {
    @TempDir
    Path temp;

    @Test
    void supervisorRefusesToStartNativeRuntimeAfterParentExit() {
        assertDoesNotThrow(() -> ManagedLocalAiProcessSupervisor.main(new String[]{
                Long.toString(Long.MAX_VALUE), java.time.Instant.EPOCH.toString(),
                temp.resolve("must-not-start").toString()}));
    }

    @Test
    void supervisorTerminatesPublishedChildWhenItsParentExits() throws Exception {
        Path helper = sleepingProcessJar();
        Path parentMarker = temp.resolve("parent.pid");
        Path childMarker = temp.resolve("child.pid");
        Path javaPath = Path.of(System.getProperty("java.home"), "bin",
                System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT).contains("win")
                        ? "java.exe" : "java");
        Process parent = new ProcessBuilder(javaPath.toString(), "-jar", helper.toString(), parentMarker.toString())
                .start();
        Process supervisor = null;
        try {
            assertTrue(waitForFile(parentMarker, Duration.ofSeconds(3)));
            String parentStartedAt = parent.info().startInstant().orElseThrow().toString();
            ProcessBuilder supervisorBuilder = new ProcessBuilder(javaPath.toString(),
                    ManagedLocalAiProcessSupervisor.class.getName(), Long.toString(parent.pid()), parentStartedAt,
                    javaPath.toString(), "-jar", helper.toString(), childMarker.toString());
            supervisorBuilder.environment().put("CLASSPATH",
                    System.getProperty("surefire.test.class.path", System.getProperty("java.class.path")));
            supervisor = supervisorBuilder.start();
            assertTrue(waitForFile(childMarker, Duration.ofSeconds(3)));
            long childPid = Long.parseLong(Files.readString(childMarker));

            parent.destroyForcibly();
            assertTrue(parent.waitFor(3, java.util.concurrent.TimeUnit.SECONDS));
            assertTrue(supervisor.waitFor(7, java.util.concurrent.TimeUnit.SECONDS));

            assertFalse(ProcessHandle.of(childPid).map(ProcessHandle::isAlive).orElse(false),
                    "the supervisor must terminate a child that was running when its parent exited");
        } finally {
            parent.destroyForcibly();
            if (supervisor != null) {
                supervisor.getOutputStream().close();
                supervisor.destroyForcibly();
            }
        }
    }

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
        assertTrue(ManagedLocalAiProcess.command(executable, model, 0, "alias", keyFile, 1)
                .containsAll(List.of("--host", "127.0.0.1", "--port", "0")));
    }

    @Test
    void launchDeadlineIncludesTimeQueuedBehindAnotherLaunch() throws Exception {
        Path cache = temp.resolve("queued-launch-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-queued",
                        readyStage(cache, "runtime-queued", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-queued",
                        readyStage(cache, "model-queued", "model.gguf", "weights")));
        var firstStarted = new java.util.concurrent.CountDownLatch(1);
        var releaseFirst = new java.util.concurrent.CountDownLatch(1);
        var first = java.util.concurrent.CompletableFuture.supplyAsync(() -> {
            try {
                return ManagedLocalAiProcess.launch(cache, runtime.root().resolve("server"),
                        model.root().resolve("model.gguf"), cache.resolve("staging/logs/first-queued.log"),
                        "expected", 2, Duration.ofSeconds(1),
                        (command, environment, log) -> {
                            firstStarted.countDown();
                            releaseFirst.await();
                            return new FakeProcess(null, "srv  llama_server: listening on http://127.0.0.1:18181\n");
                        }, (process, port, key, alias, timeout) -> { });
            } catch (Exception failure) {
                throw new java.util.concurrent.CompletionException(failure);
            }
        });
        assertTrue(firstStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));
        java.util.concurrent.CompletableFuture.delayedExecutor(150, java.util.concurrent.TimeUnit.MILLISECONDS)
                .execute(releaseFirst::countDown);

        assertThrows(ManagedLocalAiProcess.DeadlineExceededException.class,
                () -> ManagedLocalAiProcess.launch(cache, runtime.root().resolve("server"),
                        model.root().resolve("model.gguf"), cache.resolve("staging/logs/second-queued.log"),
                        "expected", 2, Duration.ofMillis(50),
                        (command, environment, log) -> new FakeProcess(null),
                        (process, port, key, alias, timeout) -> { }));

        first.join().close();
    }

    @Test
    void childOwnedEphemeralPortKeepsBearerAwayFromPreboundSquatter() throws Exception {
        AtomicBoolean squatterSawBearer = new AtomicBoolean();
        HttpServer squatter = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        squatter.createContext("/v1/models", exchange -> {
            squatterSawBearer.set(exchange.getRequestHeaders().containsKey("Authorization"));
            byte[] body = "{\"data\":[{\"id\":\"expected-alias\"}]}".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        squatter.start();
        Path cache = temp.resolve("port-theft-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-port-theft",
                        readyStage(cache, "runtime-port-theft", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-port-theft",
                        readyStage(cache, "model-port-theft", "model.gguf", "weights")));
        ManagedLocalAiProcess.Session session = null;

        try {
            session = ManagedLocalAiProcess.launch(cache, runtime.root().resolve("server"),
                    model.root().resolve("model.gguf"), cache.resolve("staging/logs/port-theft.log"),
                    "expected-alias", 2, Duration.ofSeconds(3),
                    (command, environment, log) -> ManagedLocalAiProcess.start(
                            childRuntimeCommand("expected-alias"), environment, log),
                    (process, port, key, alias, timeout) -> ManagedLocalAiProcess.requireIdentity(
                            process, port, key, alias, timeout, ManagedLocalAiProcess::requestIdentity));

            assertNotEquals(squatter.getAddress().getPort(), session.port(),
                    "the endpoint must be the port atomically bound by the launched child");
            assertFalse(squatterSawBearer.get(), "no bearer may be sent to a pre-bound foreign listener");
        } finally {
            if (session != null) {
                session.close();
            }
            squatter.stop(0);
        }
    }

    @Test
    void supervisorControlPipeTerminatesPublishedChildWhileParentRemainsAlive() throws Exception {
        Path helper = sleepingProcessJar();
        Path childMarker = temp.resolve("controlled-child.pid");
        Path descendantMarker = temp.resolve("controlled-descendant.pid");
        Path javaPath = Path.of(System.getProperty("java.home"), "bin",
                System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT).contains("win")
                        ? "java.exe" : "java");
        Process supervisor = ManagedLocalAiProcess.start(
                List.of(javaPath.toString(), "-jar", helper.toString(), childMarker.toString(),
                        helper.toString(), descendantMarker.toString()),
                ManagedLocalAiProcess.runtimeEnvironment(System.getenv()), temp.resolve("control.log"));
        try {
            assertTrue(waitForFile(childMarker, Duration.ofSeconds(3)));
            assertTrue(waitForFile(descendantMarker, Duration.ofSeconds(3)));
            long childPid = Long.parseLong(Files.readString(childMarker));
            long descendantPid = Long.parseLong(Files.readString(descendantMarker));

            ManagedLocalAiProcess.terminate(supervisor, Duration.ofSeconds(7),
                    new IllegalStateException("test cleanup"));

            assertFalse(supervisor.isAlive());
            assertFalse(ProcessHandle.of(childPid).map(ProcessHandle::isAlive).orElse(false),
                    "closing the ownership pipe must terminate the published child while SHAFT remains alive");
            assertFalse(ProcessHandle.of(descendantPid).map(ProcessHandle::isAlive).orElse(false),
                    "the supervisor must retain and terminate an already-published descendant");
        } finally {
            supervisor.getOutputStream().close();
            supervisor.destroyForcibly();
        }
    }

    @Test
    void runtimeLogIsVisibleBeforeTheOwnedProcessExits() throws Exception {
        Path cache = temp.resolve("live-log-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-live-log",
                        readyStage(cache, "runtime-live-log", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-live-log",
                        readyStage(cache, "model-live-log", "model.gguf", "weights")));
        Path log = cache.resolve("staging/logs/live.log");
        ManagedLocalAiProcess.Session session = null;

        try {
            session = ManagedLocalAiProcess.launch(cache, runtime.root().resolve("server"),
                    model.root().resolve("model.gguf"), log, "expected-alias", 2, Duration.ofSeconds(3),
                    (command, environment, ignored) -> ManagedLocalAiProcess.start(
                            childRuntimeCommand("expected-alias"), environment, log),
                    (process, port, key, alias, timeout) -> ManagedLocalAiProcess.requireIdentity(
                            process, port, key, alias, timeout, ManagedLocalAiProcess::requestIdentity));

            long deadline = System.nanoTime() + Duration.ofSeconds(1).toNanos();
            while (Files.readString(log).isEmpty() && System.nanoTime() < deadline) {
                Thread.sleep(10);
            }
            assertTrue(session.process().isAlive(), "the log proof must be observed while the runtime is live");
            assertTrue(Files.readString(log).contains("listening on http://127.0.0.1:"),
                    "startup diagnostics must be flushed before process EOF");
        } finally {
            if (session != null) {
                session.close();
            }
        }
    }

    @Test
    void publicCleanStopsTheProcessThatOwnsInstalledArtifacts() throws Exception {
        Path cache = temp.resolve("clean-live-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-clean-live",
                        readyStage(cache, "runtime-clean-live", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-clean-live",
                        readyStage(cache, "model-clean-live", "model.gguf", "weights")));
        FakeProcess process = new FakeProcess(null, "srv  llama_server: listening on http://127.0.0.1:19191\n");
        ManagedLocalAiProcess.Session session = ManagedLocalAiProcess.launch(cache,
                runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                cache.resolve("staging/logs/clean-live.log"), "expected", 2, Duration.ofSeconds(1),
                (command, environment, log) -> process,
                (started, port, key, alias, timeout) -> { });
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(false, false, "auto", cache.toString(), 1, 1),
                new ManagedLocalAiHardware.HostAccess() {
                    @Override
                    public String osName() { return "Windows 11"; }
                    @Override
                    public String architecture() { return "amd64"; }
                    @Override
                    public String abi() { return "windows-msvc"; }
                    @Override
                    public String abiVersion() { return ""; }
                    @Override
                    public long availableMemoryBytes() { return 16L * 1024 * 1024 * 1024; }
                    @Override
                    public int availableProcessors() { return 8; }
                    @Override
                    public long usableSpace(Path ignored) { return 64L * 1024 * 1024 * 1024; }
                    @Override
                    public String read(String ignored) { return null; }
                });

        try {
            service.clean();
            assertFalse(process.isAlive(), "clean must stop the process before mutating its artifacts");
        } finally {
            session.close();
        }
    }

    @Test
    void publicCleanCannotCrossAConcurrentLaunch() throws Exception {
        Path cache = temp.resolve("clean-launch-race-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-clean-race",
                        readyStage(cache, "runtime-clean-race", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-clean-race",
                        readyStage(cache, "model-clean-race", "model.gguf", "weights")));
        var startEntered = new java.util.concurrent.CountDownLatch(1);
        var releaseStart = new java.util.concurrent.CountDownLatch(1);
        FakeProcess process = new FakeProcess(null, "srv  llama_server: listening on http://127.0.0.1:19191\n");
        var launch = java.util.concurrent.CompletableFuture.supplyAsync(() -> {
            try {
                return ManagedLocalAiProcess.launch(cache, runtime.root().resolve("server"),
                        model.root().resolve("model.gguf"), cache.resolve("staging/logs/clean-race.log"),
                        "expected", 2, Duration.ofSeconds(3), (command, environment, log) -> {
                            startEntered.countDown();
                            releaseStart.await();
                            return process;
                        }, (started, port, key, alias, timeout) -> { });
            } catch (Exception failure) {
                throw new java.util.concurrent.CompletionException(failure);
            }
        });
        assertTrue(startEntered.await(1, java.util.concurrent.TimeUnit.SECONDS));
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(false, false, "auto", cache.toString(), 2, 1),
                new ManagedLocalAiHardware.HostAccess() {
                    public String osName() { return "Windows 11"; }
                    public String architecture() { return "amd64"; }
                    public String abi() { return "windows-msvc"; }
                    public String abiVersion() { return ""; }
                    public long availableMemoryBytes() { return 16L * 1024 * 1024 * 1024; }
                    public int availableProcessors() { return 8; }
                    public long usableSpace(Path ignored) { return 64L * 1024 * 1024 * 1024; }
                    public String read(String ignored) { return null; }
                });
        var clean = java.util.concurrent.CompletableFuture.runAsync(() -> {
            try {
                service.clean();
            } catch (Exception failure) {
                throw new java.util.concurrent.CompletionException(failure);
            }
        });

        Thread.sleep(100);
        assertFalse(clean.isDone(), "clean must wait for exclusive launch lifecycle ownership");
        releaseStart.countDown();
        ManagedLocalAiProcess.Session session = launch.get(2, java.util.concurrent.TimeUnit.SECONDS);
        clean.get(2, java.util.concurrent.TimeUnit.SECONDS);
        assertFalse(process.isAlive(), "clean must terminate the concurrent launch before cache mutation");
        session.close();
    }

    @Test
    void startupWithoutExactChildPortNeverReachesAuthenticatedIdentity() throws Exception {
        Path cache = temp.resolve("missing-startup-port-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-missing-port",
                        readyStage(cache, "runtime-missing-port", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-missing-port",
                        readyStage(cache, "model-missing-port", "model.gguf", "weights")));
        AtomicBoolean identityCalled = new AtomicBoolean();

        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.launch(cache,
                runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                cache.resolve("staging/logs/missing-port.log"), "expected", 2, Duration.ofMillis(300),
                (command, environment, log) -> new FakeProcess(null),
                (process, port, key, alias, timeout) -> {
                    identityCalled.set(true);
                    throw new IllegalStateException("identity must not receive an unproved endpoint");
                }));

        assertFalse(identityCalled.get(), "bearer identity must wait for an exact child-owned endpoint");
    }

    @Test
    void oversizedNumericStartupPortFailsAsBoundedInputError() throws Exception {
        Path cache = temp.resolve("oversized-startup-port-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-oversized-port",
                        readyStage(cache, "runtime-oversized-port", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-oversized-port",
                        readyStage(cache, "model-oversized-port", "model.gguf", "weights")));

        IllegalStateException failure = assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.launch(
                cache, runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                cache.resolve("staging/logs/oversized-port.log"), "expected", 2, Duration.ofMillis(300),
                (command, environment, log) -> new FakeProcess(null,
                        "srv  llama_server: listening on http://127.0.0.1:999999999999999999999999\n"),
                (process, port, key, alias, timeout) -> { }));

        assertInstanceOf(IOException.class, failure.getCause());
    }

    @Test
    void negatedListeningTextNeverQualifiesAsChildEndpoint() throws Exception {
        Path cache = temp.resolve("negated-startup-port-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-negated-port",
                        readyStage(cache, "runtime-negated-port", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-negated-port",
                        readyStage(cache, "model-negated-port", "model.gguf", "weights")));
        AtomicBoolean identityCalled = new AtomicBoolean();

        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.launch(cache,
                runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                cache.resolve("staging/logs/negated-port.log"), "expected", 2, Duration.ofMillis(300),
                (command, environment, log) -> new FakeProcess(null,
                        "server: not listening on http://127.0.0.1:19191\n"
                                + "warning: foreign process: listening on http://127.0.0.1:19192\n"
                                + "srv  llama_server: not: listening on http://127.0.0.1:19193\n"),
                (process, port, key, alias, timeout) -> {
                    identityCalled.set(true);
                    throw new IllegalStateException("identity must not receive a negated endpoint");
                }));

        assertFalse(identityCalled.get(), "negated startup text must not release the bearer");
    }

    @Test
    void runtimeIdentityIncludesTheEffectiveThreadAllocation() throws Exception {
        Path cache = temp.resolve("thread-identity-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-thread-identity",
                        readyStage(cache, "runtime-thread-identity", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-thread-identity",
                        readyStage(cache, "model-thread-identity", "model.gguf", "weights")));
        ManagedLocalAiProcess.Session session = ManagedLocalAiProcess.launch(cache,
                runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                cache.resolve("staging/logs/thread-identity.log"), "expected", 2, Duration.ofSeconds(1),
                (command, environment, log) -> new FakeProcess(null,
                        "srv  llama_server: listening on http://127.0.0.1:19191\n"),
                (process, port, key, alias, timeout) -> { });

        try {
            assertTrue(session.matches(runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                    "expected", 2));
            assertFalse(session.matches(runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                    "expected", 4), "a changed CPU allocation must require a new runtime process");
        } finally {
            session.close();
        }
    }

    @Test
    void shutdownSweepCannotMissTheStartToOwnershipPublicationWindow() throws Exception {
        Path cache = temp.resolve("start-publication-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-start-publication",
                        readyStage(cache, "runtime-start-publication", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-start-publication",
                        readyStage(cache, "model-start-publication", "model.gguf", "weights")));
        var startEntered = new java.util.concurrent.CountDownLatch(1);
        var releaseStart = new java.util.concurrent.CountDownLatch(1);
        AtomicBoolean cancelled = new AtomicBoolean();
        FakeProcess process = new FakeProcess(null, "srv  llama_server: listening on http://127.0.0.1:19191\n");
        var launch = java.util.concurrent.CompletableFuture.runAsync(() -> {
            try {
                ManagedLocalAiProcess.launch(cache, runtime.root().resolve("server"),
                        model.root().resolve("model.gguf"), cache.resolve("staging/logs/start-publication.log"),
                        "expected", 2, Duration.ofSeconds(3), cancelled::get,
                        (command, environment, log) -> {
                            startEntered.countDown();
                            boolean released = false;
                            while (!released) {
                                try {
                                    released = releaseStart.await(25, java.util.concurrent.TimeUnit.MILLISECONDS);
                                } catch (InterruptedException ignored) {
                                    // Deliberately model a native start that ignores interruption.
                                }
                            }
                            return process;
                        }, (started, port, key, alias, timeout) -> { });
            } catch (Exception failure) {
                throw new java.util.concurrent.CompletionException(failure);
            }
        });
        assertTrue(startEntered.await(1, java.util.concurrent.TimeUnit.SECONDS));
        cancelled.set(true);
        var shutdown = java.util.concurrent.CompletableFuture.runAsync(
                ManagedLocalAiProcess::terminateRetainedLaunches);

        Thread.sleep(100);
        assertFalse(shutdown.isDone(),
                "shutdown must retain its hook until an interruption-insensitive start publishes ownership");
        releaseStart.countDown();
        shutdown.get(2, java.util.concurrent.TimeUnit.SECONDS);
        assertFalse(process.isAlive(), "a process completing during shutdown must be terminated before hook return");
        assertThrows(java.util.concurrent.CompletionException.class, launch::join);
    }

    @Test
    void shutdownSweepBoundsAnUnresolvedNativeStart() throws Exception {
        Path cache = temp.resolve("bounded-start-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-bounded-start",
                        readyStage(cache, "runtime-bounded-start", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-bounded-start",
                        readyStage(cache, "model-bounded-start", "model.gguf", "weights")));
        var startEntered = new java.util.concurrent.CountDownLatch(1);
        var releaseStart = new java.util.concurrent.CountDownLatch(1);
        AtomicBoolean cancelled = new AtomicBoolean();
        FakeProcess process = new FakeProcess(null, "srv  llama_server: listening on http://127.0.0.1:19191\n");
        var launch = java.util.concurrent.CompletableFuture.runAsync(() -> {
            try {
                ManagedLocalAiProcess.launch(cache, runtime.root().resolve("server"),
                        model.root().resolve("model.gguf"), cache.resolve("staging/logs/bounded-start.log"),
                        "expected", 2, Duration.ofSeconds(5), cancelled::get,
                        (command, environment, log) -> {
                            startEntered.countDown();
                            while (true) {
                                try {
                                    releaseStart.await();
                                    return process;
                                } catch (InterruptedException ignored) {
                                    // Deliberately model a native start that never resolves during shutdown.
                                }
                            }
                        }, (started, port, key, alias, timeout) -> { });
            } catch (Exception failure) {
                throw new java.util.concurrent.CompletionException(failure);
            }
        });
        assertTrue(startEntered.await(1, java.util.concurrent.TimeUnit.SECONDS));
        cancelled.set(true);

        var shutdown = java.util.concurrent.CompletableFuture.runAsync(
                ManagedLocalAiProcess::terminateRetainedLaunches);
        shutdown.get(4, java.util.concurrent.TimeUnit.SECONDS);
        assertFalse(launch.isDone(), "shutdown must return even when native start never resolves");

        releaseStart.countDown();
        assertThrows(java.util.concurrent.CompletionException.class, launch::join);
        assertFalse(process.isAlive());
    }

    @Test
    void forceShutdownRefreshesDescendantsBeforeKillingTheParent() {
        SurvivingTreeProcess process = new SurvivingTreeProcess();
        process.descendantVisible = false;
        ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(
                process, 19191, "expected", "key", Duration.ofMillis(200));
        process.descendantVisible = true;

        session.forceKillAndAwait(Duration.ofMillis(200), new IllegalStateException("shutdown"));

        assertTrue(process.descendant.forceKillCalls.get() > 0,
                "force shutdown must refresh descendants after initial session capture");
        process.descendant.alive = false;
    }

    @Test
    void startupParseFailureRetainsSurvivingProcessTreeBeforeRetry() throws Exception {
        Path cache = temp.resolve("startup-survivor-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-startup-survivor",
                        readyStage(cache, "runtime-startup-survivor", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-startup-survivor",
                        readyStage(cache, "model-startup-survivor", "model.gguf", "weights")));
        AtomicInteger starts = new AtomicInteger();
        List<SurvivingTreeProcess> processes = new java.util.concurrent.CopyOnWriteArrayList<>();

        try {
            assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.launch(cache,
                    runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                    cache.resolve("staging/logs/startup-survivor.log"), "expected", 2,
                    Duration.ofMillis(300), (command, environment, log) -> {
                        starts.incrementAndGet();
                        SurvivingTreeProcess process = new SurvivingTreeProcess("x".repeat(4_097));
                        processes.add(process);
                        return process;
                    }, (process, port, key, alias, timeout) -> {
                        throw new AssertionError("identity must not run before startup parsing succeeds");
                    }));

            assertEquals(1, starts.get(), "a surviving startup tree must block every retry");
            assertTrue(waitForForceKillRetry(processes.getFirst().descendant, Duration.ofMillis(300)),
                    "startup failure must retain an active process-tree cleanup owner");
        } finally {
            processes.forEach(process -> process.descendant.alive = false);
            ManagedLocalAiProcess.terminateRetainedLaunches();
        }
    }

    private List<String> childRuntimeCommand(String alias) {
        Path javaExecutable = Path.of(System.getProperty("java.home"), "bin",
                System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT).contains("win")
                        ? "java.exe" : "java");
        String testClassPath = System.getProperty("surefire.test.class.path",
                System.getProperty("java.class.path"));
        return List.of(javaExecutable.toString(), "-cp", testClassPath,
                LoopbackRuntime.class.getName(), alias);
    }

    public static final class LoopbackRuntime {
        public static void main(String[] args) throws Exception {
            HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            byte[] body = ("{\"data\":[{\"id\":\"" + args[0] + "\"}]}")
                    .getBytes(StandardCharsets.UTF_8);
            server.createContext("/v1/models", exchange -> {
                exchange.sendResponseHeaders(200, body.length);
                exchange.getResponseBody().write(body);
                exchange.close();
            });
            server.start();
            System.out.println("srv  llama_server: listening on http://127.0.0.1:" + server.getAddress().getPort());
            System.out.flush();
            Thread.currentThread().join();
        }
    }

    @Test
    void launchRetriesPortTheftAndRequiresAuthenticatedAliasIdentity() throws Exception {
        AtomicInteger starts = new AtomicInteger();
        FakeProcess stolen = new FakeProcess(1, "srv  llama_server: listening on http://127.0.0.1:18181\n");
        FakeProcess owned = new FakeProcess(null, "srv  llama_server: listening on http://127.0.0.1:18182\n");
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
                2, Duration.ofSeconds(1),
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
    void launchRetriesShareOneTotalDeadline() throws Exception {
        AtomicInteger starts = new AtomicInteger();
        Path cache = temp.resolve("deadline-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-deadline",
                        readyStage(cache, "runtime-deadline", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-deadline",
                        readyStage(cache, "model-deadline", "model.gguf", "weights")));
        long started = System.nanoTime();

        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.launch(cache,
                runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                cache.resolve("staging/logs/deadline.log"), "expected-alias", 2, Duration.ofMillis(100),
                (command, environment, log) -> {
                    starts.incrementAndGet();
                    return new FakeProcess(null, "srv  llama_server: listening on http://127.0.0.1:18181\n");
                }, (process, port, key, alias, timeout) -> {
                    Thread.sleep(timeout.toMillis());
                    throw new IllegalStateException("identity unavailable");
                }));
        long elapsedMillis = Duration.ofNanos(System.nanoTime() - started).toMillis();

        assertTrue(starts.get() <= 1, "expired launch deadline must prevent another attempt");
        assertTrue(elapsedMillis < 1_200,
                "launch must not multiply the deadline across retries; fixed filesystem preflight may remain");
    }

    @Test
    void failedLaunchDoesNotRetryWhilePriorProcessTreeSurvives() throws Exception {
        AtomicInteger starts = new AtomicInteger();
        List<SurvivingTreeProcess> processes = new java.util.concurrent.CopyOnWriteArrayList<>();
        Path cache = temp.resolve("survivor-cache");
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "runtime-survivor",
                        readyStage(cache, "runtime-survivor", "server", "binary")));
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "model-survivor",
                        readyStage(cache, "model-survivor", "model.gguf", "weights")));

        try {
            assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.launch(cache,
                    runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                    cache.resolve("staging/logs/survivor.log"), "expected", 2, Duration.ofMillis(300),
                    (command, environment, log) -> {
                        starts.incrementAndGet();
                        SurvivingTreeProcess process = new SurvivingTreeProcess(
                                "srv  llama_server: listening on http://127.0.0.1:18181\n");
                        processes.add(process);
                        return process;
                    }, (process, port, key, alias, timeout) -> {
                        throw new IllegalStateException("identity failed");
                    }));
            assertEquals(1, starts.get(),
                    "a new launch must not start while an earlier process tree still survives cleanup");
            assertTrue(waitForForceKillRetry(processes.getFirst().descendant, Duration.ofMillis(300)),
                    "failed-launch survivors must retain an active cleanup owner");
            assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.launch(cache,
                    runtime.root().resolve("server"), model.root().resolve("model.gguf"),
                    cache.resolve("staging/logs/blocked-by-survivor.log"), "expected", 2,
                    Duration.ofMillis(100),
                    (command, environment, log) -> {
                        starts.incrementAndGet();
                        return new SurvivingTreeProcess();
                    }, (process, port, key, alias, timeout) -> { }));
            assertEquals(1, starts.get(), "later launches must be rejected while the retained tree survives");
        } finally {
            processes.forEach(process -> process.descendant.alive = false);
        }
    }

    private boolean waitForForceKillRetry(SurvivingHandle handle, Duration timeout) throws InterruptedException {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (handle.forceKillCalls.get() < 2 && System.nanoTime() < deadline) {
            Thread.sleep(5);
        }
        return handle.forceKillCalls.get() >= 2;
    }

    @Test
    void identityRejectsPublicHealthSpoofWrongBearerAndWrongAlias() throws Exception {
        FakeProcess process = new FakeProcess(null);
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.requireIdentity(process, 1234,
                "secret", "expected", Duration.ofMillis(50),
                (uri, bearer, timeout) -> Map.of("data", List.of(Map.of("id", "foreign")))));
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.requireIdentity(process, 1234,
                "secret", "expected", Duration.ofMillis(50),
                (uri, bearer, timeout) -> { throw new SecurityException("wrong bearer"); }));
        assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.requireIdentity(process, 1234,
                "secret", "expected", Duration.ofMillis(50),
                (uri, bearer, timeout) -> Map.of("data", List.of(Map.of("id", "expected"), Map.of("id", "foreign")))));
    }

    @Test
    void productionIdentityRequestCannotOutliveIdentityDeadline() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/v1/models", exchange -> {
            try {
                Thread.sleep(250);
                byte[] body = "{\"data\":[{\"id\":\"expected\"}]}".getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(200, body.length);
                exchange.getResponseBody().write(body);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
            } finally {
                exchange.close();
            }
        });
        server.start();
        long started = System.nanoTime();
        long elapsedMillis;

        try {
            assertThrows(IllegalStateException.class, () -> ManagedLocalAiProcess.requireIdentity(
                    new FakeProcess(null), server.getAddress().getPort(), "secret", "expected",
                    Duration.ofMillis(50), ManagedLocalAiProcess::requestIdentity));
            elapsedMillis = Duration.ofNanos(System.nanoTime() - started).toMillis();
        } finally {
            server.stop(0);
        }

        assertTrue(elapsedMillis < 200,
                "production identity HTTP must share the caller's deadline");
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
    void terminationWaitPhasesShareOneTotalDeadline() {
        BlockingProcess process = new BlockingProcess();

        assertThrows(IllegalStateException.class,
                () -> ManagedLocalAiProcess.terminate(process, Duration.ofMillis(80), null));

        assertTrue(process.requestedWaitNanos <= Duration.ofMillis(80).toNanos(),
                "parent and forced waits must share the cleanup timeout");
    }

    @Test
    void interruptedTerminationStillIssuesForceKillAndPreservesInterrupt() {
        InterruptibleProcess process = new InterruptibleProcess();
        Thread.currentThread().interrupt();

        try {
            ManagedLocalAiProcess.terminate(process, Duration.ofMillis(50), null);

            assertTrue(process.forciblyDestroyed,
                    "interrupted cleanup must still issue a force-kill before returning");
            assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
    }

    @Test
    void concurrentSessionClosersSerializeOwnershipUpdates() throws Exception {
        ConcurrentCloseProcess process = new ConcurrentCloseProcess();
        ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(
                process, 18181, "alias", "key", Duration.ofMillis(500));
        var first = java.util.concurrent.CompletableFuture.runAsync(() ->
                session.close(Duration.ofMillis(500), new IllegalStateException("first")));
        assertTrue(process.destroyEntered.await(1, java.util.concurrent.TimeUnit.SECONDS));
        var second = java.util.concurrent.CompletableFuture.runAsync(() ->
                session.close(Duration.ofMillis(500), new IllegalStateException("second")));

        try {
            Thread.sleep(100);
            assertEquals(1, process.destroyCalls.get(),
                    "only one termination transaction may mutate retained ownership at a time");
        } finally {
            process.release.countDown();
            first.join();
            second.join();
        }
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

    @Test
    void inferenceUsesAuthenticatedSchemaConstrainedLoopbackAndNormalizesOutput() throws Exception {
        var received = new java.util.concurrent.atomic.AtomicReference<String>();
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/v1/chat/completions", exchange -> {
            assertEquals("Bearer secret-key", exchange.getRequestHeaders().getFirst("Authorization"));
            received.set(new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
            byte[] response = "{\"model\":\"selected-model\",\"choices\":[{\"message\":{\"content\":\"{\\\"answer\\\":\\\"local\\\"}\"}}],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2}}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();
        try {
            var schema = JsonNodeFactory.instance.objectNode().put("type", "object");
            schema.set("properties", JsonNodeFactory.instance.objectNode()
                    .set("answer", JsonNodeFactory.instance.objectNode().put("type", "string")));
            AiRequest request = AiRequest.builder("managed-inference", schema)
                    .text("approved redacted evidence")
                    .budget(new AiBudget(100, 40, BigDecimal.ZERO))
                    .timeout(Duration.ofSeconds(2))
                    .deterministicFallback(JsonNodeFactory.instance.objectNode().put("answer", "fallback"))
                    .build();
            ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(new FakeProcess(null),
                    server.getAddress().getPort(), "selected-model", "secret-key", Duration.ZERO);

            AiResponse response = ManagedLocalAiProcess.infer(session, request);

            assertEquals(AiResponseStatus.SUCCESS, response.status());
            assertEquals("local", response.structuredPayload().path("answer").asText());
            assertEquals(3, response.usage().inputTokens());
            assertEquals(2, response.usage().outputTokens());
            assertTrue(received.get().contains("approved redacted evidence"));
            assertTrue(received.get().contains("response_format"));
            assertTrue(received.get().contains("selected-model"));
            assertFalse(received.get().contains("secret-key"));
        } finally {
            server.stop(0);
        }
    }

    @Test
    void inferenceRejectsSchemaViolationsHonorsOneDeadlineAndClosesEveryBody() {
        var schema = JsonNodeFactory.instance.objectNode().put("type", "object");
        schema.set("properties", JsonNodeFactory.instance.objectNode()
                .set("answer", JsonNodeFactory.instance.objectNode().put("type", "string")));
        schema.putArray("required").add("answer");
        AiRequest request = AiRequest.builder("managed-inference", schema)
                .timeout(Duration.ofMillis(300))
                .deterministicFallback(JsonNodeFactory.instance.objectNode().put("answer", "fallback"))
                .build();
        ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(new FakeProcess(null),
                18181, "selected-model", "secret-key", Duration.ZERO);
        AtomicBoolean closed = new AtomicBoolean();
        String invalid = "{\"model\":\"selected-model\",\"choices\":[{\"message\":{\"content\":\"{\\\"answer\\\":7}\"}}]}";

        AiResponse invalidResponse = ManagedLocalAiProcess.infer(session, request,
                (uri, bearer, body, timeout) -> new ManagedLocalAiProcess.InferenceResponse(200,
                        new ByteArrayInputStream(invalid.getBytes(StandardCharsets.UTF_8)) {
                            @Override public void close() throws IOException {
                                closed.set(true);
                                super.close();
                            }
                        }));

        assertEquals(AiResponseStatus.INVALID_RESPONSE, invalidResponse.status());
        assertEquals("fallback", invalidResponse.structuredPayload().path("answer").asText());
        assertTrue(closed.get());

        long started = System.nanoTime();
        AiResponse timedOut = ManagedLocalAiProcess.infer(session, request, (uri, bearer, body, timeout) -> {
            Thread.sleep(200);
            return new ManagedLocalAiProcess.InferenceResponse(200, new java.io.InputStream() {
                @Override public int read() throws IOException {
                    try {
                        Thread.sleep(1_000);
                    } catch (InterruptedException interrupted) {
                        Thread.currentThread().interrupt();
                        throw new java.io.InterruptedIOException();
                    }
                    return -1;
                }
            });
        });
        long elapsedMillis = Duration.ofNanos(System.nanoTime() - started).toMillis();

        assertEquals(AiResponseStatus.TIMEOUT, timedOut.status());
        assertTrue(elapsedMillis < 420, "one request deadline must cover headers and body");
    }

    @Test
    void inferenceMapsKnownHttpFailuresBeforeReadingAndClosesTheirBodies() {
        AiRequest request = AiRequest.builder("managed-inference", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofMillis(200))
                .deterministicFallback(JsonNodeFactory.instance.objectNode().put("fallback", true))
                .build();
        ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(new FakeProcess(null),
                18181, "selected-model", "secret-key", Duration.ZERO);
        AtomicBoolean closed = new AtomicBoolean();

        AiResponse authentication = ManagedLocalAiProcess.infer(session, request,
                (uri, bearer, body, timeout) -> new ManagedLocalAiProcess.InferenceResponse(401,
                        new java.io.InputStream() {
                            @Override public int read() throws IOException {
                                try {
                                    Thread.sleep(1_000);
                                } catch (InterruptedException interrupted) {
                                    Thread.currentThread().interrupt();
                                    throw new java.io.InterruptedIOException();
                                }
                                return -1;
                            }

                            @Override public void close() {
                                closed.set(true);
                            }
                        }));
        AiResponse rateLimited = ManagedLocalAiProcess.infer(session, request,
                (uri, bearer, body, timeout) -> new ManagedLocalAiProcess.InferenceResponse(429,
                        new ByteArrayInputStream(new byte[0])));

        assertEquals(AiResponseStatus.AUTHENTICATION_FAILED, authentication.status());
        assertTrue(closed.get());
        assertEquals(AiResponseStatus.RATE_LIMITED, rateLimited.status());
    }

    @Test
    void inferenceAlwaysAppliesTheConfiguredOutputTokenCeiling() {
        SHAFT.Properties.pilot.set().maxOutputTokens(17);
        try {
            var schema = JsonNodeFactory.instance.objectNode().put("type", "object");
            String success = "{\"model\":\"selected-model\",\"choices\":[{\"message\":{\"content\":\"{}\"}}]}";
            ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(new FakeProcess(null),
                    18181, "selected-model", "secret-key", Duration.ZERO);
            var bodies = new java.util.ArrayList<String>();
            ManagedLocalAiProcess.InferenceRequester requester = (uri, bearer, body, timeout) -> {
                bodies.add(body);
                return new ManagedLocalAiProcess.InferenceResponse(200,
                        new ByteArrayInputStream(success.getBytes(StandardCharsets.UTF_8)));
            };

            ManagedLocalAiProcess.infer(session, AiRequest.builder("inherit-ceiling", schema).build(), requester);
            ManagedLocalAiProcess.infer(session, AiRequest.builder("clamp-ceiling", schema)
                    .budget(new AiBudget(0, 50, BigDecimal.ZERO)).build(), requester);

            assertEquals(17, new tools.jackson.databind.ObjectMapper().readTree(bodies.get(0))
                    .path("max_tokens").asLong());
            assertEquals(17, new tools.jackson.databind.ObjectMapper().readTree(bodies.get(1))
                    .path("max_tokens").asLong());
        } catch (tools.jackson.core.JacksonException malformed) {
            throw new AssertionError(malformed);
        } finally {
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    @Test
    void inferenceNormalizesInvalidUsageCountsToUnavailable() {
        var schema = JsonNodeFactory.instance.objectNode().put("type", "object");
        String success = "{\"model\":\"selected-model\",\"choices\":[{\"message\":{\"content\":\"{}\"}}],"
                + "\"usage\":{\"prompt_tokens\":-1,\"completion_tokens\":-2}}";
        ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(new FakeProcess(null),
                18181, "selected-model", "secret-key", Duration.ZERO);

        AiResponse response = ManagedLocalAiProcess.infer(session,
                AiRequest.builder("usage-normalization", schema).build(),
                (uri, bearer, body, timeout) -> new ManagedLocalAiProcess.InferenceResponse(200,
                        new ByteArrayInputStream(success.getBytes(StandardCharsets.UTF_8))));

        assertEquals(AiResponseStatus.SUCCESS, response.status());
        assertEquals(0, response.usage().inputTokens());
        assertEquals(0, response.usage().outputTokens());
    }

    @Test
    void inferenceHonorsADeadlineStartedBeforeProcessLaunch() throws Exception {
        AiRequest request = AiRequest.builder("whole-lifecycle-deadline", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofSeconds(1)).build();
        ManagedLocalAiProcess.Session session = new ManagedLocalAiProcess.Session(new FakeProcess(null),
                18181, "selected-model", "secret-key", Duration.ZERO);
        long deadline = System.nanoTime() + Duration.ofMillis(20).toNanos();
        Thread.sleep(30);

        AiResponse response = ManagedLocalAiProcess.infer(session, request,
                (uri, bearer, body, timeout) -> { throw new AssertionError("expired request must not be sent"); },
                deadline);

        assertEquals(AiResponseStatus.TIMEOUT, response.status());
    }

    private static Path readyStage(Path cache, String prefix, String fileName, String content) throws Exception {
        Path stage = cache.resolve("staging/" + prefix + ".extract-test");
        Files.createDirectories(stage);
        Files.writeString(stage.resolve(fileName), content);
        Files.writeString(stage.resolve(".shaft-ready"), "");
        return stage;
    }

    private Path sleepingProcessJar() throws Exception {
        Path jar = temp.resolve("sleeping-process.jar");
        String className = SleepingProcess.class.getName();
        String resource = className.replace('.', '/') + ".class";
        var manifest = new java.util.jar.Manifest();
        manifest.getMainAttributes().put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0");
        manifest.getMainAttributes().put(java.util.jar.Attributes.Name.MAIN_CLASS, className);
        try (var input = SleepingProcess.class.getClassLoader().getResourceAsStream(resource);
             var output = new java.util.jar.JarOutputStream(Files.newOutputStream(jar), manifest)) {
            if (input == null) {
                throw new IllegalStateException("Missing test helper bytecode: " + resource);
            }
            output.putNextEntry(new java.util.jar.JarEntry(resource));
            input.transferTo(output);
            output.closeEntry();
        }
        return jar;
    }

    private static boolean waitForFile(Path path, Duration timeout) throws InterruptedException {
        long deadline = System.nanoTime() + timeout.toNanos();
        while ((!Files.exists(path) || fileIsEmpty(path)) && System.nanoTime() < deadline) {
            Thread.sleep(10);
        }
        return Files.exists(path) && !fileIsEmpty(path);
    }

    private static boolean fileIsEmpty(Path path) {
        try {
            return Files.size(path) == 0;
        } catch (IOException missing) {
            return true;
        }
    }

    public static final class SleepingProcess {
        private SleepingProcess() { }

        public static void main(String[] arguments) throws Exception {
            if (arguments.length == 3) {
                Path javaPath = Path.of(System.getProperty("java.home"), "bin",
                        System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT).contains("win")
                                ? "java.exe" : "java");
                new ProcessBuilder(javaPath.toString(), "-jar", arguments[1], arguments[2]).start();
                Path descendantMarker = Path.of(arguments[2]);
                long deadline = System.nanoTime() + Duration.ofSeconds(3).toNanos();
                while ((!Files.exists(descendantMarker) || Files.size(descendantMarker) == 0)
                        && System.nanoTime() < deadline) {
                    Thread.sleep(10);
                }
                if (!Files.exists(descendantMarker) || Files.size(descendantMarker) == 0) {
                    throw new IllegalStateException("Descendant did not publish its identity");
                }
            }
            Files.writeString(Path.of(arguments[0]), Long.toString(ProcessHandle.current().pid()));
            Thread.sleep(Long.MAX_VALUE);
        }
    }

    private static final class FakeProcess extends Process {
        private Integer exit;
        private boolean destroyed;
        private boolean forciblyDestroyed;
        private FakeHandle descendant;
        private final java.io.InputStream input;

        private FakeProcess(Integer exit) { this(exit, ""); }
        private FakeProcess(Integer exit, String output) {
            this.exit = exit;
            this.input = new ByteArrayInputStream(output.getBytes(StandardCharsets.UTF_8));
        }
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return input; }
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

    private static final class BlockingProcess extends Process {
        private long requestedWaitNanos;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException { Thread.currentThread().join(); return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException {
            requestedWaitNanos += unit.toNanos(timeout);
            unit.sleep(timeout);
            return false;
        }
        @Override public int exitValue() { throw new IllegalThreadStateException(); }
        @Override public void destroy() { /* Simulates a process that ignores graceful termination. */ }
        @Override public Process destroyForcibly() { return this; }
        @Override public boolean isAlive() { return true; }
    }

    private static final class InterruptibleProcess extends Process {
        private boolean forciblyDestroyed;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException { Thread.currentThread().join(); return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException {
            if (Thread.interrupted()) {
                throw new InterruptedException("cleanup interrupted");
            }
            return forciblyDestroyed;
        }
        @Override public int exitValue() { if (!forciblyDestroyed) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { /* Simulates a process that ignores graceful termination. */ }
        @Override public Process destroyForcibly() { forciblyDestroyed = true; return this; }
        @Override public boolean isAlive() { return !forciblyDestroyed; }
    }

    private static final class SurvivingTreeProcess extends Process {
        private boolean alive = true;
        private volatile boolean descendantVisible = true;
        private final SurvivingHandle descendant = new SurvivingHandle();
        private final java.io.InputStream input;
        private SurvivingTreeProcess() { this(""); }
        private SurvivingTreeProcess(String output) {
            input = new ByteArrayInputStream(output.getBytes(StandardCharsets.UTF_8));
        }
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return input; }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() { return alive ? 0 : -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) { return !alive; }
        @Override public int exitValue() { if (alive) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { /* Simulates a process that ignores graceful termination. */ }
        @Override public Process destroyForcibly() { alive = false; return this; }
        @Override public boolean isAlive() { return alive; }
        @Override public ProcessHandle toHandle() { return new ParentHandle(this); }
    }

    private static final class ConcurrentCloseProcess extends Process {
        private final AtomicInteger destroyCalls = new AtomicInteger();
        private final java.util.concurrent.CountDownLatch destroyEntered = new java.util.concurrent.CountDownLatch(1);
        private final java.util.concurrent.CountDownLatch release = new java.util.concurrent.CountDownLatch(1);
        private boolean alive = true;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException { release.await(); return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException {
            release.await(timeout, unit);
            return !alive;
        }
        @Override public int exitValue() { if (alive) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { destroyCalls.incrementAndGet(); destroyEntered.countDown(); }
        @Override public Process destroyForcibly() { alive = false; return this; }
        @Override public boolean isAlive() { return alive; }
    }

    private static final class ParentHandle implements ProcessHandle {
        private final SurvivingTreeProcess parent;
        private ParentHandle(SurvivingTreeProcess parent) { this.parent = parent; }
        @Override public long pid() { return 44; }
        @Override public java.util.Optional<ProcessHandle> parent() { return java.util.Optional.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> children() { return descendants(); }
        @Override public java.util.stream.Stream<ProcessHandle> descendants() {
            return parent.alive && parent.descendantVisible
                    ? java.util.stream.Stream.of(parent.descendant) : java.util.stream.Stream.empty();
        }
        @Override public Info info() { throw new UnsupportedOperationException(); }
        @Override public java.util.concurrent.CompletableFuture<ProcessHandle> onExit() {
            return new java.util.concurrent.CompletableFuture<>();
        }
        @Override public boolean supportsNormalTermination() { return true; }
        @Override public boolean destroy() { return true; }
        @Override public boolean destroyForcibly() { parent.alive = false; return true; }
        @Override public boolean isAlive() { return parent.alive; }
        @Override public int compareTo(ProcessHandle other) { return Long.compare(pid(), other.pid()); }
    }

    private static final class SurvivingHandle implements ProcessHandle {
        private volatile boolean alive = true;
        private final AtomicInteger forceKillCalls = new AtomicInteger();
        @Override public long pid() { return 45; }
        @Override public java.util.Optional<ProcessHandle> parent() { return java.util.Optional.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> children() { return java.util.stream.Stream.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> descendants() { return java.util.stream.Stream.empty(); }
        @Override public Info info() { throw new UnsupportedOperationException(); }
        @Override public java.util.concurrent.CompletableFuture<ProcessHandle> onExit() {
            return new java.util.concurrent.CompletableFuture<>();
        }
        @Override public boolean supportsNormalTermination() { return true; }
        @Override public boolean destroy() { return true; }
        @Override public boolean destroyForcibly() { forceKillCalls.incrementAndGet(); return true; }
        @Override public boolean isAlive() { return alive; }
        @Override public int compareTo(ProcessHandle other) { return Long.compare(pid(), other.pid()); }
    }
}
