package com.shaft.driver;

import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Collections;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.assertTrue;

/**
 * Covers {@code SHAFT.Auth}'s cache-hit/cache-miss/locking decision logic in {@code setup(name, flow)}
 * without launching a real browser: {@link SHAFT.Auth#flowRunner(SHAFT.Auth.FlowRunner)} (package-private) is stubbed so the
 * real driver-lifecycle interaction (create driver, run flow, save storage state, quit driver) never
 * runs.
 */
@Test(singleThreaded = true)
public class AuthUnitTest {
    private final String savedAuthCache = SHAFT.Properties.paths.authCache();
    private final SHAFT.Auth.FlowRunner savedFlowRunner = SHAFT.Auth.flowRunner();
    private Path cacheDir;

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws IOException {
        SHAFT.Properties.paths.set().authCache(savedAuthCache);
        SHAFT.Auth.flowRunner(savedFlowRunner);
        if (cacheDir != null && Files.exists(cacheDir)) {
            try (var walk = Files.walk(cacheDir)) {
                walk.sorted(Collections.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (IOException ignored) {
                        // best-effort cleanup
                    }
                });
            }
        }
    }

    private String useTempAuthCacheDir() throws IOException {
        cacheDir = Files.createTempDirectory("shaft-auth-cache-unit-test");
        SHAFT.Properties.paths.set().authCache(cacheDir.toString());
        return cacheDir.toString();
    }

    // ---- stateFile(name) ----

    @Test
    public void stateFileShouldRejectNullName() {
        assertThrows(NullPointerException.class, () -> SHAFT.Auth.stateFile(null));
    }

    @Test
    public void stateFileShouldRejectBlankName() {
        assertThrows(IllegalArgumentException.class, () -> SHAFT.Auth.stateFile("   "));
    }

    @Test
    public void stateFileShouldReturnJsonFileUnderConfiguredAuthCacheDirectory() throws IOException {
        String dir = useTempAuthCacheDir();
        String path = SHAFT.Auth.stateFile("standardUser");
        assertEquals(path, new java.io.File(dir, "standardUser.json").getPath());
    }

    // ---- setup(name, flow): validation ----

    @Test
    public void setupShouldRejectNullFlow() throws IOException {
        useTempAuthCacheDir();
        assertThrows(NullPointerException.class, () -> SHAFT.Auth.setup("someUser", null));
    }

    // ---- setup(name, flow): cache miss ----

    @Test
    public void setupShouldRunFlowRunnerOnCacheMissAndReturnStateFilePath() throws IOException {
        useTempAuthCacheDir();
        AtomicInteger invocationCount = new AtomicInteger();
        List<String> pathsSeenByRunner = new CopyOnWriteArrayList<>();
        Consumer<SHAFT.GUI.WebDriver> flow = driver -> { };
        SHAFT.Auth.flowRunner((passedFlow, passedPath) -> {
            invocationCount.incrementAndGet();
            pathsSeenByRunner.add(passedPath);
            Assert.assertSame(passedFlow, flow, "flowRunner should receive the exact flow instance passed to setup().");
            writeFile(passedPath);
        });

        String result = SHAFT.Auth.setup("missUser", flow);

        assertEquals(result, SHAFT.Auth.stateFile("missUser"));
        assertEquals(invocationCount.get(), 1, "flowRunner should run exactly once on a cache miss.");
        assertEquals(pathsSeenByRunner.get(0), SHAFT.Auth.stateFile("missUser"));
    }

    // ---- setup(name, flow): cache hit ----

    @Test
    public void setupShouldSkipFlowRunnerOnCacheHit() throws IOException {
        useTempAuthCacheDir();
        String name = "hitUser";
        writeFile(SHAFT.Auth.stateFile(name));
        AtomicInteger invocationCount = new AtomicInteger();
        SHAFT.Auth.flowRunner((flow, path) -> invocationCount.incrementAndGet());

        String result = SHAFT.Auth.setup(name, driver -> { });

        assertEquals(result, SHAFT.Auth.stateFile(name));
        assertEquals(invocationCount.get(), 0, "flowRunner must not run when the cache file already exists.");
    }

    // ---- setup(name, flow): flowRunner exceptions do not leave a stuck lock ----

    @Test
    public void setupShouldPropagateFlowRunnerFailureAndReleaseTheLockForARetry() throws IOException {
        useTempAuthCacheDir();
        String name = "flakyUser";
        SHAFT.Auth.flowRunner((flow, path) -> {
            throw new RuntimeException("login failed");
        });

        assertThrows(RuntimeException.class, () -> SHAFT.Auth.setup(name, driver -> { }));
        assertFalse(Files.exists(Path.of(SHAFT.Auth.stateFile(name))), "A failed flow must not leave a cache file behind.");

        AtomicInteger secondAttemptInvocations = new AtomicInteger();
        SHAFT.Auth.flowRunner((flow, path) -> {
            secondAttemptInvocations.incrementAndGet();
            writeFile(path);
        });
        String result = SHAFT.Auth.setup(name, driver -> { });

        assertEquals(result, SHAFT.Auth.stateFile(name));
        assertEquals(secondAttemptInvocations.get(), 1, "The per-name lock must be released after the first failure so a retry can proceed.");
    }

    // ---- setup(name, flow): concurrent same-name calls are serialized ----

    @Test
    public void concurrentSetupCallsForTheSameNameShouldRunFlowRunnerOnlyOnce() throws Exception {
        useTempAuthCacheDir();
        String name = "concurrentUser";
        int threadCount = 8;
        AtomicInteger invocationCount = new AtomicInteger();
        CountDownLatch readyLatch = new CountDownLatch(threadCount);
        CountDownLatch startLatch = new CountDownLatch(1);
        SHAFT.Auth.flowRunner((flow, path) -> {
            invocationCount.incrementAndGet();
            // Hold the per-name lock briefly so other threads pile up waiting on it,
            // proving that setup() serializes concurrent calls for the same name.
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            writeFile(path);
        });

        String authCacheDir = SHAFT.Properties.paths.authCache();
        ExecutorService pool = Executors.newFixedThreadPool(threadCount);
        try {
            List<Future_> futures = new CopyOnWriteArrayList<>();
            for (int i = 0; i < threadCount; i++) {
                pool.submit(() -> {
                    // SHAFT.Properties overrides are thread-local (mirrors real parallel-suite
                    // behavior), so each worker thread must set its own override before calling setup().
                    SHAFT.Properties.paths.set().authCache(authCacheDir);
                    readyLatch.countDown();
                    try {
                        startLatch.await();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                    String result = SHAFT.Auth.setup(name, driver -> { });
                    futures.add(new Future_(result));
                });
            }
            assertTrue(readyLatch.await(5, TimeUnit.SECONDS), "All worker threads should reach the start line.");
            startLatch.countDown();
            pool.shutdown();
            assertTrue(pool.awaitTermination(10, TimeUnit.SECONDS), "All setup() calls should complete.");

            assertEquals(invocationCount.get(), 1, "flowRunner must run exactly once across all concurrent callers for the same name.");
            assertEquals(futures.size(), threadCount);
            for (Future_ future : futures) {
                assertEquals(future.value, SHAFT.Auth.stateFile(name));
            }
        } finally {
            pool.shutdownNow();
        }
    }

    private static void writeFile(String path) {
        try {
            Path file = Path.of(path);
            Files.createDirectories(file.getParent());
            Files.writeString(file, "{}");
        } catch (IOException e) {
            throw new UncheckedIOExceptionForTest(e);
        }
    }

    private static final class UncheckedIOExceptionForTest extends RuntimeException {
        UncheckedIOExceptionForTest(IOException cause) {
            super(cause);
        }
    }

    /** Minimal holder to collect a result string from worker threads without importing extra utilities. */
    private static final class Future_ {
        final String value;

        Future_(String value) {
            this.value = value;
        }
    }
}
