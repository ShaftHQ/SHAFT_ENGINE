package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesktopMobileSetupServiceTest {
    @Test
    void hostPreflightCompletesBeforeStateWriteOrToolMutation(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED,
                new SetupSelection(List.of("simulator_00000000_0000_0000_0000_000000000001")));
        RecordingOperations operations = new RecordingOperations(paths);
        DesktopMobileSetupService service = new DesktopMobileSetupService(paths, plan, operations, false);

        SetupReceipt receipt = service.install(plan, approval(plan));

        assertEquals(List.of("host-preflight", "locked-preflight", "install:" + SetupTarget.NODE,
                "install:" + SetupTarget.APPIUM_SERVER,
                "install:" + SetupTarget.APPIUM_INSPECTOR_PLUGIN,
                "install:" + SetupTarget.APPIUM_XCUITEST_DRIVER,
                "install:" + SetupTarget.XCODE,
                "install:" + SetupTarget.IOS_SIMULATOR), operations.events);
        assertEquals(plan.digest(), receipt.planDigest());
        assertEquals(plan.actions(), receipt.completedActions());
        assertTrue(Files.isRegularFile(paths.receipts().resolve("mobile-ios.json")));
    }

    @Test
    void statusIsReadOnlyAndDegradedWithoutCompatibleReceipt(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        RecordingOperations operations = new RecordingOperations(paths);
        DesktopMobileSetupService service = new DesktopMobileSetupService(paths, plan, operations, false);

        SetupProfileStatus status = service.status();

        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertEquals(plan.actions().size(), status.targets().size());
        assertEquals(plan.actions().stream().map(action -> "status:" + action.target()).toList(),
                operations.events);
        assertFalse(Files.exists(paths.cacheRoot()));
        assertFalse(Files.exists(paths.dataRoot()));
    }

    @Test
    void externalPlansRemainDiagnosticOnly(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        SetupPlan external = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.EXTERNAL, SetupSelection.defaults());
        RecordingOperations operations = new RecordingOperations(paths);
        DesktopMobileSetupService service = new DesktopMobileSetupService(paths, external, operations, false);

        assertThrows(IllegalArgumentException.class, () -> service.install(external, approval(external)));
        assertTrue(operations.events.isEmpty());
        assertFalse(Files.exists(paths.cacheRoot()));
        assertFalse(Files.exists(paths.dataRoot()));
    }

    @Test
    void failedReinstallInvalidatesThePreviousReceiptBeforeMutation(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        RecordingOperations operations = new RecordingOperations(paths);
        DesktopMobileSetupService service = new DesktopMobileSetupService(paths, plan, operations, false);
        service.install(plan, approval(plan));
        assertEquals(SetupReadiness.READY, service.status().readiness());
        operations.failOn = SetupTarget.APPIUM_SERVER;

        SetupExecutionException failure = assertThrows(SetupExecutionException.class,
                () -> service.install(plan, approval(plan)));

        assertEquals(SetupTarget.APPIUM_SERVER, failure.failedAction().target());
        assertFalse(Files.exists(paths.receipts().resolve("mobile-windows.json")));
        assertTrue(Files.isRegularFile(paths.receipts().resolve("mobile-windows.previous.json")));
        assertEquals(SetupReadiness.DEGRADED, service.status().readiness());
    }

    @Test
    void concurrentDesktopMobileProfilesNeverOverlapSharedMutation(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan windowsPlan = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        SetupPlan iosPlan = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED,
                new SetupSelection(List.of("simulator_00000000_0000_0000_0000_000000000001")));
        ConcurrentOperations operations = new ConcurrentOperations();
        DesktopMobileSetupService first = new DesktopMobileSetupService(paths, windowsPlan, operations, false);
        DesktopMobileSetupService second = new DesktopMobileSetupService(paths, iosPlan, operations, false);
        var executor = java.util.concurrent.Executors.newFixedThreadPool(2);
        try {
            var firstResult = executor.submit(() -> first.install(windowsPlan, approval(windowsPlan)));
            var secondResult = executor.submit(() -> second.install(iosPlan, approval(iosPlan)));
            assertTrue(operations.firstMutation.await(5, TimeUnit.SECONDS));
            assertFalse(operations.overlap.await(250, TimeUnit.MILLISECONDS));
            operations.releaseFirst.countDown();

            firstResult.get(5, TimeUnit.SECONDS);
            secondResult.get(5, TimeUnit.SECONDS);
            assertEquals(1, operations.maximumActive.get());
        } finally {
            operations.releaseFirst.countDown();
            executor.shutdownNow();
        }
    }

    @Test
    void offlineInstallWaitsForOnlineMutationBeforeCheckingPartialState(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        RacingOfflineOperations operations = new RacingOfflineOperations(plan.actions().getLast().target());
        DesktopMobileSetupService online = new DesktopMobileSetupService(paths, plan, operations, false);
        DesktopMobileSetupService offline = new DesktopMobileSetupService(paths, plan, operations, true);
        var executor = java.util.concurrent.Executors.newFixedThreadPool(2);
        try {
            var onlineResult = executor.submit(() -> online.install(plan, approval(plan)));
            assertTrue(operations.partialMutation.await(5, TimeUnit.SECONDS));
            var offlineResult = executor.submit(() -> offline.install(plan, approval(plan)));

            assertFalse(operations.offlinePreflight.await(250, TimeUnit.MILLISECONDS));
            operations.releaseOnline.countDown();

            onlineResult.get(5, TimeUnit.SECONDS);
            offlineResult.get(5, TimeUnit.SECONDS);
            assertEquals(1, operations.maximumActive.get());
        } finally {
            operations.releaseOnline.countDown();
            executor.shutdownNow();
        }
    }

    @Test
    void separateJvmOfflineInstallWaitsForTheFilesystemTransaction(@TempDir Path temp) throws Exception {
        Path onlineLog = temp.resolve("online.log");
        Path offlineLog = temp.resolve("offline.log");
        Process online = startCrossProcessFixture("online", temp, onlineLog);
        Process offline = null;
        try {
            waitForPath(temp.resolve("partial"), Duration.ofSeconds(5));
            offline = startCrossProcessFixture("offline", temp, offlineLog);

            waitForPath(temp.resolve("offline-arrived"), Duration.ofSeconds(5));
            assertFalse(offline.waitFor(500, TimeUnit.MILLISECONDS), Files.readString(offlineLog));
            assertFalse(Files.exists(temp.resolve("offline-early")));
            assertFalse(Files.exists(temp.resolve("offline-locked")));
            Files.writeString(temp.resolve("release"), "go");

            assertTrue(online.waitFor(10, TimeUnit.SECONDS), Files.readString(onlineLog));
            assertTrue(offline.waitFor(10, TimeUnit.SECONDS), Files.readString(offlineLog));
            assertEquals(0, online.exitValue(), Files.readString(onlineLog));
            assertEquals(0, offline.exitValue(), Files.readString(offlineLog));
            assertTrue(Files.isRegularFile(temp.resolve("offline-locked")));
        } finally {
            Files.writeString(temp.resolve("release"), "go");
            if (online.isAlive()) online.destroyForcibly();
            if (offline != null && offline.isAlive()) offline.destroyForcibly();
        }
    }

    private static Process startCrossProcessFixture(String mode, Path root, Path log) throws IOException {
        Path java = Path.of(System.getProperty("java.home"), "bin",
                SetupPlatform.current() == SetupPlatform.WINDOWS ? "java.exe" : "java");
        return new ProcessBuilder(java.toString(), "-Xmx64m", "-XX:+UseSerialGC", "-cp",
                System.getProperty("java.class.path"), CrossProcessFixture.class.getName(), mode, root.toString())
                .redirectErrorStream(true).redirectOutput(log.toFile()).start();
    }

    private static void waitForPath(Path path, Duration timeout) throws Exception {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (Files.notExists(path) && System.nanoTime() < deadline) Thread.sleep(Duration.ofMillis(10));
        assertTrue(Files.exists(path), "Timed out waiting for " + path);
    }

    private static SetupApproval approval(SetupPlan plan) {
        return new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
    }

    private static ShaftCachePaths paths(Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static final class RecordingOperations implements DesktopMobileToolchainOperations {
        private final ShaftCachePaths paths;
        private final List<String> events = new ArrayList<>();
        private SetupTarget failOn;

        private RecordingOperations(ShaftCachePaths paths) {
            this.paths = paths;
        }

        @Override
        public void hostPreflight(List<SetupAction> actions) {
            if (events.isEmpty()) {
                assertFalse(Files.exists(paths.state()));
                assertFalse(Files.exists(paths.tools()));
            }
            events.add("host-preflight");
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean offline) {
            assertTrue(Files.isDirectory(paths.state()));
            assertFalse(Files.exists(paths.tools()));
            events.add("locked-preflight");
        }

        @Override
        public void install(SetupAction action) throws IOException {
            events.add("install:" + action.target());
            if (action.target() == failOn) {
                String stem = action.target() == SetupTarget.APPIUM_XCUITEST_DRIVER ? "mobile-ios" : "mobile-windows";
                assertFalse(Files.exists(paths.receipts().resolve(stem + ".json")));
                assertTrue(Files.isRegularFile(paths.receipts().resolve(stem + ".previous.json")));
                throw new IOException("fixture failure");
            }
        }

        @Override
        public SetupStatus status(SetupAction action) {
            events.add("status:" + action.target());
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }
    }

    private static final class ConcurrentOperations implements DesktopMobileToolchainOperations {
        private final AtomicBoolean first = new AtomicBoolean(true);
        private final AtomicInteger active = new AtomicInteger();
        private final AtomicInteger maximumActive = new AtomicInteger();
        private final CountDownLatch hostsReady = new CountDownLatch(2);
        private final CountDownLatch firstMutation = new CountDownLatch(1);
        private final CountDownLatch overlap = new CountDownLatch(1);
        private final CountDownLatch releaseFirst = new CountDownLatch(1);

        @Override
        public void hostPreflight(List<SetupAction> actions) throws IOException {
            hostsReady.countDown();
            try {
                if (!hostsReady.await(5, TimeUnit.SECONDS)) throw new IOException("Concurrent host preflight timed out.");
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException(interrupted);
            }
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean offline) {
            // This fixture records only mutation overlap; the lock assertion is made by maximumActive.
        }

        @Override
        public void install(SetupAction action) throws IOException {
            int current = active.incrementAndGet();
            maximumActive.accumulateAndGet(current, Math::max);
            if (current > 1) overlap.countDown();
            try {
                if (action.target() == SetupTarget.NODE && first.compareAndSet(true, false)) {
                    firstMutation.countDown();
                    if (!releaseFirst.await(5, TimeUnit.SECONDS)) throw new IOException("Mutation gate timed out.");
                }
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException(interrupted);
            } finally {
                active.decrementAndGet();
            }
        }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }
    }

    private static final class RacingOfflineOperations implements DesktopMobileToolchainOperations {
        private final SetupTarget finalTarget;
        private final AtomicBoolean firstOnlineMutation = new AtomicBoolean(true);
        private final AtomicBoolean partial = new AtomicBoolean();
        private final AtomicInteger active = new AtomicInteger();
        private final AtomicInteger maximumActive = new AtomicInteger();
        private final CountDownLatch partialMutation = new CountDownLatch(1);
        private final CountDownLatch offlinePreflight = new CountDownLatch(1);
        private final CountDownLatch releaseOnline = new CountDownLatch(1);

        private RacingOfflineOperations(SetupTarget finalTarget) {
            this.finalTarget = finalTarget;
        }

        @Override
        public void hostPreflight(List<SetupAction> actions) {
            // Host prerequisites are intentionally ready for this offline race fixture.
        }

        @Override
        public void preStatePreflight(List<SetupAction> actions, boolean offline) throws IOException {
            if (!offline) return;
            offlinePreflight.countDown();
            if (partial.get()) throw new IOException("Observed an in-flight partial installation.");
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean offline) { }

        @Override
        public void install(SetupAction action) throws IOException {
            int current = active.incrementAndGet();
            maximumActive.accumulateAndGet(current, Math::max);
            try {
                if (firstOnlineMutation.compareAndSet(true, false)) {
                    partial.set(true);
                    partialMutation.countDown();
                    if (!releaseOnline.await(5, TimeUnit.SECONDS)) throw new IOException("Fixture timed out.");
                }
                if (action.target() == finalTarget) partial.set(false);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException(interrupted);
            } finally {
                active.decrementAndGet();
            }
        }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }
    }

    public static final class CrossProcessFixture {
        public static void main(String[] arguments) throws Exception {
            String mode = arguments[0];
            Path root = Path.of(arguments[1]).toAbsolutePath();
            ShaftCachePaths paths = paths(root);
            SetupPlan plan = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                    SetupMode.MANAGED, SetupSelection.defaults());
            boolean offline = mode.equals("offline");
            new DesktopMobileSetupService(paths, plan, new CrossProcessOperations(root, offline), offline)
                    .install(plan, approval(plan));
        }
    }

    private record CrossProcessOperations(Path root, boolean offline) implements DesktopMobileToolchainOperations {
        @Override
        public void hostPreflight(List<SetupAction> actions) throws IOException {
            if (offline) Files.writeString(root.resolve("offline-arrived"), "ready");
        }

        @Override
        public void preStatePreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
            if (!requireOffline) return;
            Files.writeString(root.resolve("offline-early"), "observed");
            if (Files.exists(root.resolve("partial"))) throw new IOException("Observed partial state too early.");
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
            if (!requireOffline) return;
            if (Files.exists(root.resolve("partial"))) throw new IOException("File lock did not serialize setup.");
            Files.writeString(root.resolve("offline-locked"), "ready");
        }

        @Override
        public void install(SetupAction action) throws IOException {
            if (offline || action.target() != SetupTarget.NODE) return;
            Path partial = root.resolve("partial");
            Files.writeString(partial, "installing");
            long deadline = System.nanoTime() + Duration.ofSeconds(10).toNanos();
            while (Files.notExists(root.resolve("release")) && System.nanoTime() < deadline) {
                try {
                    Thread.sleep(Duration.ofMillis(10));
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException(interrupted);
                }
            }
            if (Files.notExists(root.resolve("release"))) throw new IOException("Fixture release timed out.");
            Files.delete(partial);
        }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }
    }
}
