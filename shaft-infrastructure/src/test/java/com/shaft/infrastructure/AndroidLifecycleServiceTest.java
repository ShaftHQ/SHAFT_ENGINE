package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AndroidLifecycleServiceTest {
    @Test
    void linkedRuntimeStateAncestorStartsNoProcess(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        Path state = fixture.paths().state();
        Path actualState = temp.resolve("actual-state");
        if (Files.exists(state)) Files.move(state, actualState);
        else Files.createDirectories(actualState);
        try {
            Files.createSymbolicLink(state, actualState);
        } catch (UnsupportedOperationException | IOException unsupported) {
            org.junit.jupiter.api.Assumptions.abort("Directory links unavailable: " + unsupported.getMessage());
        }
        RecordingRuntime runtime = new RecordingRuntime();
        AndroidLifecycleService lifecycle = new AndroidLifecycleService(fixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, fixture.request(), fixture.operations(), runtime, new RecordingHealth());

        assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));
        assertTrue(runtime.commands.isEmpty());
    }

    @Test
    void fullyStaleLeaseIsReplacedButPartialLeaseIsRejectedWithoutKillingSurvivor(@TempDir Path temp)
            throws Exception {
        Fixture staleFixture = installed(temp.resolve("stale"));
        RecordingRuntime staleRuntime = new RecordingRuntime();
        AndroidLifecycleService staleLifecycle = new AndroidLifecycleService(staleFixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, staleFixture.request(), staleFixture.operations(), staleRuntime,
                new RecordingHealth());
        staleLifecycle.start(staleFixture.plan(), staleFixture.approval(), staleFixture.options());
        staleRuntime.setAllAlive(false);

        ManagedEnvironment replacement = staleLifecycle.start(staleFixture.plan(), staleFixture.approval(),
                staleFixture.options());

        assertEquals(4, staleRuntime.commands.size());
        replacement.close();

        Fixture partialFixture = installed(temp.resolve("partial"));
        RecordingRuntime partialRuntime = new RecordingRuntime();
        AndroidLifecycleService partialLifecycle = new AndroidLifecycleService(partialFixture.paths(),
                SetupPlatform.LINUX, SetupArchitecture.X64, partialFixture.request(), partialFixture.operations(),
                partialRuntime, new RecordingHealth());
        ManagedEnvironment partial = partialLifecycle.start(partialFixture.plan(), partialFixture.approval(),
                partialFixture.options());
        partialRuntime.setAlive("appium", false);

        IOException failure = assertThrows(IOException.class, () -> partialLifecycle.start(partialFixture.plan(),
                partialFixture.approval(), partialFixture.options()));

        assertTrue(failure.getMessage().contains("partially alive"));
        assertEquals(2, partialRuntime.commands.size());
        assertTrue(partialRuntime.process("emulator").isAlive());
        assertTrue(partialRuntime.stopped.isEmpty());
        partialRuntime.setAlive("appium", true);
        partial.close();
    }

    @Test
    void shutdownSharesOneDeadlineAcrossAppiumAndEmulator(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingRuntime runtime = new RecordingRuntime();
        runtime.stopDelays.put("appium", Duration.ofMillis(100));
        AndroidLifecycleService lifecycle = new AndroidLifecycleService(fixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, fixture.request(), fixture.operations(), runtime, new RecordingHealth());
        SetupOptions options = fixture.options().withTimeouts(Duration.ofSeconds(5), Duration.ofMillis(500));
        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), options);

        environment.close();

        assertTrue(runtime.stopTimeouts.get("emulator").compareTo(runtime.stopTimeouts.get("appium")) < 0);
    }

    @Test
    void leaseRoundTripReusesProcessesAndStopsOnlyAfterFinalRelease(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingHealth health = new RecordingHealth();
        AndroidLifecycleService firstLifecycle = new AndroidLifecycleService(fixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, fixture.request(), fixture.operations(), runtime, health);
        ManagedEnvironment first = firstLifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        AndroidLifecycleService secondLifecycle = new AndroidLifecycleService(fixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, fixture.request(), fixture.operations(), runtime, health);

        ManagedEnvironment second = secondLifecycle.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(2, runtime.commands.size());
        assertEquals(4, health.events.size());
        first.close();
        assertTrue(runtime.stopped.isEmpty());
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("mobile-android-runtime.json")));

        second.close();

        assertEquals(List.of("appium", "emulator"), runtime.stopped);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-android-runtime.json")));
    }

    @ParameterizedTest
    @ValueSource(ints = {5554, 5555})
    void occupiedEmulatorPortStartsNoProcess(int occupiedPort, @TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingRuntime runtime = new RecordingRuntime();
        AndroidLifecycleService lifecycle = new AndroidLifecycleService(fixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, fixture.request(), fixture.operations(), runtime, new RecordingHealth());

        try (ServerSocket occupied = new ServerSocket()) {
            occupied.bind(new InetSocketAddress("127.0.0.1", occupiedPort));

            IOException failure = assertThrows(IOException.class,
                    () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

            assertTrue(failure.getMessage().contains(Integer.toString(occupiedPort)));
            assertTrue(runtime.commands.isEmpty());
        }
    }

    @Test
    void startsFullyBootedEmulatorBeforeLocalAppiumAndCloseStopsOwnedTree(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingHealth health = new RecordingHealth();
        AndroidLifecycleService lifecycle = new AndroidLifecycleService(fixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, fixture.request(), fixture.operations(), runtime, health);

        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(2, runtime.commands.size());
        assertTrue(runtime.commands.get(0).contains("-avd"));
        assertTrue(runtime.commands.get(0).contains(fixture.request().avdName()));
        assertTrue(runtime.commands.get(1).containsAll(List.of("--address", "127.0.0.1", "--port", "4823")));
        assertFalse(runtime.commands.get(1).contains("--relaxed-security"));
        assertEquals(List.of("emulator:emulator-5554", "appium:http://127.0.0.1:4823/"), health.events);
        assertEquals(URI.create("http://127.0.0.1:4823/"), environment.endpoint().orElseThrow());
        assertEquals(fixture.plan().digest(), environment.receipt().planDigest());
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("mobile-android-runtime.json")));

        environment.close();

        assertEquals(List.of("appium", "emulator"), runtime.stopped);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-android-runtime.json")));
    }

    @Test
    void appiumReadinessFailureCleansOnlyProcessesStartedByThisCallAndRetainsLogs(@TempDir Path temp)
            throws Exception {
        Fixture fixture = installed(temp);
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingHealth health = new RecordingHealth();
        health.failAppium = true;
        AndroidLifecycleService lifecycle = new AndroidLifecycleService(fixture.paths(), SetupPlatform.LINUX,
                SetupArchitecture.X64, fixture.request(), fixture.operations(), runtime, health);

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("Appium readiness"));
        assertEquals(List.of("appium", "emulator"), runtime.stopped);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-android-runtime.json")));
        assertTrue(Files.isDirectory(fixture.paths().state().resolve("logs")));
    }

    @Test
    void missingCompatibleInstallReceiptStartsNoProcess(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        AndroidSetupRequest request = request();
        ReadyOperations operations = new ReadyOperations();
        RecordingRuntime runtime = new RecordingRuntime();
        AndroidLifecycleService lifecycle = new AndroidLifecycleService(paths, SetupPlatform.LINUX,
                SetupArchitecture.X64, request, operations, runtime, new RecordingHealth());
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, request);
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_ANDROID, paths)
                .withMode(SetupMode.MANAGED);

        assertThrows(IOException.class, () -> lifecycle.start(plan, approval(plan), options));
        assertTrue(runtime.commands.isEmpty());
        assertFalse(Files.exists(paths.state()));
    }

    private static Fixture installed(Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AndroidSetupRequest request = request();
        ReadyOperations operations = new ReadyOperations();
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, request);
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_ANDROID, paths)
                .withMode(SetupMode.MANAGED).withTimeouts(Duration.ofSeconds(5), Duration.ofSeconds(5));
        new AndroidSetupService(paths, SetupPlatform.LINUX, SetupArchitecture.X64, request, operations, false)
                .install(plan, approval(plan));
        return new Fixture(paths, request, operations, plan, approval(plan), options);
    }

    private static SetupApproval approval(SetupPlan plan) {
        return new SetupApproval(plan.digest(), Instant.EPOCH, Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE));
    }

    private static AndroidSetupRequest request() {
        return new AndroidSetupRequest(36, "pixel_8", "google_apis", "x86_64", "runtime_avd",
                4096, 2, 4823);
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private record Fixture(ShaftCachePaths paths, AndroidSetupRequest request, ReadyOperations operations,
                           SetupPlan plan, SetupApproval approval, SetupOptions options) { }

    private static final class ReadyOperations implements AndroidToolchainOperations {
        @Override public void preflight(List<SetupAction> actions, boolean offline) { }
        @Override public void install(SetupAction action) { }
        @Override public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "ready");
        }
    }

    private static final class RecordingRuntime implements AndroidRuntimeController {
        private final List<List<String>> commands = new ArrayList<>();
        private final List<String> stopped = new ArrayList<>();
        private final List<FakeProcess> processes = new ArrayList<>();
        private final Map<String, Duration> stopDelays = new LinkedHashMap<>();
        private final Map<String, Duration> stopTimeouts = new LinkedHashMap<>();
        private long nextPid = 100;

        @Override
        public AndroidOwnedProcess start(String role, List<String> command, Path workingDirectory,
                                         Map<String, String> environment, Set<String> removedEnvironment,
                                         Path log) throws IOException {
            commands.add(List.copyOf(command));
            Files.createDirectories(log.getParent());
            Files.writeString(log, role + " log");
            FakeProcess process = new FakeProcess(role, nextPid++, stopped, stopDelays, stopTimeouts);
            processes.add(process);
            return process;
        }

        @Override
        public Optional<AndroidOwnedProcess> find(long pid, Instant startInstant, String commandIdentity) {
            return processes.stream()
                    .filter(process -> process.isAlive() && process.pid() == pid
                            && process.startInstant().equals(startInstant)
                            && process.commandIdentity().equals(commandIdentity))
                    .map(process -> (AndroidOwnedProcess) process)
                    .findFirst();
        }

        private FakeProcess process(String role) {
            return processes.stream().filter(process -> process.role.equals(role)).reduce((first, second) -> second)
                    .orElseThrow();
        }

        private void setAlive(String role, boolean alive) {
            process(role).alive = alive;
        }

        private void setAllAlive(boolean alive) {
            processes.forEach(process -> process.alive = alive);
        }
    }

    private static final class FakeProcess implements AndroidOwnedProcess {
        private final String role;
        private final long pid;
        private final List<String> stopped;
        private final Map<String, Duration> stopDelays;
        private final Map<String, Duration> stopTimeouts;
        private final Instant start = Instant.ofEpochSecond(100);
        private boolean alive = true;

        private FakeProcess(String role, long pid, List<String> stopped, Map<String, Duration> stopDelays,
                            Map<String, Duration> stopTimeouts) {
            this.role = role;
            this.pid = pid;
            this.stopped = stopped;
            this.stopDelays = stopDelays;
            this.stopTimeouts = stopTimeouts;
        }

        @Override public long pid() { return pid; }
        @Override public Instant startInstant() { return start; }
        @Override public String commandIdentity() { return role; }
        @Override public boolean isAlive() { return alive; }
        @Override
        public void stop(Duration timeout) throws IOException {
            stopTimeouts.put(role, timeout);
            Duration delay = stopDelays.getOrDefault(role, Duration.ZERO);
            try {
                Thread.sleep(delay);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted test process stop.", interrupted);
            }
            if (alive) {
                alive = false;
                stopped.add(role);
            }
        }
    }

    private static final class RecordingHealth implements AndroidRuntimeHealth {
        private final List<String> events = new ArrayList<>();
        private boolean failAppium;

        @Override
        public void awaitEmulator(String serial, AndroidRuntimeLayout layout, Map<String, String> environment,
                                  Duration timeout) {
            events.add("emulator:" + serial);
        }

        @Override
        public void awaitAppium(URI endpoint, Duration timeout) throws IOException {
            events.add("appium:" + endpoint);
            if (failAppium) throw new IOException("Appium readiness failed");
        }
    }
}
