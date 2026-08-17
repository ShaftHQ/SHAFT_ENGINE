package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesktopMobileLifecycleServiceTest {
    private static final String UDID = "00000000-0000-0000-0000-000000000001";

    @Test
    void missingReceiptStartsNoProcess(@TempDir Path temp) {
        Fixture fixture = uninstalled(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());

        assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));
        assertTrue(runtime.commands.isEmpty());
        assertTrue(devices.events.isEmpty());
    }

    @Test
    void existingSimulatorSelectionStartsNoProcess(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, SetupSelection.defaults());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("exact Simulator UDID"));
        assertTrue(runtime.commands.isEmpty());
        assertTrue(devices.events.isEmpty());
    }

    @Test
    void occupiedAppiumPortStartsNoProcess(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());

        try (ServerSocket occupied = new ServerSocket()) {
            occupied.bind(new InetSocketAddress("127.0.0.1", 4723));

            IOException failure = assertThrows(IOException.class,
                    () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

            assertTrue(failure.getMessage().contains("4723"));
            assertTrue(runtime.commands.isEmpty());
            assertTrue(devices.events.isEmpty());
        }
    }

    @Test
    void iosStartBootsShutdownSimulatorThenOnlyAppiumAndCloseStopsBoth(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        RecordingHealth health = new RecordingHealth();
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, health);

        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(1, runtime.commands.size());
        assertTrue(runtime.commands.getFirst().containsAll(List.of("--address", "127.0.0.1", "--port", "4723")));
        assertFalse(runtime.commands.getFirst().contains("WinAppDriver"));
        assertEquals(List.of("boot:" + UDID), devices.events);
        assertEquals(List.of("simulator:" + UDID, "appium:http://127.0.0.1:4723/"), health.events);
        assertEquals(URI.create("http://127.0.0.1:4723/"), environment.endpoint().orElseThrow());
        assertEquals(UDID, environment.connectionProperties().get("ios.simulator.udid"));
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("mobile-ios-runtime.json")));

        environment.close();

        assertEquals(List.of("appium"), runtime.stopped);
        assertEquals(List.of("boot:" + UDID, "shutdown:" + UDID), devices.events);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-ios-runtime.json")));
    }

    @Test
    void alreadyBootedSimulatorIsPreservedOnClose(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.BOOTED);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());

        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        environment.close();

        assertEquals(List.of("appium"), runtime.stopped);
        assertTrue(devices.events.isEmpty());
        assertEquals(DesktopMobileDeviceController.SimulatorState.BOOTED, devices.state);
    }

    @Test
    void windowsStartNeverTouchesWinAppDriver(@TempDir Path temp) throws Exception {
        Fixture fixture = installedWindows(temp);
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.MISSING);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());

        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(1, runtime.commands.size());
        assertTrue(runtime.commands.getFirst().containsAll(List.of("--address", "127.0.0.1", "--port", "4723")));
        assertTrue(devices.events.isEmpty());
        assertFalse(environment.connectionProperties().containsKey("ios.simulator.udid"));

        environment.close();

        assertEquals(List.of("appium"), runtime.stopped);
        assertTrue(devices.events.isEmpty());
    }

    @Test
    void leaseRoundTripReusesAppiumAndStopsOnlyAfterFinalRelease(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService first = lifecycle(fixture, runtime, devices, new RecordingHealth());
        ManagedEnvironment firstEnv = first.start(fixture.plan(), fixture.approval(), fixture.options());
        DesktopMobileLifecycleService second = lifecycle(fixture, runtime, devices, new RecordingHealth());

        ManagedEnvironment secondEnv = second.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(1, runtime.commands.size());
        firstEnv.close();
        assertTrue(runtime.stopped.isEmpty());
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("mobile-ios-runtime.json")));

        secondEnv.close();

        assertEquals(List.of("appium"), runtime.stopped);
        assertEquals(List.of("boot:" + UDID, "shutdown:" + UDID), devices.events);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-ios-runtime.json")));
    }

    @Test
    void fullyStaleLeaseIsReplacedButPartialIosLeaseIsRejectedWithoutShutdown(@TempDir Path temp)
            throws Exception {
        Fixture stale = installed(temp.resolve("stale"), iosSelection());
        RecordingRuntime staleRuntime = new RecordingRuntime();
        RecordingDevices staleDevices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService staleLifecycle = lifecycle(stale, staleRuntime, staleDevices,
                new RecordingHealth());
        staleLifecycle.start(stale.plan(), stale.approval(), stale.options());
        staleRuntime.setAllAlive(false);
        staleDevices.state = DesktopMobileDeviceController.SimulatorState.SHUTDOWN;

        ManagedEnvironment replacement = staleLifecycle.start(stale.plan(), stale.approval(), stale.options());
        assertEquals(2, staleRuntime.commands.size());
        replacement.close();

        Fixture partial = installed(temp.resolve("partial"), iosSelection());
        RecordingRuntime partialRuntime = new RecordingRuntime();
        RecordingDevices partialDevices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService partialLifecycle = lifecycle(partial, partialRuntime, partialDevices,
                new RecordingHealth());
        ManagedEnvironment started = partialLifecycle.start(partial.plan(), partial.approval(), partial.options());
        partialRuntime.setAllAlive(false);
        partialDevices.state = DesktopMobileDeviceController.SimulatorState.BOOTED;

        IOException failure = assertThrows(IOException.class,
                () -> partialLifecycle.start(partial.plan(), partial.approval(), partial.options()));

        assertTrue(failure.getMessage().contains("partially alive"));
        assertEquals(1, partialRuntime.commands.size());
        assertFalse(partialDevices.events.contains("shutdown:" + UDID));
        started.close();
    }

    @Test
    void appiumReadinessFailureCleansStartedSimulatorAndRetainsLogs(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        RecordingHealth health = new RecordingHealth();
        health.failAppium = true;
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, health);

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("Appium readiness"));
        assertEquals(List.of("appium"), runtime.stopped);
        assertEquals(List.of("boot:" + UDID, "shutdown:" + UDID), devices.events);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-ios-runtime.json")));
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("logs/appium-ios.log")));
    }

    @Test
    void simulatorReadinessFailureAfterBootShutsDownOnlyTheShaftBootedDevice(@TempDir Path temp)
            throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        RecordingHealth health = new RecordingHealth();
        health.failSimulator = true;
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, health);

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("Simulator readiness"));
        assertTrue(runtime.commands.isEmpty());
        assertEquals(List.of("boot:" + UDID, "shutdown:" + UDID), devices.events);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-ios-runtime.json")));
    }

    @Test
    void explicitStopEndsOwnedAppiumAndShaftBootedSimulator(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());
        lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());

        assertTrue(lifecycle.stop(Duration.ofSeconds(5)));

        assertEquals(List.of("appium"), runtime.stopped);
        assertEquals(List.of("boot:" + UDID, "shutdown:" + UDID), devices.events);
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-ios-runtime.json")));
        assertFalse(lifecycle.stop(Duration.ofSeconds(1)));
    }

    @Test
    void unknownProcessLeaseIsClearedWithoutKillingAnything(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());
        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        runtime.setAllAlive(false);
        devices.state = DesktopMobileDeviceController.SimulatorState.SHUTDOWN;
        Files.writeString(fixture.paths().state().resolve("mobile-ios-runtime.json"),
                Files.readString(fixture.paths().state().resolve("mobile-ios-runtime.json"))
                        .replace("\"pid\" : 200", "\"pid\" : 99999"));

        assertFalse(lifecycle.stop(Duration.ofSeconds(1)));

        assertEquals(List.of(), runtime.stopped);
        assertFalse(devices.events.contains("shutdown:" + UDID));
        assertFalse(Files.exists(fixture.paths().state().resolve("mobile-ios-runtime.json")));
        environment.close();
    }

    @Test
    void commandIdentityDriftRefusesToReuseOrStopTheSurvivor(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        RecordingDevices devices = new RecordingDevices(DesktopMobileDeviceController.SimulatorState.SHUTDOWN);
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime, devices, new RecordingHealth());
        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        runtime.processes.getFirst().identity = "foreign";

        IOException reuse = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));
        IOException stop = assertThrows(IOException.class, () -> lifecycle.stop(Duration.ofSeconds(1)));

        assertTrue(reuse.getMessage().contains("does not match"));
        assertTrue(stop.getMessage().contains("does not match"));
        assertEquals(List.of(), runtime.stopped);
        assertFalse(devices.events.contains("shutdown:" + UDID));
        runtime.processes.getFirst().identity = "appium";
        environment.close();
    }

    @Test
    void logsReadOwnedAppiumFileWithoutFollowingLinks(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp, iosSelection());
        RecordingRuntime runtime = new RecordingRuntime();
        DesktopMobileLifecycleService lifecycle = lifecycle(fixture, runtime,
                new RecordingDevices(DesktopMobileDeviceController.SimulatorState.BOOTED), new RecordingHealth());
        ManagedEnvironment environment = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());

        assertTrue(lifecycle.logs().contains("appium log"));

        Path log = fixture.paths().state().resolve("logs/appium-ios.log");
        Path linked = fixture.paths().state().resolve("logs/appium-ios.link");
        try {
            Files.deleteIfExists(log);
            Files.writeString(linked, "secret");
            Files.createSymbolicLink(log, linked);
            assertThrows(IOException.class, lifecycle::logs);
        } catch (UnsupportedOperationException | IOException unsupported) {
            org.junit.jupiter.api.Assumptions.assumeTrue(Files.isSymbolicLink(log),
                    "Symlink proof unavailable: " + unsupported.getMessage());
        }

        environment.close();
        if (!Files.isSymbolicLink(log)) assertEquals("", lifecycle.logs());
    }

    private static DesktopMobileLifecycleService lifecycle(Fixture fixture, RecordingRuntime runtime,
                                                           RecordingDevices devices, RecordingHealth health) {
        return new DesktopMobileLifecycleService(fixture.paths(), fixture.plan(), fixture.operations(), runtime,
                devices, health);
    }

    private static Fixture installed(Path temp, SetupSelection selection) throws Exception {
        Fixture fixture = uninstalled(temp, selection);
        new DesktopMobileSetupService(fixture.paths(), fixture.plan(), fixture.operations(), false)
                .install(fixture.plan(), fixture.approval());
        return fixture;
    }

    private static Fixture installedWindows(Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        ReadyOperations operations = new ReadyOperations();
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_WINDOWS, paths)
                .withMode(SetupMode.MANAGED).withTimeouts(Duration.ofSeconds(5), Duration.ofSeconds(5));
        new DesktopMobileSetupService(paths, plan, operations, false).install(plan, approval(plan));
        return new Fixture(paths, plan, approval(plan), options, operations);
    }

    private static Fixture uninstalled(Path temp, SetupSelection selection) {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, selection);
        ReadyOperations operations = new ReadyOperations();
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_IOS, paths)
                .withMode(SetupMode.MANAGED).withTimeouts(Duration.ofSeconds(5), Duration.ofSeconds(5));
        return new Fixture(paths, plan, approval(plan), options, operations);
    }

    private static SetupSelection iosSelection() {
        return new SetupSelection(List.of("simulator_00000000_0000_0000_0000_000000000001"));
    }

    private static SetupApproval approval(SetupPlan plan) {
        return new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private record Fixture(ShaftCachePaths paths, SetupPlan plan, SetupApproval approval, SetupOptions options,
                           ReadyOperations operations) { }

    private static final class ReadyOperations implements DesktopMobileToolchainOperations {
        @Override public void hostPreflight(List<SetupAction> actions) { }
        @Override public void lockedPreflight(List<SetupAction> actions, boolean offline) { }
        @Override public void install(SetupAction action) { }
        @Override public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "ready");
        }
    }

    private static final class RecordingRuntime implements AndroidRuntimeController {
        private final List<List<String>> commands = new ArrayList<>();
        private final List<String> stopped = new ArrayList<>();
        private final List<FakeProcess> processes = new ArrayList<>();
        private long nextPid = 200;

        @Override
        public AndroidOwnedProcess start(String role, List<String> command, Path workingDirectory,
                                         Map<String, String> environment, Set<String> removedEnvironment,
                                         Path log) throws IOException {
            commands.add(List.copyOf(command));
            Files.createDirectories(log.getParent());
            Files.writeString(log, role + " log");
            FakeProcess process = new FakeProcess(role, nextPid++, stopped);
            processes.add(process);
            return process;
        }

        @Override
        public Optional<AndroidOwnedProcess> find(long pid, Instant startInstant, String commandIdentity)
                throws IOException {
            for (FakeProcess process : processes) {
                if (process.pid() != pid) continue;
                if (!process.isAlive()) return Optional.empty();
                if (!process.startInstant().equals(startInstant)
                        || !process.commandIdentity().equals(commandIdentity)) {
                    throw new IOException("Live process identity does not match the SHAFT runtime lease: " + pid);
                }
                return Optional.of(process);
            }
            return Optional.empty();
        }

        private void setAllAlive(boolean alive) {
            processes.forEach(process -> process.alive = alive);
        }
    }

    private static final class FakeProcess implements AndroidOwnedProcess {
        private final String role;
        private final long pid;
        private final List<String> stopped;
        private final Instant start = Instant.ofEpochSecond(200);
        private String identity;
        private boolean alive = true;

        private FakeProcess(String role, long pid, List<String> stopped) {
            this.role = role;
            this.pid = pid;
            this.stopped = stopped;
            this.identity = role;
        }

        @Override public long pid() { return pid; }
        @Override public Instant startInstant() { return start; }
        @Override public String commandIdentity() { return identity; }
        @Override public boolean isAlive() { return alive; }

        @Override
        public void stop(Duration timeout) {
            if (alive) {
                alive = false;
                stopped.add(role);
            }
        }
    }

    private static final class RecordingDevices implements DesktopMobileDeviceController {
        private final List<String> events = new ArrayList<>();
        private SimulatorState state;

        private RecordingDevices(SimulatorState state) {
            this.state = state;
        }

        @Override
        public SimulatorState simulatorState(String udid) {
            return state;
        }

        @Override
        public void bootSimulator(String udid) {
            events.add("boot:" + udid);
            state = SimulatorState.BOOTED;
        }

        @Override
        public void shutdownSimulator(String udid) {
            events.add("shutdown:" + udid);
            state = SimulatorState.SHUTDOWN;
        }
    }

    private static final class RecordingHealth implements DesktopMobileRuntimeHealth {
        private final List<String> events = new ArrayList<>();
        private boolean failAppium;
        private boolean failSimulator;

        @Override
        public void awaitAppium(URI endpoint, Duration timeout) throws IOException {
            events.add("appium:" + endpoint);
            if (failAppium) throw new IOException("Appium readiness failed");
        }

        @Override
        public void awaitSimulator(String udid, Duration timeout) throws IOException {
            events.add("simulator:" + udid);
            if (failSimulator) throw new IOException("Simulator readiness failed");
        }
    }
}
