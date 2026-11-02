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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BrowserStackLocalLifecycleServiceTest {
    @Test
    void missingReceiptStartsNoProcess(@TempDir Path temp) {
        Fixture fixture = uninstalled(temp);
        RecordingOperations operations = new RecordingOperations();
        BrowserStackLocalLifecycleService lifecycle = new BrowserStackLocalLifecycleService(
                fixture.paths(), fixture.plan(), operations, () -> "test-key");

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("receipt"));
        assertTrue(operations.starts.isEmpty());
    }

    @Test
    void missingAccessKeyStartsNoProcess(@TempDir Path temp) {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        BrowserStackLocalLifecycleService lifecycle = new BrowserStackLocalLifecycleService(
                fixture.paths(), fixture.plan(), operations, () -> "");

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("BROWSERSTACK_ACCESS_KEY"));
        assertTrue(operations.starts.isEmpty());
    }

    @Test
    void startReusesThenStopsOnlyTheOwnedPid(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        BrowserStackLocalLifecycleService lifecycle = new BrowserStackLocalLifecycleService(
                fixture.paths(), fixture.plan(), operations, () -> "test-key");

        ManagedEnvironment first = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        ManagedEnvironment second = lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(1, operations.starts.size());
        assertEquals(Set.of("local", "binary"), first.connectionProperties().keySet());
        assertFalse(first.connectionProperties().values().toString().contains("test-key"));
        assertFalse(Files.readString(fixture.paths().state().resolve("browserstack-local-runtime.json"))
                .contains("test-key"));

        first.close();
        assertTrue(operations.stops.isEmpty());
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("browserstack-local-runtime.json")));

        second.close();
        assertEquals(List.of(4242L), operations.stops);
        assertFalse(Files.exists(fixture.paths().state().resolve("browserstack-local-runtime.json")));
        assertFalse(operations.commands.stream().anyMatch(command -> command.contains("--daemon")));
    }

    @Test
    void inspectFailureDoesNotDropTheLeaseOrStopTheProcess(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        BrowserStackLocalLifecycleService lifecycle = new BrowserStackLocalLifecycleService(
                fixture.paths(), fixture.plan(), operations, () -> "test-key");
        lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        operations.inspectFails = true;

        assertThrows(IOException.class, () -> lifecycle.stop(Duration.ofSeconds(1)));
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("browserstack-local-runtime.json")));
        assertTrue(operations.stops.isEmpty());
    }

    @Test
    void failedReadinessTearsDownTheStartedProcess(@TempDir Path temp) {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        operations.ready = false;
        BrowserStackLocalLifecycleService lifecycle = new BrowserStackLocalLifecycleService(
                fixture.paths(), fixture.plan(), operations, () -> "test-key");

        assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));
        assertEquals(List.of(4242L), operations.stops);
        assertFalse(Files.exists(fixture.paths().state().resolve("browserstack-local-runtime.json")));
    }

    private static Fixture uninstalled(Path temp) {
        return fixture(temp, false);
    }

    private static Fixture installed(Path temp) {
        return fixture(temp, true);
    }

    private static Fixture fixture(Path temp, boolean installReceipt) {
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        ShaftCachePaths paths = new ShaftCachePaths(cache, data, cache.resolve("downloads"),
                data.resolve("tools"), data.resolve("state"), data.resolve("receipts"));
        SetupOptions options = SetupOptions.defaults(SetupProfile.BROWSERSTACK_LOCAL, paths)
                .withMode(SetupMode.MANAGED).withTimeouts(Duration.ofSeconds(5), Duration.ofSeconds(5));
        SetupPlan plan = BrowserStackLocalSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        if (installReceipt) {
            try {
                new BrowserStackLocalSetupService(paths, plan, new RecordingOperations(), false)
                        .install(plan, approval);
            } catch (IOException failure) {
                throw new IllegalStateException(failure);
            }
        }
        return new Fixture(paths, options, plan, approval);
    }

    private record Fixture(ShaftCachePaths paths, SetupOptions options, SetupPlan plan, SetupApproval approval) { }

    private static final class RecordingOperations implements BrowserStackLocalToolchainOperations {
        private final List<String> starts = new ArrayList<>();
        private final List<Long> stops = new ArrayList<>();
        private final List<String> commands = new ArrayList<>();
        private boolean running;
        private boolean inspectFails;
        private boolean ready = true;

        @Override public void hostPreflight(List<SetupAction> actions) { /* fixture */ }
        @Override public void lockedPreflight(List<SetupAction> actions, boolean offline) { /* fixture */ }
        @Override public void install(SetupAction action) { /* receipt-only fixture */ }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }

        @Override
        public long startTunnel(Path binary, String accessKey, Path logFile) {
            starts.add(binary.toString());
            commands.add(binary + " --key (redacted)");
            running = true;
            return 4242L;
        }

        @Override
        public boolean processRunning(long pid, Path binary) throws IOException {
            if (inspectFails) throw new IOException("Unable to inspect the owned BrowserStack Local process.");
            return running && pid == 4242L;
        }

        @Override
        public void stopProcess(long pid, Path binary, Duration timeout) {
            java.util.Objects.requireNonNull(timeout, "timeout");
            stops.add(pid);
            running = false;
        }

        @Override
        public void awaitReady(Duration timeout) throws IOException {
            java.util.Objects.requireNonNull(timeout, "timeout");
            if (!ready) throw new IOException("BrowserStack Local did not become ready.");
        }
    }
}
