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
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SeleniumGridLifecycleServiceTest {
    @Test
    void missingReceiptStartsNoComposeProject(@TempDir Path temp) {
        Fixture fixture = uninstalled(temp);
        RecordingOperations operations = new RecordingOperations();
        SeleniumGridLifecycleService lifecycle = new SeleniumGridLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("receipt"));
        assertTrue(operations.ups.isEmpty());
    }

    @Test
    void occupiedGridPortStartsNoComposeProject(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        SeleniumGridLifecycleService lifecycle = new SeleniumGridLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        try (ServerSocket occupied = new ServerSocket()) {
            occupied.bind(new InetSocketAddress("127.0.0.1", 4444));

            IOException failure = assertThrows(IOException.class,
                    () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

            assertTrue(failure.getMessage().contains("4444"));
            assertTrue(operations.ups.isEmpty());
        }
    }

    @Test
    void startReusesThenStopsOnlyTheOwnedProject(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        SeleniumGridLifecycleService first = new SeleniumGridLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        ManagedEnvironment firstEnv = first.start(fixture.plan(), fixture.approval(), fixture.options());
        ManagedEnvironment secondEnv = first.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(1, operations.ups.size());
        assertEquals(SeleniumGridSetupPlanner.PROJECT, operations.ups.getFirst());
        assertEquals(URI.create("http://127.0.0.1:4444/"), firstEnv.endpoint().orElseThrow());
        assertEquals("localhost:4444", firstEnv.connectionProperties().get("executionAddress"));

        firstEnv.close();
        assertTrue(operations.downs.isEmpty());
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("selenium-grid-runtime.json")));

        secondEnv.close();
        assertEquals(List.of(SeleniumGridSetupPlanner.PROJECT), operations.downs);
        assertFalse(Files.exists(fixture.paths().state().resolve("selenium-grid-runtime.json")));
    }

    @Test
    void composeInspectFailureDoesNotDropTheLeaseOrStopContainers(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        SeleniumGridLifecycleService lifecycle = new SeleniumGridLifecycleService(
                fixture.paths(), fixture.plan(), operations);
        lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        operations.inspectFails = true;

        assertThrows(IOException.class, () -> lifecycle.stop(Duration.ofSeconds(1)));
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("selenium-grid-runtime.json")));
        assertTrue(operations.downs.isEmpty());
    }

    @Test
    void logsMatchRuntimeIdentityWithoutRequiringTheManagedPlanDigest(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        SeleniumGridLifecycleService managed = new SeleniumGridLifecycleService(
                fixture.paths(), fixture.plan(), operations);
        managed.start(fixture.plan(), fixture.approval(), fixture.options());
        Files.createDirectories(managed.logFile().getParent());
        Files.writeString(managed.logFile(), "hub ready");

        SetupPlan external = SeleniumGridSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.EXTERNAL, SetupSelection.defaults());
        SeleniumGridLifecycleService diagnostic = new SeleniumGridLifecycleService(
                fixture.paths(), external, operations);

        assertEquals("hub ready", diagnostic.logs());
    }

    @Test
    void failedReadinessTearsDownOnlyTheOwnedProject(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        operations.ready = false;
        SeleniumGridLifecycleService lifecycle = new SeleniumGridLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));
        assertEquals(List.of(SeleniumGridSetupPlanner.PROJECT), operations.ups);
        assertEquals(List.of(SeleniumGridSetupPlanner.PROJECT), operations.downs);
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
        SetupOptions options = SetupOptions.defaults(SetupProfile.SELENIUM_GRID, paths)
                .withMode(SetupMode.MANAGED).withTimeouts(Duration.ofSeconds(5), Duration.ofSeconds(5));
        SetupPlan plan = SeleniumGridSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        if (installReceipt) {
            try {
                new SeleniumGridSetupService(paths, plan, new RecordingOperations(), false).install(plan, approval);
            } catch (IOException failure) {
                throw new IllegalStateException(failure);
            }
        }
        return new Fixture(paths, options, plan, approval);
    }

    private record Fixture(ShaftCachePaths paths, SetupOptions options, SetupPlan plan, SetupApproval approval) { }

    private static final class RecordingOperations implements SeleniumGridToolchainOperations {
        private final List<String> ups = new ArrayList<>();
        private final List<String> downs = new ArrayList<>();
        private boolean running;
        private boolean ready = true;
        private boolean inspectFails;

        @Override public void hostPreflight(List<SetupAction> actions) { /* no host mutation in this fixture */ }
        @Override public void lockedPreflight(List<SetupAction> actions, boolean offline) { /* no locked preflight */ }
        @Override public void install(SetupAction action) { /* receipt-only fixture */ }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }

        @Override
        public void composeUp(Path composeFile, String project, SeleniumGridSetupPlanner.GridScale scale) {
            ups.add(project);
            running = true;
        }

        @Override
        public void composeDown(Path composeFile, String project) {
            downs.add(project);
            running = false;
        }

        @Override
        public boolean composeRunning(Path composeFile, String project) throws IOException {
            if (inspectFails) throw new IOException("Unable to inspect the SHAFT Selenium Grid compose project.");
            return running;
        }

        @Override
        public void awaitReady(URI endpoint, Duration timeout) throws IOException {
            if (!ready) throw new IOException("Grid did not become ready.");
        }
    }
}
