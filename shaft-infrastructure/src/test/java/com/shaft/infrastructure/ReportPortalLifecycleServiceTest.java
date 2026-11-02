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

class ReportPortalLifecycleServiceTest {
    @Test
    void missingReceiptStartsNoComposeProject(@TempDir Path temp) {
        Fixture fixture = uninstalled(temp);
        RecordingOperations operations = new RecordingOperations();
        ReportPortalLifecycleService lifecycle = new ReportPortalLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        IOException failure = assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

        assertTrue(failure.getMessage().contains("receipt"));
        assertTrue(operations.ups.isEmpty());
    }

    @Test
    void occupiedUiPortStartsNoComposeProject(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        ReportPortalLifecycleService lifecycle = new ReportPortalLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        try (ServerSocket occupied = new ServerSocket()) {
            occupied.bind(new InetSocketAddress("127.0.0.1", 8080));

            IOException failure = assertThrows(IOException.class,
                    () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));

            assertTrue(failure.getMessage().contains("8080"));
            assertTrue(operations.ups.isEmpty());
        }
    }

    @Test
    void startReusesThenStopsOnlyTheOwnedProject(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        ReportPortalLifecycleService first = new ReportPortalLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        ManagedEnvironment firstEnv = first.start(fixture.plan(), fixture.approval(), fixture.options());
        ManagedEnvironment secondEnv = first.start(fixture.plan(), fixture.approval(), fixture.options());

        assertEquals(1, operations.ups.size());
        assertEquals(ReportPortalSetupPlanner.PROJECT, operations.ups.getFirst());
        assertEquals(URI.create("http://127.0.0.1:8080/ui/"), firstEnv.endpoint().orElseThrow());
        assertEquals("8080", firstEnv.connectionProperties().get("serverPort"));
        assertEquals("localhost", firstEnv.connectionProperties().get("serverHost"));
        assertEquals(Set.of("serverHost", "serverPort"), firstEnv.connectionProperties().keySet());

        firstEnv.close();
        assertTrue(operations.downs.isEmpty());
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("reportportal-runtime.json")));

        secondEnv.close();
        assertEquals(List.of(ReportPortalSetupPlanner.PROJECT), operations.downs);
        assertFalse(Files.exists(fixture.paths().state().resolve("reportportal-runtime.json")));
    }

    @Test
    void composeInspectFailureDoesNotDropTheLeaseOrStopContainers(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        ReportPortalLifecycleService lifecycle = new ReportPortalLifecycleService(
                fixture.paths(), fixture.plan(), operations);
        lifecycle.start(fixture.plan(), fixture.approval(), fixture.options());
        operations.inspectFails = true;

        assertThrows(IOException.class, () -> lifecycle.stop(Duration.ofSeconds(1)));
        assertTrue(Files.isRegularFile(fixture.paths().state().resolve("reportportal-runtime.json")));
        assertTrue(operations.downs.isEmpty());
    }

    @Test
    void logsMatchRuntimeIdentityWithoutRequiringTheManagedPlanDigest(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        ReportPortalLifecycleService managed = new ReportPortalLifecycleService(
                fixture.paths(), fixture.plan(), operations);
        managed.start(fixture.plan(), fixture.approval(), fixture.options());
        Files.createDirectories(managed.logFile().getParent());
        Files.writeString(managed.logFile(), "ui ready");

        SetupPlan external = ReportPortalSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.EXTERNAL, SetupSelection.defaults());
        ReportPortalLifecycleService diagnostic = new ReportPortalLifecycleService(
                fixture.paths(), external, operations);

        assertEquals("ui ready", diagnostic.logs());
    }

    @Test
    void failedReadinessTearsDownOnlyTheOwnedProject(@TempDir Path temp) throws Exception {
        Fixture fixture = installed(temp);
        RecordingOperations operations = new RecordingOperations();
        operations.ready = false;
        ReportPortalLifecycleService lifecycle = new ReportPortalLifecycleService(
                fixture.paths(), fixture.plan(), operations);

        assertThrows(IOException.class,
                () -> lifecycle.start(fixture.plan(), fixture.approval(), fixture.options()));
        assertEquals(List.of(ReportPortalSetupPlanner.PROJECT), operations.ups);
        assertEquals(List.of(ReportPortalSetupPlanner.PROJECT), operations.downs);
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
        SetupOptions options = SetupOptions.defaults(SetupProfile.REPORT_PORTAL, paths)
                .withMode(SetupMode.MANAGED).withTimeouts(Duration.ofSeconds(5), Duration.ofSeconds(5));
        SetupPlan plan = ReportPortalSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        if (installReceipt) {
            try {
                new ReportPortalSetupService(paths, plan, new RecordingOperations(), false).install(plan, approval);
            } catch (IOException failure) {
                throw new IllegalStateException(failure);
            }
        }
        return new Fixture(paths, options, plan, approval);
    }

    private record Fixture(ShaftCachePaths paths, SetupOptions options, SetupPlan plan, SetupApproval approval) { }

    private static final class RecordingOperations implements ReportPortalToolchainOperations {
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
        public void composeUp(Path composeFile, String project) {
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
            if (inspectFails) throw new IOException("Unable to inspect the SHAFT ReportPortal compose project.");
            return running;
        }

        @Override
        public void awaitReady(URI ui, Duration timeout) throws IOException {
            if (!ready) throw new IOException("ReportPortal did not become ready.");
        }
    }
}
