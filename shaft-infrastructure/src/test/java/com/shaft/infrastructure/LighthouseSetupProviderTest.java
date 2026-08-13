package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LighthouseSetupProviderTest {
    @Test
    void externalStatusNeverExecutesInstalledManagedTools(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = paths.tools().resolve("node/24.19.0/windows-x64/node.exe");
        Path lighthouse = paths.tools().resolve("lighthouse/13.4.1/node_modules/lighthouse/cli/index.js");
        Files.createDirectories(node.getParent());
        Files.createDirectories(lighthouse.getParent());
        if (SetupPlatform.current() == SetupPlatform.WINDOWS) {
            Files.copy(Path.of(System.getenv("SystemRoot"), "System32", "where.exe"), node);
        } else {
            Files.writeString(node, "#!/bin/sh\nexit 7\n");
            assertTrue(node.toFile().setExecutable(true));
        }
        Files.writeString(lighthouse, "installed lighthouse");
        LighthouseSetupProvider provider = new LighthouseSetupProvider();
        SetupOptions options = SetupOptions.defaults(SetupProfile.LIGHTHOUSE, paths);

        SetupReport report = provider.status(options, SetupPlatform.WINDOWS, SetupArchitecture.X64);

        assertEquals(SetupReadiness.MISSING, report.readiness());
        assertTrue(report.targets().stream().allMatch(target -> target.readiness() == SetupReadiness.MISSING));
    }

    @Test
    void offlineCacheMissIsRejectedBeforeAnySetupMutation(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        LighthouseSetupService service = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, action -> { throw new IOException("offline miss"); },
                (command, log, timeout) -> { throw new AssertionError("No process may start."); }, true);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        IOException failure = assertThrows(IOException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertTrue(failure.getMessage().contains("offline miss"));
        assertFalse(Files.exists(paths.cacheRoot()));
        assertFalse(Files.exists(paths.dataRoot()));
        assertFalse(Files.exists(paths.downloads()));
        assertFalse(Files.exists(paths.tools()));
        assertFalse(Files.exists(paths.state()));
        assertFalse(Files.exists(paths.receipts()));
    }

    @Test
    void incompleteTransitiveOfflineCacheIsRejectedBeforeProcessesOrMutation(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        LighthouseSetupService service = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64,
                action -> action.target() == SetupTarget.NODE ? nodeArchive : lighthouseArchive,
                (command, log, timeout) -> { throw new AssertionError("No process may start before offline preflight."); },
                true);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        IOException failure = assertThrows(IOException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertTrue(failure.getMessage().contains("transitive npm cache is incomplete"));
        assertFalse(Files.exists(paths.cacheRoot()));
        assertFalse(Files.exists(paths.dataRoot()));
        assertFalse(Files.exists(paths.tools()));
        assertFalse(Files.exists(paths.state()));
        assertFalse(Files.exists(paths.receipts()));
    }

    @Test
    void installedNodeDoesNotExecuteBeforeOfflineTransitiveCachePreflight(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = paths.tools().resolve("node/24.19.0/windows-x64/node.exe");
        Files.createDirectories(node.getParent());
        Files.writeString(node, "structurally installed node");
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        LighthouseSetupService service = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, action -> {
                    if (action.target() == SetupTarget.NODE) throw new AssertionError("Installed Node must not refetch.");
                    return lighthouseArchive;
                }, (command, log, timeout) -> { throw new AssertionError("No executable probe before offline cache proof."); },
                true);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        IOException failure = assertThrows(IOException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertTrue(failure.getMessage().contains("transitive npm cache is incomplete"));
        assertFalse(Files.exists(paths.state()));
        assertFalse(Files.exists(paths.receipts()));
    }

    @Test
    void installsAndExactlyVerifiesTheApprovedToolchain(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        AtomicBoolean lighthouseHealthy = new AtomicBoolean(true);
        LighthouseSetupService service = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64,
                action -> action.target() == SetupTarget.NODE ? nodeArchive : lighthouseArchive,
                (command, log, timeout) -> {
                    int prefix = command.indexOf("--prefix");
                    if (command.contains("ci") && prefix >= 0) {
                        Path entry = Path.of(command.get(prefix + 1))
                                .resolve("node_modules/lighthouse/cli/index.js");
                        Files.createDirectories(entry.getParent());
                        Files.writeString(entry, "lighthouse");
                    }
                    return new ReportingSetupService.ProcessResult(0,
                            command.stream().anyMatch(part -> part.endsWith("index.js"))
                                    ? lighthouseHealthy.get() ? "13.4.1" : "13.4.0"
                                    : "v24.19.0");
                }, false);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        SetupReceipt receipt = service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(SetupReadiness.READY, service.status().readiness());
        assertTrue(Files.isRegularFile(paths.receipts().resolve("lighthouse.json")));

        lighthouseHealthy.set(false);
        assertEquals(SetupReadiness.DEGRADED, service.status().readiness());
    }

    @Test
    void failedLighthouseActionPreservesNodeAndRetryCompletes(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        AtomicBoolean failInstall = new AtomicBoolean(true);
        LighthouseSetupService service = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64,
                action -> action.target() == SetupTarget.NODE ? nodeArchive : lighthouseArchive,
                (command, log, timeout) -> {
                    int prefix = command.indexOf("--prefix");
                    if (command.contains("ci") && failInstall.getAndSet(false)) {
                        return new ReportingSetupService.ProcessResult(9, "npm failed");
                    }
                    if (command.contains("ci") && prefix >= 0) {
                        Path entry = Path.of(command.get(prefix + 1))
                                .resolve("node_modules/lighthouse/cli/index.js");
                        Files.createDirectories(entry.getParent());
                        Files.writeString(entry, "lighthouse");
                    }
                    return new ReportingSetupService.ProcessResult(0,
                            command.stream().anyMatch(part -> part.endsWith("index.js")) ? "13.4.1" : "v24.19.0");
                }, false);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());

        SetupExecutionException failure = assertThrows(SetupExecutionException.class,
                () -> service.install(plan, approval));

        assertEquals(SetupTarget.LIGHTHOUSE, failure.failedAction().target());
        assertEquals(List.of(plan.actions().getFirst()), failure.partialReceipt().completedActions());
        assertFalse(Files.exists(paths.receipts().resolve("lighthouse.json")));

        SetupReceipt receipt = service.install(plan, approval);
        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(SetupReadiness.READY, service.status().readiness());
    }

    @Test
    void wrongVersionNeverPublishesOrReceiptsLighthouse(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        LighthouseSetupService service = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64,
                action -> action.target() == SetupTarget.NODE ? nodeArchive : lighthouseArchive,
                (command, log, timeout) -> {
                    int prefix = command.indexOf("--prefix");
                    if (command.contains("ci") && prefix >= 0) {
                        Path entry = Path.of(command.get(prefix + 1)).resolve("node_modules/lighthouse/cli/index.js");
                        Files.createDirectories(entry.getParent());
                        Files.writeString(entry, "wrong lighthouse");
                    }
                    return new ReportingSetupService.ProcessResult(0,
                            command.stream().anyMatch(part -> part.endsWith("index.js")) ? "13.4.0" : "v24.19.0");
                }, false);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        SetupExecutionException failure = assertThrows(SetupExecutionException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertEquals(SetupTarget.LIGHTHOUSE, failure.failedAction().target());
        assertFalse(Files.exists(paths.tools().resolve("lighthouse/13.4.1")));
        assertFalse(Files.exists(paths.receipts().resolve("lighthouse.json")));
    }

    @Test
    void hybridInstallImmediatelyVerifiesReady(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        LighthouseSetupService service = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, action -> action.target() == SetupTarget.NODE ? nodeArchive : lighthouseArchive,
                (command, log, timeout) -> {
                    int prefix = command.indexOf("--prefix");
                    if (command.contains("ci") && prefix >= 0) {
                        Path entry = Path.of(command.get(prefix + 1)).resolve("node_modules/lighthouse/cli/index.js");
                        Files.createDirectories(entry.getParent());
                        Files.writeString(entry, "lighthouse");
                    }
                    return new ReportingSetupService.ProcessResult(0,
                            command.stream().anyMatch(part -> part.endsWith("index.js")) ? "13.4.1" : "v24.19.0");
                }, false);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64, SetupMode.HYBRID);

        service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));

        assertEquals(SetupReadiness.READY, service.status().readiness());
    }

    @Test
    void readyInstallIsReusableOfflineAfterDownloadCacheEviction(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        ReportingSetupService.CommandRunner runner = (command, log, timeout) -> {
            int prefix = command.indexOf("--prefix");
            if (command.contains("ci") && prefix >= 0) {
                Path entry = Path.of(command.get(prefix + 1)).resolve("node_modules/lighthouse/cli/index.js");
                Files.createDirectories(entry.getParent());
                Files.writeString(entry, "lighthouse");
            }
            return new ReportingSetupService.ProcessResult(0,
                    command.stream().anyMatch(part -> part.endsWith("index.js")) ? "13.4.1" : "v24.19.0");
        };
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        new LighthouseSetupService(paths, SetupPlatform.WINDOWS, SetupArchitecture.X64,
                action -> action.target() == SetupTarget.NODE ? nodeArchive : lighthouseArchive,
                runner, false).install(plan, approval);

        LighthouseSetupService offline = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, action -> { throw new AssertionError("Ready tools must not refetch artifacts."); },
                runner, true);

        assertEquals(plan.actions(), offline.install(plan, approval).completedActions());
        assertEquals(SetupReadiness.READY, offline.status().readiness());
    }

    @Test
    void concurrentProviderInstancesConvergeWithoutOverlappingFileLocks(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path lighthouseArchive = Files.writeString(temp.resolve("lighthouse.tgz"), "lighthouse");
        ReportingSetupService.ArtifactFetcher fetcher = action ->
                action.target() == SetupTarget.NODE ? nodeArchive : lighthouseArchive;
        ReportingSetupService.CommandRunner runner = (command, log, timeout) -> {
            int prefix = command.indexOf("--prefix");
            if (command.contains("ci") && prefix >= 0) {
                Path entry = Path.of(command.get(prefix + 1)).resolve("node_modules/lighthouse/cli/index.js");
                Files.createDirectories(entry.getParent());
                Files.writeString(entry, "lighthouse");
            }
            return new ReportingSetupService.ProcessResult(0,
                    command.stream().anyMatch(part -> part.endsWith("index.js")) ? "13.4.1" : "v24.19.0");
        };
        LighthouseSetupService first = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, fetcher, runner, false);
        LighthouseSetupService second = new LighthouseSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, fetcher, runner, false);
        SetupPlan plan = LighthouseSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        CountDownLatch start = new CountDownLatch(1);
        var executor = Executors.newFixedThreadPool(2);
        try {
            var one = executor.submit(() -> { start.await(); return first.install(plan, approval); });
            var two = executor.submit(() -> { start.await(); return second.install(plan, approval); });
            start.countDown();

            assertEquals(plan.actions(), one.get(30, TimeUnit.SECONDS).completedActions());
            assertEquals(plan.actions(), two.get(30, TimeUnit.SECONDS).completedActions());
            assertEquals(SetupReadiness.READY, first.status().readiness());
        } finally {
            executor.shutdownNow();
        }
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
