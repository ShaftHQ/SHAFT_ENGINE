package com.shaft.infrastructure;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingSetupServiceTest {
    @Test
    void installsAndReceiptsWindowsReportingProfileWithoutNetwork(@TempDir Path temp) throws Exception {
        exerciseInstall(temp, SetupPlatform.WINDOWS, createNodeZip(temp.resolve("node.zip")));
    }

    @Test
    void installsLinuxTarArchiveAndRoundTripsPlan(@TempDir Path temp) throws Exception {
        SetupPlan plan = exerciseInstall(temp, SetupPlatform.LINUX, createNodeTar(temp.resolve("node.tar.gz")));
        Path planFile = temp.resolve("plan.json");
        SetupPlanStore.write(planFile, plan);
        assertEquals(plan, SetupPlanStore.read(planFile));
        assertEquals(plan, SetupPlanJson.read(SetupPlanJson.write(plan)));
        assertThrows(IllegalArgumentException.class,
                () -> SetupPlanJson.read(SetupPlanJson.write(plan).replace(plan.digest(), "sha256:" + "0".repeat(64))));
        assertThrows(RuntimeException.class,
                () -> SetupPlanJson.read(SetupPlanJson.write(plan).replaceFirst("\\{", "{\\\"bogus\\\":true,")));
        assertThrows(RuntimeException.class,
                () -> SetupPlanJson.read(SetupPlanJson.write(plan).replaceFirst("\\{", "{\"mode\":\"MANAGED\",")));
    }

    @Test
    void verifiedArtifactStoreRejectsBadHashAndCachesGoodFile(@TempDir Path temp) throws Exception {
        Path source = temp.resolve("artifact.bin");
        Files.writeString(source, "verified");
        String hash = "sha256:" + VerifiedArtifactStore.digest(source);
        SetupAction good = new SetupAction(SetupTarget.ALLURE, SetupActionKind.INSTALL, "1",
                source.toUri(), hash, false, Set.of());
        VerifiedArtifactStore store = new VerifiedArtifactStore(temp.resolve("downloads"));
        Path cached = store.fetch(good);
        assertEquals(cached, store.fetch(good));
        SetupAction bad = new SetupAction(SetupTarget.ALLURE, SetupActionKind.INSTALL, "1",
                source.toUri(), "sha256:" + "0".repeat(64), false, Set.of());
        assertThrows(IOException.class, () -> store.fetch(bad));
    }

    @Test
    void architectureAliasesAndUnsupportedValueAreExplicit() {
        assertEquals(SetupArchitecture.X64, SetupArchitecture.fromOsArch("amd64"));
        assertEquals(SetupArchitecture.X64, SetupArchitecture.fromOsArch("x86_64"));
        assertEquals(SetupArchitecture.ARM64, SetupArchitecture.fromOsArch("aarch64"));
        assertEquals(SetupArchitecture.ARM64, SetupArchitecture.fromOsArch("ARM64"));
        assertThrows(IllegalArgumentException.class, () -> SetupArchitecture.fromOsArch("x86"));
        assertThrows(IllegalArgumentException.class, () -> SetupArchitecture.fromOsArch(" "));
        assertTrue(SetupArchitecture.current() == SetupArchitecture.X64
                || SetupArchitecture.current() == SetupArchitecture.ARM64);
    }

    @Test
    void rejectsDiagnosticAndManifestDivergentPlansBeforeCreatingRoots(@TempDir Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        ShaftCachePaths paths = new ShaftCachePaths(cache, data, cache.resolve("downloads"),
                data.resolve("tools"), data.resolve("state"), data.resolve("receipts"));
        ReportingSetupService service = new ReportingSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, action -> { throw new AssertionError("must not fetch"); },
                (command, log, timeout) -> { throw new AssertionError("must not execute"); });
        SetupPlan external = ReportingSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.EXTERNAL);
        assertThrows(IllegalArgumentException.class, () -> service.install(external,
                new SetupApproval(external.digest(), Instant.EPOCH, Set.of())));
        SetupAction changed = new SetupAction(SetupTarget.ALLURE, SetupActionKind.INSTALL, "3.14.3",
                java.net.URI.create("https://example.invalid/allure.tgz"), "sha256:" + "0".repeat(64),
                false, Set.of());
        SetupPlan divergent = SetupPlan.create(SetupProfile.REPORTING, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED, List.of(changed));
        assertThrows(IllegalArgumentException.class, () -> service.install(divergent,
                new SetupApproval(divergent.digest(), Instant.EPOCH, Set.of())));
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    @Test
    void versionReadinessRequiresAnExactVersion(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        ShaftCachePaths paths = new ShaftCachePaths(cache, data, cache.resolve("downloads"),
                data.resolve("tools"), data.resolve("state"), data.resolve("receipts"));
        Path node = data.resolve("tools/node/24.19.0/windows-x64/node.exe");
        Path allure = data.resolve("tools/allure/3.14.3/node_modules/allure/cli.js");
        Files.createDirectories(node.getParent());
        Files.createDirectories(allure.getParent());
        Files.writeString(node, "node");
        Files.writeString(allure, "allure");
        ReportingSetupService service = new ReportingSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, action -> { throw new AssertionError(); },
                (command, log, timeout) -> new ReportingSetupService.ProcessResult(0,
                        command.stream().anyMatch(part -> part.endsWith("cli.js")) ? "3.14.30" : "v24.19.01"));

        SetupProfileStatus status = service.status();
        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertTrue(status.targets().stream().allMatch(target -> target.readiness() == SetupReadiness.DEGRADED));
    }

    @Test
    void providerFailurePreservesFailedActionAndPartialReceipt(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        ShaftCachePaths paths = new ShaftCachePaths(cache, data, cache.resolve("downloads"),
                data.resolve("tools"), data.resolve("state"), data.resolve("receipts"));
        Path nodeArchive = createNodeZip(temp.resolve("node.zip"));
        ReportingSetupService service = new ReportingSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, action -> {
                    if (action.target() == SetupTarget.ALLURE) throw new IOException("offline");
                    return nodeArchive;
                }, (command, log, timeout) -> new ReportingSetupService.ProcessResult(0, "v24.19.0"));
        SetupPlan plan = ReportingSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        SetupExecutionException failure = assertThrows(SetupExecutionException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));
        assertEquals(SetupTarget.ALLURE, failure.failedAction().target());
        assertEquals(List.of(SetupTarget.NODE), failure.partialReceipt().completedActions().stream()
                .map(SetupAction::target).toList());
        assertTrue(Files.notExists(paths.receipts().resolve("reporting.json")));
    }

    @Test
    void failedReplacementRestoresThePreviousInstallation(@TempDir Path temp) throws Exception {
        Path installed = temp.resolve("installed");
        Path staging = temp.resolve("staging");
        Files.createDirectories(installed);
        Files.createDirectories(staging);
        Files.writeString(installed.resolve("previous.txt"), "previous");
        Files.writeString(staging.resolve("next.txt"), "next");
        java.util.concurrent.atomic.AtomicInteger moves = new java.util.concurrent.atomic.AtomicInteger();
        ReportingSetupService.MoveOperation mover = (source, destination) -> {
            if (moves.incrementAndGet() == 2) throw new IOException("publish blocked");
            Files.move(source, destination);
        };

        assertThrows(IOException.class, () -> ReportingSetupService.publish(staging, installed, mover));
        assertTrue(Files.isRegularFile(installed.resolve("previous.txt")));
        assertTrue(Files.isRegularFile(staging.resolve("next.txt")));
        assertEquals(3, moves.get());
    }

    @Test
    void realChildProcessIsKilledAndReapedOnTimeout(@TempDir Path temp) {
        Set<Long> before = ProcessHandle.allProcesses().filter(ProcessHandle::isAlive)
                .map(ProcessHandle::pid).collect(java.util.stream.Collectors.toSet());
        List<String> command = SetupPlatform.current() == SetupPlatform.WINDOWS
                ? List.of("cmd.exe", "/c", "ping -n 30 127.0.0.1")
                : List.of("sh", "-c", "sleep 30");

        IOException failure = assertThrows(IOException.class, () -> ReportingSetupService.runProcess(
                command, null, java.time.Duration.ofMillis(100), temp, temp));

        assertTrue(failure.getMessage().contains("timed out"));
        assertTrue(ProcessHandle.allProcesses().filter(ProcessHandle::isAlive)
                .noneMatch(handle -> !before.contains(handle.pid()) && handle.info().commandLine()
                        .orElse("").matches("(?is).*(ping\\s+-n\\s+30|sleep\\s+30).*")));
    }

    private static SetupPlan exerciseInstall(Path temp, SetupPlatform platform, Path nodeArchive) throws Exception {
        Path cache = temp.resolve(platform.name().toLowerCase() + "-cache").toAbsolutePath();
        Path data = temp.resolve(platform.name().toLowerCase() + "-data").toAbsolutePath();
        ShaftCachePaths paths = new ShaftCachePaths(cache, data, cache.resolve("downloads"),
                data.resolve("tools"), data.resolve("state"), data.resolve("receipts"));
        Path allurePackage = Files.writeString(temp.resolve(platform.name().toLowerCase() + "-allure.tgz"), "allure");
        ReportingSetupService.ArtifactFetcher fetcher = action ->
                action.target() == SetupTarget.NODE ? nodeArchive : allurePackage;
        ReportingSetupService.CommandRunner runner = (command, log, timeout) -> {
            int prefix = command.indexOf("--prefix");
            if (command.contains("ci") && prefix >= 0) {
                Path entry = Path.of(command.get(prefix + 1)).resolve("node_modules/allure/cli.js");
                Files.createDirectories(entry.getParent());
                Files.writeString(entry, "allure");
                if (log != null) Files.writeString(log, "installed\n");
                return new ReportingSetupService.ProcessResult(0, "installed");
            }
            return new ReportingSetupService.ProcessResult(0,
                    command.stream().anyMatch(part -> part.endsWith("allure/cli.js") || part.endsWith("allure\\cli.js"))
                            ? "3.14.3" : "v24.19.0");
        };
        ReportingSetupService service = new ReportingSetupService(paths, platform, SetupArchitecture.X64,
                fetcher, runner);
        assertEquals(SetupReadiness.MISSING, service.status().readiness());
        SetupPlan plan = ReportingSetupPlanner.plan(platform, SetupArchitecture.X64, SetupMode.MANAGED);
        SetupReceipt receipt = service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));
        assertEquals(2, receipt.completedActions().size());
        assertEquals(SetupReadiness.READY, service.status().readiness());
        assertTrue(Files.isRegularFile(paths.receipts().resolve("reporting.json")));
        assertTrue(Files.isRegularFile(service.logFile()));
        assertEquals(receipt.planDigest(), service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())).planDigest());
        return plan;
    }

    private static Path createNodeZip(Path destination) throws IOException {
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(destination))) {
            for (String entry : List.of("node-v24.19.0-win-x64/node.exe",
                    "node-v24.19.0-win-x64/node_modules/npm/bin/npm-cli.js")) {
                output.putNextEntry(new ZipEntry(entry));
                output.write("binary".getBytes(java.nio.charset.StandardCharsets.UTF_8));
                output.closeEntry();
            }
        }
        return destination;
    }

    private static Path createNodeTar(Path destination) throws IOException {
        try (TarArchiveOutputStream output = new TarArchiveOutputStream(
                new GzipCompressorOutputStream(Files.newOutputStream(destination)))) {
            for (String entry : List.of("node-v24.19.0-linux-x64/bin/node",
                    "node-v24.19.0-linux-x64/lib/node_modules/npm/bin/npm-cli.js")) {
                byte[] content = "binary".getBytes(java.nio.charset.StandardCharsets.UTF_8);
                TarArchiveEntry tarEntry = new TarArchiveEntry(entry);
                tarEntry.setSize(content.length);
                tarEntry.setMode(0755);
                output.putArchiveEntry(tarEntry);
                output.write(content);
                output.closeArchiveEntry();
            }
        }
        return destination;
    }
}
