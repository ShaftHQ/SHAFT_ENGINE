package com.shaft.infrastructure;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Host Docker/Compose implementation for the ReportPortal provider. */
final class DefaultReportPortalToolchainOperations implements ReportPortalToolchainOperations {
    private final ShaftCachePaths paths;
    private final SetupPlan plan;
    private final AndroidCommandRunner runner;
    private final boolean offline;

    DefaultReportPortalToolchainOperations(ShaftCachePaths paths, SetupPlan plan, boolean offline) {
        this(paths, plan, AndroidCommandRunner.system(paths, plan.platform(), plan.architecture()), offline);
    }

    DefaultReportPortalToolchainOperations(ShaftCachePaths paths, SetupPlan plan, AndroidCommandRunner runner,
                                           boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.plan = java.util.Objects.requireNonNull(plan, "plan");
        this.runner = java.util.Objects.requireNonNull(runner, "runner");
        this.offline = offline;
    }

    @Override
    public void hostPreflight(List<SetupAction> actions) throws IOException {
        java.util.Objects.requireNonNull(actions, "actions");
        requireDocker();
    }

    @Override
    public void lockedPreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
        java.util.Objects.requireNonNull(actions, "actions");
        VerifiedArtifactStore.requireUnlinkedAncestors(composeFile());
        if ((offline || requireOffline) && !Files.isRegularFile(composeFile(), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Offline ReportPortal setup requires a staged compose file.");
        }
    }

    @Override
    public void install(SetupAction action) throws IOException {
        if (action.target() == SetupTarget.DOCKER) return;
        if (action.target() != SetupTarget.REPORT_PORTAL) {
            throw new IllegalArgumentException("Unsupported ReportPortal install target: " + action.target());
        }
        ReportPortalSetupPlanner.ReportPortalScale scale = ReportPortalSetupPlanner.scaleFromPlan(plan);
        byte[] artifact = ReportPortalSetupPlanner.compose(scale).getBytes(StandardCharsets.UTF_8);
        String actual;
        try {
            actual = "sha256:" + HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(artifact));
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
        if (!actual.equalsIgnoreCase(action.checksum())) {
            throw new IOException("Rendered ReportPortal compose does not match the approved plan.");
        }
        writeAtomic(composeFile(), artifact);
    }

    @Override
    public SetupStatus status(SetupAction action) {
        if (action.target() == SetupTarget.DOCKER) {
            try {
                requireDocker();
                return new SetupStatus(action.target(), SetupReadiness.READY, action.version(),
                        "Docker Engine and Compose are available.");
            } catch (IOException failure) {
                return new SetupStatus(action.target(), SetupReadiness.MISSING, "", failure.getMessage());
            }
        }
        try {
            Path compose = composeFile();
            VerifiedArtifactStore.requireUnlinkedAncestors(compose);
            if (!Files.isRegularFile(compose, LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        "Managed ReportPortal compose file is missing.");
            }
            String staged = Files.readString(compose);
            String actual = "sha256:" + HexFormat.of().formatHex(
                    MessageDigest.getInstance("SHA-256").digest(staged.getBytes(StandardCharsets.UTF_8)));
            if (!actual.equalsIgnoreCase(action.checksum())) {
                return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Staged compose does not match the approved plan.");
            }
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(),
                    "Staged ReportPortal compose matches the reviewed plan.");
        } catch (IOException | NoSuchAlgorithmException failure) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    @Override
    public void composeUp(Path composeFile, String project) throws IOException {
        requireOwned(project);
        run(List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "up", "-d"),
                "Failed to start the SHAFT ReportPortal compose project.");
    }

    @Override
    public void composeDown(Path composeFile, String project) throws IOException {
        requireOwned(project);
        run(List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "down", "--remove-orphans"),
                "Failed to stop the SHAFT ReportPortal compose project.");
    }

    @Override
    public boolean composeRunning(Path composeFile, String project) throws IOException {
        requireOwned(project);
        ReportingSetupService.ProcessResult result = runner.run(
                List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "ps", "-q"),
                composeFile.getParent(), Map.of(), Set.of(), null, null, Duration.ofSeconds(15));
        if (result.exitCode() != 0) {
            throw new IOException("Unable to inspect the SHAFT ReportPortal compose project. " + result.output());
        }
        return !result.output().isBlank();
    }

    @Override
    public void awaitReady(URI ui, Duration timeout) throws IOException {
        Instant deadline = Instant.now().plus(timeout);
        IOException last = new IOException("ReportPortal did not become ready.");
        while (Instant.now().isBefore(deadline)) {
            try {
                probe(ui, "ReportPortal UI");
                return;
            } catch (IOException failure) {
                last = failure;
            }
            try {
                Thread.sleep(250);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while waiting for ReportPortal.", interrupted);
            }
        }
        throw last;
    }

    private void requireDocker() throws IOException {
        try {
            ReportingSetupService.ProcessResult compose = runner.run(List.of(docker(), "compose", "version"),
                    paths.cacheRoot(), Map.of(), Set.of(), null, null, Duration.ofSeconds(15));
            ReportingSetupService.ProcessResult engine = runner.run(List.of(docker(), "version"),
                    paths.cacheRoot(), Map.of(), Set.of(), null, null, Duration.ofSeconds(15));
            if (compose.exitCode() != 0 || engine.exitCode() != 0) {
                throw new IOException("Docker Engine 26.1.4+ and Compose v2 are required.");
            }
        } catch (IOException failure) {
            throw new IOException("Docker Engine 26.1.4+ and Compose v2 are required.", failure);
        }
    }

    private void run(List<String> command, String failure) throws IOException {
        Path log = logFile();
        Files.createDirectories(log.getParent());
        ReportingSetupService.ProcessResult result = runner.run(command, composeFile().getParent(), Map.of(),
                Set.of(), null, log, Duration.ofMinutes(5));
        if (result.exitCode() != 0) throw new IOException(failure + " " + result.output());
    }

    private static void requireOwned(String project) throws IOException {
        if (!ReportPortalSetupPlanner.PROJECT.equals(project)) {
            throw new IOException("Refusing to operate on an unowned Docker Compose project: " + project);
        }
    }

    private static void probe(URI endpoint, String owner) throws IOException {
        HttpURLConnection connection = (HttpURLConnection) endpoint.toURL().openConnection();
        try {
            connection.setConnectTimeout(1_000);
            connection.setReadTimeout(1_000);
            connection.setRequestMethod("GET");
            int code = connection.getResponseCode();
            if (code >= 200 && code < 500) return;
            throw new IOException(owner + " status returned HTTP " + code + '.');
        } finally {
            connection.disconnect();
        }
    }

    private static void writeAtomic(Path destination, byte[] bytes) throws IOException {
        Files.createDirectories(destination.getParent());
        Path temporary = Files.createTempFile(destination.getParent(), destination.getFileName().toString(), ".tmp");
        try {
            Files.write(temporary, bytes);
            VerifiedArtifactStore.move(temporary, destination);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private Path composeFile() {
        return paths.tools().resolve("reportportal").resolve("docker-compose.yml");
    }

    private Path logFile() {
        return paths.state().resolve("logs").resolve("reportportal.log");
    }

    private String docker() {
        return plan.platform() == SetupPlatform.WINDOWS ? "docker.exe" : "docker";
    }
}
