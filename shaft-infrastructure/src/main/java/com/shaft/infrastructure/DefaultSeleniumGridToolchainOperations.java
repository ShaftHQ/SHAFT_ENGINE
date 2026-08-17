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

/** Host Docker/Compose implementation for the Selenium Grid provider. */
final class DefaultSeleniumGridToolchainOperations implements SeleniumGridToolchainOperations {
    private final ShaftCachePaths paths;
    private final SetupPlan plan;
    private final AndroidCommandRunner runner;
    private final boolean offline;

    DefaultSeleniumGridToolchainOperations(ShaftCachePaths paths, SetupPlan plan, boolean offline) {
        this(paths, plan, AndroidCommandRunner.system(paths, plan.platform(), plan.architecture()), offline);
    }

    DefaultSeleniumGridToolchainOperations(ShaftCachePaths paths, SetupPlan plan, AndroidCommandRunner runner,
                                           boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.plan = java.util.Objects.requireNonNull(plan, "plan");
        this.runner = java.util.Objects.requireNonNull(runner, "runner");
        this.offline = offline;
    }

    @Override
    public void hostPreflight(List<SetupAction> actions) throws IOException {
        requireDocker();
    }

    @Override
    public void lockedPreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(composeFile());
        if ((offline || requireOffline) && !Files.isRegularFile(composeFile(), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Offline Selenium Grid setup requires a staged compose file.");
        }
    }

    @Override
    public void install(SetupAction action) throws IOException {
        if (action.target() == SetupTarget.DOCKER) return;
        if (action.target() != SetupTarget.SELENIUM_GRID) {
            throw new IllegalArgumentException("Unsupported Grid install target: " + action.target());
        }
        SeleniumGridSetupPlanner.GridScale scale = SeleniumGridSetupPlanner.scaleFromPlan(plan);
        byte[] compose = SeleniumGridSetupPlanner.compose(scale.port()).getBytes(StandardCharsets.UTF_8);
        String actual;
        try {
            actual = "sha256:" + HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(compose));
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
        if (!actual.equalsIgnoreCase(action.checksum())) {
            throw new IOException("Rendered Selenium Grid compose does not match the approved plan.");
        }
        Path destination = composeFile();
        Files.createDirectories(destination.getParent());
        Path temporary = Files.createTempFile(destination.getParent(), "docker-compose", ".tmp");
        try {
            Files.write(temporary, compose);
            VerifiedArtifactStore.move(temporary, destination);
        } finally {
            Files.deleteIfExists(temporary);
        }
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
                        "Managed Selenium Grid compose file is missing.");
            }
            String actual = "sha256:" + VerifiedArtifactStore.digest(compose);
            if (!actual.equalsIgnoreCase(action.checksum())) {
                return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Staged compose does not match the approved plan.");
            }
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(),
                    "Staged Selenium Grid compose matches the reviewed plan.");
        } catch (IOException failure) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    @Override
    public void composeUp(Path composeFile, String project, SeleniumGridSetupPlanner.GridScale scale)
            throws IOException {
        requireOwned(project);
        List<String> command = List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(),
                "up", "-d", "--scale", "chrome=" + scale.chrome(), "--scale", "edge=" + scale.edge(),
                "--scale", "firefox=" + scale.firefox());
        run(command, "Failed to start the SHAFT Selenium Grid compose project.");
    }

    @Override
    public void composeDown(Path composeFile, String project) throws IOException {
        requireOwned(project);
        run(List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "down", "--remove-orphans"),
                "Failed to stop the SHAFT Selenium Grid compose project.");
    }

    @Override
    public boolean composeRunning(Path composeFile, String project) throws IOException {
        requireOwned(project);
        ReportingSetupService.ProcessResult result = runner.run(
                List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "ps", "-q"),
                composeFile.getParent(), Map.of(), Set.of(), null, null, Duration.ofSeconds(15));
        if (result.exitCode() != 0) {
            throw new IOException("Unable to inspect the SHAFT Selenium Grid compose project. " + result.output());
        }
        return !result.output().isBlank();
    }

    @Override
    public void awaitReady(URI endpoint, Duration timeout) throws IOException {
        Instant deadline = Instant.now().plus(timeout);
        URI status = endpoint.resolve("wd/hub/status");
        IOException last = new IOException("Selenium Grid did not become ready.");
        while (Instant.now().isBefore(deadline)) {
            HttpURLConnection connection = (HttpURLConnection) status.toURL().openConnection();
            try {
                connection.setConnectTimeout(1_000);
                connection.setReadTimeout(1_000);
                connection.setRequestMethod("GET");
                if (connection.getResponseCode() == 200) return;
                last = new IOException("Selenium Grid status returned HTTP " + connection.getResponseCode() + '.');
            } catch (IOException failure) {
                last = failure;
            } finally {
                connection.disconnect();
            }
            try {
                Thread.sleep(250);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while waiting for Selenium Grid.", interrupted);
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
        if (!SeleniumGridSetupPlanner.PROJECT.equals(project)) {
            throw new IOException("Refusing to operate on an unowned Docker Compose project: " + project);
        }
    }

    private Path composeFile() {
        return paths.tools().resolve("selenium-grid").resolve("docker-compose.yml");
    }

    private Path logFile() {
        return paths.state().resolve("logs").resolve("selenium-grid.log");
    }

    private String docker() {
        return plan.platform() == SetupPlatform.WINDOWS ? "docker.exe" : "docker";
    }
}
