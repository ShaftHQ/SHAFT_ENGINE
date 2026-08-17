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

/** Host Docker/Compose implementation for the Healenium provider. */
final class DefaultHealeniumToolchainOperations implements HealeniumToolchainOperations {
    private final ShaftCachePaths paths;
    private final SetupPlan plan;
    private final AndroidCommandRunner runner;
    private final boolean offline;

    DefaultHealeniumToolchainOperations(ShaftCachePaths paths, SetupPlan plan, boolean offline) {
        this(paths, plan, AndroidCommandRunner.system(paths, plan.platform(), plan.architecture()), offline);
    }

    DefaultHealeniumToolchainOperations(ShaftCachePaths paths, SetupPlan plan, AndroidCommandRunner runner,
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
        VerifiedArtifactStore.requireUnlinkedAncestors(initFile());
        if ((offline || requireOffline) && !Files.isRegularFile(composeFile(), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Offline Healenium setup requires a staged compose file.");
        }
    }

    @Override
    public void install(SetupAction action) throws IOException {
        if (action.target() == SetupTarget.DOCKER) return;
        if (action.target() != SetupTarget.HEALENIUM) {
            throw new IllegalArgumentException("Unsupported Healenium install target: " + action.target());
        }
        HealeniumSetupPlanner.HealeniumScale scale = HealeniumSetupPlanner.scaleFromPlan(plan);
        byte[] artifact = HealeniumSetupPlanner.artifact(scale).getBytes(StandardCharsets.UTF_8);
        String actual;
        try {
            actual = "sha256:" + HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(artifact));
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
        if (!actual.equalsIgnoreCase(action.checksum())) {
            throw new IOException("Rendered Healenium compose does not match the approved plan.");
        }
        writeAtomic(composeFile(), HealeniumSetupPlanner.compose(scale).getBytes(StandardCharsets.UTF_8));
        writeAtomic(initFile(), HealeniumSetupPlanner.INIT_SQL.getBytes(StandardCharsets.UTF_8));
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
            Path init = initFile();
            VerifiedArtifactStore.requireUnlinkedAncestors(compose);
            VerifiedArtifactStore.requireUnlinkedAncestors(init);
            if (!Files.isRegularFile(compose, LinkOption.NOFOLLOW_LINKS)
                    || !Files.isRegularFile(init, LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        "Managed Healenium compose file is missing.");
            }
            String staged = Files.readString(compose) + "\n" + Files.readString(init);
            String actual = "sha256:" + HexFormat.of().formatHex(
                    MessageDigest.getInstance("SHA-256").digest(staged.getBytes(StandardCharsets.UTF_8)));
            if (!actual.equalsIgnoreCase(action.checksum())) {
                return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Staged compose does not match the approved plan.");
            }
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(),
                    "Staged Healenium compose matches the reviewed plan.");
        } catch (IOException | NoSuchAlgorithmException failure) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    @Override
    public void composeUp(Path composeFile, String project) throws IOException {
        requireOwned(project);
        run(List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "up", "-d"),
                "Failed to start the SHAFT Healenium compose project.");
    }

    @Override
    public void composeDown(Path composeFile, String project) throws IOException {
        requireOwned(project);
        run(List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "down", "--remove-orphans"),
                "Failed to stop the SHAFT Healenium compose project.");
    }

    @Override
    public boolean composeRunning(Path composeFile, String project) throws IOException {
        requireOwned(project);
        ReportingSetupService.ProcessResult result = runner.run(
                List.of(docker(), "compose", "-p", project, "-f", composeFile.toString(), "ps", "-q"),
                composeFile.getParent(), Map.of(), Set.of(), null, null, Duration.ofSeconds(15));
        if (result.exitCode() != 0) {
            throw new IOException("Unable to inspect the SHAFT Healenium compose project. " + result.output());
        }
        return !result.output().isBlank();
    }

    @Override
    public void awaitReady(URI backend, URI imitate, Duration timeout) throws IOException {
        Instant deadline = Instant.now().plus(timeout);
        IOException last = new IOException("Healenium did not become ready.");
        while (Instant.now().isBefore(deadline)) {
            try {
                probe(backend, "Healenium backend");
                probe(imitate, "Healenium selector imitator");
                return;
            } catch (IOException failure) {
                last = failure;
            }
            try {
                Thread.sleep(250);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while waiting for Healenium.", interrupted);
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
        if (!HealeniumSetupPlanner.PROJECT.equals(project)) {
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
        return paths.tools().resolve("healenium").resolve("docker-compose.yml");
    }

    private Path initFile() {
        return paths.tools().resolve("healenium").resolve("init.sql");
    }

    private Path logFile() {
        return paths.state().resolve("logs").resolve("healenium.log");
    }

    private String docker() {
        return plan.platform() == SetupPlatform.WINDOWS ? "docker.exe" : "docker";
    }
}
