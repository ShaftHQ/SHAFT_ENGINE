package com.shaft.infrastructure;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.Comparator;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;

/** Host download/extract/process implementation for BrowserStack Local. */
final class DefaultBrowserStackLocalToolchainOperations implements BrowserStackLocalToolchainOperations {
    private final ShaftCachePaths paths;
    private final SetupPlan plan;
    private final VerifiedArtifactStore store;
    private final boolean offline;

    DefaultBrowserStackLocalToolchainOperations(ShaftCachePaths paths, SetupPlan plan, boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.plan = java.util.Objects.requireNonNull(plan, "plan");
        this.store = new VerifiedArtifactStore(paths.downloads());
        this.offline = offline;
    }

    @Override
    public void hostPreflight(List<SetupAction> actions) {
        java.util.Objects.requireNonNull(actions, "actions");
    }

    @Override
    public void lockedPreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
        java.util.Objects.requireNonNull(actions, "actions");
        VerifiedArtifactStore.requireUnlinkedAncestors(binaryFile());
        if ((offline || requireOffline) && !Files.isRegularFile(binaryFile(), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Offline BrowserStack Local setup requires a staged binary.");
        }
    }

    @Override
    public void install(SetupAction action) throws IOException {
        if (action.target() != SetupTarget.BROWSERSTACK_LOCAL) {
            throw new IllegalArgumentException("Unsupported BrowserStack Local install target: " + action.target());
        }
        Path archive = store.fetch(action, offline);
        Path root = paths.tools().resolve("browserstack-local");
        Files.createDirectories(root);
        Path staging = Files.createTempDirectory(root, "extract-");
        try {
            SafeZipExtractor.extract(archive, staging);
            Path staged = staging.resolve(asset().executableName());
            if (!Files.isRegularFile(staged, LinkOption.NOFOLLOW_LINKS)) {
                throw new IOException("BrowserStack Local archive did not contain " + asset().executableName() + '.');
            }
            Path binary = binaryFile();
            Path quarantine = root.resolve(asset().executableName() + ".prev");
            VerifiedArtifactStore.replaceWithRollback(staged, binary, quarantine);
            Files.writeString(digestFile(), sha256(binary));
            if (plan.platform() != SetupPlatform.WINDOWS && !binary.toFile().setExecutable(true, false)) {
                throw new IOException("Unable to mark the BrowserStack Local binary executable: " + binary);
            }
        } finally {
            deleteTree(staging);
        }
    }

    @Override
    public SetupStatus status(SetupAction action) {
        try {
            Path binary = binaryFile();
            VerifiedArtifactStore.requireUnlinkedAncestors(binary);
            if (!Files.isRegularFile(binary, LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        "Managed BrowserStack Local binary is missing.");
            }
            Path digest = digestFile();
            if (!Files.isRegularFile(digest, LinkOption.NOFOLLOW_LINKS)
                    || !sha256(binary).equalsIgnoreCase(Files.readString(digest).trim())) {
                return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Staged BrowserStack Local binary does not match the install digest.");
            }
            store.fetch(action, true);
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(),
                    "Staged BrowserStack Local binary matches the reviewed plan.");
        } catch (IOException failure) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    @Override
    public long startTunnel(Path binary, String accessKey, Path logFile) throws IOException {
        if (!binary.equals(binaryFile())) {
            throw new IOException("Refusing to start an unowned BrowserStack Local binary.");
        }
        if (accessKey.isBlank()) throw new IOException("BrowserStack access key must not be blank.");
        Files.createDirectories(logFile.getParent());
        ProcessBuilder builder = new ProcessBuilder(startCommand(binary, accessKey))
                .directory(binary.getParent().toFile())
                .redirectErrorStream(true)
                .redirectOutput(logFile.toFile());
        Process process = builder.start();
        return process.pid();
    }

    static List<String> startCommand(Path binary, String accessKey) {
        return List.of(binary.toString(), "--key", java.util.Objects.requireNonNull(accessKey, "accessKey"));
    }

    @Override
    public boolean processRunning(long pid, Path binary) throws IOException {
        if (!binary.equals(binaryFile())) {
            throw new IOException("Refusing to inspect an unowned BrowserStack Local binary.");
        }
        Optional<ProcessHandle> handle = ProcessHandle.of(pid);
        if (handle.isEmpty() || !handle.orElseThrow().isAlive()) return false;
        Optional<String> command = handle.orElseThrow().info().command();
        if (command.isEmpty()) {
            throw new IOException("Unable to confirm BrowserStack Local process identity for pid " + pid + '.');
        }
        String observed = command.orElseThrow();
        return observed.equals(binary.toString()) || observed.endsWith(binary.getFileName().toString());
    }

    @Override
    public void stopProcess(long pid, Path binary) throws IOException {
        if (!binary.equals(binaryFile())) {
            throw new IOException("Refusing to stop an unowned BrowserStack Local binary.");
        }
        Optional<ProcessHandle> handle = ProcessHandle.of(pid);
        if (handle.isEmpty() || !handle.orElseThrow().isAlive()) return;
        ProcessHandle live = handle.orElseThrow();
        live.destroy();
        try {
            live.onExit().get(5, java.util.concurrent.TimeUnit.SECONDS);
        } catch (Exception waited) {
            live.destroyForcibly();
        }
    }

    @Override
    public void awaitReady(Duration timeout) throws IOException {
        Instant deadline = Instant.now().plus(timeout);
        IOException last = new IOException("BrowserStack Local did not become ready.");
        URI probe = URI.create("http://127.0.0.1:45691/");
        while (Instant.now().isBefore(deadline)) {
            try {
                HttpURLConnection connection = (HttpURLConnection) probe.toURL().openConnection();
                try {
                    connection.setConnectTimeout(1_000);
                    connection.setReadTimeout(1_000);
                    connection.setRequestMethod("GET");
                    int code = connection.getResponseCode();
                    if (code >= 200 && code < 500) return;
                    last = new IOException("BrowserStack Local status returned HTTP " + code + '.');
                } finally {
                    connection.disconnect();
                }
            } catch (IOException failure) {
                last = failure;
            }
            try {
                Thread.sleep(250);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while waiting for BrowserStack Local.", interrupted);
            }
        }
        throw last;
    }

    private Path binaryFile() {
        return paths.tools().resolve("browserstack-local").resolve(asset().executableName());
    }

    private Path digestFile() {
        return Path.of(binaryFile() + ".sha256");
    }

    private static String sha256(Path file) throws IOException {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(file)));
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }

    private BrowserStackLocalSetupPlanner.Asset asset() {
        return BrowserStackLocalSetupPlanner.asset(plan.platform(), plan.architecture());
    }

    private static void deleteTree(Path root) throws IOException {
        if (!Files.exists(root)) return;
        try (var walk = Files.walk(root)) {
            for (Path path : walk.sorted(Comparator.reverseOrder()).toList()) {
                Files.deleteIfExists(path);
            }
        }
    }
}
