package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.Comparator;
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
            Path cached = store.fetch(action, true);
            if (!Files.isRegularFile(cached, LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Verified BrowserStack Local archive is missing.");
            }
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
        return command.isEmpty() || command.orElseThrow().equals(binary.toString())
                || command.orElseThrow().endsWith(binary.getFileName().toString());
    }

    @Override
    public void stopProcess(long pid, Path binary) throws IOException {
        if (!binary.equals(binaryFile())) {
            throw new IOException("Refusing to stop an unowned BrowserStack Local binary.");
        }
        Optional<ProcessHandle> handle = ProcessHandle.of(pid);
        if (handle.isEmpty() || !handle.orElseThrow().isAlive()) return;
        handle.orElseThrow().destroy();
    }

    private Path binaryFile() {
        return paths.tools().resolve("browserstack-local").resolve(asset().executableName());
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
