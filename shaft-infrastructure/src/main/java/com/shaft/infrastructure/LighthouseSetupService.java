package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HexFormat;
import java.util.List;
import java.util.regex.Pattern;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/** Managed, release-pinned installation and verification for Lighthouse. */
final class LighthouseSetupService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();
    private static final Pattern LOCK_INTEGRITY = Pattern.compile("\\\"integrity\\\"\\s*:\\s*\\\"sha512-([^\\\"]+)\\\"");
    private final ShaftCachePaths paths;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;
    private final ReportingSetupService.ArtifactFetcher fetcher;
    private final ReportingSetupService.CommandRunner runner;
    private final ReportingSetupService nodeService;
    private final boolean offline;

    LighthouseSetupService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                           ReportingSetupService.ArtifactFetcher fetcher,
                           ReportingSetupService.CommandRunner runner, boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.platform = java.util.Objects.requireNonNull(platform, "platform");
        this.architecture = java.util.Objects.requireNonNull(architecture, "architecture");
        this.fetcher = java.util.Objects.requireNonNull(fetcher, "fetcher");
        this.runner = java.util.Objects.requireNonNull(runner, "runner");
        this.nodeService = new ReportingSetupService(paths, platform, architecture, fetcher, runner, offline);
        this.offline = offline;
    }

    SetupProfileStatus status() {
        SetupStatus node = nodeService.nodeStatus();
        SetupStatus lighthouse = probeLighthouse();
        SetupReadiness readiness = node.readiness() == SetupReadiness.DEGRADED
                || lighthouse.readiness() == SetupReadiness.DEGRADED ? SetupReadiness.DEGRADED
                : node.readiness() == SetupReadiness.READY && lighthouse.readiness() == SetupReadiness.READY
                ? SetupReadiness.READY : SetupReadiness.MISSING;
        return new SetupProfileStatus(1, SetupProfile.LIGHTHOUSE, readiness, List.of(node, lighthouse));
    }

    SetupReceipt install(SetupPlan plan, SetupApproval approval) throws IOException {
        requireCompatible(plan);
        SetupExecutor.validate(plan, approval);
        preflightOfflineArtifacts(plan);
        requireSafePaths();
        Path lockPath = paths.state().resolve("lighthouse.lock").toAbsolutePath().normalize();
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        try {
            jvmLock.lockInterruptibly();
            Files.createDirectories(paths.state());
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                SetupReceipt receipt = SetupExecutor.execute(plan, approval, action -> {
                    try {
                        if (action.target() == SetupTarget.NODE) nodeService.installNodeAction(action);
                        else if (action.target() == SetupTarget.LIGHTHOUSE) installLighthouse(action);
                        else throw new IllegalArgumentException("Lighthouse provider cannot install " + action.target());
                    } catch (IOException failure) {
                        throw new SetupOperationException(failure);
                    }
                });
                writeReceipt(receipt);
                return receipt;
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the Lighthouse setup lock.", interrupted);
        } finally {
            if (jvmLock.isHeldByCurrentThread()) jvmLock.unlock();
        }
    }

    private void preflightOfflineArtifacts(SetupPlan plan) throws IOException {
        if (!offline) return;
        boolean nodeInstalled = structurallyInstalledNode();
        boolean lighthouseInstalled = structurallyInstalledLighthouse();
        for (SetupAction action : plan.actions()) {
            boolean installed = action.target() == SetupTarget.NODE
                    ? nodeInstalled
                    : action.target() == SetupTarget.LIGHTHOUSE && lighthouseInstalled;
            if (!installed) fetcher.fetch(action);
        }
        if (!lighthouseInstalled) requireCompleteOfflineNpmCache();
    }

    private boolean structurallyInstalledNode() throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(nodeRoot());
        return Files.isRegularFile(nodeExecutable(), LinkOption.NOFOLLOW_LINKS);
    }

    private boolean structurallyInstalledLighthouse() throws IOException {
        Path root = lighthouseRoot();
        Path entry = lighthouseEntryPoint(root);
        Path lock = root.resolve("package-lock.json");
        Path receipt = paths.receipts().resolve("lighthouse.json");
        for (Path path : List.of(root, entry, lock, receipt)) VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.isRegularFile(entry, LinkOption.NOFOLLOW_LINKS)
                || !Files.isRegularFile(lock, LinkOption.NOFOLLOW_LINKS)
                || !Files.isRegularFile(receipt, LinkOption.NOFOLLOW_LINKS)
                || !VerifiedArtifactStore.digest(lock).equalsIgnoreCase(LighthouseSetupPlanner.LIGHTHOUSE_LOCK_SHA256)) {
            return false;
        }
        SetupReceipt saved = JSON.readValue(receipt.toFile(), SetupReceipt.class);
        return List.of(SetupMode.MANAGED, SetupMode.HYBRID).stream().anyMatch(mode -> {
            SetupPlan expected = LighthouseSetupPlanner.plan(platform, architecture, mode);
            return saved.planDigest().equals(expected.digest()) && saved.completedActions().equals(expected.actions());
        });
    }

    private void requireCompleteOfflineNpmCache() throws IOException {
        String lock;
        try (InputStream input = LighthouseSetupService.class.getResourceAsStream(
                "/com/shaft/infrastructure/lighthouse/package-lock.json")) {
            if (input == null) throw new IOException("Missing bundled Lighthouse dependency lock.");
            lock = new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
        Path contentRoot = paths.cacheRoot().resolve("npm/_cacache/content-v2/sha512");
        VerifiedArtifactStore.requireUnlinkedAncestors(contentRoot);
        var matcher = LOCK_INTEGRITY.matcher(lock);
        int checked = 0;
        while (matcher.find()) {
            byte[] digest;
            try {
                digest = Base64.getDecoder().decode(matcher.group(1));
            } catch (IllegalArgumentException malformed) {
                throw new IOException("Bundled Lighthouse lock contains an invalid integrity value.", malformed);
            }
            String hex = HexFormat.of().formatHex(digest);
            Path content = contentRoot.resolve(hex.substring(0, 2)).resolve(hex.substring(2, 4))
                    .resolve(hex.substring(4));
            VerifiedArtifactStore.requireUnlinkedAncestors(content);
            if (!Files.isRegularFile(content, LinkOption.NOFOLLOW_LINKS)
                    || !hex.equalsIgnoreCase(sha512(content))) {
                throw new IOException("Lighthouse's transitive npm cache is incomplete for offline installation: "
                        + content.getFileName());
            }
            checked++;
        }
        if (checked == 0) throw new IOException("Bundled Lighthouse lock contains no integrity-bound packages.");
    }

    private static String sha512(Path path) throws IOException {
        try {
            var digest = java.security.MessageDigest.getInstance("SHA-512");
            try (InputStream input = Files.newInputStream(path)) {
                byte[] buffer = new byte[64 * 1024];
                for (int read; (read = input.read(buffer)) >= 0;) digest.update(buffer, 0, read);
            }
            return HexFormat.of().formatHex(digest.digest());
        } catch (java.security.NoSuchAlgorithmException impossible) {
            throw new IllegalStateException(impossible);
        }
    }

    private void requireCompatible(SetupPlan plan) {
        if (plan.profile() != SetupProfile.LIGHTHOUSE) throw new IllegalArgumentException("Not a Lighthouse plan.");
        if (plan.platform() != platform || plan.architecture() != architecture) {
            throw new IllegalArgumentException("Plan platform does not match this host.");
        }
        if (plan.mode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External plans are diagnostic and cannot be installed.");
        }
        if (!LighthouseSetupPlanner.plan(platform, architecture, plan.mode()).equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the Lighthouse manifest shipped with this release.");
        }
    }

    private void installLighthouse(SetupAction action) throws IOException {
        if (probeLighthouse().readiness() == SetupReadiness.READY) return;
        if (nodeService.nodeStatus().readiness() != SetupReadiness.READY) {
            throw new IOException("Portable Node must be ready before Lighthouse installation.");
        }
        Path archive = fetcher.fetch(action);
        Path destination = lighthouseRoot();
        Files.createDirectories(destination.getParent());
        Path staging = Files.createTempDirectory(destination.getParent(), "lighthouse.staging-");
        try {
            copyManifest(staging, action.dependencyLockChecksum());
            Path log = logFile();
            Files.createDirectories(log.getParent());
            List<String> cache = new ArrayList<>(List.of(nodeExecutable().toString(), npmCli().toString(),
                    "cache", "add", archive.toString()));
            if (offline) cache.add("--offline");
            requireSuccess(runner.run(cache, log, Duration.ofMinutes(2)), "Lighthouse cache preparation failed");
            List<String> install = new ArrayList<>(List.of(nodeExecutable().toString(), npmCli().toString(), "ci",
                    "--prefix", staging.toString(), "--ignore-scripts", "--no-audit", "--no-fund"));
            if (offline) install.add("--offline");
            requireSuccess(runner.run(install, log, Duration.ofMinutes(5)), "Lighthouse npm installation failed");
            ReportingSetupService.ProcessResult verification = runner.run(List.of(nodeExecutable().toString(),
                    lighthouseEntryPoint(staging).toString(), "--version"), log, Duration.ofSeconds(30));
            requireSuccess(verification, "Lighthouse verification failed");
            if (!LighthouseSetupPlanner.LIGHTHOUSE_VERSION.equals(verification.output().trim())) {
                throw new IOException("Lighthouse verification returned unexpected version: "
                        + verification.output().trim());
            }
            ReportingSetupService.publish(staging, destination, VerifiedArtifactStore::move);
        } finally {
            deleteTree(staging);
        }
    }

    private SetupStatus probeLighthouse() {
        Path entry = lighthouseEntryPoint(lighthouseRoot());
        Path lock = lighthouseRoot().resolve("package-lock.json");
        try {
            VerifiedArtifactStore.requireUnlinkedAncestors(lighthouseRoot());
            if (!Files.isRegularFile(entry, LinkOption.NOFOLLOW_LINKS)
                    || !Files.isRegularFile(lock, LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.MISSING, "", "Not installed.");
            }
            if (!VerifiedArtifactStore.digest(lock).equalsIgnoreCase(LighthouseSetupPlanner.LIGHTHOUSE_LOCK_SHA256)) {
                return new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.DEGRADED, "",
                        "Managed dependency lock does not match this release.");
            }
            Path receiptPath = paths.receipts().resolve("lighthouse.json");
            VerifiedArtifactStore.requireUnlinkedAncestors(receiptPath);
            if (!Files.isRegularFile(receiptPath, LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.DEGRADED, "",
                        "Managed Lighthouse receipt is missing.");
            }
            SetupReceipt receipt = JSON.readValue(receiptPath.toFile(), SetupReceipt.class);
            boolean matchesRelease = List.of(SetupMode.MANAGED, SetupMode.HYBRID).stream().anyMatch(mode -> {
                SetupPlan expected = LighthouseSetupPlanner.plan(platform, architecture, mode);
                return receipt.planDigest().equals(expected.digest())
                        && receipt.completedActions().equals(expected.actions());
            });
            if (!matchesRelease) {
                return new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.DEGRADED, "",
                        "Managed Lighthouse receipt does not match this release.");
            }
            ReportingSetupService.ProcessResult result = runner.run(List.of(nodeExecutable().toString(),
                    entry.toString(), "--version"), null, Duration.ofSeconds(15));
            String version = result.output().trim();
            return result.exitCode() == 0 && version.equals(LighthouseSetupPlanner.LIGHTHOUSE_VERSION)
                    ? new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.READY, version,
                    "Verified managed installation.")
                    : new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.DEGRADED, version,
                    "Version or execution check failed.");
        } catch (IOException failure) {
            return new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    private void copyManifest(Path staging, String expectedChecksum) throws IOException {
        for (String name : List.of("package.json", "package-lock.json")) {
            try (InputStream input = LighthouseSetupService.class.getResourceAsStream(
                    "/com/shaft/infrastructure/lighthouse/" + name)) {
                if (input == null) throw new IOException("Missing bundled Lighthouse manifest: " + name);
                byte[] content = input.readAllBytes();
                if (name.endsWith("lock.json")) content = new String(content, StandardCharsets.UTF_8)
                        .replace("\r\n", "\n").replace('\r', '\n').getBytes(StandardCharsets.UTF_8);
                Files.write(staging.resolve(name), content);
            }
        }
        if (!("sha256:" + VerifiedArtifactStore.digest(staging.resolve("package-lock.json")))
                .equalsIgnoreCase(expectedChecksum)) {
            throw new IOException("Bundled Lighthouse dependency lock does not match the approved plan.");
        }
    }

    private void requireSafePaths() throws IOException {
        for (Path path : List.of(paths.cacheRoot(), paths.dataRoot(), paths.downloads(), paths.tools(),
                paths.state(), paths.receipts(), nodeRoot(), nodeExecutable(), lighthouseRoot())) {
            VerifiedArtifactStore.requireUnlinkedAncestors(path);
        }
    }

    private void writeReceipt(SetupReceipt receipt) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.receipts());
        Files.createDirectories(paths.receipts());
        Path temporary = Files.createTempFile(paths.receipts(), "lighthouse", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(receipt));
            VerifiedArtifactStore.move(temporary, paths.receipts().resolve("lighthouse.json"));
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private Path lighthouseRoot() {
        return paths.tools().resolve("lighthouse").resolve(LighthouseSetupPlanner.LIGHTHOUSE_VERSION);
    }

    private Path nodeRoot() {
        return paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
    }

    private Path nodeExecutable() {
        return platform == SetupPlatform.WINDOWS ? nodeRoot().resolve("node.exe") : nodeRoot().resolve("bin/node");
    }

    private Path npmCli() {
        return platform == SetupPlatform.WINDOWS ? nodeRoot().resolve("node_modules/npm/bin/npm-cli.js")
                : nodeRoot().resolve("lib/node_modules/npm/bin/npm-cli.js");
    }

    private static Path lighthouseEntryPoint(Path root) {
        return root.resolve("node_modules/lighthouse/cli/index.js");
    }

    private Path logFile() {
        return paths.state().resolve("logs/lighthouse-install.log");
    }

    private static void requireSuccess(ReportingSetupService.ProcessResult result, String message) throws IOException {
        if (result.exitCode() != 0) throw new IOException(message + System.lineSeparator() + result.output());
    }

    private static void deleteTree(Path root) throws IOException {
        if (root == null || Files.notExists(root)) return;
        try (var stream = Files.walk(root)) {
            for (Path path : stream.sorted(java.util.Comparator.reverseOrder()).toList()) Files.deleteIfExists(path);
        }
    }

    private static final class SetupOperationException extends RuntimeException {
        private SetupOperationException(IOException cause) { super(cause); }
    }
}
