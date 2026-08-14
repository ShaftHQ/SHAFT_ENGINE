package com.shaft.infrastructure;

import tools.jackson.databind.DeserializationFeature;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.Instant;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.LongSupplier;

/** Transactional owner of the release-matched Playwright browser installation. */
public final class PlaywrightSetupService {
    private static final JsonMapper JSON = JsonMapper.builder()
            .enable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES).build();
    private static final ConcurrentHashMap<Path, ReentrantLock> LOCKS = new ConcurrentHashMap<>();
    private final ShaftCachePaths paths;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;
    private final PlaywrightHostPlatform hostPlatform;
    private final NodeOwner nodeOwner;
    private final TimedArtifactFetcher timedArtifactFetcher;
    private final DriverExtractor driverExtractor;
    private final BrowserInstaller browserInstaller;
    private final LongSupplier nanoTime;
    private final Duration transactionTimeout;

    PlaywrightSetupService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                           NodeOwner nodeOwner, ArtifactFetcher artifactFetcher,
                           DriverExtractor driverExtractor, BrowserInstaller browserInstaller) {
        this(paths, PlaywrightHostPlatform.current(platform, architecture), architecture, nodeOwner,
                artifactFetcher, driverExtractor, browserInstaller);
    }

    PlaywrightSetupService(ShaftCachePaths paths, PlaywrightHostPlatform hostPlatform,
                           SetupArchitecture architecture, NodeOwner nodeOwner, ArtifactFetcher artifactFetcher,
                           DriverExtractor driverExtractor, BrowserInstaller browserInstaller) {
        this(paths, hostPlatform, architecture, nodeOwner,
                (action, ignored) -> artifactFetcher.fetch(action), driverExtractor, browserInstaller,
                System::nanoTime, Duration.ofMinutes(10));
    }

    PlaywrightSetupService(ShaftCachePaths paths, PlaywrightHostPlatform hostPlatform,
                           SetupArchitecture architecture, NodeOwner nodeOwner,
                           TimedArtifactFetcher artifactFetcher, DriverExtractor driverExtractor,
                           BrowserInstaller browserInstaller, LongSupplier nanoTime, Duration transactionTimeout) {
        this.paths = Objects.requireNonNull(paths, "paths");
        this.hostPlatform = Objects.requireNonNull(hostPlatform, "hostPlatform");
        this.platform = hostPlatform.platform();
        this.architecture = Objects.requireNonNull(architecture, "architecture");
        this.nodeOwner = Objects.requireNonNull(nodeOwner, "nodeOwner");
        this.timedArtifactFetcher = Objects.requireNonNull(artifactFetcher, "artifactFetcher");
        this.driverExtractor = Objects.requireNonNull(driverExtractor, "driverExtractor");
        this.browserInstaller = Objects.requireNonNull(browserInstaller, "browserInstaller");
        this.nanoTime = Objects.requireNonNull(nanoTime, "nanoTime");
        this.transactionTimeout = Objects.requireNonNull(transactionTimeout, "transactionTimeout");
        if (transactionTimeout.isZero() || transactionTimeout.isNegative()) {
            throw new IllegalArgumentException("transactionTimeout must be positive.");
        }
    }

    public SetupProfileStatus status() {
        SetupReadiness receiptReadiness = receiptReadiness();
        SetupReadiness node = nodeOwner.readiness();
        List<SetupStatus> browserStatuses = List.of(
                browserStatus(SetupTarget.PLAYWRIGHT_CHROMIUM,
                        hostPlatform.requiredPaths(SetupTarget.PLAYWRIGHT_CHROMIUM)),
                browserStatus(SetupTarget.PLAYWRIGHT_FIREFOX,
                        hostPlatform.requiredPaths(SetupTarget.PLAYWRIGHT_FIREFOX)),
                browserStatus(SetupTarget.PLAYWRIGHT_WEBKIT,
                        hostPlatform.requiredPaths(SetupTarget.PLAYWRIGHT_WEBKIT)),
                browserStatus(SetupTarget.FFMPEG, hostPlatform.requiredPaths(SetupTarget.FFMPEG)));
        boolean browsersReady = browserStatuses.stream()
                .allMatch(status -> status.readiness() == SetupReadiness.READY);
        SetupReadiness aggregate = node == SetupReadiness.READY && browsersReady
                && receiptReadiness == SetupReadiness.READY ? SetupReadiness.READY
                : node == SetupReadiness.DEGRADED || receiptReadiness == SetupReadiness.DEGRADED
                || browserStatuses.stream().anyMatch(status -> status.readiness() == SetupReadiness.DEGRADED)
                ? SetupReadiness.DEGRADED : SetupReadiness.MISSING;
        var targets = new java.util.ArrayList<SetupStatus>();
        targets.add(new SetupStatus(SetupTarget.NODE, node,
                node == SetupReadiness.READY ? ReportingSetupPlanner.NODE_VERSION : "",
                node == SetupReadiness.READY ? "Verified shared portable Node." : "Portable Node is not ready."));
        targets.addAll(browserStatuses);
        return new SetupProfileStatus(1, SetupProfile.PLAYWRIGHT, aggregate, targets);
    }

    public SetupReceipt install(SetupPlan plan, SetupApproval approval) throws IOException {
        requireCompatible(plan);
        SetupExecutor.validate(plan, approval);
        Path lockPath = paths.state().resolve("playwright.lock").toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        VerifiedArtifactStore.requireUnlinkedAncestors(browserRoot());
        ReentrantLock jvmLock = LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        try {
            jvmLock.lockInterruptibly();
            Files.createDirectories(paths.state());
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                recoverInterruptedPublication();
                SetupReceipt existing = readyReceipt();
                if (existing != null) return existing;
                return installLocked(plan);
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the Playwright setup lock.", interrupted);
        } finally {
            if (jvmLock.isHeldByCurrentThread()) jvmLock.unlock();
        }
    }

    public Path browserRoot() {
        return browserRoot(paths, hostPlatform);
    }

    static Path browserRoot(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture) {
        return browserRoot(paths, PlaywrightHostPlatform.current(platform, architecture));
    }

    static Path browserRoot(ShaftCachePaths paths, PlaywrightHostPlatform hostPlatform) {
        return paths.tools().resolve("playwright").resolve(PlaywrightSetupPlanner.PLAYWRIGHT_VERSION)
                .resolve(hostPlatform.token());
    }

    private SetupReceipt installLocked(SetupPlan plan) throws IOException {
        SetupAction nodeAction = plan.actions().getFirst();
        if (nodeOwner.readiness() != SetupReadiness.READY) nodeOwner.install(nodeAction);
        if (nodeOwner.readiness() != SetupReadiness.READY) {
            throw new IOException("Portable Node did not become ready for Playwright installation.");
        }
        long deadline = Math.addExact(nanoTime.getAsLong(), transactionTimeout.toNanos());

        PlaywrightArtifactManifest manifest = PlaywrightArtifactManifest.load();
        if (!PlaywrightSetupPlanner.PLAYWRIGHT_VERSION.equals(manifest.playwrightVersion())) {
            throw new IOException("Playwright artifact manifest version does not match the release planner.");
        }
        String lockChecksum = plan.actions().get(1).dependencyLockChecksum();
        Map<PlaywrightArtifactManifest.Artifact, Path> archives = new LinkedHashMap<>();
        for (PlaywrightArtifactManifest.Artifact artifact : manifest.requirePlatform(hostPlatform.token())) {
            try {
                archives.put(artifact, timedArtifactFetcher.fetch(
                        artifactAction(artifact, lockChecksum), remaining(deadline)));
            } catch (IOException failure) {
                SetupAction failedAction = publicAction(plan, artifact);
                SetupReceipt partial = new SetupReceipt(plan.digest(), Instant.now(), List.of(nodeAction));
                throw new SetupExecutionException(failedAction, partial, new RuntimeException(failure));
            }
        }

        Path destination = browserRoot();
        Files.createDirectories(destination.getParent());
        Path staging = Files.createTempDirectory(destination.getParent(), hostPlatform.token() + ".staging-");
        Path driverRoot = destination.getParent().resolve(".driver-" + java.util.UUID.randomUUID());
        try {
            try {
                driverExtractor.extract(nodeOwner.executable(), driverRoot);
                Duration installerTimeout = remaining(deadline);
                browserInstaller.install(nodeOwner.executable(), driverRoot, staging, archives,
                        paths.state().resolve("logs/playwright-install.log"), installerTimeout);
                Files.writeString(staging.resolve("SHAFT_PLAYWRIGHT_VERSION"),
                        PlaywrightSetupPlanner.PLAYWRIGHT_VERSION + System.lineSeparator() + lockChecksum);
                requireReadyLayout(staging);
                SetupReceipt receipt = new SetupReceipt(plan.digest(), Instant.now(), plan.actions());
                publishWithReceipt(staging, destination, receipt);
                return receipt;
            } catch (IOException failure) {
                SetupReceipt partial = new SetupReceipt(plan.digest(), Instant.now(), List.of(nodeAction));
                throw new SetupExecutionException(plan.actions().get(1), partial, new RuntimeException(failure));
            }
        } finally {
            deleteTree(driverRoot);
            deleteTree(staging);
        }
    }

    private SetupAction artifactAction(PlaywrightArtifactManifest.Artifact artifact, String lockChecksum) {
        SetupTarget target = switch (artifact.name()) {
            case "firefox" -> SetupTarget.PLAYWRIGHT_FIREFOX;
            case "webkit" -> SetupTarget.PLAYWRIGHT_WEBKIT;
            case "ffmpeg" -> SetupTarget.FFMPEG;
            default -> SetupTarget.PLAYWRIGHT_CHROMIUM;
        };
        return new SetupAction(target, SetupActionKind.INSTALL,
                PlaywrightSetupPlanner.PLAYWRIGHT_VERSION + ':' + artifact.name() + '-' + artifact.revision(),
                artifact.source(), artifact.checksum(), lockChecksum, false, java.util.Set.of());
    }

    private Duration remaining(long deadline) throws IOException {
        long remaining = deadline - nanoTime.getAsLong();
        if (remaining <= 0) throw new IOException("Playwright setup exceeded its shared transaction deadline.");
        return Duration.ofNanos(remaining);
    }

    private SetupAction publicAction(SetupPlan plan, PlaywrightArtifactManifest.Artifact artifact) {
        SetupTarget target = switch (artifact.name()) {
            case "firefox" -> SetupTarget.PLAYWRIGHT_FIREFOX;
            case "webkit" -> SetupTarget.PLAYWRIGHT_WEBKIT;
            case "ffmpeg" -> SetupTarget.FFMPEG;
            default -> SetupTarget.PLAYWRIGHT_CHROMIUM;
        };
        return plan.actions().stream().filter(action -> action.target() == target).findFirst().orElseThrow();
    }

    private SetupStatus browserStatus(SetupTarget target, List<String> requiredPaths) {
        try {
            VerifiedArtifactStore.requireUnlinkedAncestors(browserRoot());
            if (Files.notExists(browserRoot(), java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(target, SetupReadiness.MISSING, "", "Not installed.");
            }
            boolean ready = true;
            for (String required : requiredPaths) {
                Path path = browserRoot().resolve(required).normalize();
                VerifiedArtifactStore.requireUnlinkedAncestors(path);
                if (!path.startsWith(browserRoot())
                        || !Files.isRegularFile(path, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                        || (hostPlatform.requiresExecutablePermission()
                        && !required.endsWith("INSTALLATION_COMPLETE") && !Files.isExecutable(path))) {
                    ready = false;
                    break;
                }
            }
            return new SetupStatus(target, ready ? SetupReadiness.READY : SetupReadiness.DEGRADED,
                    ready ? PlaywrightSetupPlanner.PLAYWRIGHT_VERSION : "",
                    ready ? "Verified release-matched Playwright payload." : "Managed browser payload is incomplete.");
        } catch (IOException failure) {
            return new SetupStatus(target, SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    private void requireReadyLayout(Path root) throws IOException {
        Path original = browserRoot();
        for (String required : List.of(SetupTarget.PLAYWRIGHT_CHROMIUM, SetupTarget.PLAYWRIGHT_FIREFOX,
                SetupTarget.PLAYWRIGHT_WEBKIT, SetupTarget.FFMPEG).stream()
                .flatMap(target -> hostPlatform.requiredPaths(target).stream()).toList()) {
            Path file = root.resolve(required).normalize();
            VerifiedArtifactStore.requireUnlinkedAncestors(file);
            if (!file.startsWith(root) || !Files.isRegularFile(file, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                throw new IOException("Playwright installation did not publish required payload: " + required);
            }
            if (hostPlatform.requiresExecutablePermission() && !required.endsWith("INSTALLATION_COMPLETE")
                    && !Files.isExecutable(file)) {
                throw new IOException("Playwright installation did not publish an executable payload: " + required);
            }
        }
        if (!original.equals(browserRoot())) throw new IllegalStateException("Playwright browser root changed.");
    }

    private SetupReadiness receiptReadiness() {
        Path receipt = receiptPath();
        if (Files.notExists(receipt, java.nio.file.LinkOption.NOFOLLOW_LINKS)) return SetupReadiness.MISSING;
        try {
            VerifiedArtifactStore.requireUnlinkedAncestors(receipt);
            SetupReceipt value = JSON.readValue(Files.readString(receipt), SetupReceipt.class);
            SetupPlan expected = PlaywrightSetupPlanner.plan(hostPlatform, architecture, SetupMode.MANAGED);
            return value.planDigest().equals(expected.digest()) && value.completedActions().equals(expected.actions())
                    ? SetupReadiness.READY : SetupReadiness.DEGRADED;
        } catch (IOException | RuntimeException failure) {
            return SetupReadiness.DEGRADED;
        }
    }

    private SetupReceipt readyReceipt() throws IOException {
        if (status().readiness() != SetupReadiness.READY) return null;
        return JSON.readValue(Files.readString(receiptPath()), SetupReceipt.class);
    }

    private Path prepareReceipt(SetupReceipt receipt) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.receipts());
        Files.createDirectories(paths.receipts());
        Path temporary = Files.createTempFile(paths.receipts(), "playwright", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(receipt));
            return temporary;
        } catch (IOException | RuntimeException failure) {
            Files.deleteIfExists(temporary);
            throw failure;
        }
    }

    private void publishWithReceipt(Path staging, Path destination, SetupReceipt receipt) throws IOException {
        Path temporaryReceipt = prepareReceipt(receipt);
        Path receiptDestination = receiptPath();
        Path browserQuarantine = destination.resolveSibling(destination.getFileName() + ".quarantine");
        Path receiptQuarantine = receiptDestination.resolveSibling(receiptDestination.getFileName() + ".quarantine");
        boolean hadBrowser = Files.exists(destination, java.nio.file.LinkOption.NOFOLLOW_LINKS);
        boolean hadReceipt = Files.exists(receiptDestination, java.nio.file.LinkOption.NOFOLLOW_LINKS);
        boolean browserPublished = false;
        boolean receiptPublished = false;
        try {
            recoverQuarantine(browserQuarantine, destination);
            recoverQuarantine(receiptQuarantine, receiptDestination);
            hadBrowser = Files.exists(destination, java.nio.file.LinkOption.NOFOLLOW_LINKS);
            hadReceipt = Files.exists(receiptDestination, java.nio.file.LinkOption.NOFOLLOW_LINKS);
            if (hadBrowser) VerifiedArtifactStore.move(destination, browserQuarantine);
            if (hadReceipt) VerifiedArtifactStore.move(receiptDestination, receiptQuarantine);
            VerifiedArtifactStore.move(staging, destination);
            browserPublished = true;
            VerifiedArtifactStore.move(temporaryReceipt, receiptDestination);
            receiptPublished = true;
            deleteTree(browserQuarantine);
            Files.deleteIfExists(receiptQuarantine);
        } catch (IOException failure) {
            if (receiptPublished) suppress(failure, () -> Files.deleteIfExists(receiptDestination));
            if (browserPublished) suppress(failure, () -> deleteTree(destination));
            if (hadReceipt && Files.exists(receiptQuarantine, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                suppress(failure, () -> VerifiedArtifactStore.move(receiptQuarantine, receiptDestination));
            }
            if (hadBrowser && Files.exists(browserQuarantine, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                suppress(failure, () -> VerifiedArtifactStore.move(browserQuarantine, destination));
            }
            throw failure;
        } finally {
            Files.deleteIfExists(temporaryReceipt);
        }
    }

    private void recoverInterruptedPublication() throws IOException {
        Path destination = browserRoot();
        Path receiptDestination = receiptPath();
        recoverQuarantine(destination.resolveSibling(destination.getFileName() + ".quarantine"), destination);
        recoverQuarantine(receiptDestination.resolveSibling(receiptDestination.getFileName() + ".quarantine"),
                receiptDestination);
    }

    private static void recoverQuarantine(Path quarantine, Path destination) throws IOException {
        if (!Files.exists(quarantine, java.nio.file.LinkOption.NOFOLLOW_LINKS)) return;
        deleteTree(destination);
        VerifiedArtifactStore.move(quarantine, destination);
    }

    private static void suppress(IOException primary, IoOperation rollback) {
        try {
            rollback.run();
        } catch (IOException rollbackFailure) {
            primary.addSuppressed(rollbackFailure);
        }
    }

    private Path receiptPath() {
        return paths.receipts().resolve("playwright.json");
    }

    private void requireCompatible(SetupPlan plan) {
        Objects.requireNonNull(plan, "plan");
        if (plan.profile() != SetupProfile.PLAYWRIGHT || plan.platform() != platform
                || plan.architecture() != architecture || plan.mode() == SetupMode.EXTERNAL
                || !plan.equals(PlaywrightSetupPlanner.plan(hostPlatform, architecture, plan.mode()))) {
            throw new IllegalArgumentException("Plan does not match the Playwright manifest shipped with this release.");
        }
    }

    private static void deleteTree(Path root) throws IOException {
        if (root == null || Files.notExists(root, java.nio.file.LinkOption.NOFOLLOW_LINKS)) return;
        try (var entries = Files.walk(root)) {
            for (Path entry : entries.sorted(Comparator.reverseOrder()).toList()) Files.deleteIfExists(entry);
        }
    }

    interface NodeOwner {
        SetupReadiness readiness();
        void install(SetupAction action) throws IOException;
        Path executable();
    }

    @FunctionalInterface
    interface ArtifactFetcher {
        Path fetch(SetupAction action) throws IOException;
    }

    @FunctionalInterface
    interface TimedArtifactFetcher {
        Path fetch(SetupAction action, Duration timeout) throws IOException;
    }

    @FunctionalInterface
    interface DriverExtractor {
        void extract(Path nodeExecutable, Path destination) throws IOException;
    }

    @FunctionalInterface
    interface BrowserInstaller {
        void install(Path nodeExecutable, Path driverRoot, Path browserRoot,
                     Map<PlaywrightArtifactManifest.Artifact, Path> archives,
                     Path log, Duration timeout) throws IOException;
    }

    @FunctionalInterface
    private interface IoOperation {
        void run() throws IOException;
    }
}
