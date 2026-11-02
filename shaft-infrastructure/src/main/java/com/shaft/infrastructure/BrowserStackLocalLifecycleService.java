package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

/** Lease-safe lifecycle owner for one verified SHAFT BrowserStack Local process. */
final class BrowserStackLocalLifecycleService {
    static final String ACCESS_KEY_ENV = "BROWSERSTACK_ACCESS_KEY";
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();

    private final ShaftCachePaths paths;
    private final SetupPlan expectedPlan;
    private final BrowserStackLocalToolchainOperations operations;
    private final Supplier<String> accessKey;

    BrowserStackLocalLifecycleService(ShaftCachePaths paths, SetupPlan expectedPlan,
                                      BrowserStackLocalToolchainOperations operations) {
        this(paths, expectedPlan, operations, () -> System.getenv(ACCESS_KEY_ENV));
    }

    BrowserStackLocalLifecycleService(ShaftCachePaths paths, SetupPlan expectedPlan,
                                      BrowserStackLocalToolchainOperations operations,
                                      Supplier<String> accessKey) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.expectedPlan = java.util.Objects.requireNonNull(expectedPlan, "expectedPlan");
        this.operations = java.util.Objects.requireNonNull(operations, "operations");
        this.accessKey = java.util.Objects.requireNonNull(accessKey, "accessKey");
        if (expectedPlan.profile() != SetupProfile.BROWSERSTACK_LOCAL) {
            throw new IllegalArgumentException("BrowserStack Local lifecycle requires BROWSERSTACK_LOCAL.");
        }
    }

    ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        requireCompatible(plan, options);
        SetupExecutor.validate(plan, approval);
        SetupReceipt receipt = requireInstallReceipt(plan);
        requireInstalled(plan);
        String accessKey = requireAccessKey(this.accessKey.get());
        Path lockPath = lockPath();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        try {
            jvmLock.lockInterruptibly();
            Files.createDirectories(paths.state());
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                receipt = requireInstallReceipt(plan);
                requireInstalled(plan);
                Optional<BrowserStackLocalRuntimeLease> reusable = readReusable(plan, options);
                if (reusable.isPresent()) return environment(receipt, reusable.orElseThrow());
                return startNew(plan, receipt, accessKey, options.startupTimeout());
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the BrowserStack Local runtime lock.", interrupted);
        } finally {
            if (jvmLock.isHeldByCurrentThread()) jvmLock.unlock();
        }
    }

    boolean stop(Duration timeout) throws IOException {
        java.util.Objects.requireNonNull(timeout, "timeout");
        Path lockPath = lockPath();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        if (Files.notExists(leasePath(), LinkOption.NOFOLLOW_LINKS)) return false;
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<BrowserStackLocalRuntimeLease> current = readLease();
            if (current.isEmpty()) return false;
            BrowserStackLocalRuntimeLease lease = current.orElseThrow();
            requireLeaseIdentity(lease);
            if (!operations.processRunning(lease.pid(), Path.of(lease.binary()))) {
                Files.deleteIfExists(leasePath());
                return false;
            }
            if (lease.refCount() > 1) {
                writeLease(lease.withRefCount(lease.refCount() - 1));
                return true;
            }
            operations.stopProcess(lease.pid(), Path.of(lease.binary()), timeout);
            Files.deleteIfExists(leasePath());
            return true;
        } finally {
            jvmLock.unlock();
        }
    }

    String logs() throws IOException {
        Optional<BrowserStackLocalRuntimeLease> current = readLease();
        if (current.isEmpty()) return "";
        BrowserStackLocalRuntimeLease lease = current.orElseThrow();
        if (!lease.sameRuntime(expectedIdentity(expectedPlan, lease.pid()))) {
            throw new IOException("BrowserStack Local lease does not match the requested runtime.");
        }
        return OwnedLogReader.read("BrowserStack Local", logFile());
    }

    Path logFile() {
        return paths.state().resolve("logs").resolve("browserstack-local.log");
    }

    private ManagedEnvironment startNew(SetupPlan plan, SetupReceipt receipt, String accessKey, Duration timeout)
            throws IOException {
        Path binary = binaryFile();
        long pid = 0;
        try {
            pid = operations.startTunnel(binary, accessKey, logFile());
            if (!operations.processRunning(pid, binary)) {
                throw new IOException("BrowserStack Local process exited before it could be leased.");
            }
            operations.awaitReady(timeout);
            BrowserStackLocalRuntimeLease lease = new BrowserStackLocalRuntimeLease(1, plan.digest(), pid,
                    binary.toString(), 1);
            writeLease(lease);
            return environment(receipt, lease);
        } catch (IOException | RuntimeException failure) {
            if (pid > 0) {
                try {
                    operations.stopProcess(pid, binary, timeout);
                } catch (IOException cleanup) {
                    failure.addSuppressed(cleanup);
                }
            }
            throw failure;
        }
    }

    private Optional<BrowserStackLocalRuntimeLease> readReusable(SetupPlan plan, SetupOptions options)
            throws IOException {
        Optional<BrowserStackLocalRuntimeLease> existing = readLease();
        if (existing.isEmpty()) return Optional.empty();
        BrowserStackLocalRuntimeLease lease = existing.orElseThrow();
        if (!lease.planDigest().equals(plan.digest()) || !lease.sameIdentity(expectedIdentity(plan, lease.pid()))) {
            throw new IOException("An existing SHAFT BrowserStack Local lease does not match the reviewed plan.");
        }
        if (!operations.processRunning(lease.pid(), Path.of(lease.binary()))) {
            Files.deleteIfExists(leasePath());
            return Optional.empty();
        }
        if (!options.reuseOwnedProcesses()) {
            throw new IOException("A compatible SHAFT BrowserStack Local process is already active and reuse is disabled.");
        }
        BrowserStackLocalRuntimeLease incremented = lease.withRefCount(lease.refCount() + 1);
        writeLease(incremented);
        return Optional.of(incremented);
    }

    private BrowserStackLocalRuntimeLease expectedIdentity(SetupPlan plan, long pid) {
        return new BrowserStackLocalRuntimeLease(1, plan.digest(), pid, binaryFile().toString(), 1);
    }

    private ManagedEnvironment environment(SetupReceipt receipt, BrowserStackLocalRuntimeLease lease) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("local", "true");
        properties.put("binary", lease.binary());
        return new ManagedEnvironment(SetupProfile.BROWSERSTACK_LOCAL, receipt,
                Optional.of(URI.create("urn:shaft:browserstack-local:" + lease.pid())), Map.copyOf(properties), () -> {
                    try {
                        release(lease);
                    } catch (IOException failure) {
                        throw new IllegalStateException("Failed to release the owned BrowserStack Local process.",
                                failure);
                    }
                });
    }

    private void release(BrowserStackLocalRuntimeLease started) throws IOException {
        Path lockPath = lockPath();
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<BrowserStackLocalRuntimeLease> current = readLease();
            if (current.isEmpty()) return;
            BrowserStackLocalRuntimeLease lease = current.orElseThrow();
            if (!lease.sameIdentity(started)) {
                throw new IOException("BrowserStack Local lease changed; refusing to stop an unowned process.");
            }
            if (lease.refCount() > 1) {
                writeLease(lease.withRefCount(lease.refCount() - 1));
                return;
            }
            operations.stopProcess(lease.pid(), Path.of(lease.binary()), Duration.ofSeconds(5));
            Files.deleteIfExists(leasePath());
        } finally {
            jvmLock.unlock();
        }
    }

    private void requireCompatible(SetupPlan plan, SetupOptions options) {
        if (plan.profile() != SetupProfile.BROWSERSTACK_LOCAL
                || options.profile() != SetupProfile.BROWSERSTACK_LOCAL) {
            throw new IllegalArgumentException("BrowserStack Local lifecycle requires profile BROWSERSTACK_LOCAL.");
        }
        if (plan.mode() == SetupMode.EXTERNAL || options.effectiveMode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External BrowserStack Local setup cannot start a managed tunnel.");
        }
        if (!expectedPlan.equals(plan)) {
            throw new IllegalArgumentException("BrowserStack Local lifecycle plan does not match the release manifest.");
        }
    }

    static String requireAccessKey(String key) throws IOException {
        if (key == null || key.isBlank()) {
            throw new IOException("BROWSERSTACK_ACCESS_KEY must be set in the environment before start.");
        }
        return key;
    }

    private SetupReceipt requireInstallReceipt(SetupPlan plan) throws IOException {
        Path receiptPath = paths.receipts().resolve("browserstack-local.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(receiptPath);
        if (!Files.isRegularFile(receiptPath, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("A complete compatible BrowserStack Local install receipt is required before start.");
        }
        SetupReceipt receipt;
        try {
            receipt = JSON.readValue(receiptPath.toFile(), SetupReceipt.class);
        } catch (RuntimeException invalid) {
            throw new IOException("BrowserStack Local install receipt is invalid.", invalid);
        }
        if (!receipt.planDigest().equals(plan.digest()) || !receipt.completedActions().equals(plan.actions())) {
            throw new IOException("BrowserStack Local install receipt does not match the reviewed plan.");
        }
        return receipt;
    }

    private void requireInstalled(SetupPlan plan) throws IOException {
        for (SetupAction action : plan.actions()) {
            SetupStatus status = operations.status(action);
            if (status.readiness() != SetupReadiness.READY) {
                throw new IOException(action.target() + " is not ready: " + status.detail());
            }
        }
    }

    private void requireLeaseIdentity(BrowserStackLocalRuntimeLease lease) throws IOException {
        if (!lease.sameIdentity(expectedIdentity(expectedPlan, lease.pid()))) {
            throw new IOException("BrowserStack Local lease does not match the requested plan.");
        }
    }

    private Optional<BrowserStackLocalRuntimeLease> readLease() throws IOException {
        Path path = leasePath();
        VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) return Optional.empty();
        try {
            BrowserStackLocalRuntimeLease lease = JSON.readValue(path.toFile(), BrowserStackLocalRuntimeLease.class);
            if (lease.schemaVersion() != 1 || lease.refCount() < 1 || lease.pid() < 1) {
                throw new IOException("Invalid BrowserStack Local lease.");
            }
            return Optional.of(lease);
        } catch (RuntimeException invalid) {
            throw new IOException("BrowserStack Local runtime lease is invalid.", invalid);
        }
    }

    private void writeLease(BrowserStackLocalRuntimeLease lease) throws IOException {
        Files.createDirectories(paths.state());
        Path temporary = Files.createTempFile(paths.state(), "browserstack-local-runtime", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(lease));
            VerifiedArtifactStore.move(temporary, leasePath());
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private Path binaryFile() {
        BrowserStackLocalSetupPlanner.Asset asset = BrowserStackLocalSetupPlanner.asset(
                expectedPlan.platform(), expectedPlan.architecture());
        return paths.tools().resolve("browserstack-local").resolve(asset.executableName());
    }

    private Path leasePath() {
        return paths.state().resolve("browserstack-local-runtime.json");
    }

    private Path lockPath() {
        return paths.state().resolve("browserstack-local-runtime.lock").toAbsolutePath().normalize();
    }
}
