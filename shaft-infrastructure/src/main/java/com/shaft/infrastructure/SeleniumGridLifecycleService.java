package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
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

/** Lease-safe lifecycle owner for one verified SHAFT Selenium Grid compose project. */
final class SeleniumGridLifecycleService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();

    private final ShaftCachePaths paths;
    private final SetupPlan expectedPlan;
    private final SeleniumGridToolchainOperations operations;
    private final SeleniumGridSetupPlanner.GridScale scale;

    SeleniumGridLifecycleService(ShaftCachePaths paths, SetupPlan expectedPlan,
                                 SeleniumGridToolchainOperations operations) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.expectedPlan = java.util.Objects.requireNonNull(expectedPlan, "expectedPlan");
        this.operations = java.util.Objects.requireNonNull(operations, "operations");
        if (expectedPlan.profile() != SetupProfile.SELENIUM_GRID) {
            throw new IllegalArgumentException("Selenium Grid lifecycle requires the SELENIUM_GRID profile.");
        }
        this.scale = SeleniumGridSetupPlanner.scaleFromPlan(expectedPlan);
    }

    ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        requireCompatible(plan, options);
        SetupExecutor.validate(plan, approval);
        SetupReceipt receipt = requireInstallReceipt(plan);
        requireInstalled(plan);
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
                Optional<SeleniumGridRuntimeLease> reusable = readReusable(plan, options);
                if (reusable.isPresent()) return environment(receipt, reusable.orElseThrow(), options);
                return startNew(plan, receipt, options);
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the Selenium Grid runtime lock.", interrupted);
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
            Optional<SeleniumGridRuntimeLease> current = readLease();
            if (current.isEmpty()) return false;
            SeleniumGridRuntimeLease lease = current.orElseThrow();
            requireLeaseIdentity(lease);
            if (!operations.composeRunning(composeFile(), lease.project())) {
                Files.deleteIfExists(leasePath());
                return false;
            }
            if (lease.refCount() > 1) {
                writeLease(lease.withRefCount(lease.refCount() - 1));
                return true;
            }
            operations.composeDown(composeFile(), requireOwnedProject(lease.project()));
            Files.deleteIfExists(leasePath());
            return true;
        } finally {
            jvmLock.unlock();
        }
    }

    String logs() throws IOException {
        Optional<SeleniumGridRuntimeLease> current = readLease();
        if (current.isEmpty()) return "";
        SeleniumGridRuntimeLease lease = current.orElseThrow();
        if (!lease.sameRuntime(expectedIdentity(expectedPlan))) {
            throw new IOException("Selenium Grid lease does not match the requested runtime.");
        }
        return OwnedLogReader.read("Selenium Grid", logFile());
    }

    Path logFile() {
        return paths.state().resolve("logs").resolve("selenium-grid.log");
    }

    private ManagedEnvironment startNew(SetupPlan plan, SetupReceipt receipt, SetupOptions options)
            throws IOException {
        requireAvailablePort(4442, "Selenium Grid event-bus publish");
        requireAvailablePort(4443, "Selenium Grid event-bus subscribe");
        requireAvailablePort(scale.port(), "Selenium Grid");
        URI endpoint = URI.create("http://127.0.0.1:" + scale.port() + "/");
        try {
            operations.composeUp(composeFile(), SeleniumGridSetupPlanner.PROJECT, scale);
            operations.awaitReady(endpoint, options.startupTimeout());
            SeleniumGridRuntimeLease lease = new SeleniumGridRuntimeLease(1, plan.digest(),
                    SeleniumGridSetupPlanner.PROJECT, endpoint.toString(), scale.port(), scale.chrome(),
                    scale.edge(), scale.firefox(), 1);
            writeLease(lease);
            return environment(receipt, lease, options);
        } catch (IOException | RuntimeException failure) {
            try {
                operations.composeDown(composeFile(), SeleniumGridSetupPlanner.PROJECT);
            } catch (IOException cleanup) {
                failure.addSuppressed(cleanup);
            }
            throw failure;
        }
    }

    private Optional<SeleniumGridRuntimeLease> readReusable(SetupPlan plan, SetupOptions options) throws IOException {
        Optional<SeleniumGridRuntimeLease> existing = readLease();
        if (existing.isEmpty()) return Optional.empty();
        SeleniumGridRuntimeLease lease = existing.orElseThrow();
        if (!lease.planDigest().equals(plan.digest()) || !lease.sameIdentity(expectedIdentity(plan))) {
            throw new IOException("An existing SHAFT Selenium Grid lease does not match the reviewed plan.");
        }
        if (!operations.composeRunning(composeFile(), lease.project())) {
            Files.deleteIfExists(leasePath());
            return Optional.empty();
        }
        if (!options.reuseOwnedProcesses()) {
            throw new IOException("A compatible SHAFT Selenium Grid is already active and reuse is disabled.");
        }
        operations.awaitReady(URI.create(lease.endpoint()), options.startupTimeout());
        SeleniumGridRuntimeLease incremented = lease.withRefCount(lease.refCount() + 1);
        writeLease(incremented);
        return Optional.of(incremented);
    }

    private SeleniumGridRuntimeLease expectedIdentity(SetupPlan plan) {
        return new SeleniumGridRuntimeLease(1, plan.digest(), SeleniumGridSetupPlanner.PROJECT,
                "http://127.0.0.1:" + scale.port() + "/", scale.port(), scale.chrome(), scale.edge(),
                scale.firefox(), 1);
    }

    private ManagedEnvironment environment(SetupReceipt receipt, SeleniumGridRuntimeLease lease,
                                           SetupOptions options) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("executionAddress", "localhost:" + lease.port());
        properties.put("selenium.grid.endpoint", lease.endpoint());
        properties.put("selenium.grid.project", lease.project());
        return new ManagedEnvironment(SetupProfile.SELENIUM_GRID, receipt,
                Optional.of(URI.create(lease.endpoint())), Map.copyOf(properties), () -> {
                    try {
                        release(lease);
                    } catch (IOException failure) {
                        throw new IllegalStateException("Failed to release the owned Selenium Grid.", failure);
                    }
                });
    }

    private void release(SeleniumGridRuntimeLease started) throws IOException {
        Path lockPath = lockPath();
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<SeleniumGridRuntimeLease> current = readLease();
            if (current.isEmpty()) return;
            SeleniumGridRuntimeLease lease = current.orElseThrow();
            if (!lease.sameIdentity(started)) {
                throw new IOException("Selenium Grid lease changed; refusing to stop an unowned project.");
            }
            if (lease.refCount() > 1) {
                writeLease(lease.withRefCount(lease.refCount() - 1));
                return;
            }
            operations.composeDown(composeFile(), requireOwnedProject(lease.project()));
            Files.deleteIfExists(leasePath());
        } finally {
            jvmLock.unlock();
        }
    }

    private void requireCompatible(SetupPlan plan, SetupOptions options) {
        if (plan.profile() != SetupProfile.SELENIUM_GRID || options.profile() != SetupProfile.SELENIUM_GRID) {
            throw new IllegalArgumentException("Selenium Grid lifecycle requires profile SELENIUM_GRID.");
        }
        if (plan.mode() == SetupMode.EXTERNAL || options.effectiveMode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External Selenium Grid setup cannot start local containers.");
        }
        if (!expectedPlan.equals(plan)) {
            throw new IllegalArgumentException("Selenium Grid lifecycle plan does not match the release manifest.");
        }
    }

    private SetupReceipt requireInstallReceipt(SetupPlan plan) throws IOException {
        Path receiptPath = paths.receipts().resolve("selenium-grid.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(receiptPath);
        if (!Files.isRegularFile(receiptPath, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("A complete compatible Selenium Grid install receipt is required before start.");
        }
        SetupReceipt receipt;
        try {
            receipt = JSON.readValue(receiptPath.toFile(), SetupReceipt.class);
        } catch (RuntimeException invalid) {
            throw new IOException("Selenium Grid install receipt is invalid.", invalid);
        }
        if (!receipt.planDigest().equals(plan.digest()) || !receipt.completedActions().equals(plan.actions())) {
            throw new IOException("Selenium Grid install receipt does not match the reviewed plan.");
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

    private void requireAvailablePort(int port, String owner) throws IOException {
        try (ServerSocket socket = new ServerSocket()) {
            socket.setReuseAddress(false);
            socket.bind(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), port));
        } catch (IOException occupied) {
            throw new IOException(owner + " loopback port " + port + " is already occupied.", occupied);
        }
    }

    private void requireLeaseIdentity(SeleniumGridRuntimeLease lease) throws IOException {
        if (!lease.sameIdentity(expectedIdentity(expectedPlan))) {
            throw new IOException("Selenium Grid lease does not match the requested plan.");
        }
    }

    private static String requireOwnedProject(String project) throws IOException {
        if (!SeleniumGridSetupPlanner.PROJECT.equals(project)) {
            throw new IOException("Refusing to stop an unowned Docker Compose project: " + project);
        }
        return project;
    }

    private Optional<SeleniumGridRuntimeLease> readLease() throws IOException {
        Path path = leasePath();
        VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) return Optional.empty();
        try {
            SeleniumGridRuntimeLease lease = JSON.readValue(path.toFile(), SeleniumGridRuntimeLease.class);
            if (lease.schemaVersion() != 1 || lease.refCount() < 1) {
                throw new IOException("Invalid Selenium Grid lease.");
            }
            return Optional.of(lease);
        } catch (RuntimeException invalid) {
            throw new IOException("Selenium Grid runtime lease is invalid.", invalid);
        }
    }

    private void writeLease(SeleniumGridRuntimeLease lease) throws IOException {
        Files.createDirectories(paths.state());
        Path temporary = Files.createTempFile(paths.state(), "selenium-grid-runtime", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(lease));
            VerifiedArtifactStore.move(temporary, leasePath());
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private Path composeFile() {
        return paths.tools().resolve("selenium-grid").resolve("docker-compose.yml");
    }

    private Path leasePath() {
        return paths.state().resolve("selenium-grid-runtime.json");
    }

    private Path lockPath() {
        return paths.state().resolve("selenium-grid-runtime.lock").toAbsolutePath().normalize();
    }
}
