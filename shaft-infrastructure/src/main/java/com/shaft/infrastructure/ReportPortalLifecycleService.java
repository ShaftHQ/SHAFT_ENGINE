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

/** Lease-safe lifecycle owner for one verified SHAFT ReportPortal compose project. */
final class ReportPortalLifecycleService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();

    private final ShaftCachePaths paths;
    private final SetupPlan expectedPlan;
    private final ReportPortalToolchainOperations operations;
    private final ReportPortalSetupPlanner.ReportPortalScale scale;

    ReportPortalLifecycleService(ShaftCachePaths paths, SetupPlan expectedPlan,
                                 ReportPortalToolchainOperations operations) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.expectedPlan = java.util.Objects.requireNonNull(expectedPlan, "expectedPlan");
        this.operations = java.util.Objects.requireNonNull(operations, "operations");
        if (expectedPlan.profile() != SetupProfile.REPORT_PORTAL) {
            throw new IllegalArgumentException("ReportPortal lifecycle requires the REPORT_PORTAL profile.");
        }
        this.scale = ReportPortalSetupPlanner.scaleFromPlan(expectedPlan);
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
                Optional<ReportPortalRuntimeLease> reusable = readReusable(plan, options);
                if (reusable.isPresent()) return environment(receipt, reusable.orElseThrow());
                return startNew(plan, receipt, options);
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the ReportPortal runtime lock.", interrupted);
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
            Optional<ReportPortalRuntimeLease> current = readLease();
            if (current.isEmpty()) return false;
            ReportPortalRuntimeLease lease = current.orElseThrow();
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
        Optional<ReportPortalRuntimeLease> current = readLease();
        if (current.isEmpty()) return "";
        ReportPortalRuntimeLease lease = current.orElseThrow();
        if (!lease.sameRuntime(expectedIdentity(expectedPlan))) {
            throw new IOException("ReportPortal lease does not match the requested runtime.");
        }
        return OwnedLogReader.read("ReportPortal", logFile());
    }

    Path logFile() {
        return paths.state().resolve("logs").resolve("reportportal.log");
    }

    private ManagedEnvironment startNew(SetupPlan plan, SetupReceipt receipt, SetupOptions options)
            throws IOException {
        requireAvailablePort(scale.uiPort(), "ReportPortal UI");
        URI ui = URI.create("http://127.0.0.1:" + scale.uiPort() + "/ui/");
        try {
            operations.composeUp(composeFile(), ReportPortalSetupPlanner.PROJECT);
            operations.awaitReady(ui, options.startupTimeout());
            ReportPortalRuntimeLease lease = new ReportPortalRuntimeLease(1, plan.digest(),
                    ReportPortalSetupPlanner.PROJECT, ui.toString(), scale.uiPort(), 1);
            writeLease(lease);
            return environment(receipt, lease);
        } catch (IOException | RuntimeException failure) {
            try {
                operations.composeDown(composeFile(), ReportPortalSetupPlanner.PROJECT);
            } catch (IOException cleanup) {
                failure.addSuppressed(cleanup);
            }
            throw failure;
        }
    }

    private Optional<ReportPortalRuntimeLease> readReusable(SetupPlan plan, SetupOptions options) throws IOException {
        Optional<ReportPortalRuntimeLease> existing = readLease();
        if (existing.isEmpty()) return Optional.empty();
        ReportPortalRuntimeLease lease = existing.orElseThrow();
        if (!lease.planDigest().equals(plan.digest()) || !lease.sameIdentity(expectedIdentity(plan))) {
            throw new IOException("An existing SHAFT ReportPortal lease does not match the reviewed plan.");
        }
        if (!operations.composeRunning(composeFile(), lease.project())) {
            Files.deleteIfExists(leasePath());
            return Optional.empty();
        }
        if (!options.reuseOwnedProcesses()) {
            throw new IOException("A compatible SHAFT ReportPortal stack is already active and reuse is disabled.");
        }
        operations.awaitReady(URI.create(lease.endpoint()), options.startupTimeout());
        ReportPortalRuntimeLease incremented = lease.withRefCount(lease.refCount() + 1);
        writeLease(incremented);
        return Optional.of(incremented);
    }

    private ReportPortalRuntimeLease expectedIdentity(SetupPlan plan) {
        return new ReportPortalRuntimeLease(1, plan.digest(), ReportPortalSetupPlanner.PROJECT,
                "http://127.0.0.1:" + scale.uiPort() + "/ui/", scale.uiPort(), 1);
    }

    private ManagedEnvironment environment(SetupReceipt receipt, ReportPortalRuntimeLease lease) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("serverHost", "localhost");
        properties.put("serverPort", String.valueOf(lease.uiPort()));
        return new ManagedEnvironment(SetupProfile.REPORT_PORTAL, receipt,
                Optional.of(URI.create(lease.endpoint())), Map.copyOf(properties), () -> {
                    try {
                        release(lease);
                    } catch (IOException failure) {
                        throw new IllegalStateException("Failed to release the owned ReportPortal stack.", failure);
                    }
                });
    }

    private void release(ReportPortalRuntimeLease started) throws IOException {
        Path lockPath = lockPath();
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<ReportPortalRuntimeLease> current = readLease();
            if (current.isEmpty()) return;
            ReportPortalRuntimeLease lease = current.orElseThrow();
            if (!lease.sameIdentity(started)) {
                throw new IOException("ReportPortal lease changed; refusing to stop an unowned project.");
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
        if (plan.profile() != SetupProfile.REPORT_PORTAL || options.profile() != SetupProfile.REPORT_PORTAL) {
            throw new IllegalArgumentException("ReportPortal lifecycle requires profile REPORT_PORTAL.");
        }
        if (plan.mode() == SetupMode.EXTERNAL || options.effectiveMode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External ReportPortal setup cannot start local containers.");
        }
        if (!expectedPlan.equals(plan)) {
            throw new IllegalArgumentException("ReportPortal lifecycle plan does not match the release manifest.");
        }
    }

    private SetupReceipt requireInstallReceipt(SetupPlan plan) throws IOException {
        Path receiptPath = paths.receipts().resolve("reportportal.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(receiptPath);
        if (!Files.isRegularFile(receiptPath, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("A complete compatible ReportPortal install receipt is required before start.");
        }
        SetupReceipt receipt;
        try {
            receipt = JSON.readValue(receiptPath.toFile(), SetupReceipt.class);
        } catch (RuntimeException invalid) {
            throw new IOException("ReportPortal install receipt is invalid.", invalid);
        }
        if (!receipt.planDigest().equals(plan.digest()) || !receipt.completedActions().equals(plan.actions())) {
            throw new IOException("ReportPortal install receipt does not match the reviewed plan.");
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

    private void requireLeaseIdentity(ReportPortalRuntimeLease lease) throws IOException {
        if (!lease.sameIdentity(expectedIdentity(expectedPlan))) {
            throw new IOException("ReportPortal lease does not match the requested plan.");
        }
    }

    private static String requireOwnedProject(String project) throws IOException {
        if (!ReportPortalSetupPlanner.PROJECT.equals(project)) {
            throw new IOException("Refusing to stop an unowned Docker Compose project: " + project);
        }
        return project;
    }

    private Optional<ReportPortalRuntimeLease> readLease() throws IOException {
        Path path = leasePath();
        VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) return Optional.empty();
        try {
            ReportPortalRuntimeLease lease = JSON.readValue(path.toFile(), ReportPortalRuntimeLease.class);
            if (lease.schemaVersion() != 1 || lease.refCount() < 1) {
                throw new IOException("Invalid ReportPortal lease.");
            }
            return Optional.of(lease);
        } catch (RuntimeException invalid) {
            throw new IOException("ReportPortal runtime lease is invalid.", invalid);
        }
    }

    private void writeLease(ReportPortalRuntimeLease lease) throws IOException {
        Files.createDirectories(paths.state());
        Path temporary = Files.createTempFile(paths.state(), "reportportal-runtime", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(lease));
            VerifiedArtifactStore.move(temporary, leasePath());
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private Path composeFile() {
        return paths.tools().resolve("reportportal").resolve("docker-compose.yml");
    }

    private Path leasePath() {
        return paths.state().resolve("reportportal-runtime.json");
    }

    private Path lockPath() {
        return paths.state().resolve("reportportal-runtime.lock").toAbsolutePath().normalize();
    }
}
