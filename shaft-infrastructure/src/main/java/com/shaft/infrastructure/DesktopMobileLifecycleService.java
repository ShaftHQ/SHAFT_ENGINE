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
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/** Lease-safe lifecycle owner for one verified iOS or Windows Appium server. */
final class DesktopMobileLifecycleService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();

    private final ShaftCachePaths paths;
    private final SetupPlan expectedPlan;
    private final DesktopMobileToolchainOperations operations;
    private final AndroidRuntimeController runtime;
    private final DesktopMobileDeviceController devices;
    private final DesktopMobileRuntimeHealth health;
    private final DesktopMobileRuntimeLayout layout;
    private final int appiumPort;
    private final String simulatorUdid;

    DesktopMobileLifecycleService(ShaftCachePaths paths, SetupPlan expectedPlan,
                                  DesktopMobileToolchainOperations operations, AndroidRuntimeController runtime,
                                  DesktopMobileDeviceController devices, DesktopMobileRuntimeHealth health) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.expectedPlan = java.util.Objects.requireNonNull(expectedPlan, "expectedPlan");
        this.operations = java.util.Objects.requireNonNull(operations, "operations");
        this.runtime = java.util.Objects.requireNonNull(runtime, "runtime");
        this.devices = java.util.Objects.requireNonNull(devices, "devices");
        this.health = java.util.Objects.requireNonNull(health, "health");
        requireDesktopProfile(expectedPlan.profile());
        this.layout = DesktopMobileRuntimeLayout.resolve(paths, expectedPlan);
        this.appiumPort = DesktopMobileSetupPlanner.requestedAppiumPort(expectedPlan);
        this.simulatorUdid = expectedPlan.profile() == SetupProfile.MOBILE_IOS
                ? DesktopMobileSetupPlanner.requestedSimulator(expectedPlan) : "";
    }

    ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        requireCompatible(plan, options);
        SetupExecutor.validate(plan, approval);
        SetupReceipt receipt = requireInstallReceipt(plan);
        requireInstalled(plan);
        requireExactIosSimulator();
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
                Optional<ActiveRuntime> reusable = readReusable(plan, options);
                if (reusable.isPresent()) return environment(receipt, reusable.orElseThrow(), options);
                return startNew(plan, receipt, options);
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the desktop-mobile runtime lock.", interrupted);
        } finally {
            if (jvmLock.isHeldByCurrentThread()) jvmLock.unlock();
        }
    }

    String logs() throws IOException {
        Optional<DesktopMobileRuntimeLease> current = readLease();
        if (current.isEmpty()) return "";
        requireLeaseRequest(current.orElseThrow());
        return OwnedLogReader.read("Appium server", layout.appiumLog());
    }

    boolean stop(Duration timeout) throws IOException {
        Path lockPath = lockPath();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        if (Files.notExists(leasePath(), LinkOption.NOFOLLOW_LINKS)) return false;
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<DesktopMobileRuntimeLease> current = readLease();
            if (current.isEmpty()) return false;
            DesktopMobileRuntimeLease lease = current.orElseThrow();
            requireLeaseRequest(lease);
            Optional<AndroidOwnedProcess> appium = find(lease.appium());
            if (appium.isEmpty()) {
                if (lease.shaftBootedSimulator() && iosSimulatorStillOwned(lease)) {
                    throw new IOException("Desktop-mobile runtime lease is partially alive; refusing to kill an "
                            + "uncertain Simulator identity.");
                }
                Files.deleteIfExists(leasePath());
                return false;
            }
            IOException cleanup = stopStarted(appium.orElseThrow(), lease, timeout);
            if (cleanup != null) throw cleanup;
            Files.deleteIfExists(leasePath());
            return true;
        } finally {
            jvmLock.unlock();
        }
    }

    private ManagedEnvironment startNew(SetupPlan plan, SetupReceipt receipt, SetupOptions options)
            throws IOException {
        requireAvailablePort(appiumPort, "Appium");
        boolean shaftBootedSimulator = false;
        AndroidOwnedProcess appium = null;
        try {
            if (plan.profile() == SetupProfile.MOBILE_IOS) {
                DesktopMobileDeviceController.SimulatorState state = devices.simulatorState(simulatorUdid);
                if (state == DesktopMobileDeviceController.SimulatorState.MISSING) {
                    throw new IOException("Selected iOS Simulator is unavailable: " + simulatorUdid);
                }
                if (state == DesktopMobileDeviceController.SimulatorState.SHUTDOWN) {
                    devices.bootSimulator(simulatorUdid);
                    shaftBootedSimulator = true;
                    health.awaitSimulator(simulatorUdid, options.startupTimeout());
                }
            }
            URI endpoint = URI.create("http://127.0.0.1:" + appiumPort + '/');
            appium = runtime.start("appium", List.of(layout.nodeExecutable().toString(),
                    layout.appiumEntryPoint().toString(), "--address", "127.0.0.1", "--port",
                    Integer.toString(appiumPort), "--base-path", "/"), layout.appiumHome(),
                    Map.of("APPIUM_HOME", layout.appiumHome().toString()),
                    Set.of("APPIUM_HOME", "REPO_OS_OVERRIDE"), layout.appiumLog());
            health.awaitAppium(endpoint, options.startupTimeout());
            DesktopMobileRuntimeLease lease = new DesktopMobileRuntimeLease(1, plan.digest(), plan.profile(),
                    endpoint.toString(), simulatorUdid, shaftBootedSimulator, ProcessIdentity.of(appium), 1);
            writeLease(lease);
            return environment(receipt, new ActiveRuntime(lease, appium), options);
        } catch (IOException failure) {
            suppressCleanupFailure(failure, appium, shaftBootedSimulator, plan, options.shutdownTimeout());
            throw failure;
        } catch (RuntimeException failure) {
            suppressCleanupFailure(failure, appium, shaftBootedSimulator, plan, options.shutdownTimeout());
            throw failure;
        }
    }

    private void suppressCleanupFailure(Throwable failure, AndroidOwnedProcess appium,
                                        boolean shaftBootedSimulator, SetupPlan plan, Duration timeout) {
        IOException cleanup = stopStarted(appium, shaftBootedLease(plan, shaftBootedSimulator), timeout);
        if (cleanup != null) failure.addSuppressed(cleanup);
    }

    private DesktopMobileRuntimeLease shaftBootedLease(SetupPlan plan, boolean shaftBootedSimulator) {
        return new DesktopMobileRuntimeLease(1, plan.digest(), plan.profile(),
                "http://127.0.0.1:" + appiumPort + '/', simulatorUdid, shaftBootedSimulator, null, 1);
    }

    private ManagedEnvironment environment(SetupReceipt receipt, ActiveRuntime active, SetupOptions options) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("APPIUM_HOME", layout.appiumHome().toString());
        properties.put("appium.endpoint", active.lease().endpoint());
        if (!simulatorUdid.isEmpty()) properties.put("ios.simulator.udid", simulatorUdid);
        return new ManagedEnvironment(expectedPlan.profile(), receipt,
                Optional.of(URI.create(active.lease().endpoint())), Map.copyOf(properties), () -> {
                    try {
                        release(active, options.shutdownTimeout());
                    } catch (IOException failure) {
                        throw new IllegalStateException("Failed to release the owned desktop-mobile runtime.",
                                failure);
                    }
                });
    }

    private Optional<ActiveRuntime> readReusable(SetupPlan plan, SetupOptions options) throws IOException {
        Optional<DesktopMobileRuntimeLease> existing = readLease();
        if (existing.isEmpty()) return Optional.empty();
        DesktopMobileRuntimeLease lease = existing.orElseThrow();
        if (!lease.planDigest().equals(plan.digest()) || !lease.endpoint().equals(endpoint())
                || !lease.simulatorUdid().equals(simulatorUdid) || lease.profile() != plan.profile()) {
            throw new IOException("An existing SHAFT desktop-mobile lease does not match the reviewed plan.");
        }
        Optional<AndroidOwnedProcess> appium = find(lease.appium());
        if (appium.isEmpty()) {
            if (lease.shaftBootedSimulator() && iosSimulatorStillOwned(lease)) {
                throw new IOException("The SHAFT desktop-mobile runtime lease is stale or only partially alive; "
                        + "manual recovery is required before starting another runtime.");
            }
            Files.deleteIfExists(leasePath());
            return Optional.empty();
        }
        if (!options.reuseOwnedProcesses()) {
            throw new IOException("A compatible SHAFT desktop-mobile runtime is already active and reuse is disabled.");
        }
        health.awaitAppium(URI.create(lease.endpoint()), options.startupTimeout());
        if (plan.profile() == SetupProfile.MOBILE_IOS) {
            health.awaitSimulator(simulatorUdid, options.startupTimeout());
        }
        DesktopMobileRuntimeLease incremented = lease.withRefCount(lease.refCount() + 1);
        writeLease(incremented);
        return Optional.of(new ActiveRuntime(incremented, appium.orElseThrow()));
    }

    private void requireLeaseRequest(DesktopMobileRuntimeLease lease) throws IOException {
        if (lease.profile() != expectedPlan.profile() || !lease.endpoint().equals(endpoint())
                || !lease.simulatorUdid().equals(simulatorUdid)) {
            throw new IOException("Desktop-mobile runtime lease does not match the requested profile and endpoint.");
        }
    }

    private void release(ActiveRuntime active, Duration timeout) throws IOException {
        Path lockPath = lockPath();
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<DesktopMobileRuntimeLease> current = readLease();
            if (current.isEmpty()) return;
            DesktopMobileRuntimeLease lease = current.orElseThrow();
            if (!lease.sameIdentity(active.lease())) {
                throw new IOException("Desktop-mobile runtime lease changed; refusing to stop an unowned process.");
            }
            if (lease.refCount() > 1) {
                writeLease(lease.withRefCount(lease.refCount() - 1));
                return;
            }
            IOException cleanup = stopStarted(active.appium(), lease, timeout);
            if (cleanup != null) throw cleanup;
            Files.deleteIfExists(leasePath());
        } finally {
            jvmLock.unlock();
        }
    }

    private void requireCompatible(SetupPlan plan, SetupOptions options) {
        java.util.Objects.requireNonNull(plan, "plan");
        java.util.Objects.requireNonNull(options, "options");
        if (plan.profile() != expectedPlan.profile() || options.profile() != expectedPlan.profile()) {
            throw new IllegalArgumentException("Desktop-mobile lifecycle requires profile "
                    + expectedPlan.profile() + '.');
        }
        if (plan.platform() != expectedPlan.platform() || plan.architecture() != expectedPlan.architecture()) {
            throw new IllegalArgumentException("Desktop-mobile lifecycle plan does not match this host.");
        }
        if (plan.mode() == SetupMode.EXTERNAL || options.effectiveMode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External desktop-mobile setup cannot start local processes.");
        }
        if (!expectedPlan.equals(plan)) {
            throw new IllegalArgumentException("Desktop-mobile lifecycle plan does not match the release manifest.");
        }
    }

    private void requireExactIosSimulator() throws IOException {
        if (expectedPlan.profile() == SetupProfile.MOBILE_IOS && "existing".equals(simulatorUdid)) {
            throw new IOException("iOS runtime start requires an exact Simulator UDID.");
        }
    }

    private SetupReceipt requireInstallReceipt(SetupPlan plan) throws IOException {
        Path receiptPath = paths.receipts().resolve(receiptStem() + ".json");
        VerifiedArtifactStore.requireUnlinkedAncestors(receiptPath);
        if (!Files.isRegularFile(receiptPath, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("A complete compatible desktop-mobile install receipt is required before start.");
        }
        SetupReceipt receipt;
        try {
            receipt = JSON.readValue(receiptPath.toFile(), SetupReceipt.class);
        } catch (RuntimeException invalid) {
            throw new IOException("Desktop-mobile install receipt is invalid.", invalid);
        }
        if (!receipt.planDigest().equals(plan.digest()) || !receipt.completedActions().equals(plan.actions())) {
            throw new IOException("Desktop-mobile install receipt does not match the reviewed plan.");
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

    private Optional<AndroidOwnedProcess> find(ProcessIdentity identity) throws IOException {
        if (identity == null) return Optional.empty();
        return runtime.find(identity.pid(), Instant.ofEpochMilli(identity.startEpochMilli()),
                identity.commandIdentity());
    }

    private Optional<DesktopMobileRuntimeLease> readLease() throws IOException {
        Path path = leasePath();
        VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) return Optional.empty();
        try {
            DesktopMobileRuntimeLease lease = JSON.readValue(path.toFile(), DesktopMobileRuntimeLease.class);
            if (lease.schemaVersion() != 1 || lease.refCount() < 1) {
                throw new IOException("Invalid desktop-mobile lease.");
            }
            return Optional.of(lease);
        } catch (RuntimeException invalid) {
            throw new IOException("Desktop-mobile runtime lease is invalid.", invalid);
        }
    }

    private void writeLease(DesktopMobileRuntimeLease lease) throws IOException {
        Files.createDirectories(paths.state());
        Path temporary = Files.createTempFile(paths.state(), receiptStem() + "-runtime", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(lease));
            VerifiedArtifactStore.move(temporary, leasePath());
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private IOException stopStarted(AndroidOwnedProcess appium, DesktopMobileRuntimeLease lease, Duration timeout) {
        IOException failure = null;
        Instant deadline = Instant.now().plus(timeout);
        if (appium != null) {
            try {
                Duration remaining = Duration.between(Instant.now(), deadline);
                appium.stop(remaining.isNegative() ? Duration.ZERO : remaining);
            } catch (IOException cleanup) {
                failure = cleanup;
            }
        }
        if (lease != null && lease.shaftBootedSimulator()) {
            try {
                devices.shutdownSimulator(lease.simulatorUdid());
            } catch (IOException cleanup) {
                if (failure == null) failure = cleanup;
                else failure.addSuppressed(cleanup);
            }
        }
        return failure;
    }

    private boolean iosSimulatorStillOwned(DesktopMobileRuntimeLease lease) throws IOException {
        return expectedPlan.profile() == SetupProfile.MOBILE_IOS
                && devices.simulatorState(lease.simulatorUdid())
                == DesktopMobileDeviceController.SimulatorState.BOOTED;
    }

    private Path lockPath() {
        return paths.state().resolve(receiptStem() + "-runtime.lock").toAbsolutePath().normalize();
    }

    private Path leasePath() {
        return paths.state().resolve(receiptStem() + "-runtime.json");
    }

    private String receiptStem() {
        return expectedPlan.profile() == SetupProfile.MOBILE_IOS ? "mobile-ios" : "mobile-windows";
    }

    private String endpoint() {
        return "http://127.0.0.1:" + appiumPort + '/';
    }

    private static void requireDesktopProfile(SetupProfile profile) {
        if (profile != SetupProfile.MOBILE_IOS && profile != SetupProfile.MOBILE_WINDOWS) {
            throw new IllegalArgumentException("Desktop-mobile lifecycle requires an iOS or Windows profile.");
        }
    }

    private record ActiveRuntime(DesktopMobileRuntimeLease lease, AndroidOwnedProcess appium) { }

    private record ProcessIdentity(long pid, long startEpochMilli, String commandIdentity) {
        static ProcessIdentity of(AndroidOwnedProcess process) {
            return new ProcessIdentity(process.pid(), process.startInstant().toEpochMilli(),
                    process.commandIdentity());
        }
    }

    private record DesktopMobileRuntimeLease(int schemaVersion, String planDigest, SetupProfile profile,
                                             String endpoint, String simulatorUdid, boolean shaftBootedSimulator,
                                             ProcessIdentity appium, int refCount) {
        DesktopMobileRuntimeLease withRefCount(int value) {
            return new DesktopMobileRuntimeLease(schemaVersion, planDigest, profile, endpoint, simulatorUdid,
                    shaftBootedSimulator, appium, value);
        }

        boolean sameIdentity(DesktopMobileRuntimeLease other) {
            return planDigest.equals(other.planDigest) && profile == other.profile
                    && endpoint.equals(other.endpoint) && simulatorUdid.equalsIgnoreCase(other.simulatorUdid)
                    && shaftBootedSimulator == other.shaftBootedSimulator
                    && java.util.Objects.equals(appium, other.appium);
        }
    }
}
