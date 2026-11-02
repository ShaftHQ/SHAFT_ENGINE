package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/** Transaction coordinator for one exact Healenium setup plan. */
final class HealeniumSetupService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();

    private final ShaftCachePaths paths;
    private final SetupPlan expectedPlan;
    private final HealeniumToolchainOperations operations;
    private final boolean offline;

    HealeniumSetupService(ShaftCachePaths paths, SetupPlan expectedPlan,
                          HealeniumToolchainOperations operations, boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.expectedPlan = java.util.Objects.requireNonNull(expectedPlan, "expectedPlan");
        this.operations = java.util.Objects.requireNonNull(operations, "operations");
        this.offline = offline;
        if (expectedPlan.profile() != SetupProfile.HEALENIUM) {
            throw new IllegalArgumentException("Healenium setup requires the HEALENIUM profile.");
        }
    }

    SetupProfileStatus status() {
        List<SetupStatus> targets = expectedPlan.actions().stream().map(operations::status).toList();
        SetupReadiness readiness = targets.stream().allMatch(target -> target.readiness() == SetupReadiness.READY)
                ? SetupReadiness.READY
                : targets.stream().anyMatch(target -> target.readiness() == SetupReadiness.DEGRADED)
                ? SetupReadiness.DEGRADED : SetupReadiness.MISSING;
        if (readiness == SetupReadiness.READY && !hasCompatibleReceipt()) {
            ArrayList<SetupStatus> adjusted = new ArrayList<>(targets);
            SetupStatus last = adjusted.getLast();
            adjusted.set(adjusted.size() - 1, new SetupStatus(last.target(), SetupReadiness.DEGRADED,
                    last.detectedVersion(), "Managed files exist without a compatible SHAFT receipt."));
            targets = List.copyOf(adjusted);
            readiness = SetupReadiness.DEGRADED;
        }
        return new SetupProfileStatus(1, expectedPlan.profile(), readiness, targets);
    }

    SetupReceipt install(SetupPlan plan, SetupApproval approval) throws IOException {
        requireCompatible(plan);
        SetupExecutor.validate(plan, approval);
        operations.hostPreflight(plan.actions());
        Path lockPath = paths.state().resolve("healenium.lock").toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        boolean acquired = false;
        try {
            jvmLock.lockInterruptibly();
            acquired = true;
            if (!Files.isDirectory(paths.state(), LinkOption.NOFOLLOW_LINKS)) {
                operations.preStatePreflight(plan.actions(), offline);
            }
            Files.createDirectories(paths.state());
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                operations.lockedPreflight(plan.actions(), offline);
                invalidateReceipt();
                SetupReceipt receipt = SetupExecutor.execute(plan, approval, action -> {
                    try {
                        operations.install(action);
                    } catch (IOException failure) {
                        throw new SetupOperationException(failure);
                    }
                });
                Files.deleteIfExists(previousReceiptPath());
                writeReceipt(receipt);
                return receipt;
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the Healenium setup lock.", interrupted);
        } finally {
            if (acquired) jvmLock.unlock();
        }
    }

    private void requireCompatible(SetupPlan plan) {
        if (plan.mode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External plans are diagnostic and cannot be installed.");
        }
        if (!expectedPlan.equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the Healenium manifest shipped with this release.");
        }
    }

    private void writeReceipt(SetupReceipt receipt) throws IOException {
        Files.createDirectories(paths.receipts());
        Path destination = receiptPath();
        Path temporary = Files.createTempFile(paths.receipts(), "healenium", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(receipt));
            VerifiedArtifactStore.move(temporary, destination);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private boolean hasCompatibleReceipt() {
        try {
            Path receipt = receiptPath();
            VerifiedArtifactStore.requireUnlinkedAncestors(receipt);
            if (!Files.isRegularFile(receipt, LinkOption.NOFOLLOW_LINKS)) return false;
            SetupReceipt saved = JSON.readValue(receipt.toFile(), SetupReceipt.class);
            return saved.planDigest().equals(expectedPlan.digest())
                    && saved.completedActions().equals(expectedPlan.actions());
        } catch (IOException | RuntimeException invalid) {
            return false;
        }
    }

    private Path receiptPath() {
        return paths.receipts().resolve("healenium.json");
    }

    private Path previousReceiptPath() {
        return paths.receipts().resolve("healenium.previous.json");
    }

    private void invalidateReceipt() throws IOException {
        Path receipt = receiptPath();
        Path previous = previousReceiptPath();
        VerifiedArtifactStore.requireUnlinkedAncestors(receipt);
        VerifiedArtifactStore.requireUnlinkedAncestors(previous);
        if (!Files.isRegularFile(receipt, LinkOption.NOFOLLOW_LINKS)) return;
        Files.deleteIfExists(previous);
        VerifiedArtifactStore.move(receipt, previous);
    }

    private static final class SetupOperationException extends RuntimeException {
        private SetupOperationException(IOException cause) {
            super(cause);
        }
    }
}
