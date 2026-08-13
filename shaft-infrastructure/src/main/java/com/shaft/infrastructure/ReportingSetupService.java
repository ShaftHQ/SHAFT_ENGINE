package com.shaft.infrastructure;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/** Managed, release-pinned lifecycle for the REPORTING profile. */
public final class ReportingSetupService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> NODE_LOCKS = new ConcurrentHashMap<>();
    private final ShaftCachePaths paths;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;
    private final ArtifactFetcher artifactFetcher;
    private final CommandRunner commandRunner;
    private boolean offline;

    public ReportingSetupService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture) {
        this(paths, platform, architecture, false);
    }

    public ReportingSetupService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                                 boolean offline) {
        this(paths, platform, architecture,
                action -> new VerifiedArtifactStore(paths.downloads()).fetch(action, offline),
                (command, log, timeout) -> runProcess(command, log, timeout, paths.cacheRoot(),
                        nodeRoot(paths, platform, architecture)));
        this.offline = offline;
    }

    ReportingSetupService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                          ArtifactFetcher artifactFetcher, CommandRunner commandRunner) {
        this(paths, platform, architecture, artifactFetcher, commandRunner, false);
    }

    ReportingSetupService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                          ArtifactFetcher artifactFetcher, CommandRunner commandRunner, boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.platform = java.util.Objects.requireNonNull(platform, "platform");
        this.architecture = java.util.Objects.requireNonNull(architecture, "architecture");
        this.artifactFetcher = java.util.Objects.requireNonNull(artifactFetcher, "artifactFetcher");
        this.commandRunner = java.util.Objects.requireNonNull(commandRunner, "commandRunner");
        this.offline = offline;
    }

    public SetupProfileStatus status() {
        SetupStatus node = nodeStatus();
        SetupStatus allure = probe(SetupTarget.ALLURE, allureEntryPoint(), ReportingSetupPlanner.ALLURE_VERSION);
        SetupReadiness aggregate = node.readiness() == SetupReadiness.DEGRADED
                || allure.readiness() == SetupReadiness.DEGRADED ? SetupReadiness.DEGRADED
                : node.readiness() == SetupReadiness.READY && allure.readiness() == SetupReadiness.READY
                ? SetupReadiness.READY : SetupReadiness.MISSING;
        return new SetupProfileStatus(1, SetupProfile.REPORTING, aggregate, List.of(node, allure));
    }

    SetupStatus nodeStatus() {
        return probe(SetupTarget.NODE, nodeExecutable(), "v" + ReportingSetupPlanner.NODE_VERSION);
    }

    void installNodeAction(SetupAction action) throws IOException {
        if (action.target() != SetupTarget.NODE) {
            throw new IllegalArgumentException("Expected a portable Node action.");
        }
        VerifiedArtifactStore.requireUnlinkedAncestors(nodeRoot());
        if (Files.isSymbolicLink(nodeExecutable())) {
            throw new IOException("Managed portable Node executable must not be a symbolic link: " + nodeExecutable());
        }
        if (nodeStatus().readiness() == SetupReadiness.READY) return;
        withNodeInstallLock(() -> installNode(artifactFetcher.fetch(action), action));
    }

    private void withNodeInstallLock(IoOperation operation) throws IOException {
        Path lockPath = paths.state().resolve("node-install.lock").toAbsolutePath().normalize();
        ReentrantLock jvmLock = NODE_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        try {
            jvmLock.lockInterruptibly();
            if (nodeStatus().readiness() == SetupReadiness.READY) return;
            Files.createDirectories(paths.state());
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                if (nodeStatus().readiness() != SetupReadiness.READY) operation.run();
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the portable Node setup lock.", interrupted);
        } finally {
            if (jvmLock.isHeldByCurrentThread()) jvmLock.unlock();
        }
    }

    public SetupReceipt install(SetupPlan plan, SetupApproval approval) throws IOException {
        requireCompatible(plan);
        SetupExecutor.validate(plan, approval);
        Files.createDirectories(paths.state());
        Path lockPath = paths.state().resolve("reporting.lock");
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            SetupReceipt receipt = SetupExecutor.execute(plan, approval, action -> {
                try {
                    install(action);
                } catch (IOException failure) {
                    throw new SetupOperationException(failure);
                }
            });
            writeReceipt(receipt);
            return receipt;
        }
    }

    public Path logFile() {
        return paths.state().resolve("logs/reporting-install.log");
    }

    private void requireCompatible(SetupPlan plan) {
        if (plan.profile() != SetupProfile.REPORTING) throw new IllegalArgumentException("Not a reporting plan.");
        if (plan.platform() != platform || plan.architecture() != architecture) {
            throw new IllegalArgumentException("Plan platform does not match this host.");
        }
        if (plan.mode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External plans are diagnostic and cannot be installed.");
        }
        SetupPlan expected = ReportingSetupPlanner.plan(platform, architecture, plan.mode());
        if (!expected.equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the reporting manifest shipped with this release.");
        }
    }

    private void install(SetupAction action) throws IOException {
        switch (action.target()) {
            case NODE -> installNodeAction(action);
            case ALLURE -> installAllure(artifactFetcher.fetch(action), action);
            default -> throw new IllegalArgumentException("Reporting provider cannot install " + action.target());
        }
    }

    private void installNode(Path archive, SetupAction action) throws IOException {
        Path destination = nodeRoot();
        if (probe(SetupTarget.NODE, nodeExecutable(), "v" + action.version()).readiness() == SetupReadiness.READY) return;
        Path staging = stagingDirectory(destination);
        try {
            if (archive.getFileName().toString().endsWith(".zip")) extractZip(archive, staging);
            else extractTar(archive, staging);
            ProcessResult verification = commandRunner.run(List.of(executable(staging, "node").toString(),
                    "--version"), null, Duration.ofSeconds(30));
            requireSuccessful(verification, "Portable Node verification failed");
            if (!("v" + action.version()).equals(verification.output().trim())) {
                throw new IOException("Portable Node verification returned unexpected version: "
                        + verification.output().trim());
            }
            publish(staging, destination);
        } finally {
            deleteTree(staging);
        }
    }

    private void installAllure(Path packageArchive, SetupAction action) throws IOException {
        if (probe(SetupTarget.ALLURE, allureEntryPoint(), action.version()).readiness() == SetupReadiness.READY) return;
        if (probe(SetupTarget.NODE, nodeExecutable(), "v" + ReportingSetupPlanner.NODE_VERSION).readiness()
                != SetupReadiness.READY) throw new IOException("Portable Node must be ready before Allure installation.");
        Path destination = allureRoot();
        Path staging = stagingDirectory(destination);
        try {
            Path log = logFile();
            Files.createDirectories(log.getParent());
            copyReportingManifest(staging, action.dependencyLockChecksum());
            List<String> cacheCommand = new ArrayList<>(List.of(nodeExecutable().toString(), npmCli().toString(),
                    "cache", "add", packageArchive.toString()));
            if (offline) cacheCommand.add("--offline");
            requireSuccessful(commandRunner.run(cacheCommand, log, Duration.ofMinutes(2)),
                    "Allure package cache preparation failed; see " + log);
            List<String> command = new ArrayList<>(List.of(nodeExecutable().toString(), npmCli().toString(), "ci",
                    "--prefix", staging.toString(), "--ignore-scripts", "--no-audit", "--no-fund"));
            if (offline) command.add("--offline");
            requireSuccessful(commandRunner.run(command, log, Duration.ofMinutes(5)),
                    "Allure npm installation failed; see " + log);
            Path entryPoint = staging.resolve("node_modules/allure/cli.js");
            requireSuccessful(commandRunner.run(List.of(nodeExecutable().toString(), entryPoint.toString(), "--version"),
                    log, Duration.ofSeconds(30)), "Allure verification failed; see " + log);
            publish(staging, destination);
        } finally {
            deleteTree(staging);
        }
    }

    private SetupStatus probe(SetupTarget target, Path executable, String expectedVersion) {
        try {
            VerifiedArtifactStore.requireUnlinkedAncestors(executable);
            if (!Files.isRegularFile(executable, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(target, SetupReadiness.MISSING, "", "Not installed.");
            }
            ProcessResult result = target == SetupTarget.NODE
                    ? commandRunner.run(List.of(executable.toString(), "--version"), null, Duration.ofSeconds(15))
                    : commandRunner.run(List.of(nodeExecutable().toString(), executable.toString(), "--version"), null,
                    Duration.ofSeconds(15));
            String version = result.output().trim();
            String normalized = target == SetupTarget.NODE && version.startsWith("v")
                    ? version.substring(1) : version;
            String normalizedExpected = expectedVersion.startsWith("v")
                    ? expectedVersion.substring(1) : expectedVersion;
            if (result.exitCode() == 0 && normalized.equals(normalizedExpected)) {
                return new SetupStatus(target, SetupReadiness.READY, version, "Verified managed installation.");
            }
            return new SetupStatus(target, SetupReadiness.DEGRADED, version, "Version or execution check failed.");
        } catch (IOException failure) {
            return new SetupStatus(target, SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    static ProcessResult runProcess(List<String> command, Path log, Duration timeout,
                                    Path cacheRoot, Path nodeRoot) throws IOException {
        return runProcess(command, log, timeout, cacheRoot, nodeRoot, null);
    }

    static ProcessResult runProcess(List<String> command, Path log, Duration timeout,
                                    Path cacheRoot, Path nodeRoot, Path workingDirectory) throws IOException {
        ProcessBuilder builder = new ProcessBuilder(command).redirectErrorStream(true);
        if (workingDirectory != null) builder.directory(workingDirectory.toFile());
        String nodeBin = Files.isRegularFile(nodeRoot.resolve("node.exe"))
                ? nodeRoot.toString() : nodeRoot.resolve("bin").toString();
        builder.environment().put("PATH", nodeBin + java.io.File.pathSeparator
                + builder.environment().getOrDefault("PATH", ""));
        builder.environment().put("npm_config_cache", cacheRoot.resolve("npm").toString());
        Process process = builder.start();
        long deadlineNanos = System.nanoTime() + timeout.toNanos();
        InputStream input = process.getInputStream();
        var outputFuture = java.util.concurrent.CompletableFuture.supplyAsync(() -> {
            try (input) {
                return readBoundedOutput(input);
            } catch (IOException failure) {
                throw new java.util.concurrent.CompletionException(failure);
            }
        });
        Set<ObservedProcess> observedDescendants = ConcurrentHashMap.newKeySet();
        try {
            boolean finished = false;
            while (!finished) {
                process.descendants().map(ObservedProcess::capture).forEach(observedDescendants::add);
                long remaining = deadlineNanos - System.nanoTime();
                if (remaining <= 0) {
                    try {
                        destroyProcessTree(process, observedDescendants);
                    } finally {
                        outputFuture.cancel(true);
                    }
                    throw new IOException("Process timed out: " + command.getFirst());
                }
                finished = process.waitFor(Math.min(remaining, TimeUnit.MILLISECONDS.toNanos(2)),
                        TimeUnit.NANOSECONDS);
            }
        } catch (InterruptedException interrupted) {
            try {
                destroyProcessTree(process);
            } finally {
                outputFuture.cancel(true);
                Thread.currentThread().interrupt();
            }
            throw new IOException("Interrupted while running setup process.", interrupted);
        }
        String output;
        try {
            long remaining = deadlineNanos - System.nanoTime();
            if (remaining <= 0) throw new java.util.concurrent.TimeoutException();
            output = new String(outputFuture.get(remaining, TimeUnit.NANOSECONDS),
                    java.nio.charset.StandardCharsets.UTF_8);
        } catch (java.util.concurrent.TimeoutException timeoutFailure) {
            try {
                destroyProcessTree(process, observedDescendants);
            } finally {
                outputFuture.cancel(true);
            }
            throw new IOException("Process output did not close before the timeout: " + command.getFirst(),
                    timeoutFailure);
        } catch (InterruptedException interrupted) {
            try {
                destroyProcessTree(process, observedDescendants);
            } finally {
                outputFuture.cancel(true);
                Thread.currentThread().interrupt();
            }
            throw new IOException("Interrupted while reading setup process output.", interrupted);
        } catch (java.util.concurrent.ExecutionException failure) {
            IOException outputFailure = failure.getCause() instanceof IOException io
                    ? io : new IOException("Unable to read setup process output.", failure.getCause());
            try {
                destroyProcessTree(process, observedDescendants);
            } catch (IOException cleanupFailure) {
                outputFailure.addSuppressed(cleanupFailure);
            } finally {
                outputFuture.cancel(true);
            }
            throw outputFailure;
        }
        destroyProcessTree(process, observedDescendants);
        if (log != null) Files.writeString(log, output, java.nio.charset.StandardCharsets.UTF_8,
                StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        return new ProcessResult(process.exitValue(), output);
    }

    private static byte[] readBoundedOutput(InputStream input) throws IOException {
        int maximum = 1024 * 1024;
        var output = new java.io.ByteArrayOutputStream(Math.min(maximum, 64 * 1024));
        byte[] buffer = new byte[16 * 1024];
        int total = 0;
        for (int read; (read = input.read(buffer)) >= 0;) {
            total += read;
            if (total > maximum) {
                throw new IOException("Setup process output exceeded the 1 MiB safety limit.");
            }
            output.write(buffer, 0, read);
        }
        return output.toByteArray();
    }

    private static void waitForTermination(Process process, long deadlineNanos) throws IOException {
        try {
            long remaining = deadlineNanos - System.nanoTime();
            if (remaining <= 0 || !process.waitFor(remaining, TimeUnit.NANOSECONDS)) {
                throw new IOException("Setup process did not terminate after forced destruction.");
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for setup process termination.", interrupted);
        }
    }

    private static void destroyProcessTree(Process process) throws IOException {
        destroyProcessTree(process, Set.of());
    }

    private static void destroyProcessTree(Process process, Set<ObservedProcess> observed) throws IOException {
        long deadlineNanos = System.nanoTime() + Duration.ofSeconds(10).toNanos();
        var current = process.descendants().collect(java.util.stream.Collectors.toSet());
        var known = new java.util.LinkedHashSet<>(current);
        observed.stream().filter(ObservedProcess::stillMatches).map(ObservedProcess::handle).forEach(known::add);
        List<ProcessHandle> descendants = known.stream().toList().reversed();
        descendants.forEach(handle -> {
            if (handle.isAlive()) handle.destroyForcibly();
        });
        process.destroyForcibly();
        waitForTermination(process, deadlineNanos);
        for (ProcessHandle descendant : descendants) {
            try {
                long remaining = deadlineNanos - System.nanoTime();
                if (remaining <= 0) throw new java.util.concurrent.TimeoutException();
                descendant.onExit().get(remaining, TimeUnit.NANOSECONDS);
            } catch (java.util.concurrent.TimeoutException timeout) {
                throw new IOException("Setup descendant process did not terminate: " + descendant.pid(), timeout);
            } catch (java.util.concurrent.ExecutionException failure) {
                throw new IOException("Unable to observe setup descendant termination: " + descendant.pid(), failure);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while waiting for setup descendant termination.", interrupted);
            }
        }
    }

    private record ObservedProcess(ProcessHandle handle, Optional<Instant> started) {
        private static ObservedProcess capture(ProcessHandle handle) {
            return new ObservedProcess(handle, handle.info().startInstant());
        }

        private boolean stillMatches() {
            return handle.isAlive() && started.isPresent() && started.equals(handle.info().startInstant());
        }
    }

    private void writeReceipt(SetupReceipt receipt) throws IOException {
        Files.createDirectories(paths.receipts());
        Path destination = paths.receipts().resolve("reporting.json");
        Path temporary = Files.createTempFile(paths.receipts(), "reporting", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(receipt));
            VerifiedArtifactStore.move(temporary, destination);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private static void copyReportingManifest(Path staging, String expectedLockChecksum) throws IOException {
        for (String name : List.of("package.json", "package-lock.json")) {
            String resource = "/com/shaft/infrastructure/reporting/" + name;
            try (InputStream input = ReportingSetupService.class.getResourceAsStream(resource)) {
                if (input == null) throw new IOException("Missing bundled reporting manifest: " + name);
                byte[] content = input.readAllBytes();
                if ("package-lock.json".equals(name)) {
                    String canonical = new String(content, java.nio.charset.StandardCharsets.UTF_8)
                            .replace("\r\n", "\n").replace('\r', '\n');
                    content = canonical.getBytes(java.nio.charset.StandardCharsets.UTF_8);
                }
                Files.write(staging.resolve(name), content);
            }
        }
        String actual = "sha256:" + VerifiedArtifactStore.digest(staging.resolve("package-lock.json"));
        if (!actual.equalsIgnoreCase(expectedLockChecksum)) {
            throw new IOException("Bundled reporting dependency lock does not match the approved plan.");
        }
    }

    private Path nodeRoot() {
        return nodeRoot(paths, platform, architecture);
    }

    private static Path nodeRoot(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture) {
        return paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
    }

    private Path allureRoot() {
        return paths.tools().resolve("allure").resolve(ReportingSetupPlanner.ALLURE_VERSION);
    }

    private Path nodeExecutable() { return executable(nodeRoot(), "node"); }
    private Path npmCli() {
        return platform == SetupPlatform.WINDOWS
                ? nodeRoot().resolve("node_modules/npm/bin/npm-cli.js")
                : nodeRoot().resolve("lib/node_modules/npm/bin/npm-cli.js");
    }
    private Path allureEntryPoint() { return allureRoot().resolve("node_modules/allure/cli.js"); }

    private Path executable(Path root, String name) {
        return platform == SetupPlatform.WINDOWS ? root.resolve(name + ".exe") : root.resolve("bin").resolve(name);
    }

    private static Path stagingDirectory(Path destination) throws IOException {
        Files.createDirectories(destination.getParent());
        return Files.createTempDirectory(destination.getParent(), destination.getFileName() + ".staging-");
    }

    private static void publish(Path staging, Path destination) throws IOException {
        publish(staging, destination, VerifiedArtifactStore::move);
    }

    static void publish(Path staging, Path destination, MoveOperation mover) throws IOException {
        Path quarantine = destination.resolveSibling(destination.getFileName() + ".quarantine");
        VerifiedArtifactStore.replaceWithRollback(staging, destination, quarantine, mover::move);
    }

    private static void extractZip(Path archive, Path destination) throws IOException {
        try (ZipInputStream input = new ZipInputStream(Files.newInputStream(archive))) {
            for (ZipEntry entry; (entry = input.getNextEntry()) != null;) {
                Path target = archiveTarget(destination, entry.getName());
                if (target == null) continue;
                if (entry.isDirectory()) Files.createDirectories(target);
                else {
                    Files.createDirectories(target.getParent());
                    Files.copy(input, target);
                }
            }
        }
    }

    private static void extractTar(Path archive, Path destination) throws IOException {
        InputStream raw = Files.newInputStream(archive);
        InputStream compressed = new GzipCompressorInputStream(raw);
        try (TarArchiveInputStream input = new TarArchiveInputStream(compressed)) {
            for (TarArchiveEntry entry; (entry = input.getNextEntry()) != null;) {
                if (entry.isSymbolicLink() || entry.isLink()) throw new IOException("Archive links are not allowed.");
                Path target = archiveTarget(destination, entry.getName());
                if (target == null) continue;
                if (entry.isDirectory()) Files.createDirectories(target);
                else if (entry.isFile()) {
                    Files.createDirectories(target.getParent());
                    Files.copy(input, target);
                    if ((entry.getMode() & 0111) != 0) makeExecutable(target);
                }
            }
        }
    }

    private static Path archiveTarget(Path destination, String name) throws IOException {
        String normalized = name.replace('\\', '/');
        int firstSlash = normalized.indexOf('/');
        if (firstSlash < 0 || firstSlash == normalized.length() - 1) return null;
        Path target = destination.resolve(normalized.substring(firstSlash + 1)).normalize();
        if (!target.startsWith(destination)) throw new IOException("Archive entry escapes its target: " + name);
        return target;
    }

    private static void makeExecutable(Path file) throws IOException {
        try {
            Set<PosixFilePermission> permissions = EnumSet.copyOf(Files.getPosixFilePermissions(file));
            permissions.add(PosixFilePermission.OWNER_EXECUTE);
            permissions.add(PosixFilePermission.GROUP_EXECUTE);
            permissions.add(PosixFilePermission.OTHERS_EXECUTE);
            Files.setPosixFilePermissions(file, permissions);
        } catch (UnsupportedOperationException ignored) {
            if (!file.toFile().setExecutable(true, false)) throw new IOException("Cannot mark executable: " + file);
        }
    }

    private static void deleteTree(Path root) throws IOException {
        if (root == null || Files.notExists(root)) return;
        try (var paths = Files.walk(root)) {
            List<Path> reverse = paths.sorted(java.util.Comparator.reverseOrder()).toList();
            for (Path path : reverse) Files.deleteIfExists(path);
        }
    }

    private static void requireSuccessful(ProcessResult result, String message) throws IOException {
        if (result.exitCode() != 0) throw new IOException(message + System.lineSeparator() + result.output());
    }

    @FunctionalInterface
    interface ArtifactFetcher { Path fetch(SetupAction action) throws IOException; }
    @FunctionalInterface
    private interface IoOperation { void run() throws IOException; }
    @FunctionalInterface
    interface MoveOperation { void move(Path source, Path destination) throws IOException; }
    @FunctionalInterface
    interface CommandRunner { ProcessResult run(List<String> command, Path log, Duration timeout) throws IOException; }
    record ProcessResult(int exitCode, String output) { }
    private static final class SetupOperationException extends RuntimeException {
        private SetupOperationException(IOException cause) { super(cause); }
    }
}
