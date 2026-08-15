package com.shaft.ai.local;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.json.JsonSchemaValidator;

import java.io.IOException;
import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.SecureRandom;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Pattern;

/** Bounded, authenticated loopback lifecycle for the SHAFT-owned llama.cpp process. */
final class ManagedLocalAiProcess {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final tools.jackson.databind.ObjectMapper PILOT_JSON = new tools.jackson.databind.ObjectMapper();
    private static final int MAXIMUM_INFERENCE_RESPONSE_BYTES = 1024 * 1024;
    private static final int MAXIMUM_STARTUP_BYTES = 64 * 1024;
    private static final int MAXIMUM_STARTUP_LINE_BYTES = 4 * 1024;
    static final long MAX_PROCESS_TREE_RSS_BYTES = 4L * 1024 * 1024 * 1024;
    private static final Duration PROCESS_TREE_RSS_SAMPLE_INTERVAL = Duration.ofMillis(50);
    private static final SecureRandom RANDOM = new SecureRandom();
    private static final int MAX_LAUNCH_ATTEMPTS = 3;
    private static final Pattern LISTENING_ENDPOINT = Pattern.compile(
            "^srv\\s+llama_server:\\s+listening on http://127\\.0\\.0\\.1:(\\d{1,5})$");
    private static final ReentrantLock LAUNCH_LOCK = new ReentrantLock();
    private static final Object LOG_WRITE_LOCK = new Object();
    private static final java.util.concurrent.atomic.AtomicReference<Session> FAILED_LAUNCH =
            new java.util.concurrent.atomic.AtomicReference<>();
    private static final java.util.concurrent.atomic.AtomicReference<Session> ACTIVE_LAUNCH =
            new java.util.concurrent.atomic.AtomicReference<>();
    private static final java.util.concurrent.atomic.AtomicReference<LaunchReservation> PENDING_LAUNCH =
            new java.util.concurrent.atomic.AtomicReference<>();
    private static final Set<Process> SUPERVISED_PROCESSES = java.util.concurrent.ConcurrentHashMap.newKeySet();
    private static final HttpClient LOOPBACK_HTTP = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(2)).followRedirects(HttpClient.Redirect.NEVER).build();
    private static final Set<String> ENVIRONMENT_ALLOWLIST = Set.of(
            "SYSTEMROOT", "WINDIR", "TEMP", "TMP", "TMPDIR", "LANG", "LC_ALL", "LC_CTYPE", "TZ",
            "HOME", "USERPROFILE");

    private ManagedLocalAiProcess() {
    }

    static Map<String, String> runtimeEnvironment(Map<String, String> source) {
        Objects.requireNonNull(source, "source");
        Map<String, String> result = new LinkedHashMap<>();
        source.forEach((key, value) -> {
            if (key != null && value != null && ENVIRONMENT_ALLOWLIST.contains(key.toUpperCase(Locale.ROOT))) {
                result.put(key, value);
            }
        });
        return Map.copyOf(result);
    }

    static List<String> command(Path executable, Path model, int port, String alias, Path apiKeyFile, int threads) {
        requireRegular(executable, "runtime executable");
        requireRegular(model, "model");
        requireRegular(apiKeyFile, "API key file");
        if (port < 0 || port > 65_535 || alias == null || alias.isBlank()
                || threads < 1) {
            throw new IllegalArgumentException("Managed local AI launch parameters are invalid.");
        }
        return List.of(executable.toAbsolutePath().toString(), "--host", "127.0.0.1", "--port",
                Integer.toString(port), "--model", model.toAbsolutePath().toString(), "--alias", alias,
                "--api-key-file", apiKeyFile.toAbsolutePath().toString(), "--threads", Integer.toString(threads));
    }

    static Session launch(LaunchRequest request, ProcessStarter starter, IdentityProbe identity) throws Exception {
        return launch(request, new LaunchHooks(null, starter, identity));
    }

    static Session launch(LaunchRequest request, LaunchHooks hooks) throws Exception {
        return launchWithOwnership(request, hooks);
    }

    static Session launchManaged(LaunchRequest request, ProcessStarter starter, IdentityProbe identity)
            throws Exception {
        return launch(request, new LaunchHooks(ManagedLocalAiProcessTreeRss::sample, starter, identity));
    }

    static Session launchManaged(LaunchRequest request, ProcessTreeRssSampler rssSampler,
                                 ProcessStarter starter, IdentityProbe identity) throws Exception {
        return launch(request, new LaunchHooks(Objects.requireNonNull(rssSampler, "rssSampler"), starter, identity));
    }

    private static Session launchWithOwnership(LaunchRequest request, LaunchHooks hooks) throws Exception {
        Objects.requireNonNull(request, "request");
        Objects.requireNonNull(hooks, "hooks");
        if (request.timeout().isNegative() || request.timeout().isZero()) {
            throw new IllegalArgumentException("Launch timeout must be positive.");
        }
        long deadline = System.nanoTime() + request.timeout().toNanos();
        boolean locked = false;
        try {
            locked = LAUNCH_LOCK.tryLock(remaining(deadline).toNanos(), TimeUnit.NANOSECONDS);
            if (!locked) {
                throw new DeadlineExceededException();
            }
            return launchLocked(request, hooks, deadline);
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw interrupted;
        } finally {
            if (locked) {
                LAUNCH_LOCK.unlock();
            }
        }
    }

    private static Session launchLocked(LaunchRequest request, LaunchHooks hooks, long deadline) throws Exception {
        validateLaunchOwnership();
        LaunchPlan plan = prepareLaunch(request);
        return retryLaunch(plan, hooks, deadline);
    }

    private static void validateLaunchOwnership() {
        Session failedLaunch = FAILED_LAUNCH.get();
        if (failedLaunch != null && failedLaunch.hasSurvivors()) {
            throw new IllegalStateException("A prior managed local AI launch still owns a surviving process tree.");
        }
        FAILED_LAUNCH.compareAndSet(failedLaunch, null);
        Session activeLaunch = ACTIVE_LAUNCH.get();
        if (activeLaunch != null && activeLaunch.hasSurvivors()) {
            throw new IllegalStateException("A managed local AI process tree is already active.");
        }
        ACTIVE_LAUNCH.compareAndSet(activeLaunch, null);
    }

    private static LaunchPlan prepareLaunch(LaunchRequest request) throws Exception {
        RuntimeFiles files = request.files();
        Path verifiedExecutable = ManagedLocalAiCache.verifyOwnedFile(files.cache(), files.executable());
        Path verifiedModel = ManagedLocalAiCache.verifyOwnedFile(files.cache(), files.model());
        requireContainedLog(files.cache(), files.log());
        Files.createDirectories(files.log().toAbsolutePath().normalize().getParent());
        requireContainedLog(files.cache(), files.log());
        reserveLog(files.log());
        return new LaunchPlan(request, verifiedExecutable, verifiedModel);
    }

    private static Session retryLaunch(LaunchPlan plan, LaunchHooks hooks, long deadline) throws Exception {
        Exception lastFailure = null;
        for (int attempt = 0; attempt < MAX_LAUNCH_ATTEMPTS; attempt++) {
            try {
                remaining(deadline);
            } catch (DeadlineExceededException expired) {
                lastFailure = expired;
                break;
            }
            try {
                return launchAttempt(plan, hooks, deadline);
            } catch (AttemptFailure failed) {
                lastFailure = failed.failure();
                if (failed.resourceFailure() != null) {
                    throw failed.resourceFailure();
                }
                if (failed.survivors()) {
                    break;
                }
            }
        }
        throw new IllegalStateException("Managed local AI process could not establish its authenticated identity.",
                lastFailure);
    }

    private static Session launchAttempt(LaunchPlan plan, LaunchHooks hooks, long deadline) throws Exception {
        AttemptState state = new AttemptState(plan.request().files().cache());
        try {
            publishPendingLaunch(plan.request(), state, deadline);
            startCandidate(plan, hooks, state);
            awaitCandidate(plan, hooks, state, deadline);
            deleteKeyFile(state.keyFile, null);
            return state.candidate;
        } catch (InterruptedException cancelled) {
            cleanupInterruptedAttempt(state, deadline, cancelled);
            throw cancelled;
        } catch (Exception failure) {
            throw cleanupFailedAttempt(state, deadline, failure);
        } finally {
            state.reservation.resolve();
            PENDING_LAUNCH.compareAndSet(state.reservation, null);
        }
    }

    private static void publishPendingLaunch(LaunchRequest request, AttemptState state, long deadline)
            throws Exception {
        remaining(deadline);
        if (!PENDING_LAUNCH.compareAndSet(null, state.reservation)) {
            throw new IllegalStateException("Managed local AI process start is already pending.");
        }
        requireNotCancelled(request, state.reservation, "before process start");
    }

    private static void startCandidate(LaunchPlan plan, LaunchHooks hooks, AttemptState state) throws Exception {
        LaunchRequest request = plan.request();
        RuntimeSpec runtime = request.runtime();
        RuntimeFiles files = request.files();
        state.process = hooks.starter().start(
                command(plan.executable(), plan.model(), 0, runtime.alias(), state.keyFile, runtime.threads()),
                runtimeEnvironment(System.getenv()), files.log());
        synchronized (state.reservation) {
            state.reservation.bind(state.process);
            state.candidate = new Session(state.process, 0, runtime.alias(), state.key, request.timeout(),
                    plan.executable(), plan.model(), runtime.threads(), hooks.rssSampler());
            if (!ACTIVE_LAUNCH.compareAndSet(null, state.candidate)) {
                throw new IllegalStateException("Managed local AI process ownership is already registered.");
            }
        }
    }

    private static void awaitCandidate(LaunchPlan plan, LaunchHooks hooks, AttemptState state, long deadline)
            throws Exception {
        LaunchRequest request = plan.request();
        state.candidate.awaitFirstResourceSample(remaining(deadline));
        requireNotCancelled(request, state.reservation, "during process start");
        state.reservation.resolve();
        PENDING_LAUNCH.compareAndSet(state.reservation, null);
        StartupObservation startup = awaitStartup(state.process, remaining(deadline));
        state.candidate.bindLogCapture(captureLog(request.files().cache(), state.process.getInputStream(),
                request.files().log(), startup.captured(), state.key));
        int port = requireStartupPort(startup);
        state.candidate.bindPort(port);
        hooks.identity().await(state.process, port, state.key, request.runtime().alias(), remaining(deadline));
        state.candidate.requireResourceWithinLimit();
        if (!state.process.isAlive()) {
            throw new IllegalStateException("Managed local AI process exited during identity verification.");
        }
    }

    private static void requireNotCancelled(LaunchRequest request, LaunchReservation reservation, String phase) {
        if (request.cancelled().getAsBoolean() || reservation.cancelled()) {
            throw new IllegalStateException("Managed local AI launch was cancelled " + phase + '.');
        }
    }

    private static int requireStartupPort(StartupObservation startup) throws IOException {
        if (startup.port() == null) {
            throw new IOException("Managed local AI did not report its child-owned loopback endpoint.");
        }
        return startup.port();
    }

    private static void cleanupInterruptedAttempt(AttemptState state, long deadline,
                                                  InterruptedException cancelled) throws IOException {
        cleanupAttemptProcess(state, deadline, cancelled, true);
        deleteKeyFile(state.keyFile, cancelled);
        Thread.currentThread().interrupt();
    }

    private static AttemptFailure cleanupFailedAttempt(AttemptState state, long deadline, Exception failure)
            throws IOException {
        cleanupAttemptProcess(state, deadline, failure, false);
        deleteKeyFile(state.keyFile, failure);
        IllegalStateException resourceFailure = state.candidate == null ? null : state.candidate.resourceFailure();
        boolean survivors = state.candidate != null && state.candidate.hasSurvivors();
        if (survivors) {
            retainFailedLaunch(state.candidate);
        }
        return new AttemptFailure(failure, resourceFailure, survivors);
    }

    private static void cleanupAttemptProcess(AttemptState state, long deadline, Throwable primary,
                                              boolean retainInterruptedCandidate) {
        if (state.candidate != null) {
            state.candidate.close(cleanupTimeout(deadline), primary);
            if (retainInterruptedCandidate) {
                retainFailedLaunch(state.candidate);
            }
        } else if (state.process != null) {
            terminate(state.process, cleanupTimeout(deadline), primary);
        }
    }

    private static Duration cleanupTimeout(long deadline) {
        long nanos = Math.max(0, deadline - System.nanoTime());
        return Duration.ofNanos(Math.min(Duration.ofSeconds(2).toNanos(), nanos));
    }

    private static void retainFailedLaunch(Session session) {
        if (!session.hasSurvivors() || !FAILED_LAUNCH.compareAndSet(null, session)) {
            return;
        }
        Thread.ofVirtual().name("shaft-local-ai-failed-launch-reaper").start(() -> {
            try {
                while (session.hasSurvivors()) {
                    session.close(Duration.ofMillis(100),
                            new IllegalStateException("Failed managed local AI launch retained survivors."));
                    Thread.sleep(25);
                }
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
            } finally {
                if (!session.hasSurvivors()) {
                    ACTIVE_LAUNCH.compareAndSet(session, null);
                }
                FAILED_LAUNCH.compareAndSet(session, null);
            }
        });
    }

    static void terminateRetainedLaunches() {
        Throwable cleanup = new IllegalStateException("Managed local AI retained launch shutdown cleanup.");
        LaunchReservation pending = PENDING_LAUNCH.get();
        if (pending != null) {
            pending.cancel(cleanup);
            pending.awaitResolution(Duration.ofSeconds(2));
        }
        Set<Session> sessions = new java.util.LinkedHashSet<>();
        Session active = ACTIVE_LAUNCH.get();
        Session failed = FAILED_LAUNCH.get();
        if (active != null) {
            sessions.add(active);
        }
        if (failed != null) {
            sessions.add(failed);
        }
        for (Session session : sessions) {
            if (session.forceKillAndAwait(Duration.ofSeconds(2), cleanup)) {
                throw new IllegalStateException("Managed local AI process tree survived forced shutdown.", cleanup);
            }
            ACTIVE_LAUNCH.compareAndSet(session, null);
            FAILED_LAUNCH.compareAndSet(session, null);
        }
    }

    static void withLaunchExclusion(Duration timeout, CheckedRunnable action) throws Exception {
        boolean locked = false;
        try {
            locked = LAUNCH_LOCK.tryLock(timeout.toNanos(), TimeUnit.NANOSECONDS);
            if (!locked) {
                throw new IllegalStateException("Managed local AI launch ownership is busy.");
            }
            action.run();
        } finally {
            if (locked) {
                LAUNCH_LOCK.unlock();
            }
        }
    }

    static void requireIdentity(Process process, int port, String apiKey, String alias, Duration timeout,
                                JsonRequester requester) throws Exception {
        long deadline = System.nanoTime() + timeout.toNanos();
        Exception last = null;
        do {
            if (!process.isAlive()) {
                throw new IllegalStateException("Managed local AI process exited before identity verification.");
            }
            try {
                Map<String, Object> response = requester.get(
                        URI.create("http://127.0.0.1:" + port + "/v1/models"), "Bearer " + apiKey,
                        remaining(deadline));
                Object data = response.get("data");
                if (data instanceof List<?> models && models.size() == 1 && models.getFirst() instanceof Map<?, ?> model
                        && alias.equals(model.get("id"))) {
                    return;
                }
                last = new IllegalStateException("Authenticated runtime returned the wrong model identity.");
            } catch (InterruptedException cancelled) {
                Thread.currentThread().interrupt();
                throw cancelled;
            } catch (Exception failure) {
                last = failure;
            }
            Thread.sleep(10);
        } while (System.nanoTime() < deadline);
        throw new IllegalStateException("Managed local AI authenticated identity was not established.", last);
    }

    static void terminate(Process process, Duration timeout, Throwable primary) {
        terminate(process, timeout, primary, new java.util.LinkedHashSet<>());
    }

    private static boolean terminate(Process process, Duration timeout, Throwable primary,
                                     Set<ProcessHandle> retainedDescendants) {
        if (process == null) {
            return false;
        }
        revokeSupervisorOwnership(process, primary);
        if (SUPERVISED_PROCESSES.contains(process)) {
            return awaitSupervisorExit(process, timeout, primary);
        }
        return terminateDirectly(process, timeout, primary, retainedDescendants);
    }

    private static boolean awaitSupervisorExit(Process process, Duration timeout, Throwable primary) {
        try {
            boolean exited = process.waitFor(timeout.toNanos(), TimeUnit.NANOSECONDS);
            if (exited || !process.isAlive()) {
                SUPERVISED_PROCESSES.remove(process);
                return false;
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            suppress(primary, interrupted);
        }
        return true;
    }

    private static boolean terminateDirectly(Process process, Duration timeout, Throwable primary,
                                             Set<ProcessHandle> retainedDescendants) {
        Set<ProcessHandle> descendants = discoverDescendants(process, retainedDescendants, primary);
        long deadline = System.nanoTime() + timeout.toNanos();
        RuntimeException survivorFailure = null;
        try {
            survivorFailure = stopProcessTree(process, descendants, deadline);
        } catch (InterruptedException interrupted) {
            forceAfterInterruption(process, descendants, interrupted);
            Thread.currentThread().interrupt();
            suppress(primary, interrupted);
        } catch (Exception cleanup) {
            suppress(primary, cleanup);
        }
        boolean survivors = retainSurvivors(process, descendants, retainedDescendants);
        reportSurvivorFailure(primary, survivorFailure);
        return survivors;
    }

    private static Set<ProcessHandle> discoverDescendants(Process process, Set<ProcessHandle> retainedDescendants,
                                                          Throwable primary) {
        Set<ProcessHandle> discovered = new java.util.LinkedHashSet<>(retainedDescendants);
        try {
            discovered.addAll(process.toHandle().descendants().toList());
            discovered.forEach(ProcessHandle::destroy);
        } catch (Exception cleanup) {
            suppress(primary, cleanup);
        }
        return discovered;
    }

    private static RuntimeException stopProcessTree(Process process, Set<ProcessHandle> descendants, long deadline)
            throws InterruptedException {
        process.destroy();
        boolean parentExited = process.waitFor(Math.max(0, deadline - System.nanoTime()), TimeUnit.NANOSECONDS);
        descendants.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
        while (descendants.stream().anyMatch(ProcessHandle::isAlive) && System.nanoTime() < deadline) {
            Thread.sleep(10);
        }
        RuntimeException failure = descendants.stream().anyMatch(ProcessHandle::isAlive)
                ? new IllegalStateException("Managed local AI descendant did not terminate.") : null;
        if (parentExited) {
            return failure;
        }
        process.destroyForcibly();
        boolean forcedExit = process.waitFor(Math.max(0, deadline - System.nanoTime()), TimeUnit.NANOSECONDS);
        return forcedExit && !process.isAlive()
                ? failure : new IllegalStateException("Managed local AI process did not terminate.");
    }

    private static void forceAfterInterruption(Process process, Set<ProcessHandle> descendants,
                                               InterruptedException interrupted) {
        try {
            descendants.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
            if (process.isAlive()) {
                process.destroyForcibly();
            }
        } catch (Exception cleanup) {
            interrupted.addSuppressed(cleanup);
        }
    }

    private static boolean retainSurvivors(Process process, Set<ProcessHandle> descendants,
                                           Set<ProcessHandle> retainedDescendants) {
        retainedDescendants.clear();
        descendants.stream().filter(ProcessHandle::isAlive).forEach(retainedDescendants::add);
        return process.isAlive() || !retainedDescendants.isEmpty();
    }

    private static void reportSurvivorFailure(Throwable primary, RuntimeException survivorFailure) {
        if (survivorFailure != null) {
            if (primary != null) {
                primary.addSuppressed(survivorFailure);
            } else {
                throw survivorFailure;
            }
        }
    }

    private static void suppress(Throwable primary, Throwable cleanup) {
        if (primary != null) {
            primary.addSuppressed(cleanup);
        }
    }

    private static void revokeSupervisorOwnership(Process process, Throwable primary) {
        try {
            process.getOutputStream().close();
        } catch (IOException cleanup) {
            if (primary != null) {
                primary.addSuppressed(cleanup);
            }
        }
    }

    private static void requireRegular(Path path, String label) {
        if (path == null || !Files.isRegularFile(path, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                || Files.isSymbolicLink(path)) {
            throw new IllegalArgumentException("Managed local AI " + label + " is not a verified regular file.");
        }
    }

    private static void requireContainedLog(Path cache, Path log) throws IOException {
        Path cacheRoot = cache.toAbsolutePath().normalize();
        Path value = log.toAbsolutePath().normalize();
        Path parent = value.getParent();
        Path logs = cacheRoot.resolve("staging/logs");
        if (parent == null || !parent.equals(logs) || !value.startsWith(cacheRoot)) {
            throw new IllegalArgumentException("Managed local AI log must be inside cache staging/logs.");
        }
        Path current = cacheRoot;
        for (Path part : cacheRoot.relativize(parent)) {
            current = current.resolve(part);
            if (Files.exists(current, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                var attributes = Files.readAttributes(current, java.nio.file.attribute.BasicFileAttributes.class,
                        java.nio.file.LinkOption.NOFOLLOW_LINKS);
                if (attributes.isSymbolicLink() || attributes.isOther()) {
                    throw new IllegalArgumentException("Managed local AI log path contains a link or reparse point.");
                }
            }
        }
        if (Files.exists(value, LinkOption.NOFOLLOW_LINKS)) {
            var attributes = Files.readAttributes(value, java.nio.file.attribute.BasicFileAttributes.class,
                    LinkOption.NOFOLLOW_LINKS);
            if (!attributes.isRegularFile() || attributes.isSymbolicLink() || attributes.isOther()) {
                throw new IllegalArgumentException("Managed local AI log leaf is not a regular file.");
            }
        }
    }

    private static void reserveLog(Path log) throws IOException {
        try (var ignored = Files.newByteChannel(log, Set.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE,
                LinkOption.NOFOLLOW_LINKS))) {
            // The unique leaf is reserved before process launch.
        }
    }

    private static Path createKeyFile(Path cache, String key) throws IOException {
        Path directory = cache.toAbsolutePath().normalize().resolve("staging/secrets");
        Files.createDirectories(directory);
        requireOrdinaryDirectoryChain(cache.toAbsolutePath().normalize(), directory);
        if (System.getProperty("os.name").startsWith("Windows")) {
            restrictWindowsOwner(directory);
        }
        Path file = directory.resolve("key-" + secret());
        FileAttribute<?>[] attributes = Files.getFileStore(directory).supportsFileAttributeView("posix")
                ? new FileAttribute<?>[]{PosixFilePermissions.asFileAttribute(
                PosixFilePermissions.fromString("rw-------"))} : new FileAttribute<?>[0];
        try (var channel = Files.newByteChannel(file,
                Set.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS), attributes)) {
            channel.write(StandardCharsets.UTF_8.encode(key));
        }
        if (System.getProperty("os.name").startsWith("Windows")) {
            restrictWindowsOwner(file);
        }
        return file;
    }

    private static void restrictWindowsOwner(Path path) throws IOException {
        var view = Files.getFileAttributeView(path, java.nio.file.attribute.AclFileAttributeView.class,
                LinkOption.NOFOLLOW_LINKS);
        var owner = Files.getOwner(path, LinkOption.NOFOLLOW_LINKS);
        view.setAcl(List.of(java.nio.file.attribute.AclEntry.newBuilder()
                .setType(java.nio.file.attribute.AclEntryType.ALLOW).setPrincipal(owner)
                .setPermissions(java.nio.file.attribute.AclEntryPermission.values()).build()));
    }

    private static void requireOrdinaryDirectoryChain(Path cache, Path directory) throws IOException {
        Path current = cache;
        for (Path part : cache.relativize(directory)) {
            current = current.resolve(part);
            var attributes = Files.readAttributes(current, java.nio.file.attribute.BasicFileAttributes.class,
                    LinkOption.NOFOLLOW_LINKS);
            if (!attributes.isDirectory() || attributes.isSymbolicLink() || attributes.isOther()) {
                throw new IllegalArgumentException("Managed local AI secret path contains a link or reparse point.");
            }
        }
    }

    private static void deleteKeyFile(Path keyFile, Throwable primary) throws IOException {
        try {
            Files.deleteIfExists(keyFile);
        } catch (IOException cleanup) {
            if (primary != null) {
                primary.addSuppressed(cleanup);
                return;
            }
            throw cleanup;
        }
    }

    private static String secret() {
        byte[] bytes = new byte[32];
        RANDOM.nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }

    static Process start(List<String> command, Map<String, String> environment, Path log) throws IOException {
        Path java = Path.of(System.getProperty("java.home"), "bin",
                System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win") ? "java.exe" : "java");
        String classPath = System.getProperty("surefire.test.class.path", System.getProperty("java.class.path"));
        ProcessHandle current = ProcessHandle.current();
        String parentStartedAt = current.info().startInstant()
                .orElseThrow(() -> new IOException("Unable to establish the SHAFT process identity"))
                .toString();
        List<String> supervised = new java.util.ArrayList<>(List.of(java.toString(),
                ManagedLocalAiProcessSupervisor.class.getName(),
                Long.toString(current.pid()),
                parentStartedAt,
                managedWorkingDirectory(command).toString()));
        supervised.addAll(command);
        ProcessBuilder builder = new ProcessBuilder(supervised);
        builder.environment().clear();
        builder.environment().putAll(environment);
        builder.environment().put("CLASSPATH", classPath);
        builder.redirectErrorStream(true);
        Process supervisor = builder.start();
        SUPERVISED_PROCESSES.add(supervisor);
        return supervisor;
    }

    private static Path managedWorkingDirectory(List<String> command) throws IOException {
        if (command.isEmpty()) {
            throw new IllegalArgumentException("Managed local AI command is empty.");
        }
        Path executable = Path.of(command.getFirst());
        if (!executable.isAbsolute()) {
            throw new IllegalArgumentException("Managed local AI executable must be absolute.");
        }
        executable = executable.normalize();
        Path directory = executable.getParent();
        if (directory == null) {
            throw new IllegalArgumentException("Managed local AI executable directory is invalid.");
        }
        var executableAttributes = Files.readAttributes(executable,
                java.nio.file.attribute.BasicFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
        var directoryAttributes = Files.readAttributes(directory,
                java.nio.file.attribute.BasicFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
        if (!executableAttributes.isRegularFile() || executableAttributes.isSymbolicLink()
                || executableAttributes.isOther() || !directoryAttributes.isDirectory()
                || directoryAttributes.isSymbolicLink() || directoryAttributes.isOther()) {
            throw new IllegalArgumentException("Managed local AI executable path is not an ordinary managed file.");
        }
        return directory;
    }

    private static StartupObservation awaitStartup(Process process, Duration timeout) throws Exception {
        var executor = java.util.concurrent.Executors.newThreadPerTaskExecutor(
                Thread.ofVirtual().name("shaft-local-ai-startup-reader").factory());
        try {
            var task = executor.submit(() -> readStartup(process.getInputStream()));
            try {
                return task.get(timeout.toNanos(), TimeUnit.NANOSECONDS);
            } catch (java.util.concurrent.TimeoutException timeoutFailure) {
                task.cancel(true);
                try {
                    process.getInputStream().close();
                } catch (IOException cleanup) {
                    timeoutFailure.addSuppressed(cleanup);
                }
                throw new DeadlineExceededException(timeoutFailure);
            } catch (java.util.concurrent.ExecutionException failure) {
                if (failure.getCause() instanceof Exception exception) {
                    throw exception;
                }
                throw failure;
            }
        } finally {
            executor.shutdownNow();
        }
    }

    private static StartupObservation readStartup(InputStream input) throws IOException {
        ByteArrayOutputStream captured = new ByteArrayOutputStream();
        ByteArrayOutputStream line = new ByteArrayOutputStream();
        while (captured.size() < MAXIMUM_STARTUP_BYTES) {
            int next = input.read();
            if (next < 0) {
                return new StartupObservation(null, captured.toByteArray());
            }
            captured.write(next);
            if (next == '\n') {
                String text = line.toString(StandardCharsets.UTF_8).stripTrailing();
                var matcher = LISTENING_ENDPOINT.matcher(text);
                if (matcher.matches()) {
                    int port = Integer.parseInt(matcher.group(1));
                    if (port > 0 && port <= 65_535) {
                        return new StartupObservation(port, captured.toByteArray());
                    }
                    throw new IOException("Managed local AI reported an invalid loopback port.");
                }
                line.reset();
            } else {
                if (line.size() >= MAXIMUM_STARTUP_LINE_BYTES) {
                    throw new IOException("Managed local AI startup line exceeded the size limit.");
                }
                line.write(next);
            }
        }
        throw new IOException("Managed local AI startup output exceeded the size limit.");
    }

    private static Thread captureLog(Path cache, InputStream input, Path log, byte[] prefix, String secret)
            throws IOException {
        long maximumBytes = 1024 * 1024;
        appendSanitizedLog(cache, new ByteArrayInputStream(prefix), log, Set.of(secret), maximumBytes);
        Thread capture = Thread.ofVirtual().name("shaft-local-ai-log").unstarted(() -> {
            try (input) {
                appendSanitizedLog(cache, input, log, Set.of(secret), Math.max(1, maximumBytes - prefix.length));
            } catch (IOException ignored) {
                // Lifecycle status owns launch failure; logging must never mask it.
            }
        });
        capture.start();
        return capture;
    }

    private static void appendSanitizedLog(Path cache, InputStream input, Path log, Set<String> secrets,
                                           long maximumBytes) throws IOException {
        requireContainedLog(cache, log);
        ByteArrayOutputStream line = new ByteArrayOutputStream();
        long written = 0;
        boolean discardingOverlongLine = false;
        try (var output = Files.newByteChannel(log, Set.of(StandardOpenOption.WRITE,
                StandardOpenOption.APPEND, LinkOption.NOFOLLOW_LINKS))) {
            int next;
            while ((next = input.read()) >= 0) {
                if (discardingOverlongLine) {
                    if (next == '\n') {
                        discardingOverlongLine = false;
                    }
                    continue;
                }
                if (line.size() >= MAXIMUM_STARTUP_LINE_BYTES) {
                    line.reset();
                    discardingOverlongLine = true;
                    continue;
                }
                line.write(next);
                if (next == '\n') {
                    written += appendSanitizedLine(output, line.toByteArray(), secrets, maximumBytes - written);
                    line.reset();
                    if (written >= maximumBytes) {
                        input.transferTo(java.io.OutputStream.nullOutputStream());
                        return;
                    }
                }
            }
            if (!discardingOverlongLine && line.size() > 0 && written < maximumBytes) {
                appendSanitizedLine(output, line.toByteArray(), secrets, maximumBytes - written);
            }
        }
    }

    private static int appendSanitizedLine(java.nio.channels.SeekableByteChannel output, byte[] raw,
                                           Set<String> secrets, long remaining) throws IOException {
        String text = new String(raw, StandardCharsets.UTF_8)
                .replaceAll("(?i)authorization\\s*:\\s*bearer\\s+\\S+", "Authorization: [REDACTED]")
                .replaceAll("(?i)--api-key\\s+\\S+", "--api-key [REDACTED]");
        for (String secret : secrets) {
            if (secret != null && !secret.isEmpty()) {
                text = text.replace(secret, "[REDACTED]");
            }
        }
        byte[] sanitized = text.getBytes(StandardCharsets.UTF_8);
        int length = (int) Math.min(Math.max(0, remaining), sanitized.length);
        synchronized (LOG_WRITE_LOCK) {
            java.nio.ByteBuffer buffer = java.nio.ByteBuffer.wrap(sanitized, 0, length);
            while (buffer.hasRemaining()) {
                output.write(buffer);
            }
        }
        return length;
    }

    static void writeSanitizedLog(Path cache, InputStream input, Path log, Set<String> secrets, long maximumBytes)
            throws IOException {
        if (maximumBytes < 1) {
            throw new IllegalArgumentException("Log byte ceiling must be positive.");
        }
        requireContainedLog(cache, log);
        Files.createDirectories(log.toAbsolutePath().normalize().getParent());
        requireContainedLog(cache, log);
        if (!Files.exists(log, LinkOption.NOFOLLOW_LINKS)) {
            reserveLog(log);
        }
        byte[] raw = input.readNBytes((int) Math.min(Integer.MAX_VALUE, maximumBytes * 4 + 1));
        String text = new String(raw, StandardCharsets.UTF_8)
                .replaceAll("(?i)authorization\\s*:\\s*bearer\\s+\\S+", "Authorization: [REDACTED]")
                .replaceAll("(?i)--api-key\\s+\\S+", "--api-key [REDACTED]");
        for (String secret : secrets) {
            if (secret != null && !secret.isEmpty()) {
                text = text.replace(secret, "[REDACTED]");
            }
        }
        byte[] sanitized = text.getBytes(StandardCharsets.UTF_8);
        int length = (int) Math.min(maximumBytes, sanitized.length);
        try (var output = Files.newByteChannel(log, Set.of(StandardOpenOption.WRITE,
                StandardOpenOption.TRUNCATE_EXISTING, LinkOption.NOFOLLOW_LINKS))) {
            output.write(java.nio.ByteBuffer.wrap(sanitized, 0, length));
        }
        input.transferTo(java.io.OutputStream.nullOutputStream());
    }

    static Map<String, Object> requestIdentity(URI uri, String bearer, Duration timeout) throws Exception {
        long deadline = System.nanoTime() + timeout.toNanos();
        HttpRequest request = HttpRequest.newBuilder(uri).timeout(timeout)
                .header("Authorization", bearer).GET().build();
        HttpResponse<InputStream> response = LOOPBACK_HTTP.send(request, HttpResponse.BodyHandlers.ofInputStream());
        byte[] body = boundedRead(response.body(), 64 * 1024, remaining(deadline));
        if (response.statusCode() != 200 || body.length > 64 * 1024) {
            throw new IOException("Managed local AI identity response was rejected.");
        }
        JsonNode root = JSON.readTree(body);
        if (!root.isObject() || !root.path("data").isArray()) {
            throw new IOException("Managed local AI identity response is malformed.");
        }
        List<Map<String, Object>> data = new ArrayList<>();
        root.path("data").forEach(item -> {
            if (item.isObject() && item.path("id").isTextual()) {
                data.add(Map.of("id", item.path("id").textValue()));
            }
        });
        return Map.of("data", data);
    }

    static AiResponse infer(Session session, AiRequest request) {
        return infer(session, request, ManagedLocalAiProcess::requestInference);
    }

    static AiResponse infer(Session session, AiRequest request, InferenceRequester requester) {
        return infer(session, request, requester, System.nanoTime() + request.timeout().toNanos());
    }

    static AiResponse infer(Session session, AiRequest request, InferenceRequester requester, long deadline) {
        Objects.requireNonNull(session, "session");
        Objects.requireNonNull(request, "request");
        Objects.requireNonNull(requester, "requester");
        Instant started = Instant.now();
        try {
            session.requireResourceWithinLimit();
            tools.jackson.databind.node.ObjectNode body = PILOT_JSON.createObjectNode();
            body.put("model", session.alias());
            body.put("stream", false);
            long requestedOutputTokens = request.budget().maxOutputTokens();
            long configuredOutputTokens = PilotConfiguration.current().maxOutputTokens();
            body.put("max_tokens", requestedOutputTokens <= 0
                    ? configuredOutputTokens : Math.min(requestedOutputTokens, configuredOutputTokens));
            tools.jackson.databind.node.ObjectNode message = body.putArray("messages").addObject();
            message.put("role", "user");
            message.put("content", prompt(request));
            tools.jackson.databind.node.ObjectNode format = body.putObject("response_format");
            format.put("type", "json_object");
            format.set("schema", request.desiredResponseSchema());

            URI endpoint = URI.create("http://127.0.0.1:" + session.port() + "/v1/chat/completions");
            String serialized = PILOT_JSON.writeValueAsString(body);
            InferenceResponse response = requester.send(endpoint, "Bearer " + session.apiKey(), serialized,
                    remaining(deadline));
            byte[] responseBody;
            try (InputStream input = response.body()) {
                AiResponseStatus httpFailure = inferenceStatus(response.statusCode());
                if (httpFailure != null) {
                    return inferenceFailure(request, httpFailure, inferenceReason(httpFailure),
                            Duration.between(started, Instant.now()), session.alias());
                }
                responseBody = boundedRead(input, MAXIMUM_INFERENCE_RESPONSE_BYTES, remaining(deadline));
            }
            session.requireResourceWithinLimit();
            Duration duration = Duration.between(started, Instant.now());
            if (responseBody.length > MAXIMUM_INFERENCE_RESPONSE_BYTES) {
                return inferenceFailure(request, AiResponseStatus.INVALID_RESPONSE,
                        "Managed local inference response exceeded the size limit.", duration, session.alias());
            }
            tools.jackson.databind.JsonNode root = PILOT_JSON.readTree(responseBody);
            if (!root.isObject() || !session.alias().equals(root.path("model").asText())
                    || !root.path("choices").isArray() || root.path("choices").size() != 1
                    || !root.path("choices").get(0).path("message").path("content").isTextual()) {
                return inferenceFailure(request, AiResponseStatus.INVALID_RESPONSE,
                        "Managed local inference response was malformed.", duration, session.alias());
            }
            tools.jackson.databind.JsonNode payload = PILOT_JSON.readTree(
                    root.path("choices").get(0).path("message").path("content").asText());
            if (!JsonSchemaValidator.validate(request.desiredResponseSchema(), payload).isEmpty()) {
                return inferenceFailure(request, AiResponseStatus.INVALID_RESPONSE,
                        "Managed local inference response did not match the requested schema.",
                        duration, session.alias());
            }
            tools.jackson.databind.JsonNode usage = root.path("usage");
            return AiResponse.success("managed-local", session.alias(), payload, duration,
                    new AiUsage(Math.max(0, usage.path("prompt_tokens").asLong()),
                            Math.max(0, usage.path("completion_tokens").asLong()), null),
                    request.deterministicFallback());
        } catch (java.net.http.HttpTimeoutException | DeadlineExceededException timeout) {
            return inferenceFailure(request, AiResponseStatus.TIMEOUT, "Managed local inference timed out.",
                    Duration.between(started, Instant.now()), session.alias());
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            return inferenceFailure(request, AiResponseStatus.ERROR, "Managed local inference was interrupted.",
                    Duration.between(started, Instant.now()), session.alias());
        } catch (tools.jackson.core.JacksonException malformed) {
            return inferenceFailure(request, AiResponseStatus.INVALID_RESPONSE,
                    "Managed local inference response was malformed.",
                    Duration.between(started, Instant.now()), session.alias());
        } catch (Exception unavailable) {
            return inferenceFailure(request, AiResponseStatus.PROVIDER_UNAVAILABLE,
                    "Managed local inference is unavailable.",
                    Duration.between(started, Instant.now()), session.alias());
        }
    }

    static InferenceResponse requestInference(URI endpoint, String bearer, String body, Duration timeout)
            throws Exception {
        HttpRequest httpRequest = HttpRequest.newBuilder(endpoint).timeout(timeout)
                .header("Authorization", bearer).header("Accept", "application/json")
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body)).build();
        HttpResponse<InputStream> response = LOOPBACK_HTTP.send(httpRequest, HttpResponse.BodyHandlers.ofInputStream());
        return new InferenceResponse(response.statusCode(), response.body());
    }

    static Duration remaining(long deadline) throws DeadlineExceededException {
        long nanos = deadline - System.nanoTime();
        if (nanos <= 0) {
            throw new DeadlineExceededException();
        }
        return Duration.ofNanos(nanos);
    }

    private static String prompt(AiRequest request) {
        StringBuilder result = new StringBuilder("Purpose: ").append(request.purpose());
        if (!request.text().isBlank()) {
            result.append('\n').append(request.text());
        }
        request.evidence().forEach(evidence -> result.append("\n\nEvidence ").append(evidence.id())
                .append(" [").append(evidence.category()).append("]:\n").append(evidence.content()));
        return result.toString();
    }

    private static AiResponse inferenceFailure(AiRequest request, AiResponseStatus status, String reason,
                                               Duration duration, String model) {
        return AiResponse.failure(status, "managed-local", model, reason, duration,
                request.deterministicFallback());
    }

    private static AiResponseStatus inferenceStatus(int statusCode) {
        if (statusCode >= 200 && statusCode < 300) {
            return null;
        }
        if (statusCode == 401 || statusCode == 403) {
            return AiResponseStatus.AUTHENTICATION_FAILED;
        }
        if (statusCode == 408 || statusCode == 504) {
            return AiResponseStatus.TIMEOUT;
        }
        if (statusCode == 429) {
            return AiResponseStatus.RATE_LIMITED;
        }
        return statusCode >= 500 ? AiResponseStatus.PROVIDER_UNAVAILABLE : AiResponseStatus.ERROR;
    }

    private static String inferenceReason(AiResponseStatus status) {
        return switch (status) {
            case AUTHENTICATION_FAILED -> "Managed local inference authentication failed.";
            case RATE_LIMITED -> "Managed local inference rate limit was reached.";
            case TIMEOUT -> "Managed local inference timed out.";
            case PROVIDER_UNAVAILABLE -> "Managed local inference is unavailable.";
            default -> "Managed local inference request was rejected.";
        };
    }

    private static byte[] boundedRead(InputStream input, int maximumBytes, Duration timeout) throws Exception {
        var executor = java.util.concurrent.Executors.newThreadPerTaskExecutor(
                Thread.ofVirtual().name("shaft-local-ai-identity-reader").factory());
        try {
            var task = executor.submit(() -> input.readNBytes(maximumBytes + 1));
            try {
                return task.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
            } catch (java.util.concurrent.TimeoutException timeoutFailure) {
                task.cancel(true);
                try {
                    input.close();
                } catch (IOException cleanup) {
                    timeoutFailure.addSuppressed(cleanup);
                }
                throw new DeadlineExceededException(timeoutFailure);
            } catch (java.util.concurrent.ExecutionException failure) {
                if (failure.getCause() instanceof Exception exception) {
                    throw exception;
                }
                throw failure;
            }
        } finally {
            executor.shutdownNow();
        }
    }

    static final class Session implements AutoCloseable {
        private final Process process;
        private volatile int port;
        private final String alias;
        private final String apiKey;
        private final Duration shutdownTimeout;
        private final Path executable;
        private final Path model;
        private final int threads;
        private final Set<ProcessHandle> retainedDescendants = java.util.concurrent.ConcurrentHashMap.newKeySet();
        private final java.util.concurrent.atomic.AtomicReference<IllegalStateException> resourceFailure =
                new java.util.concurrent.atomic.AtomicReference<>();
        private final java.util.concurrent.atomic.AtomicBoolean stopResourceMonitoring =
                new java.util.concurrent.atomic.AtomicBoolean();
        private final java.util.concurrent.CountDownLatch firstResourceSample =
                new java.util.concurrent.CountDownLatch(1);
        private volatile long peakRssBytes;
        private volatile Thread logCapture;
        private volatile Thread resourceMonitor;

        Session(Process process, int port, String alias, String apiKey, Duration shutdownTimeout) {
            this(process, port, alias, apiKey, shutdownTimeout, null, null, -1, null);
        }

        Session(Process process, int port, String alias, String apiKey, Duration shutdownTimeout,
                ProcessTreeRssSampler sampler) {
            this(process, port, alias, apiKey, shutdownTimeout, null, null, -1,
                    Objects.requireNonNull(sampler, "sampler"));
        }

        private Session(Process process, int port, String alias, String apiKey, Duration shutdownTimeout,
                        Path executable, Path model, int threads, ProcessTreeRssSampler sampler) {
            this.process = process;
            this.port = port;
            this.alias = alias;
            this.apiKey = apiKey;
            this.shutdownTimeout = shutdownTimeout;
            this.executable = normalizeIdentity(executable);
            this.model = normalizeIdentity(model);
            this.threads = threads;
            captureDescendants();
            Thread.ofVirtual().name("shaft-local-ai-process-tree-owner").start(() -> {
                try {
                    while (process.isAlive()) {
                        captureDescendants();
                        Thread.sleep(10);
                    }
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                }
            });
            if (sampler != null) {
                startResourceMonitor(sampler);
            }
        }

        private void startResourceMonitor(ProcessTreeRssSampler sampler) {
            resourceMonitor = Thread.ofVirtual().name("shaft-local-ai-process-tree-rss").start(() -> {
                try {
                    while (process.isAlive() && resourceFailure.get() == null && !stopResourceMonitoring.get()) {
                        captureDescendants();
                        long current = sampler.sample(process, Set.copyOf(retainedDescendants));
                        if (current < 0) {
                            throw new IOException("Managed local AI process-tree RSS cannot be negative.");
                        }
                        peakRssBytes = Math.max(peakRssBytes, current);
                        if (current > MAX_PROCESS_TREE_RSS_BYTES) {
                            failResourceLimit("Managed local AI process tree exceeded the 4 GiB RSS ceiling.", null);
                            return;
                        }
                        firstResourceSample.countDown();
                        TimeUnit.NANOSECONDS.sleep(PROCESS_TREE_RSS_SAMPLE_INTERVAL.toNanos());
                    }
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    if (process.isAlive() && !stopResourceMonitoring.get()) {
                        failResourceLimit("Managed local AI process-tree RSS enforcement was interrupted.",
                                interrupted);
                    }
                } catch (Exception | LinkageError unavailable) {
                    if (!stopResourceMonitoring.get()) {
                        failResourceLimit("Managed local AI process-tree RSS inventory is unavailable.", unavailable);
                    }
                } finally {
                    firstResourceSample.countDown();
                }
            });
        }

        private void awaitFirstResourceSample(Duration timeout)
                throws InterruptedException, DeadlineExceededException {
            if (resourceMonitor == null) {
                return;
            }
            if (!firstResourceSample.await(timeout.toNanos(), TimeUnit.NANOSECONDS)) {
                throw new DeadlineExceededException();
            }
            requireResourceWithinLimit();
        }

        private void failResourceLimit(String message, Throwable cause) {
            IllegalStateException failure = cause == null
                    ? new IllegalStateException(message) : new IllegalStateException(message, cause);
            if (!resourceFailure.compareAndSet(null, failure)) {
                return;
            }
            stopResourceMonitoring.set(true);
            forceKillAndAwait(shutdownTimeout, failure);
        }

        private void requireResourceWithinLimit() {
            IllegalStateException failure = resourceFailure.get();
            if (failure != null) {
                throw failure;
            }
            if (resourceMonitor != null && !process.isAlive()) {
                throw new IllegalStateException("Managed local AI process exited during RSS enforcement.");
            }
        }

        long peakRssBytes() {
            return peakRssBytes;
        }

        IllegalStateException resourceFailure() {
            return resourceFailure.get();
        }

        private void bindPort(int childPort) {
            if (port != 0 || childPort < 1 || childPort > 65_535) {
                throw new IllegalStateException("Managed local AI child port ownership is invalid.");
            }
            port = childPort;
        }

        private void bindLogCapture(Thread capture) {
            if (logCapture != null) {
                throw new IllegalStateException("Managed local AI log ownership is already bound.");
            }
            logCapture = Objects.requireNonNull(capture, "capture");
        }

        Process process() { return process; }
        int port() { return port; }
        String alias() { return alias; }
        String apiKey() { return apiKey; }
        Duration shutdownTimeout() { return shutdownTimeout; }
        boolean matches(Path expectedExecutable, Path expectedModel, String expectedAlias, int expectedThreads) {
            return executable != null && model != null && alias.equals(expectedAlias)
                    && executable.equals(normalizeIdentity(expectedExecutable))
                    && model.equals(normalizeIdentity(expectedModel)) && threads == expectedThreads;
        }

        private static Path normalizeIdentity(Path path) {
            return path == null ? null : path.toAbsolutePath().normalize();
        }

        synchronized boolean close(Duration timeout) {
            captureDescendants();
            return terminateAndJoinLog(timeout, null);
        }

        synchronized boolean close(Duration timeout, Throwable primary) {
            captureDescendants();
            return terminateAndJoinLog(timeout, primary);
        }

        private boolean terminateAndJoinLog(Duration timeout, Throwable primary) {
            long deadline = System.nanoTime() + timeout.toNanos();
            stopResourceMonitoring.set(true);
            Thread monitor = resourceMonitor;
            if (monitor != null && !monitor.equals(Thread.currentThread())) {
                monitor.interrupt();
            }
            boolean survivors = terminate(process, timeout, primary, retainedDescendants);
            awaitResourceMonitor(deadline, primary);
            Thread capture = logCapture;
            if (capture != null && !capture.equals(Thread.currentThread())) {
                try {
                    long remaining = deadline - System.nanoTime();
                    if (remaining > 0) {
                        capture.join(remaining / 1_000_000, (int) (remaining % 1_000_000));
                    }
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    if (primary != null) {
                        primary.addSuppressed(interrupted);
                    }
                }
            }
            if (!survivors) {
                ACTIVE_LAUNCH.compareAndSet(this, null);
                FAILED_LAUNCH.compareAndSet(this, null);
            }
            return survivors;
        }

        synchronized boolean hasSurvivors() {
            return process.isAlive() || retainedDescendants.stream().anyMatch(ProcessHandle::isAlive);
        }

        void forceKillNow(Throwable primary) {
            captureDescendants();
            revokeSupervisorOwnership(process, primary);
            SUPERVISED_PROCESSES.remove(process);
            try {
                if (process.isAlive()) {
                    process.destroyForcibly();
                }
                captureDescendants();
                retainedDescendants.stream().filter(ProcessHandle::isAlive)
                        .forEach(ProcessHandle::destroyForcibly);
            } catch (RuntimeException cleanup) {
                if (primary != null) {
                    primary.addSuppressed(cleanup);
                }
            }
        }

        boolean forceKillAndAwait(Duration timeout, Throwable primary) {
            forceKillNow(primary);
            long deadline = System.nanoTime() + timeout.toNanos();
            try {
                long remaining;
                while ((process.isAlive() || retainedDescendants.stream().anyMatch(ProcessHandle::isAlive))
                        && (remaining = deadline - System.nanoTime()) > 0) {
                    if (SUPERVISED_PROCESSES.contains(process)) {
                        TimeUnit.NANOSECONDS.sleep(Math.min(remaining, Duration.ofMillis(10).toNanos()));
                        continue;
                    }
                    captureDescendants();
                    if (process.isAlive()) {
                        process.destroyForcibly();
                    }
                    retainedDescendants.stream().filter(ProcessHandle::isAlive)
                            .forEach(ProcessHandle::destroyForcibly);
                    TimeUnit.NANOSECONDS.sleep(Math.min(remaining, Duration.ofMillis(10).toNanos()));
                }
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                if (primary != null) {
                    primary.addSuppressed(interrupted);
                }
            } catch (Exception cleanup) {
                if (primary != null) {
                    primary.addSuppressed(cleanup);
                }
            }
            awaitLogCapture(deadline, primary);
            awaitResourceMonitor(deadline, primary);
            if (!process.isAlive()) {
                SUPERVISED_PROCESSES.remove(process);
            }
            return process.isAlive() || retainedDescendants.stream().anyMatch(ProcessHandle::isAlive);
        }

        private void awaitLogCapture(long deadline, Throwable primary) {
            Thread capture = logCapture;
            if (capture == null || capture.equals(Thread.currentThread())) {
                return;
            }
            try {
                long remaining = deadline - System.nanoTime();
                if (remaining > 0) {
                    capture.join(remaining / 1_000_000, (int) (remaining % 1_000_000));
                }
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                if (primary != null) {
                    primary.addSuppressed(interrupted);
                }
            }
        }

        private void awaitResourceMonitor(long deadline, Throwable primary) {
            Thread monitor = resourceMonitor;
            if (monitor == null || monitor.equals(Thread.currentThread())) {
                return;
            }
            try {
                long remaining = deadline - System.nanoTime();
                if (remaining > 0) {
                    monitor.join(remaining / 1_000_000, (int) (remaining % 1_000_000));
                }
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                if (primary != null) {
                    primary.addSuppressed(interrupted);
                }
            }
        }

        private void captureDescendants() {
            try {
                process.toHandle().descendants().forEach(retainedDescendants::add);
            } catch (RuntimeException ignored) {
                // Existing retained handles remain authoritative if the parent disappears during enumeration.
            }
        }

        @Override
        public void close() {
            close(shutdownTimeout);
        }
    }

    @FunctionalInterface interface ProcessStarter {
        Process start(List<String> command, Map<String, String> environment, Path log) throws Exception;
    }

    @FunctionalInterface interface ProcessTreeRssSampler {
        long sample(Process process, Set<ProcessHandle> retainedDescendants) throws Exception;
    }

    record RuntimeFiles(Path cache, Path executable, Path model, Path log) {
        RuntimeFiles {
            Objects.requireNonNull(cache, "cache");
            Objects.requireNonNull(executable, "executable");
            Objects.requireNonNull(model, "model");
            Objects.requireNonNull(log, "log");
        }
    }

    record RuntimeSpec(String alias, int threads) {
        RuntimeSpec {
            Objects.requireNonNull(alias, "alias");
        }
    }

    record LaunchRequest(RuntimeFiles files, RuntimeSpec runtime, Duration timeout,
                         java.util.function.BooleanSupplier cancelled) {
        LaunchRequest {
            Objects.requireNonNull(files, "files");
            Objects.requireNonNull(runtime, "runtime");
            Objects.requireNonNull(timeout, "timeout");
            Objects.requireNonNull(cancelled, "cancelled");
        }
    }

    record LaunchHooks(ProcessTreeRssSampler rssSampler, ProcessStarter starter, IdentityProbe identity) {
        LaunchHooks {
            Objects.requireNonNull(starter, "starter");
            Objects.requireNonNull(identity, "identity");
        }
    }

    private record LaunchPlan(LaunchRequest request, Path executable, Path model) {
    }

    private static final class AttemptState {
        private final String key = secret();
        private final Path keyFile;
        private final LaunchReservation reservation = new LaunchReservation(Thread.currentThread());
        private Process process;
        private Session candidate;

        private AttemptState(Path cache) throws IOException {
            keyFile = createKeyFile(cache, key);
        }
    }

    private static final class AttemptFailure extends Exception {
        private final Exception failure;
        private final IllegalStateException resourceFailure;
        private final boolean survivors;

        private AttemptFailure(Exception failure, IllegalStateException resourceFailure, boolean survivors) {
            super(failure);
            this.failure = failure;
            this.resourceFailure = resourceFailure;
            this.survivors = survivors;
        }

        private Exception failure() {
            return failure;
        }

        private IllegalStateException resourceFailure() {
            return resourceFailure;
        }

        private boolean survivors() {
            return survivors;
        }
    }

    private static final class LaunchReservation {
        private final Thread owner;
        private boolean cancelled;
        private boolean resolved;
        private Process process;

        private LaunchReservation(Thread owner) {
            this.owner = owner;
        }

        synchronized boolean cancelled() {
            return cancelled;
        }

        synchronized void bind(Process started) {
            process = Objects.requireNonNull(started, "started");
        }

        synchronized void cancel(Throwable primary) {
            cancelled = true;
            owner.interrupt();
            if (process != null && process.isAlive()) {
                revokeSupervisorOwnership(process, primary);
                if (!SUPERVISED_PROCESSES.contains(process)) {
                    try {
                        process.destroyForcibly();
                    } catch (RuntimeException cleanup) {
                        primary.addSuppressed(cleanup);
                    }
                }
            }
        }

        synchronized void resolve() {
            resolved = true;
            notifyAll();
        }

        synchronized boolean awaitResolution(Duration timeout) {
            long deadline = System.nanoTime() + timeout.toNanos();
            boolean interrupted = false;
            while (!resolved && System.nanoTime() < deadline) {
                try {
                    long remaining = deadline - System.nanoTime();
                    wait(Math.max(1, remaining / 1_000_000), (int) Math.max(0, remaining % 1_000_000));
                } catch (InterruptedException ignored) {
                    interrupted = true;
                }
            }
            if (interrupted) {
                Thread.currentThread().interrupt();
            }
            return resolved;
        }

    }
    @FunctionalInterface interface CheckedRunnable { void run() throws Exception; }
    @FunctionalInterface interface IdentityProbe {
        void await(Process process, int port, String apiKey, String alias, Duration timeout) throws Exception;
    }
    @FunctionalInterface interface JsonRequester {
        Map<String, Object> get(URI uri, String bearer, Duration timeout) throws Exception;
    }
    @FunctionalInterface interface InferenceRequester {
        InferenceResponse send(URI uri, String bearer, String body, Duration timeout) throws Exception;
    }
    record InferenceResponse(int statusCode, InputStream body) {
        InferenceResponse {
            Objects.requireNonNull(body, "body");
        }
    }
    private record StartupObservation(Integer port, byte[] captured) {
        private StartupObservation {
            captured = captured.clone();
        }

        @Override
        public byte[] captured() {
            return captured.clone();
        }
    }
    static final class DeadlineExceededException extends IOException {
        private DeadlineExceededException() {
            super("Managed local AI request deadline expired.");
        }
        private DeadlineExceededException(Throwable cause) {
            super("Managed local AI response body timed out.", cause);
        }
    }
}
