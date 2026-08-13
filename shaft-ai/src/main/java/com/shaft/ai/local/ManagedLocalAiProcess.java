package com.shaft.ai.local;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
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
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/** Bounded, authenticated loopback lifecycle for the SHAFT-owned llama.cpp process. */
final class ManagedLocalAiProcess {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final SecureRandom RANDOM = new SecureRandom();
    private static final int MAX_LAUNCH_ATTEMPTS = 3;
    private static final Set<String> ENVIRONMENT_ALLOWLIST = Set.of(
            "PATH", "SYSTEMROOT", "WINDIR", "TEMP", "TMP", "TMPDIR", "COMSPEC", "PATHEXT",
            "LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH", "DYLD_FALLBACK_LIBRARY_PATH", "LANG", "LC_ALL",
            "LC_CTYPE", "TZ", "HOME", "USERPROFILE");

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
        if (port < 1 || port > 65_535 || alias == null || alias.isBlank()
                || threads < 1) {
            throw new IllegalArgumentException("Managed local AI launch parameters are invalid.");
        }
        return List.of(executable.toAbsolutePath().toString(), "--host", "127.0.0.1", "--port",
                Integer.toString(port), "--model", model.toAbsolutePath().toString(), "--alias", alias,
                "--api-key-file", apiKeyFile.toAbsolutePath().toString(), "--threads", Integer.toString(threads));
    }

    static Session launch(Path cache, Path executable, Path model, Path log, String alias, int threads, Duration timeout,
                          PortSupplier ports, ProcessStarter starter, IdentityProbe identity) throws Exception {
        Objects.requireNonNull(timeout, "timeout");
        if (timeout.isNegative() || timeout.isZero()) {
            throw new IllegalArgumentException("Launch timeout must be positive.");
        }
        Path verifiedExecutable = ManagedLocalAiCache.verifyOwnedFile(cache, executable);
        Path verifiedModel = ManagedLocalAiCache.verifyOwnedFile(cache, model);
        requireContainedLog(cache, log);
        Files.createDirectories(log.toAbsolutePath().normalize().getParent());
        requireContainedLog(cache, log);
        reserveLog(log);
        Exception lastFailure = null;
        for (int attempt = 0; attempt < MAX_LAUNCH_ATTEMPTS; attempt++) {
            String key = secret();
            Path keyFile = createKeyFile(cache, key);
            Process process = null;
            try {
                int port = ports.next();
                process = starter.start(command(verifiedExecutable, verifiedModel, port, alias, keyFile, threads),
                        runtimeEnvironment(System.getenv()), log);
                identity.await(process, port, key, alias, timeout);
                if (!process.isAlive()) {
                    throw new IllegalStateException("Managed local AI process exited during identity verification.");
                }
                deleteKeyFile(keyFile, null);
                return new Session(process, port, alias, key, timeout);
            } catch (InterruptedException cancelled) {
                Thread.currentThread().interrupt();
                if (process != null) {
                    terminate(process, Duration.ofSeconds(2), cancelled);
                }
                deleteKeyFile(keyFile, cancelled);
                throw cancelled;
            } catch (Exception failure) {
                lastFailure = failure;
                if (process != null) {
                    terminate(process, Duration.ofSeconds(2), failure);
                }
                deleteKeyFile(keyFile, failure);
            }
        }
        throw new IllegalStateException("Managed local AI process could not establish its authenticated identity.",
                lastFailure);
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
                        URI.create("http://127.0.0.1:" + port + "/v1/models"), "Bearer " + apiKey);
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
        if (process == null) {
            return;
        }
        List<ProcessHandle> discovered = List.of();
        try {
            discovered = process.toHandle().descendants().toList();
            discovered.forEach(ProcessHandle::destroy);
        } catch (Exception cleanup) {
            if (primary != null) {
                primary.addSuppressed(cleanup);
            }
        }
        List<ProcessHandle> descendants = discovered;
        RuntimeException survivorFailure = null;
        try {
            process.destroy();
            boolean parentExited = process.waitFor(Math.max(0, timeout.toMillis()), TimeUnit.MILLISECONDS);
            descendants.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
            long deadline = System.nanoTime() + timeout.toNanos();
            while (descendants.stream().anyMatch(ProcessHandle::isAlive) && System.nanoTime() < deadline) {
                Thread.sleep(10);
            }
            if (descendants.stream().anyMatch(ProcessHandle::isAlive)) {
                survivorFailure = new IllegalStateException("Managed local AI descendant did not terminate.");
            }
            if (!parentExited) {
                process.destroyForcibly();
                boolean forcedExit = process.waitFor(Math.max(1, timeout.toMillis()), TimeUnit.MILLISECONDS);
                if (!forcedExit || process.isAlive()) {
                    survivorFailure = new IllegalStateException("Managed local AI process did not terminate.");
                }
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
        if (survivorFailure != null) {
            if (primary != null) {
                primary.addSuppressed(survivorFailure);
            } else {
                throw survivorFailure;
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

    static int availablePort() throws IOException {
        try (ServerSocket socket = new ServerSocket(0, 1, java.net.InetAddress.getLoopbackAddress())) {
            return socket.getLocalPort();
        }
    }

    static Process start(List<String> command, Map<String, String> environment, Path log) throws IOException {
        ProcessBuilder builder = new ProcessBuilder(command);
        builder.environment().clear();
        builder.environment().putAll(environment);
        builder.redirectErrorStream(true);
        Process process = builder.start();
        Thread.ofVirtual().name("shaft-local-ai-log").start(() -> {
            try {
                writeSanitizedLog(log.getParent().getParent().getParent(), process.getInputStream(), log,
                        Set.of(), 1024 * 1024);
            } catch (IOException ignored) {
                // Lifecycle status owns launch failure; logging must never mask it.
            }
        });
        return process;
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

    static Map<String, Object> requestIdentity(URI uri, String bearer) throws Exception {
        HttpRequest request = HttpRequest.newBuilder(uri).timeout(Duration.ofSeconds(2))
                .header("Authorization", bearer).GET().build();
        HttpResponse<InputStream> response = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(2)).build()
                .send(request, HttpResponse.BodyHandlers.ofInputStream());
        byte[] body = boundedRead(response.body(), 64 * 1024, Duration.ofSeconds(2));
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
                throw new IOException("Managed local AI identity response body timed out.", timeoutFailure);
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

    record Session(Process process, int port, String alias, String apiKey, Duration shutdownTimeout)
            implements AutoCloseable {
        @Override
        public void close() {
            terminate(process, shutdownTimeout, null);
        }
    }

    @FunctionalInterface interface PortSupplier { int next() throws Exception; }
    @FunctionalInterface interface ProcessStarter {
        Process start(List<String> command, Map<String, String> environment, Path log) throws Exception;
    }
    @FunctionalInterface interface IdentityProbe {
        void await(Process process, int port, String apiKey, String alias, Duration timeout) throws Exception;
    }
    @FunctionalInterface interface JsonRequester {
        Map<String, Object> get(URI uri, String bearer) throws Exception;
    }
}
