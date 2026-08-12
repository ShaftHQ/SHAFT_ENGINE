package com.shaft.ocr.internal;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

final class TessdataModelManager {
    private static final Duration DOWNLOAD_TIMEOUT = Duration.ofSeconds(60);
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();

    private final Path cacheDirectory;
    private final URI baseUri;
    private final boolean downloadsEnabled;
    private final Map<String, String> checksums;
    private final IntegrityAlgorithm integrityAlgorithm;
    private final HttpClient httpClient;

    TessdataModelManager(Path cacheDirectory, URI baseUri, boolean downloadsEnabled, Map<String, String> checksums) {
        this(cacheDirectory, baseUri, downloadsEnabled, checksums, IntegrityAlgorithm.SHA256);
    }

    TessdataModelManager(Path cacheDirectory, URI baseUri, boolean downloadsEnabled, Map<String, String> checksums,
                         IntegrityAlgorithm integrityAlgorithm) {
        this.cacheDirectory = Objects.requireNonNull(cacheDirectory, "cacheDirectory").toAbsolutePath().normalize();
        this.baseUri = Objects.requireNonNull(baseUri, "baseUri");
        this.downloadsEnabled = downloadsEnabled;
        this.checksums = Map.copyOf(Objects.requireNonNull(checksums, "checksums"));
        this.integrityAlgorithm = Objects.requireNonNull(integrityAlgorithm, "integrityAlgorithm");
        this.httpClient = HttpClient.newBuilder().connectTimeout(DOWNLOAD_TIMEOUT).build();
    }

    Path ensureAvailable(List<String> languageCodes) {
        try {
            Files.createDirectories(cacheDirectory);
        } catch (IOException exception) {
            throw new IllegalStateException("Could not create SHAFT OCR model cache: " + cacheDirectory, exception);
        }
        for (String languageCode : languageCodes) {
            ensureOne(languageCode);
        }
        return cacheDirectory;
    }

    private void ensureOne(String languageCode) {
        String expectedChecksum = checksums.get(languageCode);
        if (expectedChecksum == null) {
            throw new IllegalArgumentException("No integrity metadata is available for OCR language '" + languageCode + "'.");
        }
        Path model = cacheDirectory.resolve(languageCode + ".traineddata");
        if (Files.exists(model)) {
            requireChecksum(model, expectedChecksum, "Cached OCR model failed integrity verification", integrityAlgorithm);
            return;
        }
        if (!downloadsEnabled) {
            throw new IllegalStateException("OCR language '" + languageCode + "' is not cached at " + cacheDirectory
                    + " and first-use downloads are disabled.");
        }

        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(model, ignored -> new ReentrantLock());
        jvmLock.lock();
        try {
            withProcessLock(languageCode, model, expectedChecksum);
        } finally {
            jvmLock.unlock();
            if (!jvmLock.hasQueuedThreads()) {
                JVM_LOCKS.remove(model, jvmLock);
            }
        }
    }

    private void withProcessLock(String languageCode, Path model, String expectedChecksum) {
        Path lockPath = cacheDirectory.resolve(languageCode + ".lock");
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            if (Files.exists(model)) {
                requireChecksum(model, expectedChecksum, "Cached OCR model failed integrity verification", integrityAlgorithm);
                return;
            }
            download(languageCode, model, expectedChecksum);
        } catch (IOException exception) {
            throw new IllegalStateException("Could not lock SHAFT OCR model cache for '" + languageCode
                    + "' at " + cacheDirectory, exception);
        }
    }

    private void download(String languageCode, Path model, String expectedChecksum) {
        URI source = baseUri.resolve(languageCode + ".traineddata");
        HttpRequest request = HttpRequest.newBuilder(source).timeout(DOWNLOAD_TIMEOUT).GET().build();
        Path temporary = cacheDirectory.resolve(languageCode + "." + UUID.randomUUID() + ".tmp");
        try {
            HttpResponse<byte[]> response = httpClient.send(request, HttpResponse.BodyHandlers.ofByteArray());
            if (response.statusCode() != 200) {
                throw new IllegalStateException("Could not download OCR language '" + languageCode + "' from "
                        + source + ": HTTP " + response.statusCode());
            }
            Files.write(temporary, response.body(), StandardOpenOption.CREATE_NEW);
            requireChecksum(temporary, expectedChecksum, "Downloaded OCR model failed integrity verification", integrityAlgorithm);
            try {
                Files.move(temporary, model, StandardCopyOption.ATOMIC_MOVE);
            } catch (java.nio.file.AtomicMoveNotSupportedException exception) {
                Files.move(temporary, model);
            }
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Interrupted while downloading OCR language '" + languageCode + "'.", exception);
        } catch (IOException exception) {
            throw new IllegalStateException("Could not provision OCR language '" + languageCode + "' into "
                    + cacheDirectory, exception);
        } finally {
            try {
                Files.deleteIfExists(temporary);
            } catch (IOException ignored) {
                // A failed temporary cleanup does not replace or invalidate a known-good model.
            }
        }
    }

    private static void requireChecksum(Path file, String expectedChecksum, String message,
                                        IntegrityAlgorithm integrityAlgorithm) {
        String actualChecksum;
        try {
            actualChecksum = integrityAlgorithm.digest(Files.readAllBytes(file));
        } catch (IOException exception) {
            throw new IllegalStateException("Could not read OCR model for integrity verification: " + file, exception);
        }
        if (!actualChecksum.equalsIgnoreCase(expectedChecksum)) {
            throw new IllegalStateException(message + ": " + file + " (expected " + expectedChecksum
                    + ", actual " + actualChecksum + ").");
        }
    }

    enum IntegrityAlgorithm {
        SHA256 {
            @Override
            String digest(byte[] bytes) {
                return hash("SHA-256", bytes);
            }
        },
        GIT_BLOB_SHA1 {
            @Override
            String digest(byte[] bytes) {
                byte[] header = ("blob " + bytes.length + "\0").getBytes(java.nio.charset.StandardCharsets.UTF_8);
                byte[] input = new byte[header.length + bytes.length];
                System.arraycopy(header, 0, input, 0, header.length);
                System.arraycopy(bytes, 0, input, header.length, bytes.length);
                return hash("SHA-1", input);
            }
        };

        abstract String digest(byte[] bytes);

        private static String hash(String algorithm, byte[] bytes) {
            try {
                return HexFormat.of().formatHex(MessageDigest.getInstance(algorithm).digest(bytes));
            } catch (NoSuchAlgorithmException exception) {
                throw new IllegalStateException(algorithm + " is unavailable.", exception);
            }
        }
    }
}
