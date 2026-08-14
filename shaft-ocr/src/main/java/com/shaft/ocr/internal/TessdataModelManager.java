package com.shaft.ocr.internal;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;

final class TessdataModelManager {
    private final Path cacheDirectory;
    private final Path fallbackDirectory;
    private final boolean downloadsEnabled;
    private final Map<String, String> checksums;
    private final IntegrityAlgorithm integrityAlgorithm;

    TessdataModelManager(Path cacheDirectory, URI baseUri, boolean downloadsEnabled, Map<String, String> checksums) {
        this(cacheDirectory, null, baseUri, downloadsEnabled, checksums, IntegrityAlgorithm.SHA256);
    }

    TessdataModelManager(Path cacheDirectory, URI baseUri, boolean downloadsEnabled, Map<String, String> checksums,
                         IntegrityAlgorithm integrityAlgorithm) {
        this(cacheDirectory, null, baseUri, downloadsEnabled, checksums, integrityAlgorithm);
    }

    TessdataModelManager(Path cacheDirectory, Path fallbackDirectory, URI baseUri, boolean downloadsEnabled,
                         Map<String, String> checksums, IntegrityAlgorithm integrityAlgorithm) {
        this.cacheDirectory = Objects.requireNonNull(cacheDirectory, "cacheDirectory").toAbsolutePath().normalize();
        this.fallbackDirectory = fallbackDirectory == null ? null : fallbackDirectory.toAbsolutePath().normalize();
        Objects.requireNonNull(baseUri, "baseUri");
        this.downloadsEnabled = downloadsEnabled;
        this.checksums = Map.copyOf(Objects.requireNonNull(checksums, "checksums"));
        this.integrityAlgorithm = Objects.requireNonNull(integrityAlgorithm, "integrityAlgorithm");
    }

    Path ensureAvailable(List<String> languageCodes) {
        if (languageCodes.stream()
                .allMatch(language -> verified(cacheDirectory.resolve(language + ".traineddata"), language))) {
            return cacheDirectory;
        }
        if (fallbackDirectory != null && languageCodes.stream()
                .allMatch(language -> verified(fallbackDirectory.resolve(language + ".traineddata"), language))) {
            return fallbackDirectory;
        }
        for (String languageCode : languageCodes) {
            ensureOne(languageCode);
        }
        return cacheDirectory;
    }

    private boolean verified(Path model, String languageCode) {
        String checksum = checksums.get(languageCode);
        if (checksum == null || !Files.isRegularFile(model)) return false;
        try {
            return checksum.equalsIgnoreCase(integrityAlgorithm.digest(Files.readAllBytes(model)));
        } catch (IOException exception) {
            return false;
        }
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
        String legacy = downloadsEnabled ? " The legacy shaft.ocr.downloadEnabled flag no longer bypasses setup approval."
                : "";
        throw new IllegalStateException("OCR language '" + languageCode + "' is not cached at " + cacheDirectory
                + ". Run shaft-cli setup plan --profile OCR --language " + languageCode
                + ", review and install the approved plan with the same --language option." + legacy);
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
