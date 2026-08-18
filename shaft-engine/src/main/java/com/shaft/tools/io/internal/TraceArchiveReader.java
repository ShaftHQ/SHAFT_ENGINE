package com.shaft.tools.io.internal;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Objects;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Bounded reader for SHAFT-produced trace archives. Rejects path traversal, ZIP64,
 * duplicate names, and decompressed bombs before any entry is published.
 */
public final class TraceArchiveReader {
    static final int MAX_ENTRIES = 4_096;
    static final long MAX_UNCOMPRESSED_BYTES = 256L * 1024 * 1024;
    private static final int COPY_BUFFER_SIZE = 16 * 1024;

    private TraceArchiveReader() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Extracts one named file from {@code archive} into {@code target} after validating every
     * central-directory entry against SHAFT's portable-path and decompressed-size rules.
     *
     * @return the number of bytes written
     */
    public static long extractNamed(Path archive, String name, Path target, long maxEntryBytes) throws IOException {
        Objects.requireNonNull(archive, "archive");
        Objects.requireNonNull(name, "name");
        Objects.requireNonNull(target, "target");
        if (maxEntryBytes < 1) {
            throw new IllegalArgumentException("maxEntryBytes must be positive");
        }
        validateEntryName(name);
        Path absoluteTarget = target.toAbsolutePath();
        Path parent = absoluteTarget.getParent();
        if (parent == null) {
            throw new IOException("Trace extract target must have a parent directory.");
        }
        Files.createDirectories(parent);
        Path temporary = parent.resolve(".shaft-trace-viewer-" + java.util.UUID.randomUUID() + ".tmp");
        boolean found = false;
        long written = 0;
        try {
            try (ZipFile zip = new ZipFile(archive.toFile())) {
                validateArchive(zip);
                ZipEntry entry = zip.getEntry(name);
                if (entry == null || entry.isDirectory()) {
                    throw new IOException("Trace archive does not contain " + name + ".");
                }
                found = true;
                written = copyBounded(zip.getInputStream(entry), temporary, maxEntryBytes);
            }
            try {
                Files.move(temporary, absoluteTarget, StandardCopyOption.ATOMIC_MOVE);
            } catch (AtomicMoveNotSupportedException ignored) {
                Files.move(temporary, absoluteTarget);
            }
            return written;
        } catch (IOException | RuntimeException exception) {
            try {
                Files.deleteIfExists(temporary);
                if (found) {
                    Files.deleteIfExists(absoluteTarget);
                }
            } catch (IOException cleanupFailure) {
                exception.addSuppressed(cleanupFailure);
            }
            throw exception instanceof IOException ioException ? ioException : new IOException(exception);
        }
    }

    static void validateArchive(ZipFile zip) throws IOException {
        java.util.LinkedHashSet<String> names = new java.util.LinkedHashSet<>();
        int count = 0;
        long total = 0;
        var entries = zip.entries();
        while (entries.hasMoreElements()) {
            ZipEntry entry = entries.nextElement();
            count = Math.addExact(count, 1);
            if (count > MAX_ENTRIES) {
                throw new IOException("Trace archive contains too many entries.");
            }
            String identity = validateEntryName(entry.getName());
            if (!names.add(identity)) {
                throw new IOException("Trace archive contains a duplicate entry: " + entry.getName());
            }
            long size = entry.getSize();
            if (size < 0) {
                throw new IOException("ZIP64 trace archives are not supported.");
            }
            if (size > MAX_UNCOMPRESSED_BYTES - total) {
                throw new IOException("Trace archive exceeds the uncompressed-size limit.");
            }
            total += size;
        }
    }

    static String validateEntryName(String name) throws IOException {
        if (name == null || name.isBlank() || name.startsWith("/") || name.startsWith("\\")
                || name.contains("\\") || name.contains(":") || name.contains("//")) {
            throw new IOException("Trace archive contains an unsafe entry name.");
        }
        String candidate = name.endsWith("/") ? name.substring(0, name.length() - 1) : name;
        if (candidate.isBlank()) {
            throw new IOException("Trace archive contains an unsafe entry name.");
        }
        for (String segment : candidate.split("/", -1)) {
            if (segment.isEmpty() || ".".equals(segment) || "..".equals(segment)) {
                throw new IOException("Trace archive contains an unsafe entry name.");
            }
        }
        return candidate;
    }

    private static long copyBounded(InputStream input, Path target, long maxEntryBytes) throws IOException {
        try (input; var output = Files.newOutputStream(target)) {
            byte[] buffer = new byte[COPY_BUFFER_SIZE];
            long written = 0;
            int count;
            while ((count = input.read(buffer)) != -1) {
                written = Math.addExact(written, count);
                if (written > maxEntryBytes) {
                    throw new IOException("Trace archive entry exceeds the " + maxEntryBytes + " byte limit.");
                }
                output.write(buffer, 0, count);
            }
            return written;
        }
    }
}
