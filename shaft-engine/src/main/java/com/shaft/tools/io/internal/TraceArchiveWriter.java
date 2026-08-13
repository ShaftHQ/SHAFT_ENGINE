package com.shaft.tools.io.internal;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.CopyOption;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Writes a bounded SHAFT trace archive without buffering the complete ZIP in memory.
 */
public final class TraceArchiveWriter {
    private static final int COPY_BUFFER_SIZE = 16 * 1024;
    private static final MoveStrategy DEFAULT_MOVES = Files::move;
    private static final CopyStrategy DEFAULT_COPIES = Files::copy;

    private TraceArchiveWriter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Streams all entries to a sibling temporary archive and publishes it only after the ZIP has
     * closed successfully. An existing target is therefore preserved if any entry cannot be read.
     */
    static void write(Path target, List<Entry> entries, long maxEntryBytes, String omissionMarker) throws IOException {
        write(target, entries, maxEntryBytes, Long.MAX_VALUE, omissionMarker);
    }

    static WriteResult write(Path target, List<Entry> entries, long maxEntryBytes, long maxTotalBytes,
                             String omissionMarker) throws IOException {
        return write(target, entries, maxEntryBytes, maxTotalBytes, omissionMarker, DEFAULT_MOVES, DEFAULT_COPIES);
    }

    static void write(Path target, List<Entry> entries, long maxEntryBytes, String omissionMarker,
                      MoveStrategy moves) throws IOException {
        write(target, entries, maxEntryBytes, Long.MAX_VALUE, omissionMarker, moves, DEFAULT_COPIES);
    }

    static void write(Path target, List<Entry> entries, long maxEntryBytes, String omissionMarker,
                      MoveStrategy moves, CopyStrategy copies) throws IOException {
        write(target, entries, maxEntryBytes, Long.MAX_VALUE, omissionMarker, moves, copies);
    }

    private static WriteResult write(Path target, List<Entry> entries, long maxEntryBytes, long maxTotalBytes,
                                     String omissionMarker, MoveStrategy moves, CopyStrategy copies) throws IOException {
        Objects.requireNonNull(target, "target");
        Objects.requireNonNull(entries, "entries");
        Objects.requireNonNull(moves, "moves");
        Objects.requireNonNull(copies, "copies");
        if (maxEntryBytes < 1) {
            throw new IllegalArgumentException("maxEntryBytes must be positive");
        }
        if (maxTotalBytes < 1) {
            throw new IllegalArgumentException("maxTotalBytes must be positive");
        }
        Path absoluteTarget = target.toAbsolutePath();
        Path parent = absoluteTarget.getParent();
        if (parent == null) {
            throw new IOException("Trace archive target must have a parent directory.");
        }
        Files.createDirectories(parent);
        Path temporary = parent.resolve(absoluteTarget.getFileName() + ".tmp-" + UUID.randomUUID());
        byte[] omitted = String.valueOf(omissionMarker).getBytes(StandardCharsets.UTF_8);
        if (entries.size() > maxTotalBytes / Math.max(1, omitted.length)) {
            throw new IllegalArgumentException("maxTotalBytes must fit one omission marker per trace entry");
        }
        long requiredBytes = 0;
        int optionalEntries = 0;
        for (Entry entry : entries) {
            if (entry.required()) {
                long size = entry.size();
                if (size > maxEntryBytes) {
                    throw new IOException("Required trace entry exceeded its configured bound: " + entry.name());
                }
                requiredBytes = Math.addExact(requiredBytes, size);
            } else {
                optionalEntries++;
            }
        }
        long minimumBytes = Math.addExact(requiredBytes,
                Math.multiplyExact((long) optionalEntries, omitted.length));
        if (minimumBytes > maxTotalBytes) {
            throw new IOException("Required trace entries exceed the aggregate archive budget.");
        }
        LinkedHashSet<String> omittedPaths = new LinkedHashSet<>();
        long[] futureRequiredBytes = new long[entries.size() + 1];
        int[] futureOptionalEntries = new int[entries.size() + 1];
        for (int index = entries.size() - 1; index >= 0; index--) {
            Entry entry = entries.get(index);
            futureRequiredBytes[index] = futureRequiredBytes[index + 1];
            futureOptionalEntries[index] = futureOptionalEntries[index + 1];
            if (entry.required()) {
                futureRequiredBytes[index] = Math.addExact(futureRequiredBytes[index], entry.size());
            } else {
                futureOptionalEntries[index]++;
            }
        }
        try {
            try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(temporary,
                    StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE))) {
                long remaining = maxTotalBytes;
                for (int index = 0; index < entries.size(); index++) {
                    Entry entry = entries.get(index);
                    long futureRequired = futureRequiredBytes[index + 1];
                    long reservedMarkers = Math.multiplyExact(
                            (long) futureOptionalEntries[index + 1], omitted.length);
                    long entryBudget = entry.required() ? maxEntryBytes
                            : Math.min(maxEntryBytes, remaining - futureRequired - reservedMarkers);
                    EntryWrite written = addEntry(zip, entry, entryBudget, omitted);
                    remaining -= written.bytes();
                    if (written.omitted()) {
                        omittedPaths.add(entry.name());
                    }
                }
            }
            publish(temporary, absoluteTarget, moves, copies);
            return new WriteResult(List.copyOf(omittedPaths));
        } catch (IOException | RuntimeException exception) {
            cleanup(temporary, exception);
            throw exception;
        }
    }

    /** Copies a completed file through the same recoverable publication protocol. */
    public static void copy(Path source, Path target) throws IOException {
        Objects.requireNonNull(source, "source");
        Objects.requireNonNull(target, "target");
        Path absoluteTarget = target.toAbsolutePath();
        Path parent = absoluteTarget.getParent();
        if (parent == null) {
            throw new IOException("Trace archive target must have a parent directory.");
        }
        Files.createDirectories(parent);
        Path temporary = temporarySibling(absoluteTarget);
        try {
            Files.copy(source, temporary);
            publish(temporary, absoluteTarget, DEFAULT_MOVES, DEFAULT_COPIES);
        } catch (IOException | RuntimeException exception) {
            cleanup(temporary, exception);
            throw exception;
        }
    }

    /** Publishes in-memory bytes through the same recoverable exact-target protocol. */
    public static void writeBytes(Path target, byte[] content) throws IOException {
        Objects.requireNonNull(target, "target");
        Objects.requireNonNull(content, "content");
        Path absoluteTarget = target.toAbsolutePath();
        Path parent = absoluteTarget.getParent();
        if (parent == null) {
            throw new IOException("Target must have a parent directory.");
        }
        Files.createDirectories(parent);
        Path temporary = temporarySibling(absoluteTarget);
        try {
            Files.write(temporary, content, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
            publish(temporary, absoluteTarget, DEFAULT_MOVES, DEFAULT_COPIES);
        } catch (IOException | RuntimeException exception) {
            cleanup(temporary, exception);
            throw exception;
        }
    }

    private static EntryWrite addEntry(ZipOutputStream zip, Entry entry, long maxEntryBytes, byte[] omissionMarker)
            throws IOException {
        Objects.requireNonNull(entry, "entry");
        if (entry.optional()) {
            return addOptionalEntry(zip, entry, maxEntryBytes, omissionMarker);
        }
        return writeEntry(zip, entry, maxEntryBytes, omissionMarker);
    }

    private static EntryWrite addOptionalEntry(ZipOutputStream zip, Entry entry, long maxEntryBytes,
                                         byte[] omissionMarker) throws IOException {
        Path stable = null;
        try {
            if (entry.source() instanceof OmittedSource omittedSource) {
                byte[] marker = omittedSource.marker();
                if (marker.length > maxEntryBytes) {
                    marker = omissionMarker;
                }
                writeMarkerEntry(zip, entry.name(), marker);
                return new EntryWrite(marker.length, true);
            }
            if (entry.size() > maxEntryBytes) {
                writeMarkerEntry(zip, entry.name(), omissionMarker);
                return new EntryWrite(omissionMarker.length, true);
            }
            stable = Files.createTempFile("shaft-trace-optional-", ".tmp");
            try (InputStream input = entry.open(); var output = Files.newOutputStream(stable)) {
                byte[] buffer = new byte[COPY_BUFFER_SIZE];
                long written = 0;
                int count;
                while ((count = input.read(buffer)) != -1) {
                    written += count;
                    if (written > maxEntryBytes) {
                        throw new IOException("Optional trace entry exceeded its configured bound: " + entry.name());
                    }
                    output.write(buffer, 0, count);
                }
            }
            return writeEntry(zip, Entry.file(entry.name(), stable), maxEntryBytes, omissionMarker);
        } catch (IOException ignored) {
            writeMarkerEntry(zip, entry.name(), omissionMarker);
            return new EntryWrite(omissionMarker.length, true);
        } finally {
            if (stable != null) {
                try {
                    Files.deleteIfExists(stable);
                } catch (IOException ignored) {
                    stable.toFile().deleteOnExit();
                }
            }
        }
    }

    private static void writeMarkerEntry(ZipOutputStream zip, String name, byte[] omissionMarker) throws IOException {
        zip.putNextEntry(new ZipEntry(name));
        try {
            zip.write(omissionMarker);
        } finally {
            zip.closeEntry();
        }
    }

    private static EntryWrite writeEntry(ZipOutputStream zip, Entry entry, long maxEntryBytes, byte[] omissionMarker)
            throws IOException {
        zip.putNextEntry(new ZipEntry(entry.name()));
        try {
            if (entry.size() > maxEntryBytes) {
                zip.write(omissionMarker);
                return new EntryWrite(omissionMarker.length, true);
            }
            try (InputStream input = entry.open()) {
                byte[] buffer = new byte[COPY_BUFFER_SIZE];
                long written = 0;
                int count;
                while ((count = input.read(buffer)) != -1) {
                    written += count;
                    if (written > maxEntryBytes) {
                        throw new IOException("Trace entry changed size while being written: " + entry.name());
                    }
                    zip.write(buffer, 0, count);
                }
                return new EntryWrite(written, false);
            }
        } finally {
            zip.closeEntry();
        }
    }

    private static void publish(Path temporary, Path target, MoveStrategy moves, CopyStrategy copies) throws IOException {
        try {
            moves.move(temporary, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        } catch (AtomicMoveNotSupportedException ignored) {
            recoverableReplace(temporary, target, moves, copies);
        }
    }

    private static void recoverableReplace(Path temporary, Path target, MoveStrategy moves, CopyStrategy copies)
            throws IOException {
        if (Files.isSymbolicLink(target)) {
            recoverableReplaceSymbolicLink(temporary, target, moves);
            return;
        }
        if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
            try {
                moves.move(temporary, target, StandardCopyOption.REPLACE_EXISTING);
            } catch (IOException | RuntimeException publicationFailure) {
                try {
                    Files.deleteIfExists(target);
                } catch (IOException cleanupFailure) {
                    publicationFailure.addSuppressed(cleanupFailure);
                }
                throw publicationFailure;
            }
            return;
        }
        Path backup = target.resolveSibling(target.getFileName() + ".backup-" + UUID.randomUUID());
        try {
            copies.copy(target, backup, LinkOption.NOFOLLOW_LINKS);
        } catch (IOException backupFailure) {
            cleanup(backup, backupFailure);
            throw backupFailure;
        }
        try {
            moves.move(temporary, target, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException | RuntimeException publicationFailure) {
            try {
                copies.copy(backup, target, StandardCopyOption.REPLACE_EXISTING, LinkOption.NOFOLLOW_LINKS);
            } catch (IOException | RuntimeException restorationFailure) {
                publicationFailure.addSuppressed(restorationFailure);
                publicationFailure.addSuppressed(new IOException("Known-good trace backup retained at " + backup));
                throw publicationFailure;
            }
            try {
                Files.deleteIfExists(backup);
            } catch (IOException cleanupFailure) {
                publicationFailure.addSuppressed(cleanupFailure);
                backup.toFile().deleteOnExit();
            }
            throw publicationFailure;
        }
        try {
            Files.deleteIfExists(backup);
        } catch (IOException ignored) {
            backup.toFile().deleteOnExit();
        }
    }

    private static void recoverableReplaceSymbolicLink(Path temporary, Path target, MoveStrategy moves)
            throws IOException {
        Path backup = target.resolveSibling(target.getFileName() + ".backup-" + UUID.randomUUID());
        try {
            moves.move(target, backup, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException | RuntimeException backupFailure) {
            if (Files.exists(backup, LinkOption.NOFOLLOW_LINKS)) {
                backupFailure.addSuppressed(new IOException("Known-good symbolic-link backup retained at " + backup));
            }
            throw backupFailure;
        }
        try {
            moves.move(temporary, target, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException | RuntimeException publicationFailure) {
            try {
                Files.deleteIfExists(target);
                Files.createSymbolicLink(target, Files.readSymbolicLink(backup));
            } catch (IOException | RuntimeException restorationFailure) {
                publicationFailure.addSuppressed(restorationFailure);
                publicationFailure.addSuppressed(new IOException(
                        "Known-good symbolic-link backup retained at " + backup));
                throw publicationFailure;
            }
            try {
                Files.deleteIfExists(backup);
            } catch (IOException cleanupFailure) {
                publicationFailure.addSuppressed(cleanupFailure);
                backup.toFile().deleteOnExit();
            }
            throw publicationFailure;
        }
        try {
            Files.deleteIfExists(backup);
        } catch (IOException ignored) {
            backup.toFile().deleteOnExit();
        }
    }

    private static Path temporarySibling(Path target) {
        return target.resolveSibling(target.getFileName() + ".tmp-" + UUID.randomUUID());
    }

    private static void cleanup(Path temporary, Throwable original) {
        try {
            Files.deleteIfExists(temporary);
        } catch (IOException cleanupFailure) {
            original.addSuppressed(cleanupFailure);
        }
    }

    /** A lazily opened archive entry backed by either bounded bytes or a filesystem path. */
    record Entry(String name, byte[] bytes, Path path, Source source, boolean optional, boolean required) {
        Entry {
            if (name == null || name.isBlank() || name.startsWith("/") || name.startsWith("\\")
                    || name.contains("../") || name.contains("..\\")) {
                throw new IllegalArgumentException("Trace archive entry name must be relative and traversal-free.");
            }
            int sourceCount = (bytes == null ? 0 : 1) + (path == null ? 0 : 1) + (source == null ? 0 : 1);
            if (sourceCount != 1) {
                throw new IllegalArgumentException("Trace archive entry needs exactly one content source.");
            }
            bytes = bytes == null ? null : Arrays.copyOf(bytes, bytes.length);
        }

        static Entry text(String name, String text) {
            return bytes(name, String.valueOf(text).getBytes(StandardCharsets.UTF_8));
        }

        static Entry bytes(String name, byte[] bytes) {
            return new Entry(name, Objects.requireNonNull(bytes, "bytes"), null, null, false, false);
        }

        static Entry requiredText(String name, String text) {
            return new Entry(name, String.valueOf(text).getBytes(StandardCharsets.UTF_8), null, null, false, true);
        }

        static Entry required(String name, Source source) {
            return new Entry(name, null, null, Objects.requireNonNull(source, "source"), false, true);
        }

        static Entry optionalBytes(String name, byte[] bytes) {
            return new Entry(name, Objects.requireNonNull(bytes, "bytes"), null, null, true, false);
        }

        static Entry file(String name, Path path) {
            return new Entry(name, null, Objects.requireNonNull(path, "path"), null, false, false);
        }

        static Entry optionalFile(String name, Path path) {
            return new Entry(name, null, Objects.requireNonNull(path, "path"), null, true, false);
        }

        static Entry optional(String name, Source source) {
            return new Entry(name, null, null, Objects.requireNonNull(source, "source"), true, false);
        }

        static Entry omitted(String name) {
            return omitted(name, "");
        }

        static Entry omitted(String name, String reason) {
            return optional(name, new OmittedSource(
                    String.valueOf(reason).getBytes(StandardCharsets.UTF_8)));
        }

        long size() throws IOException {
            return bytes != null ? bytes.length : path != null ? Files.size(path) : source.size();
        }

        InputStream open() throws IOException {
            return bytes != null ? new java.io.ByteArrayInputStream(bytes)
                    : path != null ? Files.newInputStream(path) : source.open();
        }

        @Override
        public byte[] bytes() {
            return bytes == null ? null : Arrays.copyOf(bytes, bytes.length);
        }
    }

    record WriteResult(List<String> omittedPaths) {
        WriteResult {
            omittedPaths = List.copyOf(omittedPaths);
        }

    }

    private record EntryWrite(long bytes, boolean omitted) {
    }

    interface Source {
        long size() throws IOException;

        InputStream open() throws IOException;
    }

    private record OmittedSource(byte[] marker) implements Source {
        private OmittedSource {
            marker = Arrays.copyOf(marker, marker.length);
        }

        @Override
        public byte[] marker() {
            return Arrays.copyOf(marker, marker.length);
        }

        @Override
        public long size() {
            return marker.length;
        }

        @Override
        public InputStream open() {
            return new java.io.ByteArrayInputStream(marker);
        }
    }

    @FunctionalInterface
    interface MoveStrategy {
        void move(Path source, Path target, CopyOption... options) throws IOException;
    }

    @FunctionalInterface
    interface CopyStrategy {
        void copy(Path source, Path target, CopyOption... options) throws IOException;
    }
}
