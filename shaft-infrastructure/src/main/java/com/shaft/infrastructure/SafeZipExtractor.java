package com.shaft.infrastructure;

import org.apache.commons.compress.archivers.zip.UnixStat;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipFile;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

/** Bounded ZIP extraction for verified setup archives. */
final class SafeZipExtractor {
    private static final int MAX_ENTRIES = 100_000;
    private static final long MAX_ENTRY_BYTES = 512L * 1024 * 1024;
    private static final long MAX_TOTAL_BYTES = 2L * 1024 * 1024 * 1024;
    private static final long MAX_RATIO = 200;

    private SafeZipExtractor() { }

    static void extract(Path archive, Path destination) throws IOException {
        Path root = destination.toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(root);
        Files.createDirectories(root);
        ExtractionState state = new ExtractionState();
        try (ZipFile input = new ZipFile(archive.toFile())) {
            var entriesInOrder = input.getEntriesInPhysicalOrder();
            while (entriesInOrder.hasMoreElements()) {
                extractEntry(input, entriesInOrder.nextElement(), root, state);
            }
        }
    }

    private static void extractEntry(ZipFile input, ZipArchiveEntry entry, Path root,
                                     ExtractionState state) throws IOException {
        state.countEntry();
        String name = validatedName(entry.getName());
        state.addName(name);
        requireOrdinaryEntry(entry, name);
        requireReadable(input, entry, name);
        requireSafeDeclaredSize(entry, name);
        Path target = resolvedTarget(root, name);
        if (entry.isDirectory()) {
            Files.createDirectories(target);
        } else {
            writeEntry(input, entry, target, state);
        }
    }

    private static void requireReadable(ZipFile input, ZipArchiveEntry entry, String name) throws IOException {
        if (!input.canReadEntryData(entry)) {
            throw new IOException("ZIP entry uses an unsupported encoding or feature: " + name);
        }
    }

    private static void requireSafeDeclaredSize(ZipArchiveEntry entry, String name) throws IOException {
        long declared = entry.getSize();
        long compressed = entry.getCompressedSize();
        boolean excessiveRatio = declared >= 0 && compressed > 0
                && declared / Math.max(1, compressed) > MAX_RATIO;
        if (declared > MAX_ENTRY_BYTES || excessiveRatio) {
            throw new IOException("ZIP entry exceeds a safety bound: " + name);
        }
    }

    private static Path resolvedTarget(Path root, String name) throws IOException {
        Path target = root.resolve(name).normalize();
        if (!target.startsWith(root)) throw new IOException("ZIP entry escapes its target: " + name);
        return target;
    }

    private static void writeEntry(ZipFile input, ZipArchiveEntry entry, Path target,
                                   ExtractionState state) throws IOException {
        Files.createDirectories(target.getParent());
        long written = 0;
        byte[] buffer = new byte[64 * 1024];
        try (var entryInput = input.getInputStream(entry);
             var output = Files.newOutputStream(target, java.nio.file.StandardOpenOption.CREATE_NEW)) {
            for (int read; (read = entryInput.read(buffer)) >= 0;) {
                written += read;
                state.addBytes(read);
                requireExpandedBounds(written, state.totalBytes);
                output.write(buffer, 0, read);
            }
        }
    }

    private static void requireExpandedBounds(long entryBytes, long totalBytes) throws IOException {
        if (entryBytes > MAX_ENTRY_BYTES || totalBytes > MAX_TOTAL_BYTES) {
            throw new IOException("ZIP expanded data exceeds a safety bound.");
        }
    }

    private static void requireOrdinaryEntry(ZipArchiveEntry entry, String name) throws IOException {
        if (entry.isUnixSymlink()) throw new IOException("ZIP contains a symbolic link: " + name);
        int mode = entry.getUnixMode();
        if (mode == 0) return;
        int type = mode & 0170000;
        if (type != UnixStat.FILE_FLAG && type != UnixStat.DIR_FLAG) {
            throw new IOException("ZIP contains a link, device, or other special entry: " + name);
        }
    }

    private static String validatedName(String raw) throws IOException {
        requireNonBlankName(raw);
        String name = raw.replace('\\', '/');
        requireRelativeName(raw, name);
        for (String part : name.split("/")) requireSafePart(raw, part);
        return name;
    }

    private static void requireNonBlankName(String raw) throws IOException {
        if (raw == null || raw.isBlank() || raw.indexOf('\0') >= 0) {
            throw new IOException("ZIP contains a blank or NUL path.");
        }
    }

    private static void requireRelativeName(String raw, String name) throws IOException {
        if (name.startsWith("/") || name.startsWith("//") || name.matches("^[a-zA-Z]:.*")
                || name.contains(":")) {
            throw new IOException("ZIP contains an absolute, drive, UNC, or ADS path: " + raw);
        }
    }

    private static void requireSafePart(String raw, String part) throws IOException {
        if (part.equals("..") || part.equals(".")) throw new IOException("ZIP contains traversal: " + raw);
        int dot = part.indexOf('.');
        String base = dot < 0 ? part : part.substring(0, dot);
        if (base.matches("(?i)con|prn|aux|nul|com[1-9]|lpt[1-9]")) {
            throw new IOException("ZIP contains a reserved Windows path: " + raw);
        }
    }

    private static final class ExtractionState {
        private final Set<String> names = new HashSet<>();
        private int entries;
        private long totalBytes;

        private void countEntry() throws IOException {
            if (++entries > MAX_ENTRIES) throw new IOException("ZIP entry count exceeds the safety limit.");
        }

        private void addName(String name) throws IOException {
            if (!names.add(name.toLowerCase(Locale.ROOT))) {
                throw new IOException("ZIP contains a duplicate path: " + name);
            }
        }

        private void addBytes(long bytes) {
            totalBytes += bytes;
        }
    }
}
