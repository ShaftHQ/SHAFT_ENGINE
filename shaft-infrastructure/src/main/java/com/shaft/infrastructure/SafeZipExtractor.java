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
        Set<String> names = new HashSet<>();
        int entries = 0;
        long total = 0;
        try (ZipFile input = new ZipFile(archive.toFile())) {
            var entriesInOrder = input.getEntriesInPhysicalOrder();
            while (entriesInOrder.hasMoreElements()) {
                ZipArchiveEntry entry = entriesInOrder.nextElement();
                if (++entries > MAX_ENTRIES) throw new IOException("ZIP entry count exceeds the safety limit.");
                String name = validatedName(entry.getName());
                String folded = name.toLowerCase(Locale.ROOT);
                if (!names.add(folded)) throw new IOException("ZIP contains a duplicate path: " + name);
                requireOrdinaryEntry(entry, name);
                if (!input.canReadEntryData(entry)) {
                    throw new IOException("ZIP entry uses an unsupported encoding or feature: " + name);
                }
                long declared = entry.getSize();
                long compressed = entry.getCompressedSize();
                if (declared > MAX_ENTRY_BYTES || declared >= 0 && compressed > 0
                        && declared / Math.max(1, compressed) > MAX_RATIO) {
                    throw new IOException("ZIP entry exceeds a safety bound: " + name);
                }
                Path target = root.resolve(name).normalize();
                if (!target.startsWith(root)) throw new IOException("ZIP entry escapes its target: " + name);
                if (entry.isDirectory()) {
                    Files.createDirectories(target);
                    continue;
                }
                Files.createDirectories(target.getParent());
                long written = 0;
                byte[] buffer = new byte[64 * 1024];
                try (var entryInput = input.getInputStream(entry);
                     var output = Files.newOutputStream(target, java.nio.file.StandardOpenOption.CREATE_NEW)) {
                    for (int read; (read = entryInput.read(buffer)) >= 0;) {
                        written += read;
                        total += read;
                        if (written > MAX_ENTRY_BYTES || total > MAX_TOTAL_BYTES) {
                            throw new IOException("ZIP expanded data exceeds a safety bound.");
                        }
                        output.write(buffer, 0, read);
                    }
                }
            }
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
        if (raw == null || raw.isBlank() || raw.indexOf('\0') >= 0) {
            throw new IOException("ZIP contains a blank or NUL path.");
        }
        String name = raw.replace('\\', '/');
        if (name.startsWith("/") || name.startsWith("//") || name.matches("^[a-zA-Z]:.*")
                || name.contains(":")) {
            throw new IOException("ZIP contains an absolute, drive, UNC, or ADS path: " + raw);
        }
        for (String part : name.split("/")) {
            if (part.equals("..") || part.equals(".")) throw new IOException("ZIP contains traversal: " + raw);
            String base = part.contains(".") ? part.substring(0, part.indexOf('.')) : part;
            if (base.matches("(?i)con|prn|aux|nul|com[1-9]|lpt[1-9]")) {
                throw new IOException("ZIP contains a reserved Windows path: " + raw);
            }
        }
        return name;
    }
}
