package com.shaft.infrastructure;

import org.apache.commons.compress.archivers.zip.UnixStat;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SafeZipExtractorTest {
    @Test
    void extractsOrdinaryFilesInsideTheReviewedDestination(@TempDir Path temp) throws Exception {
        Path archive = archive(temp.resolve("safe.zip"), new Entry("cmdline-tools/bin/sdkmanager", "tool", 0));
        Path destination = temp.resolve("destination");

        SafeZipExtractor.extract(archive, destination);

        assertEquals("tool", Files.readString(destination.resolve("cmdline-tools/bin/sdkmanager")));
    }

    @Test
    void rejectsTraversalAndCaseFoldedDuplicateBeforeExternalMutation(@TempDir Path temp) throws Exception {
        Path outside = temp.resolve("outside.txt");
        Path traversal = archive(temp.resolve("traversal.zip"), new Entry("../outside.txt", "escape", 0));
        Path duplicate = archive(temp.resolve("duplicate.zip"),
                new Entry("Tools/sdkmanager", "one", 0), new Entry("tools/SDKMANAGER", "two", 0));

        assertThrows(IOException.class, () -> SafeZipExtractor.extract(traversal, temp.resolve("traversal-out")));
        assertThrows(IOException.class, () -> SafeZipExtractor.extract(duplicate, temp.resolve("duplicate-out")));
        assertFalse(Files.exists(outside));
    }

    @Test
    void rejectsUnixSymlinkEntriesWithoutPublishingTheLink(@TempDir Path temp) throws Exception {
        Path archive = archive(temp.resolve("symlink.zip"),
                new Entry("cmdline-tools/latest", "../../outside", UnixStat.LINK_FLAG | 0777));
        Path destination = temp.resolve("destination");

        IOException failure = assertThrows(IOException.class, () -> SafeZipExtractor.extract(archive, destination));

        assertTrue(failure.getMessage().toLowerCase().contains("link"));
        assertFalse(Files.exists(destination.resolve("cmdline-tools/latest")));
    }

    private static Path archive(Path path, Entry... entries) throws IOException {
        try (ZipArchiveOutputStream output = new ZipArchiveOutputStream(path.toFile())) {
            for (Entry item : entries) {
                ZipArchiveEntry entry = new ZipArchiveEntry(item.name());
                if (item.unixMode() != 0) entry.setUnixMode(item.unixMode());
                byte[] content = item.content().getBytes(StandardCharsets.UTF_8);
                entry.setSize(content.length);
                output.putArchiveEntry(entry);
                output.write(content);
                output.closeArchiveEntry();
            }
        }
        return path;
    }

    private record Entry(String name, String content, int unixMode) { }
}
