package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises only {@link ShowTraceViewerAction#resolveLatestTraceViewer(Path)} -- the pure
 * filesystem resolution logic, with no IDE/JCEF dependency -- so it runs headless like the rest
 * of this plugin's test suite.
 */
class ShowTraceViewerActionTest {
    @TempDir
    Path project;

    @Test
    void returnsNullWhenNoTracesExist() throws IOException {
        assertNull(ShowTraceViewerAction.resolveLatestTraceViewer(project));
    }

    @Test
    void resolvesLooseHtmlWhenAlreadyPresent() throws IOException {
        Path traceDirectory = writeTraceIndex("only-trace", Instant.parse("2026-07-08T10:00:00Z"));
        Path html = traceDirectory.resolve("SHAFT Trace Report.html");
        Files.writeString(html, "<html>loose</html>");

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertEquals(html, resolved);
        assertEquals("<html>loose</html>", Files.readString(resolved));
    }

    @Test
    void extractsHtmlFromTraceZipWhenNoLooseCopyExists() throws IOException {
        Path traceDirectory = writeTraceIndex("zipped-trace", Instant.parse("2026-07-08T10:00:00Z"));
        writeTraceZip(traceDirectory.resolve("shaft-trace.zip"), "<html>from-zip</html>");

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertEquals(traceDirectory.resolve("SHAFT Trace Report.html"), resolved);
        assertEquals("<html>from-zip</html>", Files.readString(resolved));
    }

    @Test
    void selectsTheNewestTraceByGeneratedAt() throws IOException {
        Path older = writeTraceIndex("older", Instant.parse("2026-07-08T09:00:00Z"));
        writeTraceZip(older.resolve("shaft-trace.zip"), "<html>older</html>");
        Path newer = writeTraceIndex("newer", Instant.parse("2026-07-08T11:00:00Z"));
        writeTraceZip(newer.resolve("shaft-trace.zip"), "<html>newer</html>");

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertTrue(resolved.startsWith(newer), resolved.toString());
    }

    @Test
    void fallsBackToFileModifiedTimeWhenGeneratedAtIsMissing() throws IOException {
        Path traceDirectory = project.resolve("target").resolve("shaft-traces").resolve("no-timestamp");
        Files.createDirectories(traceDirectory);
        Files.writeString(traceDirectory.resolve("index.json"), "{\"testId\": \"no-timestamp\"}");
        Files.writeString(traceDirectory.resolve("SHAFT Trace Report.html"), "<html>fallback</html>");
        Files.setLastModifiedTime(traceDirectory.resolve("index.json"), FileTime.from(Instant.now()));

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertEquals(traceDirectory.resolve("SHAFT Trace Report.html"), resolved);
    }

    private Path writeTraceIndex(String testId, Instant generatedAt) throws IOException {
        Path traceDirectory = project.resolve("target").resolve("shaft-traces").resolve(testId);
        Files.createDirectories(traceDirectory);
        Files.writeString(traceDirectory.resolve("index.json"), """
                {
                  "testId": "%s",
                  "generatedAt": "%s",
                  "archive": "target/shaft-traces/%s/shaft-trace.zip",
                  "entries": {"html": "SHAFT Trace Report.html", "json": "shaft-trace.json", "network": "shaft-network.har"}
                }
                """.formatted(testId, generatedAt, testId));
        return traceDirectory;
    }

    private void writeTraceZip(Path zipPath, String html) throws IOException {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(zipPath))) {
            zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
            zip.write(html.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            zip.closeEntry();
        }
    }
}
