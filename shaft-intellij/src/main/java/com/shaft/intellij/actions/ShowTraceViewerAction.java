package com.shaft.intellij.actions;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.project.ShaftProjectDetector;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.List;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * Opens the offline SHAFT trace viewer HTML for the most recently generated {@code
 * target/shaft-traces} trace in the user's default browser.
 * <p>
 * Uses an external-browser fallback ({@link BrowserUtil#browse}) rather than an embedded JCEF
 * panel: the plugin has no existing JCEF dependency, and plugin tests run headless, so an
 * embedded panel would be unverifiable by automated tests.
 */
public final class ShowTraceViewerAction extends AnAction implements DumbAware {
    private static final String VIEWER_HTML_NAME = "SHAFT Trace Report.html";
    private static final String TRACE_ZIP_NAME = "shaft-trace.zip";
    private static final String NOTIFICATION_TITLE = "Trace viewer";

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null) {
            return;
        }
        String basePath = project.getBasePath();
        Path viewer;
        try {
            viewer = basePath == null ? null : resolveLatestTraceViewer(Path.of(basePath));
        } catch (IOException e) {
            ShaftNotifier.warn(project, NOTIFICATION_TITLE, "Could not resolve a SHAFT trace viewer: " + e.getMessage());
            return;
        }
        if (viewer == null) {
            ShaftNotifier.warn(project, NOTIFICATION_TITLE, "No SHAFT trace was found under target/shaft-traces.");
            return;
        }
        BrowserUtil.browse(viewer.toUri());
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        event.getPresentation().setEnabledAndVisible(project != null && ShaftProjectDetector.isShaftProject(project));
    }

    /**
     * Resolves the offline viewer HTML for the most recently generated trace under
     * {@code <projectRoot>/target/shaft-traces}, extracting it from {@code shaft-trace.zip} when
     * no loose copy exists on disk. Returns {@code null} when no trace can be found. Pure
     * filesystem logic with no IDE dependency, so it is unit-testable headless.
     *
     * @param projectRoot project base directory
     * @return resolved viewer HTML path, or {@code null} when no trace exists
     */
    static Path resolveLatestTraceViewer(Path projectRoot) throws IOException {
        Path traceRoot = projectRoot.resolve("target").resolve("shaft-traces");
        if (!Files.isDirectory(traceRoot)) {
            return null;
        }
        Path newestDirectory = null;
        Instant newestGeneratedAt = null;
        try (Stream<Path> paths = Files.walk(traceRoot, 2)) {
            List<Path> indexFiles = paths
                    .filter(path -> "index.json".equals(path.getFileName().toString()))
                    .toList();
            for (Path indexPath : indexFiles) {
                Instant generatedAt = generatedAt(indexPath);
                if (newestGeneratedAt == null || generatedAt.isAfter(newestGeneratedAt)) {
                    newestGeneratedAt = generatedAt;
                    newestDirectory = indexPath.getParent();
                }
            }
        }
        return newestDirectory == null ? null : resolveViewerHtml(newestDirectory);
    }

    private static Instant generatedAt(Path indexPath) {
        try {
            String content = Files.readString(indexPath);
            JsonObject index = JsonParser.parseString(content).getAsJsonObject();
            if (index.has("generatedAt")) {
                return Instant.parse(index.get("generatedAt").getAsString());
            }
        } catch (RuntimeException | IOException ignored) {
            // Fall through to the file's modified time; a malformed index must never break discovery.
        }
        try {
            FileTime modified = Files.getLastModifiedTime(indexPath);
            return modified.toInstant();
        } catch (IOException e) {
            return Instant.EPOCH;
        }
    }

    private static Path resolveViewerHtml(Path traceDirectory) throws IOException {
        Path loose = traceDirectory.resolve(VIEWER_HTML_NAME);
        if (Files.isRegularFile(loose)) {
            return loose;
        }
        Path archive = traceDirectory.resolve(TRACE_ZIP_NAME);
        if (!Files.isRegularFile(archive)) {
            return null;
        }
        try (InputStream input = Files.newInputStream(archive); ZipInputStream zip = new ZipInputStream(input)) {
            ZipEntry entry;
            while ((entry = zip.getNextEntry()) != null) {
                if (VIEWER_HTML_NAME.equals(entry.getName())) {
                    Files.write(loose, zip.readAllBytes());
                    return loose;
                }
            }
        }
        return null;
    }
}
