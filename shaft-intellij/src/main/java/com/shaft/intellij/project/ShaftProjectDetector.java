package com.shaft.intellij.project;

import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Detects whether an IntelliJ project actually depends on SHAFT. Every SHAFT-specific action in
 * this plugin used to be offered unconditionally in any Java-capable project (SHAFT or not); this
 * is the shared check that lets them gate on the project actually being a SHAFT project instead.
 */
public final class ShaftProjectDetector {
    private static final List<String> BUILD_FILE_NAMES = List.of("pom.xml", "build.gradle", "build.gradle.kts");
    private static final List<String> SHAFT_MARKERS = List.of("io.github.shafthq", "shaft-engine", "shaft-bom");
    private static final Set<String> SKIPPED_DIRECTORY_NAMES = Set.of("target", "build", ".git", "node_modules");
    private static final long MAX_BUILD_FILE_BYTES = 200_000L;
    private static final long CACHE_TTL_MILLIS = 30_000L;

    private static final ConcurrentHashMap<Path, CacheEntry> CACHE = new ConcurrentHashMap<>();

    private record CacheEntry(boolean shaftProject, long computedAtMillis) {
    }

    private ShaftProjectDetector() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns whether the given IntelliJ project depends on SHAFT.
     *
     * @param project IntelliJ project, or {@code null}
     * @return {@code true} only when a SHAFT dependency marker was found
     */
    public static boolean isShaftProject(@Nullable Project project) {
        if (project == null || project.getBasePath() == null) {
            return false;
        }
        return isShaftProject(Path.of(project.getBasePath()));
    }

    /**
     * Returns whether the given project root depends on SHAFT: its own {@code pom.xml}/
     * {@code build.gradle}/{@code build.gradle.kts}, or one belonging to a direct child directory
     * (to catch multi-module layouts), mentions {@code io.github.shafthq}, {@code shaft-engine}, or
     * {@code shaft-bom}. Fails closed (returns {@code false}) on any I/O error, so a filesystem
     * hiccup never wrongly enables SHAFT-only actions. Results are cached briefly per root since
     * IntelliJ calls this from action {@code update()} methods on every menu refresh.
     *
     * @param projectRoot project root directory
     * @return {@code true} only when a SHAFT dependency marker was found
     */
    static boolean isShaftProject(Path projectRoot) {
        Path normalized = projectRoot.toAbsolutePath().normalize();
        long now = System.currentTimeMillis();
        CacheEntry cached = CACHE.get(normalized);
        if (cached != null && now - cached.computedAtMillis() < CACHE_TTL_MILLIS) {
            return cached.shaftProject();
        }
        boolean result = hasShaftMarker(normalized) || hasShaftMarkerInChildDirectory(normalized);
        CACHE.put(normalized, new CacheEntry(result, now));
        return result;
    }

    private static boolean hasShaftMarkerInChildDirectory(Path root) {
        try (DirectoryStream<Path> children = Files.newDirectoryStream(root, Files::isDirectory)) {
            for (Path child : children) {
                String name = child.getFileName().toString();
                if (!name.startsWith(".") && !SKIPPED_DIRECTORY_NAMES.contains(name) && hasShaftMarker(child)) {
                    return true;
                }
            }
        } catch (IOException ignored) {
            return false;
        }
        return false;
    }

    private static boolean hasShaftMarker(Path directory) {
        for (String buildFileName : BUILD_FILE_NAMES) {
            Path buildFile = directory.resolve(buildFileName);
            if (Files.isRegularFile(buildFile) && mentionsShaft(buildFile)) {
                return true;
            }
        }
        return false;
    }

    private static boolean mentionsShaft(Path buildFile) {
        try {
            String content = readBounded(buildFile).toLowerCase(Locale.ROOT);
            for (String marker : SHAFT_MARKERS) {
                if (content.contains(marker)) {
                    return true;
                }
            }
            return false;
        } catch (IOException ignored) {
            return false;
        }
    }

    private static String readBounded(Path file) throws IOException {
        try (InputStream input = Files.newInputStream(file)) {
            return new String(input.readNBytes((int) MAX_BUILD_FILE_BYTES), StandardCharsets.UTF_8);
        }
    }

    /**
     * Clears cached detection results. Test-only seam.
     */
    static void clearCacheForTests() {
        CACHE.clear();
    }
}
