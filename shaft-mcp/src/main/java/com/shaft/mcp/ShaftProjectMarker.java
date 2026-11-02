package com.shaft.mcp;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Detects whether a repository actually depends on SHAFT. Guarded reruns and verifications that
 * expect SHAFT Allure evidence use this to fail fast, before spending a real build, in a repository
 * that can never produce that evidence shape.
 */
final class ShaftProjectMarker {
    private static final List<String> BUILD_FILE_NAMES = List.of("pom.xml", "build.gradle", "build.gradle.kts");
    private static final List<String> SHAFT_MARKERS = List.of("io.github.shafthq", "shaft-engine", "shaft-bom");
    private static final Set<String> SKIPPED_DIRECTORY_NAMES = Set.of("target", "build", ".git", "node_modules");
    private static final long MAX_BUILD_FILE_BYTES = 200_000L;

    private ShaftProjectMarker() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns whether the given repository root depends on SHAFT: its own {@code pom.xml}/
     * {@code build.gradle}/{@code build.gradle.kts}, or one belonging to a direct child directory
     * (to catch multi-module layouts), mentions {@code io.github.shafthq}, {@code shaft-engine}, or
     * {@code shaft-bom}. Fails closed (returns {@code false}) on any I/O error, so a filesystem
     * hiccup never wrongly skips a guarded rerun.
     *
     * @param repositoryRoot repository root directory
     * @return {@code true} only when a SHAFT dependency marker was found
     */
    static boolean isShaftRepository(Path repositoryRoot) {
        Path normalized = repositoryRoot.toAbsolutePath().normalize();
        return hasShaftMarker(normalized) || hasShaftMarkerInChildDirectory(normalized);
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
}
