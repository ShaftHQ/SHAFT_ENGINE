package com.shaft.mcp;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Finds the most recently written Allure results directory inside an MCP workspace so Doctor and
 * Healer requests can omit explicit paths.
 */
final class McpAllureResultsLocator {
    private static final int MAX_DEPTH = 5;
    private static final Set<String> SKIPPED_DIRECTORIES = Set.of(
            ".git", ".idea", ".gradle", "node_modules", ".m2", ".venv", "venv");

    private McpAllureResultsLocator() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the populated {@code allure-results} directory with the newest result file, or
     * {@code null} when the workspace has none.
     *
     * @param workspaceRoot workspace root to scan
     * @return newest populated Allure results directory or {@code null}
     */
    static Path latest(Path workspaceRoot) {
        List<Path> candidates = new ArrayList<>();
        try {
            Files.walkFileTree(workspaceRoot, Set.of(), MAX_DEPTH, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path directory, BasicFileAttributes attributes) {
                    String name = directory.getFileName() == null
                            ? ""
                            : directory.getFileName().toString().toLowerCase(Locale.ROOT);
                    if (SKIPPED_DIRECTORIES.contains(name)) {
                        return FileVisitResult.SKIP_SUBTREE;
                    }
                    if ("allure-results".equals(name)) {
                        candidates.add(directory);
                        return FileVisitResult.SKIP_SUBTREE;
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exception) {
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException exception) {
            return null;
        }
        return candidates.stream()
                .filter(directory -> newestResultTime(directory) != null)
                .max(Comparator.comparing(McpAllureResultsLocator::newestResultTime))
                .orElse(null);
    }

    private static FileTime newestResultTime(Path directory) {
        try (var files = Files.list(directory)) {
            return files.filter(Files::isRegularFile)
                    .filter(file -> file.getFileName().toString().endsWith("-result.json"))
                    .map(McpAllureResultsLocator::lastModified)
                    .max(Comparator.naturalOrder())
                    .orElse(null);
        } catch (IOException exception) {
            return null;
        }
    }

    private static FileTime lastModified(Path file) {
        try {
            return Files.getLastModifiedTime(file);
        } catch (IOException exception) {
            return FileTime.fromMillis(0);
        }
    }
}
