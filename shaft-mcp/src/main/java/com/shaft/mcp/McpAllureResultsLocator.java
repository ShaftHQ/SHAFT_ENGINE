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
import java.util.Optional;
import java.util.Set;

/**
 * Finds the most recently written Allure evidence inside an MCP workspace so Doctor and Healer
 * requests can omit explicit paths: either a populated {@code allure-results} directory, or a SHAFT
 * single-file {@code *AllureReport.html} report inside an {@code allure-report} directory.
 */
final class McpAllureResultsLocator {
    private static final int MAX_DEPTH = 5;
    private static final String REPORT_FILE_SUFFIX = "AllureReport.html";
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

    /**
     * Returns the newest SHAFT single-file Allure HTML report ({@code *AllureReport.html} inside an
     * {@code allure-report} directory), or empty when the workspace has none.
     *
     * @param workspaceRoot workspace root to scan
     * @return newest single-file Allure report, if any
     */
    static Optional<Path> latestReport(Path workspaceRoot) {
        List<Path> candidates = new ArrayList<>();
        try {
            Files.walkFileTree(workspaceRoot, Set.of(), MAX_DEPTH, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path directory, BasicFileAttributes attributes) {
                    String name = directory.getFileName() == null
                            ? ""
                            : directory.getFileName().toString().toLowerCase(Locale.ROOT);
                    return SKIPPED_DIRECTORIES.contains(name) ? FileVisitResult.SKIP_SUBTREE : FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attributes) {
                    Path parent = file.getParent();
                    String parentName = parent == null || parent.getFileName() == null
                            ? ""
                            : parent.getFileName().toString().toLowerCase(Locale.ROOT);
                    if ("allure-report".equals(parentName) && file.getFileName().toString().endsWith(REPORT_FILE_SUFFIX)) {
                        candidates.add(file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exception) {
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException exception) {
            return Optional.empty();
        }
        return candidates.stream().max(Comparator.comparing(McpAllureResultsLocator::lastModified));
    }

    /**
     * Returns whichever Allure evidence is newest in the workspace: a populated {@code
     * allure-results} directory or a single-file {@code *AllureReport.html} report. Ties are broken
     * in favor of the {@code allure-results} directory, since it carries richer per-attachment
     * evidence than the self-contained report.
     *
     * @param workspaceRoot workspace root to scan
     * @return newest Allure evidence path, if any
     */
    static Optional<Path> latestEvidence(Path workspaceRoot) {
        Path resultsDirectory = latest(workspaceRoot);
        Optional<Path> report = latestReport(workspaceRoot);
        if (resultsDirectory == null) {
            return report;
        }
        if (report.isEmpty()) {
            return Optional.of(resultsDirectory);
        }
        FileTime resultsTime = newestResultTime(resultsDirectory);
        FileTime reportTime = lastModified(report.get());
        return reportTime.compareTo(resultsTime) > 0 ? report : Optional.of(resultsDirectory);
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
