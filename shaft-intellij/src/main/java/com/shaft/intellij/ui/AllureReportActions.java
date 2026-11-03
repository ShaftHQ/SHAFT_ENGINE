package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.notifications.ShaftNotifier;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Open / locate / merge / clean Allure evidence without making Doctor the first wall (W2.2, W2.10).
 */
public final class AllureReportActions {
    private AllureReportActions() {
        throw new IllegalStateException("Utility class");
    }

    public static @Nullable Path findAllureResults(@NotNull Path projectRoot) {
        Path[] candidates = {
                projectRoot.resolve("allure-results"),
                projectRoot.resolve("target/allure-results"),
                projectRoot.resolve("build/allure-results")
        };
        for (Path candidate : candidates) {
            if (Files.isDirectory(candidate)) {
                return candidate;
            }
        }
        return null;
    }

    public static @Nullable Path findLatestScreenshot(@NotNull Path projectRoot) throws IOException {
        Path results = findAllureResults(projectRoot);
        if (results == null) {
            return null;
        }
        try (Stream<Path> walk = Files.walk(results, 3)) {
            return walk
                    .filter(Files::isRegularFile)
                    .filter(AllureReportActions::isImageFile)
                    .max(Comparator.comparingLong(path -> path.toFile().lastModified()))
                    .orElse(null);
        }
    }

    public static void notifyEvidenceOrDoctor(@NotNull Project project, @NotNull Path projectRoot) {
        ShaftNotifier.info(project, "SHAFT evidence", evidenceMessage(projectRoot));
    }

    static @NotNull String evidenceMessage(@NotNull Path projectRoot) {
        Path results = findAllureResults(projectRoot);
        if (results == null) {
            return "No Allure results yet. Re-run the failed test, then open evidence or Doctor from the Tests panel.";
        }
        return "Allure results ready at " + projectRoot.relativize(results)
                + ". Open the folder or run Doctor from the failure notification — Doctor is optional, not the first wall.";
    }

    /**
     * Merge selected Allure results directories (e.g. Android + iOS) into {@code destination}.
     * Files keep their names; collisions get a numeric suffix. Does not delete sources.
     */
    public static @NotNull Path mergeResults(@NotNull Path destination, @NotNull List<Path> sources) throws IOException {
        if (sources.isEmpty()) {
            throw new IllegalArgumentException("At least one Allure results directory is required to merge.");
        }
        Files.createDirectories(destination);
        for (Path source : sources) {
            if (source == null || !Files.isDirectory(source)) {
                continue;
            }
            try (Stream<Path> files = Files.list(source)) {
                for (Path file : files.filter(Files::isRegularFile).toList()) {
                    Path target = uniqueSibling(destination.resolve(file.getFileName().toString()));
                    Files.copy(file, target, StandardCopyOption.COPY_ATTRIBUTES);
                }
            }
        }
        return destination;
    }

    /** Delete Allure results under the usual project locations. Returns deleted roots. */
    public static @NotNull List<Path> cleanResults(@NotNull Path projectRoot) throws IOException {
        List<Path> deleted = new ArrayList<>();
        Path[] candidates = {
                projectRoot.resolve("allure-results"),
                projectRoot.resolve("target/allure-results"),
                projectRoot.resolve("build/allure-results"),
                projectRoot.resolve("allure-report"),
                projectRoot.resolve("target/allure-report"),
                projectRoot.resolve("build/allure-report")
        };
        for (Path candidate : candidates) {
            if (Files.isDirectory(candidate)) {
                deleteRecursively(candidate);
                deleted.add(candidate);
            }
        }
        return deleted;
    }

    /** Collect unique tag / suite-group labels from {@code *-result.json} files when present. */
    public static @NotNull List<String> listTagsAndGroups(@NotNull Path resultsDir) throws IOException {
        if (!Files.isDirectory(resultsDir)) {
            return List.of();
        }
        Set<String> labels = new LinkedHashSet<>();
        try (Stream<Path> files = Files.list(resultsDir)) {
            for (Path file : files.filter(p -> p.getFileName().toString().endsWith("-result.json")).toList()) {
                String json = Files.readString(file);
                extractLabels(json, "tag", labels);
                extractLabels(json, "suite", labels);
                extractLabels(json, "package", labels);
                extractLabels(json, "story", labels);
            }
        }
        return List.copyOf(labels);
    }

    private static void extractLabels(String json, String name, Set<String> sink) {
        String needle = "\"name\":\"" + name + "\"";
        int from = 0;
        while (true) {
            int at = json.indexOf(needle, from);
            if (at < 0) {
                return;
            }
            int valueKey = json.indexOf("\"value\"", at);
            if (valueKey < 0 || valueKey > at + 80) {
                from = at + needle.length();
                continue;
            }
            int start = json.indexOf('"', valueKey + 7);
            if (start < 0) {
                return;
            }
            int end = json.indexOf('"', start + 1);
            if (end < 0) {
                return;
            }
            String value = json.substring(start + 1, end).trim();
            if (!value.isEmpty()) {
                sink.add(name + ":" + value);
            }
            from = end + 1;
        }
    }

    private static Path uniqueSibling(Path desired) throws IOException {
        if (!Files.exists(desired)) {
            return desired;
        }
        String fileName = desired.getFileName().toString();
        int dot = fileName.lastIndexOf('.');
        String stem = dot < 0 ? fileName : fileName.substring(0, dot);
        String ext = dot < 0 ? "" : fileName.substring(dot);
        Path parent = desired.getParent();
        for (int i = 2; i < 10_000; i++) {
            Path candidate = parent.resolve(stem + "-" + i + ext);
            if (!Files.exists(candidate)) {
                return candidate;
            }
        }
        throw new IOException("Could not allocate a unique name for " + fileName);
    }

    private static void deleteRecursively(Path root) throws IOException {
        Files.walkFileTree(root, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.deleteIfExists(file);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                Files.deleteIfExists(dir);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private static boolean isImageFile(@NotNull Path path) {
        String name = path.getFileName().toString().toLowerCase(Locale.ROOT);
        return name.endsWith(".png") || name.endsWith(".jpg");
    }
}
