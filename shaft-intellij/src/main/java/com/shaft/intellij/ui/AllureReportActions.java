package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.notifications.ShaftNotifier;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Stream;

/**
 * Open / locate Allure evidence without making Doctor the first wall (W2.2).
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

    private static boolean isImageFile(@NotNull Path path) {
        String name = path.getFileName().toString().toLowerCase();
        return name.endsWith(".png") || name.endsWith(".jpg");
    }
}
