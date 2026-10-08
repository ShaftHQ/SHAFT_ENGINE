package com.shaft.intellij.testindex;

import com.shaft.intellij.settings.ShaftCustomPropertiesFile;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Pure helpers for the Tests panel toolbar UX (epic #6633 PR B: #6639, #6643, #6637, #6638, #6642).
 * No IDE types — unit-testable against temp files and plain lists.
 */
public final class TestsPanelToolbarSupport {
    public static final String CUSTOM_PROPERTIES_RELATIVE =
            "src/main/resources/properties/custom.properties";
    public static final String HEADLESS_KEY = "headlessExecution";
    public static final String BROWSER_KEY = "targetBrowserName";
    public static final String PROPERTIES_FOLDER_KEY = "propertiesFolderPath";
    public static final String PROPERTIES_DIR_RELATIVE = "src/main/resources/properties";
    public static final String DEFAULT_PROFILE_LABEL = "(default custom.properties)";
    public static final List<String> BROWSER_CHOICES =
            List.of("chrome", "firefox", "edge", "safari");

    private TestsPanelToolbarSupport() {
    }

    /** Project-relative path to {@code custom.properties}. */
    public static Path customPropertiesPath(Path projectRoot) {
        return projectRoot.resolve(CUSTOM_PROPERTIES_RELATIVE);
    }

    /** Reads {@code headlessExecution}; missing key means not headless (show browser). */
    public static boolean isHeadless(Path projectRoot) {
        if (projectRoot == null) {
            return false;
        }
        Map<String, String> props = ShaftCustomPropertiesFile.read(customPropertiesPath(projectRoot));
        String value = props.get(HEADLESS_KEY);
        return value != null && Boolean.parseBoolean(value.trim());
    }

    /** Writes {@code headlessExecution} into the project's {@code custom.properties}. */
    public static void writeHeadless(Path projectRoot, boolean headless) {
        Objects.requireNonNull(projectRoot, "projectRoot");
        ShaftCustomPropertiesFile.write(
                customPropertiesPath(projectRoot),
                Map.of(HEADLESS_KEY, String.valueOf(headless)),
                Set.of());
    }

    /**
     * Show-browser toggle selected means the browser is visible ({@code headlessExecution=false}).
     */
    public static boolean showBrowserSelected(boolean headlessExecution) {
        return !headlessExecution;
    }

    public static boolean headlessFromShowBrowser(boolean showBrowserSelected) {
        return !showBrowserSelected;
    }

    /**
     * One runnable target derived from discovery: class FQN plus optional method name.
     */
    public record RunTarget(String qualifiedClassName, String methodName) {
        public RunTarget {
            Objects.requireNonNull(qualifiedClassName, "qualifiedClassName");
            if (qualifiedClassName.isBlank()) {
                throw new IllegalArgumentException("qualifiedClassName blank");
            }
            methodName = methodName == null || methodName.isBlank() ? null : methodName;
        }

        public String displayKey() {
            return methodName == null ? qualifiedClassName : qualifiedClassName + "#" + methodName;
        }
    }

    /**
     * Every discovered method when methods exist; otherwise each class once.
     */
    public static List<RunTarget> runAllTargets(List<ShaftTestDiscovery.DiscoveredTestClass> discovered) {
        if (discovered == null || discovered.isEmpty()) {
            return List.of();
        }
        List<RunTarget> targets = new ArrayList<>();
        for (ShaftTestDiscovery.DiscoveredTestClass testClass : discovered) {
            if (testClass == null || testClass.qualifiedName() == null || testClass.qualifiedName().isBlank()) {
                continue;
            }
            List<String> methods = testClass.methodNames();
            if (methods == null || methods.isEmpty()) {
                targets.add(new RunTarget(testClass.qualifiedName(), null));
            } else {
                for (String method : methods) {
                    if (method != null && !method.isBlank()) {
                        targets.add(new RunTarget(testClass.qualifiedName(), method));
                    }
                }
            }
        }
        return List.copyOf(targets);
    }

    /**
     * Last-failure targets from the index snapshot. Method-scoped ids use {@code Class#method};
     * otherwise the whole id is treated as a class name.
     */
    public static List<RunTarget> rerunFailedTargets(List<ShaftTestIndex.TestRowState> rows) {
        if (rows == null || rows.isEmpty()) {
            return List.of();
        }
        List<RunTarget> targets = new ArrayList<>();
        Set<String> seen = new LinkedHashSet<>();
        for (ShaftTestIndex.TestRowState row : rows) {
            if (row == null || row.status() != ShaftTestIndex.Status.FAIL) {
                continue;
            }
            String testId = row.testId();
            if (testId == null || testId.isBlank()) {
                continue;
            }
            RunTarget target = parseTestId(testId);
            if (seen.add(target.displayKey())) {
                targets.add(target);
            }
        }
        return List.copyOf(targets);
    }

    static RunTarget parseTestId(String testId) {
        int hash = testId.indexOf('#');
        if (hash > 0 && hash < testId.length() - 1) {
            return new RunTarget(testId.substring(0, hash), testId.substring(hash + 1));
        }
        int lastDot = testId.lastIndexOf('.');
        // "com.example.Foo.bar" style config names: treat last segment as method only when it
        // starts lowercase (Java method convention); otherwise whole string is the class.
        if (lastDot > 0 && lastDot < testId.length() - 1) {
            String maybeMethod = testId.substring(lastDot + 1);
            String maybeClass = testId.substring(0, lastDot);
            if (!maybeMethod.isEmpty()
                    && Character.isLowerCase(maybeMethod.charAt(0))
                    && maybeClass.indexOf('.') >= 0) {
                return new RunTarget(maybeClass, maybeMethod);
            }
        }
        return new RunTarget(testId, null);
    }

    /**
     * Profile labels under {@code src/main/resources/properties}: the default file plus each
     * subdirectory that contains a {@code custom.properties}.
     */
    public static List<String> listPropertyProfiles(Path projectRoot) {
        if (projectRoot == null) {
            return List.of(DEFAULT_PROFILE_LABEL);
        }
        Path propertiesDir = projectRoot.resolve(PROPERTIES_DIR_RELATIVE);
        List<String> profiles = new ArrayList<>();
        profiles.add(DEFAULT_PROFILE_LABEL);
        if (!Files.isDirectory(propertiesDir)) {
            return List.copyOf(profiles);
        }
        try (Stream<Path> children = Files.list(propertiesDir)) {
            children.filter(Files::isDirectory)
                    .sorted()
                    .forEach(dir -> {
                        if (Files.isRegularFile(dir.resolve("custom.properties"))) {
                            profiles.add(dir.getFileName().toString());
                        }
                    });
        } catch (Exception ignored) {
            // Listing failures leave the default-only list.
        }
        return List.copyOf(profiles);
    }

    /**
     * Absolute folder path for {@code -DpropertiesFolderPath}, or empty for the default profile.
     */
    public static String propertiesFolderPathForProfile(Path projectRoot, String profileLabel) {
        if (projectRoot == null || profileLabel == null || profileLabel.equals(DEFAULT_PROFILE_LABEL)
                || profileLabel.isBlank()) {
            return "";
        }
        Path folder = projectRoot.resolve(PROPERTIES_DIR_RELATIVE).resolve(profileLabel);
        return Files.isDirectory(folder) ? folder.toAbsolutePath().normalize().toString() : "";
    }

    /**
     * Expands a selected set of browsers into the run matrix. Empty selection means one run with
     * no browser override (inherit {@code custom.properties}).
     */
    public static List<String> browserRunMatrix(Set<String> selectedBrowsers) {
        if (selectedBrowsers == null || selectedBrowsers.isEmpty()) {
            return List.of("");
        }
        List<String> ordered = new ArrayList<>();
        for (String browser : BROWSER_CHOICES) {
            if (selectedBrowsers.contains(browser)) {
                ordered.add(browser);
            }
        }
        for (String browser : selectedBrowsers) {
            if (browser != null && !browser.isBlank() && !ordered.contains(browser)) {
                ordered.add(browser.trim().toLowerCase(Locale.ROOT));
            }
        }
        return ordered.isEmpty() ? List.of("") : List.copyOf(ordered);
    }

    /**
     * Whether a terminated test run should auto-open the newest trace (#6637).
     */
    public static boolean shouldAutoOpenTrace(
            boolean settingEnabled,
            int exitCode,
            boolean userCancelled,
            boolean looksLikeTestRun) {
        return settingEnabled && exitCode != 0 && !userCancelled && looksLikeTestRun;
    }
}
