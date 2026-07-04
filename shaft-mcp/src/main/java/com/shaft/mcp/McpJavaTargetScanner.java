package com.shaft.mcp;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Scans Java test sources for Capture record-at-target insertion candidates.
 */
final class McpJavaTargetScanner {
    private static final Pattern PACKAGE = Pattern.compile("\\bpackage\\s+([A-Za-z_$][\\w$]*(?:\\.[A-Za-z_$][\\w$]*)*)\\s*;");
    private static final Pattern CLASS = Pattern.compile("\\bclass\\s+([A-Za-z_$][\\w$]*)\\b");
    private static final Pattern DRIVER = Pattern.compile("\\bSHAFT\\.GUI\\.(?:WebDriver|Playwright)\\s+([A-Za-z_$][\\w$]*)\\b");
    private static final Pattern LOCATOR = Pattern.compile(
            "\\b(?:By|String)\\s+([A-Za-z_$][\\w$]*)\\s*=\\s*([^;\\n]+);");
    private static final Pattern METHOD = Pattern.compile("\\b(?:public|protected|private)?\\s*(?:static\\s+)?"
            + "[A-Za-z_$][\\w$<>\\[\\].]*\\s+([A-Za-z_$][\\w$]*)\\s*\\(");
    private static final int SUMMARY_LIMIT = 5;

    List<Candidate> scan(Path root, int maxResults) {
        int limit = maxResults <= 0 ? 20 : Math.min(maxResults, 50);
        try (var stream = Files.walk(root, 8)) {
            return stream
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".java"))
                    .filter(path -> !ignored(path))
                    .sorted(Comparator.comparing(path -> root.relativize(path).toString()))
                    .map(path -> candidate(root, path))
                    .filter(candidate -> candidate.score() > 0)
                    .sorted(Comparator.comparingInt(Candidate::score).reversed()
                            .thenComparing(Candidate::sourcePath))
                    .limit(limit)
                    .toList();
        } catch (IOException exception) {
            throw new IllegalArgumentException("Java target candidates could not be scanned.", exception);
        }
    }

    private static Candidate candidate(Path root, Path path) {
        try {
            String source = Files.readString(path, StandardCharsets.UTF_8);
            String className = first(CLASS, source);
            if (className.isBlank()) {
                return Candidate.empty(root, path);
            }
            String driver = first(DRIVER, source);
            List<String> anchors = methods(source, className);
            List<String> locators = locatorSummaries(source);
            List<String> actions = actionSummaries(source, className);
            int score = score(path, source, className, driver, anchors, locators, actions);
            return new Candidate(
                    root.relativize(path).toString().replace('\\', '/'),
                    first(PACKAGE, source),
                    className,
                    driver.isBlank() ? "driver" : driver,
                    anchors,
                    score,
                    locators,
                    actions);
        } catch (IOException exception) {
            return Candidate.empty(root, path);
        }
    }

    private static int score(
            Path path,
            String source,
            String className,
            String driver,
            List<String> anchors,
            List<String> locators,
            List<String> actions) {
        int score = 0;
        if (!driver.isBlank()) {
            score += 100;
        }
        String lowerName = className.toLowerCase(Locale.ROOT);
        if (lowerName.endsWith("page") || lowerName.endsWith("test")) {
            score += 30;
        }
        if (path.toString().replace('\\', '/').contains("src/test/java")) {
            score += 20;
        }
        if (!anchors.isEmpty()) {
            score += 10;
        }
        if (source.contains("@Test")) {
            score += 10;
        }
        if (!locators.isEmpty()) {
            score += 8;
        }
        if (!actions.isEmpty()) {
            score += 8;
        }
        if (source.contains("return this;")) {
            score += 12;
        }
        return score;
    }

    private static List<String> methods(String source, String className) {
        LinkedHashSet<String> methods = new LinkedHashSet<>();
        Matcher matcher = METHOD.matcher(source);
        while (matcher.find()) {
            String name = matcher.group(1);
            if (!name.equals(className)) {
                methods.add(name);
            }
        }
        return List.copyOf(methods);
    }

    private static List<String> locatorSummaries(String source) {
        LinkedHashSet<String> locators = new LinkedHashSet<>();
        Matcher matcher = LOCATOR.matcher(source);
        while (matcher.find() && locators.size() < SUMMARY_LIMIT) {
            String expression = matcher.group(2).trim();
            locators.add(matcher.group(1) + " = " + abbreviate(expression));
        }
        return List.copyOf(locators);
    }

    private static List<String> actionSummaries(String source, String className) {
        LinkedHashSet<String> actions = new LinkedHashSet<>();
        Matcher matcher = METHOD.matcher(source);
        while (matcher.find() && actions.size() < SUMMARY_LIMIT) {
            String name = matcher.group(1);
            if (name.equals(className)) {
                continue;
            }
            int end = Math.min(source.length(), matcher.end() + 600);
            String bodyPreview = source.substring(matcher.start(), end);
            if (bodyPreview.contains(".element()")
                    || bodyPreview.contains(".browser()")
                    || bodyPreview.contains("SHAFT.GUI")
                    || actionName(name)) {
                actions.add(name);
            }
        }
        return List.copyOf(actions);
    }

    private static boolean actionName(String name) {
        String lower = name.toLowerCase(Locale.ROOT);
        return lower.startsWith("open")
                || lower.startsWith("navigate")
                || lower.startsWith("login")
                || lower.startsWith("sign")
                || lower.startsWith("enter")
                || lower.startsWith("type")
                || lower.startsWith("click")
                || lower.startsWith("choose")
                || lower.startsWith("select")
                || lower.startsWith("submit")
                || lower.startsWith("verify")
                || lower.startsWith("assert");
    }

    private static String abbreviate(String value) {
        return value.length() <= 100 ? value : value.substring(0, 97) + "...";
    }

    private static String first(Pattern pattern, String source) {
        Matcher matcher = pattern.matcher(source);
        return matcher.find() ? matcher.group(1) : "";
    }

    private static boolean ignored(Path path) {
        String normalized = path.toString().replace('\\', '/');
        return normalized.contains("/target/")
                || normalized.contains("/build/")
                || normalized.contains("/.git/");
    }

    /**
     * Candidate Java target for Capture insertion.
     *
     * @param sourcePath workspace-relative Java source path
     * @param packageName Java package
     * @param className Java class name
     * @param driverVariableName likely SHAFT driver variable
     * @param insertionAnchors method names usable as insertion anchors
     * @param score deterministic confidence score
     * @param locatorSummaries existing locator field summaries
     * @param actionSummaries existing action method summaries
     */
    public record Candidate(
            String sourcePath,
            String packageName,
            String className,
            String driverVariableName,
            List<String> insertionAnchors,
            int score,
            List<String> locatorSummaries,
            List<String> actionSummaries) {
        /**
         * Compatibility constructor for existing candidate callers.
         */
        public Candidate(
                String sourcePath,
                String packageName,
                String className,
                String driverVariableName,
                List<String> insertionAnchors,
                int score) {
            this(sourcePath, packageName, className, driverVariableName, insertionAnchors, score, List.of(), List.of());
        }

        public Candidate {
            sourcePath = sourcePath == null ? "" : sourcePath.trim();
            packageName = packageName == null ? "" : packageName.trim();
            className = className == null ? "" : className.trim();
            driverVariableName = driverVariableName == null || driverVariableName.isBlank()
                    ? "driver"
                    : driverVariableName.trim();
            insertionAnchors = insertionAnchors == null ? List.of() : List.copyOf(insertionAnchors);
            locatorSummaries = locatorSummaries == null ? List.of() : List.copyOf(locatorSummaries);
            actionSummaries = actionSummaries == null ? List.of() : List.copyOf(actionSummaries);
        }

        private static Candidate empty(Path root, Path path) {
            return new Candidate(root.relativize(path).toString().replace('\\', '/'), "", "", "driver",
                    List.of(), 0, List.of(), List.of());
        }
    }
}
