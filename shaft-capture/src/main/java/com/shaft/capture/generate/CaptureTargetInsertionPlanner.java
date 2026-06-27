package com.shaft.capture.generate;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Builds focused snippets for inserting Capture-generated actions into an existing source target.
 */
public final class CaptureTargetInsertionPlanner {
    private static final Pattern DRIVER_TOKEN = Pattern.compile("\\bdriver\\b");
    private static final Pattern LOCATOR_FIELD = Pattern.compile(
            "\\s*(?:private|protected|public)?\\s*(?:static\\s+)?final\\s+By\\s+"
                    + "([A-Za-z_$][\\w$]*)\\s*=\\s*(.+);\\s*");
    private static final Pattern TARGET_DRIVER = Pattern.compile(
            "\\bSHAFT\\.GUI\\.(?:Driver|WebDriver|Playwright)\\s+([A-Za-z_$][\\w$]*)\\b");

    /**
     * Creates a planner.
     */
    public CaptureTargetInsertionPlanner() {
        // Public utility for CLI and MCP adapters.
    }

    /**
     * Builds record-at-target snippets without editing the target source file.
     *
     * @param generatedSource generated Capture Java source
     * @param targetSource existing Java source selected by the user
     * @param insertAfter method name or textual anchor to insert after
     * @param driverVariableName optional driver variable name used in the target source
     * @return insertion snippets and validation warnings
     */
    public CaptureTargetInsertionPlan plan(
            Path generatedSource,
            Path targetSource,
            String insertAfter,
            String driverVariableName) {
        if (generatedSource == null) {
            throw new IllegalArgumentException("Generated Capture source path is required.");
        }
        if (targetSource == null) {
            throw new IllegalArgumentException("Capture target source path is required.");
        }
        String anchor = insertAfter == null ? "" : insertAfter.trim();
        if (anchor.isBlank()) {
            throw new IllegalArgumentException("Capture insertion anchor is required.");
        }
        try {
            String generated = Files.readString(generatedSource, StandardCharsets.UTF_8);
            String target = Files.readString(targetSource, StandardCharsets.UTF_8);
            String driver = javaIdentifierOrDefault(driverVariableName, targetDriver(target));
            String method = testMethod(generated);
            String actions = actionSnippet(method, driver);
            String locators = locatorFields(generated);
            boolean anchorFound = anchorFound(target, anchor);
            List<String> warnings = new ArrayList<>();
            if (!anchorFound) {
                warnings.add("Target anchor " + anchor + " was not found in " + targetSource.getFileName()
                        + "; use the snippet manually.");
            }
            if (actions.isBlank()) {
                warnings.add("Generated Capture source did not contain a TestNG replay method body.");
            }
            if (locators.isBlank() && actions.contains("LOCATOR")) {
                warnings.add("Generated action snippet references locators, but no locator fields were extracted.");
            }
            return new CaptureTargetInsertionPlan(
                    targetSource,
                    anchor,
                    imports(generated),
                    locators,
                    actions,
                    "Paste the action snippet in " + targetSource.getFileName() + " after " + anchor
                            + ". Add locator fields/imports from the companion block if missing.",
                    anchorFound,
                    warnings);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Capture record-at-target source could not be read.", exception);
        }
    }

    private static List<String> imports(String source) {
        return source.lines()
                .filter(line -> line.startsWith("import "))
                .map(line -> line.substring("import ".length(), line.length() - 1))
                .toList();
    }

    private static String locatorFields(String source) {
        StringBuilder fields = new StringBuilder();
        source.lines().forEach(line -> {
            if (LOCATOR_FIELD.matcher(line).matches()) {
                fields.append(line.strip()).append('\n');
            }
        });
        return fields.toString();
    }

    private static String actionSnippet(String method, String driver) {
        if (method.isBlank()) {
            return "";
        }
        List<String> lines = method.lines().toList();
        StringBuilder code = new StringBuilder();
        String driverReplacement = Matcher.quoteReplacement(driver);
        for (int index = 1; index < lines.size(); index++) {
            if (index == lines.size() - 1 && lines.get(index).trim().equals("}")) {
                continue;
            }
            String trimmed = lines.get(index).trim();
            if (!trimmed.isBlank()) {
                code.append(DRIVER_TOKEN.matcher(trimmed).replaceAll(driverReplacement)).append('\n');
            }
        }
        return code.toString();
    }

    private static String testMethod(String source) {
        List<String> lines = source.lines().toList();
        for (int index = 0; index < lines.size(); index++) {
            if (!lines.get(index).trim().equals("@Test")) {
                continue;
            }
            int methodStart = index + 1;
            while (methodStart < lines.size() && !lines.get(methodStart).contains("{")) {
                methodStart++;
            }
            if (methodStart >= lines.size()) {
                return "";
            }
            int depth = 0;
            List<String> method = new ArrayList<>();
            for (int lineIndex = methodStart; lineIndex < lines.size(); lineIndex++) {
                String line = lines.get(lineIndex);
                method.add(line);
                for (int charIndex = 0; charIndex < line.length(); charIndex++) {
                    char character = line.charAt(charIndex);
                    if (character == '{') {
                        depth++;
                    } else if (character == '}') {
                        depth--;
                    }
                }
                if (depth == 0) {
                    return String.join("\n", method) + "\n";
                }
            }
        }
        return "";
    }

    private static boolean anchorFound(String source, String anchor) {
        String quoted = Pattern.quote(anchor);
        Pattern method = Pattern.compile("\\b" + quoted + "\\s*\\(");
        return method.matcher(source).find()
                || source.toLowerCase(Locale.ROOT).contains(anchor.toLowerCase(Locale.ROOT));
    }

    private static String targetDriver(String source) {
        Matcher matcher = TARGET_DRIVER.matcher(source);
        return matcher.find() ? matcher.group(1) : "driver";
    }

    private static String javaIdentifierOrDefault(String value, String fallback) {
        String candidate = value == null || value.isBlank() ? fallback : value.trim();
        if (candidate == null || candidate.isBlank() || !Character.isJavaIdentifierStart(candidate.charAt(0))) {
            return "driver";
        }
        for (int index = 1; index < candidate.length(); index++) {
            if (!Character.isJavaIdentifierPart(candidate.charAt(index))) {
                return "driver";
            }
        }
        return candidate;
    }
}
