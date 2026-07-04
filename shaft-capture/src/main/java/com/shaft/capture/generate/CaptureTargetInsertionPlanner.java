package com.shaft.capture.generate;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
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
            List<String> warnings = new ArrayList<>();
            Map<String, String> existingLocators = locatorFieldsByExpression(target);
            Map<String, String> generatedFieldAliases = new LinkedHashMap<>();
            String driver = javaIdentifierOrDefault(driverVariableName, targetDriver(target));
            String method = testMethod(generated);
            List<LocatorSnippet> inlineLocators = inlineLocators(method, existingLocators, warnings);
            String locators = locatorFields(generated, existingLocators, generatedFieldAliases, warnings);
            if (locators.isBlank() && !inlineLocators.isEmpty()) {
                locators = locatorFields(inlineLocators);
            }
            String actions = deduplicateActionLines(
                    actionSnippet(method, driver, inlineLocators, generatedFieldAliases),
                    target,
                    warnings);
            boolean anchorFound = anchorFound(target, anchor);
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
                    imports(generated, locators),
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

    private static List<String> imports(String source, String locatorFields) {
        List<String> imports = new ArrayList<>(source.lines()
                .filter(line -> line.startsWith("import "))
                .map(line -> line.substring("import ".length(), line.length() - 1))
                .toList());
        if (!locatorFields.isBlank() && !imports.contains("org.openqa.selenium.By")) {
            imports.add("org.openqa.selenium.By");
        }
        return List.copyOf(imports);
    }

    private static Map<String, String> locatorFieldsByExpression(String source) {
        Map<String, String> locators = new LinkedHashMap<>();
        source.lines().forEach(line -> {
            Matcher matcher = LOCATOR_FIELD.matcher(line);
            if (matcher.matches()) {
                locators.putIfAbsent(stripTrailingSemicolon(matcher.group(2).trim()), matcher.group(1));
            }
        });
        return Map.copyOf(locators);
    }

    private static String locatorFields(
            String source,
            Map<String, String> existingLocators,
            Map<String, String> generatedFieldAliases,
            List<String> warnings) {
        StringBuilder fields = new StringBuilder();
        source.lines().forEach(line -> {
            Matcher matcher = LOCATOR_FIELD.matcher(line);
            if (matcher.matches()) {
                String fieldName = matcher.group(1);
                String expression = stripTrailingSemicolon(matcher.group(2).trim());
                String existingField = existingLocators.get(expression);
                if (existingField != null) {
                    generatedFieldAliases.put(fieldName, existingField);
                    addWarning(warnings, "Reused existing locator field " + existingField + " for " + expression + ".");
                    return;
                }
                fields.append(line.strip()).append('\n');
            }
        });
        return fields.toString();
    }

    private static String locatorFields(List<LocatorSnippet> locators) {
        StringBuilder fields = new StringBuilder();
        for (LocatorSnippet locator : locators) {
            if (locator.existing()) {
                continue;
            }
            fields.append("private final By ")
                    .append(locator.fieldName())
                    .append(" = ")
                    .append(locator.expression())
                    .append(";\n");
        }
        return fields.toString();
    }

    private static String actionSnippet(
            String method,
            String driver,
            List<LocatorSnippet> locators,
            Map<String, String> generatedFieldAliases) {
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
                code.append(replaceLocators(
                                DRIVER_TOKEN.matcher(trimmed).replaceAll(driverReplacement),
                                locators,
                                generatedFieldAliases))
                        .append('\n');
            }
        }
        return code.toString();
    }

    private static String replaceLocators(
            String line,
            List<LocatorSnippet> locators,
            Map<String, String> generatedFieldAliases) {
        String result = line;
        for (Map.Entry<String, String> alias : generatedFieldAliases.entrySet()) {
            result = Pattern.compile("\\b" + Pattern.quote(alias.getKey()) + "\\b")
                    .matcher(result)
                    .replaceAll(Matcher.quoteReplacement(alias.getValue()));
        }
        for (LocatorSnippet locator : locators) {
            result = result.replace(locator.expression(), locator.fieldName());
        }
        return result;
    }

    private static String deduplicateActionLines(String actions, String target, List<String> warnings) {
        if (actions.isBlank()) {
            return "";
        }
        List<String> targetLines = target.lines()
                .map(String::trim)
                .filter(line -> !line.isBlank())
                .toList();
        StringBuilder result = new StringBuilder();
        for (String line : actions.lines().toList()) {
            String trimmed = line.trim();
            if (trimmed.isBlank()) {
                continue;
            }
            if (targetLines.contains(trimmed)) {
                addWarning(warnings, "Skipped duplicate action line already present in target: " + trimmed);
                continue;
            }
            result.append(trimmed).append('\n');
        }
        return result.toString();
    }

    private static List<LocatorSnippet> inlineLocators(
            String method,
            Map<String, String> existingLocators,
            List<String> warnings) {
        Map<String, LocatorSnippet> locators = new LinkedHashMap<>();
        for (String line : method.lines().toList()) {
            for (String expression : locatorExpressions(line)) {
                locators.computeIfAbsent(expression, ignored -> {
                    String existingField = existingLocators.get(expression);
                    if (existingField != null) {
                        addWarning(warnings, "Reused existing locator field " + existingField + " for " + expression + ".");
                        return new LocatorSnippet(existingField, expression, true);
                    }
                    return new LocatorSnippet(uniqueFieldName(expression, locators.size() + 1), expression, false);
                });
            }
        }
        return List.copyOf(locators.values());
    }

    private static List<String> locatorExpressions(String line) {
        List<String> expressions = new ArrayList<>();
        int index = 0;
        while (index < line.length()) {
            int start = line.indexOf("SHAFT.GUI.Locator.", index);
            if (start < 0) {
                break;
            }
            int end = expressionEnd(line, start);
            if (end > start) {
                expressions.add(line.substring(start, end).trim());
                index = end;
            } else {
                index = start + 1;
            }
        }
        return expressions;
    }

    private static int expressionEnd(String line, int start) {
        int depth = 0;
        boolean string = false;
        boolean escaped = false;
        for (int index = start; index < line.length(); index++) {
            char character = line.charAt(index);
            if (string) {
                if (escaped) {
                    escaped = false;
                } else if (character == '\\') {
                    escaped = true;
                } else if (character == '"') {
                    string = false;
                }
                continue;
            }
            if (character == '"') {
                string = true;
            } else if (character == '(') {
                depth++;
            } else if (character == ')') {
                depth--;
                if (depth == 0) {
                    return index + 1;
                }
            }
        }
        return line.length();
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

    private static String stripTrailingSemicolon(String value) {
        return value.endsWith(";") ? value.substring(0, value.length() - 1).trim() : value;
    }

    private static void addWarning(List<String> warnings, String warning) {
        if (!warnings.contains(warning)) {
            warnings.add(warning);
        }
    }

    private static String uniqueFieldName(String expression, int index) {
        Matcher quoted = Pattern.compile("\"([^\"]+)\"").matcher(expression);
        String seed = quoted.find() ? quoted.group(1) : "capturedElement";
        String base = lowerCamel(seed);
        if (base.isBlank()) {
            base = "capturedElement";
        }
        if (index > 1) {
            base = base + index;
        }
        return javaIdentifierOrDefault(base + "Locator", "capturedElement" + index + "Locator");
    }

    private static String lowerCamel(String value) {
        String[] parts = (value == null ? "" : value)
                .replaceAll("([a-z0-9])([A-Z])", "$1 $2")
                .replaceAll("[^A-Za-z0-9]+", " ")
                .trim()
                .split("\\s+");
        if (parts.length == 0 || parts[0].isBlank()) {
            return "";
        }
        StringBuilder result = new StringBuilder(parts[0].toLowerCase(Locale.ROOT));
        for (int index = 1; index < parts.length; index++) {
            String part = parts[index].toLowerCase(Locale.ROOT);
            result.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
        }
        return result.toString();
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

    private record LocatorSnippet(String fieldName, String expression, boolean existing) {
    }
}
