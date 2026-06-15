package com.shaft.mcp;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Extracts reusable code blocks from deterministic Capture generation output.
 */
final class McpCaptureCodeBlockService {
    private static final Pattern DRIVER_TOKEN = Pattern.compile("\\bdriver\\b");

    /**
     * Extracts full-class and test-method snippets from a generated Java source file.
     *
     * @param sourcePath generated Java source
     * @param driverVariableName desired driver variable name for method snippets
     * @return MCP code blocks
     */
    List<McpCodeBlock> fromGeneratedSource(Path sourcePath, String driverVariableName) {
        try {
            String source = Files.readString(sourcePath, StandardCharsets.UTF_8);
            List<String> imports = imports(source);
            List<McpCodeBlock> blocks = new ArrayList<>();
            blocks.add(new McpCodeBlock(
                    "capture-full-class",
                    "Generated SHAFT replay class",
                    McpCodeBlock.Kind.FULL_CLASS,
                    "java",
                    imports,
                    source,
                    "Use as a generated TestNG class or copy selected methods into your suite.",
                    true,
                    List.of(),
                    List.of()));
            String method = testMethod(source);
            if (!method.isBlank()) {
                String driver = javaIdentifierOrDefault(driverVariableName, "driver");
                blocks.add(new McpCodeBlock(
                        "capture-test-method",
                        "Generated replay method body",
                        McpCodeBlock.Kind.TEST_METHOD,
                        "java",
                        imports,
                        DRIVER_TOKEN.matcher(method).replaceAll(driver),
                        "Paste into an existing class that already owns a SHAFT.GUI.WebDriver named " + driver + ".",
                        true,
                        List.of(),
                        List.of("Generated helper fields or test data references may also be needed.")));
            }
            return List.copyOf(blocks);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Generated Capture source could not be read.", exception);
        }
    }

    private static List<String> imports(String source) {
        return source.lines()
                .filter(line -> line.startsWith("import "))
                .map(line -> line.substring("import ".length(), line.length() - 1))
                .toList();
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

    private static String javaIdentifierOrDefault(String value, String fallback) {
        String candidate = value == null || value.isBlank() ? fallback : value.trim();
        if (!Character.isJavaIdentifierStart(candidate.charAt(0))) {
            return fallback;
        }
        for (int index = 1; index < candidate.length(); index++) {
            if (!Character.isJavaIdentifierPart(candidate.charAt(index))) {
                return fallback;
            }
        }
        return candidate;
    }
}
