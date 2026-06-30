package com.shaft.intellij.mcp;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Applies the current IntelliJ project as the workspace for MCP launches.
 */
final class ShaftMcpProjectScope {
    static final String WORKSPACE_ENVIRONMENT_VARIABLE = "SHAFT_MCP_WORKSPACE_ROOT";
    private static final String USER_DIR_PROPERTY = "user.dir";
    private static final String WORKSPACE_PROPERTY = "shaft.mcp.workspaceRoot";

    private ShaftMcpProjectScope() {
        throw new IllegalStateException("Utility class");
    }

    static List<String> commandForProject(List<String> command, Path projectRoot) throws IOException {
        if (command == null || command.isEmpty()) {
            return List.of();
        }
        Path root = normalize(projectRoot);
        List<String> scoped = new ArrayList<>(command);
        for (int index = 1; index < scoped.size(); index++) {
            String token = scoped.get(index);
            if (token != null && token.startsWith("@") && token.length() > 1) {
                scoped.set(index, "@" + scopedArgFile(Path.of(token.substring(1)), root));
                return List.copyOf(scoped);
            }
        }
        if (isJavaCommand(scoped.get(0))) {
            return javaCommandForProject(scoped, root);
        }
        return List.copyOf(scoped);
    }

    static Map<String, String> environmentForProject(Map<String, String> environment, Path projectRoot) {
        Map<String, String> scoped = new LinkedHashMap<>(environment == null ? Map.of() : environment);
        scoped.put(WORKSPACE_ENVIRONMENT_VARIABLE, normalize(projectRoot).toString());
        return Map.copyOf(scoped);
    }

    private static Path scopedArgFile(Path source, Path projectRoot) throws IOException {
        List<String> original = Files.readAllLines(source, StandardCharsets.UTF_8);
        List<String> scoped = new ArrayList<>(original.size() + 2);
        boolean sawUserDir = false;
        boolean sawWorkspace = false;
        for (String line : original) {
            String property = propertyName(line);
            if (USER_DIR_PROPERTY.equals(property)) {
                scoped.add(systemProperty(USER_DIR_PROPERTY, projectRoot));
                sawUserDir = true;
            } else if (WORKSPACE_PROPERTY.equals(property)) {
                scoped.add(systemProperty(WORKSPACE_PROPERTY, projectRoot));
                sawWorkspace = true;
            } else {
                scoped.add(line);
            }
        }
        List<String> withRequiredProperties = new ArrayList<>(scoped.size() + 2);
        if (!sawUserDir) {
            withRequiredProperties.add(systemProperty(USER_DIR_PROPERTY, projectRoot));
        }
        if (!sawWorkspace) {
            withRequiredProperties.add(systemProperty(WORKSPACE_PROPERTY, projectRoot));
        }
        withRequiredProperties.addAll(scoped);

        Path generated = Files.createTempFile("shaft-intellij-mcp-", ".args");
        generated.toFile().deleteOnExit();
        Files.write(generated, withRequiredProperties, StandardCharsets.UTF_8);
        return generated;
    }

    private static String propertyName(String line) {
        String value = unquote(line == null ? "" : line.trim());
        if (!value.startsWith("-D")) {
            return "";
        }
        int separator = value.indexOf('=');
        return separator > 2 ? value.substring(2, separator) : "";
    }

    private static String systemProperty(String property, Path projectRoot) {
        return quote("-D" + property + "=" + projectRoot.toString().replace('\\', '/'));
    }

    private static List<String> javaCommandForProject(List<String> command, Path projectRoot) {
        List<String> scoped = new ArrayList<>(command.size() + 2);
        scoped.add(command.get(0));
        scoped.add(systemPropertyToken(USER_DIR_PROPERTY, projectRoot));
        scoped.add(systemPropertyToken(WORKSPACE_PROPERTY, projectRoot));
        for (int index = 1; index < command.size(); index++) {
            String property = propertyName(command.get(index));
            if (!USER_DIR_PROPERTY.equals(property) && !WORKSPACE_PROPERTY.equals(property)) {
                scoped.add(command.get(index));
            }
        }
        return List.copyOf(scoped);
    }

    private static String systemPropertyToken(String property, Path projectRoot) {
        return "-D" + property + "=" + projectRoot.toString().replace('\\', '/');
    }

    private static boolean isJavaCommand(String token) {
        String normalized = token == null ? "" : token.replace('\\', '/').toLowerCase();
        int slash = normalized.lastIndexOf('/');
        String executable = slash >= 0 ? normalized.substring(slash + 1) : normalized;
        return "java".equals(executable) || "java.exe".equals(executable);
    }

    private static String quote(String value) {
        return "\"" + value.replace("\"", "\\\"") + "\"";
    }

    private static String unquote(String value) {
        if (value.length() >= 2 && ((value.charAt(0) == '"' && value.charAt(value.length() - 1) == '"')
                || (value.charAt(0) == '\'' && value.charAt(value.length() - 1) == '\''))) {
            return value.substring(1, value.length() - 1);
        }
        return value;
    }

    private static Path normalize(Path projectRoot) {
        return (projectRoot == null ? Path.of(".") : projectRoot).toAbsolutePath().normalize();
    }
}
