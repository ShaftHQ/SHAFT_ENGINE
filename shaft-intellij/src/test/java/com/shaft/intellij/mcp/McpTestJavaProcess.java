package com.shaft.intellij.mcp;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

/** Bounded Java command used by fake MCP subprocess tests on Windows. */
final class McpTestJavaProcess {
    private McpTestJavaProcess() {
    }

    static List<String> command(Class<?> mainClass, String... arguments) {
        return command(mainClass, List.of(), arguments);
    }

    static List<String> command(Class<?> mainClass, List<String> jvmOptions, String... arguments) {
        List<String> command = new ArrayList<>();
        command.add(javaExecutable());
        command.add("@" + ClasspathArgumentFile.PATH);
        command.addAll(jvmOptions);
        command.add(mainClass.getName());
        command.addAll(Arrays.asList(arguments));
        return List.copyOf(command);
    }

    static String commandLine(Class<?> mainClass, String... arguments) {
        return commandLine(mainClass, List.of(), arguments);
    }

    static String commandLine(Class<?> mainClass, List<String> jvmOptions, String... arguments) {
        return command(mainClass, jvmOptions, arguments).stream()
                .map(McpTestJavaProcess::quote)
                .reduce((left, right) -> left + " " + right)
                .orElseThrow();
    }

    static Path classpathArgumentFile() {
        return ClasspathArgumentFile.PATH;
    }

    private static String javaExecutable() {
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win");
        return Paths.get(System.getProperty("java.home"), "bin", windows ? "java.exe" : "java").toString();
    }

    private static String quote(String value) {
        return "\"" + value.replace("\"", "\\\"") + "\"";
    }

    private static final class ClasspathArgumentFile {
        private static final Path PATH = create();

        private static Path create() {
            try {
                Path file = Files.createTempFile("shaft-intellij-mcp-test-classpath-", ".args")
                        .toAbsolutePath().normalize();
                String classpath = System.getProperty("java.class.path").replace('\\', '/');
                Files.writeString(file, "-cp" + System.lineSeparator()
                                + "\"" + classpath.replace("\"", "\\\"") + "\"" + System.lineSeparator(),
                        StandardCharsets.UTF_8);
                file.toFile().deleteOnExit();
                return file;
            } catch (IOException exception) {
                throw new UncheckedIOException("Could not create the bounded MCP test classpath argument file.",
                        exception);
            }
        }
    }
}
