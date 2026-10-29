package com.shaft.tools.io.internal;

import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/** Shared, bounded Java-source lookup for trace, diagnostics, and step evidence. */
final class SourceCodeContext {
    private static final int RADIUS = 2;
    private static final int MAX_READ_CHARACTERS = 1_000_000;
    private static final int MAX_LINE_CHARACTERS = 16_384;

    private SourceCodeContext() {
        throw new IllegalStateException("Utility class");
    }

    static Path findSourceFile(StackTraceElement frame) {
        return findSourceFile(frame, null);
    }

    static Path findSourceFile(StackTraceElement frame, Class<?> caller) {
        String fileName = frame.getFileName();
        if (fileName == null || !fileName.endsWith(".java") || fileName.contains("/") || fileName.contains("\\")) {
            return null;
        }
        int packageEnd = frame.getClassName().lastIndexOf('.');
        String packagePath = packageEnd < 0 ? "" : frame.getClassName().substring(0, packageEnd).replace('.', '/');
        LinkedHashSet<Path> roots = new LinkedHashSet<>();
        Path module = moduleRoot(caller);
        if (module != null) {
            roots.add(module.resolve("src/test/java"));
            roots.add(module.resolve("src/main/java"));
        }
        roots.add(Path.of("src/test/java"));
        roots.add(Path.of("src/main/java"));
        roots.add(Path.of("shaft-engine/src/test/java"));
        roots.add(Path.of("shaft-engine/src/main/java"));
        for (Path root : roots) {
            Path candidate = root.resolve(packagePath).resolve(fileName).normalize();
            try {
                if (Files.isRegularFile(candidate) && candidate.toRealPath().startsWith(root.toRealPath())) {
                    return candidate;
                }
            } catch (IOException | SecurityException ignored) {
                // An unavailable source root is normal for compiled-only test distributions.
            }
        }
        return null;
    }

    static String relative(Path sourceFile, Class<?> caller) {
        Path absolute = sourceFile.toAbsolutePath().normalize();
        Path workingDirectory = Path.of("").toAbsolutePath().normalize();
        if (absolute.startsWith(workingDirectory)) {
            return workingDirectory.relativize(absolute).toString().replace('\\', '/');
        }
        Path module = moduleRoot(caller);
        if (module != null && absolute.startsWith(module)) {
            return module.getFileName() + "/" + module.relativize(absolute).toString().replace('\\', '/');
        }
        return sourceFile.getFileName().toString();
    }

    private static Path moduleRoot(Class<?> caller) {
        if (caller == null) {
            return null;
        }
        try {
            var source = caller.getProtectionDomain().getCodeSource();
            if (source == null || !"file".equals(source.getLocation().getProtocol())) {
                return null;
            }
            Path output = Path.of(source.getLocation().toURI()).toAbsolutePath().normalize();
            Path parent = output.getParent();
            if (parent != null && "target".equals(parent.getFileName().toString())
                    && ("test-classes".equals(output.getFileName().toString())
                    || "classes".equals(output.getFileName().toString()))) {
                return parent.getParent();
            }
        } catch (java.net.URISyntaxException | RuntimeException ignored) {
            // Custom class loaders may not expose a usable Maven output directory.
        }
        return null;
    }

    static Window read(Path sourceFile, int lineNumber) {
        if (lineNumber < 1) {
            return new Window(List.of(), "Source line metadata is unavailable.");
        }
        if (sourceFile == null) {
            return new Window(List.of(), "Java source is unavailable on the test runner.");
        }
        int start = Math.max(1, lineNumber - RADIUS);
        long end = (long) lineNumber + RADIUS;
        List<Line> lines = new ArrayList<>();
        StringBuilder text = new StringBuilder();
        int current = 1;
        int read = 0;
        try (Reader reader = Files.newBufferedReader(sourceFile, StandardCharsets.UTF_8)) {
            int character;
            while (current <= end && (character = reader.read()) != -1) {
                if (++read > MAX_READ_CHARACTERS) {
                    return new Window(List.of(), "Source context omitted because the read limit was reached.");
                }
                if (character == '\n') {
                    if (current >= start) {
                        lines.add(new Line(current, withoutCarriageReturn(text)));
                    }
                    current++;
                    text.setLength(0);
                } else if (current >= start) {
                    if (text.length() >= MAX_LINE_CHARACTERS) {
                        return new Window(List.of(), "Source context omitted because a line exceeds the size limit.");
                    }
                    text.append((char) character);
                }
            }
            if (current >= start && current <= end && !text.isEmpty()) {
                lines.add(new Line(current, withoutCarriageReturn(text)));
            }
        } catch (IOException | SecurityException exception) {
            return new Window(List.of(), "Java source could not be read on the test runner.");
        }
        if (lines.stream().noneMatch(line -> line.number() == lineNumber)) {
            return new Window(List.of(), "The recorded line is outside the available source file.");
        }
        return new Window(List.copyOf(lines), "");
    }

    private static String withoutCarriageReturn(StringBuilder text) {
        int end = text.length();
        return text.substring(0, end > 0 && text.charAt(end - 1) == '\r' ? end - 1 : end);
    }

    static String snippet(Path sourceFile, int lineNumber) {
        Window window = read(sourceFile, lineNumber);
        StringBuilder snippet = new StringBuilder();
        for (Line line : window.lines()) {
            snippet.append(line.number() == lineNumber ? "> " : "  ")
                    .append(line.number()).append(": ").append(line.text()).append(System.lineSeparator());
        }
        return snippet.toString().trim();
    }

    record Line(int number, String text) { }

    record Window(List<Line> lines, String unavailableReason) { }
}
