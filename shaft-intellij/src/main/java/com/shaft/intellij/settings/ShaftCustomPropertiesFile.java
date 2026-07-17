package com.shaft.intellij.settings;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Pure, line-based read/modify/write helper for a SHAFT project's {@code custom.properties} file
 * (issue #3665 part A), conventionally at {@code src/main/resources/properties/custom.properties}
 * relative to the project root -- see {@code Capture.java}/{@code Healing.java} and siblings in
 * {@code shaft-engine/src/main/java/com/shaft/properties/internal/} for the real
 * {@code "file:src/main/resources/properties/custom.properties"} lookup convention this mirrors.
 * <p>
 * Deliberately NOT built on {@link java.util.Properties#load}/{@link java.util.Properties#store}:
 * {@code Properties#store} rewrites the entire file with its own formatting, discards comments,
 * and stamps a timestamp header -- which would silently destroy a real user's hand-curated
 * {@code custom.properties}. This class instead only ever touches the specific lines it needs to,
 * leaving everything else (comments, blank lines, key order, unrelated keys) byte-for-byte intact.
 * <p>
 * Takes a {@link Path}, not a {@code Project}: no SDK/PSI dependency, so it is trivially
 * unit-testable against real temp files with zero platform mocking.
 */
final class ShaftCustomPropertiesFile {
    private ShaftCustomPropertiesFile() {
    }

    /**
     * Reads {@code file} into a key/value map. Non-comment ({@code #}/{@code !} prefixed after
     * trimming), non-blank lines containing {@code =} are parsed as {@code key=value} (both sides
     * trimmed); when a key repeats, the last occurrence wins, matching real {@code .properties}
     * semantics. Returns an empty map when {@code file} does not exist.
     */
    static Map<String, String> read(Path file) {
        Map<String, String> result = new LinkedHashMap<>();
        if (file == null || !Files.isRegularFile(file)) {
            return result;
        }
        for (String line : readLines(file)) {
            parseKeyValue(line).ifPresent(entry -> result.put(entry[0], entry[1]));
        }
        return result;
    }

    /**
     * Applies {@code setKeys} and {@code removeKeys} to {@code file}. For each key in
     * {@code setKeys}, the first existing non-comment {@code key=value} line for that key has only
     * its value replaced (the rest of the file is untouched); if no such line exists, a new
     * {@code key=value} line is appended. For each key in {@code removeKeys}, its line is deleted
     * entirely if present (restoring SHAFT's own default). When {@code file} does not exist, it is
     * created (with parent directories) only if {@code setKeys} is non-empty, with a minimal header
     * comment followed by the {@code setKeys} entries; if {@code setKeys} is empty, nothing is
     * created.
     */
    static void write(Path file, Map<String, String> setKeys, Set<String> removeKeys) {
        if (file == null) {
            return;
        }
        Map<String, String> sets = setKeys == null ? Map.of() : setKeys;
        Set<String> removes = removeKeys == null ? Set.of() : removeKeys;
        if (!Files.isRegularFile(file)) {
            if (!sets.isEmpty()) {
                writeNewFile(file, sets);
            }
            return;
        }
        writeExistingFile(file, sets, removes);
    }

    private static void writeExistingFile(Path file, Map<String, String> sets, Set<String> removes) {
        List<String> original = readLines(file);
        List<String> lines = applyRemovals(original, removes);
        applySets(lines, sets);
        // Skip the write entirely when nothing actually changed (e.g. removeKeys named keys that
        // were never present): avoids touching a real user's file -- and its mtime/line-ending
        // normalization -- on every Settings "Apply" even when this section had no edits.
        if (!lines.equals(original)) {
            writeLines(file, lines);
        }
    }

    private static List<String> applyRemovals(List<String> original, Set<String> removes) {
        List<String> lines = new ArrayList<>(original);
        if (!removes.isEmpty()) {
            lines.removeIf(line -> parseKeyValue(line).map(kv -> removes.contains(kv[0])).orElse(false));
        }
        return lines;
    }

    private static void applySets(List<String> lines, Map<String, String> sets) {
        for (Map.Entry<String, String> entry : sets.entrySet()) {
            int index = indexOfKey(lines, entry.getKey());
            String newLine = entry.getKey() + "=" + entry.getValue();
            if (index >= 0) {
                lines.set(index, newLine);
            } else {
                lines.add(newLine);
            }
        }
    }

    private static void writeNewFile(Path file, Map<String, String> sets) {
        try {
            Path parent = file.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        List<String> content = new ArrayList<>();
        content.add("# SHAFT custom overrides");
        content.add("# Added by the SHAFT IntelliJ plugin Settings panel.");
        for (Map.Entry<String, String> entry : sets.entrySet()) {
            content.add(entry.getKey() + "=" + entry.getValue());
        }
        writeLines(file, content);
    }

    private static int indexOfKey(List<String> lines, String key) {
        for (int i = 0; i < lines.size(); i++) {
            Optional<String[]> parsed = parseKeyValue(lines.get(i));
            if (parsed.isPresent() && parsed.get()[0].equals(key)) {
                return i;
            }
        }
        return -1;
    }

    private static Optional<String[]> parseKeyValue(String line) {
        if (line == null) {
            return Optional.empty();
        }
        String trimmed = line.trim();
        if (trimmed.isEmpty() || trimmed.startsWith("#") || trimmed.startsWith("!")) {
            return Optional.empty();
        }
        int equalsIndex = trimmed.indexOf('=');
        if (equalsIndex < 0) {
            return Optional.empty();
        }
        String key = trimmed.substring(0, equalsIndex).trim();
        String value = trimmed.substring(equalsIndex + 1).trim();
        if (key.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(new String[]{key, value});
    }

    private static List<String> readLines(Path file) {
        try {
            return Files.readAllLines(file, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static void writeLines(Path file, List<String> lines) {
        try {
            Files.write(file, lines, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
