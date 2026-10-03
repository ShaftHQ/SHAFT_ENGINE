package com.shaft.intellij.testindex;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Reads the newest Allure result per test from {@code allure-results} or {@code target/allure-results}
 * (issues #6423, #6424). Malformed files are skipped; a missing directory is an empty map.
 */
public final class LastRunResults {
    private static final Pattern FRAME = Pattern.compile("^\\s*at\\s+([\\w.$]+)\\.([\\w$<>]+)\\([^:()]*:(\\d+)\\)");

    /** A source location of a failure. */
    public record Frame(String className, String method, int line) {
    }

    /** The newest outcome of one test. {@code frame} is null for passed tests or unknown traces. */
    public record Result(String fullName, String status, long start, long durationMillis, Frame frame) {
        public boolean failed() {
            return "failed".equals(status) || "broken".equals(status);
        }

        /** Compact inline hint, for example {@code ✗ failed · 1.3 s}. */
        public String label() {
            return (failed() ? "✗ " : "✓ ") + status + String.format(Locale.ROOT, " · %.1f s", durationMillis / 1000.0);
        }
    }

    private LastRunResults() {
    }

    /** Newest result keyed by {@code fullName} and {@code qualifiedClass#method}. */
    public static Map<String, Result> read(Path project) {
        Map<String, Result> newest = new HashMap<>();
        for (Path directory : List.of(project.resolve("allure-results"), project.resolve("target/allure-results"))) {
            if (!Files.isDirectory(directory)) {
                continue;
            }
            try (DirectoryStream<Path> files = Files.newDirectoryStream(directory, "*-result.json")) {
                for (Path file : files) {
                    Result result = parse(file);
                    if (result != null) {
                        newest.merge(result.fullName(), result, (a, b) -> a.start() >= b.start() ? a : b);
                    }
                }
            } catch (IOException | RuntimeException ignored) {
                // An unreadable directory contributes nothing.
            }
        }
        Map<String, Result> keyed = new HashMap<>(newest);
        newest.values().forEach(result -> keyed.put(SmartTagHistoryReader.toMethodKey(result.fullName()), result));
        return Map.copyOf(keyed);
    }

    /** Failed or broken tests of the last run, newest first. */
    public static List<Result> failures(Path project) {
        return read(project).values().stream().distinct().filter(Result::failed)
                .sorted((a, b) -> Long.compare(b.start(), a.start())).toList();
    }

    private static Result parse(Path file) {
        try {
            JsonObject json = JsonParser.parseString(Files.readString(file, StandardCharsets.UTF_8)).getAsJsonObject();
            String fullName = SmartTagHistoryReader.text(json, "fullName");
            if (fullName.isBlank()) {
                return null;
            }
            long start = SmartTagHistoryReader.longValue(json, "start");
            long stop = SmartTagHistoryReader.longValue(json, "stop");
            JsonObject details = json.has("statusDetails") && json.get("statusDetails").isJsonObject()
                    ? json.getAsJsonObject("statusDetails") : new JsonObject();
            String status = SmartTagHistoryReader.text(json, "status").toLowerCase(Locale.ROOT);
            return new Result(fullName, status, start, Math.max(0, stop - start),
                    frame(SmartTagHistoryReader.text(details, "trace"), fullName));
        } catch (IOException | RuntimeException malformed) {
            return null;
        }
    }

    /** Prefers the frame inside the test class, else the first frame outside test frameworks. */
    static Frame frame(String trace, String fullName) {
        String testClass = fullName.contains(".") ? fullName.substring(0, fullName.lastIndexOf('.')) : fullName;
        Frame fallback = null;
        for (String line : trace.split("\\R")) {
            Matcher matcher = FRAME.matcher(line);
            if (!matcher.find()) {
                continue;
            }
            Frame frame;
            try {
                frame = new Frame(matcher.group(1), matcher.group(2), Integer.parseInt(matcher.group(3)));
            } catch (NumberFormatException overflow) {
                continue;
            }
            if (frame.className().equals(testClass)) {
                return frame;
            }
            if (fallback == null && !frame.className().matches("(java|jdk|sun|org\\.testng|org\\.junit)\\..*")) {
                fallback = frame;
            }
        }
        return fallback;
    }
}
