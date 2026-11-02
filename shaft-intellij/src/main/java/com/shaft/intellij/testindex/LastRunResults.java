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
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Reads the newest Allure result per test from {@code allure-results} or {@code target/allure-results}
 * (issues #6423, #6424). Malformed files are skipped; a missing directory is an empty map.
 */
public final class LastRunResults {
    static final int HINT_LIMIT = 160;
    private static final Pattern FRAME = Pattern.compile("^\\s*at\\s+([\\w.$]+)\\.([\\w$<>]+)\\([^:()]*:(\\d+)\\)");

    /** A source location of a failure. */
    public record Frame(String className, String method, int line) {
    }

    /**
     * The newest outcome of one test. {@code frame} is null for passed tests or unknown traces;
     * {@code message} is the Allure {@code statusDetails.message}, blank when absent.
     */
    public record Result(String fullName, String status, long start, long durationMillis, Frame frame, String message) {
        public boolean failed() {
            return "failed".equals(status) || "broken".equals(status);
        }

        /** Compact inline hint, for example {@code ✗ failed · 1.3 s}. */
        public String label() {
            return (failed() ? "✗ " : "✓ ") + status + String.format(Locale.ROOT, " · %.1f s", durationMillis / 1000.0);
        }

        /** One-line failure message for an end-of-line hint, capped at {@value #HINT_LIMIT} characters (#6636). */
        public String failureHint() {
            String line = message == null || message.isBlank() ? status : message.strip().split("\\R", 2)[0].strip();
            return "✗ " + (line.length() > HINT_LIMIT ? line.substring(0, HINT_LIMIT - 1) + "…" : line);
        }
    }

    private static final Map<Path, Cached> CACHE = new ConcurrentHashMap<>();

    private record Cached(List<Long> stamp, Map<String, Result> results) {
    }

    private LastRunResults() {
    }

    /**
     * Newest result keyed by {@code fullName} and {@code qualifiedClass#method}. Cached per project
     * until a result directory's modification time or file count changes (issue #6635): inlay hints
     * call this on every highlighting pass, and re-parsing every result file each time was slow.
     */
    public static Map<String, Result> read(Path project) {
        List<Path> directories = directories(project);
        List<Long> stamp = stamp(directories);
        Cached cached = CACHE.get(project);
        if (cached != null && cached.stamp().equals(stamp)) {
            return cached.results();
        }
        Map<String, Result> results = parseAll(directories);
        CACHE.put(project, new Cached(stamp, results));
        return results;
    }

    private static List<Path> directories(Path project) {
        return List.of(project.resolve("allure-results"), project.resolve("target/allure-results"));
    }

    /** Modification time and result-file count per directory; -1 for a missing directory. */
    private static List<Long> stamp(List<Path> directories) {
        List<Long> stamp = new java.util.ArrayList<>();
        for (Path directory : directories) {
            long modified = -1;
            long count = -1;
            if (Files.isDirectory(directory)) {
                try (DirectoryStream<Path> files = Files.newDirectoryStream(directory, "*-result.json")) {
                    modified = Files.getLastModifiedTime(directory).toMillis();
                    count = 0;
                    java.util.Iterator<Path> iterator = files.iterator();
                    while (iterator.hasNext()) {
                        iterator.next();
                        count++;
                    }
                } catch (IOException | RuntimeException unreadable) {
                    modified = -2;
                }
            }
            stamp.add(modified);
            stamp.add(count);
        }
        return stamp;
    }

    private static Map<String, Result> parseAll(List<Path> directories) {
        Map<String, Result> newest = new HashMap<>();
        for (Path directory : directories) {
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
                    frame(SmartTagHistoryReader.text(details, "trace"), fullName),
                    SmartTagHistoryReader.text(details, "message"));
        } catch (IOException | RuntimeException malformed) {
            return null;
        }
    }

    /**
     * Failures of the last run whose frame is in {@code qualifiedClass}, keyed by 1-based line; the
     * newest failure wins when several tests fail at the same line (#6636).
     */
    public static Map<Integer, Result> failuresAt(Map<String, Result> results, String qualifiedClass) {
        Map<Integer, Result> byLine = new java.util.TreeMap<>();
        results.values().stream().distinct()
                .filter(result -> result.failed() && result.frame() != null
                        && result.frame().className().replace('$', '.').equals(qualifiedClass))
                .forEach(result -> byLine.merge(result.frame().line(), result,
                        (a, b) -> a.start() >= b.start() ? a : b));
        return byLine;
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
