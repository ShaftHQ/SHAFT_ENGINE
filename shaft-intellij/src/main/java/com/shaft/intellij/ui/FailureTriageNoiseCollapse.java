package com.shaft.intellij.ui;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Collapse noisy failure logs so the first real error is highlighted (W4.2 / #7606 thin).
 */
public final class FailureTriageNoiseCollapse {
    private static final Pattern EXCEPTION_LINE = Pattern.compile(
            "^\\s*([a-zA-Z0-9_$.]*(?:Exception|Error|Failure|AssertionError)\\b.*)$");
    private static final Pattern NOISE = Pattern.compile(
            "(?i)(SLF4J|Downloading from|Progress \\(|\\[INFO\\]|\\[DEBUG\\]|Download https)");

    private FailureTriageNoiseCollapse() {
        throw new IllegalStateException("Utility class");
    }

    public static @NotNull String firstRealError(@Nullable String log) {
        if (log == null || log.isBlank()) {
            return "";
        }
        String[] lines = log.split("\\R");
        String matched = matchExceptionOrAssertion(lines);
        if (!matched.isEmpty()) {
            return matched;
        }
        return firstNonNoiseLine(lines);
    }

    private static @NotNull String matchExceptionOrAssertion(@NotNull String[] lines) {
        for (String line : lines) {
            if (isNoise(line)) {
                continue;
            }
            Matcher m = EXCEPTION_LINE.matcher(line);
            if (m.find()) {
                return m.group(1).trim();
            }
            if (looksLikeAssertion(line)) {
                return line.trim();
            }
        }
        return "";
    }

    private static @NotNull String firstNonNoiseLine(@NotNull String[] lines) {
        for (String line : lines) {
            String trimmed = line.trim();
            if (!trimmed.isEmpty() && !isNoise(trimmed)) {
                return trimmed;
            }
        }
        return "";
    }

    private static boolean isNoise(@NotNull String line) {
        return NOISE.matcher(line).find();
    }

    private static boolean looksLikeAssertion(@NotNull String line) {
        String lower = line.toLowerCase(Locale.ROOT);
        return lower.contains("assertion") || lower.contains("expected:") || lower.contains("but was:");
    }

    public static @NotNull List<String> collapse(@Nullable String log, int maxLines) {
        List<String> kept = new ArrayList<>();
        if (log == null || log.isBlank()) {
            return kept;
        }
        String first = firstRealError(log);
        if (!first.isEmpty()) {
            kept.add("FIRST_ERROR: " + first);
        }
        int limit = Math.max(1, maxLines);
        for (String line : log.split("\\R")) {
            if (kept.size() >= limit || isNoise(line)) {
                if (kept.size() >= limit) {
                    break;
                }
                continue;
            }
            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            boolean duplicateFirst = kept.size() == 1 && ("FIRST_ERROR: " + trimmed).equals(kept.get(0));
            if (!kept.contains(trimmed) && !duplicateFirst) {
                kept.add(trimmed);
            }
        }
        return kept;
    }
}
