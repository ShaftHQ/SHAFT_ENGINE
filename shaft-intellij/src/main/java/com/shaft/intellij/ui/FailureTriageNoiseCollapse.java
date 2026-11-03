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
        for (String line : lines) {
            if (NOISE.matcher(line).find()) {
                continue;
            }
            Matcher m = EXCEPTION_LINE.matcher(line);
            if (m.find()) {
                return m.group(1).trim();
            }
            String lower = line.toLowerCase(Locale.ROOT);
            if (lower.contains("assertion") || lower.contains("expected:") || lower.contains("but was:")) {
                return line.trim();
            }
        }
        for (String line : lines) {
            String trimmed = line.trim();
            if (!trimmed.isEmpty() && !NOISE.matcher(trimmed).find()) {
                return trimmed;
            }
        }
        return "";
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
        for (String line : log.split("\\R")) {
            if (NOISE.matcher(line).find()) {
                continue;
            }
            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            if (kept.size() >= Math.max(1, maxLines)) {
                break;
            }
            if (!kept.contains(trimmed) && !(kept.size() == 1 && ("FIRST_ERROR: " + trimmed).equals(kept.get(0)))) {
                kept.add(trimmed);
            }
        }
        return kept;
    }
}
