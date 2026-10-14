package com.shaft.tools.io.internal;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.regex.Matcher;

/**
 * Counts an Allure 3 suite name that may live only inside a base64 widget.
 * A plaintext miss is not an empty report. An empty results directory stays unverified.
 * The read is a bounded window and does not load the single-file HTML as one string.
 */
public final class AllureSuiteWidgetReader {
    public enum Presence {
        PRESENT,
        ABSENT,
        UNVERIFIED
    }

    private static final int WINDOW = 65_536;
    private static final int CARRY = 4_096;
    private AllureSuiteWidgetReader() {
    }

    /**
     * @param resultsDirectory Allure results directory
     * @param widgetFile       widget payload, not the whole single-file report
     * @param suiteName        suite name to find in plaintext or in one decoded blob
     */
    public static Presence suite(Path resultsDirectory, Path widgetFile, String suiteName) throws IOException {
        if (!hasResultJson(resultsDirectory)) {
            return Presence.UNVERIFIED;
        }
        if (suiteName == null || suiteName.isBlank() || widgetFile == null || !Files.isRegularFile(widgetFile)) {
            return Presence.ABSENT;
        }
        return count(widgetFile, suiteName) > 0 ? Presence.PRESENT : Presence.ABSENT;
    }

    /**
     * @return 1 when the suite name is present in plaintext, in a decoded blob, or in both; otherwise 0
     */
    public static int count(Path widgetFile, String suiteName) throws IOException {
        if (widgetFile == null || suiteName == null || suiteName.isBlank() || !Files.isRegularFile(widgetFile)) {
            return 0;
        }
        boolean found = false;
        byte[] slide = new byte[WINDOW + CARRY];
        ReportBlobScan.Latin1Window text = new ReportBlobScan.Latin1Window(slide);
        Matcher tokens = ReportBlobScan.tokens(text);
        int slideLen = 0;
        try (InputStream input = Files.newInputStream(widgetFile)) {
            while (true) {
                int read = input.read(slide, slideLen, WINDOW);
                if (read < 0) {
                    break;
                }
                slideLen += read;
                text.setLength(slideLen);
                if (containsSuite(text, tokens, suiteName)) {
                    found = true;
                }
                int keep = Math.min(slideLen, CARRY);
                int discard = slideLen - keep;
                if (discard > 0) {
                    System.arraycopy(slide, discard, slide, 0, keep);
                    slideLen = keep;
                }
            }
        }
        return found ? 1 : 0;
    }

    private static boolean containsSuite(CharSequence text, Matcher tokens, String suiteName) {
        if (outsideBase64(text, tokens, suiteName)) {
            return true;
        }
        tokens.reset();
        while (tokens.find()) {
            int length = tokens.end() - tokens.start();
            if (!ReportBlobScan.atLeast(tokens, ReportBlobScan.WIDGET_MINIMUM) || length > WINDOW) {
                continue;
            }
            try {
                String decoded = new String(Base64.getDecoder().decode(tokens.group()), StandardCharsets.UTF_8);
                if (decoded.contains(suiteName)) {
                    return true;
                }
            } catch (IllegalArgumentException ignored) {
                // not a widget payload
            }
        }
        return false;
    }

    private static boolean outsideBase64(CharSequence text, Matcher tokens, String suiteName) {
        int from = 0;
        int limit = text.length() - suiteName.length();
        while (from <= limit) {
            int at = indexOf(text, suiteName, from);
            if (at < 0) {
                return false;
            }
            if (!insideRun(tokens, at, at + suiteName.length())) {
                return true;
            }
            from = at + 1;
        }
        return false;
    }

    private static int indexOf(CharSequence text, String needle, int from) {
        int limit = text.length() - needle.length();
        for (int i = from; i <= limit; i++) {
            if (matchesAt(text, needle, i)) {
                return i;
            }
        }
        return -1;
    }

    private static boolean matchesAt(CharSequence text, String needle, int offset) {
        for (int i = 0; i < needle.length(); i++) {
            if (text.charAt(offset + i) != needle.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    private static boolean insideRun(Matcher matcher, int start, int end) {
        matcher.reset();
        while (matcher.find()) {
            if (ReportBlobScan.atLeast(matcher, ReportBlobScan.WIDGET_MINIMUM)
                    && matcher.start() <= start
                    && matcher.end() >= end) {
                return true;
            }
            if (matcher.start() > end) {
                return false;
            }
        }
        return false;
    }

    private static boolean hasResultJson(Path resultsDirectory) throws IOException {
        if (resultsDirectory == null || !Files.isDirectory(resultsDirectory)) {
            return false;
        }
        try (var listing = Files.list(resultsDirectory)) {
            return listing.anyMatch(path -> {
                String name = path.getFileName().toString();
                return name.endsWith("-result.json") && Files.isRegularFile(path);
            });
        }
    }
}
