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
        byte[] pending = new byte[0];
        try (InputStream input = Files.newInputStream(widgetFile)) {
            byte[] window = new byte[WINDOW];
            while (true) {
                int read = input.read(window);
                if (read < 0) {
                    break;
                }
                byte[] chunk = new byte[pending.length + read];
                System.arraycopy(pending, 0, chunk, 0, pending.length);
                System.arraycopy(window, 0, chunk, pending.length, read);
                if (containsSuite(chunk, suiteName)) {
                    found = true;
                }
                int carry = Math.min(chunk.length, CARRY);
                pending = new byte[carry];
                System.arraycopy(chunk, chunk.length - carry, pending, 0, carry);
            }
        }
        return found ? 1 : 0;
    }

    private static boolean containsSuite(byte[] chunk, String suiteName) {
        String text = new String(chunk, StandardCharsets.ISO_8859_1);
        if (outsideBase64(text, suiteName)) {
            return true;
        }
        Matcher matcher = ReportBlobScan.tokens(text);
        while (matcher.find()) {
            int length = matcher.end() - matcher.start();
            if (!ReportBlobScan.atLeast(matcher, ReportBlobScan.WIDGET_MINIMUM) || length > WINDOW) {
                continue;
            }
            try {
                String decoded = new String(Base64.getDecoder().decode(matcher.group()), StandardCharsets.UTF_8);
                if (decoded.contains(suiteName)) {
                    return true;
                }
            } catch (IllegalArgumentException ignored) {
                // not a widget payload
            }
        }
        return false;
    }

    private static boolean outsideBase64(String text, String suiteName) {
        int from = 0;
        while (from <= text.length() - suiteName.length()) {
            int at = text.indexOf(suiteName, from);
            if (at < 0) {
                return false;
            }
            if (!insideRun(text, at, at + suiteName.length())) {
                return true;
            }
            from = at + 1;
        }
        return false;
    }

    private static boolean insideRun(String text, int start, int end) {
        Matcher matcher = ReportBlobScan.tokens(text);
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
