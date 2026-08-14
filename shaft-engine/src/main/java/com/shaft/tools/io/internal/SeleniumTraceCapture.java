package com.shaft.tools.io.internal;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chromium.HasCdp;

import java.util.function.UnaryOperator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Bounded, capability-derived Selenium snapshot capture for private trace evidence. */
final class SeleniumTraceCapture {
    private static final int MAX_SNAPSHOT_CHARACTERS = 200_000;
    private static final int SANITIZATION_OVERLAP_CHARACTERS = 1_024;
    private static final Pattern QUOTED_SECRET_START = Pattern.compile(
            "(?i)(?:password|passwd|pwd|secret|token|access[_-]?key|api[_-]?key)[\\\"']?\\s*[:=]\\s*([\\\"'])");
    private static final Pattern URL_CREDENTIAL_START = Pattern.compile("://[^:/\\s]+:[^@/\\s]*$");

    private SeleniumTraceCapture() {
        throw new IllegalStateException("Utility class");
    }

    static Result capture(WebDriver driver, UnaryOperator<String> sanitizer, boolean allowResourceComplete) {
        if (driver == null) {
            return unavailable("none", "No active browser driver was registered for this thread.");
        }
        String structural;
        try {
            structural = driver.getPageSource();
        } catch (RuntimeException ignored) {
            structural = null;
        }
        if (structural == null) {
            return unavailable("webdriver", "Snapshot capture failed.");
        }
        Result result = fromContent("webdriver", "structural", "webdriver-page-source", structural, sanitizer);
        if (allowResourceComplete && driver instanceof HasCdp && "available".equals(result.status())) {
            return new Result(result.provider(), result.fidelity(), result.status(),
                    "Resource-complete CDP capture was omitted because the provider has no enforceable response-size bound.",
                    result.type(), result.content(), result.truncated());
        }
        return result;
    }

    static Result fromContent(String provider, String fidelity, String type, String content,
                              UnaryOperator<String> sanitizer) {
        String raw = content == null ? "" : content;
        boolean providerTruncated = raw.length() > MAX_SNAPSHOT_CHARACTERS + SANITIZATION_OVERLAP_CHARACTERS;
        String boundedRaw = providerTruncated
                ? raw.substring(0, MAX_SNAPSHOT_CHARACTERS + SANITIZATION_OVERLAP_CHARACTERS)
                : raw;
        if (hasUnsafeSensitiveConstruct(boundedRaw, providerTruncated)) {
            return new Result(provider, "omitted", "omitted-sensitive-boundary",
                    "Snapshot crossed a sensitive field at the bounded capture limit.",
                    "omitted-sensitive-boundary", "", true);
        }
        String sanitized = sanitizer.apply(boundedRaw);
        boolean truncated = providerTruncated || sanitized.length() > MAX_SNAPSHOT_CHARACTERS;
        String bounded = sanitized.substring(0, Math.min(sanitized.length(), MAX_SNAPSHOT_CHARACTERS));
        if (bounded.isBlank()) {
            return unavailable(provider, "Snapshot provider returned no content.");
        }
        String effectiveFidelity = truncated ? "partial" : fidelity;
        String status = truncated ? "truncated" : "available";
        String reason = truncated ? "Snapshot exceeded the bounded trace capture limit." : "";
        return new Result(provider, effectiveFidelity, status, reason, type, bounded, truncated);
    }

    private static Result unavailable(String provider, String reason) {
        return new Result(provider, "unavailable", "unavailable", reason, "unavailable", "", false);
    }

    private static boolean hasUnsafeSensitiveConstruct(String boundedRaw, boolean providerTruncated) {
        String tail = boundedRaw;
        Matcher secret = QUOTED_SECRET_START.matcher(tail);
        while (secret.find()) {
            char delimiter = secret.group(1).charAt(0);
            int closing = firstUnescaped(tail, delimiter, secret.end());
            if (closing < 0 || hasEscapedDelimiter(tail, delimiter, secret.end(), closing)) {
                return true;
            }
        }
        return providerTruncated && URL_CREDENTIAL_START.matcher(tail).find();
    }

    private static int firstUnescaped(String value, char delimiter, int start) {
        for (int index = start; index < value.length(); index++) {
            if (value.charAt(index) != delimiter) {
                continue;
            }
            int backslashes = 0;
            for (int cursor = index - 1; cursor >= start && value.charAt(cursor) == '\\'; cursor--) {
                backslashes++;
            }
            if (backslashes % 2 == 0) {
                return index;
            }
        }
        return -1;
    }

    private static boolean hasEscapedDelimiter(String value, char delimiter, int start, int end) {
        for (int index = start; index < end; index++) {
            if (value.charAt(index) != delimiter) {
                continue;
            }
            int backslashes = 0;
            for (int cursor = index - 1; cursor >= start && value.charAt(cursor) == '\\'; cursor--) {
                backslashes++;
            }
            if (backslashes % 2 != 0) {
                return true;
            }
        }
        return false;
    }

    record Result(String provider, String fidelity, String status, String reason, String type, String content,
                  boolean truncated) {
    }
}
