package com.shaft.heal.internal;

import java.util.Objects;
import java.util.regex.Pattern;

final class PrivacySanitizer {
    private static final int MAX_LENGTH = 256;
    private static final Pattern BEARER = Pattern.compile("(?i)bearer\\s+[a-z0-9._~+/-]+=*");
    private static final Pattern JWT = Pattern.compile("\\beyJ[a-zA-Z0-9_-]{8,}\\.[a-zA-Z0-9_-]{8,}\\.[a-zA-Z0-9_-]{8,}\\b");
    private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
            "(?i)(password|passwd|secret|token|api[-_]?key|authorization|cookie)\\s*[:=]\\s*[^\\s,;]+");
    private static final Pattern LONG_TOKEN = Pattern.compile("\\b[a-zA-Z0-9_-]{32,}\\b");

    private PrivacySanitizer() {
        throw new IllegalStateException("Utility class");
    }

    static SanitizedValue sanitize(String value) {
        String sanitized = Objects.requireNonNullElse(value, "");
        int redacted = 0;
        for (Pattern pattern : new Pattern[]{BEARER, JWT, SECRET_ASSIGNMENT, LONG_TOKEN}) {
            var matcher = pattern.matcher(sanitized);
            if (matcher.find()) {
                redacted++;
                sanitized = matcher.replaceAll("[REDACTED]");
            }
        }
        sanitized = sanitized.replaceAll("[\\p{Cntrl}&&[^\\r\\n\\t]]", "").trim();
        if (sanitized.length() > MAX_LENGTH) {
            sanitized = sanitized.substring(0, MAX_LENGTH);
        }
        return new SanitizedValue(sanitized, redacted);
    }

    record SanitizedValue(String value, int redactedCount) {
    }
}
