package com.shaft.cli.internal;

import java.util.regex.PatternSyntaxException;

/**
 * Applies optional regex-based redaction before attaching SSH transcripts to reports.
 */
public final class SshOutputRedactor {
    private SshOutputRedactor() {
    }

    public static String apply(String text, String javaRegexReplacement) {
        if (text == null) {
            return null;
        }
        if (javaRegexReplacement == null || javaRegexReplacement.isBlank()) {
            return text;
        }
        try {
            return text.replaceAll(javaRegexReplacement, "[REDACTED]");
        } catch (PatternSyntaxException ex) {
            return text;
        }
    }
}
