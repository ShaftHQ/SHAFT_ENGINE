package com.shaft.doctor.internal;

import com.shaft.doctor.model.Diagnosis;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Bounded diagnosis text and SECRET_LIKE / FAILED_LOCATOR sanitizer for provider prompts.
 */
public final class DoctorProviderDiagnosis {
    /**
     * Maximum ranked causes included in a provider diagnosis payload.
     */
    public static final int MAX_RANKED_CAUSES = 5;

    private static final Pattern SECRET_LIKE = Pattern.compile(
            "(?i)([\"']?authorization[\"']?\\s*[:=]\\s*(?:(?:basic|bearer)\\s+)?(?:\"[^\"]+\"|'[^']+'|[^\\s,;]+)"
                    + "|[\"']?bearer[\"']?\\s+[a-z0-9._\\-]{8,}"
                    + "|[\"']?api[_-]?key[\"']?\\s*[:=]\\s*(?:\"[^\"]+\"|'[^']+'|[^\\s,;]+)"
                    + "|sk-[a-z0-9._\\-]{8,})");
    private static final Pattern[] SPECIFIC_LOCATORS = {
            Pattern.compile("(?i)getBy(?:Role|TestId|Text|Label|Placeholder|AltText|Title)\\s*\\([^\\r\\n]+?\\)"),
            Pattern.compile("(?i)SHAFT\\.GUI\\.Locator\\.[A-Za-z]+\\s*\\([^\\r\\n]+?\\)"),
            Pattern.compile("(?i)\\{\\s*[\"']using[\"']\\s*:\\s*[\"'][^\"']+[\"']\\s*,\\s*[\"']value[\"']\\s*:\\s*[\"'][^\"']+[\"']\\s*\\}")
    };
    private static final Pattern FAILED_LOCATOR = Pattern.compile(
            "(?i)(By\\.(?:id|name|cssSelector|xpath|className|tagName|linkText|partialLinkText)\\s*:?\\s*[^\\r\\n,;]+"
                    + "|locator\\([^\\r\\n]+?\\)"
                    + "|[\"']?selector[\"']?\\s*[:=]\\s*(?:\"[^\"]+\"|'[^']+'|[^\\r\\n,;]+))");

    private DoctorProviderDiagnosis() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Builds the bounded diagnosis block sent to a provider.
     *
     * @param diagnosis deterministic diagnosis
     * @return primary cause, confidence, sanitized summary, and ranked category:trust pairs
     */
    public static String boundedText(Diagnosis diagnosis) {
        String ranked = diagnosis.rankedCauses().stream()
                .limit(MAX_RANKED_CAUSES)
                .map(cause -> cause.category() + ":" + cause.trustPercentage())
                .reduce((left, right) -> left + "," + right)
                .orElse("");
        return """
                primaryCause=%s
                confidence=%s
                summary=%s
                rankedCauses=%s
                """.formatted(
                diagnosis.primaryCause(),
                diagnosis.confidence(),
                sanitize(diagnosis.summary()),
                ranked);
    }

    /**
     * Redacts secret-like values and locator dialects from provider-bound text.
     *
     * @param content raw text, may be {@code null}
     * @return sanitized text, never {@code null}
     */
    public static String sanitize(String content) {
        if (content == null) {
            return "";
        }
        String sanitized = SECRET_LIKE.matcher(content).replaceAll("[REDACTED]");
        for (Pattern pattern : SPECIFIC_LOCATORS) {
            sanitized = pattern.matcher(sanitized).replaceAll("[LOCATOR]");
        }
        return FAILED_LOCATOR.matcher(sanitized).replaceAll("[LOCATOR]");
    }

    /**
     * Reports whether the text contains a secret-like token.
     *
     * @param content text to inspect
     * @return {@code true} when a secret-like match is present
     */
    public static boolean containsSecret(String content) {
        return content != null && SECRET_LIKE.matcher(content).find();
    }

    /**
     * Extracts the first failed-locator match from evidence text.
     *
     * @param text evidence text
     * @return collapsed locator match, or empty when none is present
     */
    public static String firstFailedLocator(String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        Matcher earliestSpecific = null;
        int specificStart = Integer.MAX_VALUE;
        for (Pattern pattern : SPECIFIC_LOCATORS) {
            Matcher matcher = pattern.matcher(text);
            if (matcher.find() && matcher.start() < specificStart) {
                earliestSpecific = matcher;
                specificStart = matcher.start();
            }
        }
        Matcher failed = FAILED_LOCATOR.matcher(text);
        boolean failedFound = failed.find();
        int failedStart = failedFound ? failed.start() : Integer.MAX_VALUE;
        int failedEnd = failedFound ? failed.end() : -1;
        // Prefer a specific dialect that starts inside a greedy By.* span so same-line
        // By.cssSelector: ... getByRole(...) does not return truncated getByRole("button".
        boolean specificSwallowedByFailed = failedFound && earliestSpecific != null
                && failedStart < specificStart && specificStart < failedEnd;
        if (earliestSpecific != null
                && (specificStart <= failedStart || specificSwallowedByFailed)) {
            return earliestSpecific.group().replaceAll("\\s+", " ").trim();
        }
        if (failedFound) {
            return failed.group(1).replaceAll("\\s+", " ").trim();
        }
        return "";
    }
}
