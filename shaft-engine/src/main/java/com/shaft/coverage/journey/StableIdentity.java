package com.shaft.coverage.journey;

import java.util.Locale;
import java.util.Objects;
import java.util.regex.Pattern;

/**
 * Normalizes raw journey/view/interaction/state tokens into stable identities (FR-1, SC-2).
 *
 * <p>Retry markers, duplicate-view suffixes, volatile query strings, and identifier-looking path
 * segments are stripped or parameterized so repeated or mutated observations do not inflate
 * coverage.
 */
public final class StableIdentity {
    private static final Pattern RETRY_MARKERS = Pattern.compile(
            "(?i)([#._-]retry[-_]?\\d+|\\(retry\\s*\\d+\\)|\\[retry\\s*\\d+\\]|-attempt-\\d+)");
    private static final Pattern DUP_MARKERS = Pattern.compile(
            "(?i)([#._-](?:dup|copy|clone)[-_]?\\d+|\\[(?:dup|copy|clone)[-_\\s]*\\d+\\])");
    private static final Pattern UUID = Pattern.compile(
            "(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$");
    private static final Pattern NUMERIC = Pattern.compile("^\\d+$");
    private static final Pattern WHITESPACE = Pattern.compile("\\s+");

    private StableIdentity() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns a stable identity string for the supplied raw token.
     *
     * @param raw raw journey, view, interaction, or state token
     * @return normalized non-blank identity
     * @throws IllegalArgumentException when {@code raw} is null/blank after normalization
     */
    public static String normalize(String raw) {
        Objects.requireNonNull(raw, "raw");
        String value = raw.strip();
        value = stripQueryAndFragment(value);
        value = RETRY_MARKERS.matcher(value).replaceAll("");
        value = DUP_MARKERS.matcher(value).replaceAll("");
        value = parameterizePath(value);
        value = WHITESPACE.matcher(value).replaceAll(" ");
        value = value.strip().toLowerCase(Locale.ROOT);
        while (value.contains("//")) {
            value = value.replace("//", "/");
        }
        if (value.isBlank()) {
            throw new IllegalArgumentException("Identity must not be blank after normalization");
        }
        return value;
    }

    private static String stripQueryAndFragment(String value) {
        int query = value.indexOf('?');
        if (query >= 0) {
            value = value.substring(0, query);
        }
        int hash = value.indexOf('#');
        if (hash >= 0) {
            value = value.substring(0, hash);
        }
        return value;
    }

    private static String parameterizePath(String value) {
        if (!value.contains("/")) {
            return value;
        }
        StringBuilder out = new StringBuilder();
        String[] parts = value.split("/", -1);
        for (int i = 0; i < parts.length; i++) {
            if (i > 0) {
                out.append('/');
            }
            String part = parts[i];
            if (part.isEmpty()) {
                continue;
            }
            if (NUMERIC.matcher(part).matches() || UUID.matcher(part).matches()) {
                out.append("{id}");
            } else {
                out.append(part);
            }
        }
        return out.toString();
    }
}
