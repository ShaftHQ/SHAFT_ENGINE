package com.shaft.capture.privacy;

import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Deterministic capture privacy configuration.
 *
 * @param sensitiveFieldNames logical field names that must never be persisted
 * @param sensitiveSelectors selectors that identify secret inputs
 * @param sensitiveAttributeNames attributes that must be removed
 * @param sensitiveUrlParameters URL parameters whose values must be removed
 * @param sensitiveValuePatterns regular expressions identifying secret values
 * @param externalDataPath relative JSON file used for ordinary captured values
 */
public record CapturePrivacyPolicy(
        Set<String> sensitiveFieldNames,
        List<String> sensitiveSelectors,
        Set<String> sensitiveAttributeNames,
        Set<String> sensitiveUrlParameters,
        List<String> sensitiveValuePatterns,
        String externalDataPath) {
    /**
     * Creates immutable normalized privacy rules.
     */
    public CapturePrivacyPolicy {
        sensitiveFieldNames = lower(sensitiveFieldNames);
        sensitiveSelectors = sensitiveSelectors == null
                ? List.of()
                : sensitiveSelectors.stream().map(String::trim).filter(value -> !value.isEmpty()).toList();
        sensitiveAttributeNames = lower(sensitiveAttributeNames);
        sensitiveUrlParameters = lower(sensitiveUrlParameters);
        sensitiveValuePatterns = sensitiveValuePatterns == null ? List.of() : List.copyOf(sensitiveValuePatterns);
        externalDataPath = normalizePath(externalDataPath);
    }

    /**
     * Returns conservative browser-capture defaults.
     *
     * @return default privacy policy
     */
    public static CapturePrivacyPolicy defaults() {
        return new CapturePrivacyPolicy(
                Set.of("password", "passwd", "secret", "token", "api-key", "apikey",
                        "authorization", "cookie", "credit-card", "cvv"),
                List.of("input[type=password]", "[autocomplete=current-password]",
                        "[autocomplete=new-password]"),
                Set.of("value", "password", "token", "secret", "authorization", "cookie"),
                Set.of("token", "access_token", "refresh_token", "api_key", "apikey",
                        "password", "secret", "authorization"),
                List.of(
                        "(?i)bearer\\s+[a-z0-9._~+/=-]+",
                        "\\beyJ[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\b",
                        "\\b(?:AKIA|ASIA)[A-Z0-9]{16}\\b",
                        "(?s)-----BEGIN [A-Z ]*PRIVATE KEY-----.*?-----END [A-Z ]*PRIVATE KEY-----"),
                "capture-data.json");
    }

    private static Set<String> lower(Set<String> values) {
        return values == null
                ? Set.of()
                : values.stream()
                .map(value -> value == null ? "" : value.trim().toLowerCase(Locale.ROOT))
                .filter(value -> !value.isEmpty())
                .collect(Collectors.toUnmodifiableSet());
    }

    private static String normalizePath(String value) {
        String normalized = value == null || value.isBlank() ? "capture-data.json" : value.trim().replace('\\', '/');
        if (java.nio.file.Path.of(normalized).isAbsolute() || normalized.startsWith("../")
                || normalized.contains("/../")) {
            throw new IllegalArgumentException("Capture external data path must be relative.");
        }
        return normalized;
    }
}
