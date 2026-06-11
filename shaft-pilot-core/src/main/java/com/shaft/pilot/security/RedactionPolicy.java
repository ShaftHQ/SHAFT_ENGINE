package com.shaft.pilot.security;

import java.util.List;
import java.util.Set;

/**
 * Deterministic redaction rules.
 *
 * @param sensitiveAttributes case-insensitive structured or DOM attribute names
 * @param selectors CSS selectors whose values and text must be removed
 * @param customPatterns additional regular expressions
 */
public record RedactionPolicy(
        Set<String> sensitiveAttributes,
        List<String> selectors,
        List<String> customPatterns) {
    /**
     * Creates immutable redaction rules.
     */
    public RedactionPolicy {
        sensitiveAttributes = sensitiveAttributes == null
                ? Set.of()
                : sensitiveAttributes.stream().map(String::toLowerCase).collect(java.util.stream.Collectors.toUnmodifiableSet());
        selectors = selectors == null ? List.of() : List.copyOf(selectors);
        customPatterns = customPatterns == null ? List.of() : List.copyOf(customPatterns);
    }
}
