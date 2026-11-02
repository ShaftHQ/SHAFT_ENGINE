package com.shaft.gui.capabilities;

import java.util.Objects;

/**
 * Immutable support description for one automation feature.
 *
 * @param feature feature being described
 * @param support effective support level
 * @param detail implementation or unsupported reason
 * @param alternative suggested alternative when unsupported
 */
public record AutomationCapability(
        AutomationFeature feature,
        CapabilitySupport support,
        String detail,
        String alternative) {

    public AutomationCapability {
        Objects.requireNonNull(feature, "feature");
        Objects.requireNonNull(support, "support");
        detail = normalize(detail, "No support detail was provided.");
        alternative = normalize(alternative, "Use getNativeDriver() for a backend-specific extension.");
    }

    private static String normalize(String value, String fallback) {
        return value == null || value.isBlank() ? fallback : value;
    }
}
