package com.shaft.gui.internal.healing;

import java.util.Map;
import java.util.Objects;

/**
 * Plain-data element evidence for seeding SHAFT Heal history without a live driver or element
 * (issue #4161). shaft-engine must not depend on shaft-heal, so this mirrors shaft-heal's
 * {@code LocatorFingerprint} field-for-field for the web platform; an optional provider converts
 * it into a real fingerprint and persists it.
 *
 * @param tagName element tag name
 * @param accessibleName computed accessible name
 * @param associatedLabel associated label text
 * @param visibleText bounded visible text
 * @param id stable ID
 * @param name stable name
 * @param role accessible role
 * @param type semantic type
 * @param placeholder placeholder text
 * @param title title text
 * @param testIds configured test ID attributes
 * @param semanticAttributes other permitted semantic attributes
 * @param displayed whether the element was displayed when recorded
 * @param enabled whether the element was enabled when recorded
 * @param selected whether the element was selected when recorded
 */
public record HealingFingerprintSeed(
        String tagName,
        String accessibleName,
        String associatedLabel,
        String visibleText,
        String id,
        String name,
        String role,
        String type,
        String placeholder,
        String title,
        Map<String, String> testIds,
        Map<String, String> semanticAttributes,
        boolean displayed,
        boolean enabled,
        boolean selected) {
    /**
     * Creates an immutable seed with null-safe defaults.
     */
    public HealingFingerprintSeed {
        tagName = safe(tagName);
        accessibleName = safe(accessibleName);
        associatedLabel = safe(associatedLabel);
        visibleText = safe(visibleText);
        id = safe(id);
        name = safe(name);
        role = safe(role);
        type = safe(type);
        placeholder = safe(placeholder);
        title = safe(title);
        testIds = testIds == null ? Map.of() : Map.copyOf(testIds);
        semanticAttributes = semanticAttributes == null ? Map.of() : Map.copyOf(semanticAttributes);
    }

    private static String safe(String value) {
        return Objects.requireNonNullElse(value, "");
    }
}
