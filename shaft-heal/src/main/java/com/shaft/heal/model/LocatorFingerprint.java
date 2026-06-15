package com.shaft.heal.model;

import java.util.Map;
import java.util.Objects;

/**
 * Privacy-minimized semantic fingerprint for a verified web or native element.
 *
 * @param schemaVersion fingerprint schema version
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
 * @param domPathChecksum checksum of the structural DOM path
 * @param platform evidence platform
 * @param nativeAttributes permitted native accessibility attributes
 * @param bounds bounded element rectangle
 * @param displayed whether the element was displayed
 * @param enabled whether the element was enabled
 * @param selected whether the element was selected
 * @param ancestorChecksum checksum of stable bounded ancestor evidence
 */
public record LocatorFingerprint(
        String schemaVersion,
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
        String domPathChecksum,
        HealingPlatform platform,
        Map<String, String> nativeAttributes,
        String bounds,
        boolean displayed,
        boolean enabled,
        boolean selected,
        String ancestorChecksum) {
    public static final String CURRENT_SCHEMA_VERSION = "2.0";

    /**
     * Creates a backward-compatible web fingerprint.
     */
    public LocatorFingerprint(
            String schemaVersion,
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
            String domPathChecksum) {
        this(schemaVersion, tagName, accessibleName, associatedLabel, visibleText, id, name,
                role, type, placeholder, title, testIds, semanticAttributes, domPathChecksum,
                HealingPlatform.WEB, Map.of(), "", true, true, false, "");
    }

    /**
     * Creates an immutable fingerprint.
     */
    public LocatorFingerprint {
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
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
        domPathChecksum = safe(domPathChecksum);
        platform = platform == null ? HealingPlatform.WEB : platform;
        nativeAttributes = nativeAttributes == null ? Map.of() : Map.copyOf(nativeAttributes);
        bounds = safe(bounds);
        ancestorChecksum = safe(ancestorChecksum);
    }

    private static String safe(String value) {
        return Objects.requireNonNullElse(value, "");
    }
}
