package com.shaft.heal.model;

import java.util.Map;
import java.util.Objects;

/**
 * Privacy-minimized semantic and DOM fingerprint for a verified web element.
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
        String domPathChecksum) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

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
    }

    private static String safe(String value) {
        return Objects.requireNonNullElse(value, "");
    }
}
