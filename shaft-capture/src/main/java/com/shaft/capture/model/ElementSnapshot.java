package com.shaft.capture.model;

import java.util.List;
import java.util.Map;

/**
 * Sanitized element evidence at the time of an action.
 *
 * @param logicalElementId stable logical element identifier
 * @param tagName element tag name
 * @param role accessible role
 * @param accessibleName sanitized accessible name
 * @param label sanitized associated label
 * @param normalizedAttributes sanitized normalized attributes
 * @param locatorCandidates deterministic locator evidence
 * @param visible whether the element was visible
 * @param enabled whether the element was enabled
 * @param selected whether the element was selected
 */
public record ElementSnapshot(
        String logicalElementId,
        String tagName,
        String role,
        String accessibleName,
        String label,
        Map<String, String> normalizedAttributes,
        List<LocatorCandidate> locatorCandidates,
        boolean visible,
        boolean enabled,
        boolean selected) {
    /**
     * Creates an immutable element snapshot with candidates in best-first order.
     */
    public ElementSnapshot {
        logicalElementId = ModelSupport.requireText(logicalElementId, "Logical element ID");
        tagName = ModelSupport.text(tagName).toLowerCase();
        role = ModelSupport.text(role).toLowerCase();
        accessibleName = ModelSupport.text(accessibleName);
        label = ModelSupport.text(label);
        normalizedAttributes = ModelSupport.strings(normalizedAttributes);
        locatorCandidates = locatorCandidates == null
                ? List.of()
                : locatorCandidates.stream().sorted(LocatorCandidate.BEST_FIRST).toList();
    }
}
