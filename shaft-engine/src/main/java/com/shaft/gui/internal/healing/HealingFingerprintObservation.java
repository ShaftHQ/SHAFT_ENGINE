package com.shaft.gui.internal.healing;

import org.openqa.selenium.By;

import java.util.Objects;

/**
 * Recorded element evidence supplied to an optional provider without a live driver or element
 * (issue #4161) -- e.g. generation-time seeding from shaft-capture's already-computed per-element
 * DOM signal, so recorded fingerprint history exists before a generated test is ever replayed.
 *
 * @param pageUrl recorded page URL where the element was captured
 * @param originalLocator locator the generated test will resolve at replay/execution time
 * @param action current action name
 * @param fingerprint recorded element evidence
 */
public record HealingFingerprintObservation(
        String pageUrl,
        By originalLocator,
        String action,
        HealingFingerprintSeed fingerprint) {
    /**
     * Creates a validated observation.
     */
    public HealingFingerprintObservation {
        pageUrl = Objects.requireNonNullElse(pageUrl, "");
        originalLocator = Objects.requireNonNull(originalLocator, "originalLocator");
        action = Objects.requireNonNullElse(action, "ELEMENT_RESOLUTION");
        fingerprint = Objects.requireNonNull(fingerprint, "fingerprint");
    }
}
