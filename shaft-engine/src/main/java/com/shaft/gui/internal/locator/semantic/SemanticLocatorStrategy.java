package com.shaft.gui.internal.locator.semantic;

/**
 * Locator strategy ordered by user-perceived semantics (issue #5457 FR-1).
 * Lower {@link #ordinal()} = higher precedence. Structural strategies are the
 * deterministic fallback when semantic signals are absent (FR-4).
 */
public enum SemanticLocatorStrategy {
    /** ARIA role paired with accessible name. */
    ROLE,
    /** Computed accessible name alone. */
    ACCESSIBLE_NAME,
    /** Associated {@code <label>} (or equivalent) text. */
    LABEL,
    /** Visible normalized text. */
    TEXT,
    /** Explicit test contract ({@code data-testid}, {@code data-test}, {@code data-qa}). */
    TEST_ID,
    /** Authored element id (structural contract, still preferred over CSS/XPath). */
    ID,
    /** Element {@code name} attribute. */
    NAME,
    /** CSS selector fallback. */
    CSS,
    /** XPath fallback (last resort). */
    XPATH;

    /**
     * Whether this strategy is user-perceived / test-contract semantics rather than structure.
     */
    public boolean isSemantic() {
        return ordinal() <= TEST_ID.ordinal();
    }

    /**
     * Base confidence when the candidate is unique (FR-3).
     */
    public double baseConfidence() {
        return switch (this) {
            case ROLE -> 1.0;
            case ACCESSIBLE_NAME -> 0.92;
            case LABEL -> 0.88;
            case TEXT -> 0.82;
            case TEST_ID -> 0.80;
            case ID -> 0.55;
            case NAME -> 0.45;
            case CSS -> 0.35;
            case XPATH -> 0.25;
        };
    }
}
