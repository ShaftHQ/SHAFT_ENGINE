package com.shaft.gui.image;

/** Selects the visual matching strategy for an {@link ImageTarget}. */
public enum ImageMatchingMode {
    /** Chooses the strongest suitable strategy and may use a fallback. */
    AUTO,
    /** Uses multi-scale template matching. */
    TEMPLATE,
    /** Uses local feature matching and geometric verification. */
    FEATURE
}
