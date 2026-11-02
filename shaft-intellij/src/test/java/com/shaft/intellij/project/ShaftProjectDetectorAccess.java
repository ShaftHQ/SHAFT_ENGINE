package com.shaft.intellij.project;

/**
 * Test bridge to clear {@link ShaftProjectDetector}'s per-root cache from other packages.
 */
public final class ShaftProjectDetectorAccess {
    private ShaftProjectDetectorAccess() {
    }

    /**
     * Clears cached detection results so a light project reused across test classes is re-detected.
     */
    public static void clearCache() {
        ShaftProjectDetector.clearCacheForTests();
    }
}
