package com.shaft.gui.internal.image;

import java.util.List;

/**
 * SHAFT-owned contract for optional visual-processing implementations.
 *
 * <p>Implementations may use optional computer-vision libraries, while engine callers interact only with
 * SHAFT and JDK types. Providers are discovered through {@link java.util.ServiceLoader}.</p>
 */
public interface VisualProcessingProvider {
    /**
     * Finds a reference image within a page screenshot.
     *
     * @param referenceImagePath path to the reference image
     * @param currentPageScreenshot encoded current-page screenshot
     * @return the matched x/y coordinates, or an empty list when no match is found
     */
    List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot);

    /**
     * Loads any native libraries required by this provider.
     */
    void load();
}
