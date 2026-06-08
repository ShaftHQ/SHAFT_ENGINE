package com.shaft.gui.internal.image;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

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
     * Compares an element screenshot against its visual baseline using the requested optional engine.
     *
     * @param driver active WebDriver instance
     * @param elementLocator locator of the element being compared
     * @param elementScreenshot encoded screenshot of the element
     * @param visualValidationEngine requested visual validation engine
     * @param referenceImagePath path to the saved reference image
     * @param differencesImagePath path prefix for a generated differences image
     * @return {@code true} when the comparison passes or creates a new baseline
     */
    Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                   ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                   String referenceImagePath, String differencesImagePath);

    /**
     * Loads any native libraries required by this provider.
     */
    void load();
}
