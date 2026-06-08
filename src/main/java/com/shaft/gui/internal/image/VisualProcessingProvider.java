package com.shaft.gui.internal.image;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.List;

/**
 * Contract for optional visual-processing behavior that can be supplied by a SHAFT visual provider.
 * <p>
 * Implementations must keep their public descriptors limited to SHAFT-owned, JDK, and retained Selenium types so
 * core screenshot and assertion classes can load when third-party visual libraries are absent.
 */
public interface VisualProcessingProvider {
    /**
     * Returns a deterministic provider priority. Higher values are preferred.
     *
     * @return the provider priority
     */
    default int priority() {
        return 0;
    }

    /**
     * Reports whether this provider can execute in the current classpath and runtime environment.
     *
     * @return {@code true} when the provider is available
     */
    default boolean isAvailable() {
        return true;
    }

    /**
     * Initializes optional visual dependencies used by this provider.
     */
    default void initialize() {
        // default providers may not need eager initialization
    }

    /**
     * Finds a reference image inside the supplied screenshot bytes.
     *
     * @param referenceImagePath the path to the reference image
     * @param currentPageScreenshot the screenshot bytes to search
     * @return the detected center coordinates, or an empty list when no match is found
     */
    List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot);

    /**
     * Compares an element screenshot against its configured visual baseline.
     *
     * @param driver the active Selenium driver
     * @param elementLocator the element locator used to derive baseline identity
     * @param elementScreenshot the element screenshot bytes
     * @param visualValidationEngine the requested validation engine
     * @return {@code true} when the comparison passes
     */
    Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                   ImageProcessingActions.VisualValidationEngine visualValidationEngine);
}
