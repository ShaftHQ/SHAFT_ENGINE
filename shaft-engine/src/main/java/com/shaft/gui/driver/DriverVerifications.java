package com.shaft.gui.driver;

import com.shaft.validation.internal.NativeValidationsBuilder;
import org.openqa.selenium.By;

/**
 * Public contract for driver-level soft verifications.
 */
public interface DriverVerifications {
    BrowserAssertions browser();

    ElementAssertions element(By elementLocator);

    default ElementAssertions element(ShaftLocator elementLocator) {
        return element(elementLocator.toBy());
    }

    /**
     * Starts soft verifications against a lazily composed portable element target.
     *
     * @param elementTarget portable element target
     * @return element verifications facade
     */
    default ElementAssertions element(ElementTarget elementTarget) {
        return element(elementTarget.toBy());
    }

    NativeValidationsBuilder object(Object actual);

    /**
     * Starts soft verifications against focused values from the current mobile session.
     *
     * @return mobile-session verification starters
     */
    default MobileAssertions mobileValues() {
        throw new UnsupportedOperationException(
                "mobileValues is not supported by this driver validation implementation.");
    }
}
