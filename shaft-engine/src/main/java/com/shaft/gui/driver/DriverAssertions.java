package com.shaft.gui.driver;

import com.shaft.validation.internal.NativeValidationsBuilder;
import org.openqa.selenium.By;

/**
 * Public contract for driver-level hard assertions.
 */
public interface DriverAssertions {
    /**
     * Starts a hard assertion on the browser.
     */
    BrowserAssertions browser();

    /**
     * Starts a hard assertion on the located element.
     */
    ElementAssertions element(By elementLocator);

    /**
     * Starts a hard assertion on the element found by the SHAFT locator.
     */
    default ElementAssertions element(ShaftLocator elementLocator) {
        return element(elementLocator.toBy());
    }

    /**
     * Starts hard assertions against a lazily composed portable element target.
     *
     * @param elementTarget portable element target
     * @return element assertions facade
     */
    default ElementAssertions element(ElementTarget elementTarget) {
        return element(elementTarget.toBy());
    }

    /**
     * Starts a hard assertion on any object value.
     */
    NativeValidationsBuilder object(Object object);

    /**
     * Starts hard assertions against focused values from the current mobile session.
     *
     * @return mobile-session assertion starters
     */
    default MobileAssertions mobileValues() {
        throw new UnsupportedOperationException(
                "mobileValues is not supported by this driver validation implementation.");
    }
}
