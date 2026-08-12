package com.shaft.gui.driver;

import com.shaft.validation.internal.NativeValidationsBuilder;
import org.openqa.selenium.By;

/**
 * Public contract for driver-level hard assertions.
 */
public interface DriverAssertions {
    BrowserAssertions browser();

    ElementAssertions element(By elementLocator);

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

    NativeValidationsBuilder object(Object object);
}
