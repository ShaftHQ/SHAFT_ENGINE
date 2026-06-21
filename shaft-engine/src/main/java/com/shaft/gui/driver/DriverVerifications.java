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

    NativeValidationsBuilder object(Object actual);
}
