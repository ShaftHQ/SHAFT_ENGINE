package com.shaft.gui.driver;

import com.shaft.validation.internal.NativeValidationsBuilder;

/**
 * Public contract for browser-level hard/soft validation starters.
 */
public interface BrowserAssertions {
    NativeValidationsBuilder attribute(String browserAttribute);

    NativeValidationsBuilder url();

    NativeValidationsBuilder title();

    NativeValidationsBuilder text();
}
