package com.shaft.gui.driver;

import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.VisualValidationsBuilder;

/**
 * Public contract for browser-level hard/soft validation starters.
 */
public interface BrowserAssertions {
    NativeValidationsBuilder attribute(String browserAttribute);

    NativeValidationsBuilder url();

    NativeValidationsBuilder title();

    /**
     * Starts an assertion against the current browser alert, confirm, or prompt dialog text.
     *
     * @return a native validation builder for alert text comparisons
     */
    default NativeValidationsBuilder alertText() {
        throw new UnsupportedOperationException("alertText is not supported by this browser assertions implementation.");
    }

    NativeValidationsBuilder text();

    /**
     * Starts a visual-regression assertion against the current page's baseline full-page screenshot.
     *
     * @return a VisualValidationsBuilder to optionally set diff-budget/mask options and then perform() your validation
     */
    default VisualValidationsBuilder matchesScreenshot() {
        throw new UnsupportedOperationException("matchesScreenshot is not supported by this browser assertions implementation.");
    }
}
