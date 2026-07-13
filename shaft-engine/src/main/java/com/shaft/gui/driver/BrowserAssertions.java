package com.shaft.gui.driver;

import com.shaft.validation.VisualComparisonOptions;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;

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
     * Asserts that the current page matches its baseline full-page screenshot. Executes immediately,
     * like every other assertion &mdash; no {@code perform()} is required.
     *
     * @return a ValidationsExecutor object to optionally set a custom validation message
     */
    default ValidationsExecutor matchesScreenshot() {
        throw new UnsupportedOperationException("matchesScreenshot is not supported by this browser assertions implementation.");
    }

    /**
     * Asserts that the current page matches its baseline full-page screenshot, using the given
     * diff-budget/mask options (see {@link VisualComparisonOptions}). Executes immediately.
     *
     * @param options the visual comparison options (diff budgets, masks), or {@code null} for defaults
     * @return a ValidationsExecutor object to optionally set a custom validation message
     */
    default ValidationsExecutor matchesScreenshot(VisualComparisonOptions options) {
        throw new UnsupportedOperationException("matchesScreenshot is not supported by this browser assertions implementation.");
    }
}
