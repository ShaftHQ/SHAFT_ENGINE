package com.shaft.gui.driver;

import com.shaft.gui.capabilities.AutomationFeature;
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

    default NativeValidationsBuilder pageSourceValue() {
        throw new UnsupportedOperationException("pageSourceValue is not supported by this browser assertions implementation.");
    }

    default NativeValidationsBuilder windowHandleValue() {
        throw new UnsupportedOperationException("windowHandleValue is not supported by this browser assertions implementation.");
    }

    default NativeValidationsBuilder windowPositionValue() {
        throw new UnsupportedOperationException("windowPositionValue is not supported by this browser assertions implementation.");
    }

    default NativeValidationsBuilder windowSizeValue() {
        throw new UnsupportedOperationException("windowSizeValue is not supported by this browser assertions implementation.");
    }

    /**
     * Starts an assertion against the number of open browsing contexts in the current session.
     *
     * @return a native validation builder for browsing-context count comparisons
     */
    default NativeValidationsBuilder browsingContextCountValue() {
        throw new UnsupportedOperationException(
                "browsingContextCountValue is not supported by this browser assertions implementation.");
    }

    /** Starts an assertion against whether a current or observed browser dialog is present. */
    default NativeValidationsBuilder dialogPresentValue() {
        throw unsupported("dialogPresentValue");
    }

    /** Starts an assertion against the number of completed or observed downloads. */
    default NativeValidationsBuilder downloadCountValue() {
        throw unsupported("downloadCountValue");
    }

    /** Starts an assertion against the number of retained browser-network observations. */
    default NativeValidationsBuilder networkObservationCountValue() {
        throw unsupported("networkObservationCountValue");
    }

    /** Starts an assertion against the number of retained browser-console messages. */
    default NativeValidationsBuilder consoleMessageCountValue() {
        throw unsupported("consoleMessageCountValue");
    }

    /** Starts an assertion against the number of retained browser-console errors. */
    default NativeValidationsBuilder consoleErrorCountValue() {
        throw unsupported("consoleErrorCountValue");
    }

    /**
     * Starts an assertion against support proven for one feature by the current live session.
     *
     * @param feature stable feature identifier
     */
    default NativeValidationsBuilder featureSupportedValue(AutomationFeature feature) {
        throw unsupported("featureSupportedValue");
    }

    private static UnsupportedOperationException unsupported(String operation) {
        return new UnsupportedOperationException(
                operation + " is not supported by this browser assertions implementation.");
    }

    /**
     * Asserts that the current page matches its baseline full-page screenshot. Executes immediately,
     * like every other assertion.
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
