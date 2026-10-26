package com.shaft.gui.element.internal.interaction;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * Bounded dismissal of a transient Android overlay, including the system
 * "app keeps stopping" button {@code android:id/aerr_close} (#7155, #7083).
 * A button that vanishes between the look and the tap counts as closed.
 */
public final class AndroidTransientOverlay {

    public static final int MAX_DISMISSALS = 3;
    public static final By APP_CRASH_CLOSE = By.id("android:id/aerr_close");

    private AndroidTransientOverlay() {
    }

    /**
     * Closes one crash-dialog button, or returns 0 when none is showing.
     */
    public static int dismissOnce(WebDriver driver) {
        if (!(driver instanceof AppiumDriver)) {
            return 0;
        }
        return dismissOnce(() -> driver.findElements(APP_CRASH_CLOSE));
    }

    static int dismissOnce(Supplier<List<WebElement>> finder) {
        List<WebElement> found;
        try {
            found = finder.get();
        } catch (RuntimeException ignored) {
            return 0;
        }
        if (found == null || found.isEmpty()) {
            return 0;
        }
        try {
            found.get(0).click();
        } catch (RuntimeException vanished) {
            // The dialog left between the look and the tap. That is success.
        }
        return 1;
    }

    /**
     * Looks again after each dismissal, at most {@link #MAX_DISMISSALS} times.
     * Returns the first present result.
     */
    public static <T> Optional<T> recheck(Supplier<Optional<T>> ready, Supplier<List<WebElement>> finder) {
        Optional<T> found = ready.get();
        int attempt = 0;
        while (found.isEmpty() && attempt < MAX_DISMISSALS && dismissOnce(finder) > 0) {
            attempt++;
            found = ready.get();
        }
        return found;
    }
}
