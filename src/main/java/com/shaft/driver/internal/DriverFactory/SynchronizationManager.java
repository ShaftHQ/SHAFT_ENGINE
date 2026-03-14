package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.Browser;
import org.openqa.selenium.support.ui.FluentWait;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;

public class SynchronizationManager {
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds

    // Pre-built immutable exception lists to avoid re-allocation per FluentWait call
    private static final List<Class<? extends Exception>> BASE_EXCEPTIONS = List.of(
            ClassCastException.class,
            org.openqa.selenium.NoSuchElementException.class,
            org.openqa.selenium.StaleElementReferenceException.class,
            org.openqa.selenium.JavascriptException.class,
            org.openqa.selenium.ElementClickInterceptedException.class,
            MultipleElementsFoundException.class,
            ExecutionException.class,
            InterruptedException.class
    );

    private static final List<Class<? extends Exception>> VISIBILITY_EXCEPTIONS = List.of(
            org.openqa.selenium.ElementNotInteractableException.class,
            org.openqa.selenium.InvalidElementStateException.class,
            org.openqa.selenium.interactions.MoveTargetOutOfBoundsException.class
    );

    private final WebDriver driver;
    private final boolean isSafari;

    public SynchronizationManager(WebDriver driver) {
        this.driver = driver;
        this.isSafari = SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName());
    }

    public FluentWait<WebDriver> fluentWait() {
        return fluentWait(false);
    }

    public FluentWait<WebDriver> fluentWait(boolean isValidToCheckForVisibility) {
        return new FluentWait<>(driver)
                .withTimeout(Duration.ofSeconds((long) (SHAFT.Properties.timeouts.defaultElementIdentificationTimeout())))
                .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                .ignoreAll(getExpectedExceptions(isValidToCheckForVisibility));
    }

    private List<Class<? extends Exception>> getExpectedExceptions(boolean isValidToCheckForVisibility) {
        // Fast path: no visibility check and no Safari — return cached base list directly
        if (!isValidToCheckForVisibility && !isSafari) {
            return BASE_EXCEPTIONS;
        }

        // Build a composite list only when extra exceptions are needed
        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>(BASE_EXCEPTIONS);

        if (isValidToCheckForVisibility) {
            expectedExceptions.addAll(VISIBILITY_EXCEPTIONS);
        }

        if (isSafari) {
            // the generic exception is added to handle a case with WebKit whereby the browser doesn't state the cause of the issue
            expectedExceptions.add(org.openqa.selenium.WebDriverException.class);
        }

        return expectedExceptions;
    }
}
