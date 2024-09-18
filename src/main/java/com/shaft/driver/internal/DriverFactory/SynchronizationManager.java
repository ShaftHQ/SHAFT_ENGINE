package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.Browser;
import org.openqa.selenium.support.ui.FluentWait;

import java.time.Duration;
import java.util.ArrayList;
import java.util.concurrent.ExecutionException;

public class SynchronizationManager {
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds
    private final WebDriver driver;

    public SynchronizationManager(WebDriver driver) {
        this.driver = driver;
    }

    public FluentWait<?> fluentWait() {
        return fluentWait(false);
    }

    public FluentWait<?> fluentWait(boolean isValidToCheckForVisibility) {
        return new FluentWait<>(driver)
                .withTimeout(Duration.ofSeconds((long) (SHAFT.Properties.timeouts.defaultElementIdentificationTimeout())))
                .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                .ignoreAll(getExpectedExceptions(isValidToCheckForVisibility));
    }

    private ArrayList<Class<? extends Exception>> getExpectedExceptions(boolean isValidToCheckForVisibility) {
        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
        expectedExceptions.add(java.lang.ClassCastException.class);
        expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
        expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);
        expectedExceptions.add(org.openqa.selenium.JavascriptException.class);
        expectedExceptions.add(org.openqa.selenium.ElementClickInterceptedException.class);

        if (isValidToCheckForVisibility) {
            expectedExceptions.add(org.openqa.selenium.ElementNotInteractableException.class);
            expectedExceptions.add(org.openqa.selenium.InvalidElementStateException.class);
            expectedExceptions.add(org.openqa.selenium.interactions.MoveTargetOutOfBoundsException.class);
        }

        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            // the generic exception is added to handle a case with WebKit whereby the browser doesn't state the cause of the issue
            expectedExceptions.add(org.openqa.selenium.WebDriverException.class);
        }

        // to handle failure inside a virtual thread
        expectedExceptions.add(ExecutionException.class);
        expectedExceptions.add(InterruptedException.class);
//        expectedExceptions.add(RuntimeException.class);

        return expectedExceptions;
    }
}
