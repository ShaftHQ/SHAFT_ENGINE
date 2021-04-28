package com.shaft.gui.element;

import java.time.Duration;
import java.util.ArrayList;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.shaft.gui.driver.DriverFactory;
import com.shaft.tools.io.ReportManagerHelper;

class ElementActionsHelper {
    private static final int DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER = Integer
            .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim());
    private static final int ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 1; // seconds
    private static final boolean FORCE_CHECK_FOR_ELEMENT_VISIBILITY = Boolean
            .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());

    private ElementActionsHelper() {
        throw new IllegalStateException("Utility class");
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator) {
        return waitForElementPresence(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return waitForElementPresence(driver, elementLocator, numberOfAttempts, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        return waitForElementPresence(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION, checkForVisibility);
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts, boolean checkForVisibility) {
        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
        expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
        expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);

        try {
            return new FluentWait<>(driver)
                    .withTimeout(Duration.ofSeconds(
                            (long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * numberOfAttempts))
                    .pollingEvery(Duration.ofSeconds(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                    .ignoreAll(expectedExceptions)
                    .until(nestedDriver -> {
                        nestedDriver.findElement(elementLocator);
                        return nestedDriver.findElements(elementLocator).size();
                    });
        } catch (org.openqa.selenium.TimeoutException e) {
            // In case the element was not found and the timeout expired
            // ReportManagerHelper.logDiscrete(e);
            return 0;
        }
    }

    protected static boolean waitForElementToBeVisible(WebDriver driver, By elementLocator) {
        if (FORCE_CHECK_FOR_ELEMENT_VISIBILITY && !DriverFactory.isMobileNativeExecution()) {
            ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
            expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
            expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);
            expectedExceptions.add(org.openqa.selenium.ElementNotVisibleException.class);
            expectedExceptions.add(org.openqa.selenium.WebDriverException.class);
            // UnsupportedCommandException getElementLocationOnceScrolledIntoView
            // TODO: appium -> swipe element into view

            try {
                new FluentWait<>(driver)
                        .withTimeout(Duration.ofSeconds(
                                (long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION))
                        .pollingEvery(Duration.ofSeconds(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                        .ignoreAll(expectedExceptions)
                        .until(nestedDriver -> {
                            ((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort();
                            return true;
                        });
            } catch (org.openqa.selenium.TimeoutException e) {
                // In case the element was not visible and the timeout expired
                ReportManagerHelper.logDiscrete(e);
            }
            if (Boolean.FALSE.equals(driver.findElement(elementLocator).isDisplayed())) {
                try {
                    new WebDriverWait(driver, (long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION).until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
                } catch (org.openqa.selenium.TimeoutException e) {
                    ReportManagerHelper.logDiscrete(e);
                    return false;
                }
            }
        }
        return true;
    }

    protected static boolean waitForElementToBeClickable(WebDriver driver, By elementLocator) {
        if (!DriverFactory.isMobileNativeExecution()) {
            try {
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                        .until(ExpectedConditions.elementToBeClickable(elementLocator));
            } catch (org.openqa.selenium.TimeoutException e) {
                ReportManagerHelper.logDiscrete(e);
                return false;
            }
        }
        return true;
    }

    protected static boolean waitForElementTextToBeNot(WebDriver driver, By elementLocator, String textShouldNotBe) {
        try {
            (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                    .until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, textShouldNotBe)));
        } catch (org.openqa.selenium.TimeoutException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
        return true;
    }
}
