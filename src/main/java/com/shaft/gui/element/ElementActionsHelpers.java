package com.shaft.gui.element;

import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.*;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;
import java.util.ArrayList;

class ElementActionsHelpers {
    private static final int DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER = Integer
            .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim());
    private static final int ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 1; // seconds
    private static final boolean FORCE_CHECK_FOR_ELEMENT_VISIBILITY = Boolean
            .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());

    static int waitForElementPresence(WebDriver driver, By elementLocator) {
        return waitForElementPresence(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return waitForElementPresence(driver, elementLocator, numberOfAttempts, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    static int waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        return waitForElementPresence(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION, checkForVisibility);
    }

    static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts, boolean checkForVisibility) {
        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
        expectedExceptions.add(NoSuchElementException.class);
        expectedExceptions.add(StaleElementReferenceException.class);
        if (checkForVisibility) expectedExceptions.add(ElementNotVisibleException.class);

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
        } catch (TimeoutException e) {
            // In case the element was not found and the timeout expired
            ReportManager.logDiscrete(e);
            return 0;
        }
    }

    static boolean waitForElementToBeVisible(WebDriver driver, By elementLocator) {
        if (FORCE_CHECK_FOR_ELEMENT_VISIBILITY) {
            try {
                (new WebDriverWait(driver, (long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION))
                        .until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
            } catch (TimeoutException e) {
                ReportManager.logDiscrete(e);
                return false;
            }
        }
        return true;
    }

    static boolean waitForElementToBeClickable(WebDriver driver, By elementLocator) {
        try {
            (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                    .until(ExpectedConditions.elementToBeClickable(elementLocator));
        } catch (TimeoutException e) {
            ReportManager.logDiscrete(e);
            return false;
        }
        return true;
    }

    static boolean waitForElementTextToBeNot(WebDriver driver, By elementLocator, String textShouldNotBe) {
        try {
            (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                    .until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, textShouldNotBe)));
        } catch (TimeoutException e) {
            ReportManager.logDiscrete(e);
            return false;
        }
        return true;
    }
}
