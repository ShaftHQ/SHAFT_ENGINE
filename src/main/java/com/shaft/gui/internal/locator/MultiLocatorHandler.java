package com.shaft.gui.internal.locator;

import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Wait;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;
import java.util.List;

public class MultiLocatorHandler {
    private static int uniqueNumber = 1;

    /**
     * Get a unique By locator for an element within a list of multiple elements based on exact text match.
     *
     * @param multipleElementsLocator The locator for the list of elements.
     * @param visibleTextOfExactElement The exact text content to match.
     * @return A unique By locator for the specific element found, or null if not found.
     * in case multiple options found, returns the first one
     */
    public static By getLocatorByText(By multipleElementsLocator, String visibleTextOfExactElement) {
        waitAndHandleExceptions(multipleElementsLocator);

        List<WebElement> elements = DriverFactoryHelper.getDriver().get().findElements(multipleElementsLocator);
        for (WebElement element : elements) {
            if (element.getText().equals(visibleTextOfExactElement)) {
                String className = injectUniqueClassName(element);
                return By.className(className);
            }
        }
        ReportManager.logDiscrete("element with text \"" + visibleTextOfExactElement + "\" was not found");
        return null;
    }
    /**
     * Get a unique By locator for an element within a list of multiple elements based on partial text match.
     *
     * @param multipleElementsLocator The locator for the list of elements.
     * @param partialTextOfExactElement The partial text content to match.
     * @return A unique By locator for the specific element found, or null if not found.
     * in case multiple options found, returns the first one
     */
    public static By getLocatorContainingText(By multipleElementsLocator, String partialTextOfExactElement) {
        waitAndHandleExceptions(multipleElementsLocator);

        List<WebElement> elements = DriverFactoryHelper.getDriver().get().findElements(multipleElementsLocator);
        for (WebElement element : elements) {
            if (element.getText().contains(partialTextOfExactElement)) {
                String className = injectUniqueClassName(element);
                return By.className(className);
            }
        }
        ReportManager.logDiscrete("element containing \"" + partialTextOfExactElement + "\" was not found");
        return null;
    }
    /**
     * Get a unique By locator for an element within a list of multiple elements based on its index.
     * starting from 0
     * @param multipleElementsLocator The locator for the list of elements.
     * @param index The index of the element to retrieve.
     * @return A unique By locator for the element at the specified index, or null if not found.
     */
    public static By getLocatorByIndex(By multipleElementsLocator, int index) {
        waitAndHandleExceptions(multipleElementsLocator);

        List<WebElement> elements = DriverFactoryHelper.getDriver().get().findElements(multipleElementsLocator);
        if (elements.size() <= index) {
            ReportManager.logDiscrete("element with locator " + multipleElementsLocator
                    + " with index '" + index + "' was not found");
            return null;
        }

        String className = injectUniqueClassName(elements.get(index));
        return By.className(className);
    }
    private static void waitAndHandleExceptions(By multipleElementsLocator) {
        Wait wait = new WebDriverWait(DriverFactoryHelper.getDriver().get(), Duration.ofSeconds(10));
        try {
            wait.until(ExpectedConditions.presenceOfAllElementsLocatedBy(multipleElementsLocator));
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), multipleElementsLocator, throwable);
        }
    }
    private static String injectUniqueClassName(WebElement element) {
        JavascriptExecutor jsExecutor = (JavascriptExecutor) DriverFactoryHelper.getDriver().get();
        String className = "shaft-unique-identifier-" + (uniqueNumber++);
        jsExecutor.executeScript("arguments[0].classList.add('" + className + "');", element);
        return className;
    }
}