package com.shaft.gui.element.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.enums.internal.ElementAction;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.support.locators.RelativeLocator;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public class ElementIdentificationCore {
    private static final boolean GET_ELEMENT_HTML = true; //TODO: expose parameter
    private final ElementActionsHelper elementActionsHelper;

    ElementIdentificationCore(ElementActionsHelper elementActionsHelper) {
        this.elementActionsHelper = elementActionsHelper;
    }

    //TODO: keep enhancing this method until we only need to make ONE WebDriver call per element in case of Type and Click (including element name)
    public List<Object> waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility, Object... action) {
        boolean isValidToCheckForVisibility = elementActionsHelper.isValidToCheckForVisibility(elementLocator, checkForVisibility);
        var isMobileExecution = DriverFactoryHelper.isMobileNativeExecution() || DriverFactoryHelper.isMobileWebExecution();

        // check to see if the element was already found
        AtomicBoolean isFound = new AtomicBoolean(false);
        AtomicReference<ElementInformation> previouslyFoundElementInformation = new AtomicReference<>();
        previouslyFoundElementInformation.set(Elements.found.stream().filter(elementInformation -> Objects.equals(elementInformation.getLocator(), elementLocator)).findFirst().orElse(null));
        isFound.set(previouslyFoundElementInformation.get() != null);

        try {
            return new SynchronizationManager(driver).fluentWait(isValidToCheckForVisibility)
                    .until(f -> {
                        try (ExecutorService myExecutor = Executors.newVirtualThreadPerTaskExecutor()) {
                            ElementInformation elementInformation;

                            if (!isFound.get()) {
                                elementInformation = new ElementInformation();
                                By shadowDomLocator = ShadowLocatorBuilder.shadowDomLocator.get();
                                By cssSelector = ShadowLocatorBuilder.cssSelector.get();

                                // LOCATE THE ELEMENT
                                final WebElement targetElement = block1GetElement(shadowDomLocator, cssSelector, elementLocator, driver);
                                var threadRect = myExecutor.submit(() -> block2GetElementLocation(targetElement, elementInformation));
                                var threadLocate = myExecutor.submit(() -> block3ScrollElementIntoView(isValidToCheckForVisibility, isMobileExecution, driver, targetElement));
                                var threadCount = myExecutor.submit(() -> block4GetNumberOfFoundElements(shadowDomLocator, cssSelector, elementLocator, driver, elementInformation));

                                // SYNCHRONIZATION POINT
                                threadRect.get();
                                threadLocate.get();
                                threadCount.get();

                                // STORE THE ELEMENT
                                elementInformation.setFirstElement(targetElement);
                                elementInformation.setLocator(elementLocator);
                                Elements.found.add(elementInformation);
                            } else {
                                elementInformation = previouslyFoundElementInformation.get();
                            }

                            // REFRESH ELEMENT INFORMATION
                            var threadHTML = myExecutor.submit(() -> block5GetElementHtml(elementInformation, isMobileExecution));
                            var threadName = myExecutor.submit(() -> block6GetElementName(elementInformation));
                            threadHTML.get();
                            threadName.get();

                            // PERFORM ACTION AND RETURN
                            block7PerformAction(driver, elementInformation, action);
                            return elementInformation.toList();
                        } catch (ExecutionException | InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                    });
        } catch (TimeoutException timeoutException) {
            // In case the element was not found / not visible and the timeout expired
            var causeMessage = timeoutException.getCause().getMessage();
            causeMessage = !causeMessage.isBlank() && causeMessage.contains("\n") ? timeoutException.getMessage() + " || " + causeMessage.substring(0, causeMessage.indexOf("\n")) : timeoutException.getMessage();
            ReportManager.logDiscrete(causeMessage);
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(timeoutException);
            return elementInformation;
        } catch (InvalidSelectorException invalidSelectorException) {
            //break and fail immediately if invalid selector
            elementActionsHelper.reportActionResult(driver, null, null, null, null, null, false);
            FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", invalidSelectorException);
            //unreachable code
            return new ArrayList<>();
//            var elementInformation = new ArrayList<>();
//            elementInformation.add(0);
//            elementInformation.add(null);
//            elementInformation.add(invalidSelectorException);
//            return elementInformation;
        }
    }

    private WebElement block1GetElement(By shadowDomLocator, By cssSelector, By elementLocator, WebDriver driver) {
        // BLOCK #1 :: GETTING THE ELEMENT
        if (shadowDomLocator != null && cssSelector == elementLocator) {
            return driver.findElement(shadowDomLocator)
                    .getShadowRoot()
                    .findElement(cssSelector);
        } else if (LocatorBuilder.getIFrameLocator().get() != null) {
            try {
                return driver.switchTo().frame(driver.findElement(LocatorBuilder.getIFrameLocator().get())).findElement(elementLocator);
            } catch (NoSuchElementException exception) {
                return driver.findElement(elementLocator);
            }
        }
        return driver.findElement(elementLocator);
    }

    private void block2GetElementLocation(WebElement targetElement, ElementInformation elementInformation) {
        // BLOCK #2 :: GETTING THE ELEMENT LOCATION (RECT)
        try {
            elementInformation.setElementRect(targetElement.getRect());
        } catch (ElementNotInteractableException elementNotInteractableException) {
            // this exception happens sometimes with certain browsers and causes a timeout
            // this empty block should handle that issue
        }
    }

    private void block3ScrollElementIntoView(boolean isValidToCheckForVisibility, boolean isMobileExecution, WebDriver driver, WebElement targetElement) {
        // BLOCK #3 :: SCROLLING TO ELEMENT | CONFIRMING IT IS DISPLAYED
        if (isValidToCheckForVisibility) {
            if (!isMobileExecution) {
                try {
                    // native Javascript scroll to center (smooth / auto)
                    ((JavascriptExecutor) driver).executeScript("""
                            arguments[0].scrollIntoView({behavior: "smooth", block: "center", inline: "center"});""", targetElement);
                } catch (Throwable throwable) {
                    try {
                        // w3c compliant scroll
                        new Actions(driver).scrollToElement(targetElement).perform();
                    } catch (Throwable throwable1) {
                        // old school selenium scroll
                        ((Locatable) driver).getCoordinates().inViewPort();
                    }
                }
            } else {
                targetElement.isDisplayed();
            }
        }
    }

    private void block4GetNumberOfFoundElements(By shadowDomLocator, By cssSelector, By elementLocator, WebDriver driver, ElementInformation elementInformation) {
        // BLOCK #4 :: GETTING THE NUMBER OF FOUND ELEMENTS
        if (shadowDomLocator != null && cssSelector == elementLocator) {
            elementInformation.setNumberOfFoundElements(driver.findElement(shadowDomLocator)
                    .getShadowRoot()
                    .findElements(cssSelector)
                    .size());
        } else {
            elementInformation.setNumberOfFoundElements(driver.findElements(elementLocator).size());
        }
    }

    private void block5GetElementHtml(ElementInformation elementInformation, boolean isMobileExecution) {
        // BLOCK #5 :: GETTING THE INNER AND OUTER HTML
        if (!isMobileExecution && GET_ELEMENT_HTML) {
            elementInformation.setOuterHTML(elementInformation.getFirstElement().getAttribute("outerHTML"));
            elementInformation.setInnerHTML(elementInformation.getFirstElement().getAttribute("innerHTML"));
        }
    }

    private void block6GetElementName(ElementInformation elementInformation) {
        // BLOCK #6 :: GETTING ELEMENT NAME
        if (SHAFT.Properties.reporting.captureElementName()) {
            var elementName = JavaHelper.formatLocatorToString(elementInformation.getLocator());
            try {
                var accessibleName = elementInformation.getFirstElement().getAccessibleName();
                if (accessibleName != null && !accessibleName.isBlank()) {
                    elementName = accessibleName;
                }
            } catch (Throwable throwable) {
                //happens on some elements that show unhandled inspector error
                //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                //ignore
            }
            elementInformation.setElementName(elementName);
        }
    }

    private void block7PerformAction(WebDriver driver, ElementInformation elementInformation, Object... action) {
        if (action != null && action.length > 0) {
            // fail if multiple elements are found and flag is enabled
            if (elementInformation.getNumberOfFoundElements() > 1
                    && SHAFT.Properties.flags.forceCheckElementLocatorIsUnique() &&
                    !(elementInformation.getLocator() instanceof RelativeLocator.RelativeBy)) {
                elementActionsHelper.reportActionResult(driver, null, null, null, null, null, false);
                FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", new MultipleElementsFoundException("Multiple elements found matching this locator \"" + JavaHelper.formatLocatorToString(elementInformation.getLocator()) + "\""));
            }
            // BLOCK #6 :: PERFORMING ACTION  (WITH OPTIONAL ARGS)
            // attempt to perform action inside the loop to guarantee higher odds of success and reduced WebDriver calls
            switch (action.length) {
                case 1 ->
                        elementInformation.setActionResult(elementActionsHelper.performAction(driver, elementInformation, (ElementAction) action[0], ""));
                case 2 ->
                        elementInformation.setActionResult(elementActionsHelper.performAction(driver, elementInformation, (ElementAction) action[0], action[1]));
            }
        }
    }
}
