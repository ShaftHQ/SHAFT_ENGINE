package com.shaft.gui.element.internal;

import com.google.common.base.Throwables;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import com.shaft.gui.internal.healing.HealingManager;
import com.shaft.gui.internal.healing.HealingResolution;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.FlakeProfiler;
import com.shaft.tools.io.internal.MobileTraceMetadata;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.AppiumDriver;
import lombok.Getter;
import org.apache.logging.log4j.Level;
import org.junit.jupiter.api.Assertions;
import org.openqa.selenium.*;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.remote.DriverCommand;
import org.openqa.selenium.support.locators.RelativeLocator;

import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Helper utilities for low-level element discovery, interaction, and reporting.
 */
public class ElementActionsHelper {
    /** Masking character used when obfuscating sensitive values in logs and reports. */
    public static final String OBFUSCATED_STRING = "•";
    private static final boolean GET_ELEMENT_HTML = true; //TODO: expose parameter
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds
    private static final String PAGE_SNAPSHOT_ATTACHMENT_TYPE = "Page Snapshot";
    private static final String PAGE_HTML_ATTACHMENT_TYPE = "Page HTML";
    private final boolean isSilent;

    /**
     * Creates a new element actions helper.
     *
     * @param isSilent {@code true} to suppress non-critical report logs; {@code false} otherwise
     */
    public ElementActionsHelper(boolean isSilent) {
        this.isSilent = isSilent;
    }

    boolean isSilent() {
        return isSilent;
    }

    /**
     * Safely calls {@code driver.findElements(locator)}, handling the infinite recursion
     * between Selenium 4.41.0's {@code ElementLocation} and Appium java-client 10.0.0's
     * {@code AppiumBy.findElements(SearchContext)}.
     *
     * <p>The normal {@code driver.findElements()} path is attempted first. If a
     * {@code StackOverflowError} occurs (caused by Appium 3.x returning
     * {@code InvalidArgumentException} for non-W3C locator strategies, triggering
     * {@code ElementLocation} ↔ {@code AppiumBy} infinite recursion), the method
     * falls back to sending the find command directly via {@code AppiumDriver.execute()}.
     *
     * @param driver  the WebDriver instance
     * @param locator the element locator
     * @return the list of found elements, or an empty list if none found or an error occurs
     */
    public static List<WebElement> safeFindElements(WebDriver driver, By locator) {
        try {
            return driver.findElements(locator);
        } catch (StackOverflowError e) {
            // Fallback for Selenium 4.41.0 + Appium 3.x recursion bug
            if (locator instanceof AppiumBy && driver instanceof AppiumDriver appiumDriver) {
                try {
                    var params = ((By.Remotable) locator).getRemoteParameters();
                    var response = appiumDriver.execute(
                            DriverCommand.FIND_ELEMENTS,
                            Map.of("using", params.using(), "value", String.valueOf(params.value())));
                    @SuppressWarnings("unchecked")
                    List<WebElement> result = (List<WebElement>) response.getValue();
                    return result != null ? result : Collections.emptyList();
                } catch (WebDriverException ex) {
                    return Collections.emptyList();
                }
            }
            return Collections.emptyList();
        }
    }

    /**
     * Safely calls {@code driver.findElement(locator)}, handling the infinite recursion
     * between Selenium 4.41.0's {@code ElementLocation} and Appium java-client 10.0.0's
     * {@code AppiumBy.findElements(SearchContext)}.
     *
     * @param driver  the WebDriver instance
     * @param locator the element locator
     * @return the found element
     * @throws NoSuchElementException if the element cannot be found
     * @see #safeFindElements(WebDriver, By) for details on the recursion issue
     */
    public static WebElement safeFindElement(WebDriver driver, By locator) {
        try {
            return driver.findElement(locator);
        } catch (StackOverflowError e) {
            // Fallback for Selenium 4.41.0 + Appium 3.x recursion bug
            if (locator instanceof AppiumBy && driver instanceof AppiumDriver appiumDriver) {
                try {
                    var params = ((By.Remotable) locator).getRemoteParameters();
                    var response = appiumDriver.execute(
                            DriverCommand.FIND_ELEMENT,
                            Map.of("using", params.using(), "value", String.valueOf(params.value())));
                    WebElement element = (WebElement) response.getValue();
                    if (element == null) {
                        throw new NoSuchElementException("Cannot locate an element using " + locator);
                    }
                    return element;
                } catch (NoSuchElementException ex) {
                    throw ex;
                } catch (WebDriverException ex) {
                    throw new NoSuchElementException("Cannot locate an element using " + locator, ex);
                }
            }
            throw new NoSuchElementException("Element not found due to locator incompatibility (StackOverflowError): " + locator, e);
        }
    }

    /**
     * Waits for an on-screen image reference to appear within the current viewport.
     *
     * @param driver the active WebDriver instance
     * @param elementReferenceScreenshot path to the reference image file
     * @return list containing current screenshot, reference screenshot bytes, and matched coordinates
     */
    public List<Object> waitForElementPresence(WebDriver driver, String elementReferenceScreenshot) {
        long startTime = System.currentTimeMillis();
        long elapsedTime;
        List<Integer> coordinates;
        boolean isFound = false;
        byte[] currentScreenImage;

        List<Object> returnedValue = new LinkedList<>();
        if (FileActions.getInstance(true).doesFileExist(elementReferenceScreenshot)) {
            do {
                try {
                    //noinspection BusyWait
                    Thread.sleep(ELEMENT_IDENTIFICATION_POLLING_DELAY);
                } catch (InterruptedException e) {
                    ReportManagerHelper.logDiscrete(e);
                }
                currentScreenImage = new ScreenshotManager().takeScreenshot(driver, null);
                coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage);
                if (!Collections.emptyList().equals(coordinates)) {
                    isFound = true;
                }
                elapsedTime = System.currentTimeMillis() - startTime;
            } while (!isFound && elapsedTime < SHAFT.Properties.timeouts.defaultElementIdentificationTimeout() * 1000L);
            returnedValue.add(currentScreenImage);
            returnedValue.add(FileActions.getInstance(true).readFileAsByteArray(elementReferenceScreenshot));
            returnedValue.add(coordinates);
        } else {
            // reference screenshot doesn't exist
            ReportManager.log("Reference screenshot not found. Kindly confirm the image exists under this path: \"" + elementReferenceScreenshot + "\"");
            currentScreenImage = new ScreenshotManager().takeScreenshot(driver, null);
            returnedValue.add(currentScreenImage);
            returnedValue.add(new byte[0]);
            returnedValue.add(Collections.emptyList());
        }
        return returnedValue;
    }

    /**
     * Waits for an on-screen image reference to disappear from the current viewport.
     *
     * @param driver the active WebDriver instance
     * @param elementReferenceScreenshot path to the reference image file
     * @return list containing current screenshot, reference screenshot bytes, and matched coordinates
     */
    public List<Object> waitForElementInvisibility(WebDriver driver, String elementReferenceScreenshot) {
        long startTime = System.currentTimeMillis();
        long elapsedTime;
        List<Integer> coordinates = Collections.emptyList();
        byte[] currentScreenImage;

        List<Object> returnedValue = new LinkedList<>();
        if (FileActions.getInstance(true).doesFileExist(elementReferenceScreenshot)) {
            do {
                try {
                    //noinspection BusyWait
                    Thread.sleep(ELEMENT_IDENTIFICATION_POLLING_DELAY);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    ReportManagerHelper.logDiscrete(e);
                }
                currentScreenImage = new ScreenshotManager().takeScreenshot(driver, null);
                coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage);
                elapsedTime = System.currentTimeMillis() - startTime;
            } while (!Collections.emptyList().equals(coordinates)
                    && elapsedTime < SHAFT.Properties.timeouts.defaultElementIdentificationTimeout() * 1000L);
            returnedValue.add(currentScreenImage);
            returnedValue.add(FileActions.getInstance(true).readFileAsByteArray(elementReferenceScreenshot));
            returnedValue.add(coordinates);
        } else {
            ReportManager.log("Reference screenshot not found. Kindly confirm the image exists under this path: \"" + elementReferenceScreenshot + "\"");
            currentScreenImage = new ScreenshotManager().takeScreenshot(driver, null);
            returnedValue.add(currentScreenImage);
            returnedValue.add(new byte[0]);
            returnedValue.add(List.of(-1));
        }
        return returnedValue;
    }

    private boolean isValidToCheckForVisibility(By elementLocator, boolean checkForVisibility) {
        var locatorString = JavaHelper.formatLocatorToString(elementLocator).toLowerCase();
        return checkForVisibility && !locatorString.contains("type='file'") && !locatorString.contains("type=\"file\"") && !locatorString.contains("frame") && !elementLocator.equals(By.tagName("html"));
    }

    //TODO: keep enhancing this method until we only need to make ONE WebDriver call per element in case of Type and Click (including element name)
    /**
     * Waits for an element to be present (and optionally visible), then returns collected metadata.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param checkForVisibility whether visibility must be validated
     * @return element information payload used by downstream action methods
     */
    public List<Object> waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        boolean isValidToCheckForVisibility = isValidToCheckForVisibility(elementLocator, checkForVisibility);
        var isMobileExecution = DriverFactoryHelper.isMobileNativeExecution() || DriverFactoryHelper.isMobileWebExecution();
        boolean collectLocatorHealth = LocatorHealthReporter.isEnabled();
        long locatorHealthStart = collectLocatorHealth ? System.nanoTime() : 0L;
        AtomicInteger locatorHealthPollingAttempts = collectLocatorHealth ? new AtomicInteger() : null;
        AtomicInteger locatorHealthStaleRetries = collectLocatorHealth ? new AtomicInteger() : null;
        try {
            List<Object> locatedElement = new SynchronizationManager(driver).fluentWait(isValidToCheckForVisibility)
                    .until(f -> {
                        if (locatorHealthPollingAttempts != null) {
                            locatorHealthPollingAttempts.incrementAndGet();
                        }
                        try {
                            ElementInformation elementInformation = new ElementInformation();
                            List<WebElement> targetElements;
                            // BLOCK #1 :: GETTING THE ELEMENT
                            By shadowDomLocator = ShadowLocatorBuilder.shadowDomLocator.get();
                            By cssSelector = ShadowLocatorBuilder.cssSelector.get();
                            if (shadowDomLocator != null && cssSelector == elementLocator) {
                                SearchContext shadowRoot = driver.findElement(shadowDomLocator).getShadowRoot();
                                targetElements = shadowRoot.findElements(cssSelector);
                                ensureElementListIsNotEmpty(targetElements, cssSelector);
                            } else if (LocatorBuilder.getIFrameLocator().get() != null) {
                                try {
                                    targetElements = driver.switchTo()
                                            .frame(driver.findElement(LocatorBuilder.getIFrameLocator().get()))
                                            .findElements(elementLocator);
                                    ensureElementListIsNotEmpty(targetElements, elementLocator);
                                } catch (NoSuchElementException exception) {
                                    targetElements = resolveMatchingElements(driver, elementLocator);
                                }
                            } else {
                                try {
                                    targetElements = resolveMatchingElements(driver, elementLocator);
                                } catch (InvalidSelectorException invalidSelectorException) {
                                    //break and fail immediately if invalid selector
                                    reportActionResult(driver, null, null, null, null, null, false);
                                    FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", invalidSelectorException);
                                    throw invalidSelectorException;
                                }
                            }
                            WebElement targetElement = targetElements.getFirst();
                            // BLOCK #2 :: GETTING THE ELEMENT LOCATION (RECT)
                            try {
                                elementInformation.setElementRect(targetElement.getRect());
                            } catch (ElementNotInteractableException elementNotInteractableException) {
                                // this exception happens sometimes with certain browsers and causes a timeout
                                // this empty block should handle that issue
                            }
                            // BLOCK #3 :: SCROLLING TO ELEMENT | CONFIRMING IT IS DISPLAYED
                            if (isValidToCheckForVisibility) {
                                if (!isMobileExecution) {
                                    try {
                                        // native Javascript scroll to center (smooth / auto)
                                        ((JavascriptExecutor) driver).executeScript("""
                                                    
                                                    arguments[0].scrollIntoView({behavior: "smooth", block: "center", inline: "center"});""",
                                                targetElement);
                                    } catch (Throwable throwable1) {
                                        try {
                                            // w3c compliant scroll
                                            new Actions(driver).
                                                    scrollToElement(targetElement).
                                                    perform();
                                        } catch (Throwable throwable2) {
                                            // old school selenium scroll
                                            ((Locatable) driver).getCoordinates().
                                                    inViewPort();
                                        }
                                    }
                                } else {
                                    targetElement.
                                            isDisplayed();
                                }
                            }
                            // BLOCK #4 :: GETTING THE NUMBER OF FOUND ELEMENTS
                            elementInformation.setNumberOfFoundElements(targetElements.size());

                            // BLOCK #5 :: GETTING INNER HTML AND OUTER HTML
                            if (!

                                    isMobileExecution &&
                                    GET_ELEMENT_HTML) {
                                elementInformation.setOuterHTML(targetElement.getDomProperty("outerHTML"));
                                elementInformation.setInnerHTML(
                                        targetElement.getDomProperty("innerHTML"));
                            }
                            // BLOCK #6 :: GETTING ELEMENT NAME
                            if (SHAFT.Properties.reporting.
                                    captureElementName()) {
                                var elementName = JavaHelper.
                                        formatLocatorToString(elementLocator);
                                try {
                                    // getAccessibleName() triggers GET .../computedlabel which is
                                    // unsupported by Appium native sessions (returns 404)
                                    if (!DriverFactoryHelper.isMobileNativeExecution()) {
                                        var accessibleName = targetElement.getAccessibleName();
                                        if (
                                                accessibleName != null && !accessibleName.
                                                        isBlank()) {
                                            elementName =
                                                    accessibleName;
                                        }
                                    }
                                } catch (Throwable throwable) {
                                    //happens on some elements that show unhandled inspector error
                                    //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                                    //ignore
                                }
                                elementInformation.setElementName(elementName);
                            }
                            elementInformation.setFirstElement(targetElement);
                            elementInformation.setLocator(elementLocator);
                            HealingManager.observe(
                                    driver,
                                    elementLocator,
                                    targetElements,
                                    "ELEMENT_RESOLUTION",
                                    LocatorBuilder.getIFrameLocator().get(),
                                    shadowDomLocator,
                                    cssSelector);

                            return elementInformation.toList();
                            // int numberOfFoundElements
                            // WebElement firstElement
                            // By locator
                            // String outerHTML (or empty string)
                            // String innerHTML (or empty string)
                            // String elementName (or empty string)
                        } catch (StaleElementReferenceException staleElementReferenceException) {
                            if (locatorHealthStaleRetries != null) {
                                locatorHealthStaleRetries.incrementAndGet();
                            }
                            throw staleElementReferenceException;
                        }
                    });
            recordLocatorHealthLookup(
                    elementLocator,
                    locatorHealthStart,
                    locatorHealthPollingAttempts,
                    resolvedElementCount(locatedElement),
                    false,
                    locatorHealthStaleRetries);
            return locatedElement;
        } catch (org.openqa.selenium.TimeoutException timeoutException) {
            recordLocatorHealthLookup(
                    elementLocator,
                    locatorHealthStart,
                    locatorHealthPollingAttempts,
                    0,
                    true,
                    locatorHealthStaleRetries);
            // In case the element was not found / not visible and the timeout expired
            HealingResolution resolution = HealingManager.resolve(
                            driver,
                            elementLocator,
                            "ELEMENT_RESOLUTION",
                            isValidToCheckForVisibility,
                            LocatorBuilder.getIFrameLocator().get(),
                            ShadowLocatorBuilder.shadowDomLocator.get(),
                            ShadowLocatorBuilder.cssSelector.get())
                    .orElse(null);
            if (resolution != null) {
                return recoveredElementInformation(driver, elementLocator, resolution);
            }
            var causeMessage = timeoutException.getCause().getMessage();
            causeMessage = !causeMessage.isBlank() && causeMessage.contains("\n") ? timeoutException.getMessage() + " || " + causeMessage.substring(0, causeMessage.indexOf("\n")) : timeoutException.getMessage();
            ReportManager.logDiscrete(causeMessage);
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(timeoutException);
            return elementInformation;
        } catch (org.openqa.selenium.InvalidSelectorException invalidSelectorException) {
            recordLocatorHealthLookup(
                    elementLocator,
                    locatorHealthStart,
                    locatorHealthPollingAttempts,
                    0,
                    false,
                    locatorHealthStaleRetries);
            // In case the selector is not valid
            ReportManager.logDiscrete(invalidSelectorException.getMessage());
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(invalidSelectorException);
            return elementInformation;
        }
    }

    private static void recordLocatorHealthLookup(
            By elementLocator,
            long locatorHealthStart,
            AtomicInteger pollingAttempts,
            int foundElementCount,
            boolean timedOut,
            AtomicInteger staleRetries) {
        if (pollingAttempts == null || staleRetries == null) {
            return;
        }
        LocatorHealthReporter.recordLookup(
                elementLocator,
                TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - locatorHealthStart),
                pollingAttempts.get(),
                foundElementCount,
                timedOut,
                staleRetries.get());
    }

    private static int resolvedElementCount(List<Object> locatedElement) {
        if (locatedElement == null || locatedElement.isEmpty()) {
            return 0;
        }
        Object elementCount = locatedElement.getFirst();
        if (elementCount instanceof Number number) {
            return number.intValue();
        }
        try {
            return Integer.parseInt(String.valueOf(elementCount));
        } catch (NumberFormatException exception) {
            return 0;
        }
    }

    private static List<WebElement> resolveMatchingElements(WebDriver driver, By elementLocator) {
        List<WebElement> targetElements = safeFindElements(driver, elementLocator);
        ensureElementListIsNotEmpty(targetElements, elementLocator);
        return targetElements;
    }

    private static void ensureElementListIsNotEmpty(List<WebElement> elements, By elementLocator) {
        if (elements.isEmpty()) {
            throw new NoSuchElementException("Cannot locate an element using " + JavaHelper.formatLocatorToString(elementLocator));
        }
    }

    private List<Object> recoveredElementInformation(
            WebDriver driver,
            By originalLocator,
            HealingResolution resolution) {
        WebElement element = resolution.elements().getFirst();
        ElementInformation information = new ElementInformation();
        information.setNumberOfFoundElements(1);
        information.setFirstElement(element);
        information.setLocator(resolution.selectedLocator());
        try {
            information.setElementRect(element.getRect());
        } catch (WebDriverException ignored) {
            // Optional metadata must not invalidate an otherwise safe recovery.
        }
        if (!DriverFactoryHelper.isMobileNativeExecution() && GET_ELEMENT_HTML) {
            information.setOuterHTML(Objects.requireNonNullElse(element.getDomProperty("outerHTML"), ""));
            information.setInnerHTML(Objects.requireNonNullElse(element.getDomProperty("innerHTML"), ""));
        }
        if (SHAFT.Properties.reporting.captureElementName()) {
            try {
                information.setElementName(Objects.requireNonNullElse(element.getAccessibleName(), ""));
            } catch (WebDriverException ignored) {
                information.setElementName(JavaHelper.formatLocatorToString(resolution.selectedLocator()));
            }
        }
        HealingManager.recordOutcome(
                driver,
                resolution,
                originalLocator,
                "ELEMENT_RESOLUTION",
                true,
                "");
        return information.toList();
    }

    /**
     * Scrolls through the page until the target element is found or timeout is reached.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @return element information including match count and first matched element
     */
    public List<Object> scrollToFindElement(WebDriver driver, By elementLocator) {
        var elementInformation = new ArrayList<>();
        try {
            boolean elementFound = new SynchronizationManager(driver).fluentWait().until(f -> {
                List<WebElement> targetElements = safeFindElements(driver, elementLocator);
                if (!targetElements.isEmpty()) {
                    ReportManagerHelper.logDiscrete("Element found.", Level.DEBUG);
                    elementInformation.add(targetElements.size());
                    elementInformation.add(targetElements.getFirst());
                    return true;
                }
                try {
                    new Actions(driver).scrollByAmount(0, driver.manage().window().getSize().getHeight()).perform();
                } catch (WebDriverException webDriverException) {
                    // this can happen on firefox or if any browser isn't using the actions API properly
                    ((JavascriptExecutor) driver).executeScript("return window.scrollBy(0, arguments[0]);", driver.manage().window().getSize().getHeight());
                }
                return false;
            });
            if (!elementFound) {
                elementInformation.add(0);
                elementInformation.add(null);
            }
        } catch (org.openqa.selenium.TimeoutException timeoutException) {
            // In case the element was not found / not visible and the timeout expired
            ReportManager.logDiscrete(timeoutException.getMessage() + " || " + timeoutException.getCause().getMessage().substring(0, timeoutException.getCause().getMessage().indexOf("\n")));
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(timeoutException);
        }
        return elementInformation;
    }


    //TODO: delete this method after understanding what the heck it's supposed to be doing!
    /**
     * Waits until an element is clickable and optionally performs a follow-up action.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param actionToExecute optional action name to execute after clickability is confirmed
     * @return {@code true} when the element is clickable; otherwise {@code false}
     */
    public boolean waitForElementToBeClickable(WebDriver driver, By elementLocator, String actionToExecute) {
        SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails();

        if (!DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                new SynchronizationManager(driver).fluentWait(false)
                        .until(f -> {
                            WebElement targetElement = safeFindElement(driver, elementLocator);
                            return targetElement.isDisplayed() && targetElement.isEnabled();
                        });

                return new SynchronizationManager(driver).fluentWait(true)
                        .until(f -> {
                            if (!actionToExecute.isEmpty()) {
                                if (actionToExecute.equalsIgnoreCase("ClickAndHold")) {
                                    (new Actions(driver)).clickAndHold(((WebElement) this.identifyUniqueElement(driver, elementLocator).get(1))).build().perform();
                                }
                            }
                            return true;
                        });
            } catch (org.openqa.selenium.TimeoutException e) {
                ReportManagerHelper.logDiscrete(e);
                return false;
            }
        }
        return true;
    }

    /**
     * Waits until the element text differs from the specified value.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param textShouldNotBe text value that must no longer be present
     * @return {@code true} when the condition is met; otherwise {@code false}
     */
    public boolean waitForElementTextToBeNot(WebDriver driver, By elementLocator, String textShouldNotBe) {
        try {
            new SynchronizationManager(driver).fluentWait()
                    .until(f -> !safeFindElement(driver, elementLocator).getText().equals(textShouldNotBe));
        } catch (org.openqa.selenium.TimeoutException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
        return true;
    }

    /**
     * Executes a native mobile JavaScript command through the driver.
     *
     * @param driver the active WebDriver instance
     * @param command mobile command expression (for example, {@code mobile: scroll})
     * @param parameters command parameters map
     */
    public void executeNativeMobileCommandUsingJavascript(WebDriver driver, String command, Map<String, String> parameters) {
        ((JavascriptExecutor) driver).executeScript(command, parameters);
    }

    /**
     * Submits a form element using JavaScript in non-mobile executions.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the form element to submit
     */
    public void submitFormUsingJavascript(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isNotMobileExecution()) {
            ((JavascriptExecutor) driver).executeScript("arguments[0].submit();", this.identifyUniqueElement(driver, elementLocator).get(1));
        }
    }

    /**
     * Changes a web element visibility state by setting inline style via JavaScript.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param desiredIsVisibleState {@code true} to show element; {@code false} to hide it
     */
    public void changeWebElementVisibilityUsingJavascript(WebDriver driver, By elementLocator, boolean desiredIsVisibleState) {
        if (DriverFactoryHelper.isNotMobileExecution()) {

            if (Boolean.TRUE.equals(desiredIsVisibleState)) {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:block !important;');", this.identifyUniqueElement(driver, elementLocator).get(1));
            } else {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');", this.identifyUniqueElement(driver, elementLocator).get(1));
            }
        }
    }

    /**
     * Captures an action screenshot for reporting, with pass/fail aware behavior.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element, or {@code null} for full page
     * @param actionName current action name
     * @param testData current action test data
     * @param passFailStatus current action status
     * @return screenshot attachment payload
     */
    public List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName, String testData, boolean passFailStatus) {
        if (passFailStatus) {
            try {
                if (elementLocator != null) {
                    return new ScreenshotManager().takeScreenshot(driver, elementLocator, actionName, true);
                } else if (testData != null) {
                    return new ScreenshotManager().takeScreenshot(driver, null, actionName, true);
                }
                // else only happens when switching to default content so there is no need to
                // take a screenshot
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
                ReportManager.logDiscrete("Could not take an element screenshot because the element is no longer available. Capturing the full page instead.");
                return new ScreenshotManager().takeScreenshot(driver, null, actionName, true);
            }
        } else {
            return new ScreenshotManager().takeScreenshot(driver, null, actionName, false);
        }
        return new ArrayList<>();
    }

    /**
     * Resolves a human-readable element name for reporting purposes.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @return accessible name when available; otherwise formatted locator text
     */
    public String getElementName(WebDriver driver, By elementLocator) {
        if (SHAFT.Properties.reporting.captureElementName()) {
            try {
                // getAccessibleName() triggers GET .../computedlabel which is
                // unsupported by Appium native sessions (returns 404)
                if (!DriverFactoryHelper.isMobileNativeExecution()) {
                    var accessibleName = ((WebElement) identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getAccessibleName();
                    if (accessibleName != null && !accessibleName.isBlank()) {
                        return accessibleName;
                    }
                }
            } catch (Throwable throwable) {
                var rootCause = Throwables.getRootCause(throwable).getClass();

                if (rootCause.equals(NoSuchElementException.class) || rootCause.equals(InvalidSelectorException.class) || rootCause.equals(MultipleElementsFoundException.class)) {

                    throw throwable;
                }
                //happens on some elements that show unhandled inspector error
                //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                //ignore
            }
        }
        return JavaHelper.formatLocatorToString(elementLocator);
    }

    /**
     * Performs a keyboard-based clipboard action on the active element.
     *
     * @param driver the active WebDriver instance
     * @param action clipboard action to execute
     * @return {@code true} when action succeeds; otherwise {@code false}
     */
    public boolean performClipboardActions(WebDriver driver, ClipboardAction action) {
        try {
            Keys cmdCtrl = SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.MAC.name()) ? Keys.COMMAND : Keys.CONTROL;
            switch (action) {
                case COPY -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("c").keyUp(cmdCtrl).perform();
                case PASTE -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("v").keyUp(cmdCtrl).perform();
                case CUT -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("x").keyUp(cmdCtrl).perform();
                case SELECT_ALL -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("a").keyUp(cmdCtrl).perform();
                case UNSELECT_ALL -> (new Actions(driver)).sendKeys(Keys.ESCAPE).perform();
                default -> {
                    return false;
                }
            }
            return true;
        } catch (HeadlessException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
    }

    /**
     * Checks whether a target class appears in the provided throwable stack trace.
     *
     * @param classObject class to search for
     * @param throwable throwable containing stack trace frames
     * @return {@code true} when class is found in the stack trace; otherwise {@code false}
     */
    public boolean isFoundInStacktrace(Class<?> classObject, Throwable throwable) {
        var targetClassName = classObject.getName();
        for (StackTraceElement element : throwable.getStackTrace()) {
            if (element.getClassName().equals(targetClassName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Identifies a unique matching element while enforcing visibility checks.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @return element information payload for the matched element
     */
    public List<Object> identifyUniqueElement(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, true);
    }

    /**
     * Identifies a unique matching element without visibility checks.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @return element information payload for the matched element
     */
    public List<Object> identifyUniqueElementIgnoringVisibility(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, false);
    }

    private List<Object> identifyUniqueElement(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        var matchingElementsInformation = getMatchingElementsInformation(driver, elementLocator, checkForVisibility);

        if (elementLocator != null) {
            // in case of regular locator
            switch (Integer.parseInt(matchingElementsInformation.get(0).toString())) {
                case 0 -> {
                    reportActionResult(driver, null, null, null, null, null, false);
                    if (matchingElementsInformation.size() > 2 && matchingElementsInformation.get(2) instanceof Throwable) {
                        FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element using this locator \"" + JavaHelper.formatLocatorToString(elementLocator) + "\"", (Throwable) matchingElementsInformation.get(2));
                    }
                    FailureReporter.fail("Failed to identify unique element using this locator \"" + JavaHelper.formatLocatorToString(elementLocator) + "\"");
                }
                case 1 -> {
                    return matchingElementsInformation;
                }
                default -> {
                    if (SHAFT.Properties.flags.forceCheckElementLocatorIsUnique() && !(elementLocator instanceof RelativeLocator.RelativeBy)) {
                        reportActionResult(driver, null, null, null, null, null, false);
                        FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", new MultipleElementsFoundException("Multiple elements found matching this locator \"" + JavaHelper.formatLocatorToString(elementLocator) + "\""));
                    }
                    return matchingElementsInformation;
                }
            }
        } else {
            // in case locator is null
            failAction(driver, "element locator is NULL.", null);
        }
        //unreachable code
        return matchingElementsInformation;
    }

    /**
     * Collects element-match metadata for the supplied locator.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param checkForVisibility whether visibility must be validated
     * @return list containing count and resolved element details
     */
    public List<Object> getMatchingElementsInformation(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        if (elementLocator == null) {
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            return elementInformation;
        }
        if (!elementLocator.equals(By.tagName("html"))) {
            return this.waitForElementPresence(driver, elementLocator, checkForVisibility);
        } else {
            //if locator is just tag-name html
            var elementInformation = new ArrayList<>();
            elementInformation.add(1);
            elementInformation.add(null);
            return elementInformation;
        }
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public int getElementsCount(WebDriver driver, By elementLocator) {
        return Integer.parseInt(this.getMatchingElementsInformation(driver, elementLocator, false).getFirst().toString());
    }

    /**
     * Reports a successful element action with a single screenshot attachment.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param testData test data associated with the action
     * @param screenshot screenshot attachment payload
     * @param elementName resolved element name for reporting
     */
    public void passAction(WebDriver driver, By elementLocator, String testData, List<Object> screenshot, String elementName) {
        //TODO: open calling methods, and test if Appium can also fetch the element name instead of passing null
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(screenshot);
        passAction(driver, elementLocator, actionName, testData, attachments, elementName);
    }

    /**
     * Reports a successful element action with optional attachments.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param actionName action name to report
     * @param testData test data associated with the action
     * @param screenshots screenshot attachments payload
     * @param elementName resolved element name for reporting
     */
    public void passAction(WebDriver driver, By elementLocator, String actionName, String testData, List<List<Object>> screenshots, String elementName) {
        reportActionResult(driver, actionName, testData, elementLocator, screenshots, elementName, true);
    }

    /**
     * Reports a failed element action using inferred action name and optional root cause.
     *
     * @param driver the active WebDriver instance
     * @param elementLocator locator of the target element
     * @param rootCauseException optional root cause exceptions
     */
    public void failAction(WebDriver driver, By elementLocator, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, null, elementLocator, null, rootCauseException);
    }

    /**
     * Reports a failed element action with test data and optional root cause.
     *
     * @param driver the active WebDriver instance
     * @param testData test data associated with the action
     * @param elementLocator locator of the target element
     * @param rootCauseException optional root cause exceptions
     */
    public void failAction(WebDriver driver, String testData, By elementLocator, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, null, rootCauseException);
    }

    /**
     * Reports a failed element action with explicit attachments and optional root cause.
     *
     * @param driver the active WebDriver instance
     * @param testData test data associated with the action
     * @param elementLocator locator of the target element
     * @param attachments pre-collected attachments
     * @param rootCauseException optional root cause exceptions
     */
    public void failAction(WebDriver driver, String testData, By elementLocator, List<List<Object>> attachments, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, attachments, rootCauseException);
    }

    /**
     * Reports a failed element action using explicit action metadata and optional root cause.
     *
     * @param driver the active WebDriver instance
     * @param actionName action name to report
     * @param testData test data associated with the action
     * @param elementLocator locator of the target element
     * @param screenshots optional screenshot attachments
     * @param rootCauseException optional root cause exceptions
     */
    public void failAction(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots, Throwable... rootCauseException) {
        //TODO: merge all fail actions, make all methods call this one, get elementName where applicable instead of reporting null
        //this condition works if this is the first level of failure, but the first level is usually caught by the calling method

        String elementName = elementLocator != null ? JavaHelper.formatLocatorToString(elementLocator) : "";
        if (elementLocator != null && (rootCauseException.length >= 1 && Throwables.getRootCause(rootCauseException[0]).getClass() != MultipleElementsFoundException.class && Throwables.getRootCause(rootCauseException[0]).getClass() != NoSuchElementException.class && Throwables.getRootCause(rootCauseException[0]).getClass() != InvalidSelectorException.class)) {
            try {
                // getAccessibleName() triggers GET .../computedlabel which is
                // unsupported by Appium native sessions (returns 404)
                if (!DriverFactoryHelper.isMobileNativeExecution()) {
                    var accessibleName = ((WebElement) this.identifyUniqueElement(driver, elementLocator).get(1)).getAccessibleName();
                    if (accessibleName != null && !accessibleName.isBlank()) {
                        elementName = accessibleName;
                    }
                }
            } catch (WebDriverException e) {
                //happens on some elements that show unhandled inspector error
                //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                //ignore
            }
        }

        String message;
        if (rootCauseException.length >= 1) {
            message = reportActionResult(driver, actionName, testData, elementLocator, screenshots, elementName, false, rootCauseException[0]);
        } else {
            message = reportActionResult(driver, actionName, testData, null, screenshots, elementName, false);
        }
        if (rootCauseException.length >= 1) {
            Assertions.fail(message, rootCauseException[0]);
        } else {
            Assertions.fail(message);
        }
    }

    /**
     * Builds a user-facing report message for element actions.
     *
     * @param actionName action name
     * @param testData action test data
     * @param elementName resolved element name
     * @param passFailStatus action status flag
     * @return formatted report message
     */
    public String createReportMessage(String actionName, String testData, String elementName, Boolean passFailStatus) {
        String message = "";

        if (Boolean.FALSE.equals(passFailStatus)) {
            message = message + "Failed to ";
        }

        actionName = JavaHelper.convertToSentenceCase(actionName);

        message = message + actionName;

        if (testData != null && !testData.isEmpty() && testData.length() < 500) {
            message = message + " \"" + testData.trim() + "\"";
        }

        if ((elementName != null && !elementName.isEmpty())) {
            var preposition = getPreposition(actionName);
            message = message + preposition + "\"" + elementName.trim() + "\"";
        }

        message = message + ".";
        return message;
    }

    private String getPreposition(String actionName) {
        var preposition = " ";
        if (actionName.toLowerCase().contains("type") || actionName.toLowerCase().contains("setproperty value using javascript")) {
            preposition = " into ";
        } else if (actionName.toLowerCase().contains("get") || actionName.toLowerCase().contains("select")) {
            preposition = " from ";
        } else if (actionName.toLowerCase().contains("clipboard")) {
            preposition = " on ";
        } else if (actionName.toLowerCase().contains("drag and drop") || actionName.toLowerCase().contains("key press") || actionName.toLowerCase().contains("wait") || actionName.toLowerCase().contains("submit") || actionName.toLowerCase().contains("switch")) {
            preposition = " against ";
        } else if (actionName.toLowerCase().contains("hover")) {
            preposition = " over ";
        }
        return preposition;
    }

    private List<List<Object>> createReportAttachments(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots, Boolean passFailStatus, Throwable... rootCauseException) {
        actionName = JavaHelper.convertToSentenceCase(actionName);
        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Test Data - " + actionName, "Actual Value", testData);
            attachments.add(actualValueAttachment);
        }
        if (screenshots != null && !screenshots.isEmpty()) {
            // screenshot taken before action (in case of click)
            attachments.addAll(screenshots);
        } else if (driver != null) {
            List<Object> newScreenshot;
            if (actionName.equals("Identify unique element")) {
                newScreenshot = takeScreenshot(driver, null, actionName, testData, passFailStatus);
            } else {
                newScreenshot = takeScreenshot(driver, elementLocator, actionName, testData, passFailStatus);
            }
            if (newScreenshot != null && !newScreenshot.isEmpty()) {
                attachments.add(newScreenshot);
            }
        }

        if (driver != null && (Boolean.FALSE.equals(passFailStatus)
                || SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().equalsIgnoreCase("always"))) {
            long profilerStart = FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
            var pageSnapshot = new BrowserActionsHelper(false).capturePageSnapshot(driver);
            if (profilerStart != 0L) {
                FlakeProfiler.recordEvidenceCapture("page snapshot", actionName,
                        TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerStart));
            }
            var attachmentType = "";
            if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
                attachmentType = PAGE_SNAPSHOT_ATTACHMENT_TYPE;
            } else if (pageSnapshot.startsWith("<html")) {
                attachmentType = PAGE_HTML_ATTACHMENT_TYPE;
            }
            List<Object> sourceAttachment = Arrays.asList(attachmentType, actionName, pageSnapshot);
            attachments.add(sourceAttachment);
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Exception - " + actionName, "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
        }

        if (attachments.isEmpty() || (attachments.size() == 1 && attachments.getFirst().isEmpty())) {
            return null;
        } else {
            return attachments;
        }
    }

    /**
     * Finalizes action reporting by logging formatted message and attachments.
     *
     * @param driver the active WebDriver instance
     * @param actionName action name, or {@code null} to infer from stack trace
     * @param testData action test data
     * @param elementLocator locator of the target element
     * @param screenshots pre-collected screenshots
     * @param elementName resolved element name
     * @param passFailStatus action status flag
     * @param rootCauseException optional root cause exceptions
     * @return final report message
     */
    public String reportActionResult(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots, String elementName, Boolean passFailStatus, Throwable... rootCauseException) {
        if (actionName == null) {
            actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        }
        String message = createReportMessage(actionName, testData, elementName, passFailStatus);
        List<List<Object>> attachments = createReportAttachments(driver, actionName, testData, elementLocator, screenshots, passFailStatus, rootCauseException);

        if (message.contains("Failed") && rootCauseException != null && rootCauseException.length > 0) {
            var rootCauseThrowable = Throwables.getRootCause(rootCauseException[0]);
            var rootCauseMessage = rootCauseThrowable.getLocalizedMessage();
            String rootCause = " Root cause: \"" + rootCauseThrowable.getClass().getName() + ": " + (rootCauseMessage != null ? rootCauseMessage.split("\n")[0] : "No message") + "\"";
            message += rootCause;
        }
        if (!isSilent || actionName.equals("identifyUniqueElement")) {
            if (attachments != null && !attachments.isEmpty()) {
                long profilerStart = FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
                ReportManagerHelper.log(message, attachments);
                if (profilerStart != 0L) {
                    FlakeProfiler.recordEvidenceCapture("report attachment", actionName,
                            TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerStart));
                }
            } else {
                ReportManager.log(message);
            }
        }
        if (!isSilent && !calledFromElementActions()) {
            String category = traceCategory();
            TraceEventRecorder.record(category, actionName, Boolean.TRUE.equals(passFailStatus) ? "passed" : "failed",
                    elementLocator == null ? "" : JavaHelper.formatLocatorToString(elementLocator), driver, message,
                    firstThrowable(rootCauseException), traceMetadata(driver, testData, elementName, category,
                            !Boolean.TRUE.equals(passFailStatus)), summarizeAttachments(attachments));
        }
        return message;
    }

    private static boolean calledFromElementActions() {
        for (StackTraceElement frame : Thread.currentThread().getStackTrace()) {
            if ("com.shaft.gui.element.internal.Actions".equals(frame.getClassName())) {
                return true;
            }
        }
        return false;
    }

    private static String traceCategory() {
        for (StackTraceElement frame : Thread.currentThread().getStackTrace()) {
            if ("com.shaft.gui.element.TouchActions".equals(frame.getClassName())) {
                return "touch";
            }
        }
        return "element";
    }

    private static Throwable firstThrowable(Throwable[] throwables) {
        return throwables == null || throwables.length == 0 ? null : throwables[0];
    }

    private static Map<String, String> traceMetadata(WebDriver driver, String testData, String elementName,
                                                     String category, boolean includeNativeSource) {
        Map<String, String> metadata = new LinkedHashMap<>();
        if (testData != null && !testData.isBlank()) {
            metadata.put("testDataLength", String.valueOf(testData.length()));
            if ("touch".equals(category)) {
                metadata.put("gestureParameters", testData);
            }
        }
        if (elementName != null && !elementName.isBlank()) {
            metadata.put("elementName", elementName);
        }
        metadata.putAll(MobileTraceMetadata.mobileMetadata(driver, includeNativeSource));
        return metadata;
    }

    private static List<String> summarizeAttachments(List<List<Object>> attachments) {
        if (attachments == null || attachments.isEmpty()) {
            return List.of();
        }
        List<String> summaries = new ArrayList<>();
        for (List<Object> attachment : attachments) {
            if (attachment == null || attachment.isEmpty()) {
                continue;
            }
            String description = String.valueOf(attachment.getFirst());
            String name = attachment.size() > 1 ? String.valueOf(attachment.get(1)) : "";
            Object payload = attachment.size() > 2 ? attachment.get(2) : null;
            summaries.add(description + (name.isBlank() ? "" : " - " + name) + payloadSize(payload));
        }
        return summaries;
    }

    private static String payloadSize(Object payload) {
        if (payload instanceof byte[] bytes) {
            return " (" + bytes.length + " bytes)";
        }
        if (payload instanceof CharSequence text) {
            return " (" + text.length() + " chars)";
        }
        return "";
    }

    /**
     * Strategies used to extract textual content from a web element.
     */
    @Getter
    public enum TextDetectionStrategy {
        /** Reads text via WebElement#getText(). */
        TEXT("text"),
        /** Reads text via DOM textContent property. */
        CONTENT("textContent"),
        /** Reads text via DOM value property. */
        VALUE("value"),
        /** Represents an unresolved text strategy. */
        UNDEFINED("undefined");
        private final String value;

        TextDetectionStrategy(String strategy) {
            this.value = strategy;
        }

    }
}
