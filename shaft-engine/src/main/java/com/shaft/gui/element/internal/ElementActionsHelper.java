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
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
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

/**
 * Helper utilities for low-level element discovery, interaction, and reporting.
 */
public class ElementActionsHelper {
    /** Masking character used when obfuscating sensitive values in logs and reports. */
    public static final String OBFUSCATED_STRING = "•";
    private static final boolean GET_ELEMENT_HTML = true; //TODO: expose parameter
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds
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
        try {
            return new SynchronizationManager(driver).fluentWait(isValidToCheckForVisibility)
                    .until(f -> {
                        final WebElement[] targetElement = new WebElement[1];
                        ElementInformation elementInformation = new ElementInformation();
                        // BLOCK #1 :: GETTING THE ELEMENT
                        By shadowDomLocator = ShadowLocatorBuilder.shadowDomLocator.get();
                        By cssSelector = ShadowLocatorBuilder.cssSelector.get();
                        if (shadowDomLocator != null && cssSelector == elementLocator) {
                            targetElement[0] = driver.findElement(shadowDomLocator)
                                    .getShadowRoot()
                                    .findElement(cssSelector);
                        } else if (LocatorBuilder.getIFrameLocator().get() != null) {
                            try {
                                targetElement[0] = driver.switchTo().frame(driver.findElement(LocatorBuilder.getIFrameLocator().get())).findElement(elementLocator);
                            } catch (NoSuchElementException exception) {
                                targetElement[0] = safeFindElement(driver, elementLocator);
                            }
                        } else {
                            try {
                                targetElement[0] = safeFindElement(driver, elementLocator);
                            } catch (InvalidSelectorException invalidSelectorException) {
                                //break and fail immediately if invalid selector
                                reportActionResult(driver, null, null, null, null, null, false);
                                FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", invalidSelectorException);
                            }
                        }
                        // BLOCK #2 :: GETTING THE ELEMENT LOCATION (RECT)
                        try {
                            elementInformation.setElementRect(targetElement[0].getRect());
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
                                            targetElement[0]);
                                } catch (Throwable throwable) {
                                    try {
                                        // w3c compliant scroll
                                        new Actions(driver).
                                                scrollToElement(targetElement[0]).
                                                perform();
                                    } catch (Throwable throwable1) {
                                        // old school selenium scroll
                                        ((Locatable) driver).getCoordinates().
                                                inViewPort();
                                    }
                                }
                            } else {
                                targetElement[0].
                                        isDisplayed();
                            }
                        }
                        // BLOCK #4 :: GETTING THE NUMBER OF FOUND ELEMENTS
                        if (shadowDomLocator != null &&
                                cssSelector == elementLocator) {
                            elementInformation.setNumberOfFoundElements(driver.
                                    findElement(shadowDomLocator)
                                    .getShadowRoot
                                            ()
                                    .findElements(cssSelector)
                                    .size());
                        } else {
                            elementInformation.setNumberOfFoundElements(safeFindElements(driver, elementLocator).size());
                        }

                        // BLOCK #5 :: GETTING INNER HTML AND OUTER HTML
                        if (!

                                isMobileExecution &&
                                GET_ELEMENT_HTML) {
                            elementInformation.setOuterHTML(targetElement[0].getDomProperty("outerHTML"));
                            elementInformation.setInnerHTML(
                                    targetElement[0].getDomProperty("innerHTML"));
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
                                    var accessibleName = targetElement[0].getAccessibleName();
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
                        elementInformation.setFirstElement(targetElement[0]);
                        elementInformation.setLocator(elementLocator);
                        HealingManager.observe(
                                driver,
                                elementLocator,
                                List.of(targetElement[0]),
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
                    });
        } catch (org.openqa.selenium.TimeoutException timeoutException) {
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
            // In case the selector is not valid
            ReportManager.logDiscrete(invalidSelectorException.getMessage());
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(invalidSelectorException);
            return elementInformation;
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
                if (!safeFindElements(driver, elementLocator).isEmpty()) {
                    ReportManagerHelper.logDiscrete("Element found.", Level.DEBUG);
                    elementInformation.add(safeFindElements(driver, elementLocator).size());
                    elementInformation.add(safeFindElement(driver, elementLocator));
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
                        .until(f -> safeFindElement(driver, elementLocator).isDisplayed() && safeFindElement(driver, elementLocator).isEnabled());

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
            return new ScreenshotManager().takeScreenshot(driver, elementLocator, actionName, false);
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
            var logMessage = "";
            var pageSnapshot = new BrowserActionsHelper(false).capturePageSnapshot(driver);
            if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
                logMessage = "page snapshot";
            } else if (pageSnapshot.startsWith("<html")) {
                logMessage = "page HTML";
            }
            List<Object> sourceAttachment = Arrays.asList(actionName, logMessage, pageSnapshot);
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
                ReportManagerHelper.log(message, attachments);
            } else {
                ReportManager.log(message);
            }
        }
        return message;
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
