package com.shaft.gui.element;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.*;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.time.Duration;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import static java.util.Arrays.asList;

@SuppressWarnings({"unused", "UnusedReturnValue"})
public class TouchActions extends FluentWebDriverAction {
    private static final int DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT = 5;
    private static final boolean CAPTURE_CLICKED_ELEMENT_TEXT = SHAFT.Properties.reporting.captureElementName();

    public TouchActions() {
        initialize();
    }

    public TouchActions(WebDriver driver) {
        initialize(driver);
    }

    public TouchActions(DriverFactoryHelper helper) {
        initialize(helper);
    }

    @Override public TouchActions and() {
        return this;
    }

    public WebDriverElementValidationsBuilder assertThat(By elementLocator) {
        return new WizardHelpers.WebDriverAssertions(driverFactoryHelper).element(elementLocator);
    }

    public WebDriverElementValidationsBuilder verifyThat(By elementLocator) {
        return new WizardHelpers.WebDriverVerifications(driverFactoryHelper).element(elementLocator);
    }

    /**
     * Sends a key-press via the device soft keyboard.
     *
     * @param key the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public TouchActions nativeKeyboardKeyPress(KeyboardKeys key) {
        try {
            ((RemoteWebDriver) driverFactoryHelper.getDriver()).executeScript("performEditorAction", key.getValue());
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), key.name(), null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    /**
     * Hides the device native soft keyboard.
     *
     * @return a self-reference to be used to chain actions
     */
    public TouchActions hideNativeKeyboard() {
        try {
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                androidDriver.hideKeyboard();
            } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                iosDriver.hideKeyboard();
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
            }
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        return this;
    }

    /**
     * Taps an element once on a touch-enabled screen
     *
     * @param elementReferenceScreenshot relative path to the reference image from the local object repository
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("unchecked")
    public TouchActions tap(String elementReferenceScreenshot) {
        // Wait for element presence and get the needed data
        var objects = elementActionsHelper.waitForElementPresence(driverFactoryHelper.getDriver(), elementReferenceScreenshot);
        byte[] currentScreenImage = (byte[]) objects.get(0);
        byte[] referenceImage = (byte[]) objects.get(1);
        List<Integer> coordinates = (List<Integer>) objects.get(2);

        // Prepare screenshots for reporting
        var screenshotManager = new ScreenshotManager();
        var screenshot = screenshotManager.prepareImageForReport(currentScreenImage, "tap - Current Screen Image");
        var referenceScreenshot = screenshotManager.prepareImageForReport(referenceImage, "tap - Reference Screenshot");
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(referenceScreenshot);
        attachments.add(screenshot);

        // If coordinates are empty then OpenCV couldn't find the element on screen
        if (Collections.emptyList().equals(coordinates)) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), "Couldn't find reference element on the current screen. If you can see it in the attached image then kindly consider cropping it and updating your reference image under this path \"" + elementReferenceScreenshot + "\".", null, attachments);
        } else {
            // Perform tap action by coordinates
//            if (DriverFactoryHelper.isMobileNativeExecution()) {
            PointerInput input = new PointerInput(PointerInput.Kind.TOUCH, "finger1");
            Sequence tap = new Sequence(input, 0);
            tap.addAction(input.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), coordinates.get(0), coordinates.get(1)));
            tap.addAction(input.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
            tap.addAction(new Pause(input, Duration.ofMillis(200)));
            tap.addAction(input.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
            try {
                ((RemoteWebDriver) driverFactoryHelper.getDriver()).perform(ImmutableList.of(tap));
            } catch (UnsupportedCommandException exception) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, exception);
            }
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, null);
        }
        return this;
    }

    /**
     * Taps an element once on a touch-enabled screen
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions tap(By elementLocator) {
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).click(elementLocator);
//        try {
//            String elementText = "";
//            if (CAPTURE_CLICKED_ELEMENT_TEXT) {
//                try {
//                    if (DriverFactoryHelper.isMobileNativeExecution()) {
//                        elementText = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1)).getAttribute("text");
//                    } else {
//                        elementText = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1)).getText();
//                    }
//                } catch (Exception e) {
//                    // do nothing
//                }
//            }
//            List<Object> screenshot = elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), elementLocator, "tap", null, true);
//            // takes screenshot before clicking the element out of view
//
//            try {
////                fixing https://github.com/ShaftHQ/SHAFT_ENGINE/issues/501
//                ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1)).click();
//            } catch (Exception e) {
//                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, e);
//            }
//
//            if (elementText == null || elementText.isEmpty()) {
//                elementText = JavaHelper.formatLocatorToString(elementLocator);
//            }
//            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, elementText.replaceAll("\n", " "), screenshot, null);
//        } catch (Throwable throwable) {
//            // has to be throwable to catch assertion errors in case element was not found
//            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
//        }
        return this;
    }

    /**
     * Double-Taps an element on a touch-enabled screen
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions doubleTap(By elementLocator) {
        try {
            String elementText = "";
            try {
                elementText = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1)).getText();
            } catch (Exception e) {
                // do nothing
            }

            // takes screenshot before clicking the element out of view
            var screenshot = elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), elementLocator, "doubleTap", null, true);
            List<List<Object>> attachments = new LinkedList<>();
            attachments.add(screenshot);

            try {
                (new Actions(driverFactoryHelper.getDriver())).doubleClick(((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1))).perform();
            } catch (Exception e) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, e);
            }

            if (elementText != null && !elementText.isEmpty()) {
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, elementText.replaceAll("\n", " "), screenshot, null);
            } else {
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, null);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Performs a long-tap on an element to trigger the context menu on a
     * touch-enabled screen
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions longTap(By elementLocator) {
        try {
            String elementText = "";
            try {
                elementText = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1)).getText();
            } catch (Exception e) {
                // do nothing
            }
            // takes screenshot before clicking the element out of view
            List<Object> screenshot = elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), elementLocator, "longPress", null, true);
            List<List<Object>> attachments = new LinkedList<>();
            attachments.add(screenshot);

            try {
                new Actions(driverFactoryHelper.getDriver()).clickAndHold(((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1))).perform();
            } catch (Exception e) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, e);
            }

            if (elementText != null && !elementText.isEmpty()) {
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, elementText.replaceAll("\n", " "), screenshot, null);
            } else {
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, null);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Send the currently active app to the background, and return after a certain number of seconds.
     *
     * @param secondsToSpendInTheBackground number of seconds before returning to the app
     * @return a self-reference to be used to chain actions
     */
    public TouchActions sendAppToBackground(int secondsToSpendInTheBackground) {
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                androidDriver.runAppInBackground(Duration.ofSeconds(secondsToSpendInTheBackground));
            } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                iosDriver.runAppInBackground(Duration.ofSeconds(secondsToSpendInTheBackground));
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
            }
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
        }
        return this;
    }

    /**
     * Send the currently active app to the background and leave the app deactivated.
     *
     * @return a self-reference to be used to chain actions
     */
    public TouchActions sendAppToBackground() {
        return sendAppToBackground(-1);
    }

    /**
     * Activates an app that has been previously deactivated or sent to the background.
     *
     * @param appPackageName the full name for the app package that you want to activate. for example [com.apple.Preferences] or [io.appium.android.apis]
     * @return a self-reference to be used to chain actions
     */
    public TouchActions activateAppFromBackground(String appPackageName) {
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                androidDriver.activateApp(appPackageName);
            } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                iosDriver.activateApp(appPackageName);
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
            }
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
        }
        return this;
    }

    /**
     * Swipes the sourceElement onto the destinationElement on a touch-enabled
     * screen
     *
     * @param sourceElementLocator      the locator of the webElement that needs to
     *                                  be swiped (By xpath, id, selector, name
     *                                  ...etc.)
     * @param destinationElementLocator the locator of the webElement that you'll
     *                                  drop the sourceElement on (By xpath, id,
     *                                  selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeToElement(By sourceElementLocator, By destinationElementLocator) {
        try {
            WebElement sourceElement = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), sourceElementLocator).get(1));
            WebElement destinationElement = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), destinationElementLocator).get(1));

            String startLocation = sourceElement.getLocation().toString();

            try {
                new Actions(driverFactoryHelper.getDriver()).dragAndDrop(sourceElement, destinationElement).perform();
            } catch (Exception e) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), sourceElementLocator, e);
            }

            String endLocation = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), sourceElementLocator).get(1)).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (SHAFT.Properties.flags.validateSwipeToElement()) {
                if (!endLocation.equals(startLocation)) {
                    elementActionsHelper.passAction(driverFactoryHelper.getDriver(), sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, null);
                } else {
                    elementActionsHelper.failAction(driverFactoryHelper.getDriver(), reportMessage, sourceElementLocator);
                }
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), sourceElementLocator, throwable);
        }
        return this;
    }

    /**
     * Swipes an element with the desired x and y offset. Swiping direction is
     * determined by the positive/negative nature of the offset. Swiping destination
     * is determined by the value of the offset.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param xOffset        the horizontal offset by which the element should be
     *                       swiped. positive value is "right" and negative value is
     *                       "left"
     * @param yOffset        the vertical offset by which the element should be
     *                       swiped. positive value is "down" and negative value is
     *                       "up"
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeByOffset(By elementLocator, int xOffset, int yOffset) {
        try {
            WebElement sourceElement = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1));
            Point elementLocation = sourceElement.getLocation();
            String startLocation = elementLocation.toString();
            try {
                new Actions(driverFactoryHelper.getDriver()).dragAndDropBy(sourceElement, xOffset, yOffset).perform();
            } catch (Exception e) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, e);
            }

            String endLocation = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1)).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, null);
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), reportMessage, elementLocator);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Attempts to scroll the element into view in case of native mobile elements.
     *
     * @param targetElementLocator the locator of the webElement under test (By xpath, id,
     *                             selector, name ...etc.)
     * @param swipeDirection       SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By targetElementLocator, SwipeDirection swipeDirection) {
        return swipeElementIntoView(null, targetElementLocator, swipeDirection);
    }

    //TODO: swipeToEndOfView(SwipeDirection swipeDirection)
    //TODO: waitUntilElementIsNotVisible(String elementReferenceScreenshot)

    /**
     * Waits until a specific element is now visible on the current screen
     *
     * @param elementReferenceScreenshot relative path to the reference image from the local object repository
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("unchecked")
    public TouchActions waitUntilElementIsVisible(String elementReferenceScreenshot) {
        var visualIdentificationObjects = elementActionsHelper.waitForElementPresence(driverFactoryHelper.getDriver(), elementReferenceScreenshot);
        byte[] currentScreenImage = (byte[]) visualIdentificationObjects.get(0);
        byte[] referenceImage = (byte[]) visualIdentificationObjects.get(1);
        List<Integer> coordinates = (List<Integer>) visualIdentificationObjects.get(2);

        // prepare attachments
        var screenshotManager = new ScreenshotManager();
        var screenshot = screenshotManager.prepareImageForReport(currentScreenImage, "waitUntilElementIsVisible - Current Screen Image");
        var referenceScreenshot = screenshotManager.prepareImageForReport(referenceImage, "waitUntilElementIsVisible - Reference Screenshot");
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(referenceScreenshot);
        attachments.add(screenshot);

        if (!Collections.emptyList().equals(coordinates)) {
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, null);
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), "Couldn't find reference element on the current screen. If you can see it in the attached image then kindly consider cropping it and updating your reference image under this path \"" + elementReferenceScreenshot + "\".", null, attachments);
        }
        return this;
    }

    /**
     * Attempts to scroll element into view using the new W3C compliant actions for android and ios and AI for image identification
     *
     * @param elementReferenceScreenshot relative path to the reference image from the local object repository
     * @param swipeDirection             SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(String elementReferenceScreenshot, SwipeDirection swipeDirection) {
        return swipeElementIntoView(null, elementReferenceScreenshot, swipeDirection);
    }

    /**
     * Attempts to scroll element into view using the new W3C compliant actions for android and ios and AI for image identification
     *
     * @param scrollableElementLocator   the locator of the container/view/scrollable webElement that the scroll action will be performed inside
     * @param elementReferenceScreenshot relative path to the reference image from the local object repository
     * @param swipeDirection             SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("unchecked")
    public TouchActions swipeElementIntoView(By scrollableElementLocator, String elementReferenceScreenshot, SwipeDirection swipeDirection) {
        // Prepare attachments for reporting
        List<List<Object>> attachments = new LinkedList<>();
        try {
            //noinspection CaughtExceptionImmediatelyRethrown
            try {
                if (driverFactoryHelper.getDriver() instanceof AppiumDriver appiumDriver) {
                    // appium native application
                    var visualIdentificationObjects = attemptToSwipeElementIntoViewInNativeApp(scrollableElementLocator, elementReferenceScreenshot, swipeDirection);
                    byte[] currentScreenImage = (byte[]) visualIdentificationObjects.get(0);
                    byte[] referenceImage = (byte[]) visualIdentificationObjects.get(1);
                    List<Integer> coordinates = (List<Integer>) visualIdentificationObjects.get(2);

                    // prepare attachments
                    var screenshotManager = new ScreenshotManager();
                    var screenshot = screenshotManager.prepareImageForReport(currentScreenImage, "swipeElementIntoView - Current Screen Image");
                    var referenceScreenshot = screenshotManager.prepareImageForReport(referenceImage, "swipeElementIntoView - Reference Screenshot");
                    attachments = new LinkedList<>();
                    attachments.add(referenceScreenshot);
                    attachments.add(screenshot);

                    // If coordinates are empty then OpenCV couldn't find the element on screen
                    if (Collections.emptyList().equals(coordinates)) {
                        elementActionsHelper.failAction(driverFactoryHelper.getDriver(), "Couldn't find reference element on the current screen. If you can see it in the attached image then kindly consider cropping it and updating your reference image.", null, attachments);
                    }
                } else {
                    // Wait for element presence and get the needed data
                    var objects = elementActionsHelper.waitForElementPresence(driverFactoryHelper.getDriver(), elementReferenceScreenshot);
                    byte[] currentScreenImage = (byte[]) objects.get(0);
                    byte[] referenceImage = (byte[]) objects.get(1);
                    List<Integer> coordinates = (List<Integer>) objects.get(2);

                    // prepare attachments
                    var screenshotManager = new ScreenshotManager();
                    var screenshot = screenshotManager.prepareImageForReport(currentScreenImage, "swipeElementIntoView - Current Screen Image");
                    var referenceScreenshot = screenshotManager.prepareImageForReport(referenceImage, "swipeElementIntoView - Reference Screenshot");
                    attachments = new LinkedList<>();
                    attachments.add(referenceScreenshot);
                    attachments.add(screenshot);

                    // If coordinates are empty then OpenCV couldn't find the element on screen
                    if (Collections.emptyList().equals(coordinates)) {
                        elementActionsHelper.failAction(driverFactoryHelper.getDriver(), "Couldn't find reference element on the current screen. If you can see it in the attached image then kindly consider cropping it and updating your reference image.", null, attachments);
                    } else {
                        new Actions(driverFactoryHelper.getDriver()).scrollFromOrigin(WheelInput.ScrollOrigin.fromViewport(), coordinates.get(0), coordinates.get(1)).perform();
                    }
                }
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, null);
            } catch (AssertionError assertionError) {
                //bubble up
                throw assertionError;
            } catch (Exception exception) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), "Couldn't find reference element on the current screen. If you can see it in the attached image then kindly consider cropping it and updating your reference image.", null, attachments, exception);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), scrollableElementLocator, throwable);
        }
        return this;
    }

    /**
     * Attempts to scroll element into view using the new W3C compliant actions for android and ios
     *
     * @param scrollableElementLocator the locator of the container/view/scrollable webElement that the scroll action will be performed inside
     * @param targetElementLocator     the locator of the webElement that you want to scroll to under test (By xpath, id,
     *                                 selector, name ...etc.)
     * @param swipeDirection           SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By scrollableElementLocator, By targetElementLocator, SwipeDirection swipeDirection) {
        // Fix issue #641 Element locator is NULL by make internalScrollableElementLocator can be null and the condition become 'OR' not 'AND'
        try {
            try {
                if (driverFactoryHelper.getDriver() instanceof AppiumDriver appiumDriver) {
                    // appium native application
                    boolean isElementFound = attemptToSwipeElementIntoViewInNativeApp(scrollableElementLocator, targetElementLocator, swipeDirection);
                    if (Boolean.FALSE.equals(isElementFound)) {
                        elementActionsHelper.failAction(appiumDriver, targetElementLocator);
                    }
                } else {
                    // regular touch screen device
                    if (scrollableElementLocator != null) {
                        new Actions(driverFactoryHelper.getDriver()).moveToElement(((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), scrollableElementLocator).get(1))).scrollToElement(((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), targetElementLocator).get(1))).perform();
                    } else {
                        new Actions(driverFactoryHelper.getDriver()).scrollToElement(((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), targetElementLocator).get(1))).perform();
                    }
                }
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), targetElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            } catch (UnsupportedCommandException unsupportedCommandException) {
                throw unsupportedCommandException;
            } catch (Exception e) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), targetElementLocator, e);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), scrollableElementLocator, throwable);
        }
        return this;
    }

    /**
     * Attempts to scroll element into view using androidUIAutomator
     *
     * @param targetText element text to be used to swipe it into view
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(String targetText) {
        driverFactoryHelper.getDriver().findElement(AppiumBy.androidUIAutomator("new UiScrollable(new UiSelector().scrollable(true))"
                + ".scrollIntoView(new UiSelector().textContains(\"" + targetText + "\"))"));
        return this;
    }

    /**
     * Attempts to scroll element into view using androidUIAutomator
     *
     * @param targetText element text to be used to swipe it into view
     * @param movement   SwipeMovement.VERTICAL or HORIZONTAL
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(String targetText, SwipeMovement movement) {
        switch (movement) {
            case VERTICAL:
                driverFactoryHelper.getDriver().findElement(AppiumBy.androidUIAutomator("new UiScrollable(new UiSelector().scrollable(true))"
                        + ".scrollIntoView(new UiSelector().textContains(\"" + targetText + "\"))"));
                break;
            case HORIZONTAL:
                driverFactoryHelper.getDriver().findElement(AppiumBy.androidUIAutomator("new UiScrollable(new UiSelector()).setAsHorizontalList().scrollIntoView("
                        + "new UiSelector().textContains(\"" + targetText + "\"));"));
                break;
        }
        return this;
    }

    /**
     * Rotate between portrait and landscape modes
     *
     * @param orientation ScreenOrientation.LANDSCAPE or PORTRAIT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions rotate(ScreenOrientation orientation) {
        ((AndroidDriver) driverFactoryHelper.getDriver()).rotate(orientation);
        return this;
    }

    @SuppressWarnings("unchecked")
    private List<Object> attemptToSwipeElementIntoViewInNativeApp(By scrollableElementLocator, String targetElementImage, SwipeDirection swipeDirection) {
        boolean isElementFound = false;
        boolean canStillScroll = true;
        var isDiscrete = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);
        List<Object> visualIdentificationObjects;

        // force SHAFT back into the loop even if canStillScroll is false, or ignore it completely for the first 5 scroll attempts
        int blindScrollingAttempts = 0;

        do {
            // appium native device
            // Wait for element presence and get the needed data
            visualIdentificationObjects = elementActionsHelper.waitForElementPresence(driverFactoryHelper.getDriver(), targetElementImage);
            List<Integer> coordinates = (List<Integer>) visualIdentificationObjects.get(2);

            if (!Collections.emptyList().equals(coordinates)) {
                // element is already on screen
                isElementFound = true;
                ReportManager.logDiscrete("Element found on screen.");
            } else {
                // for the animated GIF:
                elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), null, "swipeElementIntoView", null, true);
                canStillScroll = attemptW3cCompliantActionsScroll(swipeDirection, scrollableElementLocator, null);
                if (!canStillScroll) {
                    // check if element can be found after scrolling to the end of the page
                    visualIdentificationObjects = elementActionsHelper.waitForElementPresence(driverFactoryHelper.getDriver(), targetElementImage);
                    coordinates = (List<Integer>) visualIdentificationObjects.get(2);
                    if (!Collections.emptyList().equals(coordinates)) {
                        isElementFound = true;
                        ReportManager.logDiscrete("Element found on screen.");
                    }
                }
            }
            blindScrollingAttempts++;
        } while (Boolean.FALSE.equals(isElementFound) && (blindScrollingAttempts < DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT || Boolean.TRUE.equals(canStillScroll)));
        ReportManagerHelper.setDiscreteLogging(isDiscrete);
        return visualIdentificationObjects;
    }

    private boolean attemptToSwipeElementIntoViewInNativeApp(By scrollableElementLocator, By targetElementLocator, SwipeDirection swipeDirection) {
        boolean isElementFound = false;
        boolean canStillScroll = true;
        var isDiscrete = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);

        // force SHAFT back into the loop even if canStillScroll is false, or ignore it completely for the first 5 scroll attempts
        int blindScrollingAttempts = 0;

        do {
            // appium native device
            if (elementActionsHelper.waitForElementPresenceWithReducedTimeout(driverFactoryHelper.getDriver(), targetElementLocator) > 0) {
                // element is already on screen
                isElementFound = true;
                ReportManager.logDiscrete("Element found on screen.");
            } else {
                // for the animated GIF:
                elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), null, "swipeElementIntoView", null, true);
                canStillScroll = attemptW3cCompliantActionsScroll(swipeDirection, scrollableElementLocator, targetElementLocator);
                if (!canStillScroll && elementActionsHelper.waitForElementPresenceWithReducedTimeout(driverFactoryHelper.getDriver(), targetElementLocator) > 0) {
                    // element was found after scrolling to the end of the page
                    isElementFound = true;
                    ReportManager.logDiscrete("Element found on screen.");
                }
            }
            blindScrollingAttempts++;
        } while (Boolean.FALSE.equals(isElementFound) && (blindScrollingAttempts < DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT || Boolean.TRUE.equals(canStillScroll)));
        ReportManagerHelper.setDiscreteLogging(isDiscrete);
        return isElementFound;
    }

    private void attemptUISelectorScroll(SwipeDirection swipeDirection, int scrollableElementInstanceNumber) {
        ReportManager.logDiscrete("Swiping to find Element using UiSelector.");
        int scrollingSpeed = 100;
        String scrollDirection = "Forward";
        ReportManager.logDiscrete("Swiping to find Element using UiSelector.");
        By androidUIAutomator = AppiumBy
                .androidUIAutomator("new UiScrollable(new UiSelector().scrollable(true).instance("
                        + scrollableElementInstanceNumber + ")).scroll" + scrollDirection + "(" + scrollingSpeed + ")");
        elementActionsHelper.getElementsCount(driverFactoryHelper.getDriver(), androidUIAutomator);
    }

    private boolean attemptW3cCompliantActionsScroll(SwipeDirection swipeDirection, By scrollableElementLocator, By targetElementLocator) {
        var logMessage = "Swiping to find Element using W3C Compliant Actions. SwipeDirection \"" + swipeDirection + "\"";
        if (targetElementLocator != null) {
            logMessage += ", TargetElementLocator \"" + targetElementLocator + "\"";
        }
        if (scrollableElementLocator != null) {
            logMessage += ", inside ScrollableElement \"" + scrollableElementLocator + "\"";
        }
        logMessage += ".";
        ReportManager.logDiscrete(logMessage);

        Dimension screenSize = driverFactoryHelper.getDriver().manage().window().getSize();
        boolean canScrollMore = true;

        var scrollParameters = new HashMap<>();

        if (scrollableElementLocator != null) {
            //scrolling inside an element
            Rectangle elementRectangle = ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), scrollableElementLocator).get(1)).getRect();
            scrollParameters.putAll(ImmutableMap.of(
                    "height", elementRectangle.getHeight() * 90 / 100
            ));
            //percent 0.5 works for UP/DOWN, optimized to 0.8 to scroll faster and introduced delay 1000ms after every scroll action to increase stability
            switch (swipeDirection) {
                case UP ->
                        scrollParameters.putAll(ImmutableMap.of("percent", 0.8, "height", elementRectangle.getHeight() * 90 / 100, "width", elementRectangle.getWidth(), "left", elementRectangle.getX(), "top", elementRectangle.getHeight() - 100));
                case DOWN ->
                        scrollParameters.putAll(ImmutableMap.of("percent", 0.8, "height", elementRectangle.getHeight() * 90 / 100, "width", elementRectangle.getWidth(), "left", elementRectangle.getX(), "top", 100));
                case RIGHT ->
                        scrollParameters.putAll(ImmutableMap.of("percent", 1, "height", elementRectangle.getHeight(), "width", elementRectangle.getWidth() * 70 / 100, "left", 100, "top", elementRectangle.getY()));
                case LEFT ->
                        scrollParameters.putAll(ImmutableMap.of("percent", 1, "height", elementRectangle.getHeight(), "width", elementRectangle.getWidth(), "left", elementRectangle.getX() + (elementRectangle.getWidth() * 50 / 100), "top", elementRectangle.getY()));
            }
        } else {
            //scrolling inside the screen
            scrollParameters.putAll(ImmutableMap.of(
                    "width", screenSize.getWidth(), "height", screenSize.getHeight() * 90 / 100,
                    "percent", 0.8
            ));
            switch (swipeDirection) {
                case UP -> scrollParameters.putAll(ImmutableMap.of("left", 0, "top", screenSize.getHeight() - 100));
                case DOWN -> scrollParameters.putAll(ImmutableMap.of("left", 0, "top", 100));
                // expected issues with RIGHT and LEFT
                case RIGHT -> scrollParameters.putAll(ImmutableMap.of("left", 100, "top", 0));
                case LEFT -> scrollParameters.putAll(ImmutableMap.of("left", screenSize.getWidth() - 100, "top", 0));
            }
        }

        if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
            scrollParameters.putAll(ImmutableMap.of(
                    "direction", swipeDirection.toString()
            ));
            canScrollMore = (Boolean) androidDriver.executeScript("mobile:scrollGesture", scrollParameters);
        } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
            scrollParameters.putAll(ImmutableMap.of(
                    "direction", swipeDirection.toString()
            ));
            //http://appium.github.io/appium-xcuitest-driver/4.16/execute-methods/#mobile-scroll
            var ret = iosDriver.executeScript("mobile:scroll", scrollParameters);
            canScrollMore = ret == null || (Boolean) ret;
        }
        var logMessageAfter = "Attempted to scroll using these parameters: \"" + scrollParameters + "\"";
        if (canScrollMore) {
            logMessageAfter += ", there is still more room to keep scrolling.";
        } else {
            logMessageAfter += ", there is no more room to keep scrolling.";
        }
        ReportManager.logDiscrete(logMessageAfter);
        return canScrollMore;
    }


    private void attemptPinchToZoomIn() {

        PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
        PointerInput finger2 = new PointerInput(PointerInput.Kind.TOUCH, "finger2");

        Dimension size = driverFactoryHelper.getDriver().manage().window().getSize();
        Point source = new Point(size.getWidth(), size.getHeight());

        Sequence pinchAndZoom1 = new Sequence(finger, 0);
        pinchAndZoom1.addAction(finger.createPointerMove(Duration.ofMillis(0),
                        PointerInput.Origin.viewport(), source.x / 2, source.y / 2))
                .addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()))
                .addAction(new Pause(finger, Duration.ofMillis(110)))
                .addAction(finger.createPointerMove(Duration.ofMillis(600),
                        PointerInput.Origin.viewport(), source.x / 3, source.y / 3))
                .addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));


        Sequence pinchAndZoom2 = new Sequence(finger2, 0);
        pinchAndZoom2.addAction(finger2.createPointerMove(Duration.ofMillis(0),
                        PointerInput.Origin.viewport(), source.x / 2, source.y / 2))
                .addAction(finger2.createPointerDown(PointerInput.MouseButton.LEFT.asArg()))
                .addAction(finger2.createPointerMove(Duration.ofMillis(600),
                        PointerInput.Origin.viewport(), source.x * 3 / 4, source.y * 3 / 4))
                .addAction(finger2.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));

        ((RemoteWebDriver) driverFactoryHelper.getDriver()).perform(asList(pinchAndZoom1, pinchAndZoom2));
    }


    private void attemptPinchToZoomOut() {

        PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
        PointerInput finger2 = new PointerInput(PointerInput.Kind.TOUCH, "finger2");

        Dimension size = driverFactoryHelper.getDriver().manage().window().getSize();
        Point source = new Point(size.getWidth(), size.getHeight());

        Sequence pinchAndZoom1 = new Sequence(finger, 0);
        pinchAndZoom1
                .addAction(finger.createPointerMove(Duration.ofMillis(0),
                        PointerInput.Origin.viewport(), source.x / 3, source.y / 3))
                .addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()))
                .addAction(new Pause(finger, Duration.ofMillis(110)))
                .addAction(finger.createPointerMove(Duration.ofMillis(600),
                        PointerInput.Origin.viewport(), source.x / 2, source.y / 2))
                .addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));


        Sequence pinchAndZoom2 = new Sequence(finger2, 0);
        pinchAndZoom2.addAction(finger2.createPointerMove(Duration.ofMillis(0),
                        PointerInput.Origin.viewport(), source.x * 3 / 4, source.y * 3 / 4))
                .addAction(finger2.createPointerDown(PointerInput.MouseButton.LEFT.asArg()))
                .addAction(new Pause(finger, Duration.ofMillis(100)))
                .addAction(finger2.createPointerMove(Duration.ofMillis(600),
                        PointerInput.Origin.viewport(), source.x / 2, source.y / 2))
                .addAction(finger2.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));

        ((RemoteWebDriver) driverFactoryHelper.getDriver()).perform(asList(pinchAndZoom1, pinchAndZoom2));
    }

    /**
     * Attempts to zoom the current screen IN/ OUT in case of zoom enabled screen.
     *
     * @param zoomDirection ZoomDirection.IN or OUT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions pinchToZoom(ZoomDirection zoomDirection) {
        try {
            switch (zoomDirection) {
                case IN -> attemptPinchToZoomIn();
                case OUT -> attemptPinchToZoomOut();
            }
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), zoomDirection.name(), null, null);
        return this;
    }


    public enum ZoomDirection {
        IN, OUT
    }


    /**
     * SwipeDirection; swiping UP means the screen will move downwards
     */
    public enum SwipeDirection {
        UP, DOWN, LEFT, RIGHT
    }

    public enum SwipeMovement {
        HORIZONTAL, VERTICAL
    }

    public enum SwipeTechnique {
        W3C_ACTIONS, UI_SELECTOR
    }

    public enum KeyboardKeys {
        GO(ImmutableMap.of("action", "go")), DONE(ImmutableMap.of("action", "done")), SEARCH(ImmutableMap.of("action", "search")), SEND(ImmutableMap.of("action", "send")),
        NEXT(ImmutableMap.of("action", "next")), PREVIOUS(ImmutableMap.of("action", "previous")), NORMAL(ImmutableMap.of("action", "normal")), UNSPECIFIED(ImmutableMap.of("action", "unspecified")), NONE(ImmutableMap.of("action", "none"));

        private final ImmutableMap<?, ?> value;

        KeyboardKeys(ImmutableMap<?, ?> type) {
            this.value = type;
        }

        private ImmutableMap<?, ?> getValue() {
            return value;
        }
    }

}