package com.shaft.gui.element;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.*;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Duration;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

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

    @Override
    public TouchActions and() {
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
            if (driverFactoryHelper.getDriver() instanceof RemoteWebDriver remoteWebDriver) {
                new SynchronizationManager(driverFactoryHelper.getDriver()).fluentWait(false).until(d -> {
                    boolean isKeyboardShown = Boolean.parseBoolean(String.valueOf(remoteWebDriver.executeScript("mobile: isKeyboardShown")));
                    if (isKeyboardShown)
                        remoteWebDriver.executeScript("mobile: performEditorAction", key.getValue());
                    return isKeyboardShown;
                });
            }
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
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).doubleClick(elementLocator);
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
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).clickAndHold(elementLocator);
        return this;
    }

    /**
     * Send the currently active app to the background, and return after a certain number of seconds.
     *
     * @param secondsToSpendInTheBackground number of seconds before returning to the app
     * @return a self-reference to be used to chain actions
     */
    public TouchActions sendAppToBackground(int secondsToSpendInTheBackground) {
        if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
            androidDriver.runAppInBackground(Duration.ofSeconds(secondsToSpendInTheBackground));
        } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
            iosDriver.runAppInBackground(Duration.ofSeconds(secondsToSpendInTheBackground));
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
        }
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
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
        if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
            androidDriver.activateApp(appPackageName);
        } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
            iosDriver.activateApp(appPackageName);
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
        }
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        return this;
    }

    /**
     * Uploads a file to the device or simulator/emulator. This is particularly useful for BrowserStack
     * and other cloud-based mobile testing platforms that require files to be uploaded to the device
     * before they can be used in tests (e.g., for file upload scenarios, camera roll testing, etc.).
     * <p>
     * For Android: Uploads the file to the device's external storage or specified path.
     * For iOS: Uploads the file to the app's sandbox container.
     * <p>
     * Note: The file path on the device and the actual behavior may vary depending on the platform
     * and testing environment (local Appium vs BrowserStack vs other cloud providers).
     *
     * @param deviceFilePath the absolute path where the file should be stored on the device.
     *                       For Android example: "/sdcard/Download/sample.pdf" or "@com.example.app:id/files/sample.pdf"
     *                       For iOS example: "@com.example.app/Documents/sample.pdf"
     * @param localFilePath  the absolute or relative path to the file on the local machine that should be uploaded
     * @return a self-reference to be used to chain actions
     */
    public TouchActions pushFile(String deviceFilePath, String localFilePath) {
        try {
            File localFile = new File(localFilePath);
            if (!localFile.exists()) {
                throw new IOException("Local file not found: " + localFilePath);
            }
            
            byte[] fileContent = Files.readAllBytes(localFile.toPath());
            
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                androidDriver.pushFile(deviceFilePath, fileContent);
            } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                iosDriver.pushFile(deviceFilePath, fileContent);
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
                return this;
            }
            
            String testData = "Device Path: \"" + deviceFilePath + "\", Local File: \"" + localFilePath + "\"";
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), testData, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    /**
     * Uploads a file to the device or simulator/emulator using a File object. This is particularly useful for BrowserStack
     * and other cloud-based mobile testing platforms that require files to be uploaded to the device
     * before they can be used in tests (e.g., for file upload scenarios, camera roll testing, etc.).
     * <p>
     * For Android: Uploads the file to the device's external storage or specified path.
     * For iOS: Uploads the file to the app's sandbox container.
     * <p>
     * Note: The file path on the device and the actual behavior may vary depending on the platform
     * and testing environment (local Appium vs BrowserStack vs other cloud providers).
     *
     * @param deviceFilePath the absolute path where the file should be stored on the device.
     *                       For Android example: "/sdcard/Download/sample.pdf" or "@com.example.app:id/files/sample.pdf"
     *                       For iOS example: "@com.example.app/Documents/sample.pdf"
     * @param localFile      the File object representing the file on the local machine that should be uploaded
     * @return a self-reference to be used to chain actions
     */
    public TouchActions pushFile(String deviceFilePath, File localFile) {
        try {
            if (!localFile.exists()) {
                throw new IOException("Local file not found: " + localFile.getAbsolutePath());
            }
            
            byte[] fileContent = Files.readAllBytes(localFile.toPath());
            
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                androidDriver.pushFile(deviceFilePath, fileContent);
            } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                iosDriver.pushFile(deviceFilePath, fileContent);
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
                return this;
            }
            
            String testData = "Device Path: \"" + deviceFilePath + "\", Local File: \"" + localFile.getAbsolutePath() + "\"";
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), testData, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    /**
     * Downloads a file from the device or simulator/emulator to the local machine. This is useful for
     * retrieving files that were generated or modified during test execution on mobile devices.
     * <p>
     * For Android: Downloads the file from the device's file system.
     * For iOS: Downloads the file from the app's sandbox container.
     * <p>
     * Note: The file path on the device and the actual behavior may vary depending on the platform
     * and testing environment (local Appium vs BrowserStack vs other cloud providers).
     *
     * @param deviceFilePath    the absolute path to the file on the device that should be downloaded.
     *                          For Android example: "/sdcard/Download/sample.pdf"
     *                          For iOS example: "@com.example.app/Documents/sample.pdf"
     * @param localFilePath     the absolute or relative path where the downloaded file should be saved on the local machine
     * @return a self-reference to be used to chain actions
     */
    public TouchActions pullFile(String deviceFilePath, String localFilePath) {
        try {
            byte[] fileContent;
            
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                fileContent = androidDriver.pullFile(deviceFilePath);
            } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                fileContent = iosDriver.pullFile(deviceFilePath);
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
                return this;
            }
            
            File localFile = new File(localFilePath);
            File parentDir = localFile.getParentFile();
            if (parentDir != null && !parentDir.exists()) {
                Files.createDirectories(parentDir.toPath());
            }
            
            Files.write(localFile.toPath(), fileContent);
            
            String testData = "Device Path: \"" + deviceFilePath + "\", Local File: \"" + localFilePath + "\"";
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), testData, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
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
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).dragAndDrop(sourceElementLocator, destinationElementLocator);
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
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).dragAndDropByOffset(elementLocator, xOffset, yOffset);
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
                    boolean isElementFound = attemptW3cCompliantActionsScroll(swipeDirection, scrollableElementLocator, targetElementLocator);
                    if (!isElementFound) {
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
        if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
            androidDriver.rotate(orientation);
        } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
            iosDriver.rotate(orientation);
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
        }
        return this;
    }

    @SuppressWarnings("unchecked")
    private List<Object> attemptToSwipeElementIntoViewInNativeApp(By scrollableElementLocator, String targetElementImage, SwipeDirection swipeDirection) {
        List<Object> visualIdentificationObjects;
        // appium native device
        // Wait for element presence and get the needed data
        visualIdentificationObjects = elementActionsHelper.waitForElementPresence(driverFactoryHelper.getDriver(), targetElementImage);
        List<Integer> coordinates = (List<Integer>) visualIdentificationObjects.get(2);

        if (!Collections.emptyList().equals(coordinates)) {
            // element is already on screen
            ReportManager.logDiscrete("Element found on screen.");
        } else {
            attemptW3cCompliantActionsScroll(swipeDirection, scrollableElementLocator, null);
        }
        return visualIdentificationObjects;
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

    private HashMap<Object, Object> prepareParameters(SwipeDirection swipeDirection, By scrollableElementLocator, By targetElementLocator) {
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
        scrollParameters.putAll(ImmutableMap.of(
                "direction", swipeDirection.toString()
        ));
        return scrollParameters;
    }

    private boolean performW3cCompliantScroll(HashMap<Object, Object> scrollParameters) {
        boolean canScrollMore = true;
        if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
            canScrollMore = Boolean.parseBoolean(String.valueOf(androidDriver.executeScript("mobile: scrollGesture", scrollParameters)));
        } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
            //http://appium.github.io/appium-xcuitest-driver/4.16/execute-methods/#mobile-scroll
            var ret = iosDriver.executeScript("mobile: scroll", scrollParameters);
            canScrollMore = ret == null || (Boolean) ret;
        }
        return canScrollMore;
    }

    private boolean attemptW3cCompliantActionsScroll(SwipeDirection swipeDirection, By scrollableElementLocator, By targetElementLocator) {
        var scrollParameters = prepareParameters(swipeDirection, scrollableElementLocator, targetElementLocator);
        AtomicBoolean canScrollMore = new AtomicBoolean(true);

        new SynchronizationManager(driverFactoryHelper.getDriver()).fluentWait().until(f -> {
            // for the animated GIF:
            elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), null, "swipeElementIntoView", null, true);

            var elementExistsOnViewPort = !driverFactoryHelper.getDriver().findElements(targetElementLocator).isEmpty();
            if (elementExistsOnViewPort)
                return true;
            canScrollMore.set(performW3cCompliantScroll(scrollParameters));
            elementExistsOnViewPort = !driverFactoryHelper.getDriver().findElements(targetElementLocator).isEmpty();
            if (!canScrollMore.get() && !elementExistsOnViewPort)
                throw new RuntimeException("Element not found after scrolling to the end of the page.");
            return elementExistsOnViewPort;
        });
        ReportManager.logDiscrete("Element found on screen.");
        return true;
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