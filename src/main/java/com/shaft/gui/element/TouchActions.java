package com.shaft.gui.element;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.MobileBy;
import io.appium.java_client.MobileElement;
import io.appium.java_client.TouchAction;
import io.appium.java_client.touch.WaitOptions;
import io.appium.java_client.touch.offset.ElementOption;
import io.appium.java_client.touch.offset.PointOption;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Pause;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;

import java.time.Duration;
import java.util.List;

@SuppressWarnings("unused")
public class TouchActions {
    private static final int DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT = 10;
    private final WebDriver driver;

    public TouchActions(WebDriver driver) {
        this.driver = driver;
        RecordManager.startVideoRecording(driver);
    }

    /**
     * This is a convenience method to be able to call Element Actions from within the current Touch Actions instance.
     * <p>
     * Sample use would look like this:
     * new TouchActions(driver).tap(username_textbox).performElementAction().type(username_textbox, "username");
     *
     * @return a ElementActions object
     */
    public ElementActions performElementAction() {
        return new ElementActions(driver);
    }

    /**
     * Sends a keypress via the device keyboard.
     *
     * @param key the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public TouchActions nativeKeyboardKeyPress(KeyboardKeys key) {
        try {
            ((JavascriptExecutor) driver).executeScript("mobile: performEditorAction", key.getValue());
            ElementActions.passAction(driver, null, key.name());
        } catch (Exception rootCauseException) {
            ElementActions.failAction(driver, null, rootCauseException);
        }
        ElementActions.passAction(driver, null, key.name());
        return this;
    }

    /**
     * Taps an element once on a touch-enabled screen
     *
     * @param elementReferenceScreenshot relative path to the reference image from the local object repository, ends with /
     * @return a self-reference to be used to chain actions
     */
    public TouchActions tap(String elementReferenceScreenshot) {
        List<Object> screenshot = ElementActions.takeScreenshot(driver, null, "tap", null, true);

        if (BrowserFactory.isMobileNativeExecution()) {
            byte[] currentScreenImage = ((AppiumDriver<MobileElement>) driver).getScreenshotAs(OutputType.BYTES);
            List<Integer> coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage, 1);
            PointerInput input = new PointerInput(PointerInput.Kind.TOUCH, "finger1");
            Sequence tap = new Sequence(input, 0);
            tap.addAction(input.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), coordinates.get(0), coordinates.get(1)));
            tap.addAction(input.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
            tap.addAction(new Pause(input, Duration.ofMillis(200)));
            tap.addAction(input.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
            try {
                ((AppiumDriver<?>) driver).perform(ImmutableList.of(tap));
            } catch (UnsupportedCommandException exception) {
                ElementActions.failAction(driver, null, exception);
            }
        } else {
            byte[] currentScreenImage = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
            List<Integer> coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage, 1);
            (new org.openqa.selenium.interactions.touch.TouchActions(driver))
                    .down(coordinates.get(0), coordinates.get(1))
                    .up(coordinates.get(0), coordinates.get(1))
                    .perform();
        }
        ElementActions.passAction(driver, null, screenshot);
        return this;
    }

    /**
     * Taps an element once on a touch-enabled screen
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions tap(By elementLocator) {
        By internalElementLocator = elementLocator;
        if (ElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
            String elementText = "";
            try {
                if (BrowserFactory.isMobileNativeExecution()) {
                    elementText = driver.findElement(internalElementLocator).getAttribute("text");
                } else {
                    elementText = driver.findElement(internalElementLocator).getText();
                }
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, internalElementLocator, "tap", null, true);
            // takes screenshot before clicking the element out of view

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver))
                            .tap(ElementOption.element(driver.findElement(internalElementLocator))).perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).singleTap(driver.findElement(internalElementLocator)).perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, internalElementLocator);
        }
        return this;
    }

    /**
     * Double-Taps an element on a touch-enabled screen
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions doubleTap(By elementLocator) {
        By internalElementLocator = elementLocator;
        if (ElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
            String elementText = "";
            try {
                elementText = driver.findElement(internalElementLocator).getText();
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, internalElementLocator, "doubleTap", null, true);
            // takes screenshot before clicking the element out of view

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver))
                            .tap(ElementOption.element(driver.findElement(internalElementLocator)))
                            .tap(ElementOption.element(driver.findElement(internalElementLocator))).perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).doubleTap(driver.findElement(internalElementLocator)).perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, internalElementLocator);
        }
        return this;
    }

    /**
     * Performs a long-tap on an element to trigger the context menu on a
     * touch-enabled screen
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions longTap(By elementLocator) {
        By internalElementLocator = elementLocator;
        if (ElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
            String elementText = "";
            try {
                elementText = driver.findElement(internalElementLocator).getText();
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, internalElementLocator, "longPress", null, true);
            // takes screenshot before clicking the element out of view

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver))
                            .longPress(ElementOption.element(driver.findElement(internalElementLocator))).perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).longPress(driver.findElement(internalElementLocator)).perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, internalElementLocator);
        }
        return this;
    }

    /**
     * Swipes the sourceElement onto the destinationElement on a touch-enabled
     * screen
     *
     * @param sourceElementLocator      the locator of the webElement that needs to
     *                                  be swiped (By xpath, id, selector, name
     *                                  ...etc)
     * @param destinationElementLocator the locator of the webElement that you'll
     *                                  drop the sourceElement on (By xpath, id,
     *                                  selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeToElement(By sourceElementLocator, By destinationElementLocator) {
        By internalSourceElementLocator = sourceElementLocator;
        By internalDestinationElementLocator = destinationElementLocator;
        if (ElementActions.identifyUniqueElement(driver, internalSourceElementLocator)
                && ElementActions.identifyUniqueElement(driver, internalDestinationElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalSourceElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalSourceElementLocator);
            internalDestinationElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalDestinationElementLocator);

            WebElement sourceElement = driver.findElement(internalSourceElementLocator);
            WebElement destinationElement = driver.findElement(internalDestinationElementLocator);

            String startLocation = sourceElement.getLocation().toString();

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver)).press(ElementOption.element(sourceElement))
                            .moveTo(PointOption.point(destinationElement.getLocation())).release().perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).clickAndHold(sourceElement).release(destinationElement).perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, internalSourceElementLocator, e);
            }

            String endLocation = driver.findElement(internalSourceElementLocator).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                ElementActions.passAction(driver, internalSourceElementLocator, reportMessage);
            } else {
                ElementActions.failAction(driver, reportMessage, internalSourceElementLocator);
            }
        } else {
            ElementActions.failAction(driver, internalSourceElementLocator);
        }
        return this;
    }

    /**
     * Swipes an element with the desired x and y offset. Swiping direction is
     * determined by the positive/negative nature of the offset. Swiping destination
     * is determined by the value of the offset.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param xOffset        the horizontal offset by which the element should be
     *                       swiped. positive value is "right" and negative value is
     *                       "left"
     * @param yOffset        the vertical offset by which the element should be
     *                       swiped. positive value is "down" and negative value is
     *                       "up"
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeByOffset(By elementLocator, int xOffset, int yOffset) {
        By internalElementLocator = elementLocator;
        if (ElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);

            WebElement sourceElement = driver.findElement(internalElementLocator);
            Point elementLocation = sourceElement.getLocation();
            String startLocation = elementLocation.toString();

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver))
                            .press(ElementOption.element(sourceElement)).moveTo(PointOption
                            .point(elementLocation.getX() + xOffset, elementLocation.getY() + yOffset))
                            .release().perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).clickAndHold(sourceElement).moveByOffset(xOffset, yOffset).release()
                            .perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, internalElementLocator, e);
            }

            String endLocation = driver.findElement(internalElementLocator).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                ElementActions.passAction(driver, internalElementLocator, reportMessage);
            } else {
                ElementActions.failAction(driver, reportMessage, internalElementLocator);
            }
        } else {
            ElementActions.failAction(driver, internalElementLocator);
        }
        return this;
    }

    /**
     * Attempts to scroll the element into view in case of native mobile elements.
     *
     * @param targetElementLocator the locator of the webElement under test (By xpath, id,
     *                             selector, name ...etc)
     * @param swipeDirection       SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By targetElementLocator, SwipeDirection swipeDirection) {
        return swipeElementIntoView(targetElementLocator, swipeDirection, 0);
    }

    /**
     * Attempts to scroll the element into view in case of native mobile elements.
     *
     * @param targetElementLocator            the locator of the webElement under test (By xpath, id,
     *                                        selector, name ...etc)
     * @param swipeDirection                  SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @param scrollableElementInstanceNumber in case of multiple scrollable views, insert the instance number here (starts with 0)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By targetElementLocator, SwipeDirection swipeDirection, int scrollableElementInstanceNumber) {
        By internalElementLocator = targetElementLocator;
        internalElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
        try {
            if (driver instanceof AppiumDriver<?>) {
                // appium native application
                boolean isElementFound = attemptToSwipeElementIntoViewInNativeApp(internalElementLocator, swipeDirection, scrollableElementInstanceNumber);
                if (Boolean.FALSE.equals(isElementFound)) {
                    ElementActions.failAction(driver, internalElementLocator);
                }
            } else {
                // regular touch screen device
                if (ElementActions.identifyUniqueElement(driver, internalElementLocator)) {
                    Point elementLocation = driver.findElement(internalElementLocator).getLocation();
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).scroll(elementLocation.getX(), elementLocation.getY()).perform();
                } else {
                    ElementActions.failAction(driver, internalElementLocator);
                }
            }
            ElementActions.passAction(driver, internalElementLocator);
        } catch (Exception e) {
            ElementActions.failAction(driver, internalElementLocator, e);
        }
        return this;
    }

    private boolean attemptToSwipeElementIntoViewInNativeApp(By elementLocator, SwipeDirection swipeDirection, int scrollableElementInstanceNumber) {
        boolean isElementFound = false;
        int attemptsToFindElement = 0;
        By androidUIAutomator = MobileBy.AndroidUIAutomator("new UiScrollable(new UiSelector().scrollable(true)).scrollForward()");
        do {
            // appium native device
            if (!driver.findElements(elementLocator).isEmpty()
                    && ElementActions.identifyUniqueElement(driver, elementLocator)) {
                // element is already on screen
                isElementFound = true;
                ReportManager.logDiscrete("Element found on screen.");
            } else {
                // for the animated GIF:
                ElementActions.takeScreenshot(driver, elementLocator, "swipeElementIntoView", null, true);
                ReportManager.logDiscrete("Swiping to find Element.");
                if (System.getProperty("targetOperatingSystem").equals("Android")) {
                    By androidUIAutomator = MobileBy.AndroidUIAutomator("new UiScrollable(new UiSelector().scrollable(true)).scrollForward()");
                    int scrollableElementsCount = ElementActions.getElementsCount(driver, androidUIAutomator);
                    if (scrollableElementsCount == 1) {
                        //this line will fluent wait for the scrollable element and initiate a one screen scroll
                        ElementActions.identifyUniqueElement(driver, androidUIAutomator);
                    } else {
                        androidUIAutomator = MobileBy.AndroidUIAutomator("new UiScrollable(new UiSelector().scrollable(true).instance(" + scrollableElementInstanceNumber + ")).scrollForward()");
                        driver.findElement(androidUIAutomator);
                    }
                } else {
                    swipeScreen(swipeDirection);
                }
                attemptsToFindElement++;
            }
        } while (Boolean.FALSE.equals(isElementFound) && attemptsToFindElement < DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT);
        // TODO: devise a way to break the loop when no further scrolling options are
        // available. do not use visual comparison which is the easy but costly way to
        // do it.
        return isElementFound;
    }

    private void attemptTouchActionScroll(SwipeDirection swipeDirection) {
        Dimension screenSize = driver.manage().window().getSize();
        Point startingPoint = new Point(0, 0);
        Point endingPoint = new Point(0, 0);

        switch (swipeDirection) {
            case DOWN -> {
                startingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight() * 50 / 100);
                endingPoint = new Point(screenSize.getWidth() / 2, 0);
            }
            case UP -> {
                startingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight() * 50 / 100);
                endingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight());
            }
            case RIGHT -> {
                startingPoint = new Point(screenSize.getWidth() * 50 / 100, screenSize.getHeight() / 2);
                endingPoint = new Point(0, screenSize.getHeight() / 2);
            }
            case LEFT -> {
                startingPoint = new Point(screenSize.getWidth() * 50 / 100, screenSize.getHeight() / 2);
                endingPoint = new Point(screenSize.getWidth(), screenSize.getHeight() / 2);
            }
        }
        WaitOptions delay = WaitOptions.waitOptions(Duration.ofMillis(1000));
        (new TouchAction<>((AppiumDriver<?>) driver)).press(PointOption.point(startingPoint)).waitAction(delay)
                .moveTo(PointOption.point(endingPoint)).waitAction(delay).release().perform();
    }

    /**
     * Performs swipe from the center of screen
     * Reference: http://appium.io/docs/en/writing-running-appium/tutorial/swipe/simple-screen/
     *
     * @param dir the direction of swipe
     * @version java-client: 7.3.0
     **/
    private void swipeScreen(SwipeDirection dir) {
        // Animation default time:
        //  - Android: 300 ms
        //  - iOS: 200 ms
        // final value depends on your app and could be greater
        final int ANIMATION_TIME = 300; // ms

        final int PRESS_TIME = 300; // ms

        int edgeBorder = 10; // better avoid edges
        PointOption pointOptionStart, pointOptionEnd;

        // init screen variables
        Dimension dims = driver.manage().window().getSize();

        // init start point = center of screen
        pointOptionStart = PointOption.point(dims.width / 2, dims.height / 2);

        switch (dir) {
            case DOWN -> pointOptionEnd = PointOption.point(dims.width / 2, dims.height - edgeBorder);
            case UP -> pointOptionEnd = PointOption.point(dims.width / 2, edgeBorder);
            case LEFT -> pointOptionEnd = PointOption.point(edgeBorder, dims.height / 2);
            case RIGHT -> pointOptionEnd = PointOption.point(dims.width - edgeBorder, dims.height / 2);
            default -> throw new IllegalArgumentException("swipeScreen(): dir: '" + dir + "' NOT supported");
        }

        // execute swipe using TouchAction
        try {
            (new TouchAction<>((AppiumDriver<?>) driver))
                    .press(pointOptionStart)
                    // a bit more reliable when we add small wait
                    .waitAction(WaitOptions.waitOptions(Duration.ofMillis(PRESS_TIME)))
                    .moveTo(pointOptionEnd)
                    .release().perform();
        } catch (Exception e) {
            ReportManagerHelper.log(e);
            return;
        }

        // always allow swipe action to complete
        try {
            Thread.sleep(ANIMATION_TIME);
        } catch (InterruptedException e) {
            // ignore
        }
    }

    public enum SwipeDirection {
        UP, DOWN, LEFT, RIGHT
    }

    public enum KeyboardKeys {
        GO(ImmutableMap.of("action", "go")), DONE(ImmutableMap.of("action", "done")), SEARCH(ImmutableMap.of("action", "search")), SEND(ImmutableMap.of("action", "send")),
        NEXT(ImmutableMap.of("action", "next")), PREVIOUS(ImmutableMap.of("action", "previous")), NORMAL(ImmutableMap.of("action", "normal")), UNSPECIFIED(ImmutableMap.of("action", "unspecified")), NONE(ImmutableMap.of("action", "none"));

        private final ImmutableMap value;

        KeyboardKeys(ImmutableMap type) {
            this.value = type;
        }

        protected ImmutableMap getValue() {
            return value;
        }
    }

}
