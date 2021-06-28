package com.shaft.gui.element;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.MobileBy;
import io.appium.java_client.MobileDriver;
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
     * @return a WebDriverElementActions object
     */
    public ElementActions performElementAction() {
        return new ElementActions(driver);
    }

    /**
     * Sends a keypress via the device soft keyboard.
     *
     * @param key the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public TouchActions nativeKeyboardKeyPress(KeyboardKeys key) {
        try {
            ((JavascriptExecutor) driver).executeScript("mobile: performEditorAction", key.getValue());
            WebDriverElementActions.passAction(driver, null, key.name());
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
        }
        WebDriverElementActions.passAction(driver, null, key.name());
        return this;
    }
    
    /**
     * Hides the device native soft keyboard.
     * 
     * @return a self-reference to be used to chain actions
     */
    public TouchActions hideNativeKeyboard() {
        try {
            ((AppiumDriver<?>) driver).hideKeyboard();
            WebDriverElementActions.passAction(driver, null);
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
        }
        WebDriverElementActions.passAction(driver, null);
        return this;    }

    /**
     * Taps an element once on a touch-enabled screen
     *
     * @param elementReferenceScreenshot relative path to the reference image from the local object repository, ends with /
     * @return a self-reference to be used to chain actions
     */
    public TouchActions tap(String elementReferenceScreenshot) {
        List<Object> screenshot = WebDriverElementActions.takeScreenshot(driver, null, "tap", null, true);

        if (DriverFactoryHelper.isMobileNativeExecution()) {
            byte[] currentScreenImage = ((AppiumDriver) driver).getScreenshotAs(OutputType.BYTES);
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
                WebDriverElementActions.failAction(driver, null, exception);
            }
        } else {
            byte[] currentScreenImage = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
            List<Integer> coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage, 1);
            (new org.openqa.selenium.interactions.touch.TouchActions(driver))
                    .down(coordinates.get(0), coordinates.get(1))
                    .up(coordinates.get(0), coordinates.get(1))
                    .perform();
        }
        WebDriverElementActions.passAction(driver, null, screenshot);
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
        if (WebDriverElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
            String elementText = "";
            try {
                if (DriverFactoryHelper.isMobileNativeExecution()) {
                    elementText = driver.findElement(internalElementLocator).getAttribute("text");
                } else {
                    elementText = driver.findElement(internalElementLocator).getText();
                }
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = WebDriverElementActions.takeScreenshot(driver, internalElementLocator, "tap", null, true);
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
                WebDriverElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                WebDriverElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                WebDriverElementActions.passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            WebDriverElementActions.failAction(driver, internalElementLocator);
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
        if (WebDriverElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
            String elementText = "";
            try {
                elementText = driver.findElement(internalElementLocator).getText();
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = WebDriverElementActions.takeScreenshot(driver, internalElementLocator, "doubleTap", null, true);
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
                WebDriverElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                WebDriverElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                WebDriverElementActions.passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            WebDriverElementActions.failAction(driver, internalElementLocator);
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
        if (WebDriverElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
            String elementText = "";
            try {
                elementText = driver.findElement(internalElementLocator).getText();
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = WebDriverElementActions.takeScreenshot(driver, internalElementLocator, "longPress", null, true);
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
                WebDriverElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                WebDriverElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                WebDriverElementActions.passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            WebDriverElementActions.failAction(driver, internalElementLocator);
        }
        return this;
    }
    
    /**
     * Send the currently active app to the background, and return after a certain number of seconds.
     * 
     * @param secondsToSpendInTheBackground number of seconds before returning back to the app
     * @return a self-reference to be used to chain actions
     */
    public TouchActions sendAppToBackground(int secondsToSpendInTheBackground) {
    		if (DriverFactoryHelper.isMobileNativeExecution()) {
                ((MobileDriver) driver).runAppInBackground(Duration.ofSeconds(secondsToSpendInTheBackground));
                WebDriverElementActions.passAction(driver, null);
            }else {
                WebDriverElementActions.failAction(driver, null);
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
            ((MobileDriver) driver).activateApp(appPackageName);
            WebDriverElementActions.passAction(driver, null);
        }else {
            WebDriverElementActions.failAction(driver, null);
		}
		return this;
    }
    
    /**
     * Close the app which was provided in the capabilities at session creation and quits the session. Then re-Launches the app and restarts the session.
     * 
     * @return a self-reference to be used to chain actions
     */
    public TouchActions restartApp() {
		if (DriverFactoryHelper.isMobileNativeExecution()) {
            ((MobileDriver) driver).closeApp();
            ((MobileDriver) driver).launchApp();
            WebDriverElementActions.passAction(driver, null);
        }else {
	        WebDriverElementActions.failAction(driver, null);
		}
		return this;
    }
    
    /**
     * Resets the currently running app together with the session.
     * 
     * @return a self-reference to be used to chain actions
     */
    public TouchActions resetApp() {
		if (DriverFactoryHelper.isMobileNativeExecution()) {
            ((MobileDriver) driver).resetApp();
            WebDriverElementActions.passAction(driver, null);
        }else {
	        WebDriverElementActions.failAction(driver, null);
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
        if (WebDriverElementActions.identifyUniqueElement(driver, internalSourceElementLocator)
                && WebDriverElementActions.identifyUniqueElement(driver, internalDestinationElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalSourceElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalSourceElementLocator);
            internalDestinationElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalDestinationElementLocator);

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
                WebDriverElementActions.failAction(driver, internalSourceElementLocator, e);
            }

            String endLocation = driver.findElement(internalSourceElementLocator).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                WebDriverElementActions.passAction(driver, internalSourceElementLocator, reportMessage);
            } else {
                WebDriverElementActions.failAction(driver, reportMessage, internalSourceElementLocator);
            }
        } else {
            WebDriverElementActions.failAction(driver, internalSourceElementLocator);
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
        if (WebDriverElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);

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
                WebDriverElementActions.failAction(driver, internalElementLocator, e);
            }

            String endLocation = driver.findElement(internalElementLocator).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                WebDriverElementActions.passAction(driver, internalElementLocator, reportMessage);
            } else {
                WebDriverElementActions.failAction(driver, reportMessage, internalElementLocator);
            }
        } else {
            WebDriverElementActions.failAction(driver, internalElementLocator);
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
        return swipeElementIntoView(targetElementLocator, swipeDirection, SwipeTechnique.TOUCH_ACTIONS, 0);
    }

    /**
     * Attempts to scroll the element into view in case of native mobile elements.
     *
     * @param targetElementLocator the locator of the webElement under test (By xpath, id,
     *                             selector, name ...etc)
     * @param swipeDirection       SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @param swipeTechnique		SwipeTechnique.TOUCH_ACTIONS, or UI_SELECTOR
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By targetElementLocator, SwipeDirection swipeDirection, SwipeTechnique swipeTechnique) {
        return swipeElementIntoView(targetElementLocator, swipeDirection, swipeTechnique, 0);
    }

    /**
     * Attempts to scroll the element into view in case of native mobile elements.
     *
     * @param targetElementLocator            the locator of the webElement under test (By xpath, id,
     *                                        selector, name ...etc)
     * @param swipeDirection                  SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @param swipeTechnique		SwipeTechnique.TOUCH_ACTIONS, or UI_SELECTOR
     * @param scrollableElementInstanceNumber in case of multiple scrollable views, insert the instance number here (starts with 0)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By targetElementLocator, SwipeDirection swipeDirection, SwipeTechnique swipeTechnique, int scrollableElementInstanceNumber) {
        By internalElementLocator = targetElementLocator;
        internalElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
        try {
            if (driver instanceof AppiumDriver<?>) {
                // appium native application
                boolean isElementFound = attemptToSwipeElementIntoViewInNativeApp(internalElementLocator, swipeDirection, swipeTechnique, scrollableElementInstanceNumber);
                if (Boolean.FALSE.equals(isElementFound)) {
                    WebDriverElementActions.failAction(driver, internalElementLocator);
                }
            } else {
                // regular touch screen device
                if (WebDriverElementActions.identifyUniqueElement(driver, internalElementLocator)) {
                    Point elementLocation = driver.findElement(internalElementLocator).getLocation();
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).scroll(elementLocation.getX(), elementLocation.getY()).perform();
                } else {
                    WebDriverElementActions.failAction(driver, internalElementLocator);
                }
            }
            WebDriverElementActions.passAction(driver, internalElementLocator);
        } catch (Exception e) {
            WebDriverElementActions.failAction(driver, internalElementLocator, e);
        }
        return this;
    }

    private boolean attemptToSwipeElementIntoViewInNativeApp(By elementLocator, SwipeDirection swipeDirection, SwipeTechnique swipeTechnique, int scrollableElementInstanceNumber) {
        boolean isElementFound = false;
        int attemptsToFindElement = 0;
        String lastPageSourceBeforeSwiping = "";

        do {
            // appium native device
            if (!driver.findElements(elementLocator).isEmpty()
                    && WebDriverElementActions.isElementDisplayed(driver, elementLocator)) {
                // element is already on screen
                isElementFound = true;
                ReportManager.logDiscrete("Element found on screen.");
            } else {
                // for the animated GIF:
                WebDriverElementActions.takeScreenshot(driver, elementLocator, "swipeElementIntoView", null, true);
                lastPageSourceBeforeSwiping = driver.getPageSource();
                switch (swipeTechnique) {
                    case TOUCH_ACTIONS -> attemptTouchActionScroll(swipeDirection);
                    case UI_SELECTOR -> attemptUISelectorScroll(swipeDirection, scrollableElementInstanceNumber);
                }
                attemptsToFindElement++;
            }
        } while (Boolean.FALSE.equals(isElementFound) && attemptsToFindElement < DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT && !lastPageSourceBeforeSwiping.equals(driver.getPageSource()));
        // TODO: devise a way to break the loop when no further scrolling options are
        // available. do not use visual comparison which is the easy but costly way to
        // do it.
        return isElementFound;
    }
    
    private void attemptUISelectorScroll(SwipeDirection swipeDirection, int scrollableElementInstanceNumber) {
    	ReportManager.logDiscrete("Swiping to find Element using UiSelector.");
		int scrollingSpeed = 100;
		String scrollDirection = "Forward";
		ReportManager.logDiscrete("Swiping to find Element using UiSelector.");
		By androidUIAutomator = MobileBy
				.AndroidUIAutomator("new UiScrollable(new UiSelector().scrollable(true).instance("
						+ scrollableElementInstanceNumber + ")).scroll" + scrollDirection + "(" + scrollingSpeed + ")");
		WebDriverElementActions.getElementsCount(driver, androidUIAutomator);
    }

    private void attemptTouchActionScroll(SwipeDirection swipeDirection) {
    	ReportManager.logDiscrete("Swiping to find Element using TouchAction.");
        Dimension screenSize = driver.manage().window().getSize();
        Point startingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight() / 2);
        Point endingPoint = new Point(0, 0);

        switch (swipeDirection) {
            case DOWN -> endingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight() * 80/100);
            case UP -> endingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight() * 20/100);
            case LEFT -> endingPoint = new Point(screenSize.getWidth() * 80/100, screenSize.getHeight() / 2);
            case RIGHT -> endingPoint = new Point(screenSize.getWidth() * 20/100, screenSize.getHeight() / 2);
        }
        
        WaitOptions delay = WaitOptions.waitOptions(Duration.ofMillis(300));
        (new TouchAction<>((AppiumDriver<?>) driver))
        		.press(PointOption.point(startingPoint))
        		.waitAction(delay)
                .moveTo(PointOption.point(endingPoint))
                .release().perform();
    }

    /**
     * SwipeDirection; swiping UP means the screen will move downwards
     *
     */
    public enum SwipeDirection {
        UP, DOWN, LEFT, RIGHT
    }

    public enum SwipeTechnique {
        TOUCH_ACTIONS, UI_SELECTOR
    }

    public enum KeyboardKeys {
        GO(ImmutableMap.of("action", "go")), DONE(ImmutableMap.of("action", "done")), SEARCH(ImmutableMap.of("action", "search")), SEND(ImmutableMap.of("action", "send")),
        NEXT(ImmutableMap.of("action", "next")), PREVIOUS(ImmutableMap.of("action", "previous")), NORMAL(ImmutableMap.of("action", "normal")), UNSPECIFIED(ImmutableMap.of("action", "unspecified")), NONE(ImmutableMap.of("action", "none"));

        private final ImmutableMap<?,?> value;

        KeyboardKeys(ImmutableMap<?,?> type) {
            this.value = type;
        }

        protected ImmutableMap<?,?> getValue() {
            return value;
        }
    }

}
