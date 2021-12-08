package com.shaft.gui.element;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.Pause;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.remote.RemoteWebElement;

import java.time.Duration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Arrays.asList;

@SuppressWarnings({"unused"})
public class TouchActions {
    // TODO: migrate away from all deprecated methods
    // https://github.com/appium/java-client/blob/087df2052abc177cea446825c48e3ab297a8ad6b/docs/v7-to-v8-migration-guide.md#touch-actions
    private static final int DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT = 10;
    private static final boolean CAPTURE_CLICKED_ELEMENT_TEXT = Boolean.valueOf(System.getProperty("captureClickedElementText"));
    private final WebDriver driver;

    public TouchActions(WebDriver driver) {
        this.driver = driver;
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
            ((AppiumDriver)driver).executeScript("mobile: performEditorAction", key.getValue());
            WebDriverElementActions.passAction(driver, null, key.name());
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
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
            if (driver instanceof AndroidDriver androidDriver){
                androidDriver.hideKeyboard();
            }else if (driver instanceof IOSDriver iosDriver){
                iosDriver.hideKeyboard();
            }else{
                WebDriverElementActions.failAction(driver, null);
            }
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
                ((AppiumDriver) driver).perform(ImmutableList.of(tap));
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
        // Override current locator with the aiGeneratedElementLocator
        By internalElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(elementLocator);
        if (WebDriverElementActions.identifyUniqueElement(driver, internalElementLocator)) {
            String elementText = "";
            if (CAPTURE_CLICKED_ELEMENT_TEXT) {
                try {
                    if (DriverFactoryHelper.isMobileNativeExecution()){
                    elementText = driver.findElement(internalElementLocator).getAttribute("text");
                } else{
                    elementText = driver.findElement(internalElementLocator).getText();
                }
            } catch(Exception e){
                // do nothing
            }
        }
            List<Object> screenshot = WebDriverElementActions.takeScreenshot(driver, internalElementLocator, "tap", null, true);
            // takes screenshot before clicking the element out of view

            try {
                if (driver instanceof AppiumDriver appiumDriver) {
                    new Actions(appiumDriver).click(driver.findElement(internalElementLocator)).perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).singleTap(driver.findElement(internalElementLocator)).perform();
                }
            } catch (Exception e) {
                WebDriverElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText == null || elementText.equals("")){
                elementText = internalElementLocator.toString();
            }
            WebDriverElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
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
                if (driver instanceof AppiumDriver appiumDriver) {
                    // appium native device
                    (new Actions(driver)).doubleClick(driver.findElement(internalElementLocator)).perform();
//                    (new TouchAction<>((appiumDriver))
//                            .tap(ElementOption.element(driver.findElement(internalElementLocator)))
//                            .tap(ElementOption.element(driver.findElement(internalElementLocator))).perform();
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
                if (driver instanceof AppiumDriver appiumDriver) {
                    // appium native device
                    new Actions(appiumDriver).clickAndHold(driver.findElement(internalElementLocator)).perform();
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
                if (driver instanceof AndroidDriver androidDriver){
                    androidDriver.runAppInBackground(Duration.ofSeconds(secondsToSpendInTheBackground));
                }else if (driver instanceof IOSDriver iosDriver){
                    iosDriver.runAppInBackground(Duration.ofSeconds(secondsToSpendInTheBackground));
                }else{
                    WebDriverElementActions.failAction(driver, null);
                }
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
            if (driver instanceof AndroidDriver androidDriver){
                androidDriver.activateApp(appPackageName);
            }else if (driver instanceof IOSDriver iosDriver){
                iosDriver.activateApp(appPackageName);
            }else{
                WebDriverElementActions.failAction(driver, null);
            }
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
    @Deprecated(forRemoval = true)
    public TouchActions restartApp() {
		if (DriverFactoryHelper.isMobileNativeExecution()) {
            if (driver instanceof AndroidDriver androidDriver){
                androidDriver.closeApp();
                androidDriver.launchApp();
            }else if (driver instanceof IOSDriver iosDriver){
                iosDriver.closeApp();
                iosDriver.launchApp();
            }else {
                WebDriverElementActions.failAction(driver, null);
            }
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
    @Deprecated(forRemoval = true)
    public TouchActions resetApp() {
		if (DriverFactoryHelper.isMobileNativeExecution()) {
            if (driver instanceof AndroidDriver androidDriver){
                androidDriver.resetApp();
            }else if (driver instanceof IOSDriver iosDriver){
                iosDriver.resetApp();
            }else {
                WebDriverElementActions.failAction(driver, null);
            }
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
                if (driver instanceof AppiumDriver appiumDriver) {
                    // appium native device
                    new Actions(appiumDriver).dragAndDrop(sourceElement, destinationElement).perform();
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
                if (driver instanceof AppiumDriver appiumDriver) {
                    // appium native device
                    new Actions(appiumDriver).dragAndDropBy(sourceElement, xOffset, yOffset).perform();
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
        return swipeElementIntoView(null, targetElementLocator, swipeDirection);
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
    @Deprecated(forRemoval = true)
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
    @Deprecated(forRemoval = true)
    public TouchActions swipeElementIntoView(By targetElementLocator, SwipeDirection swipeDirection, SwipeTechnique swipeTechnique, int scrollableElementInstanceNumber) {
        By internalElementLocator = targetElementLocator;
        internalElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
        try {
            if (driver instanceof AppiumDriver appiumDriver) {
                // appium native application
                boolean isElementFound = attemptToSwipeElementIntoViewInNativeApp(internalElementLocator, swipeDirection, swipeTechnique, scrollableElementInstanceNumber);
                if (Boolean.FALSE.equals(isElementFound)) {
                    WebDriverElementActions.failAction(appiumDriver, internalElementLocator);
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

    /**
     * Attempts to scroll element into view using the new W3C compliant actions for android and ios
     * @param scrollableElementLocator the locator of the container/view/scrollable webElement that the scroll action will be performed inside
     * @param targetElementLocator the locator of the webElement that you want to scroll to under test (By xpath, id,
     *                             selector, name ...etc)
     * @param swipeDirection       SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By scrollableElementLocator, By targetElementLocator, SwipeDirection swipeDirection) {
        By internalScrollableElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(scrollableElementLocator);
        By internalTargetElementLocator = WebDriverElementActions.updateLocatorWithAIGeneratedOne(targetElementLocator);

        try {
            if (driver instanceof AppiumDriver appiumDriver) {
                // appium native application
                boolean isElementFound = attemptToSwipeElementIntoViewInNativeApp(scrollableElementLocator, targetElementLocator, swipeDirection);
                if (Boolean.FALSE.equals(isElementFound)) {
                    WebDriverElementActions.failAction(appiumDriver, internalTargetElementLocator);
                }
            } else {
                // regular touch screen device
                if (WebDriverElementActions.identifyUniqueElement(driver, internalTargetElementLocator)) {
                    Point elementLocation = driver.findElement(internalTargetElementLocator).getLocation();
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).scroll(elementLocation.getX(), elementLocation.getY()).perform();
                } else {
                    WebDriverElementActions.failAction(driver, internalTargetElementLocator);
                }
            }
            WebDriverElementActions.passAction(driver, internalTargetElementLocator);
        } catch (Exception e) {
            WebDriverElementActions.failAction(driver, internalTargetElementLocator, e);
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
                    case W3C_ACTIONS -> attemptW3cCompliantActionsScroll(swipeDirection, null, elementLocator);
                    case UI_SELECTOR -> attemptUISelectorScroll(swipeDirection, scrollableElementInstanceNumber);
                }
                attemptsToFindElement++;
            }

            //attempting to change scrolling method if page source was not changed
            if (lastPageSourceBeforeSwiping.equals(driver.getPageSource())){
                if (swipeTechnique.equals(SwipeTechnique.W3C_ACTIONS)){
                    swipeTechnique = SwipeTechnique.UI_SELECTOR;
                }else{
                    swipeTechnique = SwipeTechnique.W3C_ACTIONS;
                }
            }
        } while (Boolean.FALSE.equals(isElementFound) && attemptsToFindElement < DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT);
        // TODO: devise a way to break the loop when no further scrolling options are
        // available. do not use visual comparison which is the easy but costly way to
        // do it.
        return isElementFound;
    }

    private boolean attemptToSwipeElementIntoViewInNativeApp(By scrollableElementLocator, By targetElementLocator, SwipeDirection swipeDirection) {
        boolean isElementFound = false;
        boolean canStillScroll = true;
        var isDiscrete = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);

        do {
            // appium native device
            if (!driver.findElements(targetElementLocator).isEmpty()
                    && WebDriverElementActions.isElementDisplayed(driver, targetElementLocator)) {
                // element is already on screen
                isElementFound = true;
                ReportManager.logDiscrete("Element found on screen.");
            } else {
                // for the animated GIF:
                WebDriverElementActions.takeScreenshot(driver, null, "swipeElementIntoView", null, true);
                canStillScroll = attemptW3cCompliantActionsScroll(swipeDirection, scrollableElementLocator, targetElementLocator);
            }
        } while (Boolean.FALSE.equals(isElementFound) && Boolean.TRUE.equals(canStillScroll));

        //final check after reaching the end of the scrollable area
        if (Boolean.FALSE.equals(isElementFound)
            && !driver.findElements(targetElementLocator).isEmpty()
            && WebDriverElementActions.isElementDisplayed(driver, targetElementLocator)){
                // element is already on screen
                isElementFound = true;
                ReportManager.logDiscrete("Element found on screen.");
        }
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
		WebDriverElementActions.getElementsCount(driver, androidUIAutomator);
    }

    private boolean attemptW3cCompliantActionsScroll(SwipeDirection swipeDirection, By scrollableElementLocator, By targetElementLocator) {
    	var logMessage = "Swiping to find Element using W3C Compliant Actions. SwipeDirection ["+swipeDirection+"], TargetElementLocator ["+targetElementLocator+"]";
        if (scrollableElementLocator != null){
            logMessage += ", inside ScrollableElement ["+scrollableElementLocator+"]";
        }
        logMessage += ".";
        ReportManager.logDiscrete(logMessage);

        Dimension screenSize = driver.manage().window().getSize();
        boolean canScrollMore = true;

        var scrollParameters =  new HashMap<>();

        if (scrollableElementLocator!=null) {
            //scrolling inside an element
            Rectangle elementRectangle = ((RemoteWebElement) driver.findElement(scrollableElementLocator)).getRect();
            scrollParameters.putAll(ImmutableMap.of(
                    "height", elementRectangle.getHeight() *90/100
            ));
            //percent 0.5 works for UP/DOWN, optimized to 0.8 to scroll faster and introduced delay 1000ms after every scroll action to increase stability
            switch (swipeDirection){
                case UP -> scrollParameters.putAll(ImmutableMap.of("percent", 0.8, "height", elementRectangle.getHeight() *90/100, "width", elementRectangle.getWidth(), "left", elementRectangle.getX(), "top", elementRectangle.getHeight() - 100));
                case DOWN -> scrollParameters.putAll(ImmutableMap.of("percent", 0.8, "height", elementRectangle.getHeight() *90/100, "width", elementRectangle.getWidth(), "left", elementRectangle.getX(), "top", 100));
                case RIGHT -> scrollParameters.putAll(ImmutableMap.of("percent", 1, "height", elementRectangle.getHeight(), "width", elementRectangle.getWidth()*70/100, "left", 100, "top", elementRectangle.getY()));
                case LEFT -> scrollParameters.putAll(ImmutableMap.of("percent", 1, "height", elementRectangle.getHeight(), "width", elementRectangle.getWidth(), "left", elementRectangle.getX()+(elementRectangle.getWidth() *50/100), "top", elementRectangle.getY()));
            }
        }else{
            //scrolling inside the screen
            scrollParameters.putAll(ImmutableMap.of(
                    "width", screenSize.getWidth(), "height", screenSize.getHeight() *90/100,
                    "percent", 0.8
            ));
            switch (swipeDirection){
                case UP -> scrollParameters.putAll(ImmutableMap.of("left", 0, "top", screenSize.getHeight() - 100));
                case DOWN -> scrollParameters.putAll(ImmutableMap.of("left", 0, "top", 100));
//                case RIGHT -> scrollParameters.putAll(ImmutableMap.of("left", 100, "top", 0));
//                case LEFT -> scrollParameters.putAll(ImmutableMap.of("left", screenSize.getWidth() - 100, "top", 0));
            }
        }

        if (driver instanceof AndroidDriver androidDriver){
            scrollParameters.putAll(ImmutableMap.of(
                    "direction", swipeDirection.toString()
            ));
            canScrollMore = (Boolean) ((JavascriptExecutor) androidDriver).executeScript("mobile: scrollGesture", scrollParameters);
        } else if (driver instanceof IOSDriver iosDriver) {
            scrollParameters.putAll(ImmutableMap.of(
                    "direction", swipeDirection.toString()
            ));
            canScrollMore = (Boolean) ((JavascriptExecutor) iosDriver).executeScript("mobile: scroll", scrollParameters);
        }
        var logMessageAfter = "Attempted to scroll using these parameters: ["+scrollParameters+"]";
        if (canScrollMore){
            logMessageAfter += ", there is still more room to keep scrolling.";
        }else{
            logMessageAfter += ", there is no more room to keep scrolling.";
        }
        try {
            //insert delay for scrolling to finish
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        ReportManager.logDiscrete(logMessageAfter);
        return canScrollMore;
    }
    
    
   
    private void attemptPinchToZoomIn()
    {

    	PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
        PointerInput finger2 = new PointerInput(PointerInput.Kind.TOUCH, "finger2");

        Dimension size = driver.manage().window().getSize();
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

        ((AppiumDriver) driver).perform(asList(pinchAndZoom1, pinchAndZoom2));
    }

    
    private void attemptPinchToZoomOut()
    {

    	PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
        PointerInput finger2 = new PointerInput(PointerInput.Kind.TOUCH, "finger2");

        Dimension size = driver.manage().window().getSize();
        Point source = new Point(size.getWidth(), size.getHeight());

        Sequence pinchAndZoom1 = new Sequence(finger, 0);
        pinchAndZoom1
        .addAction(finger.createPointerMove(Duration.ofMillis(0),
                PointerInput.Origin.viewport(), source.x / 3, source.y / 3))
        .addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()))
        .addAction(new Pause(finger, Duration.ofMillis(110)))
        .addAction(finger.createPointerMove(Duration.ofMillis(600),
                PointerInput.Origin.viewport(), source.x / 2, source.y / 2 ))
        .addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));


        Sequence pinchAndZoom2 = new Sequence(finger2, 0);
        pinchAndZoom2.addAction(finger2.createPointerMove(Duration.ofMillis(0),
                PointerInput.Origin.viewport(), source.x * 3 / 4, source.y * 3 / 4 ))
        .addAction(finger2.createPointerDown(PointerInput.MouseButton.LEFT.asArg()))
        .addAction(new Pause(finger, Duration.ofMillis(100)))
        .addAction(finger2.createPointerMove(Duration.ofMillis(600),
                PointerInput.Origin.viewport(), source.x / 2 , source.y / 2))
        .addAction(finger2.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));

        ((AppiumDriver) driver).perform(asList(pinchAndZoom1, pinchAndZoom2));
    }

    /**
     * Attempts to zoom the current screen IN/ OUT in case of zoom enabled screen.
     * @param zoomDirection       ZoomDirection.IN or OUT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions pinchToZoom(ZoomDirection zoomDirection) {
    	try {
    	switch (zoomDirection) {
    		case IN -> attemptPinchToZoomIn();
    		case OUT -> attemptPinchToZoomOut();
    	}
    	} catch(Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
        }
        WebDriverElementActions.passAction(driver, null, zoomDirection.name());
    	return this;
    }

   
     public enum ZoomDirection {
            IN, OUT
        }


    /**
     * SwipeDirection; swiping UP means the screen will move downwards
     *
     */
    public enum SwipeDirection {
        UP, DOWN, LEFT, RIGHT
    }

    public enum SwipeTechnique {
        W3C_ACTIONS, UI_SELECTOR
    }

    public enum KeyboardKeys {
        GO(ImmutableMap.of("action", "go")), DONE(ImmutableMap.of("action", "done")), SEARCH(ImmutableMap.of("action", "search")), SEND(ImmutableMap.of("action", "send")),
        NEXT(ImmutableMap.of("action", "next")), PREVIOUS(ImmutableMap.of("action", "previous")), NORMAL(ImmutableMap.of("action", "normal")), UNSPECIFIED(ImmutableMap.of("action", "unspecified")), NONE(ImmutableMap.of("action", "none"));

        private final ImmutableMap<?,?> value;

        KeyboardKeys(ImmutableMap<?,?> type) {
            this.value = type;
        }

        private ImmutableMap<?,?> getValue() {
            return value;
        }
    }

}
