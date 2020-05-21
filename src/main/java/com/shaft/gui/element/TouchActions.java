package com.shaft.gui.element;

import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.TouchAction;
import io.appium.java_client.touch.offset.ElementOption;
import io.appium.java_client.touch.offset.PointOption;
import org.openqa.selenium.*;

import java.util.List;

public class TouchActions {
    private static final int DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT = 10;
    private final WebDriver driver;
    // TODO: add to appium properties. divide it to execution properties and
    // platform properties

    public TouchActions(WebDriver driver) {
        this.driver = driver;
        RecordManager.startVideoRecording();
    }

    /**
     * Taps an element once on a touch-enabled screen
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public TouchActions tap(By elementLocator) {
        if (ElementActions.identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = ElementActions.updateLocatorWithAIGenratedOne(elementLocator);
            String elementText = "";
            try {
                elementText = driver.findElement(elementLocator).getText();
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, elementLocator, "tap", null, true);
            // takes screenshot before clicking the element out of view

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver))
                            .tap(ElementOption.element(driver.findElement(elementLocator))).perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).singleTap(driver.findElement(elementLocator)).perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, elementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, elementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, elementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, elementLocator);
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
        if (ElementActions.identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = ElementActions.updateLocatorWithAIGenratedOne(elementLocator);
            String elementText = "";
            try {
                elementText = driver.findElement(elementLocator).getText();
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, elementLocator, "doubleTap", null, true);
            // takes screenshot before clicking the element out of view

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver))
                            .tap(ElementOption.element(driver.findElement(elementLocator)))
                            .tap(ElementOption.element(driver.findElement(elementLocator))).perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).doubleTap(driver.findElement(elementLocator)).perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, elementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, elementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, elementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, elementLocator);
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
        if (ElementActions.identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = ElementActions.updateLocatorWithAIGenratedOne(elementLocator);
            String elementText = "";
            try {
                elementText = driver.findElement(elementLocator).getText();
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, elementLocator, "longPress", null, true);
            // takes screenshot before clicking the element out of view

            try {
                if (driver instanceof AppiumDriver<?>) {
                    // appium native device
                    (new TouchAction<>((AppiumDriver<?>) driver))
                            .longPress(ElementOption.element(driver.findElement(elementLocator))).perform();
                } else {
                    // regular touch screen device
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).longPress(driver.findElement(elementLocator)).perform();
                }
            } catch (Exception e) {
                ElementActions.failAction(driver, elementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, elementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, elementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, elementLocator);
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
        if (ElementActions.identifyUniqueElement(driver, sourceElementLocator)
                && ElementActions.identifyUniqueElement(driver, destinationElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            sourceElementLocator = ElementActions.updateLocatorWithAIGenratedOne(sourceElementLocator);
            destinationElementLocator = ElementActions.updateLocatorWithAIGenratedOne(destinationElementLocator);

            WebElement sourceElement = driver.findElement(sourceElementLocator);
            WebElement destinationElement = driver.findElement(destinationElementLocator);

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
                ElementActions.failAction(driver, sourceElementLocator, e);
            }

            String endLocation = driver.findElement(sourceElementLocator).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                ElementActions.passAction(driver, sourceElementLocator, reportMessage);
            } else {
                ElementActions.failAction(driver, reportMessage, sourceElementLocator);
            }
        } else {
            ElementActions.failAction(driver, sourceElementLocator);
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
        if (ElementActions.identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = ElementActions.updateLocatorWithAIGenratedOne(elementLocator);

            WebElement sourceElement = driver.findElement(elementLocator);
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
                ElementActions.failAction(driver, elementLocator, e);
            }

            String endLocation = driver.findElement(elementLocator).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                ElementActions.passAction(driver, elementLocator, reportMessage);
            } else {
                ElementActions.failAction(driver, reportMessage, elementLocator);
            }
        } else {
            ElementActions.failAction(driver, elementLocator);
        }
        return this;
    }

    /**
     * Attempts to scroll the element into view in case of native mobile elements.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param swipeDirection SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By elementLocator, SwipeDirection swipeDirection) {
        return swipeElementIntoView(elementLocator, swipeDirection, DEFAULT_NUMBER_OF_ATTEMPTS_TO_SCROLL_TO_ELEMENT);
    }

    /**
     * Attempts to scroll the element into view in case of native mobile elements.
     *
     * @param elementLocator                 the locator of the webElement under
     *                                       test (By xpath, id, selector, name
     *                                       ...etc)
     * @param swipeDirection                 SwipeDirection.DOWN, UP, RIGHT, or LEFT
     * @param attemptsToScrollAndFindElement number of attempts to scroll and find
     *                                       the element
     * @return a self-reference to be used to chain actions
     */
    public TouchActions swipeElementIntoView(By elementLocator, SwipeDirection swipeDirection,
                                             int attemptsToScrollAndFindElement) {
        elementLocator = ElementActions.updateLocatorWithAIGenratedOne(elementLocator);
        try {
            if (driver instanceof AppiumDriver<?>) {
                // appium native application
                attemptToSwipeElementIntoViewInNativeApp(elementLocator, swipeDirection,
                        attemptsToScrollAndFindElement);
            } else {
                // regular touch screen device
                if (ElementActions.identifyUniqueElement(driver, elementLocator)) {
                    Point elementLocation = driver.findElement(elementLocator).getLocation();
                    (new org.openqa.selenium.interactions.touch.TouchActions(driver)).scroll(elementLocation.getX(), elementLocation.getY()).perform();
                } else {
                    ElementActions.failAction(driver, elementLocator);
                }
            }
            ElementActions.passAction(driver, elementLocator);
        } catch (Exception e) {
            ElementActions.failAction(driver, elementLocator, e);
        }
        return this;
    }

    private void attemptToSwipeElementIntoViewInNativeApp(By elementLocator, SwipeDirection swipeDirection,
                                                          int attemptsToScrollAndFindElement) {
        Boolean isElementFound = false;
        int attemptsToFindElement = 0;
        do {
            // appium native device
            if (!driver.findElements(elementLocator).isEmpty()
                    && ElementActions.identifyUniqueElement(driver, elementLocator)) {
                // element is already on screen
                isElementFound = true;
                ReportManager.logDiscrete("Element is already onscreen.");
            } else {
                // for the animated GIF:
                ElementActions.takeScreenshot(driver, elementLocator, "swipeElementIntoView", null, true);

                Dimension screenSize = driver.manage().window().getSize();
                Point startingPoint = new Point(0, 0);
                Point endingPoint = new Point(0, 0);

                switch (swipeDirection) {
                    case DOWN:
                        startingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight() * 80 / 100);
                        endingPoint = new Point(screenSize.getWidth() / 2, 0);
                        break;
                    case UP:
                        startingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight() * 20 / 100);
                        endingPoint = new Point(screenSize.getWidth() / 2, screenSize.getHeight());
                        break;
                    case RIGHT:
                        startingPoint = new Point(screenSize.getWidth() * 80 / 100, screenSize.getHeight() / 2);
                        endingPoint = new Point(0, screenSize.getHeight() / 2);
                        break;
                    case LEFT:
                        startingPoint = new Point(screenSize.getWidth() * 20 / 100, screenSize.getHeight() / 2);
                        endingPoint = new Point(screenSize.getWidth(), screenSize.getHeight() / 2);
                        break;
                }
                (new TouchAction<>((AppiumDriver<?>) driver)).press(PointOption.point(startingPoint))
                        .moveTo(PointOption.point(endingPoint)).release().perform();
                attemptsToFindElement++;
            }
        } while (Boolean.FALSE.equals(isElementFound) || attemptsToFindElement >= attemptsToScrollAndFindElement);
        // TODO: devise a way to break the loop when no further scrolling options are
        // available. do not use visual comparison which is the easy but costly way to
        // do it.
    }

    public enum SwipeDirection {
        UP, DOWN, LEFT, RIGHT
    }

}
