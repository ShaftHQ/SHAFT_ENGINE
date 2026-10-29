package com.shaft.gui.element.internal.interaction;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Pause;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.remote.RemoteWebElement;

import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * iOS XCUITest reports {@code element.click()} as success when {@code hittable} is false
 * and the control does not run. Both {@code driver.element().click(locator)} and
 * {@code driver.touch().tap(locator)} call this after that reported click.
 *
 * <p>Recovery order:
 * <ol>
 *   <li>Element-anchored {@code mobile: tap} with the element id and element-relative
 *       centre coordinates ({@code width/2}, {@code height/2}).</li>
 *   <li>If page source is unchanged, a W3C touch pointer tap at the element's centre
 *       (viewport coordinates from {@link WebElement#getRect()}), matching the
 *       diagnosis in #6908 that a centre pointer tap reaches the control when
 *       element click does not.</li>
 * </ol>
 * Screen-only taps without an element rectangle are not used. When neither recovery
 * changes the page source, the action fails instead of returning success.
 */
public final class IosNonHittableTap {

    static final String NO_EFFECT_MESSAGE = "element is not hittable; the tap had no effect";

    private static final Duration TOUCH_PAUSE = Duration.ofMillis(100);

    private IosNonHittableTap() {
    }

    public static void afterReportedClick(WebDriver driver, WebElement element) {
        if (!isIosXcui(driver) || element == null) {
            return;
        }
        if (!"false".equalsIgnoreCase(readHittable(element))) {
            return;
        }
        String before = readPageSource(driver);
        boolean anchored = elementAnchoredTap(driver, element);
        String afterMobile = readPageSource(driver);
        if (anchored && pageChanged(before, afterMobile)) {
            return;
        }
        String beforeW3c = afterMobile != null ? afterMobile : before;
        boolean w3c = w3cElementCenterTap(driver, element);
        String afterW3c = readPageSource(driver);
        if (w3c && pageChanged(beforeW3c, afterW3c)) {
            return;
        }
        throw new IosTapHadNoEffectException(NO_EFFECT_MESSAGE);
    }

    static boolean isIosXcui(WebDriver driver) {
        if (!(driver instanceof IOSDriver)) {
            return false;
        }
        return !driver.getClass().getName().contains("Flutter");
    }

    private static boolean pageChanged(String before, String after) {
        return before != null && after != null && !before.equals(after);
    }

    private static String readHittable(WebElement element) {
        String dom = readAttribute(element, true);
        if (dom != null && !dom.isBlank()) {
            return dom.trim();
        }
        String attribute = readAttribute(element, false);
        return attribute == null ? "" : attribute.trim();
    }

    private static String readAttribute(WebElement element, boolean domAttribute) {
        try {
            return domAttribute ? element.getDomAttribute("hittable") : element.getAttribute("hittable");
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    private static String readPageSource(WebDriver driver) {
        try {
            return driver.getPageSource();
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    /**
     * @return true when {@code mobile: tap} was issued with the element id and element-relative centre x,y
     */
    static boolean elementAnchoredTap(WebDriver driver, WebElement element) {
        if (!(driver instanceof JavascriptExecutor executor) || !(element instanceof RemoteWebElement remote)) {
            return false;
        }
        String elementId;
        Dimension size;
        try {
            elementId = remote.getId();
            size = element.getSize();
        } catch (RuntimeException ignored) {
            return false;
        }
        if (elementId == null || elementId.isBlank() || size == null) {
            return false;
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("elementId", elementId);
        params.put("x", size.getWidth() / 2.0);
        params.put("y", size.getHeight() / 2.0);
        try {
            executor.executeScript("mobile: tap", params);
            return true;
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    /**
     * W3C touch tap at the element's centre. Same geometry as {@link ClickStrategies#tapWithTouch}.
     *
     * @return true when {@code perform} was issued
     */
    static boolean w3cElementCenterTap(WebDriver driver, WebElement element) {
        if (!(driver instanceof AppiumDriver appiumDriver)) {
            return false;
        }
        int x;
        int y;
        try {
            Rectangle rect = element.getRect();
            if (rect == null) {
                return false;
            }
            x = rect.getX() + Math.max(rect.getWidth() / 2, 1);
            y = rect.getY() + Math.max(rect.getHeight() / 2, 1);
        } catch (RuntimeException ignored) {
            return false;
        }
        PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "shaft-ios-hittable-finger");
        Sequence tap = new Sequence(finger, 0);
        PointerInput.Origin view = PointerInput.Origin.viewport();
        tap.addAction(finger.createPointerMove(Duration.ZERO, view, x, y));
        tap.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
        tap.addAction(new Pause(finger, TOUCH_PAUSE));
        tap.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
        try {
            appiumDriver.perform(List.of(tap));
            return true;
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    /**
     * Failure for a non-hittable iOS tap that did not change the page.
     * Not an {@code InvalidElementStateException}: element waits ignore that type and would retry the no-op.
     */
    public static final class IosTapHadNoEffectException extends RuntimeException {
        public IosTapHadNoEffectException(String message) {
            super(message);
        }
    }
}
