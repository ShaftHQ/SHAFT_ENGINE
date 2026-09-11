package com.shaft.gui.element.internal.interaction;

import com.shaft.tools.io.ReportManager;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Pause;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;

import java.time.Duration;
import java.util.List;

/**
 * Selenium click ladder: native click → optional scroll/stabilize retry → flagged JS fallback.
 * Mobile native (#5732 Wave D): native click → W3C touch tap when WebDriver click flakes
 * (does not change desktop/web defaults; JS fallback stays web-oriented).
 */
public final class ClickStrategies {

    private static final String JS_CLICK = "arguments[0].click();";
    private static final String JS_SCROLL_CENTER = """
            arguments[0].scrollIntoView({behavior: "auto", block: "center", inline: "center"});""";
    private static final Duration TOUCH_PAUSE = Duration.ofMillis(100);

    private ClickStrategies() {
    }

    public static void click(WebDriver driver, WebElement element, ElementKind kind, boolean javascriptFallbackEnabled) {
        click(driver, element, kind, javascriptFallbackEnabled, false);
    }

    /**
     * @param mobileNativeTouchFallback when true (mobile native only), retry failed clicks with a W3C
     *                                  touch tap instead of JavaScript click
     */
    public static void click(WebDriver driver, WebElement element, ElementKind kind,
                             boolean javascriptFallbackEnabled, boolean mobileNativeTouchFallback) {
        if (kind == ElementKind.DISABLED) {
            throw new InvalidElementStateException(
                    "Refusing to click a disabled/readonly/aria-disabled element.");
        }
        try {
            element.click();
        } catch (InvalidElementStateException firstFailure) {
            if (mobileNativeTouchFallback) {
                stabilizeMobile(driver, element);
                try {
                    element.click();
                } catch (RuntimeException retryFailure) {
                    tapWithTouch(driver, element);
                    ReportManager.logDiscrete(
                            "Performed Click using W3C touch tap after WebDriver click failed on mobile native.");
                }
                return;
            }
            stabilize(driver, element);
            try {
                element.click();
            } catch (InvalidElementStateException retryFailure) {
                if (javascriptFallbackEnabled && driver instanceof JavascriptExecutor executor) {
                    executor.executeScript(JS_CLICK, element);
                    ReportManager.logDiscrete(
                            "Performed Click using JavaScript; If the report is showing that the click passed but you observe that no action was taken, we recommend trying a different element locator.");
                } else {
                    throw retryFailure;
                }
            }
        }
    }

    /**
     * Best-effort focus tap for mobile typing: WebDriver click, then W3C touch if needed.
     */
    public static void focusTap(WebDriver driver, WebElement element) {
        try {
            element.click();
        } catch (RuntimeException ignored) {
            tapWithTouch(driver, element);
        }
    }

    static void tapWithTouch(WebDriver driver, WebElement element) {
        if (!(driver instanceof AppiumDriver appiumDriver)) {
            throw new InvalidElementStateException(
                    "Mobile touch tap fallback requires an AppiumDriver session.");
        }
        Rectangle rect = element.getRect();
        int x = rect.getX() + Math.max(rect.getWidth() / 2, 1);
        int y = rect.getY() + Math.max(rect.getHeight() / 2, 1);
        PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "shaft-mobile-finger");
        Sequence tap = new Sequence(finger, 0);
        PointerInput.Origin view = PointerInput.Origin.viewport();
        tap.addAction(finger.createPointerMove(Duration.ZERO, view, x, y));
        tap.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
        tap.addAction(new Pause(finger, TOUCH_PAUSE));
        tap.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
        appiumDriver.perform(List.of(tap));
    }

    private static void stabilize(WebDriver driver, WebElement element) {
        if (driver instanceof JavascriptExecutor executor) {
            try {
                executor.executeScript(JS_SCROLL_CENTER, element);
            } catch (RuntimeException ignored) {
                // Best-effort scroll before retry; native exception still drives fallback/fail.
            }
        }
    }

    private static void stabilizeMobile(WebDriver driver, WebElement element) {
        // Native contexts rarely support DOM scrollIntoView; keep a no-op hook for symmetry.
        if (driver instanceof JavascriptExecutor executor) {
            try {
                executor.executeScript(JS_SCROLL_CENTER, element);
            } catch (RuntimeException ignored) {
                // Ignore — touch fallback follows.
            }
        }
    }
}
