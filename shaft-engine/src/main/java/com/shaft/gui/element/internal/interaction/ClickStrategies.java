package com.shaft.gui.element.internal.interaction;

import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

/**
 * Selenium click ladder: native click → optional scroll/stabilize retry → flagged JS fallback.
 * When JS fallback is off, the native {@link InvalidElementStateException} is preserved.
 */
public final class ClickStrategies {

    private static final String JS_CLICK = "arguments[0].click();";
    private static final String JS_SCROLL_CENTER = """
            arguments[0].scrollIntoView({behavior: "auto", block: "center", inline: "center"});""";

    private ClickStrategies() {
    }

    public static void click(WebDriver driver, WebElement element, ElementKind kind, boolean javascriptFallbackEnabled) {
        if (kind == ElementKind.DISABLED) {
            throw new InvalidElementStateException(
                    "Refusing to click a disabled/readonly/aria-disabled element.");
        }
        try {
            element.click();
        } catch (InvalidElementStateException firstFailure) {
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

    private static void stabilize(WebDriver driver, WebElement element) {
        if (driver instanceof JavascriptExecutor executor) {
            try {
                executor.executeScript(JS_SCROLL_CENTER, element);
            } catch (RuntimeException ignored) {
                // Best-effort scroll before retry; native exception still drives fallback/fail.
            }
        }
    }
}
