package com.shaft.gui.element.internal.interaction;

import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.RemoteWebElement;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * iOS XCUITest reports {@code element.click()} as success when {@code hittable} is false
 * and the control does not run. Both {@code driver.element().click(locator)} and
 * {@code driver.touch().tap(locator)} call this after that reported click.
 *
 * <p>The recovery is an element-anchored {@code mobile: tap} (element id only).
 * Viewport coordinates are never the success path. When the page source does not change,
 * the action fails instead of returning success.
 */
public final class IosNonHittableTap {

    static final String NO_EFFECT_MESSAGE = "element is not hittable; the tap had no effect";

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
        String after = readPageSource(driver);
        if (!anchored || before == null || before.equals(after)) {
            throw new IosTapHadNoEffectException(NO_EFFECT_MESSAGE);
        }
    }

    static boolean isIosXcui(WebDriver driver) {
        if (!(driver instanceof IOSDriver)) {
            return false;
        }
        return !driver.getClass().getName().contains("Flutter");
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
     * @return true when {@code mobile: tap} was issued with the element id and no coordinates
     */
    static boolean elementAnchoredTap(WebDriver driver, WebElement element) {
        if (!(driver instanceof JavascriptExecutor executor) || !(element instanceof RemoteWebElement remote)) {
            return false;
        }
        String elementId;
        try {
            elementId = remote.getId();
        } catch (RuntimeException ignored) {
            return false;
        }
        if (elementId == null || elementId.isBlank()) {
            return false;
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("elementId", elementId);
        try {
            executor.executeScript("mobile: tap", params);
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
