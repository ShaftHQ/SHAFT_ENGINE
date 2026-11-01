package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.WebElement;

import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * XCUITest attribute names differ from Android UiAutomator2 and the web DOM.
 * {@code class}/{@code className} become {@code type} (#7455). Other Android-only
 * names are mapped or skipped so WDA never sees an unknown attribute (#7669).
 */
public final class IosXcuiAttributeNames {
    private static final Map<String, String> IOS_XCUI_ISSUED = Map.ofEntries(
            Map.entry("class", "type"),
            Map.entry("classname", "type"),
            Map.entry("clickable", "hittable"),
            Map.entry("resource-id", "name"),
            Map.entry("resourceid", "name"),
            Map.entry("content-desc", "label"),
            Map.entry("contentdesc", "label"),
            Map.entry("scrollable", "type"),
            Map.entry("bounds", "rect"),
            Map.entry("checked", "selected"));

    private static final Set<String> ANDROID_ONLY_SKIP = Set.of(
            "checkable",
            "long-clickable",
            "longclickable",
            "password",
            "package");

    private IosXcuiAttributeNames() {
    }

    /**
     * Attribute name to send to the driver. On an iOS native XCUITest session,
     * Android / web aliases are remapped; Android-only names with no XCUI peer
     * become empty (do not call the driver). Every other session keeps the
     * requested name.
     *
     * @param name requested attribute name
     * @return the name to issue; empty when {@code name} is null or must not be queried
     */
    public static String issued(String name) {
        if (name == null) {
            return "";
        }
        if (!isIosXcuiNative()) {
            return name;
        }
        String key = name.toLowerCase(Locale.ROOT);
        if (ANDROID_ONLY_SKIP.contains(key)) {
            return "";
        }
        return IOS_XCUI_ISSUED.getOrDefault(key, name);
    }

    /**
     * Maps a raw driver value back to the caller's requested attribute semantics
     * on iOS XCUITest. Other sessions return {@code raw} unchanged.
     *
     * @param requested original attribute name from the caller
     * @param raw       value returned for {@link #issued(String)}
     * @return caller-facing value
     */
    public static String interpret(String requested, String raw) {
        if (requested == null || !isIosXcuiNative() || !"scrollable".equalsIgnoreCase(requested)) {
            return raw;
        }
        return isXcuiScrollContainer(raw) ? "true" : "false";
    }

    /**
     * Reads {@link WebElement#getAttribute(String)} with iOS mapping. Skipped
     * Android-only names return {@code null}; other driver errors propagate.
     *
     * @param element element to read
     * @param name    requested attribute name
     * @return attribute value, possibly interpreted; {@code null} when skipped
     */
    public static String readAttribute(WebElement element, String name) {
        String issued = issued(name);
        if (issued.isEmpty()) {
            return null;
        }
        return interpret(name, element.getAttribute(issued));
    }

    /**
     * Reads {@link WebElement#getDomAttribute(String)} with the same iOS mapping as
     * {@link #readAttribute(WebElement, String)}.
     *
     * @param element element to read
     * @param name    requested attribute name
     * @return attribute value, possibly interpreted; {@code null} when skipped
     */
    public static String readDomAttribute(WebElement element, String name) {
        String issued = issued(name);
        if (issued.isEmpty()) {
            return null;
        }
        return interpret(name, element.getDomAttribute(issued));
    }

    /**
     * Soft read for internal callers that must not fail the capture path.
     * Returns {@code null} when skipped or the driver throws.
     *
     * @param element element to read
     * @param name    requested attribute name
     * @return attribute value or {@code null}
     */
    public static String read(WebElement element, String name) {
        try {
            return readAttribute(element, name);
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    /**
     * True when {@code type} names a scrollable XCUI container.
     *
     * @param type XCUI {@code type} / {@code wdType} value
     * @return whether the type is a scroll container
     */
    static boolean isXcuiScrollContainer(String type) {
        if (type == null || type.isBlank()) {
            return false;
        }
        String normalized = type.toLowerCase(Locale.ROOT);
        return normalized.contains("scrollview")
                || normalized.contains("table")
                || normalized.contains("collectionview")
                || normalized.contains("webkit")
                || normalized.contains("textview");
    }

    /**
     * iOS native sessions use XCUITest unless the automation name is Flutter.
     * Mobile web keeps DOM attribute names. Android is not this session.
     */
    static boolean isIosXcuiNative() {
        String platform = SHAFT.Properties.platform.targetPlatform();
        if (platform == null || !"ios".equalsIgnoreCase(platform)) {
            return false;
        }
        String browserName = SHAFT.Properties.mobile.browserName();
        if (browserName != null && !browserName.isBlank()) {
            return false;
        }
        String automation = SHAFT.Properties.mobile.automationName();
        return automation == null || !automation.toLowerCase(Locale.ROOT).contains("flutter");
    }
}
