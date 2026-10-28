package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;

import java.util.Locale;

/**
 * XCUITest names the widget type {@code type}. {@code class} and {@code className}
 * are web and Android names; WDA rejects them. {@code wdType} is not requested:
 * the driver already lists {@code type}.
 */
public final class IosXcuiAttributeNames {
    private IosXcuiAttributeNames() {
    }

    /**
     * Attribute name to send to the driver. On an iOS native XCUITest session,
     * {@code class} and {@code className} become {@code type}. Every other session
     * keeps the requested name, including Android {@code class}.
     *
     * @param name requested attribute name
     * @return the name to issue
     */
    public static String issued(String name) {
        if (name == null || !isIosXcuiNative() || !isClassAlias(name)) {
            return name;
        }
        return "type";
    }

    private static boolean isClassAlias(String name) {
        return "class".equalsIgnoreCase(name) || "classname".equalsIgnoreCase(name);
    }

    /**
     * iOS native sessions use XCUITest unless the automation name is Flutter.
     * Mobile web keeps DOM {@code class}. Android is not this session.
     */
    private static boolean isIosXcuiNative() {
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
