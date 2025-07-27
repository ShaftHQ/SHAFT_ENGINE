package com.shaft.gui.internal.locator;

public class Locator {

    public static LocatorBuilder hasTagName(String tagName) {
        return LocatorBuilder.hasTagName(tagName);
    }

    public static LocatorBuilder hasAnyTagName() {
        return LocatorBuilder.hasTagName("*");
    }

    public static LocatorBuilder hasRole(Role ariaRole) {
        return LocatorBuilder.byRole(ariaRole);
    }
}
