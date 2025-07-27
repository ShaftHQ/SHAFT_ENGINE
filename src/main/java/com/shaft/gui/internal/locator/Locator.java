package com.shaft.gui.internal.locator;

import org.openqa.selenium.Beta;
import org.openqa.selenium.By;

public class Locator {

    public static LocatorBuilder hasTagName(String tagName) {
        return LocatorBuilder.hasTagName(tagName);
    }

    public static LocatorBuilder hasAnyTagName() {
        return LocatorBuilder.hasTagName("*");
    }

    @Beta
    public static LocatorBuilder hasRole(Role ariaRole) {
        return LocatorBuilder.byRole(ariaRole);
    }

    @Beta
    public static By inputField(String inputFieldName) {
        return SmartLocators.inputField(inputFieldName);
    }

    @Beta
    public static By clickableField(String clickableFieldName) {
        return SmartLocators.clickableField(clickableFieldName);
    }
}
