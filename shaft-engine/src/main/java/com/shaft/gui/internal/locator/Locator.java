package com.shaft.gui.internal.locator;

import lombok.NonNull;
import org.openqa.selenium.Beta;
import org.openqa.selenium.By;

public class Locator {

    public static LocatorBuilder hasTagName(@NonNull String tagName) {
        return LocatorBuilder.hasTagName(tagName);
    }

    public static LocatorBuilder hasAnyTagName() {
        return LocatorBuilder.hasTagName("*");
    }

    @Beta
    public static LocatorBuilder hasRole(@NonNull Role ariaRole) {
        return LocatorBuilder.byRole(ariaRole);
    }

    @Beta
    public static By inputField(@NonNull String elementName) {
        return SmartLocators.inputField(elementName);
    }

    @Beta
    public static By clickableField(@NonNull String elementName) {
        return SmartLocators.clickableField(elementName);
    }
}
