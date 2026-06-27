package com.shaft.gui.internal.locator;

import lombok.NonNull;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.Beta;
import org.openqa.selenium.By;

public class Locator {

    /**
     * Builds a locator by element id.
     *
     * @param id target element id
     * @return Selenium locator
     */
    public static By id(@NonNull String id) {
        return By.id(id);
    }

    /**
     * Builds a locator by element name.
     *
     * @param name target element name
     * @return Selenium locator
     */
    public static By name(@NonNull String name) {
        return By.name(name);
    }

    /**
     * Builds a locator by tag name.
     *
     * @param tagName target element tag name
     * @return Selenium locator
     */
    public static By tagName(@NonNull String tagName) {
        return By.tagName(tagName);
    }

    /**
     * Builds a locator by class name.
     *
     * @param className target element class name
     * @return Selenium locator
     */
    public static By className(@NonNull String className) {
        return By.className(className);
    }

    /**
     * Builds a locator by CSS selector.
     *
     * @param cssSelector target CSS selector
     * @return Selenium locator
     */
    public static By cssSelector(@NonNull String cssSelector) {
        return By.cssSelector(cssSelector);
    }

    /**
     * Builds a locator by XPath.
     *
     * @param xpath target XPath expression
     * @return Selenium locator
     */
    public static By xpath(@NonNull String xpath) {
        return By.xpath(xpath);
    }

    /**
     * Builds an Appium locator by accessibility id.
     *
     * @param accessibilityId target accessibility id
     * @return Appium locator
     */
    public static By accessibilityId(@NonNull String accessibilityId) {
        return AppiumBy.accessibilityId(accessibilityId);
    }

    /**
     * Builds an Android UiAutomator locator.
     *
     * @param uiAutomatorExpression target UiAutomator expression
     * @return Appium locator
     */
    public static By androidUiAutomator(@NonNull String uiAutomatorExpression) {
        return AppiumBy.androidUIAutomator(uiAutomatorExpression);
    }

    /**
     * Builds an iOS predicate string locator.
     *
     * @param predicate target iOS predicate string
     * @return Appium locator
     */
    public static By iosPredicateString(@NonNull String predicate) {
        return AppiumBy.iOSNsPredicateString(predicate);
    }

    /**
     * Builds an iOS class chain locator.
     *
     * @param classChain target iOS class chain
     * @return Appium locator
     */
    public static By iosClassChain(@NonNull String classChain) {
        return AppiumBy.iOSClassChain(classChain);
    }

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
