package com.shaft.gui.internal.locator;

import lombok.NonNull;
import io.appium.java_client.AppiumBy;
import com.shaft.gui.ocr.OcrTarget;
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


    /**
     * Builds a Flutter ValueKey locator ({@code AppiumBy.flutterKey}).
     * Available with Appium Flutter Integration Driver sessions.
     *
     * @param key Flutter ValueKey / Key string
     * @return Flutter Appium locator
     */
    public static By flutterKey(@NonNull String key) {
        return AppiumBy.flutterKey(key);
    }

    /**
     * Builds a Flutter exact-text locator ({@code AppiumBy.flutterText}).
     *
     * @param text exact visible text on the widget
     * @return Flutter Appium locator
     */
    public static By flutterText(@NonNull String text) {
        return AppiumBy.flutterText(text);
    }

    /**
     * Builds a Flutter partial-text locator ({@code AppiumBy.flutterTextContaining}).
     *
     * @param partialText substring of the visible text on the widget
     * @return Flutter Appium locator
     */
    public static By flutterTextContaining(@NonNull String partialText) {
        return AppiumBy.flutterTextContaining(partialText);
    }

    /**
     * Builds a Flutter widget-type locator ({@code AppiumBy.flutterType}).
     *
     * @param widgetType Flutter widget type name (e.g. {@code TextField}, {@code ElevatedButton})
     * @return Flutter Appium locator
     */
    public static By flutterType(@NonNull String widgetType) {
        return AppiumBy.flutterType(widgetType);
    }

    /**
     * Builds a Flutter semantics-label locator ({@code AppiumBy.flutterSemanticsLabel}).
     * Also covers Tooltip messages exposed as semantics labels.
     *
     * @param semanticsLabel value of the Semantics label attribute
     * @return Flutter Appium locator
     */
    public static By flutterSemanticsLabel(@NonNull String semanticsLabel) {
        return AppiumBy.flutterSemanticsLabel(semanticsLabel);
    }

    /**
     * Builds a Flutter descendant hierarchy locator ({@code AppiumBy.flutterDescendant}).
     * Requires java-client Flutter Integration Driver support (1.4.0+ finder).
     *
     * @param of parent Flutter locator
     * @param matching descendant Flutter locator
     * @return Flutter Appium locator
     */
    public static By flutterDescendant(@NonNull AppiumBy.FlutterBy of, @NonNull AppiumBy.FlutterBy matching) {
        return AppiumBy.flutterDescendant(of, matching);
    }

    /**
     * Builds a Flutter ancestor hierarchy locator ({@code AppiumBy.flutterAncestor}).
     * Requires java-client Flutter Integration Driver support (1.4.0+ finder).
     *
     * @param of child Flutter locator
     * @param matching ancestor Flutter locator
     * @return Flutter Appium locator
     */
    public static By flutterAncestor(@NonNull AppiumBy.FlutterBy of, @NonNull AppiumBy.FlutterBy matching) {
        return AppiumBy.flutterAncestor(of, matching);
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

    /**
     * Builds an OCR target that matches a complete recognized visible-text line.
     *
     * @param text complete visible text to recognize
     * @return immutable OCR target
     */
    public static OcrTarget hasOcrText(@NonNull String text) {
        return OcrTarget.exact(text);
    }

    /**
     * Builds an OCR target that matches part of a recognized visible-text line.
     *
     * @param text partial visible text to recognize
     * @return immutable OCR target
     */
    public static OcrTarget containsOcrText(@NonNull String text) {
        return OcrTarget.containing(text);
    }
}
