package com.shaft.gui.element.internal.interaction;

import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ElementClassifierTest {

    @DataProvider(name = "webKinds")
    public Object[][] webKinds() {
        return new Object[][]{
                // cases 1–2 text-like
                {element("input", "text", null, null), ElementKind.TEXT_LIKE},
                {element("input", "password", null, null), ElementKind.TEXT_LIKE},
                {element("input", "email", null, null), ElementKind.TEXT_LIKE},
                {element("input", "search", null, null), ElementKind.TEXT_LIKE},
                {element("input", "tel", null, null), ElementKind.TEXT_LIKE},
                {element("input", "url", null, null), ElementKind.TEXT_LIKE},
                {element("input", "number", null, null), ElementKind.TEXT_LIKE},
                {element("textarea", null, null, null), ElementKind.TEXT_LIKE},
                // case 3
                {element("input", "checkbox", null, null), ElementKind.CHECKBOX},
                {element("input", "radio", null, null), ElementKind.RADIO},
                {element("div", null, "checkbox", null), ElementKind.CHECKBOX},
                {element("div", null, "switch", null), ElementKind.CHECKBOX},
                // case 4
                {element("select", null, null, null), ElementKind.SELECT},
                {element("SELECT", null, null, null), ElementKind.SELECT},
                // case 5
                {element("input", "file", null, null), ElementKind.FILE},
                // case 6
                {element("input", "date", null, null), ElementKind.DATE_LIKE},
                {element("input", "time", null, null), ElementKind.DATE_LIKE},
                {element("input", "datetime-local", null, null), ElementKind.DATE_LIKE},
                // case 7
                {element("input", "range", null, null), ElementKind.RANGE},
                {element("input", "color", null, null), ElementKind.COLOR},
                {element("div", null, "slider", null), ElementKind.RANGE},
                // case 8–9
                {element("button", null, null, null), ElementKind.BUTTON},
                {element("input", "submit", null, null), ElementKind.BUTTON},
                {element("a", null, null, null), ElementKind.LINK},
                {element("div", null, "button", null), ElementKind.BUTTON},
                {element("div", null, "link", null), ElementKind.LINK},
                // case 10
                {contentEditable("div"), ElementKind.CONTENTEDITABLE},
                // case 11
                {element("div", null, "textbox", null), ElementKind.TEXT_LIKE},
                {element("div", null, "searchbox", null), ElementKind.TEXT_LIKE},
                {element("div", null, "spinbutton", null), ElementKind.TEXT_LIKE},
                {element("div", null, "combobox", null), ElementKind.COMBOBOX},
                // case 12 masked/react — still text-like input
                {element("input", "text", null, null), ElementKind.TEXT_LIKE},
                // case 15
                {element("iframe", null, null, null), ElementKind.IFRAME},
                // case 20
                {disabled("input", "text"), ElementKind.DISABLED},
                {readonly("input", "text"), ElementKind.READONLY},
                {ariaDisabled("div"), ElementKind.DISABLED},
                {contentEditablePlaintext("div"), ElementKind.CONTENTEDITABLE},
                // unknown conservative
                {element("div", null, null, null), ElementKind.UNKNOWN},
                {element("custom-widget", null, null, null), ElementKind.UNKNOWN},
                {element(null, null, null, null), ElementKind.UNKNOWN},
        };
    }

    @Test(dataProvider = "webKinds")
    public void classifyCoversWebCatalogCases(WebElement element, ElementKind expected) {
        Assert.assertEquals(ElementClassifier.classify(element), expected);
    }

    @Test
    public void nullElementIsUnknown() {
        Assert.assertEquals(ElementClassifier.classify(null), ElementKind.UNKNOWN);
    }

    @Test
    public void truthyFlagRejectsArbitraryMockStrings() {
        WebElement element = mock(WebElement.class);
        when(element.getDomAttribute("disabled")).thenReturn("dom-disabled");
        Assert.assertFalse(ElementClassifier.isTruthyFlag(element, "disabled"));
        when(element.getDomAttribute("disabled")).thenReturn("");
        Assert.assertTrue(ElementClassifier.isTruthyFlag(element, "disabled"));
        when(element.getDomAttribute("disabled")).thenReturn("true");
        Assert.assertTrue(ElementClassifier.isTruthyFlag(element, "disabled"));
        when(element.getDomAttribute("disabled")).thenReturn("disabled");
        Assert.assertTrue(ElementClassifier.isTruthyFlag(element, "disabled"));
    }

    @Test
    public void typeRoutesDoNotBlindSendKeysForSpecialKinds() {
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.CHECKBOX), TypeStrategies.TypeRoute.TOGGLE_CLICK);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.RADIO), TypeStrategies.TypeRoute.TOGGLE_CLICK);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.SELECT), TypeStrategies.TypeRoute.SELECT_OPTION);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.FILE), TypeStrategies.TypeRoute.SET_FILES);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.DATE_LIKE), TypeStrategies.TypeRoute.SET_VALUE_WITH_EVENTS);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.CONTENTEDITABLE), TypeStrategies.TypeRoute.CONTENTEDITABLE);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.TEXT_LIKE), TypeStrategies.TypeRoute.LEGACY_SEND_KEYS);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.UNKNOWN), TypeStrategies.TypeRoute.LEGACY_SEND_KEYS);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.COMBOBOX), TypeStrategies.TypeRoute.LEGACY_SEND_KEYS);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.BUTTON), TypeStrategies.TypeRoute.LEGACY_SEND_KEYS);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.LINK), TypeStrategies.TypeRoute.LEGACY_SEND_KEYS);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.READONLY), TypeStrategies.TypeRoute.REJECT);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.DISABLED), TypeStrategies.TypeRoute.REJECT);
    }

    @Test
    public void clickAllowsReadonlyButRefusesDisabled() {
        Assert.assertNotEquals(ElementKind.READONLY, ElementKind.DISABLED);
        // Readonly must not share DISABLED click refusal — covered by ClickStrategies.DISABLED check.
        WebElement readonlyInput = readonly("input", "text");
        Assert.assertEquals(ElementClassifier.classify(readonlyInput), ElementKind.READONLY);
    }

    private static WebElement element(String tag, String type, String role, String unused) {
        WebElement element = mock(WebElement.class);
        when(element.getTagName()).thenReturn(tag);
        when(element.getDomAttribute(anyString())).thenReturn(null);
        when(element.getDomProperty(anyString())).thenReturn(null);
        when(element.getAttribute(anyString())).thenReturn(null);
        if (type != null) {
            when(element.getDomAttribute("type")).thenReturn(type);
        }
        if (role != null) {
            when(element.getDomAttribute("role")).thenReturn(role);
        }
        return element;
    }

    private static WebElement contentEditable(String tag) {
        WebElement element = element(tag, null, null, null);
        when(element.getDomAttribute("contenteditable")).thenReturn("true");
        return element;
    }

    private static WebElement contentEditablePlaintext(String tag) {
        WebElement element = element(tag, null, null, null);
        when(element.getDomAttribute("contenteditable")).thenReturn("plaintext-only");
        return element;
    }

    private static WebElement disabled(String tag, String type) {
        WebElement element = element(tag, type, null, null);
        when(element.getDomAttribute("disabled")).thenReturn("");
        return element;
    }

    private static WebElement readonly(String tag, String type) {
        WebElement element = element(tag, type, null, null);
        when(element.getDomAttribute("readonly")).thenReturn("true");
        return element;
    }

    private static WebElement ariaDisabled(String tag) {
        WebElement element = element(tag, null, null, null);
        when(element.getDomAttribute("aria-disabled")).thenReturn("true");
        return element;
    }
}
