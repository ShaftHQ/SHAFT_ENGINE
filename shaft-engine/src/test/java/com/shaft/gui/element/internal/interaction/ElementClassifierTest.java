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
                {element("input", "text", null), ElementKind.TEXT_LIKE},
                {element("input", "password", null), ElementKind.TEXT_LIKE},
                {element("input", "email", null), ElementKind.TEXT_LIKE},
                {element("input", "search", null), ElementKind.TEXT_LIKE},
                {element("input", "tel", null), ElementKind.TEXT_LIKE},
                {element("input", "url", null), ElementKind.TEXT_LIKE},
                {element("input", "number", null), ElementKind.TEXT_LIKE},
                {element("textarea", null, null), ElementKind.TEXT_LIKE},
                // case 3
                {element("input", "checkbox", null), ElementKind.CHECKBOX},
                {element("input", "radio", null), ElementKind.RADIO},
                {element("div", null, "checkbox"), ElementKind.CHECKBOX},
                {element("div", null, "switch"), ElementKind.CHECKBOX},
                // case 4
                {element("select", null, null), ElementKind.SELECT},
                {element("SELECT", null, null), ElementKind.SELECT},
                // case 5
                {element("input", "file", null), ElementKind.FILE},
                // case 6
                {element("input", "date", null), ElementKind.DATE_LIKE},
                {element("input", "time", null), ElementKind.DATE_LIKE},
                {element("input", "datetime-local", null), ElementKind.DATE_LIKE},
                // case 7
                {element("input", "range", null), ElementKind.RANGE},
                {element("input", "color", null), ElementKind.COLOR},
                {element("div", null, "slider"), ElementKind.RANGE},
                // case 8–9
                {element("button", null, null), ElementKind.BUTTON},
                {element("input", "submit", null), ElementKind.BUTTON},
                {element("a", null, null), ElementKind.LINK},
                {element("div", null, "button"), ElementKind.BUTTON},
                {element("div", null, "link"), ElementKind.LINK},
                // case 10
                {contentEditable("div"), ElementKind.CONTENTEDITABLE},
                // case 11
                {element("div", null, "textbox"), ElementKind.TEXT_LIKE},
                {element("div", null, "searchbox"), ElementKind.TEXT_LIKE},
                {element("div", null, "spinbutton"), ElementKind.TEXT_LIKE},
                {element("div", null, "combobox"), ElementKind.COMBOBOX},
                // case 12 masked/react — still text-like input
                {element("input", "text", null), ElementKind.TEXT_LIKE},
                // case 15
                {element("iframe", null, null), ElementKind.IFRAME},
                // case 20
                {disabled("input", "text"), ElementKind.DISABLED},
                {readonly("input", "text"), ElementKind.READONLY},
                {ariaDisabled("div"), ElementKind.DISABLED},
                {contentEditablePlaintext("div"), ElementKind.CONTENTEDITABLE},
                // unknown conservative
                {element("div", null, null), ElementKind.UNKNOWN},
                {element("custom-widget", null, null), ElementKind.UNKNOWN},
                {element(null, null, null), ElementKind.UNKNOWN},
        };
    }

    @Test(dataProvider = "webKinds")
    public void classifyCoversWebCatalogCases(WebElement element, ElementKind expected) {
        Assert.assertEquals(ElementClassifier.classify(element), expected);
    }

    @Test
    public void nullElementIsUnknown() {
        Assert.assertEquals(ElementClassifier.classify((WebElement) null), ElementKind.UNKNOWN);
        Assert.assertEquals(ElementClassifier.classify((ElementSignals) null), ElementKind.UNKNOWN);
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
    public void contentEditableRejectsArbitraryMockStrings() {
        Assert.assertFalse(ElementClassifier.isContentEditableAttributeValue(null));
        Assert.assertFalse(ElementClassifier.isContentEditableAttributeValue("false"));
        Assert.assertFalse(ElementClassifier.isContentEditableAttributeValue("dom-contenteditable"));
        Assert.assertFalse(ElementClassifier.isContentEditableAttributeValue("yes"));
        Assert.assertTrue(ElementClassifier.isContentEditableAttributeValue(""));
        Assert.assertTrue(ElementClassifier.isContentEditableAttributeValue("true"));
        Assert.assertTrue(ElementClassifier.isContentEditableAttributeValue("TRUE"));
        Assert.assertTrue(ElementClassifier.isContentEditableAttributeValue("plaintext-only"));
        Assert.assertTrue(ElementClassifier.isContentEditableAttributeValue("contenteditable"));

        WebElement mockedInput = mock(WebElement.class);
        when(mockedInput.getTagName()).thenReturn("input");
        when(mockedInput.getDomAttribute(anyString())).thenAnswer(invocation -> "dom-" + invocation.getArgument(0));
        when(mockedInput.getDomAttribute("type")).thenReturn("text");
        when(mockedInput.getDomProperty(anyString())).thenAnswer(invocation -> "dom-" + invocation.getArgument(0));
        when(mockedInput.getAttribute(anyString())).thenAnswer(invocation -> "attr-" + invocation.getArgument(0));
        // Coverage-style mocks must stay TEXT_LIKE / UNKNOWN, never CONTENTEDITABLE.
        Assert.assertEquals(ElementClassifier.classify(mockedInput), ElementKind.TEXT_LIKE);
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

    private static WebElement element(String tag, String type, String role) {
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
        WebElement element = element(tag, null, null);
        when(element.getDomAttribute("contenteditable")).thenReturn("true");
        return element;
    }

    private static WebElement contentEditablePlaintext(String tag) {
        WebElement element = element(tag, null, null);
        when(element.getDomAttribute("contenteditable")).thenReturn("plaintext-only");
        return element;
    }

    private static WebElement disabled(String tag, String type) {
        WebElement element = element(tag, type, null);
        when(element.getDomAttribute("disabled")).thenReturn("");
        return element;
    }

    private static WebElement readonly(String tag, String type) {
        WebElement element = element(tag, type, null);
        when(element.getDomAttribute("readonly")).thenReturn("true");
        return element;
    }

    private static WebElement ariaDisabled(String tag) {
        WebElement element = element(tag, null, null);
        when(element.getDomAttribute("aria-disabled")).thenReturn("true");
        return element;
    }

    @Test(description = "Issue #6833: native Android classification reads only native signals, once each, and never web-only attributes")
    public void nativeSessionsReadOnlyNativeSignalsOnce() {
        String originalPlatform = com.shaft.driver.SHAFT.Properties.platform.targetPlatform();
        com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform("Android");
        try {
            WebElement button = mock(WebElement.class);
            when(button.getTagName()).thenReturn("android.widget.Button");
            when(button.getDomAttribute("class")).thenReturn("android.widget.Button");
            when(button.getDomAttribute("enabled")).thenReturn("true");
            Assert.assertEquals(ElementClassifier.classify(button), ElementKind.BUTTON);

            WebElement disabled = mock(WebElement.class);
            when(disabled.getTagName()).thenReturn("android.widget.Button");
            when(disabled.getDomAttribute("class")).thenReturn("android.widget.Button");
            when(disabled.getDomAttribute("enabled")).thenReturn("false");
            Assert.assertEquals(ElementClassifier.classify(disabled), ElementKind.DISABLED);

            org.mockito.Mockito.verify(button, org.mockito.Mockito.times(1)).getDomAttribute("class");
            org.mockito.Mockito.verify(button, org.mockito.Mockito.times(1)).getDomAttribute("enabled");
            org.mockito.Mockito.verify(button, org.mockito.Mockito.never()).getAttribute(anyString());
            org.mockito.Mockito.verify(button, org.mockito.Mockito.never()).getDomProperty(anyString());
            for (String webOnly : new String[]{"type", "role", "contenteditable", "disabled", "readonly",
                    "aria-disabled", "autocomplete", "mask", "data-mask", "data-inputmask", "data-input-mask"}) {
                org.mockito.Mockito.verify(button, org.mockito.Mockito.never()).getDomAttribute(webOnly);
            }
        } finally {
            com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
        }
    }

    @Test(description = "#7455: iOS XCUITest reads the widget type, not class or className")
    public void iosXcuiReadsTypeInsteadOfClass() {
        String originalPlatform = com.shaft.driver.SHAFT.Properties.platform.targetPlatform();
        String originalBrowser = com.shaft.driver.SHAFT.Properties.mobile.browserName();
        String originalAutomation = com.shaft.driver.SHAFT.Properties.mobile.automationName();
        com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform("iOS");
        com.shaft.driver.SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        try {
            WebElement button = mock(WebElement.class);
            when(button.getTagName()).thenReturn("XCUIElementTypeButton");
            when(button.getDomAttribute("type")).thenReturn("XCUIElementTypeButton");
            when(button.getDomAttribute("enabled")).thenReturn("true");
            Assert.assertEquals(ElementClassifier.classify(button), ElementKind.BUTTON);
            org.mockito.Mockito.verify(button, org.mockito.Mockito.times(1)).getDomAttribute("type");
            org.mockito.Mockito.verify(button, org.mockito.Mockito.never()).getDomAttribute("class");
            org.mockito.Mockito.verify(button, org.mockito.Mockito.never()).getDomAttribute("className");
            org.mockito.Mockito.verify(button, org.mockito.Mockito.never()).getAttribute(anyString());
        } finally {
            com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
            com.shaft.driver.SHAFT.Properties.mobile.set().browserName(originalBrowser).automationName(originalAutomation);
        }
    }

    @Test(description = "Web classify reads class from getAttribute when getDomAttribute throws")
    public void webClassifyReadsClassFromGetAttributeWhenDomAttributeThrows() {
        String originalPlatform = com.shaft.driver.SHAFT.Properties.platform.targetPlatform();
        com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform("LINUX");
        try {
            WebElement element = mock(WebElement.class);
            when(element.getTagName()).thenReturn(null);
            when(element.getDomAttribute(anyString())).thenThrow(new RuntimeException("dom attribute unsupported"));
            when(element.getAttribute("class")).thenReturn("android.widget.Button");
            Assert.assertEquals(ElementClassifier.classify(element), ElementKind.BUTTON);
            org.mockito.Mockito.verify(element).getAttribute("class");
        } finally {
            com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
        }
    }

    @Test(description = "Issue #6833: a failed native attribute read is not retried through getAttribute")
    public void nativeAttributeFailureIsNotRetried() {
        String originalPlatform = com.shaft.driver.SHAFT.Properties.platform.targetPlatform();
        com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform("Android");
        try {
            WebElement element = mock(WebElement.class);
            when(element.getTagName()).thenReturn("android.widget.EditText");
            when(element.getDomAttribute(anyString())).thenThrow(new org.openqa.selenium.WebDriverException("unknown attribute"));
            ElementClassifier.classify(element);
            org.mockito.Mockito.verify(element, org.mockito.Mockito.never()).getAttribute(anyString());
        } finally {
            com.shaft.driver.SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
        }
    }

    @Test(description = "#6775 nightly: Flutter reports enabled=false on editable fields; native classification must not reject type()")
    public void nativeEnabledFalseIsNotClassifiedDisabled() {
        String original = com.shaft.driver.SHAFT.Properties.mobile.automationName();
        com.shaft.driver.SHAFT.Properties.mobile.set().automationName("FlutterIntegration");
        try {
            assertFlutterFieldIsTypeable();
        } finally {
            com.shaft.driver.SHAFT.Properties.mobile.set().automationName(original);
        }
    }

    private static void assertFlutterFieldIsTypeable() {
        WebElement field = mock(WebElement.class);
        when(field.getTagName()).thenReturn("android.widget.EditText");
        when(field.getDomAttribute("enabled")).thenReturn("false");
        when(field.getDomAttribute("class")).thenReturn("android.widget.EditText");
        Assert.assertNotEquals(ElementClassifier.classify(ElementClassifier.fromNativeElement(field)), ElementKind.DISABLED);
    }
}
