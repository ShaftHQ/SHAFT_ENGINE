package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
import com.shaft.gui.element.internal.interaction.ElementClassifier;
import com.shaft.gui.element.internal.interaction.ElementKind;
import com.shaft.gui.element.internal.interaction.ElementSignals;
import com.shaft.gui.element.internal.interaction.TypeStrategies;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.TraceEventRecorder;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Wave C (#5732): Playwright type/click strategies mirroring Wave B kinds.
 */
@Test(singleThreaded = true)
public class PlaywrightInteractionStrategiesUnitTest {

    @AfterMethod(alwaysRun = true)
    public void clearState() {
        TraceEventRecorder.clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void typeTextLikeUsesFillNotPressSequentially() {
        Locator locator = locatorWithSignals(signals("INPUT", "text"));
        new ElementActions(mock(PlaywrightSession.class)).type(locator, "hello");
        verify(locator).evaluate(anyString());
        verify(locator).fill("hello");
        verify(locator, never()).pressSequentially(anyString());
        verify(locator, never()).click();
    }

    @Test
    public void typeCheckboxClicksAndNeverFill() {
        Locator locator = locatorWithSignals(signals("INPUT", "checkbox"));
        new ElementActions(mock(PlaywrightSession.class)).type(locator, "ignored");
        verify(locator).click();
        verify(locator, never()).fill(anyString());
        verify(locator, never()).pressSequentially(anyString());
    }

    @Test
    public void typeSelectUsesSelectOptionNotFill() {
        Locator locator = locatorWithSignals(signals("SELECT", ""));
        new ElementActions(mock(PlaywrightSession.class)).type(locator, "Egypt");
        verify(locator).selectOption("Egypt");
        verify(locator, never()).fill(anyString());
        verify(locator, never()).pressSequentially(anyString());
    }

    @Test
    public void typeFileUsesSetInputFilesNotFill() {
        Locator locator = locatorWithSignals(signals("INPUT", "file"));
        new ElementActions(mock(PlaywrightSession.class)).type(locator, "/tmp/upload.txt");
        verify(locator).setInputFiles(Path.of("/tmp/upload.txt"));
        verify(locator, never()).fill(anyString());
        verify(locator, never()).clear();
    }

    @Test
    public void typeContentEditableUsesPressSequentiallyNotFill() {
        Map<String, Object> map = signals("DIV", "");
        map.put("contentEditable", "true");
        map.put("isContentEditable", "true");
        Locator locator = locatorWithSignals(map);
        new ElementActions(mock(PlaywrightSession.class)).type(locator, "editor text");
        verify(locator).evaluate(anyString());
        verify(locator).press("ControlOrMeta+A");
        verify(locator).pressSequentially("editor text");
        verify(locator, never()).fill(anyString());
    }

    @Test
    public void typeDateUsesFill() {
        Locator locator = locatorWithSignals(signals("INPUT", "date"));
        new ElementActions(mock(PlaywrightSession.class)).type(locator, "2024-01-15");
        verify(locator).fill("2024-01-15");
        verify(locator, never()).pressSequentially(anyString());
    }

    @Test
    public void typeMaskedInputUsesPressSequentially() {
        Map<String, Object> map = signals("INPUT", "text");
        map.put("dataMask", "00/00/0000");
        Locator locator = locatorWithSignals(map);
        new ElementActions(mock(PlaywrightSession.class)).type(locator, "12/31/2024");
        verify(locator).evaluate(anyString());
        verify(locator).pressSequentially("12/31/2024");
        verify(locator, never()).fill(anyString());
    }

    @Test
    public void typeReadonlyRejects() {
        Map<String, Object> map = signals("INPUT", "text");
        map.put("readonly", "true");
        Locator locator = locatorWithSignals(map);
        Assert.expectThrows(IllegalStateException.class,
                () -> new ElementActions(mock(PlaywrightSession.class)).type(locator, "nope"));
        verify(locator, never()).fill(anyString());
        verify(locator, never()).pressSequentially(anyString());
    }

    @Test
    public void typeAppendTextLikeUsesFillNotPressSequentially() {
        Locator locator = locatorWithSignals(signals("INPUT", "text"));
        when(locator.inputValue()).thenReturn("front");
        new ElementActions(mock(PlaywrightSession.class)).typeAppend(locator, " back");
        verify(locator).evaluate(anyString());
        verify(locator).fill("front back");
        verify(locator, never()).pressSequentially(anyString());
    }

    @Test
    public void typeAppendContentEditableUsesPressSequentiallyNotFill() {
        Map<String, Object> map = signals("DIV", "");
        map.put("contentEditable", "true");
        map.put("isContentEditable", "true");
        Locator locator = locatorWithSignals(map);
        new ElementActions(mock(PlaywrightSession.class)).typeAppend(locator, " more");
        // typeAppend + typeByKind(append) each readSignals via evaluate.
        verify(locator, atLeastOnce()).evaluate(anyString());
        verify(locator).pressSequentially(" more");
        verify(locator, never()).fill(anyString());
        verify(locator, never()).press("ControlOrMeta+A");
    }

    @Test
    public void typeAppendMaskedInputUsesPressSequentially() {
        Map<String, Object> map = signals("INPUT", "text");
        map.put("dataMask", "00/00/0000");
        Locator locator = locatorWithSignals(map);
        new ElementActions(mock(PlaywrightSession.class)).typeAppend(locator, "2024");
        verify(locator, atLeastOnce()).evaluate(anyString());
        verify(locator).pressSequentially("2024");
        verify(locator, never()).fill(anyString());
    }

    @Test
    public void clickUsesDefaultActionabilityWithoutForceOptions() {
        Locator locator = mock(Locator.class);
        when(locator.toString()).thenReturn("getByRole(\"button\")");
        new ElementActions(mock(PlaywrightSession.class)).click(locator);
        // Default Locator.click() — no ClickOptions overload (force/trial stay off / unexposed).
        verify(locator).click();
    }

    @Test
    public void playwrightRoutesMatchWaveCMatrix() {
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.TEXT_LIKE, false),
                TypeStrategies.PlaywrightTypeRoute.FILL);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.TEXT_LIKE, true),
                TypeStrategies.PlaywrightTypeRoute.PRESS_SEQUENTIALLY);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.CONTENTEDITABLE, false),
                TypeStrategies.PlaywrightTypeRoute.PRESS_SEQUENTIALLY);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.CHECKBOX, false),
                TypeStrategies.PlaywrightTypeRoute.TOGGLE_CLICK);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.SELECT, false),
                TypeStrategies.PlaywrightTypeRoute.SELECT_OPTION);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.FILE, false),
                TypeStrategies.PlaywrightTypeRoute.SET_FILES);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.DATE_LIKE, false),
                TypeStrategies.PlaywrightTypeRoute.FILL);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.DISABLED, false),
                TypeStrategies.PlaywrightTypeRoute.REJECT);
    }

    @Test
    public void looksMaskedIsConservative() {
        Assert.assertFalse(ElementClassifier.looksMasked(
                ElementSignals.of("input", "text", null, null, null, null, null, null)));
        Assert.assertTrue(ElementClassifier.looksMasked(new ElementSignals(
                "input", "text", null, null, null, null, null, null,
                "99/99/9999", null, null, null, null)));
        Assert.assertTrue(ElementClassifier.looksMasked(new ElementSignals(
                "input", "text", null, null, null, null, null, null,
                null, null, null, "form-control imask", null)));
        Assert.assertTrue(ElementClassifier.looksMasked(new ElementSignals(
                "input", "text", null, null, null, null, null, null,
                null, null, null, null, "one-time-code")));
        Assert.assertFalse(ElementClassifier.looksMasked(new ElementSignals(
                "input", "text", null, null, null, null, null, null,
                null, null, null, "unmasked-field", null)));
    }

    @Test
    public void classifyFromEvaluateMapMatchesWebKinds() {
        Assert.assertEquals(ElementClassifier.classify(ElementClassifier.fromEvaluateMap(
                signals("INPUT", "checkbox"))), ElementKind.CHECKBOX);
        Assert.assertEquals(ElementClassifier.classify(ElementClassifier.fromEvaluateMap(
                signals("SELECT", ""))), ElementKind.SELECT);
        Map<String, Object> editable = signals("DIV", "");
        editable.put("contentEditable", "true");
        Assert.assertEquals(ElementClassifier.classify(ElementClassifier.fromEvaluateMap(editable)),
                ElementKind.CONTENTEDITABLE);
    }

    private static Locator locatorWithSignals(Map<String, Object> signals) {
        Locator locator = mock(Locator.class);
        when(locator.evaluate(anyString())).thenReturn(signals);
        when(locator.toString()).thenReturn("locator");
        return locator;
    }

    private static Map<String, Object> signals(String tagName, String type) {
        Map<String, Object> map = new HashMap<>();
        map.put("tagName", tagName);
        map.put("type", type);
        map.put("role", "");
        map.put("contentEditable", null);
        map.put("isContentEditable", "false");
        map.put("disabled", null);
        map.put("readonly", null);
        map.put("ariaDisabled", null);
        map.put("dataMask", null);
        map.put("dataInputmask", null);
        map.put("mask", null);
        map.put("className", "");
        map.put("autocomplete", null);
        return map;
    }
}
