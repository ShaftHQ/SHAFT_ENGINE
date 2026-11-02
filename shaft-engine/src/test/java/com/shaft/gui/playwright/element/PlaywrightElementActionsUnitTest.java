package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.options.BoundingBox;
import com.microsoft.playwright.Page;
import com.shaft.gui.internal.locator.CompositeLocator;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.driver.ElementTarget;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.TraceEventRecorder;
import com.shaft.tools.io.internal.TraceEventRecorderTestProbe;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import com.shaft.gui.playwright.validation.PlaywrightElementValidationsBuilder;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.annotations.AfterMethod;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class PlaywrightElementActionsUnitTest {
    @AfterMethod(alwaysRun = true)
    public void clearTraceState() {
        TraceEventRecorder.clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void clickShouldEmitOnePlaywrightBackendTraceAction() {
        SHAFT.Properties.reporting.set().traceEnabled(true);
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        when(locator.toString()).thenReturn("getByRole(\"button\", name=\"Save\")");

        new ElementActions(session).click(locator);

        String json = TraceEventRecorderTestProbe.json();
        Assert.assertEquals(count(json, "\"name\": \"click\""), 1);
        Assert.assertTrue(json.contains("\"status\": \"passed\""), json);
        Assert.assertEquals(TraceEventRecorderTestProbe.latestBackend(), "MICROSOFT_PLAYWRIGHT");
        Assert.assertTrue(json.contains("getByRole"), json);
        verify(locator).click();
    }

    @Test
    public void failedClickShouldRecordFailureAndRethrowTheOriginalException() {
        SHAFT.Properties.reporting.set().traceEnabled(true);
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        IllegalStateException expected = new IllegalStateException("provider click failed");
        Mockito.doThrow(expected).when(locator).click();

        IllegalStateException actual = Assert.expectThrows(IllegalStateException.class,
                () -> new ElementActions(session).click(locator));

        Assert.assertSame(actual, expected);
        String json = TraceEventRecorderTestProbe.json();
        Assert.assertTrue(json.contains("\"status\": \"failed\""), json);
        Assert.assertTrue(json.contains("provider click failed"), json);
        Assert.assertEquals(count(json, "\"name\": \"click\""), 1);
    }

    @Test
    public void smartClickAndTypeShouldResolveStringNames() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        when(session.page()).thenReturn(page);
        when(page.locator(anyString())).thenReturn(locator);
        when(locator.count()).thenReturn(1);
        when(locator.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT",
                "type", "email",
                "role", "",
                "isContentEditable", "false"));

        ElementActions actions = new ElementActions(session);

        Assert.assertSame(actions.click("Save"), actions);
        Assert.assertSame(actions.type("Email", "user@example.com"), actions);
        verify(locator).click();
        verify(locator).fill("user@example.com");
        verify(locator, never()).pressSequentially(anyString());
    }

    @Test
    public void compositeLocatorShouldUseFirstUniqueConvertibleAlternative() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator primary = mock(Locator.class);
        Locator secondary = mock(Locator.class);
        when(session.page()).thenReturn(page);
        when(page.locator("[id=\"primary\"]")).thenReturn(primary);
        when(page.locator("xpath=//button")).thenReturn(secondary);
        when(primary.count()).thenReturn(2);
        when(secondary.count()).thenReturn(1);

        ElementActions actions = new ElementActions(session);

        Assert.assertSame(actions.click(new TestCompositeLocator(By.id("primary"), By.xpath("//button"))), actions);
        verify(primary, never()).click();
        verify(secondary).click();
    }

    @Test
    public void typeAppendShouldFillCurrentValueInOneOperation() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        // Stub evaluate so TEXT_LIKE → fill is proven (not UNKNOWN fill-fallback).
        when(locator.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT",
                "type", "text",
                "role", "",
                "isContentEditable", "false"));
        when(locator.inputValue()).thenReturn("frontend");

        ElementActions actions = new ElementActions(session);

        Assert.assertSame(actions.typeAppend(locator, " backend"), actions);
        verify(locator).evaluate(anyString());
        verify(locator).fill("frontend backend");
        verify(locator, never()).pressSequentially(anyString());
    }


    @Test
    public void locatorActionsShouldCoverCommonFluentPaths() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        com.microsoft.playwright.Mouse mouse = mock(com.microsoft.playwright.Mouse.class);
        Locator locator = mock(Locator.class);
        Locator destination = mock(Locator.class);
        BoundingBox box = new BoundingBox();
        box.x = 10;
        box.y = 20;
        box.width = 30;
        box.height = 40;

        when(session.page()).thenReturn(page);
        when(page.mouse()).thenReturn(mouse);
        when(locator.boundingBox()).thenReturn(box);
        when(locator.count()).thenReturn(3);
        when(locator.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT",
                "type", "text",
                "role", "",
                "isContentEditable", "false"));
        when(locator.inputValue()).thenReturn("");

        ElementActions actions = new ElementActions(session);
        Assert.assertSame(actions.click(locator), actions);
        Assert.assertSame(actions.clickUsingJavascript(locator), actions);
        Assert.assertSame(actions.scrollToElement(locator), actions);
        Assert.assertSame(actions.clickAndHold(locator), actions);
        Assert.assertSame(actions.doubleClick(locator), actions);
        Assert.assertSame(actions.hover(locator), actions);
        Assert.assertSame(actions.dragAndDrop(locator, destination), actions);
        Assert.assertSame(actions.dragAndDropByOffset(locator, 4, 5), actions);
        Assert.assertSame(actions.select(locator, "Option A"), actions);
        Assert.assertSame(actions.type(locator, "hello"), actions);
        Assert.assertSame(actions.clear(locator), actions);
        Assert.assertSame(actions.typeSecure(locator, "secret"), actions);
        Assert.assertSame(actions.typeFileLocationForUpload(locator, "/tmp/a.txt"), actions);
        Assert.assertEquals(actions.getElementsCount(locator), 3);

        verify(locator, atLeastOnce()).click();
        verify(locator).evaluate("element => element.click()");
        verify(locator).scrollIntoViewIfNeeded();
        verify(locator).dblclick();
        verify(locator).hover();
        verify(locator).dragTo(destination);
        verify(locator).selectOption("Option A");
        verify(locator, atLeastOnce()).fill("hello");
        verify(locator).clear();
        verify(locator).setInputFiles(java.nio.file.Path.of("/tmp/a.txt"));
        verify(locator, atLeastOnce()).fill("secret");
        verify(mouse, atLeastOnce()).down();
        verify(mouse, atLeastOnce()).up();
    }

    @Test
    public void typeOnContentEditableShouldPressSequentially() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        when(locator.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "DIV",
                "type", "",
                "role", "",
                "isContentEditable", "true"));
        ElementActions actions = new ElementActions(session);
        Assert.assertSame(actions.type(locator, "abc"), actions);
        verify(locator).pressSequentially("abc");
        verify(locator, never()).fill(anyString());
    }

    @Test
    public void getTableRowsDataShouldMapHeaderCells() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator table = mock(Locator.class);
        when(table.evaluate(anyString())).thenReturn(List.of(
                List.of("Name", "Age"),
                List.of("Ada", "36"),
                List.of("Grace", "40")));
        List<Map<String, String>> data = new ElementActions(session).getTableRowsData(table);
        Assert.assertEquals(data.size(), 2);
        Assert.assertEquals(data.get(0).get("Name"), "Ada");
        Assert.assertEquals(data.get(1).get("Age"), "40");
    }


    @Test
    public void fluentHelpersAndUnsupportedIframeShouldBeDeterministic() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        ElementActions actions = new ElementActions(session);

        Assert.assertSame(actions.and(), actions);
        Assert.assertSame(actions.switchToDefaultContent(), actions);
        Assert.assertSame(actions.switchToParentFrame(), actions);
        Assert.assertEquals(actions.getCurrentFrame(), "PLAYWRIGHT_PAGE");
        Assert.expectThrows(UnsupportedOperationException.class, () -> actions.switchToIframe(By.id("frame")));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> actions.executeNativeMobileCommand("mobile: shell", Map.of("cmd", "echo")));
        Assert.assertNotNull(actions.assertThat(locator));
        Assert.assertNotNull(actions.verifyThat(locator));
        Assert.assertTrue(actions.assertThat(locator) instanceof PlaywrightElementValidationsBuilder);
        Assert.assertTrue(actions.verifyThat(locator) instanceof PlaywrightElementValidationsBuilder);
    }

    @Test
    public void setValueAndSubmitFormUsingJavascriptShouldEvaluateScripts() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        ElementActions actions = new ElementActions(session);
        Assert.assertSame(actions.setValueUsingJavaScript(locator, "typed-via-js"), actions);
        Assert.assertSame(actions.submitFormUsingJavaScript(locator), actions);
        verify(locator).evaluate(contains("element.value = value"), eq("typed-via-js"));
        verify(locator).evaluate(contains("element.form ? element.form.submit()"));
    }

    @Test
    public void typeRoutesShouldCoverToggleSelectFileAndReject() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator checkbox = mock(Locator.class);
        Locator select = mock(Locator.class);
        Locator file = mock(Locator.class);
        Locator disabled = mock(Locator.class);

        when(checkbox.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "checkbox", "role", "checkbox", "isContentEditable", "false"));
        when(select.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "SELECT", "type", "", "role", "", "isContentEditable", "false"));
        when(file.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "file", "role", "", "isContentEditable", "false"));
        when(disabled.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "text", "role", "", "isContentEditable", "false",
                "disabled", "true"));

        ElementActions actions = new ElementActions(session);
        Assert.assertSame(actions.type(checkbox, "ignored"), actions);
        Assert.assertSame(actions.type(select, "Option Z"), actions);
        Assert.assertSame(actions.type(file, "/tmp/upload.bin"), actions);
        Assert.expectThrows(IllegalStateException.class, () -> actions.type(disabled, "nope"));

        verify(checkbox).click();
        verify(select).selectOption("Option Z");
        verify(file).setInputFiles(java.nio.file.Path.of("/tmp/upload.bin"));
    }

    @Test
    public void captureScreenshotAndAriaSnapshotShouldUseLocatorApis() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        Page page = mock(Page.class);
        when(locator.page()).thenReturn(page);
        when(locator.screenshot(any())).thenReturn(new byte[] {1, 2, 3, 4});
        when(locator.evaluate(anyString())).thenReturn(List.of(Map.of("role", "button", "name", "Save")));

        ElementActions actions = new ElementActions(session);
        Assert.assertSame(actions.captureScreenshot(locator), actions);
        String snapshot = actions.ariaSnapshot(locator);
        Assert.assertNotNull(snapshot);
        verify(locator).screenshot(any());
        verify(locator, atLeastOnce()).evaluate(anyString());
    }

    @Test
    public void getTableRowsDataShouldHandleEmptyAndBlankHeaders() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator empty = mock(Locator.class);
        Locator blankHeaders = mock(Locator.class);
        when(empty.evaluate(anyString())).thenReturn(List.of());
        when(blankHeaders.evaluate(anyString())).thenReturn(List.of(
                List.of(" ", ""),
                List.of("a", "b")));
        ElementActions actions = new ElementActions(session);
        Assert.assertTrue(actions.getTableRowsData(empty).isEmpty());
        List<Map<String, String>> rows = actions.getTableRowsData(blankHeaders);
        Assert.assertEquals(rows.size(), 1);
        Assert.assertEquals(rows.get(0).get("column1"), "a");
        Assert.assertEquals(rows.get(0).get("column2"), "b");
    }

    @Test
    public void typeAppendOnPressSequentiallyRouteShouldNotUseFill() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        when(locator.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "DIV", "type", "", "role", "", "isContentEditable", "true"));
        when(locator.inputValue()).thenThrow(new RuntimeException("not an input"));
        when(locator.textContent()).thenReturn("pre");
        ElementActions actions = new ElementActions(session);
        Assert.assertSame(actions.typeAppend(locator, "fix"), actions);
        verify(locator).pressSequentially("fix");
        verify(locator, never()).fill(anyString());
    }


    @Test
    public void typeDateRangeColorAndMaskedShouldUseFillOrPress() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator date = mock(Locator.class);
        Locator range = mock(Locator.class);
        Locator color = mock(Locator.class);
        Locator masked = mock(Locator.class);
        when(date.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "date", "role", "", "isContentEditable", "false"));
        when(range.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "range", "role", "", "isContentEditable", "false"));
        when(color.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "color", "role", "", "isContentEditable", "false"));
        when(masked.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "text", "role", "textbox", "isContentEditable", "false",
                "dataMask", "true"));
        ElementActions actions = new ElementActions(session);
        Assert.assertSame(actions.type(date, "2020-01-02"), actions);
        Assert.assertSame(actions.type(range, "7"), actions);
        Assert.assertSame(actions.type(color, "#fff"), actions);
        Assert.assertSame(actions.type(masked, "1234"), actions);
        verify(date).fill("2020-01-02");
        verify(range).fill("7");
        verify(color).fill("#fff");
        verify(masked).pressSequentially("1234");
    }


    @Test
    public void byAndShaftLocatorOverloadsShouldResolveThroughPage() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        Locator destination = mock(Locator.class);
        com.microsoft.playwright.Mouse mouse = mock(com.microsoft.playwright.Mouse.class);
        when(session.page()).thenReturn(page);
        when(page.locator("[id=\"cov\"]")).thenReturn(locator);
        when(page.locator("[id=\"dest\"]")).thenReturn(destination);
        when(page.mouse()).thenReturn(mouse);
        when(locator.count()).thenReturn(1);
        BoundingBox box = new BoundingBox();
        box.x = 10;
        box.y = 20;
        box.width = 30;
        box.height = 40;
        when(locator.boundingBox()).thenReturn(box);
        when(locator.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "INPUT", "type", "text", "role", "textbox", "isContentEditable", "false"));
        when(locator.inputValue()).thenReturn("base");
        when(locator.page()).thenReturn(page);
        when(locator.screenshot(any())).thenReturn(new byte[] {9, 8, 7});
        when(destination.evaluate(anyString())).thenReturn(Map.of(
                "tagName", "BUTTON", "type", "button", "role", "button", "isContentEditable", "false"));

        ElementActions actions = new ElementActions(session);
        By by = By.id("cov");
        ShaftLocator shaft = ShaftLocator.from(by);
        By destBy = By.id("dest");
        ShaftLocator destShaft = ShaftLocator.from(destBy);

        Assert.assertSame(actions.click(by), actions);
        Assert.assertSame(actions.click(shaft), actions);
        Assert.assertSame(actions.clickUsingJavascript(by), actions);
        Assert.assertSame(actions.clickUsingJavascript(shaft), actions);
        Assert.assertSame(actions.scrollToElement(by), actions);
        Assert.assertSame(actions.scrollToElement(shaft), actions);
        Assert.assertSame(actions.clickAndHold(by), actions);
        Assert.assertSame(actions.clickAndHold(shaft), actions);
        Assert.assertSame(actions.doubleClick(by), actions);
        Assert.assertSame(actions.doubleClick(shaft), actions);
        Assert.assertSame(actions.hover(by), actions);
        Assert.assertSame(actions.hover(shaft), actions);
        Assert.assertSame(actions.dragAndDrop(by, destBy), actions);
        Assert.assertSame(actions.dragAndDrop(shaft, destShaft), actions);
        Assert.assertSame(actions.dragAndDropByOffset(by, 2, 3), actions);
        Assert.assertSame(actions.dragAndDropByOffset(shaft, 2, 3), actions);
        Assert.assertSame(actions.select(by, "Opt"), actions);
        Assert.assertSame(actions.select(shaft, "Opt"), actions);
        Assert.assertSame(actions.type(by, "typed"), actions);
        Assert.assertSame(actions.type(shaft, "typed"), actions);
        Assert.assertSame(actions.clear(by), actions);
        Assert.assertSame(actions.clear(shaft), actions);
        Assert.assertSame(actions.typeAppend(by, " more"), actions);
        Assert.assertSame(actions.typeAppend(shaft, " more"), actions);
        Assert.assertSame(actions.typeSecure(by, "secret"), actions);
        Assert.assertSame(actions.typeSecure(shaft, "secret"), actions);
        Assert.assertSame(actions.typeFileLocationForUpload(by, "/tmp/u.bin"), actions);
        Assert.assertSame(actions.typeFileLocationForUpload(shaft, "/tmp/u.bin"), actions);
        Assert.assertSame(actions.setValueUsingJavaScript(by, "js"), actions);
        Assert.assertSame(actions.setValueUsingJavaScript(shaft, "js"), actions);
        Assert.assertSame(actions.submitFormUsingJavaScript(by), actions);
        Assert.assertSame(actions.submitFormUsingJavaScript(shaft), actions);
        Assert.assertSame(actions.captureScreenshot(by), actions);
        Assert.assertSame(actions.captureScreenshot(shaft), actions);
        Assert.assertEquals(actions.getElementsCount(by), 1);
        Assert.assertEquals(actions.getElementsCount(shaft), 1);
        // aria/table By overloads need divergent evaluate payloads; covered via Locator APIs elsewhere.
        Assert.assertNotNull(actions.assertThat(by));
        Assert.assertNotNull(actions.assertThat(shaft));
        Assert.assertNotNull(actions.assertThat(ElementTarget.located(shaft)));
        Assert.assertNotNull(actions.verifyThat(by));
        Assert.assertNotNull(actions.verifyThat(shaft));
        Assert.assertNotNull(actions.verifyThat(ElementTarget.located(shaft)));
        Assert.assertSame(actions.hoverAndClick(List.of(by), destBy), actions);

        verify(locator, atLeastOnce()).click();
        verify(locator, atLeastOnce()).dblclick();
        verify(locator, atLeastOnce()).hover();
        verify(locator, atLeastOnce()).clear();
        verify(locator, atLeastOnce()).selectOption("Opt");
        verify(locator, atLeastOnce()).fill(anyString());
        verify(locator, atLeastOnce()).setInputFiles(java.nio.file.Path.of("/tmp/u.bin"));
        verify(locator, atLeastOnce()).screenshot(any());
        verify(locator, atLeastOnce()).dragTo(destination);
    }

    private static final class TestCompositeLocator extends By implements CompositeLocator {
        private final List<By> alternatives;

        private TestCompositeLocator(By... alternatives) {
            this.alternatives = List.of(alternatives);
        }

        @Override
        public List<By> alternatives() {
            return alternatives;
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return List.of();
        }

        @Override
        public String toString() {
            return "TestCompositeLocator";
        }
    }

    private static int count(String text, String needle) {
        int count = 0;
        int index = 0;
        while ((index = text.indexOf(needle, index)) >= 0) {
            count++;
            index += needle.length();
        }
        return count;
    }
}
