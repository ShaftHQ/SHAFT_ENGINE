package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.options.BoundingBox;
import com.microsoft.playwright.Page;
import com.shaft.gui.internal.locator.CompositeLocator;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.TraceEventRecorder;
import com.shaft.tools.io.internal.TraceEventRecorderTestProbe;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.annotations.AfterMethod;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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

        verify(locator, Mockito.atLeastOnce()).click();
        verify(locator).evaluate("element => element.click()");
        verify(locator).scrollIntoViewIfNeeded();
        verify(locator).dblclick();
        verify(locator).hover();
        verify(locator).dragTo(destination);
        verify(locator).selectOption("Option A");
        verify(locator, Mockito.atLeastOnce()).fill("hello");
        verify(locator).clear();
        verify(locator).setInputFiles(java.nio.file.Path.of("/tmp/a.txt"));
        verify(locator, Mockito.atLeastOnce()).fill("secret");
        verify(mouse, Mockito.atLeastOnce()).down();
        verify(mouse, Mockito.atLeastOnce()).up();
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
