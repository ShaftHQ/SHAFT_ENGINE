package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
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

        ElementActions actions = new ElementActions(session);

        Assert.assertSame(actions.click("Save"), actions);
        Assert.assertSame(actions.type("Email", "user@example.com"), actions);
        verify(locator).click();
        verify(locator).fill("user@example.com");
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
        when(locator.inputValue()).thenReturn("frontend");

        ElementActions actions = new ElementActions(session);

        Assert.assertSame(actions.typeAppend(locator, " backend"), actions);
        verify(locator).fill("frontend backend");
        verify(locator, never()).pressSequentially(anyString());
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
