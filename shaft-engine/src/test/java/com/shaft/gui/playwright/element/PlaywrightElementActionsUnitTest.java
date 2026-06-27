package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.gui.internal.locator.CompositeLocator;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class PlaywrightElementActionsUnitTest {

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
}
