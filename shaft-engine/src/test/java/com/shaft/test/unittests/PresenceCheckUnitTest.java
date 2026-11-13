package com.shaft.test.unittests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.PageSourcePresence;
import com.shaft.gui.element.internal.PresenceProbe;
import com.shaft.gui.internal.locator.Locator;
import com.shaft.properties.internal.Properties;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidSelectorException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Issue #6842: report-free presence checks with an explicit timeout. */
@Test(singleThreaded = true)
public class PresenceCheckUnitTest {
    private static final By PROMPT = By.id("update-prompt");
    private static final By HOME = By.id("home");

    private WebDriver driver() {
        WebDriver driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        when(((JavascriptExecutor) driver).executeScript(anyString())).thenReturn("complete");
        when(driver.findElements(Mockito.any(By.class))).thenReturn(List.of());
        return driver;
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void absentElementReturnsFalseWithinTheExplicitTimeout() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        ElementActions actions = new ElementActions(driver(), true);
        long start = System.nanoTime();
        Assert.assertFalse(actions.isPresent(PROMPT, Duration.ofMillis(300)));
        Assert.assertTrue(Duration.ofNanos(System.nanoTime() - start).toMillis() < 3_000);
    }

    @Test
    public void waitUntilAnyPresentReturnsWhicheverLocatorAppeared() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        WebDriver driver = driver();
        when(driver.findElements(HOME)).thenReturn(List.of()).thenReturn(List.of(mock(WebElement.class)));
        ElementActions actions = new ElementActions(driver, true);
        Assert.assertEquals(actions.waitUntilAnyPresent(Duration.ofSeconds(5), PROMPT, HOME), Optional.of(HOME));
    }

    @Test
    public void transientLookupFailuresKeepPollingButInvalidLocatorsFail() {
        WebDriver driver = driver();
        when(driver.findElements(PROMPT)).thenThrow(new StaleElementReferenceException("mid-transition"))
                .thenReturn(List.of(mock(WebElement.class)));
        Assert.assertEquals(PresenceProbe.firstPresent(driver, Duration.ofSeconds(5), PROMPT), Optional.of(PROMPT));
        when(driver.findElements(HOME)).thenThrow(new InvalidSelectorException("bad"));
        Assert.assertThrows(InvalidSelectorException.class, () -> PresenceProbe.firstPresent(driver, Duration.ZERO, HOME));
        Assert.assertThrows(IllegalArgumentException.class, () -> PresenceProbe.firstPresent(driver, Duration.ZERO));
    }

    /** A {@code findElements} hit is present and counts as one even when {@code isDisplayed()} is false. */
    @Test
    public void hiddenFindElementsMatchIsPresentAndCountsAsOne() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        WebElement hidden = mock(WebElement.class);
        doReturn(false).when(hidden).isDisplayed();
        Mockito.clearInvocations(hidden); // the stubbing call must not count as a production call
        WebDriver driver = driver();
        when(driver.findElements(PROMPT)).thenReturn(List.of(hidden));
        ElementActions actions = new ElementActions(driver, true);

        Assert.assertTrue(actions.isPresent(PROMPT, Duration.ZERO));
        Assert.assertEquals(actions.getElementsCount(PROMPT), 1);
        verify(hidden, never()).isDisplayed();
    }

    /** A page-source node with {@code displayed=false} counts only on the page-source path. */
    @Test
    public void displayedFalsePageSourceNodeCountsWithoutFindElements() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        By checkbox = Locator.accessibilityId("generic_termsAndConditions_checkBox");
        WebDriver driver = driver();
        when(driver.getPageSource()).thenReturn("""
                <?xml version="1.0" encoding="UTF-8"?>
                <hierarchy>
                  <android.widget.CheckBox resource-id="com.example:id/generic_termsAndConditions_checkBox" content-desc="generic_termsAndConditions_checkBox" displayed="false" clickable="true"/>
                  <android.view.View resource-id="com.example:id/other" displayed="false"/>
                </hierarchy>
                """);
        ElementActions actions = new ElementActions(driver, true);

        Assert.assertFalse(actions.isPresent(checkbox, Duration.ZERO));
        Assert.assertEquals(actions.getElementsCount(checkbox), 0);
        Assert.assertTrue(actions.isPresentInPageSource(checkbox));
        Assert.assertEquals(actions.getPageSourceElementsCount(checkbox), 1);
        Assert.assertEquals(actions.getPageSourceElementsCount(By.id("generic_termsAndConditions_checkBox")), 1);
        Assert.assertEquals(actions.getPageSourceElementsCount(
                By.xpath("//*[@displayed='false' and @clickable='true']")), 1);
        Assert.assertThrows(IllegalArgumentException.class,
                () -> actions.getPageSourceElementsCount(By.cssSelector(".missing")));
    }

    /** A null page source is an empty document on the page-source path. */
    @Test
    public void nullPageSourceCountsAsZero() {
        WebDriver driver = driver();
        when(driver.getPageSource()).thenReturn(null);
        ElementActions actions = new ElementActions(driver, true);

        Assert.assertFalse(actions.isPresentInPageSource(By.id("any")));
        Assert.assertEquals(actions.getPageSourceElementsCount(By.id("any")), 0);
    }

    /** Blank documents, name and resource-id branches, and rejected locators. */
    @Test
    public void pageSourceCountCoversNameResourceIdAndRejections() {
        Assert.assertEquals(PageSourcePresence.count(null, By.id("a")), 0);
        Assert.assertEquals(PageSourcePresence.count("   ", By.id("a")), 0);
        Assert.assertThrows(IllegalArgumentException.class, () -> PageSourcePresence.count("<a/>", null));
        Assert.assertThrows(IllegalArgumentException.class, () -> PageSourcePresence.count("<a/>", By.id("")));
        Assert.assertThrows(IllegalArgumentException.class, () -> PageSourcePresence.count("<a/>", locator("By.custom")));
        Assert.assertThrows(IllegalArgumentException.class, () -> PageSourcePresence.count("not xml", By.id("a")));
        Assert.assertThrows(IllegalArgumentException.class, () -> PageSourcePresence.count("<a/>", By.xpath("(((")));

        String xml = """
                <?xml version="1.0"?>
                <hierarchy>
                  <node name="only-name"/>
                  <node name="other" text="only-text"/>
                  <node name="other" text="other" content-desc="only-desc"/>
                  <node id="plain"/>
                  <node resource-id="exact-id"/>
                  <node resource-id="com.example:id/short"/>
                  <node resource-id="com.example:id/only-access"/>
                </hierarchy>
                """;
        Assert.assertEquals(PageSourcePresence.count(xml, By.name("only-name")), 1);
        Assert.assertEquals(PageSourcePresence.count(xml, By.name("only-text")), 1);
        Assert.assertEquals(PageSourcePresence.count(xml, By.name("only-desc")), 1);
        Assert.assertEquals(PageSourcePresence.count(xml, By.name("missing")), 0);
        Assert.assertEquals(PageSourcePresence.count(xml, By.id("plain")), 1);
        Assert.assertEquals(PageSourcePresence.count(xml, By.id("exact-id")), 1);
        Assert.assertEquals(PageSourcePresence.count(xml, By.id("short")), 1);
        Assert.assertEquals(PageSourcePresence.count(xml, By.id("com.example:id/missing")), 0);
        Assert.assertEquals(PageSourcePresence.count(xml, locator("id: plain")), 1);
        Assert.assertEquals(PageSourcePresence.count(xml, Locator.accessibilityId("only-access")), 1);
    }

    private static By locator(String rendered) {
        return new By() {
            @Override
            public List<WebElement> findElements(SearchContext context) {
                return List.of();
            }

            @Override
            public String toString() {
                return rendered;
            }
        };
    }
}
