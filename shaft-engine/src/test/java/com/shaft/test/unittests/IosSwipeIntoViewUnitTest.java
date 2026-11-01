package com.shaft.test.unittests;

import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.element.internal.IosScrollView;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.RemoteWebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

public class IosSwipeIntoViewUnitTest {

    @Test
    public void swipeElementIntoViewUsesDragOnTallestScrollView() throws Exception {
        IOSDriver driver = createMockIOSDriver();
        when(driver.manage().window().getSize()).thenReturn(new Dimension(390, 844));
        when(driver.getPageSource()).thenReturn("before", "after-drag", "after-drag");

        RemoteWebElement toolbar = mock(RemoteWebElement.class);
        when(toolbar.isDisplayed()).thenReturn(true);
        when(toolbar.getRect()).thenReturn(new org.openqa.selenium.Rectangle(0, 0, 60, 390));

        RemoteWebElement list = mock(RemoteWebElement.class);
        when(list.isDisplayed()).thenReturn(true);
        when(list.getRect()).thenReturn(new org.openqa.selenium.Rectangle(0, 80, 700, 390));

        WebElement hidden = mock(WebElement.class);
        when(hidden.isDisplayed()).thenReturn(false);
        when(hidden.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, 900, 40, 100));

        WebElement visible = onScreenElement();
        AtomicInteger drags = new AtomicInteger();
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> {
            By by = invocation.getArgument(0);
            if (String.valueOf(by).contains("XCUIElementTypeScrollView")) {
                return List.of(toolbar, list);
            }
            return drags.get() >= 1 ? List.of(visible) : List.of(hidden);
        });
        doAnswer(invocation -> {
            drags.incrementAndGet();
            return null;
        }).when(driver).executeScript(eq("mobile: dragFromToForDuration"), anyMap());

        TouchActions touchActions = touchActionsWithScreenshots(driver);
        TouchActions returned = touchActions.swipeElementIntoView(By.id("serviceRow"), TouchActions.SwipeDirection.DOWN);

        Assert.assertSame(returned, touchActions);
        verify(driver, atLeastOnce()).executeScript(eq("mobile: dragFromToForDuration"), anyMap());
        verify(driver, never()).executeScript(eq("mobile: scroll"), anyMap());

        @SuppressWarnings("unchecked")
        var captor = org.mockito.ArgumentCaptor.forClass(Map.class);
        verify(driver, atLeastOnce()).executeScript(eq("mobile: dragFromToForDuration"), captor.capture());
        Map<String, Object> drag = captor.getAllValues().getFirst();
        Map<String, Object> expected = IosScrollView.dragParameters(
                TouchActions.SwipeDirection.DOWN, list.getRect());
        Assert.assertEquals(drag.get("duration"), expected.get("duration"));
        Assert.assertEquals(drag.get("fromX"), expected.get("fromX"));
        Assert.assertEquals(drag.get("fromY"), expected.get("fromY"));
        Assert.assertEquals(drag.get("toX"), expected.get("toX"));
        Assert.assertEquals(drag.get("toY"), expected.get("toY"));
    }

    @Test
    public void swipeElementIntoViewDragsUpToRevealUpperRows() throws Exception {
        IOSDriver driver = createMockIOSDriver();
        when(driver.manage().window().getSize()).thenReturn(new Dimension(390, 844));
        when(driver.getPageSource()).thenReturn("a", "b");

        RemoteWebElement list = mock(RemoteWebElement.class);
        when(list.isDisplayed()).thenReturn(true);
        when(list.getRect()).thenReturn(new org.openqa.selenium.Rectangle(0, 80, 700, 390));

        WebElement hidden = mock(WebElement.class);
        when(hidden.isDisplayed()).thenReturn(false);
        when(hidden.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, -40, 40, 100));
        WebElement visible = onScreenElement();
        AtomicInteger drags = new AtomicInteger();
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> {
            By by = invocation.getArgument(0);
            if (String.valueOf(by).contains("XCUIElementTypeScrollView")) {
                return List.of(list);
            }
            return drags.get() >= 1 ? List.of(visible) : List.of(hidden);
        });
        doAnswer(invocation -> {
            drags.incrementAndGet();
            return null;
        }).when(driver).executeScript(eq("mobile: dragFromToForDuration"), anyMap());

        touchActionsWithScreenshots(driver)
                .swipeElementIntoView(By.id("headerRow"), TouchActions.SwipeDirection.UP);

        @SuppressWarnings("unchecked")
        var captor = org.mockito.ArgumentCaptor.forClass(Map.class);
        verify(driver, atLeastOnce()).executeScript(eq("mobile: dragFromToForDuration"), captor.capture());
        Map<String, Object> drag = captor.getAllValues().getFirst();
        Map<String, Object> expected = IosScrollView.dragParameters(
                TouchActions.SwipeDirection.UP, list.getRect());
        Assert.assertEquals(drag.get("fromY"), expected.get("fromY"));
        Assert.assertEquals(drag.get("toY"), expected.get("toY"));
        Assert.assertTrue(((Number) drag.get("fromY")).intValue() < ((Number) drag.get("toY")).intValue());
    }

    @Test
    public void swipeElementIntoViewFallsBackToMobileScrollWhenNoScrollView() throws Exception {
        IOSDriver driver = createMockIOSDriver();
        when(driver.manage().window().getSize()).thenReturn(new Dimension(390, 844));
        when(driver.getPageSource()).thenReturn("same");

        WebElement hidden = mock(WebElement.class);
        when(hidden.isDisplayed()).thenReturn(false);
        when(hidden.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, 900, 40, 100));
        WebElement visible = onScreenElement();
        AtomicInteger scrolls = new AtomicInteger();
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> {
            By by = invocation.getArgument(0);
            if (String.valueOf(by).contains("XCUIElementTypeScrollView")) {
                return List.of();
            }
            return scrolls.get() >= 1 ? List.of(visible) : List.of(hidden);
        });
        doAnswer(invocation -> {
            scrolls.incrementAndGet();
            return Boolean.TRUE;
        }).when(driver).executeScript(eq("mobile: scroll"), anyMap());

        touchActionsWithScreenshots(driver)
                .swipeElementIntoView(By.id("row"), TouchActions.SwipeDirection.DOWN);

        verify(driver, atLeastOnce()).executeScript(eq("mobile: scroll"), anyMap());
        verify(driver, never()).executeScript(eq("mobile: dragFromToForDuration"), anyMap());
    }

    @Test
    public void swipeElementIntoViewUsesCallerScrollableForDrag() throws Exception {
        IOSDriver driver = createMockIOSDriver();
        when(driver.manage().window().getSize()).thenReturn(new Dimension(390, 844));
        when(driver.getPageSource()).thenReturn("1", "2");

        RemoteWebElement container = mock(RemoteWebElement.class);
        when(container.getId()).thenReturn("services-scroll");
        when(container.isDisplayed()).thenReturn(true);
        when(container.getRect()).thenReturn(new org.openqa.selenium.Rectangle(0, 120, 600, 390));

        WebElement hidden = mock(WebElement.class);
        when(hidden.isDisplayed()).thenReturn(false);
        when(hidden.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, 900, 40, 100));
        WebElement visible = onScreenElement();
        AtomicInteger drags = new AtomicInteger();
        when(driver.findElements(any(By.class))).thenAnswer(invocation ->
                drags.get() >= 1 ? List.of(visible) : List.of(hidden));
        doAnswer(invocation -> {
            drags.incrementAndGet();
            return null;
        }).when(driver).executeScript(eq("mobile: dragFromToForDuration"), anyMap());

        ElementActionsHelper helper = screenshotHelper();
        when(helper.identifyUniqueElement(any(), eq(AppiumBy.accessibilityId("dashboard_services_scroll"))))
                .thenReturn(List.of("dashboard_services_scroll", container));
        TouchActions touchActions = new TouchActions(driver);
        injectElementActionsHelper(touchActions, helper);

        touchActions.swipeElementIntoView(AppiumBy.accessibilityId("dashboard_services_scroll"),
                By.id("row"), TouchActions.SwipeDirection.DOWN);

        @SuppressWarnings("unchecked")
        var captor = org.mockito.ArgumentCaptor.forClass(Map.class);
        verify(driver, atLeastOnce()).executeScript(eq("mobile: dragFromToForDuration"), captor.capture());
        Map<String, Object> drag = captor.getAllValues().getFirst();
        Map<String, Object> expected = IosScrollView.dragParameters(
                TouchActions.SwipeDirection.DOWN, container.getRect());
        Assert.assertEquals(drag, expected);
        verify(driver, never()).findElements(eq(IosScrollView.SCROLL_VIEW));
    }

    private TouchActions touchActionsWithScreenshots(WebDriver driver) throws Exception {
        TouchActions touchActions = new TouchActions(driver);
        injectElementActionsHelper(touchActions, screenshotHelper());
        return touchActions;
    }

    private ElementActionsHelper screenshotHelper() {
        ElementActionsHelper helper = mock(ElementActionsHelper.class);
        when(helper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        return helper;
    }

    private WebElement onScreenElement() {
        WebElement onScreen = mock(WebElement.class);
        when(onScreen.isDisplayed()).thenReturn(true);
        when(onScreen.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, 10, 20, 20));
        return onScreen;
    }

    private IOSDriver createMockIOSDriver() {
        IOSDriver driver = mock(IOSDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(390, 844));
        when(driver.findElements(any(By.class))).thenReturn(List.of());
        return driver;
    }

    private void injectElementActionsHelper(TouchActions touchActions, ElementActionsHelper elementActionsHelper)
            throws Exception {
        Field elementActionsHelperField = FluentWebDriverAction.class.getDeclaredField("elementActionsHelper");
        elementActionsHelperField.setAccessible(true);
        elementActionsHelperField.set(touchActions, elementActionsHelper);
    }
}
