package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.MobileSwipeDirection;
import com.shaft.gui.element.TouchActions;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.List;

public class MobileGestureActionsTest {
    @Test
    public void categorizedGesturesShouldDelegateToTheRetainedTouchImplementation() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("gesture-delegate"));
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        TouchActions touch = Mockito.mock(TouchActions.class, Mockito.RETURNS_SELF);
        GestureActions gestures = new GestureActions(mobile, touch);
        By source = By.id("source");
        By destination = By.id("destination");
        Duration duration = Duration.ofMillis(250);

        Assert.assertSame(gestures.tap().on(source).at(10, 20).doubleOn(source).longPress(source).and(), gestures);
        Assert.assertSame(gestures.swipe().fromTo(source, destination).byOffset(source, 3, 4)
                .fromTo(1, 2, 30, 40, duration)
                .intoView(destination, MobileSwipeDirection.UP)
                .toEnd(MobileSwipeDirection.DOWN).and(), gestures);
        Assert.assertSame(gestures.drag().fromTo(source, destination).and(), gestures);
        Assert.assertSame(gestures.zoom().in().out().and(), gestures);

        Mockito.verify(touch).tap(source);
        Mockito.verify(touch).tapByCoordinates(10, 20);
        Mockito.verify(touch).doubleTap(source);
        Mockito.verify(touch).longTap(source);
        Mockito.verify(touch).swipeByOffset(source, 3, 4);
        Mockito.verify(touch).swipeByCoordinates(1, 2, 30, 40, 250);
        Mockito.verify(touch).swipeElementIntoView(destination, TouchActions.SwipeDirection.UP);
        Mockito.verify(touch).swipeToEndOfView(TouchActions.SwipeDirection.DOWN);
        Mockito.verify(touch, Mockito.times(2)).swipeToElement(source, destination);
        Mockito.verify(touch).pinchToZoom(TouchActions.ZoomDirection.IN);
        Mockito.verify(touch).pinchToZoom(TouchActions.ZoomDirection.OUT);
    }

    @Test
    public void realRetainedTouchBoundaryShouldExposeTheOriginalProviderFailure() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("gesture-real-provider"));
        IllegalStateException providerFailure = new IllegalStateException("native perform failed");
        Mockito.doThrow(providerFailure).when(driver).perform(Mockito.anyCollection());
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        GestureActions gestures = new GestureActions(mobile, new TouchActions(driver));

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> gestures.tap().at(4, 8));

        Assert.assertSame(thrown, providerFailure);
    }

    @Test
    public void realElementActionBoundaryShouldExposeTheOriginalProviderFailure() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("gesture-real-element-provider"));
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.when(element.isDisplayed()).thenReturn(true);
        Mockito.when(element.isEnabled()).thenReturn(true);
        Mockito.when(element.getAccessibleName()).thenReturn("gesture target");
        Mockito.when(element.getRect()).thenReturn(new Rectangle(1, 2, 30, 40));
        By locator = By.id("gesture-target");
        Mockito.when(driver.findElements(locator)).thenReturn(List.of(element));
        IllegalStateException providerFailure = new IllegalStateException("native element click failed");
        Mockito.doThrow(providerFailure).when(element).click();
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        GestureActions gestures = new GestureActions(mobile, new TouchActions(driver));

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> gestures.tap().on(locator));

        Assert.assertSame(thrown, providerFailure);
    }

    @Test
    public void nestedRetainedIntoViewBoundaryShouldExposeTheOriginalProviderFailure() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("gesture-real-nested-provider"));
        IllegalStateException providerFailure = new IllegalStateException("native scroll failed");
        Mockito.doThrow(providerFailure).when(driver).executeScript(
                Mockito.eq("flutter: scrollTillVisible"), Mockito.any(Object[].class));
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        GestureActions gestures = new GestureActions(mobile, new TouchActions(driver));

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> gestures.swipe()
                .intoView(AppiumBy.flutterSemanticsLabel("target"), MobileSwipeDirection.UP));

        Assert.assertSame(thrown, providerFailure);
    }

    @Test
    public void providerFailureReusedByEvidenceProbeShouldRemainTheOriginalFailure() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("gesture-reused-provider-failure"));
        IllegalStateException providerFailure = new IllegalStateException("reused native failure");
        By target = AppiumBy.flutterSemanticsLabel("target");
        Mockito.doThrow(providerFailure).when(driver).executeScript(
                Mockito.eq("flutter: scrollTillVisible"), Mockito.any(Object[].class));
        Mockito.when(driver.findElements(target)).thenThrow(providerFailure);
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        GestureActions gestures = new GestureActions(mobile, new TouchActions(driver));

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> gestures.swipe()
                .intoView(target, MobileSwipeDirection.UP));

        Assert.assertSame(thrown, providerFailure);
    }

    @Test
    public void genericDragShouldUseTheCrossDriverElementAction() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("gesture-real-drag"));
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        GestureActions gestures = new GestureActions(mobile, new TouchActions(driver));
        By source = By.id("source");
        By destination = By.id("destination");

        try (MockedConstruction<com.shaft.gui.element.internal.Actions> constructed =
                     Mockito.mockConstruction(com.shaft.gui.element.internal.Actions.class,
                             (mock, context) -> Mockito.when(mock.dragAndDrop(
                                             Mockito.any(By.class), Mockito.any(By.class)))
                                     .thenReturn(mock))) {
            gestures.drag().fromTo(source, destination);

            Assert.assertEquals(constructed.constructed().size(), 1);
            Mockito.verify(constructed.constructed().getFirst()).dragAndDrop(source, destination);
        }
    }

}
