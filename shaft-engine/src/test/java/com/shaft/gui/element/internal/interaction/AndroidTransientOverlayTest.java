package com.shaft.gui.element.internal.interaction;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

public class AndroidTransientOverlayTest {

    @Test
    public void crashCloseIsDismissedAndTheReadyCheckRetriesWithinTheBound() {
        Assert.assertEquals(AndroidTransientOverlay.APP_CRASH_CLOSE.toString(), "By.id: android:id/aerr_close");
        AtomicInteger clicks = new AtomicInteger();
        AtomicInteger looks = new AtomicInteger();
        WebElement button = new org.openqa.selenium.remote.RemoteWebElement() {
            @Override
            public void click() {
                clicks.incrementAndGet();
            }
        };
        Optional<String> found = AndroidTransientOverlay.recheck(() -> {
            int seen = looks.incrementAndGet();
            return seen >= 3 ? Optional.of("ready") : Optional.empty();
        }, () -> List.of(button));
        Assert.assertEquals(found.orElseThrow(), "ready");
        Assert.assertEquals(clicks.get(), 2);
        Assert.assertTrue(clicks.get() <= AndroidTransientOverlay.MAX_DISMISSALS);
    }

    @Test
    public void dismissalStopsWhenTheCrashButtonIsGone() {
        AtomicInteger clicks = new AtomicInteger();
        WebElement button = new org.openqa.selenium.remote.RemoteWebElement() {
            @Override
            public void click() {
                if (clicks.incrementAndGet() == 1) {
                    throw new org.openqa.selenium.NoSuchElementException("vanished");
                }
            }
        };
        AtomicInteger finds = new AtomicInteger();
        Optional<String> found = AndroidTransientOverlay.recheck(
                Optional::empty,
                () -> finds.incrementAndGet() == 1 ? List.of(button) : List.of());
        Assert.assertTrue(found.isEmpty());
        Assert.assertEquals(clicks.get(), 1);
        Assert.assertEquals(finds.get(), 2);
    }

    @Test
    public void dismissalNeverExceedsTheBound() {
        AtomicInteger clicks = new AtomicInteger();
        WebElement button = new org.openqa.selenium.remote.RemoteWebElement() {
            @Override
            public void click() {
                clicks.incrementAndGet();
            }
        };
        Optional<String> found = AndroidTransientOverlay.recheck(Optional::empty, () -> List.of(button));
        Assert.assertTrue(found.isEmpty());
        Assert.assertEquals(clicks.get(), AndroidTransientOverlay.MAX_DISMISSALS);
    }

    @Test
    public void aNonAppiumDriverAndAFailedLookupHaveNoDialog() {
        WebDriver desktop = mock(WebDriver.class);
        Assert.assertEquals(AndroidTransientOverlay.dismissOnce(desktop), 0);
        Assert.assertEquals(AndroidTransientOverlay.dismissOnce(() -> {
            throw new IllegalStateException("down");
        }), 0);
        Assert.assertEquals(AndroidTransientOverlay.dismissOnce(() -> null), 0);
    }

    @Test
    public void anAppiumSessionClicksTheCrashButtonAndSurvivesAFailedLookup() {
        AppiumDriver driver = mock(AppiumDriver.class);
        WebElement button = mock(WebElement.class);
        when(driver.findElements(AndroidTransientOverlay.APP_CRASH_CLOSE)).thenReturn(List.of(button));
        Assert.assertEquals(AndroidTransientOverlay.dismissOnce(driver), 1);
        verify(button).click();
        when(driver.findElements(AndroidTransientOverlay.APP_CRASH_CLOSE))
                .thenThrow(new NoSuchElementException("gone"));
        Assert.assertEquals(AndroidTransientOverlay.dismissOnce(driver), 0);
    }
}
