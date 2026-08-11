package com.shaft.gui.driver;

import org.openqa.selenium.By;

import java.time.Duration;

/** Swipe and scroll gestures. */
public interface MobileSwipeActionsContract {
    MobileSwipeActionsContract fromTo(By source, By destination);
    MobileSwipeActionsContract byOffset(By locator, int xOffset, int yOffset);
    MobileSwipeActionsContract fromTo(int startX, int startY, int endX, int endY, Duration duration);
    MobileSwipeActionsContract intoView(By locator, MobileSwipeDirection direction);
    MobileSwipeActionsContract toEnd(MobileSwipeDirection direction);
    MobileGestureActionsContract and();
}
