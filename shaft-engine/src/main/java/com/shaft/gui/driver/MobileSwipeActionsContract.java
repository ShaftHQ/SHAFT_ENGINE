package com.shaft.gui.driver;

import org.openqa.selenium.By;

import java.time.Duration;

/** Swipe and scroll gestures. */
public interface MobileSwipeActionsContract {
    /**
     * Swipes from the source element to the destination element.
     */
    MobileSwipeActionsContract fromTo(By source, By destination);
    /**
     * Swipes the located element by the given pixel offset.
     */
    MobileSwipeActionsContract byOffset(By locator, int xOffset, int yOffset);
    /**
     * Swipes between two screen points over the given duration.
     */
    MobileSwipeActionsContract fromTo(int startX, int startY, int endX, int endY, Duration duration);
    /**
     * Swipes in the given direction until the located element is in view.
     */
    MobileSwipeActionsContract intoView(By locator, MobileSwipeDirection direction);
    /**
     * Swipes in the given direction until the end of the scrollable content.
     */
    MobileSwipeActionsContract toEnd(MobileSwipeDirection direction);
    /**
     * Returns the gesture actions to chain the next gesture.
     */
    MobileGestureActionsContract and();
}
