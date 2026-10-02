package com.shaft.gui.driver;

import org.openqa.selenium.By;

/** Tap and press gestures. */
public interface MobileTapActionsContract {
    /**
     * Taps the located element.
     */
    MobileTapActionsContract on(By locator);
    /**
     * Taps the given screen point.
     */
    MobileTapActionsContract at(int x, int y);
    /**
     * Double-taps the located element.
     */
    MobileTapActionsContract doubleOn(By locator);
    /**
     * Long-presses the located element.
     */
    MobileTapActionsContract longPress(By locator);
    /**
     * Returns the gesture actions to chain the next gesture.
     */
    MobileGestureActionsContract and();
}
