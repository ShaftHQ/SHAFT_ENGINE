package com.shaft.gui.driver;

import org.openqa.selenium.By;

/** Tap and press gestures. */
public interface MobileTapActionsContract {
    MobileTapActionsContract on(By locator);
    MobileTapActionsContract at(int x, int y);
    MobileTapActionsContract doubleOn(By locator);
    MobileTapActionsContract longPress(By locator);
    MobileGestureActionsContract and();
}
