package com.shaft.gui.driver;

import org.openqa.selenium.By;

/** Drag gestures. */
public interface MobileDragActionsContract {
    MobileDragActionsContract fromTo(By source, By destination);
    MobileGestureActionsContract and();
}
