package com.shaft.gui.driver;

import org.openqa.selenium.By;

/** Drag gestures. */
public interface MobileDragActionsContract {
    /**
     * Drags from the source element to the destination element.
     */
    MobileDragActionsContract fromTo(By source, By destination);
    /**
     * Returns the gesture actions to chain the next gesture.
     */
    MobileGestureActionsContract and();
}
