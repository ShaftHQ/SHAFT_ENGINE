package com.shaft.gui.driver;

/** Pinch zoom gestures. */
public interface MobileZoomActionsContract {
    /**
     * Zooms in with a pinch-open gesture.
     */
    MobileZoomActionsContract in();
    /**
     * Zooms out with a pinch-close gesture.
     */
    MobileZoomActionsContract out();
    /**
     * Returns the gesture actions to chain the next gesture.
     */
    MobileGestureActionsContract and();
}
