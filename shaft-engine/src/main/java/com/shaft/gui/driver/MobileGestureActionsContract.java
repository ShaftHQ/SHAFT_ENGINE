package com.shaft.gui.driver;

/** Concise mobile gesture actions. */
public interface MobileGestureActionsContract {
    /** Returns tap and press gestures. */
    MobileTapActionsContract tap();

    /** Returns swipe and scroll gestures. */
    MobileSwipeActionsContract swipe();

    /** Returns drag gestures. */
    MobileDragActionsContract drag();

    /** Returns pinch zoom gestures. */
    MobileZoomActionsContract zoom();

    /** Returns the owning mobile namespace. */
    MobileActionsContract and();
}
