package com.shaft.gui.element.internal;

import com.shaft.gui.element.TouchActions;
import org.testng.annotations.Test;

import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertTrue;

/**
 * Opposite-direction swipe-into-view uses a box on the scrollable that is large enough to move it.
 */
public class ScrollMotionBoxPathTest {
    private static final int WIDTH = 1080;
    private static final int HEIGHT = 2400;

    @Test
    public void oppositeDirectionBoxIsOnTheScrollableAndTallEnoughToMoveIt() {
        ScrollMotion.Box up = ScrollMotion.nullContainerBox(TouchActions.SwipeDirection.UP, WIDTH, HEIGHT);
        assertNotEquals(up.top(), 2300);
        assertNotEquals(up.height(), 100);
        assertOnScrollable(TouchActions.SwipeDirection.UP, up);

        for (TouchActions.SwipeDirection direction : TouchActions.SwipeDirection.values()) {
            assertOnScrollable(direction, ScrollMotion.nullContainerBox(direction, WIDTH, HEIGHT));
        }
    }

    private static void assertOnScrollable(TouchActions.SwipeDirection direction, ScrollMotion.Box box) {
        assertTrue(box.entirelyOnScreen(WIDTH, HEIGHT), direction + " " + box);
        boolean vertical = direction == TouchActions.SwipeDirection.UP
                || direction == TouchActions.SwipeDirection.DOWN;
        if (vertical) {
            assertTrue(box.height() >= HEIGHT / 2, direction + " " + box);
        } else {
            assertTrue(box.width() >= WIDTH / 2, direction + " " + box);
        }
    }
}
