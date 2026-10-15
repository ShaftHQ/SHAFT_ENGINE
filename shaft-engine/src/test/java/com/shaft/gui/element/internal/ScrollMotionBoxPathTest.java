package com.shaft.gui.element.internal;

import com.shaft.gui.element.TouchActions;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
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
        for (TouchActions.SwipeDirection direction : new TouchActions.SwipeDirection[]{
                TouchActions.SwipeDirection.DOWN, TouchActions.SwipeDirection.LEFT, TouchActions.SwipeDirection.RIGHT}) {
            assertOnScrollable(direction, ScrollMotion.nullContainerBox(direction, WIDTH, HEIGHT));
        }
    }

    @Test
    public void upKeepsTheUnclampedHistoricalRectangleProvenOnBrowserStack() {
        ScrollMotion.Box up = ScrollMotion.nullContainerBox(TouchActions.SwipeDirection.UP, WIDTH, HEIGHT);
        assertEquals(up, new ScrollMotion.Box(0, HEIGHT - 100, WIDTH, HEIGHT * 90 / 100));
        assertNotEquals(up.height(), 100, "UP must not be clamped to a thin strip");
    }

    @Test
    public void noOpFirstGestureStillTriesTheOppositeDirection() {
        // 0ffb2b1d scenario: the user chose the wrong direction and nothing moved.
        assertEquals(ScrollMotion.step(false, false, false), ScrollMotion.Step.FLIP);
        assertEquals(ScrollMotion.step(false, false, true), ScrollMotion.Step.FAIL_NO_MOVEMENT);
    }

    @Test
    public void endReachedAfterMovementSearchesTheRestOfTheRangeOnce() {
        // scrollInExpandableLists regression: movement, then the end, without finding the target.
        assertEquals(ScrollMotion.step(true, false, false), ScrollMotion.Step.CONTINUE);
        assertEquals(ScrollMotion.step(false, true, false), ScrollMotion.Step.FLIP);
        assertEquals(ScrollMotion.step(false, true, true), ScrollMotion.Step.FAIL_END);
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
