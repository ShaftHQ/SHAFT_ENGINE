package com.shaft.gui.element.internal;

import com.shaft.gui.element.TouchActions;

/**
 * Screen clamping and the swipe-into-view stop rule.
 *
 * <p>A gesture that returns false is the end of the scrollable only after an earlier gesture in the
 * same search actually moved it. A false result before any movement means the caller's direction
 * did not move the scrollable, so swipe-into-view tries the opposite direction on that axis.
 * {@code swipeToEndOfView} does not use this rule; it stops on the first false in the requested
 * direction.
 */
public final class ScrollMotion {
    private ScrollMotion() {
    }

    public enum Stop {
        /** Stay on the current direction and keep searching. */
        CONTINUE,
        /** The gesture did not move, and the opposite direction has not been tried. */
        TRY_OPPOSITE,
        /** A later gesture returned false after an earlier one moved the scrollable. */
        END_AFTER_MOVEMENT,
        /** Both directions on this axis returned false without ever moving. */
        NO_MOVEMENT
    }

    public record Box(int left, int top, int width, int height) {
        public boolean entirelyOnScreen(int screenWidth, int screenHeight) {
            return left >= 0 && top >= 0 && width > 0 && height > 0
                    && left + (long) width <= screenWidth
                    && top + (long) height <= screenHeight;
        }
    }

    /**
     * Null-container Android {@code mobile: scrollGesture} box.
     *
     * <p>The box is built already on the screen. UP and LEFT used to start 100px from the far edge
     * and then let {@link #clampToScreen} cut them down to a thin strip, so the opposite-direction
     * search reported that the scrollable did not move. DOWN and RIGHT keep a 100px margin and a
     * span of about nine tenths of the screen. LEFT uses that same span, placed so the far
     * edge still clears the margin. UP is the documented exception: see {@code historicalUpBox}.
     */
    public static Box nullContainerBox(TouchActions.SwipeDirection direction, int screenWidth, int screenHeight) {
        int safeWidth = Math.max(screenWidth, 1);
        int safeHeight = Math.max(screenHeight, 1);
        return direction == TouchActions.SwipeDirection.UP
                ? historicalUpBox(safeWidth, safeHeight)
                : clampedBox(direction, safeWidth, safeHeight);
    }

    // Documented exception (#6931): UP keeps the historical rectangle (top = H - 100,
    // height = 0.9H) without clamping. UiAutomator2 resolves it against the scrollable.
    // Both clamped variants (a 100px strip, or a 0.9H box near the top) produce a
    // scrollGesture that does not move ApiDemos lists on BrowserStack.
    private static Box historicalUpBox(int safeWidth, int safeHeight) {
        return new Box(0, Math.max(0, safeHeight - 100), safeWidth, safeHeight * 90 / 100);
    }

    private static Box clampedBox(TouchActions.SwipeDirection direction, int safeWidth, int safeHeight) {
        boolean vertical = direction == TouchActions.SwipeDirection.DOWN;
        int axis = vertical ? safeHeight : safeWidth;
        int span = motionSpan(axis);
        int margin = edgeMargin(axis, span);
        int width = vertical ? safeWidth : span;
        int height = vertical ? span : Math.min(safeHeight, Math.max(span, safeHeight * 90 / 100));
        return clampToScreen(clampedLeft(direction, margin, width, safeWidth),
                vertical ? Math.min(margin, Math.max(0, safeHeight - height)) : 0,
                width, Math.min(height, safeHeight), safeWidth, safeHeight);
    }

    private static int clampedLeft(TouchActions.SwipeDirection direction, int margin, int width, int safeWidth) {
        return switch (direction) {
            case LEFT -> Math.max(0, safeWidth - margin - width);
            case RIGHT -> Math.min(margin, Math.max(0, safeWidth - width));
            case UP, DOWN -> 0;
        };
    }

    private static int motionSpan(int axis) {
        int span = axis * 90 / 100;
        return span < 1 ? axis : span;
    }

    private static int edgeMargin(int axis, int span) {
        int margin = Math.min(100, Math.max(0, axis / 10));
        if (margin + span > axis) {
            margin = Math.max(0, axis - span);
        }
        return margin;
    }

    public static Box clampToScreen(int left, int top, int width, int height, int screenWidth, int screenHeight) {
        int maxWidth = Math.max(screenWidth, 1);
        int maxHeight = Math.max(screenHeight, 1);
        int boxLeft = clampStart(left, maxWidth);
        int boxTop = clampStart(top, maxHeight);
        return new Box(
                boxLeft,
                boxTop,
                clampExtent(boxLeft, width, maxWidth),
                clampExtent(boxTop, height, maxHeight));
    }

    private static int clampStart(int start, int limit) {
        if (start < 0) {
            return 0;
        }
        if (start > limit - 1) {
            return limit - 1;
        }
        return start;
    }

    private static int clampExtent(int start, int extent, int limit) {
        int fitted = extent > limit - start ? limit - start : extent;
        return fitted < 1 ? 1 : fitted;
    }

    public static TouchActions.SwipeDirection opposite(TouchActions.SwipeDirection direction) {
        return switch (direction) {
            case UP -> TouchActions.SwipeDirection.DOWN;
            case DOWN -> TouchActions.SwipeDirection.UP;
            case LEFT -> TouchActions.SwipeDirection.RIGHT;
            case RIGHT -> TouchActions.SwipeDirection.LEFT;
        };
    }

    /**
     * @param gestureMoved       this gesture returned true (the scrollable moved)
     * @param movedEarlier       some earlier gesture in this search returned true
     * @param oppositeAlreadyTried the other direction on this axis was already issued
     */
    public static Stop classify(boolean gestureMoved, boolean movedEarlier, boolean oppositeAlreadyTried) {
        if (gestureMoved) {
            return Stop.CONTINUE;
        }
        if (movedEarlier) {
            return Stop.END_AFTER_MOVEMENT;
        }
        if (!oppositeAlreadyTried) {
            return Stop.TRY_OPPOSITE;
        }
        return Stop.NO_MOVEMENT;
    }

    /** What the W3C swipe-into-view loop does after one gesture that did not reveal the target. */
    public enum Step {
        /** Keep swiping in the current direction. */
        CONTINUE,
        /** Swipe the other way: either the first gesture did not move, or one end was reached. */
        FLIP,
        /** Both ends were searched. */
        FAIL_END,
        /** Neither direction moved the scrollable. */
        FAIL_NO_MOVEMENT
    }

    /**
     * Next step of swipe-into-view (#6931). A gesture that does not move tries the opposite
     * direction once (the 0ffb2b1d fix). An end reached after movement also searches the rest
     * of the range once before failing (the scrollInExpandableLists regression).
     *
     * @param gestureMoved the last gesture reported it can scroll further
     * @param movedEarlier an earlier gesture in the current direction moved
     * @param oppositeTried the direction was already flipped once
     */
    public static Step step(boolean gestureMoved, boolean movedEarlier, boolean oppositeTried) {
        return switch (classify(gestureMoved, movedEarlier, oppositeTried)) {
            case CONTINUE -> Step.CONTINUE;
            case TRY_OPPOSITE -> Step.FLIP;
            case END_AFTER_MOVEMENT -> oppositeTried ? Step.FAIL_END : Step.FLIP;
            case NO_MOVEMENT -> Step.FAIL_NO_MOVEMENT;
        };
    }
}
