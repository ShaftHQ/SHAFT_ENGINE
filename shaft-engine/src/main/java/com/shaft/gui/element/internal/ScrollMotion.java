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
     * Null-container Android {@code mobile: scrollGesture} box. The historical margins are kept,
     * then the rectangle is clamped so it lies entirely on the screen.
     */
    public static Box nullContainerBox(TouchActions.SwipeDirection direction, int screenWidth, int screenHeight) {
        int width = screenWidth;
        int height = screenHeight * 90 / 100;
        int left = 0;
        int top = 0;
        switch (direction) {
            case UP -> top = screenHeight - 100;
            case DOWN -> top = 100;
            case RIGHT -> left = 100;
            case LEFT -> left = screenWidth - 100;
        }
        return clampToScreen(left, top, width, height, screenWidth, screenHeight);
    }

    public static Box clampToScreen(int left, int top, int width, int height, int screenWidth, int screenHeight) {
        int maxWidth = Math.max(screenWidth, 1);
        int maxHeight = Math.max(screenHeight, 1);
        int boxLeft = left;
        int boxTop = top;
        int boxWidth = width;
        int boxHeight = height;
        if (boxLeft < 0) {
            boxLeft = 0;
        }
        if (boxTop < 0) {
            boxTop = 0;
        }
        if (boxLeft > maxWidth - 1) {
            boxLeft = maxWidth - 1;
        }
        if (boxTop > maxHeight - 1) {
            boxTop = maxHeight - 1;
        }
        if (boxWidth > maxWidth - boxLeft) {
            boxWidth = maxWidth - boxLeft;
        }
        if (boxHeight > maxHeight - boxTop) {
            boxHeight = maxHeight - boxTop;
        }
        if (boxWidth < 1) {
            boxWidth = 1;
        }
        if (boxHeight < 1) {
            boxHeight = 1;
        }
        return new Box(boxLeft, boxTop, boxWidth, boxHeight);
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
}
