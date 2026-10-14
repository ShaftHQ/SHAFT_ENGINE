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
     * span of about nine tenths of the screen. UP and LEFT use that same span, placed so the far
     * edge still clears the margin.
     */
    public static Box nullContainerBox(TouchActions.SwipeDirection direction, int screenWidth, int screenHeight) {
        int safeWidth = Math.max(screenWidth, 1);
        int safeHeight = Math.max(screenHeight, 1);
        boolean vertical = direction == TouchActions.SwipeDirection.UP || direction == TouchActions.SwipeDirection.DOWN;
        int span = motionSpan(vertical ? safeHeight : safeWidth);
        int margin = edgeMargin(vertical ? safeHeight : safeWidth, span);
        int width = vertical ? safeWidth : span;
        int height = vertical ? span : Math.min(safeHeight, Math.max(span, safeHeight * 90 / 100));
        if (height > safeHeight) {
            height = safeHeight;
        }
        int top = switch (direction) {
            case UP -> Math.max(0, safeHeight - margin - height);
            case DOWN -> Math.min(margin, Math.max(0, safeHeight - height));
            case LEFT, RIGHT -> 0;
        };
        int left = switch (direction) {
            case LEFT -> Math.max(0, safeWidth - margin - width);
            case RIGHT -> Math.min(margin, Math.max(0, safeWidth - width));
            case UP, DOWN -> 0;
        };
        return clampToScreen(left, top, width, height, safeWidth, safeHeight);
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
}
