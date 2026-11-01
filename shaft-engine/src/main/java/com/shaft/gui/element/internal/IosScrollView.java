package com.shaft.gui.element.internal;

import com.shaft.gui.element.TouchActions;
import org.openqa.selenium.By;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Helpers for iOS SwiftUI / XCUITest swipe-into-view (#7680).
 *
 * <p>{@code mobile: scroll} often no-ops against SwiftUI lists while short toolbar
 * {@code XCUIElementTypeScrollView} nodes sit above the real list. Prefer the tallest
 * on-screen scroll view and drag with {@code mobile: dragFromToForDuration}.
 */
public final class IosScrollView {
    public static final By SCROLL_VIEW = By.className("XCUIElementTypeScrollView");
    public static final double DRAG_DURATION_SECONDS = 0.35;

    private IosScrollView() {
    }

    /**
     * Pick the tallest displayed scroll view that intersects the screen.
     * Short header/toolbar scroll views lose to the main list.
     */
    public static WebElement pickTallest(List<WebElement> candidates, int screenWidth, int screenHeight) {
        if (candidates == null || candidates.isEmpty()) {
            return null;
        }
        int safeWidth = Math.max(screenWidth, 1);
        int safeHeight = Math.max(screenHeight, 1);
        List<Scored> scored = new ArrayList<>();
        for (WebElement candidate : candidates) {
            Scored score = score(candidate, safeWidth, safeHeight);
            if (score != null) {
                scored.add(score);
            }
        }
        if (scored.isEmpty()) {
            return null;
        }
        scored.sort(Comparator.comparingInt((Scored s) -> s.height).reversed()
                .thenComparing(Comparator.comparingInt((Scored s) -> s.area).reversed()));
        return scored.getFirst().element;
    }

    /**
     * Build {@code mobile: dragFromToForDuration} parameters for one swipe step.
     *
     * <p>The drag box starts 15% below the scroll view top (and 15% in from the left for
     * horizontal swipes) so the gesture stays inside the list, not the chrome.
     * {@link TouchActions.SwipeDirection#DOWN} reveals lower rows (finger moves up);
     * {@link TouchActions.SwipeDirection#UP} reveals upper rows (finger moves down).
     */
    public static Map<String, Object> dragParameters(TouchActions.SwipeDirection direction, Rectangle rect) {
        if (direction == null) {
            throw new IllegalArgumentException("direction is required");
        }
        if (rect == null) {
            throw new IllegalArgumentException("rect is required");
        }
        int left = rect.getX();
        int top = rect.getY();
        int width = Math.max(rect.getWidth(), 1);
        int height = Math.max(rect.getHeight(), 1);
        int insetTop = top + height * 15 / 100;
        int usableHeight = Math.max(height - height * 15 / 100, 1);
        int insetLeft = left + width * 15 / 100;
        int usableWidth = Math.max(width - width * 15 / 100, 1);
        int midX = left + Math.max(width / 2, 1);
        int midY = insetTop + Math.max(usableHeight / 2, 1);

        int fromX;
        int fromY;
        int toX;
        int toY;
        switch (direction) {
            case DOWN -> {
                fromX = midX;
                toX = midX;
                fromY = insetTop + usableHeight * 80 / 100;
                toY = insetTop + usableHeight * 20 / 100;
            }
            case UP -> {
                fromX = midX;
                toX = midX;
                fromY = insetTop + usableHeight * 20 / 100;
                toY = insetTop + usableHeight * 80 / 100;
            }
            case RIGHT -> {
                fromY = midY;
                toY = midY;
                fromX = insetLeft + usableWidth * 80 / 100;
                toX = insetLeft + usableWidth * 20 / 100;
            }
            case LEFT -> {
                fromY = midY;
                toY = midY;
                fromX = insetLeft + usableWidth * 20 / 100;
                toX = insetLeft + usableWidth * 80 / 100;
            }
            default -> throw new IllegalArgumentException("Unsupported swipe direction: " + direction);
        }

        Map<String, Object> drag = new LinkedHashMap<>();
        drag.put("duration", DRAG_DURATION_SECONDS);
        drag.put("fromX", fromX);
        drag.put("fromY", fromY);
        drag.put("toX", toX);
        drag.put("toY", toY);
        return drag;
    }

    private static Scored score(WebElement element, int screenWidth, int screenHeight) {
        boolean displayed;
        Rectangle rect;
        try {
            displayed = element.isDisplayed();
        } catch (RuntimeException ignored) {
            return null;
        }
        if (!displayed) {
            return null;
        }
        try {
            rect = element.getRect();
        } catch (RuntimeException ignored) {
            return null;
        }
        int width = rect.getWidth();
        int height = rect.getHeight();
        if (width <= 0 || height <= 0) {
            return null;
        }
        int left = Math.max(rect.getX(), 0);
        int top = Math.max(rect.getY(), 0);
        int right = Math.min(rect.getX() + width, screenWidth);
        int bottom = Math.min(rect.getY() + height, screenHeight);
        int visibleHeight = Math.max(0, bottom - top);
        int visibleWidth = Math.max(0, right - left);
        int area = visibleHeight * visibleWidth;
        if (area <= 0 || visibleHeight <= 0) {
            return null;
        }
        return new Scored(element, visibleHeight, area);
    }

    private record Scored(WebElement element, int height, int area) {
    }
}
