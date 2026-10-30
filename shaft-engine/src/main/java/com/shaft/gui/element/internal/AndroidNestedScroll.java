package com.shaft.gui.element.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

/**
 * Helpers for Android Compose / nested-scroll swipe-into-view (#7623).
 *
 * <p>Compose controls often stay in the accessibility tree with {@code displayed=false}
 * and inverted or empty clip bounds until a real nested scrollable moves. Null-container
 * {@code mobile: scrollGesture} can report no movement while {@code scrollable=true}
 * (e.g. {@code nestedScrollView}) would move under an {@code elementId}-bound gesture.
 */
public final class AndroidNestedScroll {
    /** UiAutomator2 nodes that advertise they can scroll. */
    public static final By SCROLLABLE = By.xpath("//*[@scrollable='true']");

    private AndroidNestedScroll() {
    }

    /**
     * Snapshot of a swipe target that is already in the tree but may not be in the viewport.
     */
    public record TargetPresence(boolean displayed, int x, int y, int width, int height) {
        public boolean invertedOrEmpty() {
            return width <= 0 || height <= 0;
        }

        public boolean clippedOutOfView() {
            return !displayed || invertedOrEmpty();
        }

        /**
         * Short diagnosis for failure messages, or {@code null} when the target looks fine.
         */
        public String diagnosis() {
            if (!clippedOutOfView()) {
                return null;
            }
            return "target has inverted / non-displayed bounds (displayed=" + displayed
                    + ", x=" + x + ", y=" + y + ", w=" + width + ", h=" + height + ")";
        }
    }

    public static TargetPresence inspect(WebElement element) {
        boolean displayed;
        Rectangle rect;
        try {
            displayed = element.isDisplayed();
        } catch (RuntimeException ignored) {
            displayed = false;
        }
        try {
            rect = element.getRect();
        } catch (RuntimeException ignored) {
            return new TargetPresence(displayed, 0, 0, 0, 0);
        }
        return new TargetPresence(displayed, rect.getX(), rect.getY(), rect.getWidth(), rect.getHeight());
    }

    /**
     * Prefer a displayed nested-scroll resource-id, else the largest positive-area displayed
     * scrollable that intersects the screen.
     */
    public static WebElement pickScrollable(List<WebElement> candidates, int screenWidth, int screenHeight) {
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
        scored.sort(Comparator
                .comparingInt((Scored s) -> s.nestedScroll ? 0 : 1)
                .thenComparing(Comparator.comparingInt((Scored s) -> s.area).reversed()));
        return scored.getFirst().element;
    }

    public static String resourceId(WebElement element) {
        try {
            String dom = element.getDomAttribute("resource-id");
            if (dom != null && !dom.isBlank()) {
                return dom;
            }
        } catch (RuntimeException ignored) {
            // fall through to getAttribute
        }
        try {
            String attr = element.getAttribute("resource-id");
            return attr == null ? "" : attr;
        } catch (RuntimeException ignored) {
            return "";
        }
    }

    public static boolean looksLikeNestedScroll(String resourceId) {
        if (resourceId == null || resourceId.isBlank()) {
            return false;
        }
        String lower = resourceId.toLowerCase(Locale.ROOT);
        return lower.contains("nestedscroll");
    }

    private static Scored score(WebElement element, int screenWidth, int screenHeight) {
        TargetPresence presence = inspect(element);
        if (!presence.displayed() || presence.invertedOrEmpty()) {
            return null;
        }
        int left = Math.max(presence.x(), 0);
        int top = Math.max(presence.y(), 0);
        int right = Math.min(presence.x() + presence.width(), screenWidth);
        int bottom = Math.min(presence.y() + presence.height(), screenHeight);
        int area = Math.max(0, right - left) * Math.max(0, bottom - top);
        if (area <= 0) {
            return null;
        }
        return new Scored(element, area, looksLikeNestedScroll(resourceId(element)));
    }

    private record Scored(WebElement element, int area, boolean nestedScroll) {
    }
}
