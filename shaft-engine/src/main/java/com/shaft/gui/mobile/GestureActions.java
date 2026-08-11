package com.shaft.gui.mobile;

import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileDragActionsContract;
import com.shaft.gui.driver.MobileGestureActionsContract;
import com.shaft.gui.driver.MobileSwipeActionsContract;
import com.shaft.gui.driver.MobileSwipeDirection;
import com.shaft.gui.driver.MobileTapActionsContract;
import com.shaft.gui.driver.MobileZoomActionsContract;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.tools.io.internal.TraceEventRecorder;
import org.openqa.selenium.By;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/** Categorized mobile gesture facade. */
final class GestureActions implements MobileGestureActionsContract {
    private final MobileActions mobile;
    private final TouchActions touch;

    GestureActions(MobileActions mobile) {
        this(mobile, mobile.touchActions());
    }

    GestureActions(MobileActions mobile, TouchActions touch) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
        this.touch = Objects.requireNonNull(touch, "touch");
    }

    @Override public MobileTapActionsContract tap() { return new Tap(this); }
    @Override public MobileSwipeActionsContract swipe() { return new Swipe(this); }
    @Override public MobileDragActionsContract drag() { return new Drag(this); }
    @Override public MobileZoomActionsContract zoom() { return new Zoom(this); }
    @Override public MobileActionsContract and() { return mobile; }

    private void perform(String operation, String locator, Runnable action) {
        var event = TraceEventRecorder.start("mobile/gesture", operation, locator, mobile.traceDriver());
        try {
            mobile.driver();
            TraceEventRecorder.withoutNestedEvents(() -> {
                action.run();
                return null;
            });
            TraceEventRecorder.finish(event, "passed", "Mobile gesture completed.", null, Map.of(), List.of());
        } catch (RuntimeException exception) {
            RuntimeException providerFailure = providerFailure(exception);
            TraceEventRecorder.finish(event, "failed", "Mobile gesture failed.", providerFailure, Map.of(), List.of());
            throw providerFailure;
        }
    }

    private static RuntimeException providerFailure(RuntimeException exception) {
        RuntimeException current = exception;
        var visited = java.util.Collections.newSetFromMap(
                new java.util.IdentityHashMap<RuntimeException, Boolean>());
        while (current instanceof Actions.ActionExecutionException legacyFailure && visited.add(current)
                && legacyFailure.originalFailure() instanceof RuntimeException cause) {
            current = cause;
        }
        return current;
    }

    private static By locator(By locator) {
        return Objects.requireNonNull(locator, "gesture locator");
    }

    private static int durationMillis(Duration duration) {
        Objects.requireNonNull(duration, "gesture duration");
        if (duration.isNegative()) {
            throw new IllegalArgumentException("The gesture duration must not be negative.");
        }
        long millis = duration.toMillis();
        if (millis > Integer.MAX_VALUE) {
            throw new IllegalArgumentException("The gesture duration is too large.");
        }
        return (int) millis;
    }

    private static TouchActions.SwipeDirection direction(MobileSwipeDirection direction) {
        return TouchActions.SwipeDirection.valueOf(Objects.requireNonNull(direction, "swipe direction").name());
    }

    private record Tap(GestureActions owner) implements MobileTapActionsContract {
        @Override public MobileTapActionsContract on(By locator) {
            owner.perform("tap", String.valueOf(locator), () -> owner.touch.tap(locator(locator))); return this;
        }
        @Override public MobileTapActionsContract at(int x, int y) {
            owner.perform("tap-at", "x=" + x + ",y=" + y, () -> owner.touch.tapByCoordinates(x, y)); return this;
        }
        @Override public MobileTapActionsContract doubleOn(By locator) {
            owner.perform("double-tap", String.valueOf(locator), () -> owner.touch.doubleTap(locator(locator))); return this;
        }
        @Override public MobileTapActionsContract longPress(By locator) {
            owner.perform("long-press", String.valueOf(locator), () -> owner.touch.longTap(locator(locator))); return this;
        }
        @Override public MobileGestureActionsContract and() { return owner; }
    }

    private record Swipe(GestureActions owner) implements MobileSwipeActionsContract {
        @Override public MobileSwipeActionsContract fromTo(By source, By destination) {
            owner.perform("swipe-from-to", source + " -> " + destination,
                    () -> owner.touch.swipeToElement(locator(source), locator(destination))); return this;
        }
        @Override public MobileSwipeActionsContract byOffset(By locator, int xOffset, int yOffset) {
            owner.perform("swipe-by-offset", locator + ",x=" + xOffset + ",y=" + yOffset,
                    () -> owner.touch.swipeByOffset(locator(locator), xOffset, yOffset)); return this;
        }
        @Override public MobileSwipeActionsContract fromTo(int startX, int startY, int endX, int endY, Duration duration) {
            owner.perform("swipe-coordinates", "(" + startX + "," + startY + ")->(" + endX + "," + endY + ")",
                    () -> owner.touch.swipeByCoordinates(startX, startY, endX, endY, durationMillis(duration))); return this;
        }
        @Override public MobileSwipeActionsContract intoView(By locator, MobileSwipeDirection direction) {
            owner.perform("swipe-into-view", String.valueOf(locator),
                    () -> owner.touch.swipeElementIntoView(locator(locator), direction(direction))); return this;
        }
        @Override public MobileSwipeActionsContract toEnd(MobileSwipeDirection direction) {
            owner.perform("swipe-to-end", String.valueOf(direction),
                    () -> owner.touch.swipeToEndOfView(direction(direction))); return this;
        }
        @Override public MobileGestureActionsContract and() { return owner; }
    }

    private record Drag(GestureActions owner) implements MobileDragActionsContract {
        @Override public MobileDragActionsContract fromTo(By source, By destination) {
            owner.perform("drag-from-to", source + " -> " + destination,
                    () -> owner.touch.swipeToElement(locator(source), locator(destination))); return this;
        }
        @Override public MobileGestureActionsContract and() { return owner; }
    }

    private record Zoom(GestureActions owner) implements MobileZoomActionsContract {
        @Override public MobileZoomActionsContract in() {
            owner.perform("zoom-in", "", () -> owner.touch.pinchToZoom(TouchActions.ZoomDirection.IN)); return this;
        }
        @Override public MobileZoomActionsContract out() {
            owner.perform("zoom-out", "", () -> owner.touch.pinchToZoom(TouchActions.ZoomDirection.OUT)); return this;
        }
        @Override public MobileGestureActionsContract and() { return owner; }
    }
}
