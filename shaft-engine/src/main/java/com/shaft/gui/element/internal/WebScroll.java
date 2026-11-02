package com.shaft.gui.element.internal;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.Map;

/**
 * Synchronous scroll-into-view for web drivers.
 *
 * <p>One check serves {@code scrollingMode} {@code w3c}, {@code javascript}, and {@code legacy},
 * locate-time scroll, and the non-Appium touch scroll. The scroll assigns {@code scrollLeft} and
 * {@code scrollTop} on the nested overflow ancestor (or the document) and returns only after that
 * assignment. It does not use {@code behavior: "smooth"}. Unchanged {@code scrollLeft} and
 * {@code scrollTop} are not movement. A target that is still outside the viewport is not treated
 * as scrolled into view. The document scrolling element is measured against the layout viewport
 * ({@code 0, 0, innerWidth, innerHeight}). A nested overflow ancestor keeps its own border box.
 */
public final class WebScroll {
    static final String METRICS_SCRIPT = """
            /* shaft-scroll-metrics */
            const element = arguments[0];
            const preferred = arguments[1];
            function canScroll(node) {
              if (!node || node.nodeType !== 1) return false;
              const style = window.getComputedStyle(node);
              const overflow = ((style.overflow || '') + (style.overflowY || '') + (style.overflowX || '')).toLowerCase();
              const scrolls = node.scrollHeight > node.clientHeight + 1 || node.scrollWidth > node.clientWidth + 1;
              return scrolls && (overflow.includes('auto') || overflow.includes('scroll') || overflow.includes('overlay'));
            }
            function ancestor(node) {
              if (canScroll(preferred)) return preferred;
              let current = node && node.parentElement;
              while (current) {
                if (canScroll(current)) return current;
                current = current.parentElement;
              }
              return document.scrollingElement || document.documentElement;
            }
            function layoutViewport() {
              return {left: 0, top: 0, right: window.innerWidth || 0, bottom: window.innerHeight || 0};
            }
            function viewOf(node) {
              const root = document.scrollingElement || document.documentElement;
              if (node === root || node === document.documentElement) {
                return layoutViewport();
              }
              return node.getBoundingClientRect();
            }
            const scroller = ancestor(element);
            const child = element.getBoundingClientRect();
            const view = viewOf(scroller);
            return {
              childLeft: child.left,
              childTop: child.top,
              childRight: child.right,
              childBottom: child.bottom,
              viewLeft: view.left,
              viewTop: view.top,
              viewRight: view.right,
              viewBottom: view.bottom,
              scrollLeft: scroller.scrollLeft || 0,
              scrollTop: scroller.scrollTop || 0,
              maxScrollLeft: Math.max(0, (scroller.scrollWidth || 0) - (scroller.clientWidth || 0)),
              maxScrollTop: Math.max(0, (scroller.scrollHeight || 0) - (scroller.clientHeight || 0))
            };
            """;

    static final String APPLY_SCRIPT = """
            /* shaft-scroll-apply */
            const element = arguments[0];
            const preferred = arguments[1];
            const nextLeft = arguments[2];
            const nextTop = arguments[3];
            function canScroll(node) {
              if (!node || node.nodeType !== 1) return false;
              const style = window.getComputedStyle(node);
              const overflow = ((style.overflow || '') + (style.overflowY || '') + (style.overflowX || '')).toLowerCase();
              const scrolls = node.scrollHeight > node.clientHeight + 1 || node.scrollWidth > node.clientWidth + 1;
              return scrolls && (overflow.includes('auto') || overflow.includes('scroll') || overflow.includes('overlay'));
            }
            function ancestor(node) {
              if (canScroll(preferred)) return preferred;
              let current = node && node.parentElement;
              while (current) {
                if (canScroll(current)) return current;
                current = current.parentElement;
              }
              return document.scrollingElement || document.documentElement;
            }
            const scroller = ancestor(element);
            scroller.scrollLeft = nextLeft;
            scroller.scrollTop = nextTop;
            return {scrollLeft: scroller.scrollLeft || 0, scrollTop: scroller.scrollTop || 0};
            """;

    private WebScroll() {
    }

    public record Metrics(double childLeft, double childTop, double childRight, double childBottom,
                          double viewLeft, double viewTop, double viewRight, double viewBottom,
                          double scrollLeft, double scrollTop, double maxScrollLeft, double maxScrollTop) {
    }

    public static boolean scrollIntoView(WebDriver driver, WebElement element) {
        return scrollIntoView(driver, element, null);
    }

    /**
     * @param scroller preferred overflow container, or null to walk ancestors from the target
     * @return true only when the target intersects the scrollport after the call
     */
    public static boolean scrollIntoView(WebDriver driver, WebElement element, WebElement scroller) {
        if (!(driver instanceof JavascriptExecutor executor) || element == null) {
            return false;
        }
        try {
            Metrics current = read(executor, element, scroller);
            if (current == null) {
                return false;
            }
            if (intersects(current)) {
                return true;
            }
            for (int step = 0; step < 4; step++) {
                double[] next = nextOffset(current);
                if (next == null) {
                    return false;
                }
                double beforeLeft = current.scrollLeft();
                double beforeTop = current.scrollTop();
                executor.executeScript(APPLY_SCRIPT, element, scroller, next[0], next[1]);
                Metrics after = read(executor, element, scroller);
                if (after == null || !offsetsMoved(beforeLeft, beforeTop, after.scrollLeft(), after.scrollTop())) {
                    return false;
                }
                if (intersects(after)) {
                    return true;
                }
                current = after;
            }
            return intersects(current);
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    public static boolean intersects(Metrics metrics) {
        double left = Math.max(metrics.childLeft(), metrics.viewLeft());
        double top = Math.max(metrics.childTop(), metrics.viewTop());
        double right = Math.min(metrics.childRight(), metrics.viewRight());
        double bottom = Math.min(metrics.childBottom(), metrics.viewBottom());
        return right > left && bottom > top;
    }

    /**
     * Next scroll offsets that move the child toward the scrollport, or null when the scroller
     * cannot move any closer.
     */
    public static double[] nextOffset(Metrics metrics) {
        if (intersects(metrics)) {
            return null;
        }
        double nextLeft = metrics.scrollLeft();
        double nextTop = metrics.scrollTop();
        if (metrics.childTop() >= metrics.viewBottom()) {
            nextTop = metrics.scrollTop() + (metrics.childTop() - metrics.viewTop());
        } else if (metrics.childBottom() <= metrics.viewTop()) {
            nextTop = metrics.scrollTop() - (metrics.viewBottom() - metrics.childBottom());
        }
        if (metrics.childLeft() >= metrics.viewRight()) {
            nextLeft = metrics.scrollLeft() + (metrics.childLeft() - metrics.viewLeft());
        } else if (metrics.childRight() <= metrics.viewLeft()) {
            nextLeft = metrics.scrollLeft() - (metrics.viewRight() - metrics.childRight());
        }
        nextLeft = clamp(nextLeft, 0, metrics.maxScrollLeft());
        nextTop = clamp(nextTop, 0, metrics.maxScrollTop());
        if (!offsetsMoved(metrics.scrollLeft(), metrics.scrollTop(), nextLeft, nextTop)) {
            return null;
        }
        return new double[]{nextLeft, nextTop};
    }

    public static boolean offsetsMoved(double beforeLeft, double beforeTop, double afterLeft, double afterTop) {
        return Math.abs(beforeLeft - afterLeft) > 0.5 || Math.abs(beforeTop - afterTop) > 0.5;
    }

    private static double clamp(double value, double min, double max) {
        double upper = Math.max(min, max);
        return Math.max(min, Math.min(upper, value));
    }

    private static Metrics read(JavascriptExecutor executor, WebElement element, WebElement scroller) {
        Object raw = executor.executeScript(METRICS_SCRIPT, element, scroller);
        if (!(raw instanceof Map<?, ?> map)) {
            return null;
        }
        Double childLeft = number(map.get("childLeft"));
        Double childTop = number(map.get("childTop"));
        Double childRight = number(map.get("childRight"));
        Double childBottom = number(map.get("childBottom"));
        Double viewLeft = number(map.get("viewLeft"));
        Double viewTop = number(map.get("viewTop"));
        Double viewRight = number(map.get("viewRight"));
        Double viewBottom = number(map.get("viewBottom"));
        Double scrollLeft = number(map.get("scrollLeft"));
        Double scrollTop = number(map.get("scrollTop"));
        Double maxScrollLeft = number(map.get("maxScrollLeft"));
        Double maxScrollTop = number(map.get("maxScrollTop"));
        if (childLeft == null || childTop == null || childRight == null || childBottom == null
                || viewLeft == null || viewTop == null || viewRight == null || viewBottom == null
                || scrollLeft == null || scrollTop == null || maxScrollLeft == null || maxScrollTop == null) {
            return null;
        }
        return new Metrics(childLeft, childTop, childRight, childBottom, viewLeft, viewTop, viewRight, viewBottom,
                scrollLeft, scrollTop, maxScrollLeft, maxScrollTop);
    }

    private static Double number(Object value) {
        if (value instanceof Number number) {
            return number.doubleValue();
        }
        return null;
    }
}
