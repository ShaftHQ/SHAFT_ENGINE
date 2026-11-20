package com.shaft.gui.element.internal;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Custom scroll offsets that do not move must fall back to native nearest scroll,
 * and success is still intersection with the scrollport.
 */
public class WebScrollTest {
    @Test
    public void nativeNearestScrollSucceedsWhenCustomOffsetsDoNotMove() {
        StuckScroller scroller = new StuckScroller(true);
        WebDriver driver = scroller.driver();

        assertTrue(WebScroll.scrollIntoView(driver, scroller.element));
        assertTrue(scroller.nativeAfterFailedApply(), "native nearest scroll did not follow a stuck apply");
        assertTrue(scroller.inView);
    }

    @Test
    public void staysOutsideWhenNativeNearestScrollDoesNotIntersect() {
        StuckScroller scroller = new StuckScroller(false);
        WebDriver driver = scroller.driver();

        assertFalse(WebScroll.scrollIntoView(driver, scroller.element));
        assertTrue(scroller.nativeAfterFailedApply(), "native nearest scroll did not follow a stuck apply");
        assertFalse(scroller.inView);
    }

    private static final class StuckScroller {
        private final boolean bringIntoView;
        private final List<String> scripts = new ArrayList<>();
        private final WebElement element = mock(WebElement.class);
        private boolean inView;

        private StuckScroller(boolean bringIntoView) {
            this.bringIntoView = bringIntoView;
        }

        private WebDriver driver() {
            WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
            doAnswer(invocation -> {
                String script = invocation.getArgument(0);
                scripts.add(script);
                if (script.contains("shaft-scroll-apply")) {
                    return offsets();
                }
                if (script.contains("scrollIntoView({block: \"nearest\", inline: \"nearest\"})")) {
                    inView = bringIntoView;
                    return null;
                }
                if (script.contains("shaft-scroll-metrics")) {
                    return inView ? inside() : outside();
                }
                return null;
            }).when((JavascriptExecutor) driver).executeScript(anyString(), any(Object[].class));
            return driver;
        }

        private boolean nativeAfterFailedApply() {
            int applyAt = -1;
            int nativeAt = -1;
            for (int index = 0; index < scripts.size(); index++) {
                String script = scripts.get(index);
                if (applyAt < 0 && script.contains("shaft-scroll-apply")) {
                    applyAt = index;
                }
                if (nativeAt < 0 && script.contains("scrollIntoView({block: \"nearest\", inline: \"nearest\"})")) {
                    nativeAt = index;
                }
            }
            return applyAt >= 0 && nativeAt > applyAt;
        }

        private static Map<String, Object> outside() {
            return metrics(500, 540, 0, 0, 800);
        }

        private static Map<String, Object> inside() {
            return metrics(20, 60, 0, 0, 800);
        }

        private static Map<String, Object> offsets() {
            Map<String, Object> offsets = new LinkedHashMap<>();
            offsets.put("scrollLeft", 0);
            offsets.put("scrollTop", 0);
            return offsets;
        }

        private static Map<String, Object> metrics(int childTop, int childBottom, int scrollLeft, int scrollTop, int maxScrollTop) {
            Map<String, Object> metrics = new LinkedHashMap<>();
            metrics.put("childLeft", 10);
            metrics.put("childTop", childTop);
            metrics.put("childRight", 50);
            metrics.put("childBottom", childBottom);
            metrics.put("viewLeft", 0);
            metrics.put("viewTop", 0);
            metrics.put("viewRight", 300);
            metrics.put("viewBottom", 200);
            metrics.put("scrollLeft", scrollLeft);
            metrics.put("scrollTop", scrollTop);
            metrics.put("maxScrollLeft", 0);
            metrics.put("maxScrollTop", maxScrollTop);
            return metrics;
        }
    }
}
