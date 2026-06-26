package com.shaft.gui.element.internal;

import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

final class ElementActionabilityDiagnostics {
    private static final int PREVIEW_LIMIT = 120;
    private static final String TARGETABILITY_SCRIPT = """
            const element = arguments[0];
            const rect = element.getBoundingClientRect();
            const centerX = Math.floor(rect.left + rect.width / 2);
            const centerY = Math.floor(rect.top + rect.height / 2);
            const centerTarget = document.elementFromPoint(centerX, centerY);
            const style = window.getComputedStyle(element);
            const targetMatches = centerTarget === element || element.contains(centerTarget);
            const summary = (node) => {
              if (!node) {
                return {};
              }
              const tagName = (node.tagName || '').toLowerCase();
              const id = node.id ? '#' + node.id : '';
              const classes = (node.className && typeof node.className === 'string')
                ? '.' + node.className.trim().split(/\\s+/).filter(Boolean).slice(0, 3).join('.')
                : '';
              return {
                tagName,
                selector: `${tagName}${id}${classes}`,
                textPreview: (node.innerText || node.textContent || '').trim().slice(0, 120)
              };
            };
            return {
              viewport: { width: window.innerWidth, height: window.innerHeight },
              centerPoint: { x: centerX, y: centerY },
              css: {
                display: style.display,
                visibility: style.visibility,
                opacity: style.opacity,
                pointerEvents: style.pointerEvents
              },
              centerTargetMatchesElement: targetMatches,
              obscuringElement: targetMatches ? {} : summary(centerTarget)
            };
            """;

    private ElementActionabilityDiagnostics() {
        throw new IllegalStateException("Utility class");
    }

    static Map<String, Object> collect(By locator, WebDriver driver, List<WebElement> matchedElements, Throwable failure) {
        Map<String, Object> diagnostics = new LinkedHashMap<>();
        List<String> warnings = new ArrayList<>();
        diagnostics.put("locator", locator == null ? "" : JavaHelper.formatLocatorToString(locator));
        putIfPresent(diagnostics, "currentUrl",
                driver == null ? null : safe("current URL", warnings, driver::getCurrentUrl));
        putIfPresent(diagnostics, "failureType", failure == null ? null : failure.getClass().getName());
        putIfPresent(diagnostics, "failureMessage", failure == null ? null : failure.getMessage());
        putIfPresent(diagnostics, "context", context());

        if (matchedElements == null) {
            warn(warnings, "Matched elements were unavailable while collecting actionability diagnostics.");
            diagnostics.put("recommendation", "Inspect the locator lookup failure before retrying the action.");
            return finish(diagnostics, warnings);
        }

        diagnostics.put("matchCount", matchedElements.size());
        if (matchedElements.isEmpty()) {
            diagnostics.put("recommendation", "Check whether the locator matches an element before the action or wait for it to appear.");
            return finish(diagnostics, warnings);
        }
        if (matchedElements.size() > 1) {
            diagnostics.put("recommendation", "Make the locator unique before retrying the action.");
            return finish(diagnostics, warnings);
        }

        WebElement element = matchedElements.getFirst();
        putIfPresent(diagnostics, "displayed", safeElement("displayed state", diagnostics, warnings, element::isDisplayed));
        putIfPresent(diagnostics, "enabled", safeElement("enabled state", diagnostics, warnings, element::isEnabled));
        putIfPresent(diagnostics, "selected", safeElement("selected state", diagnostics, warnings, element::isSelected));
        putIfPresent(diagnostics, "tagName", safeElement("tag name", diagnostics, warnings, element::getTagName));
        putIfPresent(diagnostics, "accessibleName", safeElement("accessible name", diagnostics, warnings, element::getAccessibleName));
        putIfPresent(diagnostics, "textPreview", preview(safeElement("text preview", diagnostics, warnings, element::getText)));
        putIfPresent(diagnostics, "rect", rect(safeElement("element rectangle", diagnostics, warnings, element::getRect)));
        putIfPresent(diagnostics, "accessibleNameHints", accessibleNameHints(element, diagnostics, warnings));

        Map<String, Object> css = css(element, diagnostics, warnings);
        Map<String, Object> probe = targetabilityProbe(driver, element, warnings);
        mergeCss(css, mapValue(probe.get("css")));
        putIfPresent(diagnostics, "css", css);
        putIfPresent(diagnostics, "viewportSize", mapValue(probe.get("viewport")));
        putIfPresent(diagnostics, "centerPoint", mapValue(probe.get("centerPoint")));
        putIfPresent(diagnostics, "centerTargetMatchesElement", probe.get("centerTargetMatchesElement"));
        putIfPresent(diagnostics, "obscuringElement", mapValue(probe.get("obscuringElement")));
        diagnostics.put("recommendation", recommendation(diagnostics, css));
        return finish(diagnostics, warnings);
    }

    private static Map<String, Object> targetabilityProbe(WebDriver driver, WebElement element, List<String> warnings) {
        if (!(driver instanceof JavascriptExecutor javascriptExecutor)) {
            warn(warnings, "Targetability probe skipped because the driver does not execute JavaScript.");
            return Map.of();
        }
        try {
            Object result = javascriptExecutor.executeScript(TARGETABILITY_SCRIPT, element);
            return mapValue(result);
        } catch (RuntimeException e) {
            warn(warnings, "Targetability probe failed: " + e.getClass().getSimpleName() + ": " + value(e.getMessage()));
            return Map.of();
        }
    }

    private static Map<String, Object> css(WebElement element, Map<String, Object> diagnostics, List<String> warnings) {
        Map<String, Object> css = new LinkedHashMap<>();
        putIfPresent(css, "display", safeElement("CSS display", diagnostics, warnings, () -> element.getCssValue("display")));
        putIfPresent(css, "visibility", safeElement("CSS visibility", diagnostics, warnings, () -> element.getCssValue("visibility")));
        putIfPresent(css, "opacity", safeElement("CSS opacity", diagnostics, warnings, () -> element.getCssValue("opacity")));
        putIfPresent(css, "pointerEvents", safeElement("CSS pointer-events", diagnostics, warnings, () -> element.getCssValue("pointer-events")));
        return css;
    }

    private static Map<String, Object> accessibleNameHints(WebElement element, Map<String, Object> diagnostics, List<String> warnings) {
        Map<String, Object> hints = new LinkedHashMap<>();
        putIfPresent(hints, "ariaLabel", safeElement("aria-label", diagnostics, warnings, () -> element.getDomAttribute("aria-label")));
        putIfPresent(hints, "title", safeElement("title", diagnostics, warnings, () -> element.getDomAttribute("title")));
        putIfPresent(hints, "placeholder", safeElement("placeholder", diagnostics, warnings, () -> element.getDomAttribute("placeholder")));
        putIfPresent(hints, "role", safeElement("role", diagnostics, warnings, () -> element.getDomAttribute("role")));
        putIfPresent(hints, "id", safeElement("id", diagnostics, warnings, () -> element.getDomAttribute("id")));
        return hints;
    }

    private static Map<String, Object> context() {
        Map<String, Object> context = new LinkedHashMap<>();
        By frame = LocatorBuilder.getIFrameLocator().get();
        By shadowHost = ShadowLocatorBuilder.shadowDomLocator.get();
        By shadowSelector = ShadowLocatorBuilder.cssSelector.get();
        putIfPresent(context, "frame", frame == null ? null : JavaHelper.formatLocatorToString(frame));
        putIfPresent(context, "shadowHost", shadowHost == null ? null : JavaHelper.formatLocatorToString(shadowHost));
        putIfPresent(context, "shadowSelector", shadowSelector == null ? null : JavaHelper.formatLocatorToString(shadowSelector));
        return context;
    }

    private static Map<String, Object> rect(Rectangle rect) {
        if (rect == null) {
            return Map.of();
        }
        Map<String, Object> value = new LinkedHashMap<>();
        value.put("x", rect.getX());
        value.put("y", rect.getY());
        value.put("width", rect.getWidth());
        value.put("height", rect.getHeight());
        return value;
    }

    private static String recommendation(Map<String, Object> diagnostics, Map<String, Object> css) {
        if (hasValues(diagnostics.get("obscuringElement"))) {
            return "Wait for or close the blocking element before retrying the action.";
        }
        if (Boolean.FALSE.equals(diagnostics.get("displayed"))) {
            return "Wait until the element is visible before retrying the action.";
        }
        if (Boolean.FALSE.equals(diagnostics.get("enabled"))) {
            return "Wait until the element is enabled before retrying the action.";
        }
        if ("none".equalsIgnoreCase(String.valueOf(css.get("pointerEvents")))) {
            return "Remove pointer-events:none or target an actionable child before retrying the action.";
        }
        if (Boolean.TRUE.equals(diagnostics.get("stale"))) {
            return "Re-locate the element before retrying; the page may have re-rendered during the action.";
        }
        return "Inspect the locator, CSS, viewport, and page state before retrying the action.";
    }

    private static <T> T safe(String label, List<String> warnings, Supplier<T> supplier) {
        try {
            return supplier.get();
        } catch (RuntimeException e) {
            warn(warnings, "Could not read " + label + ": " + e.getClass().getSimpleName());
            return null;
        }
    }

    private static <T> T safeElement(String label, Map<String, Object> diagnostics, List<String> warnings, Supplier<T> supplier) {
        try {
            return supplier.get();
        } catch (StaleElementReferenceException e) {
            diagnostics.put("stale", true);
            warn(warnings, "Element became stale while reading " + label + ".");
            return null;
        } catch (RuntimeException e) {
            warn(warnings, "Could not read " + label + ": " + e.getClass().getSimpleName());
            return null;
        }
    }

    private static void mergeCss(Map<String, Object> css, Map<String, Object> probeCss) {
        for (Map.Entry<String, Object> entry : probeCss.entrySet()) {
            putIfPresent(css, entry.getKey(), entry.getValue());
        }
    }

    private static Map<String, Object> mapValue(Object value) {
        if (!(value instanceof Map<?, ?> source)) {
            return Map.of();
        }
        Map<String, Object> copy = new LinkedHashMap<>();
        for (Map.Entry<?, ?> entry : source.entrySet()) {
            String key = String.valueOf(entry.getKey());
            Object entryValue = entry.getValue();
            if (entryValue instanceof Map<?, ?>) {
                putIfPresent(copy, key, mapValue(entryValue));
            } else {
                putIfPresent(copy, key, entryValue);
            }
        }
        return copy;
    }

    private static Map<String, Object> finish(Map<String, Object> diagnostics, List<String> warnings) {
        if (!warnings.isEmpty()) {
            diagnostics.put("warnings", List.copyOf(warnings));
        }
        return diagnostics;
    }

    private static void putIfPresent(Map<String, Object> target, String key, Object value) {
        if (!hasValues(value)) {
            return;
        }
        target.put(key, value);
    }

    private static boolean hasValues(Object value) {
        if (value == null) {
            return false;
        }
        if (value instanceof String text) {
            return !text.isBlank();
        }
        if (value instanceof Map<?, ?> map) {
            return !map.isEmpty();
        }
        if (value instanceof List<?> list) {
            return !list.isEmpty();
        }
        return true;
    }

    private static String preview(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        String normalized = text.replaceAll("\\s+", " ").trim();
        return normalized.length() <= PREVIEW_LIMIT ? normalized : normalized.substring(0, PREVIEW_LIMIT);
    }

    private static void warn(List<String> warnings, String warning) {
        if (!warnings.contains(warning)) {
            warnings.add(warning);
        }
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }
}
