package com.shaft.gui.element.internal;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.internal.AttachmentReporter;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * One redacted, size-capped "Failure evidence" JSON per failed element action (#6399), built on
 * {@link ElementActionabilityDiagnostics} plus page title, per-frame match counts, and the nearest
 * candidate elements when the locator matched nothing.
 */
final class FailureEvidence {
    static final int MAX_BYTES = 256 * 1024;
    private static final int MAX_FRAMES = 5;
    private static final int MAX_TEXT = 2_000;
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final String CANDIDATES_SCRIPT = """
            const tokens = arguments[0];
            const scored = [];
            const nodes = document.querySelectorAll('[id],[name],[data-testid],[aria-label],button,a,input,select,textarea,label');
            for (let i = 0; i < nodes.length && i < 2000; i++) {
              const n = nodes[i];
              const text = (n.innerText || n.value || '').trim().slice(0, 80);
              const hay = [n.id, n.getAttribute('name'), n.getAttribute('data-testid'), n.getAttribute('aria-label'), text]
                .join(' ').toLowerCase();
              const score = tokens.filter(t => hay.includes(t)).length;
              if (score > 0) {
                scored.push({tagName: n.tagName.toLowerCase(), id: n.id || '', name: n.getAttribute('name') || '',
                  testId: n.getAttribute('data-testid') || '', ariaLabel: n.getAttribute('aria-label') || '', text, score});
              }
            }
            return scored.sort((a, b) => b.score - a.score).slice(0, 5);
            """;

    private FailureEvidence() {
        throw new IllegalStateException("Utility class");
    }

    static void attach(By locator, WebDriver driver, List<WebElement> matched, Throwable failure,
                       Map<String, Object> actionability, boolean screenshotAttached) {
        if (!SHAFT.Properties.reporting.attachFailureEvidence()) {
            return;
        }
        try {
            ByteArrayOutputStream content = new ByteArrayOutputStream();
            content.writeBytes(render(locator, driver, matched, failure, actionability, screenshotAttached)
                    .getBytes(StandardCharsets.UTF_8));
            AttachmentReporter.attachBasedOnFileType("Failure evidence", "failure-evidence.json", content,
                    "Failure evidence");
        } catch (RuntimeException evidenceFailure) {
            // evidence is best effort and must never mask the original failure
            ReportManagerHelper.logDiscrete("Could not attach failure evidence: " + evidenceFailure.getMessage(),
                    org.apache.logging.log4j.Level.WARN);
        }
    }

    static String render(By locator, WebDriver driver, List<WebElement> matched, Throwable failure,
                         Map<String, Object> actionability, boolean screenshotAttached) {
        Map<String, Object> evidence = new LinkedHashMap<>(actionability == null || actionability.isEmpty()
                ? ElementActionabilityDiagnostics.collect(locator, driver, matched, failure)
                : actionability);
        evidence.putIfAbsent("matchCount", matched == null ? 0 : matched.size());
        evidence.put("title", safe(() -> driver.getTitle()));
        evidence.put("screenshotAttached", screenshotAttached);
        if (matched == null || matched.isEmpty()) {
            evidence.put("frameMatchCounts", frameMatchCounts(locator, driver));
            evidence.put("nearestCandidates", nearestCandidates(locator, driver));
        }
        String json = json(redact(evidence));
        if (json.getBytes(StandardCharsets.UTF_8).length > MAX_BYTES) {
            evidence.remove("nearestCandidates");
            evidence.replaceAll((key, value) -> value instanceof String text && text.length() > MAX_TEXT
                    ? text.substring(0, MAX_TEXT) + "…" : value);
            evidence.put("truncated", true);
            json = json(redact(evidence));
        }
        return json.getBytes(StandardCharsets.UTF_8).length > MAX_BYTES
                ? json(Map.of("locator", locatorText(locator), "truncated", true))
                : json;
    }

    private static List<Map<String, Object>> frameMatchCounts(By locator, WebDriver driver) {
        List<Map<String, Object>> counts = new ArrayList<>();
        if (locator == null || driver == null) {
            return counts;
        }
        int frames = safeInt(() -> driver.findElements(By.tagName("iframe")).size());
        for (int index = 0; index < Math.min(frames, MAX_FRAMES); index++) {
            final int frame = index;
            try {
                driver.switchTo().frame(frame);
                counts.add(Map.of("frameIndex", frame, "matchCount", safeInt(() -> driver.findElements(locator).size())));
            } catch (RuntimeException ignored) {
                // a detached or cross-origin frame simply has no count
            } finally {
                safe(() -> driver.switchTo().parentFrame());
            }
        }
        return counts;
    }

    private static Object nearestCandidates(By locator, WebDriver driver) {
        if (!(driver instanceof JavascriptExecutor executor) || locator == null) {
            return List.of();
        }
        String value = locatorText(locator).replaceFirst("^By\\.[^:]+:\\s*", "").toLowerCase(Locale.ROOT);
        List<String> tokens = Arrays.stream(value.split("[^a-z0-9]+")).filter(token -> token.length() >= 3)
                .distinct().limit(5).toList();
        if (tokens.isEmpty()) {
            return List.of();
        }
        Object result = safe(() -> executor.executeScript(CANDIDATES_SCRIPT, tokens));
        return result == null ? List.of() : result;
    }

    private static Object redact(Object value) {
        if (value instanceof String text) {
            return FailureTraceReporter.redactInvocationText(text);
        }
        if (value instanceof Map<?, ?> map) {
            Map<String, Object> copy = new LinkedHashMap<>();
            map.forEach((key, item) -> copy.put(String.valueOf(key), redact(item)));
            return copy;
        }
        if (value instanceof List<?> list) {
            return list.stream().map(FailureEvidence::redact).toList();
        }
        return value;
    }

    private static String json(Object value) {
        try {
            return MAPPER.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            throw new IllegalStateException("Could not serialize failure evidence.", e);
        }
    }

    private static String locatorText(By locator) {
        return locator == null ? "" : JavaHelper.formatLocatorToString(locator);
    }

    private static int safeInt(java.util.function.Supplier<Integer> supplier) {
        Integer value = safe(supplier::get);
        return value == null ? 0 : value;
    }

    private static <T> T safe(java.util.function.Supplier<T> supplier) {
        try {
            return supplier.get();
        } catch (RuntimeException ignored) {
            return null;
        }
    }
}
