package com.shaft.heal.internal;

import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.LocatorFingerprint;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

final class FingerprintExtractor {
    private static final String LABEL_SCRIPT = """
            const element = arguments[0];
            if (element.labels && element.labels.length) {
              return Array.from(element.labels).map(label => label.innerText || label.textContent || '').join(' ');
            }
            const id = element.getAttribute('id');
            if (id) {
              const label = document.querySelector('label[for="' + CSS.escape(id) + '"]');
              if (label) return label.innerText || label.textContent || '';
            }
            const parent = element.closest ? element.closest('label') : null;
            return parent ? (parent.innerText || parent.textContent || '') : '';
            """;
    private static final String DOM_PATH_SCRIPT = """
            let element = arguments[0];
            const parts = [];
            while (element && element.nodeType === Node.ELEMENT_NODE && parts.length < 12) {
              let index = 1;
              let sibling = element.previousElementSibling;
              while (sibling) {
                if (sibling.tagName === element.tagName) index++;
                sibling = sibling.previousElementSibling;
              }
              parts.unshift(element.tagName.toLowerCase() + ':nth-of-type(' + index + ')');
              element = element.parentElement;
            }
            return parts.join('>');
            """;
    private final HealingConfiguration configuration;

    FingerprintExtractor(HealingConfiguration configuration) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
    }

    LocatorFingerprint extract(WebDriver driver, WebElement element) {
        String type = attribute(element, "type");
        boolean sensitiveInput = "password".equalsIgnoreCase(type)
                || "current-password".equalsIgnoreCase(attribute(element, "autocomplete"))
                || "new-password".equalsIgnoreCase(attribute(element, "autocomplete"));
        Map<String, String> testIds = new LinkedHashMap<>();
        configuration.testIdAttributes().forEach(attribute -> {
            String value = attribute(element, attribute);
            if (!value.isBlank()) {
                testIds.put(attribute, value);
            }
        });
        Map<String, String> semantic = new LinkedHashMap<>();
        add(semantic, "aria-label", attribute(element, "aria-label"));
        add(semantic, "aria-labelledby", attribute(element, "aria-labelledby"));
        add(semantic, "alt", attribute(element, "alt"));
        add(semantic, "autocomplete", attribute(element, "autocomplete"));

        String domPath = javascript(driver, element, DOM_PATH_SCRIPT);
        return new LocatorFingerprint(
                LocatorFingerprint.CURRENT_SCHEMA_VERSION,
                call(element::getTagName),
                sensitiveInput ? "" : call(element::getAccessibleName),
                sensitiveInput ? "" : javascript(driver, element, LABEL_SCRIPT),
                sensitiveInput ? "" : call(element::getText),
                attribute(element, "id"),
                attribute(element, "name"),
                attribute(element, "role"),
                type,
                sensitiveInput ? "" : attribute(element, "placeholder"),
                sensitiveInput ? "" : attribute(element, "title"),
                sanitize(testIds),
                sanitize(semantic),
                domPath.isBlank() ? "" : HealingSupport.sha256(domPath));
    }

    private static Map<String, String> sanitize(Map<String, String> values) {
        Map<String, String> result = new LinkedHashMap<>();
        values.forEach((key, value) -> {
            String sanitized = HealingSupport.sanitize(value);
            if (!sanitized.isBlank()) {
                result.put(key, sanitized);
            }
        });
        return Map.copyOf(result);
    }

    private static void add(Map<String, String> values, String key, String value) {
        if (!value.isBlank()) {
            values.put(key, value);
        }
    }

    private static String attribute(WebElement element, String name) {
        return HealingSupport.sanitize(call(() -> element.getDomAttribute(name)));
    }

    private static String javascript(WebDriver driver, WebElement element, String script) {
        if (!(driver instanceof JavascriptExecutor executor)) {
            return "";
        }
        try {
            Object result = executor.executeScript(script, element);
            return result == null ? "" : HealingSupport.sanitize(String.valueOf(result));
        } catch (WebDriverException exception) {
            return "";
        }
    }

    private static String call(Supplier<String> supplier) {
        try {
            return HealingSupport.sanitize(supplier.get());
        } catch (WebDriverException exception) {
            return "";
        }
    }
}
