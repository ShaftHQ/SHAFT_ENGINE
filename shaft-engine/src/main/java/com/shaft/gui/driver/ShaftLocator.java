package com.shaft.gui.driver;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import org.openqa.selenium.By;

import java.util.Objects;

/**
 * Backend-neutral SHAFT locator that resolves to Selenium {@link By} or
 * Playwright {@link Locator}.
 */
public final class ShaftLocator {
    public enum Strategy {
        CSS,
        XPATH,
        TEXT
    }

    private final Strategy strategy;
    private final String value;

    private ShaftLocator(Strategy strategy, String value) {
        this.strategy = Objects.requireNonNull(strategy, "strategy");
        this.value = Objects.requireNonNull(value, "value");
    }

    public static ShaftLocator css(String selector) {
        return new ShaftLocator(Strategy.CSS, selector);
    }

    public static ShaftLocator xpath(String xpath) {
        return new ShaftLocator(Strategy.XPATH, xpath);
    }

    public static ShaftLocator text(String text) {
        return new ShaftLocator(Strategy.TEXT, text);
    }

    public static ShaftLocator from(By locator) {
        String locatorText = locator.toString();
        if (locatorText.startsWith("By.cssSelector: ")) {
            return css(locatorText.substring("By.cssSelector: ".length()));
        }
        if (locatorText.startsWith("By.xpath: ")) {
            return xpath(locatorText.substring("By.xpath: ".length()));
        }
        if (locatorText.startsWith("By.id: ")) {
            return css("[id=\"" + cssAttributeValue(locatorText.substring("By.id: ".length())) + "\"]");
        }
        if (locatorText.startsWith("By.name: ")) {
            return css("[name=\"" + cssAttributeValue(locatorText.substring("By.name: ".length())) + "\"]");
        }
        if (locatorText.startsWith("By.className: ")) {
            return css("." + cssIdentifier(locatorText.substring("By.className: ".length())));
        }
        if (locatorText.startsWith("By.tagName: ")) {
            return css(locatorText.substring("By.tagName: ".length()));
        }
        if (locatorText.startsWith("By.linkText: ")) {
            return xpath("//a[normalize-space(.)=" + xpathLiteral(locatorText.substring("By.linkText: ".length())) + "]");
        }
        if (locatorText.startsWith("By.partialLinkText: ")) {
            return xpath("//a[contains(normalize-space(.)," + xpathLiteral(locatorText.substring("By.partialLinkText: ".length())) + ")]");
        }
        throw new IllegalArgumentException("Unsupported locator conversion for Playwright: " + locatorText);
    }

    public Strategy strategy() {
        return strategy;
    }

    public String value() {
        return value;
    }

    public By toBy() {
        return switch (strategy) {
            case CSS -> By.cssSelector(value);
            case XPATH -> By.xpath(value);
            case TEXT -> By.xpath("//*[normalize-space(.)=" + xpathLiteral(value) + "]");
        };
    }

    public Locator toPlaywrightLocator(Page page) {
        return page.locator(toPlaywrightSelector());
    }

    public String toPlaywrightSelector() {
        return switch (strategy) {
            case CSS -> value;
            case XPATH -> "xpath=" + value;
            case TEXT -> "text=" + playwrightTextLiteral(value);
        };
    }

    @Override
    public String toString() {
        return strategy + ":" + value;
    }

    private static String cssAttributeValue(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
    }

    private static String cssIdentifier(String value) {
        return value.replace("\\", "\\\\").replace(".", "\\.");
    }

    private static String xpathLiteral(String value) {
        if (!value.contains("'")) {
            return "'" + value + "'";
        }
        if (!value.contains("\"")) {
            return "\"" + value + "\"";
        }
        return "concat('" + value.replace("'", "',\"'\",'") + "')";
    }

    private static String playwrightTextLiteral(String value) {
        return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }
}
