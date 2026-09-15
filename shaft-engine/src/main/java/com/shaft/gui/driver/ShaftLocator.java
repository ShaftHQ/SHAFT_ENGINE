package com.shaft.gui.driver;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.AriaRole;
import com.shaft.gui.internal.locator.Role;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorResolution;
import org.openqa.selenium.By;

import java.util.Locale;
import java.util.Objects;
import java.util.Optional;

/**
 * Backend-neutral SHAFT locator that resolves to Selenium {@link By} or
 * Playwright {@link Locator}.
 *
 * <p>Strategies {@link Strategy#ROLE} and {@link Strategy#ACCESSIBLE_NAME} are the
 * portable a11y surface for issue #5821 (Selenium role builders and Playwright
 * {@code getByRole}/{@code getByLabel}). Backend differences: Playwright prefers
 * engine role queries; Selenium uses SHAFT {@code Locator.hasRole} / aria-label
 * when an enum role is known, else role-attribute xpath.
 */
public final class ShaftLocator {
    public enum Strategy {
        CSS,
        XPATH,
        TEXT,
        /** ARIA role paired with accessible name. */
        ROLE,
        /** Accessible name / aria-label (or Playwright getByLabel). */
        ACCESSIBLE_NAME
    }

    private final Strategy strategy;
    private final String value;
    /** Accessible name when {@link Strategy#ROLE}; otherwise empty. */
    private final String secondaryValue;

    private ShaftLocator(Strategy strategy, String value) {
        this(strategy, value, "");
    }

    private ShaftLocator(Strategy strategy, String value, String secondaryValue) {
        this.strategy = Objects.requireNonNull(strategy, "strategy");
        this.value = Objects.requireNonNull(value, "value");
        this.secondaryValue = Objects.requireNonNullElse(secondaryValue, "");
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

    /**
     * Portable role + accessible name locator (Selenium + Playwright).
     *
     * @param role ARIA role name (e.g. {@code button}, {@code link})
     * @param accessibleName computed accessible name
     * @return portable locator
     */
    public static ShaftLocator role(String role, String accessibleName) {
        Objects.requireNonNull(role, "role");
        Objects.requireNonNull(accessibleName, "accessibleName");
        if (role.isBlank() || accessibleName.isBlank()) {
            throw new IllegalArgumentException("role and accessibleName must be non-blank");
        }
        return new ShaftLocator(Strategy.ROLE, role.trim(), accessibleName);
    }

    /**
     * Portable accessible-name locator (aria-label / getByLabel).
     *
     * @param accessibleName accessible name
     * @return portable locator
     */
    public static ShaftLocator accessibleName(String accessibleName) {
        Objects.requireNonNull(accessibleName, "accessibleName");
        if (accessibleName.isBlank()) {
            throw new IllegalArgumentException("accessibleName must be non-blank");
        }
        return new ShaftLocator(Strategy.ACCESSIBLE_NAME, accessibleName);
    }

    /**
     * Builds a portable locator from a semantic resolution, preserving ROLE /
     * ACCESSIBLE_NAME strategies when selected by {@code SemanticLocatorResolver}.
     *
     * @param resolution resolver output
     * @return portable locator for Selenium and Playwright
     */
    public static ShaftLocator from(SemanticLocatorResolution resolution) {
        Objects.requireNonNull(resolution, "resolution");
        return switch (resolution.strategy()) {
            case ROLE -> role(resolution.evidence().role(), resolution.evidence().accessibleName());
            case ACCESSIBLE_NAME -> accessibleName(resolution.evidence().accessibleName());
            case LABEL, TEXT -> text(resolution.expression());
            case XPATH -> xpath(resolution.expression());
            default -> from(resolution.locator());
        };
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

    /**
     * Secondary value (accessible name for {@link Strategy#ROLE}).
     *
     * @return accessible name or empty
     */
    public String secondaryValue() {
        return secondaryValue;
    }

    public By toBy() {
        return switch (strategy) {
            case CSS -> By.cssSelector(value);
            case XPATH -> By.xpath(value);
            case TEXT -> By.xpath("//*[normalize-space(.)=" + xpathLiteral(value) + "]");
            case ROLE -> roleToBy(value, secondaryValue);
            case ACCESSIBLE_NAME -> com.shaft.gui.internal.locator.Locator.hasAnyTagName()
                    .containsAttribute("aria-label", value)
                    .build();
        };
    }

    public Locator toPlaywrightLocator(Page page) {
        Objects.requireNonNull(page, "page");
        return switch (strategy) {
            case CSS -> page.locator(value);
            case XPATH -> page.locator("xpath=" + value);
            case TEXT -> page.locator("text=" + playwrightTextLiteral(value));
            case ROLE -> page.getByRole(toAriaRole(value),
                    new Page.GetByRoleOptions().setName(secondaryValue).setExact(true));
            case ACCESSIBLE_NAME -> page.getByLabel(value, new Page.GetByLabelOptions().setExact(true));
        };
    }

    /**
     * Resolves this locator scoped under an existing Playwright locator (descendant).
     *
     * @param parent Playwright parent locator
     * @return scoped Playwright locator
     */
    public Locator toPlaywrightLocator(Locator parent) {
        Objects.requireNonNull(parent, "parent");
        return switch (strategy) {
            case CSS -> parent.locator(value);
            case XPATH -> parent.locator("xpath=" + value);
            case TEXT -> parent.locator("text=" + playwrightTextLiteral(value));
            case ROLE -> parent.getByRole(toAriaRole(value),
                    new Locator.GetByRoleOptions().setName(secondaryValue).setExact(true));
            case ACCESSIBLE_NAME -> parent.getByLabel(value, new Locator.GetByLabelOptions().setExact(true));
        };
    }

    /**
     * Playwright string selector for CSS/XPATH/TEXT. ROLE / ACCESSIBLE_NAME must use
     * {@link #toPlaywrightLocator(Page)} / {@link #toPlaywrightLocator(Locator)}.
     *
     * @return selector string
     * @throws UnsupportedOperationException for ROLE / ACCESSIBLE_NAME
     */
    public String toPlaywrightSelector() {
        return switch (strategy) {
            case CSS -> value;
            case XPATH -> "xpath=" + value;
            case TEXT -> "text=" + playwrightTextLiteral(value);
            case ROLE, ACCESSIBLE_NAME -> throw new UnsupportedOperationException(
                    strategy + " has no string selector; use toPlaywrightLocator(Page|Locator) "
                            + "(Playwright getByRole/getByLabel).");
        };
    }

    @Override
    public String toString() {
        if (strategy == Strategy.ROLE) {
            return strategy + ":" + value + ":" + secondaryValue;
        }
        return strategy + ":" + value;
    }

    private static By roleToBy(String role, String accessibleName) {
        Optional<Role> mapped = mapShaftRole(role);
        if (mapped.isPresent()) {
            return com.shaft.gui.internal.locator.Locator.hasRole(mapped.get())
                    .hasNormalizedText(accessibleName)
                    .build();
        }
        return By.xpath("//*[@role=" + xpathLiteral(role)
                + " and normalize-space(.)=" + xpathLiteral(accessibleName) + "]");
    }

    private static Optional<Role> mapShaftRole(String role) {
        if (role == null || role.isBlank()) {
            return Optional.empty();
        }
        String normalized = role.trim().toUpperCase(Locale.ROOT).replace('-', '_');
        try {
            return Optional.of(Role.valueOf(normalized));
        } catch (IllegalArgumentException ignored) {
            return switch (normalized) {
                case "ROW" -> Optional.of(Role.TABLE_ROW);
                case "CELL", "GRIDCELL" -> Optional.of(Role.TABLE_CELL);
                case "COLUMNHEADER" -> Optional.of(Role.TABLE_COLUMNHEADER);
                case "IMG", "IMAGE" -> Optional.of(Role.IMAGE);
                default -> Optional.empty();
            };
        }
    }

    private static AriaRole toAriaRole(String role) {
        String normalized = role.trim().toUpperCase(Locale.ROOT).replace('-', '_');
        if ("IMAGE".equals(normalized)) {
            normalized = "IMG";
        }
        if ("TABLE_ROW".equals(normalized) || "TABLEROW".equals(normalized)) {
            normalized = "ROW";
        }
        if ("TABLE_CELL".equals(normalized) || "TABLECELL".equals(normalized)) {
            normalized = "CELL";
        }
        if ("TABLE_COLUMNHEADER".equals(normalized)) {
            normalized = "COLUMNHEADER";
        }
        try {
            return AriaRole.valueOf(normalized);
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException("Unsupported ARIA role for Playwright getByRole: " + role, ex);
        }
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
