package com.shaft.gui.internal.locator.semantic;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.internal.aria.AriaNode;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * Live DOM / aria-snapshot match-count inspection that feeds
 * {@link SemanticLocatorResolver} (issue #5817 / parent #5457).
 *
 * <p>Backend-neutral where possible: aria forests need no browser; Selenium and
 * Playwright paths count uniqueness with the same portable {@link ShaftLocator}
 * strategies. Does not heal silently.
 */
public final class SemanticMatchInspector {
    private SemanticMatchInspector() {
    }

    /**
     * Builds evidence for a target identified by role + accessible name inside an
     * aria snapshot forest, with per-strategy uniqueness counts across the forest.
     *
     * @param forest aria-accessible-name tree (from {@code AriaSnapshotHelper})
     * @param targetRole ARIA role of the intended node
     * @param targetAccessibleName accessible name of the intended node
     * @return evidence ready for {@link SemanticLocatorResolver#resolve}
     * @throws IllegalArgumentException when the forest has no matching node
     */
    public static SemanticElementEvidence fromAriaForest(
            List<AriaNode> forest,
            String targetRole,
            String targetAccessibleName) {
        Objects.requireNonNull(forest, "forest");
        String role = safe(targetRole);
        String name = safe(targetAccessibleName);
        if (role.isBlank() || name.isBlank()) {
            throw new IllegalArgumentException("fromAriaForest requires non-blank role and accessible name");
        }
        List<AriaNode> flat = flatten(forest);
        AriaNode target = flat.stream()
                .filter(n -> role.equals(n.role()) && name.equals(n.name()))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException(
                        "No aria node with role=\"" + role + "\" name=\"" + name + "\""));

        int roleMatches = countRoleName(flat, role, name);
        int accessibleNameMatches = countAccessibleName(flat, name);
        int textMatches = countAccessibleName(flat, name); // forest names ≈ visible/accessible text

        return SemanticElementEvidence.builder()
                .role(target.role(), target.name(), roleMatches)
                .accessibleName(target.name(), accessibleNameMatches)
                .visibleText(target.name(), textMatches)
                .inspectionNotes("aria-forest inspection; roleMatches=" + roleMatches
                        + " nameMatches=" + accessibleNameMatches)
                .build();
    }

    /**
     * Localized / inaccessible forest path (FR-4 / SC-3): when the target has no
     * usable semantic name, only structural hints are retained and confidence is
     * capped by the resolver's fallback rules.
     *
     * @param forest aria forest (used only for notes / emptiness check)
     * @param css structural CSS for the target (may be blank)
     * @param cssMatchCount uniqueness count for {@code css}
     * @param xpath structural XPath for the target (may be blank)
     * @param xpathMatchCount uniqueness count for {@code xpath}
     * @param notes inspection provenance
     * @return evidence with no semantic signals
     */
    public static SemanticElementEvidence inaccessibleWithStructuralFallback(
            List<AriaNode> forest,
            String css,
            int cssMatchCount,
            String xpath,
            int xpathMatchCount,
            String notes) {
        Objects.requireNonNull(forest, "forest");
        SemanticElementEvidence.Builder builder = SemanticElementEvidence.builder()
                .inspectionNotes(safe(notes).isBlank()
                        ? "inaccessible aria-forest; structural fallback only"
                        : notes);
        if (!safe(css).isBlank()) {
            builder.css(css, cssMatchCount);
        }
        if (!safe(xpath).isBlank()) {
            builder.xpath(xpath, xpathMatchCount);
        }
        return builder.build();
    }

    /**
     * Inspects the live Selenium DOM around a unique seed locator and returns
     * evidence with counted strategy uniqueness.
     *
     * @param driver active driver / search context
     * @param seedLocator locator that identifies the intended element
     * @return evidence with live match counts
     * @throws IllegalArgumentException when the seed is missing or not unique
     */
    public static SemanticElementEvidence fromSelenium(SearchContext driver, By seedLocator) {
        Objects.requireNonNull(driver, "driver");
        Objects.requireNonNull(seedLocator, "seedLocator");
        List<WebElement> seeds = driver.findElements(seedLocator);
        if (seeds.isEmpty()) {
            throw new IllegalArgumentException("Seed locator matched 0 elements: " + seedLocator);
        }
        if (seeds.size() > 1) {
            throw new IllegalArgumentException(
                    "Seed locator matched " + seeds.size() + " elements (must be unique): " + seedLocator);
        }
        WebElement target = seeds.get(0);
        DomSignals signals = readDomSignals(target);
        SearchContext root = driver instanceof WebDriver webDriver ? webDriver : driver;
        return buildEvidenceFromSignals(signals, strategy -> countSelenium(root, strategy, signals));
    }

    /**
     * Inspects the live Playwright page around a seed locator and returns evidence
     * with counted strategy uniqueness ({@code getByRole} / {@code getByLabel} where applicable).
     *
     * @param page current Playwright page
     * @param seed portable seed that uniquely identifies the target
     * @return evidence with live match counts
     * @throws IllegalArgumentException when the seed is missing or not unique
     */
    public static SemanticElementEvidence fromPlaywright(Page page, ShaftLocator seed) {
        Objects.requireNonNull(page, "page");
        Objects.requireNonNull(seed, "seed");
        Locator target = seed.toPlaywrightLocator(page);
        int seedCount = target.count();
        if (seedCount == 0) {
            throw new IllegalArgumentException("Seed locator matched 0 elements: " + seed);
        }
        if (seedCount > 1) {
            throw new IllegalArgumentException(
                    "Seed locator matched " + seedCount + " elements (must be unique): " + seed);
        }
        DomSignals signals = readPlaywrightSignals(target);
        return buildEvidenceFromSignals(signals, strategy -> countPlaywright(page, strategy, signals));
    }

    private static SemanticElementEvidence buildEvidenceFromSignals(
            DomSignals signals,
            java.util.function.ToIntFunction<SemanticLocatorStrategy> counter) {
        SemanticElementEvidence.Builder builder = SemanticElementEvidence.builder()
                .inspectionNotes(signals.notes());
        applySemanticSignals(builder, signals, counter);
        applyStructuralSignals(builder, signals, counter);
        return builder.build();
    }

    private static void applySemanticSignals(
            SemanticElementEvidence.Builder builder,
            DomSignals signals,
            java.util.function.ToIntFunction<SemanticLocatorStrategy> counter) {
        if (!signals.role().isBlank() && !signals.accessibleName().isBlank()) {
            builder.role(signals.role(), signals.accessibleName(),
                    counter.applyAsInt(SemanticLocatorStrategy.ROLE));
        }
        if (!signals.accessibleName().isBlank()) {
            builder.accessibleName(signals.accessibleName(),
                    counter.applyAsInt(SemanticLocatorStrategy.ACCESSIBLE_NAME));
        }
        if (!signals.label().isBlank()) {
            builder.label(signals.label(), counter.applyAsInt(SemanticLocatorStrategy.LABEL));
        }
        if (!signals.visibleText().isBlank()) {
            builder.visibleText(signals.visibleText(), counter.applyAsInt(SemanticLocatorStrategy.TEXT));
        }
        if (!signals.testId().isBlank()) {
            builder.testId(signals.testId(), counter.applyAsInt(SemanticLocatorStrategy.TEST_ID));
        }
    }

    private static void applyStructuralSignals(
            SemanticElementEvidence.Builder builder,
            DomSignals signals,
            java.util.function.ToIntFunction<SemanticLocatorStrategy> counter) {
        if (!signals.id().isBlank()) {
            builder.id(signals.id(), counter.applyAsInt(SemanticLocatorStrategy.ID));
        }
        if (!signals.name().isBlank()) {
            builder.name(signals.name(), counter.applyAsInt(SemanticLocatorStrategy.NAME));
        }
        if (!signals.css().isBlank()) {
            builder.css(signals.css(), counter.applyAsInt(SemanticLocatorStrategy.CSS));
        }
        if (!signals.xpath().isBlank()) {
            builder.xpath(signals.xpath(), counter.applyAsInt(SemanticLocatorStrategy.XPATH));
        }
    }

    private static int countSelenium(SearchContext root, SemanticLocatorStrategy strategy, DomSignals signals) {
        By by = locatorFor(strategy, signals);
        if (by == null) {
            return 0;
        }
        return root.findElements(by).size();
    }

    private static int countPlaywright(Page page, SemanticLocatorStrategy strategy, DomSignals signals) {
        ShaftLocator locator = portableFor(strategy, signals);
        if (locator == null) {
            return 0;
        }
        return locator.toPlaywrightLocator(page).count();
    }

    private static By locatorFor(SemanticLocatorStrategy strategy, DomSignals signals) {
        ShaftLocator portable = portableFor(strategy, signals);
        return portable == null ? null : portable.toBy();
    }

    private static ShaftLocator portableFor(SemanticLocatorStrategy strategy, DomSignals signals) {
        return switch (strategy) {
            case ROLE -> signals.role().isBlank() || signals.accessibleName().isBlank()
                    ? null
                    : ShaftLocator.role(signals.role(), signals.accessibleName());
            case ACCESSIBLE_NAME -> signals.accessibleName().isBlank()
                    ? null
                    : ShaftLocator.accessibleName(signals.accessibleName());
            case LABEL -> signals.label().isBlank() ? null : ShaftLocator.text(signals.label());
            case TEXT -> signals.visibleText().isBlank() ? null : ShaftLocator.text(signals.visibleText());
            case TEST_ID -> signals.testId().isBlank() ? null : ShaftLocator.css(testIdSelector(signals.testId()));
            case ID -> signals.id().isBlank() ? null : ShaftLocator.css("[id=\"" + cssEscape(signals.id()) + "\"]");
            case NAME -> signals.name().isBlank()
                    ? null
                    : ShaftLocator.css("[name=\"" + cssEscape(signals.name()) + "\"]");
            case CSS -> signals.css().isBlank() ? null : ShaftLocator.css(signals.css());
            case XPATH -> signals.xpath().isBlank() ? null : ShaftLocator.xpath(signals.xpath());
        };
    }

    private static DomSignals readDomSignals(WebElement element) {
        String role = firstNonBlank(attr(element, "role"), implicitRole(element));
        String accessibleName = safe(element.getAccessibleName());
        if (accessibleName.isBlank()) {
            accessibleName = firstNonBlank(attr(element, "aria-label"), attr(element, "title"), safe(element.getText()).trim());
        }
        String label = attr(element, "aria-label");
        String visibleText = normalize(safe(element.getText()));
        String testId = firstNonBlank(attr(element, "data-testid"), attr(element, "data-test"), attr(element, "data-qa"));
        String id = attr(element, "id");
        String name = attr(element, "name");
        String css = id.isBlank() ? "" : "#" + cssEscape(id);
        String xpath = id.isBlank() ? "" : "//*[@id=" + xpathLiteral(id) + "]";
        return new DomSignals(role, accessibleName, label, visibleText, testId, id, name, css, xpath,
                "selenium DOM inspection");
    }

    @SuppressWarnings("unchecked")
    private static DomSignals readPlaywrightSignals(Locator target) {
        Object raw = target.evaluate("""
                (el) => {
                  const attr = (n) => el.getAttribute(n) || '';
                  const role = attr('role') || '';
                  const ariaLabel = attr('aria-label');
                  const title = attr('title');
                  const text = (el.innerText || el.textContent || '').replace(/\\s+/g, ' ').trim();
                  const id = attr('id');
                  const name = attr('name');
                  const testId = attr('data-testid') || attr('data-test') || attr('data-qa');
                  let accessibleName = ariaLabel || title || text;
                  try {
                    if (el.accessibleName) { accessibleName = el.accessibleName; }
                  } catch (e) { /* ignore */ }
                  return {
                    role, accessibleName, label: ariaLabel, visibleText: text,
                    testId, id, name,
                    css: id ? ('#' + id) : '',
                    xpath: id ? ("//*[@id='" + id.replace(/'/g, "\\\\'") + "']") : ''
                  };
                }
                """);
        Map<String, Object> map = raw instanceof Map<?, ?> m ? (Map<String, Object>) m : Map.of();
        return new DomSignals(
                safe(stringVal(map.get("role"))),
                safe(stringVal(map.get("accessibleName"))),
                safe(stringVal(map.get("label"))),
                safe(stringVal(map.get("visibleText"))),
                safe(stringVal(map.get("testId"))),
                safe(stringVal(map.get("id"))),
                safe(stringVal(map.get("name"))),
                safe(stringVal(map.get("css"))),
                safe(stringVal(map.get("xpath"))),
                "playwright DOM inspection");
    }

    private static String stringVal(Object value) {
        return value == null ? "" : String.valueOf(value);
    }

    private static String implicitRole(WebElement element) {
        String tag = safe(element.getTagName()).toLowerCase(Locale.ROOT);
        return switch (tag) {
            case "button" -> "button";
            case "a" -> attr(element, "href").isBlank() ? "" : "link";
            case "img" -> "img";
            case "select" -> "combobox";
            case "textarea" -> "textbox";
            case "input" -> switch (safe(attr(element, "type")).toLowerCase(Locale.ROOT)) {
                case "", "text", "email", "password", "search", "tel", "url" -> "textbox";
                case "checkbox" -> "checkbox";
                case "radio" -> "radio";
                case "button", "submit", "reset" -> "button";
                default -> "textbox";
            };
            case "h1", "h2", "h3", "h4", "h5", "h6" -> "heading";
            default -> "";
        };
    }

    private static List<AriaNode> flatten(List<AriaNode> forest) {
        List<AriaNode> flat = new ArrayList<>();
        for (AriaNode node : forest) {
            flattenInto(node, flat);
        }
        return flat;
    }

    private static void flattenInto(AriaNode node, List<AriaNode> out) {
        out.add(node);
        for (AriaNode child : node.children()) {
            flattenInto(child, out);
        }
    }

    private static int countRoleName(List<AriaNode> flat, String role, String name) {
        int count = 0;
        for (AriaNode node : flat) {
            if (role.equals(node.role()) && name.equals(node.name())) {
                count++;
            }
        }
        return count;
    }

    private static int countAccessibleName(List<AriaNode> flat, String name) {
        int count = 0;
        for (AriaNode node : flat) {
            if (name.equals(node.name())) {
                count++;
            }
        }
        return count;
    }

    private static String attr(WebElement element, String name) {
        try {
            String value = element.getDomAttribute(name);
            if (value == null) {
                value = element.getAttribute(name);
            }
            return safe(value);
        } catch (RuntimeException ignored) {
            return "";
        }
    }

    private static String firstNonBlank(String... values) {
        for (String value : values) {
            if (!safe(value).isBlank()) {
                return value.trim();
            }
        }
        return "";
    }

    private static String normalize(String value) {
        return safe(value).replaceAll("\\s+", " ").trim();
    }

    private static String safe(String value) {
        return value == null ? "" : value;
    }

    private static String testIdSelector(String testId) {
        String escaped = cssEscape(testId);
        return "[data-testid=\"" + escaped + "\"],[data-test=\"" + escaped + "\"],[data-qa=\"" + escaped + "\"]";
    }

    private static String cssEscape(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
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

    private record DomSignals(
            String role,
            String accessibleName,
            String label,
            String visibleText,
            String testId,
            String id,
            String name,
            String css,
            String xpath,
            String notes) {
    }
}
