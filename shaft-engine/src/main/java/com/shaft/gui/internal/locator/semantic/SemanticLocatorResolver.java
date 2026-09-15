package com.shaft.gui.internal.locator.semantic;

import com.shaft.gui.internal.locator.Locator;
import com.shaft.gui.internal.locator.Role;
import org.openqa.selenium.By;

import java.util.Locale;
import java.util.Optional;

/**
 * Resolves a Selenium {@link By} from inspected accessibility/DOM evidence using
 * semantic precedence, strict uniqueness (or explicit scope), retained confidence,
 * and deterministic structural fallback (issue #5457 FR-1–FR-4).
 *
 * <p>Does not heal silently and does not require a live browser backend: callers
 * supply match counts from their inspection path (engine find, MCP snapshot, fixture).
 */
public final class SemanticLocatorResolver {
    private SemanticLocatorResolver() {
    }

    /**
     * Picks the highest-precedence eligible strategy and builds a locator.
     *
     * @param evidence inspected a11y/DOM evidence with per-strategy match counts
     * @return resolution carrying locator, strategy, confidence, and evidence
     * @throws AmbiguousSemanticLocatorException when a preferred signal matches &gt;1
     *         without {@code scopedIndex}/{@code scopeRoot}
     * @throws IllegalArgumentException when no usable signal is present
     */
    public static SemanticLocatorResolution resolve(SemanticElementEvidence evidence) {
        for (SemanticLocatorStrategy strategy : SemanticLocatorStrategy.values()) {
            Optional<String> expression = expressionFor(evidence, strategy);
            if (expression.isEmpty()) {
                continue;
            }
            int matchCount = evidence.matchCount(strategy);
            if (matchCount == 0) {
                // Signal present in evidence bag but inspection found no matches — skip.
                continue;
            }
            boolean scoped = evidence.scopedIndex().isPresent() || evidence.scopeRoot().isPresent();
            if (matchCount > 1 && !scoped) {
                throw new AmbiguousSemanticLocatorException(strategy, expression.get(), matchCount);
            }
            By locator = buildLocator(strategy, expression.get(), evidence);
            double confidence = confidence(strategy, matchCount, scoped, evidence.hasSemanticSignal());
            // usedSemanticFallback = we had to drop below semantic strategies
            boolean usedSemanticFallback = !strategy.isSemantic();
            return new SemanticLocatorResolution(
                    locator,
                    strategy,
                    expression.get(),
                    confidence,
                    matchCount,
                    scoped && matchCount > 1,
                    usedSemanticFallback,
                    evidence);
        }
        throw new IllegalArgumentException(
                "No usable locator signal in evidence (semantic and structural fallbacks empty)."
                        + (evidence.inspectionNotes().isBlank() ? "" : " notes=" + evidence.inspectionNotes()));
    }

    private static Optional<String> expressionFor(SemanticElementEvidence evidence, SemanticLocatorStrategy strategy) {
        return switch (strategy) {
            case ROLE -> {
                if (evidence.role().isBlank() || evidence.accessibleName().isBlank()) {
                    yield Optional.empty();
                }
                yield Optional.of(evidence.role() + ":" + evidence.accessibleName());
            }
            case ACCESSIBLE_NAME -> blankToEmpty(evidence.accessibleName());
            case LABEL -> blankToEmpty(evidence.label());
            case TEXT -> blankToEmpty(evidence.visibleText());
            case TEST_ID -> blankToEmpty(evidence.testId());
            case ID -> blankToEmpty(evidence.id());
            case NAME -> blankToEmpty(evidence.name());
            case CSS -> blankToEmpty(evidence.css());
            case XPATH -> blankToEmpty(evidence.xpath());
        };
    }

    private static Optional<String> blankToEmpty(String value) {
        return value == null || value.isBlank() ? Optional.empty() : Optional.of(value);
    }

    private static By buildLocator(SemanticLocatorStrategy strategy, String expression, SemanticElementEvidence evidence) {
        By built = switch (strategy) {
            case ROLE -> roleLocator(evidence.role(), evidence.accessibleName());
            case ACCESSIBLE_NAME -> Locator.hasAnyTagName()
                    .containsAttribute("aria-label", expression)
                    .build();
            case LABEL, TEXT -> Locator.hasAnyTagName().hasNormalizedText(expression).build();
            case TEST_ID -> By.cssSelector("[data-testid=\"" + cssEscape(expression)
                    + "\"],[data-test=\"" + cssEscape(expression)
                    + "\"],[data-qa=\"" + cssEscape(expression) + "\"]");
            case ID -> Locator.hasAnyTagName().hasId(expression).build();
            case NAME -> By.name(expression);
            case CSS -> By.cssSelector(expression);
            case XPATH -> By.xpath(expression);
        };
        if (evidence.scopedIndex().isPresent()) {
            // Encode nth as XPath positional wrapper when possible; CSS/name stay as-is with note
            // that callers should prefer scopeRoot for non-XPath backends in later waves.
            int oneBased = evidence.scopedIndex().getAsInt() + 1;
            if (strategy == SemanticLocatorStrategy.CSS || strategy == SemanticLocatorStrategy.NAME
                    || strategy == SemanticLocatorStrategy.TEST_ID) {
                return built;
            }
            String xpath = stripByPrefix(built);
            if (xpath.startsWith("//") || xpath.startsWith("(")) {
                return By.xpath("(" + xpath + ")[" + oneBased + "]");
            }
        }
        return built;
    }

    private static By roleLocator(String role, String accessibleName) {
        Optional<Role> mapped = mapRole(role);
        if (mapped.isPresent()) {
            return Locator.hasRole(mapped.get()).hasNormalizedText(accessibleName).build();
        }
        return By.xpath("//*[@role=" + xpathLiteral(role)
                + " and normalize-space(.)=" + xpathLiteral(accessibleName) + "]");
    }

    private static Optional<Role> mapRole(String role) {
        if (role == null || role.isBlank()) {
            return Optional.empty();
        }
        String normalized = role.trim().toUpperCase(Locale.ROOT).replace('-', '_');
        // ARIA "columnheader" etc. align with Role enum names where possible.
        try {
            return Optional.of(Role.valueOf(normalized));
        } catch (IllegalArgumentException ignored) {
            return switch (normalized) {
                case "ROW" -> Optional.of(Role.TABLE_ROW);
                case "CELL", "GRIDCELL" -> Optional.of(Role.TABLE_CELL);
                case "COLUMNHEADER" -> Optional.of(Role.TABLE_COLUMNHEADER);
                default -> Optional.empty();
            };
        }
    }

    private static double confidence(
            SemanticLocatorStrategy strategy,
            int matchCount,
            boolean scoped,
            boolean hadSemantic) {
        double base = strategy.baseConfidence();
        if (matchCount > 1 && scoped) {
            base *= 0.85;
        }
        if (!strategy.isSemantic() && hadSemantic) {
            // Preferring structural while semantic existed is unexpected here (shouldn't happen);
            // keep base.
            return base;
        }
        if (!strategy.isSemantic() && !hadSemantic) {
            return Math.min(base, 0.40);
        }
        return base;
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

    private static String stripByPrefix(By locator) {
        String text = locator.toString();
        if (text.startsWith("By.xpath: ")) {
            return text.substring("By.xpath: ".length());
        }
        if (text.startsWith("By.cssSelector: ")) {
            return text.substring("By.cssSelector: ".length());
        }
        return text;
    }
}
