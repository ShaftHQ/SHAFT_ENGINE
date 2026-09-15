package com.shaft.heal.internal;

import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorStrategy;
import com.shaft.heal.model.LocatorFingerprint;
import org.openqa.selenium.By;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Builds suggested replacement locators from a {@link LocatorFingerprint} in engine
 * {@link SemanticLocatorStrategy} FR-1 order (issue #5820 / #5457).
 *
 * <p>Suggestions only — callers must still enforce uniqueness / ambiguity review (FR-2)
 * and must not silently auto-apply an arbitrary match. When semantic fingerprint fields
 * are empty, only structural proposals are emitted (FR-4).
 */
final class SemanticFingerprintProposals {
    private SemanticFingerprintProposals() {
    }

    /**
     * Ordered locator suggestions for a candidate element fingerprint.
     *
     * @param fingerprint privacy-minimized element fingerprint
     * @param shadowContext whether discovery is inside a shadow root (skips label-for xpath)
     * @return proposals in FR-1 order with strategy + base confidence retained
     */
    static List<Proposal> suggestions(LocatorFingerprint fingerprint, boolean shadowContext) {
        Objects.requireNonNull(fingerprint, "fingerprint");
        List<Proposal> proposals = new ArrayList<>();
        if (hasSemanticFields(fingerprint)) {
            addRole(proposals, fingerprint);
            addAccessibleName(proposals, fingerprint);
            addLabel(proposals, fingerprint, shadowContext);
            addText(proposals, fingerprint, shadowContext);
            addTestIds(proposals, fingerprint);
            addId(proposals, fingerprint);
            addName(proposals, fingerprint);
            addWeakAttributes(proposals, fingerprint);
        } else {
            addId(proposals, fingerprint);
            addName(proposals, fingerprint);
            addWeakAttributes(proposals, fingerprint);
            addTagFallback(proposals, fingerprint);
        }
        return List.copyOf(proposals);
    }

    /**
     * Whether the fingerprint carries user-perceived / test-contract semantics.
     *
     * @param fingerprint fingerprint
     * @return true when ROLE/NAME/LABEL/TEXT/TEST_ID signals exist
     */
    static boolean hasSemanticFields(LocatorFingerprint fingerprint) {
        return (present(fingerprint.role()) && present(fingerprint.accessibleName()))
                || present(fingerprint.accessibleName())
                || present(fingerprint.associatedLabel())
                || present(fingerprint.visibleText())
                || !fingerprint.testIds().isEmpty();
    }

    private static void addRole(List<Proposal> proposals, LocatorFingerprint fingerprint) {
        if (present(fingerprint.role()) && present(fingerprint.accessibleName())) {
            By locator = ShaftLocator.role(fingerprint.role(), fingerprint.accessibleName()).toBy();
            proposals.add(new Proposal(
                    SemanticLocatorStrategy.ROLE,
                    fingerprint.role() + ":" + fingerprint.accessibleName(),
                    locator,
                    SemanticLocatorStrategy.ROLE.baseConfidence()));
        }
    }

    private static void addAccessibleName(List<Proposal> proposals, LocatorFingerprint fingerprint) {
        if (present(fingerprint.accessibleName())) {
            proposals.add(new Proposal(
                    SemanticLocatorStrategy.ACCESSIBLE_NAME,
                    fingerprint.accessibleName(),
                    ShaftLocator.accessibleName(fingerprint.accessibleName()).toBy(),
                    SemanticLocatorStrategy.ACCESSIBLE_NAME.baseConfidence()));
            addAttribute(proposals, SemanticLocatorStrategy.ACCESSIBLE_NAME,
                    "aria-label", fingerprint.accessibleName(),
                    SemanticLocatorStrategy.ACCESSIBLE_NAME.baseConfidence() * 0.95);
        }
        String aria = fingerprint.semanticAttributes().get("aria-label");
        if (present(aria) && !aria.equals(fingerprint.accessibleName())) {
            addAttribute(proposals, SemanticLocatorStrategy.ACCESSIBLE_NAME, "aria-label", aria,
                    SemanticLocatorStrategy.ACCESSIBLE_NAME.baseConfidence() * 0.9);
        }
    }

    private static void addLabel(List<Proposal> proposals, LocatorFingerprint fingerprint, boolean shadowContext) {
        if (shadowContext || !present(fingerprint.associatedLabel())) {
            return;
        }
        String label = xpathLiteral(fingerprint.associatedLabel());
        proposals.add(new Proposal(
                SemanticLocatorStrategy.LABEL,
                fingerprint.associatedLabel(),
                By.xpath("//*[@id = //label[normalize-space(.)=" + label + "]/@for]"),
                SemanticLocatorStrategy.LABEL.baseConfidence()));
        proposals.add(new Proposal(
                SemanticLocatorStrategy.LABEL,
                fingerprint.associatedLabel(),
                By.xpath("//label[normalize-space(.)=" + label + "]//*"),
                SemanticLocatorStrategy.LABEL.baseConfidence() * 0.9));
    }

    private static void addText(List<Proposal> proposals, LocatorFingerprint fingerprint, boolean shadowContext) {
        if (shadowContext || !present(fingerprint.visibleText())) {
            return;
        }
        if (fingerprint.visibleText().equals(fingerprint.accessibleName())) {
            return;
        }
        proposals.add(new Proposal(
                SemanticLocatorStrategy.TEXT,
                fingerprint.visibleText(),
                By.xpath("//*[normalize-space(.)=" + xpathLiteral(fingerprint.visibleText()) + "]"),
                SemanticLocatorStrategy.TEXT.baseConfidence()));
    }

    private static void addTestIds(List<Proposal> proposals, LocatorFingerprint fingerprint) {
        fingerprint.testIds().forEach((attribute, value) -> {
            if (present(value)) {
                proposals.add(new Proposal(
                        SemanticLocatorStrategy.TEST_ID,
                        value,
                        attributeLocator(attribute, value),
                        SemanticLocatorStrategy.TEST_ID.baseConfidence()));
            }
        });
    }

    private static void addId(List<Proposal> proposals, LocatorFingerprint fingerprint) {
        if (present(fingerprint.id())) {
            proposals.add(new Proposal(
                    SemanticLocatorStrategy.ID,
                    fingerprint.id(),
                    By.id(fingerprint.id()),
                    SemanticLocatorStrategy.ID.baseConfidence()));
        }
    }

    private static void addName(List<Proposal> proposals, LocatorFingerprint fingerprint) {
        if (present(fingerprint.name())) {
            proposals.add(new Proposal(
                    SemanticLocatorStrategy.NAME,
                    fingerprint.name(),
                    By.name(fingerprint.name()),
                    SemanticLocatorStrategy.NAME.baseConfidence()));
        }
    }

    private static void addWeakAttributes(List<Proposal> proposals, LocatorFingerprint fingerprint) {
        addAttribute(proposals, SemanticLocatorStrategy.CSS, "placeholder", fingerprint.placeholder(),
                SemanticLocatorStrategy.CSS.baseConfidence());
        addAttribute(proposals, SemanticLocatorStrategy.CSS, "title", fingerprint.title(),
                SemanticLocatorStrategy.CSS.baseConfidence());
    }

    private static void addTagFallback(List<Proposal> proposals, LocatorFingerprint fingerprint) {
        By fallback = fingerprint.tagName().isBlank()
                ? By.cssSelector("*")
                : By.tagName(fingerprint.tagName());
        proposals.add(new Proposal(
                SemanticLocatorStrategy.CSS,
                fingerprint.tagName().isBlank() ? "*" : fingerprint.tagName(),
                fallback,
                Math.min(SemanticLocatorStrategy.CSS.baseConfidence(), 0.40)));
    }

    private static void addAttribute(
            List<Proposal> proposals,
            SemanticLocatorStrategy strategy,
            String attribute,
            String value,
            double confidence) {
        if (present(value)) {
            proposals.add(new Proposal(strategy, value, attributeLocator(attribute, value), confidence));
        }
    }

    private static By attributeLocator(String attribute, String value) {
        String escaped = value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\d ")
                .replace("\n", "\\a ");
        return By.cssSelector("[" + attribute + "=\"" + escaped + "\"]");
    }

    private static String xpathLiteral(String value) {
        if (!value.contains("'")) {
            return "'" + value + "'";
        }
        if (!value.contains("\"")) {
            return "\"" + value + "\"";
        }
        String[] parts = value.split("'", -1);
        StringBuilder result = new StringBuilder("concat(");
        for (int index = 0; index < parts.length; index++) {
            if (index > 0) {
                result.append(", \"'\", ");
            }
            result.append("'").append(parts[index]).append("'");
        }
        return result.append(")").toString();
    }

    private static boolean present(String value) {
        return value != null && !value.isBlank();
    }

    /**
     * One suggested locator with retained strategy confidence (FR-3).
     *
     * @param strategy engine FR-1 strategy
     * @param expression human-readable expression
     * @param locator Selenium locator
     * @param confidence base confidence in {@code [0,1]}
     */
    record Proposal(SemanticLocatorStrategy strategy, String expression, By locator, double confidence) {
        Proposal {
            strategy = Objects.requireNonNull(strategy, "strategy");
            expression = Objects.requireNonNullElse(expression, "");
            locator = Objects.requireNonNull(locator, "locator");
            if (confidence < 0.0 || confidence > 1.0) {
                throw new IllegalArgumentException("confidence must be in [0,1]");
            }
        }

        Map<String, Object> toEvidenceMap() {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("strategy", strategy.name());
            map.put("expression", expression);
            map.put("locator", locator.toString());
            map.put("confidence", confidence);
            return Map.copyOf(map);
        }
    }
}
