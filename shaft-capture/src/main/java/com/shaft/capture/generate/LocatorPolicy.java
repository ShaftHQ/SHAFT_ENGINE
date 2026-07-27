package com.shaft.capture.generate;

import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.gui.internal.locator.Locator;
import com.shaft.gui.internal.locator.Role;
import org.openqa.selenium.By;

import java.util.Locale;
import java.util.Optional;
import java.util.Set;

/**
 * Issue #4271: the single deterministic locator-selection policy for every SHAFT surface that
 * identifies an element from captured evidence.
 */
public final class LocatorPolicy {
    private static final Set<String> TAGS_WITHOUT_OWN_TEXT = Set.of("input", "textarea", "select");

    private LocatorPolicy() {
    }

    /**
     * Plans how {@code candidate} would be emitted for {@code target}.
     *
     * @param target the captured element
     * @param candidate one piece of captured locator evidence
     * @return the plan, or empty when the evidence is not trustworthy enough to emit
     */
    public static Optional<LocatorPlan> plan(ElementSnapshot target, LocatorCandidate candidate) {
        return idPlan(candidate)
                .or(() -> rolePlan(target, candidate))
                .or(() -> xpathPlan(candidate));
    }

    private static Optional<LocatorPlan> idPlan(LocatorCandidate candidate) {
        String id = candidate.expression();
        if (candidate.strategy() != LocatorCandidate.LocatorStrategy.ID
                || candidate.uniquenessCount() != 1
                || !candidate.stable()
                // LocatorBuilder.hasId interpolates the raw value into [@id="..."] unescaped, so a
                // value carrying a double quote would build a malformed XPath. Excluded here rather
                // than emitted broken; such an element still reaches the verified-XPath tier.
                || id.indexOf('"') >= 0) {
            return Optional.empty();
        }
        return Optional.of(new LocatorPlan(
                Tier.UNIQUE_ID,
                "SHAFT.GUI.Locator.hasAnyTagName().hasId(\"" + javaString(id) + "\").build()",
                Locator.hasAnyTagName().hasId(id).build()));
    }

    private static Optional<LocatorPlan> xpathPlan(LocatorCandidate candidate) {
        String xpath = candidate.replayXpath();
        if (xpath.isBlank() || isAbsolute(xpath)) {
            return Optional.empty();
        }
        return Optional.of(new LocatorPlan(
                Tier.VERIFIED_XPATH,
                "By.xpath(\"" + javaString(xpath) + "\")",
                By.xpath(xpath)));
    }

    /**
     * Mirrors {@code GeneratedCodeGuardrails}' {@code ABSOLUTE_XPATH} rule, so that guardrail
     * becomes unreachable through this policy instead of merely catching its output.
     */
    private static boolean isAbsolute(String xpath) {
        String trimmed = xpath.trim();
        return (trimmed.startsWith("/") && !trimmed.startsWith("//")) || trimmed.startsWith("(/");
    }

    private static Optional<LocatorPlan> rolePlan(ElementSnapshot target, LocatorCandidate candidate) {
        if (candidate.strategy() != LocatorCandidate.LocatorStrategy.ROLE || !candidate.roleXpathVerified()) {
            return Optional.empty();
        }
        Role role = ariaRole(target.role());
        if (role == null) {
            return Optional.empty();
        }
        String name = semanticName(target, candidate);
        String prefix = "SHAFT.GUI.Locator.hasRole(Role." + role.name() + ")";
        if (name.isBlank() || !tagCanCarryOwnText(target.tagName())) {
            return Optional.of(new LocatorPlan(
                    Tier.VERIFIED_ROLE, prefix + ".build()", Locator.hasRole(role).build()));
        }
        return Optional.of(new LocatorPlan(
                Tier.VERIFIED_ROLE,
                prefix + ".hasNormalizedText(\"" + javaString(name) + "\").build()",
                Locator.hasRole(role).hasNormalizedText(name).build()));
    }

    /**
     * Issue #4239 P1.4a: input/textarea/select can never carry their own visible text, so pairing a
     * verified role with {@code hasNormalizedText} compiles but is unsatisfiable at replay.
     */
    private static boolean tagCanCarryOwnText(String tagName) {
        return !TAGS_WITHOUT_OWN_TEXT.contains(tagName.toLowerCase(Locale.ROOT));
    }

    private static String semanticName(ElementSnapshot target, LocatorCandidate candidate) {
        if (candidate.strategy() == LocatorCandidate.LocatorStrategy.ROLE
                && candidate.expression().contains(":")) {
            return candidate.expression().substring(candidate.expression().indexOf(':') + 1);
        }
        String name = target.accessibleName().isBlank() ? target.label() : target.accessibleName();
        return name.isBlank() ? candidate.expression() : name;
    }

    /**
     * Maps a raw ARIA role string to one of the 14 {@link Role} constants, case-insensitively,
     * including the known ARIA-name vs. enum-name mismatches. Returns {@code null} when the role has
     * no equivalent -- which is precisely why the mapping must be part of the tier decision.
     *
     * @param rawRole the captured role string
     * @return the mapped role, or {@code null} when unmappable
     */
    static Role ariaRole(String rawRole) {
        return switch (rawRole == null ? "" : rawRole.trim().toLowerCase(Locale.ROOT)) {
            case "button" -> Role.BUTTON;
            case "link" -> Role.LINK;
            case "textbox" -> Role.TEXTBOX;
            case "checkbox" -> Role.CHECKBOX;
            case "radio" -> Role.RADIO;
            case "combobox" -> Role.COMBOBOX;
            case "heading" -> Role.HEADING;
            case "img", "image" -> Role.IMAGE;
            case "list" -> Role.LIST;
            case "listitem" -> Role.LISTITEM;
            case "table", "grid" -> Role.TABLE;
            case "row" -> Role.TABLE_ROW;
            case "cell", "gridcell" -> Role.TABLE_CELL;
            case "columnheader" -> Role.TABLE_COLUMNHEADER;
            default -> null;
        };
    }

    private static String javaString(String value) {
        return value == null ? "" : value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace("\t", "\\t");
    }

    /**
     * Emission tiers, most preferred first.
     */
    public enum Tier {
        /** A unique, stable, human-authored {@code id}. */
        UNIQUE_ID,
        /** A recorder-verified ARIA role that maps to a {@link Role} constant. */
        VERIFIED_ROLE,
        /** A native, relative, self-verified XPath the recorder resolved against the live DOM. */
        VERIFIED_XPATH
    }

    /**
     * How one candidate is emitted.
     *
     * @param tier the tier that admitted the candidate
     * @param source the Java source text to render
     * @param runtimeLocator the locator the rendered source resolves to at replay
     */
    public record LocatorPlan(Tier tier, String source, By runtimeLocator) {
    }
}
