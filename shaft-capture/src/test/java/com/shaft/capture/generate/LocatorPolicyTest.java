package com.shaft.capture.generate;

import com.shaft.capture.guardrail.GeneratedCodeGuardrails;
import com.shaft.capture.guardrail.GuardrailViolation;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.gui.driver.ShaftLocator;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #4271: the unified deterministic locator policy. One function maps captured evidence to a
 * {@link LocatorPolicy.LocatorPlan}; the generation gate is "a plan exists" and the renderer is
 * "the plan's source text", so the two can never disagree the way issue #4264 documents.
 */
class LocatorPolicyTest {
    @Test
    void uniqueStableIdPlansAsTierOneRenderedThroughTheShaftLocatorBuilder() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ID, "username", 1, true, true,
                Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE));

        Optional<LocatorPolicy.LocatorPlan> plan = LocatorPolicy.plan(target(candidate), candidate);

        assertEquals(LocatorPolicy.Tier.UNIQUE_ID, plan.orElseThrow().tier());
        assertEquals("SHAFT.GUI.Locator.hasAnyTagName().hasId(\"username\").build()",
                plan.orElseThrow().source());
        assertEquals("By.xpath: //*[@id=\"username\"]", plan.orElseThrow().runtimeLocator().toString());
    }

    /**
     * Issue #4271 review finding 1: tier 1's whole premise is that only a human-authored id reaches
     * it. That premise rested on the recorder's {@code dynamic()} check
     * ({@code shaft-capture-recorder.js:17-19}), which tests only for an 8+ digit run or a UUID --
     * so every mainstream framework auto-id below is classified {@code stable=true} and would have
     * been promoted over a live-verified ARIA role. Each of these must be refused by tier 1.
     *
     * <p>The check lives here, in Java, rather than in the recorder: it is unit-testable (the repo
     * has no JS test harness), and it protects every producer of {@link LocatorCandidate} -- the
     * shaft-mcp adapters construct candidates directly and never run the recorder's {@code dynamic()}
     * at all, so a recorder-only fix would leave exactly the surfaces #4272 is about unprotected.
     */
    @Test
    void frameworkGeneratedIdsAreNeverAdmittedToTheIdTier() {
        for (String generatedId : List.of(
                ":r1:",                              // React 18 useId
                ":R1mcq:",                           // React useId, nested variant
                "radix-:R1mcq:",                     // Radix UI
                "mat-input-3",                       // Angular Material
                "cdk-overlay-0",                     // Angular CDK
                "ng-select-12",                      // ng-select
                "react-select-2-input",              // react-select
                "ember1234",                         // Ember
                "j_idt42",                           // JSF
                "ctl00_ContentPlaceHolder1_txtUser", // ASP.NET WebForms
                "sc-bdVaJa",                         // styled-components
                "css-1a2b3c",                        // emotion
                "jss42",                             // JSS / Material-UI v4
                "emotion-css-7f8g9h",                // emotion, prefixed
                "id5",                               // Apache Wicket
                "id1b",                              // Apache Wicket, hex counter
                "yui_3_18_1_1_1",                    // YUI
                "GWT_uid_9",                         // GWT
                "u_0_3",                             // legacy Facebook / React
                "select2-country-container",         // select2
                "css-1s2u09g-control",               // emotion / react-select v5, suffixed
                "css-b62m3t-container")) {           // emotion / react-select v5, suffixed
            LocatorCandidate candidate = new LocatorCandidate(
                    LocatorCandidate.LocatorStrategy.ID, generatedId, 1, true, true, Set.of());

            assertEquals(Optional.empty(), LocatorPolicy.plan(target(candidate), candidate),
                    "framework-generated id must never win tier 1: " + generatedId);
        }
    }

    /**
     * Issue #4273: shaft-mcp's {@code PlaywrightService} computes real live-DOM evidence (a real
     * uniqueness count plus a real stable signal) at the moment a Playwright action executes,
     * instead of the {@code McpPlaywrightCaptureAdapter} fabricating {@code stable=true} for every
     * recorded id. Reusing this exact heuristic instead of a second copy is only possible if it is
     * public; this pins that contract directly, independent of {@link LocatorPolicy#plan}.
     */
    @Test
    void looksAutoGeneratedIsPubliclyReusableByOtherShaftSurfaces() {
        assertTrue(LocatorPolicy.looksAutoGenerated("mat-input-3"));
        assertEquals(false, LocatorPolicy.looksAutoGenerated("username"));
    }

    /**
     * The counterpart to the case above: hardening tier 1 must not swallow ordinary human-authored
     * ids. Detection is necessarily heuristic and errs toward refusing, but these must still pass.
     */
    @Test
    void genuinelyAuthoredIdsAreStillAdmittedToTheIdTier() {
        for (String authoredId : List.of(
                "username", "login-btn", "login-button", "custom-button", "submit", "x",
                "email_address", "checkout-form", "primaryNavigation",
                // Review round 2: the ^sc-/^css- prefixes over-refused these plausible authored ids,
                // so those two rules are now narrowed to the hash-shaped forms the libraries emit.
                "sc-header", "css-container", "search-results")) {
            LocatorCandidate candidate = new LocatorCandidate(
                    LocatorCandidate.LocatorStrategy.ID, authoredId, 1, true, true, Set.of());

            assertEquals(LocatorPolicy.Tier.UNIQUE_ID,
                    LocatorPolicy.plan(target(candidate), candidate).orElseThrow().tier(),
                    "authored id must still reach tier 1: " + authoredId);
        }
    }

    @Test
    void verifiedRoleWithAMappableAriaRolePlansAsTierTwoThroughHasRole() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "button:Log in", 1, true, true,
                Set.of(), "//button[normalize-space(.)=\"Log in\"]", true);

        Optional<LocatorPolicy.LocatorPlan> plan =
                LocatorPolicy.plan(target("button", "button", "Log in", candidate), candidate);

        assertEquals(LocatorPolicy.Tier.VERIFIED_ROLE, plan.orElseThrow().tier());
        assertEquals("SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasNormalizedText(\"Log in\").build()",
                plan.orElseThrow().source());
    }

    /**
     * Issue #4271: {@code CaptureGenerator.isLadderEligible} admitted any {@code roleXpathVerified}
     * ROLE candidate at rung 1, but the renderer additionally required the raw role string to map to
     * one of the 14 {@code Role} constants and silently fell through when it did not. That divergence
     * is a second instance of issue #4264's class, so the mapping check now lives inside the tier
     * decision itself: an unmappable role can never be admitted as a role plan.
     */
    @Test
    void verifiedRoleWithNoMappableAriaRoleIsNeverATierTwoPlan() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "tab:Settings", 1, true, true,
                Set.of(), "", true);

        Optional<LocatorPolicy.LocatorPlan> plan =
                LocatorPolicy.plan(target("div", "tab", "Settings", candidate), candidate);

        assertEquals(Optional.empty(), plan);
    }

    /**
     * Issue #4239 P1.4a, preserved: input/textarea/select can never carry their own text, so pairing
     * a verified role with {@code hasNormalizedText} would be unsatisfiable at replay.
     */
    @Test
    void verifiedRoleOnATextlessFormControlOmitsTheTextPredicate() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "textbox:Username", 1, true, true,
                Set.of(), "//input[@id=\"username\"]", true);

        Optional<LocatorPolicy.LocatorPlan> plan =
                LocatorPolicy.plan(target("input", "textbox", "Username", candidate), candidate);

        assertEquals("SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()", plan.orElseThrow().source());
    }

    @Test
    void anyStrategyCarryingASelfVerifiedReplayXpathPlansAsTierThreeNativeXpath() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.LABEL, "Username", 1, true, true,
                Set.of(), "//label[normalize-space(.)=\"Username\"]/input");

        Optional<LocatorPolicy.LocatorPlan> plan = LocatorPolicy.plan(target(candidate), candidate);

        assertEquals(LocatorPolicy.Tier.VERIFIED_XPATH, plan.orElseThrow().tier());
        assertEquals("By.xpath(\"//label[normalize-space(.)=\\\"Username\\\"]/input\")",
                plan.orElseThrow().source());
    }

    /**
     * Issue #4264, fixed by construction: a non-unique ID candidate that legitimately earned
     * eligibility through a self-verified {@code replayXpath} must render as that XPath, never as the
     * literal {@code SHAFT.GUI.Locator.id(...)} form the {@code NON_ARIA_LOCATOR} guardrail bans.
     */
    @Test
    void nonUniqueIdWithAVerifiedReplayXpathRendersAsNativeXpathNotTheBannedIdBuilderCall() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ID, "row-3-delete", 3, true, true,
                Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE), "//*[@id=\"row-3-delete\"]");

        Optional<LocatorPolicy.LocatorPlan> plan = LocatorPolicy.plan(target(candidate), candidate);

        assertEquals(LocatorPolicy.Tier.VERIFIED_XPATH, plan.orElseThrow().tier());
        assertEquals("By.xpath(\"//*[@id=\\\"row-3-delete\\\"]\")", plan.orElseThrow().source());
    }

    /**
     * Issue #4271: the {@code ABSOLUTE_XPATH} guardrail becomes unreachable through the policy rather
     * than merely being caught after the fact.
     */
    @Test
    void anAbsoluteReplayXpathIsNeverEligible() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.LABEL, "Username", 1, true, true,
                Set.of(), "/html/body/div[2]/form/input");

        assertEquals(Optional.empty(), LocatorPolicy.plan(target(candidate), candidate));
    }

    /**
     * {@code LocatorBuilder.hasId} interpolates the raw value into {@code [@id="..."]} without
     * escaping, so an id carrying a double quote would build a malformed XPath. It is excluded from
     * the id tier deterministically rather than emitted broken.
     */
    @Test
    void anIdContainingADoubleQuoteIsNotEligibleForTheIdTier() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ID, "wei\"rd", 1, true, true, Set.of());

        assertEquals(Optional.empty(), LocatorPolicy.plan(target(candidate), candidate));
    }

    @Test
    void evidenceWithNoVerifiedSignalHasNoPlanAtAll() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.CSS, "form > div:nth-child(2) > input", 1, true, false,
                Set.of(LocatorCandidate.LocatorSignal.GENERATED, LocatorCandidate.LocatorSignal.POSITIONAL));

        assertEquals(Optional.empty(), LocatorPolicy.plan(target(candidate), candidate));
    }

    /**
     * Issue #4271 acceptance (d)/(5): the gate and the renderer are the same function, so the only
     * way a policy-violating locator could still be emitted is if a plan's own rendered source
     * tripped a guardrail. Every tier is checked against the real {@link GeneratedCodeGuardrails}
     * ruleset, making {@code NON_ARIA_LOCATOR}, {@code SHAFT_LOCATOR_XPATH}, {@code SMART_LOCATOR}
     * and {@code ABSOLUTE_XPATH} unreachable through this policy rather than merely caught after it.
     */
    @Test
    void everyTierRendersSourceThatClearsEveryErrorSeverityGuardrail() {
        for (Map.Entry<LocatorPolicy.Tier, LocatorPolicy.LocatorPlan> entry : everyTier().entrySet()) {
            String statement = "driver.element().click(" + entry.getValue().source() + ");";
            List<GuardrailViolation> errors = GeneratedCodeGuardrails.check(statement).violations().stream()
                    .filter(violation -> "ERROR".equals(violation.severity()))
                    .toList();
            assertTrue(errors.isEmpty(), entry.getKey() + " rendered " + statement + " -> " + errors);
        }
    }

    /**
     * Issue #4271 requirement 2, proven by execution rather than by reading: every tier resolves to a
     * single backend-neutral XPath, so the same plan drives a Selenium {@code By} and a Playwright
     * selector identically. {@link ShaftLocator#from(By)} is the exact conversion the Playwright
     * {@code ElementActions.click(By)} overload performs, and it throws on any strategy it cannot
     * carry across -- so a passing run here is a real portability proof, not an assumption.
     */
    @Test
    void everyTierResolvesToABackendNeutralXpathUsableByBothSeleniumAndPlaywright() {
        for (Map.Entry<LocatorPolicy.Tier, LocatorPolicy.LocatorPlan> entry : everyTier().entrySet()) {
            ShaftLocator portable = ShaftLocator.from(entry.getValue().runtimeLocator());

            String selector = portable.toPlaywrightSelector();

            assertEquals(ShaftLocator.Strategy.XPATH, portable.strategy(), entry.getKey().toString());
            assertTrue(selector.startsWith("xpath="), entry.getKey() + " -> " + selector);
            assertTrue(selector.contains("//"), entry.getKey() + " must stay relative -> " + selector);
        }
    }

    /**
     * Every tier is exercised by the two property tests above; this map fails loudly if a tier is
     * ever added without extending them.
     */
    private static Map<LocatorPolicy.Tier, LocatorPolicy.LocatorPlan> everyTier() {
        Map<LocatorPolicy.Tier, LocatorPolicy.LocatorPlan> plans = new java.util.EnumMap<>(LocatorPolicy.Tier.class);
        for (LocatorCandidate candidate : List.of(
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.ID, "username", 1, true, true, Set.of()),
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE, "textbox:Username", 1, true, true,
                        Set.of(), "", true),
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.LABEL, "Username", 1, true, true,
                        Set.of(), "//label[normalize-space(.)=\"Username\"]/input"))) {
            LocatorPolicy.LocatorPlan plan = LocatorPolicy.plan(target(candidate), candidate).orElseThrow();
            plans.put(plan.tier(), plan);
        }
        assertEquals(Set.of(LocatorPolicy.Tier.values()), plans.keySet(), "a tier is untested");
        return plans;
    }

    private static ElementSnapshot target(LocatorCandidate... candidates) {
        return target("input", "textbox", "Username", candidates);
    }

    private static ElementSnapshot target(
            String tagName,
            String role,
            String accessibleName,
            LocatorCandidate... candidates) {
        return new ElementSnapshot(
                "username",
                tagName,
                role,
                accessibleName,
                accessibleName,
                Map.of("id", "username"),
                List.of(candidates),
                true,
                true,
                false);
    }
}
