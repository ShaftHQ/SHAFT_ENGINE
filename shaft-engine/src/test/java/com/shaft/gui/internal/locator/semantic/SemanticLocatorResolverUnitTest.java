package com.shaft.gui.internal.locator.semantic;

import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Vertical slice for issue #5457: semantic precedence, uniqueness, evidence/confidence,
 * and deterministic fallback — fixture-driven, no live browser.
 */
public class SemanticLocatorResolverUnitTest {

    @Test(description = "FR-1/SC-1: unique role+name beats structural css/xpath even when those are unique")
    public void uniqueRoleAndNamePreferredOverStructural() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .role("button", "Sign in", 1)
                .css("div.wrapper > form > button.btn-primary:nth-child(3)", 1)
                .xpath("/html/body/div[2]/form/button[1]", 1)
                .inspectionNotes("fixture: unique role+name survives DOM restructure")
                .build();

        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertTrue(resolution.locator().toString().toLowerCase().contains("button")
                        || resolution.locator().toString().contains("Sign in"),
                resolution.locator().toString());
        Assert.assertEquals(resolution.expression(), "button:Sign in");
        Assert.assertEquals(resolution.confidence(), 1.0);
        Assert.assertFalse(resolution.usedSemanticFallback());
        Assert.assertSame(resolution.evidence(), evidence);
    }

    @Test(description = "FR-2/SC-2: ambiguous role+name without scope fails visibly")
    public void ambiguousRoleWithoutScopeThrows() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .role("button", "OK", 3)
                .css(".ok", 1)
                .build();

        AmbiguousSemanticLocatorException thrown = Assert.expectThrows(
                AmbiguousSemanticLocatorException.class,
                () -> SemanticLocatorResolver.resolve(evidence));

        Assert.assertEquals(thrown.strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertEquals(thrown.matchCount(), 3);
        Assert.assertTrue(thrown.getMessage().contains("Ambiguous"));
    }

    @Test(description = "FR-2: ambiguous signal accepted when scopedIndex provided")
    public void ambiguousRoleAcceptedWithScopedIndex() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .role("button", "OK", 2)
                .scopedIndex(1)
                .inspectionNotes("fixture: duplicate OK buttons, pick second")
                .build();

        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertTrue(resolution.usedExplicitScope());
        Assert.assertTrue(resolution.confidence() < 1.0);
        Assert.assertTrue(resolution.locator().toString().contains("[2]")
                        || resolution.locator().toString().contains("OK"),
                resolution.locator().toString());
    }

    @Test(description = "FR-1: accessible name used when role missing")
    public void accessibleNameWhenRoleMissing() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .accessibleName("Email address", 1)
                .xpath("//div[3]/input", 1)
                .build();

        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.ACCESSIBLE_NAME);
        Assert.assertTrue(resolution.locator().toString().contains("aria-label"));
        Assert.assertEquals(resolution.confidence(), 0.92);
    }

    @Test(description = "FR-1: label precedes text and structural fallbacks")
    public void labelPreferredOverTextAndXpath() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .label("Password", 1)
                .visibleText("Password", 2)
                .xpath("//input[@type='password']", 1)
                .build();

        // visibleText is ambiguous (2) but label is unique — must pick LABEL, not throw on TEXT
        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.LABEL);
    }

    @Test(description = "FR-3: resolution retains inspection notes and match count")
    public void resolutionPreservesEvidenceAndConfidence() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .testId("login-submit", 1)
                .inspectionNotes("aria snapshot node #4; confidence from uniqueness")
                .build();

        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.TEST_ID);
        Assert.assertEquals(resolution.matchCount(), 1);
        Assert.assertEquals(resolution.evidence().inspectionNotes(),
                "aria snapshot node #4; confidence from uniqueness");
        Assert.assertEquals(resolution.confidence(), SemanticLocatorStrategy.TEST_ID.baseConfidence());
    }

    @Test(description = "FR-4/SC-3: no semantic signals → deterministic xpath/css fallback")
    public void inaccessibleFixtureFallsBackDeterministically() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .css("#anon-widget > span", 1)
                .xpath("//div[@id='anon-widget']/span", 1)
                .inspectionNotes("fixture: canvas-adjacent, no role/name/label/testid")
                .build();

        Assert.assertFalse(evidence.hasSemanticSignal());

        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        // CSS precedes XPATH in the enum order
        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.CSS);
        Assert.assertTrue(resolution.usedSemanticFallback());
        Assert.assertTrue(resolution.confidence() <= 0.40);
        Assert.assertEquals(resolution.locator(), By.cssSelector("#anon-widget > span"));
    }

    @Test(description = "FR-4: empty evidence fails loudly")
    public void emptyEvidenceThrows() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder().build();

        Assert.expectThrows(IllegalArgumentException.class,
                () -> SemanticLocatorResolver.resolve(evidence));
    }

    @Test(description = "SC-1: same role+name evidence yields same locator after structural fields change")
    public void semanticChoiceSurvivesStructuralRestructure() {
        SemanticElementEvidence before = SemanticElementEvidence.builder()
                .role("link", "Docs", 1)
                .xpath("/html/body/nav/ul/li[2]/a", 1)
                .build();
        SemanticElementEvidence after = SemanticElementEvidence.builder()
                .role("link", "Docs", 1)
                .xpath("/html/body/header/div/a[3]", 1)
                .css("header a.docs-link", 1)
                .build();

        SemanticLocatorResolution r1 = SemanticLocatorResolver.resolve(before);
        SemanticLocatorResolution r2 = SemanticLocatorResolver.resolve(after);

        Assert.assertEquals(r1.strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertEquals(r2.strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertEquals(r1.expression(), r2.expression());
        Assert.assertEquals(r1.locator().toString(), r2.locator().toString());
    }
}
