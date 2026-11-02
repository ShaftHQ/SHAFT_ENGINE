package com.shaft.gui.internal.locator.semantic;

import com.shaft.gui.driver.ShaftLocator;
import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.AriaRole;
import java.util.Map;
import com.shaft.gui.internal.aria.AriaNode;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Live match-count inspection for #5817 — aria-forest fixtures plus mocked Selenium counts.
 */
public class SemanticMatchInspectorUnitTest {

    @Test(description = "unique role+name forest → evidence counts feed ROLE resolution")
    public void uniqueRoleNameForestFeedsResolver() {
        List<AriaNode> forest = List.of(
                new AriaNode("navigation", "", List.of(
                        new AriaNode("link", "Home", List.of()),
                        new AriaNode("link", "Docs", List.of())
                )),
                new AriaNode("button", "Sign in", List.of())
        );

        SemanticElementEvidence evidence = SemanticMatchInspector.fromAriaForest(forest, "button", "Sign in");
        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(evidence.matchCount(SemanticLocatorStrategy.ROLE), 1);
        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertFalse(resolution.usedSemanticFallback());
        Assert.assertEquals(ShaftLocator.from(resolution).strategy(), ShaftLocator.Strategy.ROLE);
    }

    @Test(description = "FR-2/SC-2: duplicate role+name in forest → visible ambiguity")
    public void ambiguousRoleNameInForestFailsVisibly() {
        List<AriaNode> forest = List.of(
                new AriaNode("button", "OK", List.of()),
                new AriaNode("button", "OK", List.of()),
                new AriaNode("button", "OK", List.of())
        );

        SemanticElementEvidence evidence = SemanticMatchInspector.fromAriaForest(forest, "button", "OK");
        Assert.assertEquals(evidence.matchCount(SemanticLocatorStrategy.ROLE), 3);

        AmbiguousSemanticLocatorException thrown = Assert.expectThrows(
                AmbiguousSemanticLocatorException.class,
                () -> SemanticLocatorResolver.resolve(evidence));
        Assert.assertEquals(thrown.matchCount(), 3);
    }

    @Test(description = "localized fixture: French accessible name still unique ROLE")
    public void localizedAccessibleNameStillResolvesRole() {
        List<AriaNode> forest = List.of(
                new AriaNode("button", "Connexion", List.of()),
                new AriaNode("link", "Accueil", List.of())
        );

        SemanticElementEvidence evidence = SemanticMatchInspector.fromAriaForest(forest, "button", "Connexion");
        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertEquals(resolution.expression(), "button:Connexion");
        Assert.assertEquals(resolution.confidence(), 1.0);
    }

    @Test(description = "FR-4/SC-3: inaccessible forest uses structural fallback only")
    public void inaccessibleForestFallsBackStructurally() {
        List<AriaNode> forest = List.of(
                new AriaNode("generic", "", List.of(new AriaNode("generic", "", List.of())))
        );

        SemanticElementEvidence evidence = SemanticMatchInspector.inaccessibleWithStructuralFallback(
                forest,
                "#anon-widget > span",
                1,
                "//div[@id='anon-widget']/span",
                1,
                "fixture: canvas-adjacent, no role/name");

        Assert.assertFalse(evidence.hasSemanticSignal());
        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);
        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.CSS);
        Assert.assertTrue(resolution.usedSemanticFallback());
        Assert.assertTrue(resolution.confidence() <= 0.40);
    }

    @Test(description = "missing target in forest fails loudly")
    public void missingTargetThrows() {
        List<AriaNode> forest = List.of(new AriaNode("button", "Save", List.of()));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> SemanticMatchInspector.fromAriaForest(forest, "button", "Delete"));
    }

    @Test(description = "Selenium inspection counts ROLE uniqueness via portable ShaftLocator")
    public void seleniumInspectionCountsRoleMatches() {
        SearchContext driver = mock(SearchContext.class);
        WebElement target = mock(WebElement.class);
        By seed = By.id("login");

        when(driver.findElements(seed)).thenReturn(List.of(target));
        when(target.getTagName()).thenReturn("button");
        when(target.getAccessibleName()).thenReturn("Sign in");
        when(target.getDomAttribute("role")).thenReturn("button");
        when(target.getDomAttribute("aria-label")).thenReturn("Sign in");
        when(target.getDomAttribute("id")).thenReturn("login");
        when(target.getDomAttribute("name")).thenReturn(null);
        when(target.getDomAttribute("data-testid")).thenReturn(null);
        when(target.getDomAttribute("data-test")).thenReturn(null);
        when(target.getDomAttribute("data-qa")).thenReturn(null);
        when(target.getDomAttribute("title")).thenReturn(null);
        when(target.getDomAttribute("href")).thenReturn(null);
        when(target.getDomAttribute("type")).thenReturn(null);
        when(target.getText()).thenReturn("Sign in");
        when(target.getAttribute("role")).thenReturn("button");
        when(target.getAttribute("aria-label")).thenReturn("Sign in");
        when(target.getAttribute("id")).thenReturn("login");

        By roleBy = ShaftLocator.role("button", "Sign in").toBy();
        when(driver.findElements(argThat(by -> by != null && by.toString().equals(roleBy.toString()))))
                .thenReturn(List.of(target));
        when(driver.findElements(argThat(by -> by != null
                && by.toString().contains("aria-label")
                && by.toString().contains("Sign in"))))
                .thenReturn(List.of(target));
        when(driver.findElements(argThat(by -> by != null
                && by.toString().contains("normalize-space")
                && by.toString().contains("Sign in")
                && !by.toString().equals(roleBy.toString()))))
                .thenReturn(List.of(target));
        when(driver.findElements(argThat(by -> by != null && by.toString().contains("#login"))))
                .thenReturn(List.of(target));
        when(driver.findElements(argThat(by -> by != null && by.toString().contains("@id="))))
                .thenReturn(List.of(target));
        when(driver.findElements(argThat(by -> by != null && by.toString().contains("[id="))))
                .thenReturn(List.of(target));

        SemanticElementEvidence evidence = SemanticMatchInspector.fromSelenium(driver, seed);
        Assert.assertEquals(evidence.role(), "button");
        Assert.assertEquals(evidence.accessibleName(), "Sign in");
        Assert.assertTrue(evidence.matchCount(SemanticLocatorStrategy.ROLE) >= 1);

        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);
        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.ROLE);
    }

    @Test(description = "non-unique Selenium seed fails visibly (no silent pick)")
    public void nonUniqueSeleniumSeedThrows() {
        SearchContext driver = mock(SearchContext.class);
        By seed = By.cssSelector(".dup");
        when(driver.findElements(seed)).thenReturn(List.of(mock(WebElement.class), mock(WebElement.class)));

        Assert.expectThrows(IllegalArgumentException.class,
                () -> SemanticMatchInspector.fromSelenium(driver, seed));
    }

    @Test(description = "#5835: Selenium path resolves <label for> live association")
    public void seleniumResolvesLabelForAssociation() {
        SearchContext driver = mock(SearchContext.class);
        WebElement target = mock(WebElement.class);
        WebElement labelEl = mock(WebElement.class);
        By seed = By.id("email");

        when(driver.findElements(seed)).thenReturn(List.of(target));
        when(target.getTagName()).thenReturn("input");
        when(target.getAccessibleName()).thenReturn("Email");
        when(target.getDomAttribute("role")).thenReturn(null);
        when(target.getDomAttribute("aria-label")).thenReturn(null);
        when(target.getDomAttribute("id")).thenReturn("email");
        when(target.getDomAttribute("name")).thenReturn(null);
        when(target.getDomAttribute("data-testid")).thenReturn(null);
        when(target.getDomAttribute("data-test")).thenReturn(null);
        when(target.getDomAttribute("data-qa")).thenReturn(null);
        when(target.getDomAttribute("title")).thenReturn(null);
        when(target.getDomAttribute("href")).thenReturn(null);
        when(target.getDomAttribute("type")).thenReturn("email");
        when(target.getText()).thenReturn("");
        when(target.getAttribute("role")).thenReturn(null);
        when(target.getAttribute("aria-label")).thenReturn(null);
        when(target.getAttribute("id")).thenReturn("email");
        when(labelEl.getText()).thenReturn("Email address");

        when(driver.findElements(argThat(by -> by != null
                && by.toString().contains("label[for")
                && by.toString().contains("email"))))
                .thenReturn(List.of(labelEl));

        when(driver.findElements(argThat(by -> by != null
                && !by.equals(seed)
                && !(by.toString().contains("label[for") && by.toString().contains("email")))))
                .thenReturn(List.of(target));

        SemanticElementEvidence evidence = SemanticMatchInspector.fromSelenium(driver, seed);
        Assert.assertEquals(evidence.label(), "Email address");
        Assert.assertTrue(evidence.hasSemanticSignal());
    }

    @Test(description = "#5835: dedicated fromPlaywright mock unit test")
    public void fromPlaywrightMockCountsRoleMatches() {
        Page page = mock(Page.class);
        Locator target = mock(Locator.class);
        Locator counted = mock(Locator.class);
        ShaftLocator seed = ShaftLocator.role("button", "Save");

        when(page.getByRole(eq(AriaRole.BUTTON), any(Page.GetByRoleOptions.class))).thenReturn(target);
        when(target.count()).thenReturn(1);
        when(target.evaluate(any(String.class))).thenReturn(Map.of(
                "role", "button",
                "accessibleName", "Save",
                "label", "Save",
                "visibleText", "Save",
                "testId", "",
                "id", "save-btn",
                "name", "",
                "css", "#save-btn",
                "xpath", "//*[@id='save-btn']"
        ));
        when(counted.count()).thenReturn(1);
        when(page.getByLabel(any(String.class), any(Page.GetByLabelOptions.class))).thenReturn(counted);
        when(page.locator(any(String.class))).thenReturn(counted);

        SemanticElementEvidence evidence = SemanticMatchInspector.fromPlaywright(page, seed);
        Assert.assertEquals(evidence.role(), "button");
        Assert.assertEquals(evidence.accessibleName(), "Save");
        Assert.assertEquals(evidence.matchCount(SemanticLocatorStrategy.ROLE), 1);
        Assert.assertTrue(evidence.inspectionNotes().contains("playwright"));
    }
}
