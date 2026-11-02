package com.shaft.gui.driver;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.AriaRole;
import com.shaft.gui.internal.locator.semantic.SemanticElementEvidence;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorResolution;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorResolver;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorStrategy;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Portable ROLE / ACCESSIBLE_NAME strategies for #5821.
 */
public class ShaftLocatorSemanticUnitTest {

    @Test(description = "ROLE round-trips to Selenium By and Playwright getByRole")
    public void roleRoundTripsSeleniumAndPlaywright() {
        ShaftLocator locator = ShaftLocator.role("button", "Sign in");

        Assert.assertEquals(locator.strategy(), ShaftLocator.Strategy.ROLE);
        Assert.assertEquals(locator.value(), "button");
        Assert.assertEquals(locator.secondaryValue(), "Sign in");

        By selenium = locator.toBy();
        Assert.assertTrue(selenium.toString().toLowerCase().contains("button")
                        || selenium.toString().contains("Sign in"),
                selenium.toString());

        Page page = mock(Page.class);
        Locator pw = mock(Locator.class);
        when(page.getByRole(eq(AriaRole.BUTTON), any(Page.GetByRoleOptions.class))).thenReturn(pw);

        Assert.assertSame(locator.toPlaywrightLocator(page), pw);
        verify(page).getByRole(eq(AriaRole.BUTTON), any(Page.GetByRoleOptions.class));

        Assert.expectThrows(UnsupportedOperationException.class, locator::toPlaywrightSelector);
    }

    @Test(description = "ACCESSIBLE_NAME round-trips to aria-label By and Playwright getByLabel")
    public void accessibleNameRoundTrips() {
        ShaftLocator locator = ShaftLocator.accessibleName("Email address");

        Assert.assertEquals(locator.strategy(), ShaftLocator.Strategy.ACCESSIBLE_NAME);
        Assert.assertTrue(locator.toBy().toString().contains("aria-label"));

        Page page = mock(Page.class);
        Locator pw = mock(Locator.class);
        when(page.getByLabel(eq("Email address"), any(Page.GetByLabelOptions.class))).thenReturn(pw);

        Assert.assertSame(locator.toPlaywrightLocator(page), pw);
        verify(page).getByLabel(eq("Email address"), any(Page.GetByLabelOptions.class));
    }

    @Test(description = "from(SemanticLocatorResolution) preserves ROLE strategy")
    public void fromResolutionPreservesRole() {
        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .role("link", "Docs", 1)
                .css("a.docs", 1)
                .build();
        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);

        Assert.assertEquals(resolution.strategy(), SemanticLocatorStrategy.ROLE);
        ShaftLocator portable = ShaftLocator.from(resolution);
        Assert.assertEquals(portable.strategy(), ShaftLocator.Strategy.ROLE);
        Assert.assertEquals(portable.value(), "link");
        Assert.assertEquals(portable.secondaryValue(), "Docs");
    }

    @Test(description = "ElementTarget Playwright chain uses getByRole for ROLE descendants")
    public void elementTargetChainsRoleOnPlaywrightParent() {
        Page page = mock(Page.class);
        Locator root = mock(Locator.class);
        Locator roleChild = mock(Locator.class);
        when(page.locator("#root")).thenReturn(root);
        when(root.getByRole(eq(AriaRole.BUTTON), any(Locator.GetByRoleOptions.class))).thenReturn(roleChild);

        ElementTarget target = ElementTarget.located(ShaftLocator.css("#root"))
                .descendant(ShaftLocator.role("button", "Save"));

        Assert.assertSame(target.toPlaywrightLocator(page), roleChild);
        verify(root).getByRole(eq(AriaRole.BUTTON), any(Locator.GetByRoleOptions.class));
    }

    @Test(description = "blank role/name factories fail loudly")
    public void blankFactoriesThrow() {
        Assert.expectThrows(IllegalArgumentException.class, () -> ShaftLocator.role(" ", "x"));
        Assert.expectThrows(IllegalArgumentException.class, () -> ShaftLocator.role("button", " "));
        Assert.expectThrows(IllegalArgumentException.class, () -> ShaftLocator.accessibleName(""));
    }

    @Test(description = "unmapped ARIA role still builds Selenium xpath; Playwright rejects unknown")
    public void customRoleUsesXpathOnSelenium() {
        ShaftLocator locator = ShaftLocator.role("switch", "Dark mode");
        Assert.assertTrue(locator.toBy().toString().contains("@role"));
        Assert.assertTrue(locator.toBy().toString().contains("Dark mode"));

        Page page = mock(Page.class);
        // Playwright AriaRole has SWITCH
        Locator pw = mock(Locator.class);
        when(page.getByRole(eq(AriaRole.SWITCH), any(Page.GetByRoleOptions.class))).thenReturn(pw);
        Assert.assertSame(locator.toPlaywrightLocator(page), pw);
    }
}
