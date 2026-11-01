package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import java.util.function.BooleanSupplier;
import com.microsoft.playwright.options.BoundingBox;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.assertions.LocatorAssertions;
import com.microsoft.playwright.assertions.PlaywrightAssertions;
import com.shaft.gui.driver.ElementRectangle;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsHelper;
import org.mockito.MockedStatic;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Unit coverage for Playwright element validation builders (issue #7077).
 */
public class PlaywrightElementValidationsBuilderCoverageUnitTest {

    @BeforeMethod(alwaysRun = true)
    public void softVisualEvidence() {
        com.shaft.driver.SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        com.shaft.driver.SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
    }

    @AfterMethod(alwaysRun = true)
    public void resetVerificationState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void nativeBuildersShouldReturnFluentSeams() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        PlaywrightElementValidationsBuilder builder = new PlaywrightElementValidationsBuilder(
                ValidationEnums.ValidationCategory.SOFT_ASSERT, session, locator, "cov");

        Assert.assertNotNull(builder.elementCount());
        Assert.assertNotNull(builder.elementRectangle());
        Assert.assertNotNull(builder.elementAccessibleName());
        Assert.assertNotNull(builder.elementRole());
        Assert.assertNotNull(builder.attribute("id"));
        Assert.assertNotNull(builder.domAttribute("data-x"));
        Assert.assertNotNull(builder.domProperty("value"));
        Assert.assertNotNull(builder.property("value"));
        Assert.assertNotNull(builder.text());
        Assert.assertNotNull(builder.textTrimmed());
        Assert.assertNotNull(builder.cssProperty("color"));
        Assert.assertTrue(builder.elementCount() instanceof NativeValidationsBuilder);
    }

    @Test
    public void positiveStateAssertionsShouldFinishThroughPlaywrightAssertMocks() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions assertions = mock(LocatorAssertions.class);
        when(locator.count()).thenReturn(1);
        when(locator.isVisible()).thenReturn(true);
        when(locator.isEnabled()).thenReturn(true);
        when(locator.isChecked()).thenReturn(true);
        when(locator.getAttribute("selected")).thenReturn("true");

        try (MockedStatic<PlaywrightAssertions> mocked = mockStatic(PlaywrightAssertions.class)) {
            mocked.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(assertions);
            PlaywrightElementValidationsBuilder builder = new PlaywrightElementValidationsBuilder(
                    ValidationEnums.ValidationCategory.SOFT_ASSERT, session, locator, "cov-state");
            Assert.assertNotNull(builder.isVisible());
            Assert.assertNotNull(builder.isEnabled());
            Assert.assertNotNull(builder.isChecked());
            Assert.assertNotNull(builder.isSelected());
            Assert.assertNotNull(builder.exists());
        }
    }



    @Test
    public void attributeEqualsShouldEvaluateThroughPlaywrightAssertMocks() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions assertions = mock(LocatorAssertions.class);
        when(session.page()).thenReturn(page);
        when(locator.page()).thenReturn(page);
        when(locator.count()).thenReturn(1);
        when(locator.getAttribute("id")).thenReturn("save");
        when(locator.textContent()).thenReturn("Saved");
        when(locator.ariaSnapshot()).thenReturn("- button \"Saved\"");
        BoundingBox box = new BoundingBox();
        box.x = 1;
        box.y = 2;
        box.width = 3;
        box.height = 4;
        when(locator.boundingBox()).thenReturn(box);
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            return condition.getAsBoolean();
        }).when(page).waitForCondition(any(BooleanSupplier.class));

        try (MockedStatic<PlaywrightAssertions> mocked = mockStatic(PlaywrightAssertions.class)) {
            mocked.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(assertions);
            PlaywrightElementValidationsBuilder builder = new PlaywrightElementValidationsBuilder(
                    ValidationEnums.ValidationCategory.SOFT_ASSERT, session, locator, "cov-eq");
            Assert.assertNotNull(builder.attribute("id").isEqualTo("save"));
            Assert.assertNotNull(builder.text().isEqualTo("Saved"));
            Assert.assertNotNull(builder.elementCount().isEqualTo(1));
            Assert.assertNotNull(builder.elementRole().isEqualTo("button"));
            Assert.assertNotNull(builder.elementRectangle().isEqualTo(new ElementRectangle(1, 2, 3, 4)));
            Assert.assertNotNull(builder.elementAccessibleName().isEqualTo("Saved"));
        }
    }

}
