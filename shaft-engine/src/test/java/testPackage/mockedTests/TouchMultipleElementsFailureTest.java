package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

/**
 * issue #4332: {@link com.shaft.gui.element.internal.ElementActionsHelper#identifyUniqueElement}
 * is a separate ambiguous-locator resolution path from {@link com.shaft.gui.element.internal.Actions#performAction}
 * (fixed for click/type/etc. by issue #4321 / PR #4326) -- exercised here via
 * {@code touch().swipeElementIntoView(By, By, SwipeDirection)}'s non-Appium branch (TouchActions.java:739/741),
 * which is one of the 8+ {@code identifyUniqueElement} call sites in TouchActions. This branch runs against a
 * plain headless desktop browser (no Appium/mobile driver needed), same as {@code TouchActionsTests}.
 */
public class TouchMultipleElementsFailureTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    // issue #4321 second-pass review pattern, reused here: a short identification timeout keeps the
    // last-resort-narrowing test (which must legitimately wait out the full window) fast, without
    // changing the behavior under test.
    private static final int SHORT_IDENTIFICATION_TIMEOUT_SECONDS = 6;
    String mockedHTML = TestPageServer.url("multipleElementsFixture.html");
    // 3 elements match, but only 1 is displayed+enabled (the other 2 are hidden/disabled) -- this
    // MUST auto-resolve to the single actionable element instead of throwing, unlike mockedHTML
    // above where all 3 matches are genuinely actionable.
    String oneVisibleHTML = TestPageServer.url("multipleElementsOneVisibleFixture.html");
    // #save is permanently disabled and #cancel is permanently enabled -- the narrowing must still
    // only apply as a LAST RESORT, after the full identification timeout is exhausted, never during
    // the retry loop itself.
    String disabledEnabledButtonsHTML = TestPageServer.url("multipleButtonsDisabledEnabledFixture.html");

    @Test
    public void swipeElementIntoViewThrowsOnGenuinelyAmbiguousLocator() {
        driver.get().browser().navigateToURL(mockedHTML);
        assertThrowsMultipleElementsFoundException(() ->
                driver.get().touch().swipeElementIntoView(null, By.xpath("//input"), TouchActions.SwipeDirection.DOWN));
    }

    @Test
    public void swipeElementIntoViewResolvesToTheOnlyDisplayedAndEnabledElementAmongMultipleMatches() {
        driver.get().browser().navigateToURL(oneVisibleHTML);
        // the locator itself is genuinely ambiguous (3 matches); the action must still succeed
        // because exactly one of them is displayed and enabled.
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//input")), 3);
        driver.get().touch().swipeElementIntoView(null, By.xpath("//input"), TouchActions.SwipeDirection.DOWN);
    }

    @Test
    public void swipeElementIntoViewResolvesToTheOnlyEnabledButtonOnlyAfterTheIdentificationTimeoutIsExhausted() {
        driver.get().browser().navigateToURL(disabledEnabledButtonsHTML);
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//button")), 2);

        long startMillis = System.currentTimeMillis();
        driver.get().touch().swipeElementIntoView(null, By.xpath("//button"), TouchActions.SwipeDirection.DOWN);
        long elapsedMillis = System.currentTimeMillis() - startMillis;

        // proves the narrowing was NOT applied on the first poll (which would resolve near-instantly)
        // -- it must only kick in as a last resort once the full identification timeout is spent.
        Assert.assertTrue(elapsedMillis >= 4_000,
                "Expected the disambiguation to wait out most of the " + SHORT_IDENTIFICATION_TIMEOUT_SECONDS
                        + "s identification timeout as a last resort, but it resolved after only "
                        + elapsedMillis + "ms.");
    }

    @BeforeMethod
    void beforeMethod() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(SHORT_IDENTIFICATION_TIMEOUT_SECONDS);
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
        Properties.clearForCurrentThread();
    }

    private static void assertThrowsMultipleElementsFoundException(Runnable action) {
        // issue #4341: ElementActionsHelper#failAction now throws RuntimeException (matching
        // Actions.performAction's reportBroken pipeline) instead of AssertionError, so this can
        // assert on RuntimeException just like MultipleElementsFailureTest does.
        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, action::run);
        Throwable cause = thrown;
        while (cause != null && !(cause instanceof MultipleElementsFoundException)) {
            cause = cause.getCause();
        }
        Assert.assertTrue(cause instanceof MultipleElementsFoundException,
                "Expected a MultipleElementsFoundException in the cause chain, but got: " + thrown);
    }
}
