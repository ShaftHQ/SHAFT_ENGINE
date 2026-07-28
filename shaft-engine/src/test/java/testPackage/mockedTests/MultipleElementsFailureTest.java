package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

public class MultipleElementsFailureTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    // issue #4321 second-pass review: a short identification timeout keeps this class's
    // last-resort-narrowing tests (which must legitimately wait out the full window) fast, without
    // changing the behavior under test.
    private static final int SHORT_IDENTIFICATION_TIMEOUT_SECONDS = 6;
    String mockedHTML = TestPageServer.url("multipleElementsFixture.html");
    // issue #4321: 3 elements match, but only 1 is displayed+enabled (the other 2 are hidden /
    // disabled respectively) -- this MUST auto-resolve to the single actionable element instead of
    // throwing, unlike mockedHTML above where all 3 matches are genuinely actionable.
    String oneVisibleHTML = TestPageServer.url("multipleElementsOneVisibleFixture.html");
    // issue #4321 second-pass review (PR #4326 independent review): 2 buttons match, #save is
    // permanently disabled and #cancel is permanently enabled -- the narrowing must still only
    // apply as a LAST RESORT, after the full identification timeout is exhausted, never during the
    // retry loop itself.
    String disabledEnabledButtonsHTML = TestPageServer.url("multipleButtonsDisabledEnabledFixture.html");
    // issue #4321 second-pass review: #save starts disabled and becomes enabled shortly after
    // navigation (well before the identification timeout elapses), so by the time a last-resort
    // check would run, BOTH buttons are displayed+enabled -- this locator must stay genuinely
    // ambiguous and throw, never narrow early to #cancel while #save is still disabled.
    String delayedEnableButtonsHTML = TestPageServer.url("multipleButtonsDelayedEnableFixture.html");


    @Test
    public void type() {
        driver.get().browser().navigateToURL(mockedHTML);
        assertThrowsMultipleElementsFoundException(() -> driver.get().element().type(By.xpath("//input"), "standard_user"));
    }

    @Test
    public void click() {
        driver.get().browser().navigateToURL(mockedHTML);
        assertThrowsMultipleElementsFoundException(() -> driver.get().element().click(By.xpath("//input")));
    }

    @Test
    public void clickUsingJS() {
        driver.get().browser().navigateToURL(mockedHTML);
        assertThrowsMultipleElementsFoundException(() -> driver.get().element().clickUsingJavascript(By.xpath("//input")));
    }

    @Test
    public void typeResolvesToTheOnlyVisibleAndEnabledElementAmongMultipleMatches() {
        driver.get().browser().navigateToURL(oneVisibleHTML);
        // the locator itself is genuinely ambiguous (3 matches); the action must still succeed
        // because exactly one of them is displayed and enabled.
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//input")), 3);
        driver.get().element().type(By.xpath("//input"), "standard_user");
    }

    @Test
    public void clickResolvesToTheOnlyVisibleAndEnabledElementAmongMultipleMatches() {
        driver.get().browser().navigateToURL(oneVisibleHTML);
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//input")), 3);
        driver.get().element().click(By.xpath("//input"));
    }

    @Test
    public void clickResolvesToTheOnlyEnabledButtonOnlyAfterTheIdentificationTimeoutIsExhausted() {
        driver.get().browser().navigateToURL(disabledEnabledButtonsHTML);
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//button")), 2);

        long startMillis = System.currentTimeMillis();
        driver.get().element().click(By.xpath("//button"));
        long elapsedMillis = System.currentTimeMillis() - startMillis;

        // proves the narrowing was NOT applied on the first poll (which would resolve near-instantly)
        // -- it must only kick in as a last resort once the full identification timeout is spent.
        Assert.assertTrue(elapsedMillis >= 4_000,
                "Expected the disambiguation to wait out most of the " + SHORT_IDENTIFICATION_TIMEOUT_SECONDS
                        + "s identification timeout as a last resort, but it resolved after only "
                        + elapsedMillis + "ms.");
        // proves WHICH button was actually clicked -- #save's `onclick` marker never fires because a
        // native `disabled` button ignores click events entirely, so a silent no-op on #save would
        // leave the title unchanged and fail this assertion, instead of passing as it would if this
        // test only checked that "some click succeeded".
        Assert.assertEquals(driver.get().browser().getCurrentWindowTitle(), "cancel-clicked");
    }

    @Test
    public void staysAmbiguousWhenTheSecondMatchBecomesActionableBeforeTheIdentificationTimeoutElapses() {
        driver.get().browser().navigateToURL(delayedEnableButtonsHTML);
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//button")), 2);
        // by the time the full identification timeout elapses, #save has also become enabled, so
        // BOTH buttons are actionable -- this must throw, never silently click #cancel.
        assertThrowsMultipleElementsFoundException(() -> driver.get().element().click(By.xpath("//button")));
    }

    @BeforeMethod
    void beforeMethod() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(SHORT_IDENTIFICATION_TIMEOUT_SECONDS);
        driver.set(new SHAFT.GUI.WebDriver());
    }


    @AfterMethod(alwaysRun = true)
    void afterMethod() {
        if (driver.get() != null) driver.get().quit();
        Properties.clearForCurrentThread();
    }

    private static void assertThrowsMultipleElementsFoundException(Runnable action) {
        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, action::run);
        Throwable cause = thrown;
        while (cause != null && !(cause instanceof MultipleElementsFoundException)) {
            cause = cause.getCause();
        }
        Assert.assertTrue(cause instanceof MultipleElementsFoundException,
                "Expected a MultipleElementsFoundException in the cause chain, but got: " + thrown);
    }
}
