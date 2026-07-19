package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

/**
 * Live, local, headless-Chrome integration coverage for issue #3749's layered lazy-loading
 * readiness waits (Increment D), replacing the flaky external-site legacy test retired in
 * issue #3773 with a self-contained fixture ({@code lazyLoadingFixture.html}) served locally
 * over HTTP via {@link TestPageServer}.
 * <p>
 * Run scoped and headless: {@code mvn -pl shaft-engine test
 * '-Dtest=LazyLoadingFixtureLiveTest' -DheadlessExecution=true '-Dallure.automaticallyOpen=false'}.
 * <p>
 * {@code singleThreaded = true}: SHAFT's default TestNG configuration parallelizes test methods
 * across threads (see {@code TestNGListenerHelper.configureTestNGProperties}). Each method here
 * drives its own real, timing-calibrated headless Chrome session against the same local
 * {@link TestPageServer}; running them concurrently caused resource contention (multiple
 * simultaneous browser launches plus concurrent delayed requests against one local HTTP server)
 * that made the fixture's carefully-bounded timings unreliable. Forcing this class onto one
 * thread keeps each test's timing assumptions isolated and deterministic.
 */
@Test(singleThreaded = true)
public class LazyLoadingFixtureLiveTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private static final String FIXTURE_URL = TestPageServer.url("lazyLoadingFixture.html");

    @Test(description = "The default readiness wait (waitForLazyLoading, run automatically by navigateToURL) "
            + "must hold for a real in-flight fetch kicked off on load, so the delayed marker element already "
            + "exists by the time navigateToURL returns -- without any explicit wait afterward.")
    public void navigateToURLWaitsForDelayedFetchWithoutExplicitWait() {
        driver.get().browser().navigateToURL(FIXTURE_URL);

        // Deliberately a raw, non-retrying DOM query (not SHAFT's assertThat().element().exists(),
        // which itself polls for up to defaultElementIdentificationTimeout): this test's whole point
        // is proving the element is there the instant navigateToURL returns, with no wait of any kind
        // -- a polling assertion here would mask a readiness wait that never actually held.
        Assert.assertTrue(elementExistsNow("immediately-present"), "statically present content must be there from the start");
        Assert.assertTrue(elementExistsNow("delayed-api-result"),
                "the delayed fetch's marker element must already exist the instant navigateToURL returns: "
                        + "its readiness wait must have held for the in-flight request");
    }

    @Test(description = "With timeouts.lazyLoadingDomStabilityQuietWindowMillis enabled, the readiness wait folds "
            + "in the DOM-mutation-observer signal, and IntersectionObserver-driven hydration of content already "
            + "in the initial viewport is present by the time navigateToURL returns.")
    public void domStabilityQuietWindowHoldsForInViewportHydration() {
        // Note on timing: the DOM-stability quiet window can only ever *extend* a wait for a mutation
        // it has already observed -- once a poll finds the mutation marker unchanged, that counts as
        // "stable" immediately, even if an application is about to mutate the DOM a moment later. So a
        // fixture cannot deterministically prove "the wait held *for* a not-yet-happened mutation"
        // without racing the quiet window against the mutation's own delay. To keep this test
        // non-flaky, the hero section hydrates essentially immediately (data-hydrate-delay-ms="0" in
        // the fixture) rather than racing a longer delay against the quiet window.
        SHAFT.Properties.timeouts.set().lazyLoadingDomStabilityQuietWindowMillis(300);
        try {
            driver.get().browser().navigateToURL(FIXTURE_URL);

            // Non-retrying rationale as above: a polling assertion here would mask an early return.
            Assert.assertTrue(elementExistsNow("hero-section-content"),
                    "the hero section's IntersectionObserver hydration (DOM mutation, no network signal) must "
                            + "already have completed the instant navigateToURL returns");
        } finally {
            SHAFT.Properties.timeouts.set().lazyLoadingDomStabilityQuietWindowMillis(0);
        }
    }

    @Test(description = "scrollToLoadAll() must bound-progressively scroll the page until the infinite list's "
            + "scroll-triggered batches have all loaded (all N batches present), then restore scroll position.")
    public void scrollToLoadAllReachesBoundedEndOfInfiniteList() {
        driver.get().browser().navigateToURL(FIXTURE_URL)
                .and().scrollToLoadAll();

        driver.get().assertThat().element(By.id("infinite-list-complete")).exists()
                .withCustomReportMessage("the infinite list must have reached its bounded end within the scroll sweep's step ceiling")
                .perform();

        long itemCount = (Long) js().executeScript("return document.querySelectorAll('.infinite-list-item').length;");
        Assert.assertEquals(itemCount, 20L, "all 4 batches of 5 items each must be present after scrollToLoadAll()");
    }

    /**
     * Raw, non-retrying existence check: unlike {@code assertThat().element().exists()}, this never
     * polls -- it queries the live DOM exactly once. Used where the test's whole point is proving
     * something is already true at a specific instant (right after {@code navigateToURL} returns),
     * where a retrying assertion would silently mask a readiness wait that never actually held.
     */
    private boolean elementExistsNow(String elementId) {
        return (Boolean) js().executeScript("return document.getElementById(arguments[0]) !== null;", elementId);
    }

    private JavascriptExecutor js() {
        return (JavascriptExecutor) driver.get().getDriver();
    }

    @BeforeMethod(description = "Initialize Driver")
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(description = "Quit Driver", alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
        driver.remove();
    }
}
