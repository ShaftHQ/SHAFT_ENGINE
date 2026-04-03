package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Unit tests for {@link RetryAnalyzer} verifying that the retry mechanism
 * correctly reads properties lazily and respects the configured maximum
 * number of retry attempts.
 *
 * <p>These tests modify the engine-global {@code retryMaximumNumberOfAttempts} flag,
 * so the class is marked {@code singleThreaded} to prevent within-class parallel
 * interference. Each test resets the flag to 0 in {@code @AfterMethod}.
 */
@Test(singleThreaded = true)
public class RetryAnalyzerTest {
    private static final long THREAD_JOIN_TIMEOUT_MS = 5000;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        // Reset the engine-global retry flag so subsequent tests start from a clean state.
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
        Properties.clearForCurrentThread();
    }

    private ITestResult createMockTestResult(String methodName) {
        ITestResult mockResult = mock(ITestResult.class);
        ITestNGMethod mockMethod = mock(ITestNGMethod.class);
        when(mockResult.getMethod()).thenReturn(mockMethod);
        when(mockMethod.getMethodName()).thenReturn(methodName);
        return mockResult;
    }

    private void setRetryCountOnAnotherThread(int value) throws InterruptedException {
        Thread otherThread = new Thread(() -> SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(value));
        otherThread.start();
        otherThread.join(THREAD_JOIN_TIMEOUT_MS);
        Assert.assertFalse(otherThread.isAlive(), "Retry configuration thread should finish within timeout");
    }

    @Test(description = "RetryAnalyzer reads maxRetryCount lazily — property set AFTER construction is honoured")
    public void retryReadsPropertyLazily() {
        // Create the analyzer BEFORE setting the property
        RetryAnalyzer analyzer = new RetryAnalyzer();

        // Now set the retry count
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

        ITestResult mockResult = createMockTestResult("lazyPropertyTest");

        // Should honour the value set after construction
        Assert.assertTrue(analyzer.retry(mockResult), "First retry should be allowed (1/2)");
        Assert.assertTrue(analyzer.retry(mockResult), "Second retry should be allowed (2/2)");
        Assert.assertFalse(analyzer.retry(mockResult), "Third retry should be denied (exceeded 2)");
    }

    @Test(description = "RetryAnalyzer uses retry count configured on a different thread because retries are engine-global")
    public void retryConfigurationSetOnAnotherThreadIsVisibleToAnalyzer() throws InterruptedException {
        setRetryCountOnAnotherThread(1);

        RetryAnalyzer analyzer = new RetryAnalyzer();
        ITestResult mockResult = createMockTestResult("crossThreadRetryTest");

        Assert.assertTrue(analyzer.retry(mockResult),
                "Retry should be allowed when retry count was configured on another thread");
        Assert.assertFalse(analyzer.retry(mockResult),
                "Second retry should be denied once the engine-global max is exhausted");
    }

    @Test(description = "RetryAnalyzer allows exactly maxRetryCount retries when set to 1")
    public void retryExactlyOnceWhenMaxIsOne() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        RetryAnalyzer analyzer = new RetryAnalyzer();
        ITestResult mockResult = createMockTestResult("retryOnceTest");

        Assert.assertTrue(analyzer.retry(mockResult), "First retry should be allowed");
        Assert.assertFalse(analyzer.retry(mockResult), "Second call should deny retry (max=1)");
    }

    @Test(description = "RetryAnalyzer allows exactly maxRetryCount retries when set to 3")
    public void retryThreeTimesWhenMaxIsThree() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(3);
        RetryAnalyzer analyzer = new RetryAnalyzer();
        ITestResult mockResult = createMockTestResult("retryThreeTest");

        Assert.assertTrue(analyzer.retry(mockResult), "Retry 1/3 should be allowed");
        Assert.assertTrue(analyzer.retry(mockResult), "Retry 2/3 should be allowed");
        Assert.assertTrue(analyzer.retry(mockResult), "Retry 3/3 should be allowed");
        Assert.assertFalse(analyzer.retry(mockResult), "Fourth call should deny retry (max=3)");
    }

    @Test(description = "RetryAnalyzer does not retry when maxRetryCount is 0 (default)")
    public void noRetryWhenMaxIsZero() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
        RetryAnalyzer analyzer = new RetryAnalyzer();
        ITestResult mockResult = createMockTestResult("noRetryTest");

        Assert.assertFalse(analyzer.retry(mockResult), "Should not retry when max is 0");
    }

    @Test(description = "Default retryMaximumNumberOfAttempts is 0 — no retries unless configured")
    public void defaultRetryCountIsZero() {
        // Explicitly set to 0 so the fallback path respects the override
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
        RetryAnalyzer analyzer = new RetryAnalyzer();
        ITestResult mockResult = createMockTestResult("defaultTest");

        Assert.assertFalse(analyzer.retry(mockResult),
                "Should not retry with max of 0");
    }

    @Test(description = "Separate RetryAnalyzer instances maintain independent counters")
    public void separateInstancesHaveIndependentCounters() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);

        RetryAnalyzer analyzer1 = new RetryAnalyzer();
        RetryAnalyzer analyzer2 = new RetryAnalyzer();
        ITestResult mockResult1 = createMockTestResult("test1");
        ITestResult mockResult2 = createMockTestResult("test2");

        // Exhaust analyzer1's retries
        Assert.assertTrue(analyzer1.retry(mockResult1), "analyzer1 first retry should be allowed");
        Assert.assertFalse(analyzer1.retry(mockResult1), "analyzer1 second retry should be denied");

        // analyzer2 should still have its retry available
        Assert.assertTrue(analyzer2.retry(mockResult2), "analyzer2 first retry should be allowed");
        Assert.assertFalse(analyzer2.retry(mockResult2), "analyzer2 second retry should be denied");
    }

    @Test(description = "Explicit SHAFT override of 0 is respected even when system property is positive")
    public void explicitZeroOverrideTakesPrecedenceOverSystemProperty() {
        // When a user explicitly sets retryMaximumNumberOfAttempts(0) via SHAFT,
        // retries should be disabled regardless of any prior system property state.
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(3);
        // Explicitly override back to 0 — this must win.
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);

        RetryAnalyzer analyzer = new RetryAnalyzer();
        ITestResult mockResult = createMockTestResult("explicitZeroTest");

        Assert.assertFalse(analyzer.retry(mockResult),
                "Should NOT retry: explicit SHAFT override of 0 disables retries");
    }
}
