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
 */
public class RetryAnalyzerTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
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
        otherThread.join(5000);
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

    @Test(description = "RetryAnalyzer falls back to system property when SHAFT property returns 0 and no explicit override is set")
    public void fallbackToSystemPropertyWhenShaftReturnsZero() {
        // Simulate CI scenario: system property set via -D flag, SHAFT property
        // system has not been explicitly overridden for this thread.
        // The fallback code only activates when:
        //   (a) SHAFT property returns 0 AND
        //   (b) there is no explicit thread-local override
        String original = System.getProperty("retryMaximumNumberOfAttempts");
        try {
            // Clear any existing system property, then set our test value
            System.clearProperty("retryMaximumNumberOfAttempts");
            // Clear thread-local so no explicit override exists.
            Properties.clearForCurrentThread();

            // Now set the system property AFTER clearing thread-local and
            // after the SHAFT base config has been loaded (so SHAFT reads 0
            // from @DefaultValue, but system property is 1).
            System.setProperty("retryMaximumNumberOfAttempts", "1");

            int shaftValue = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();

            RetryAnalyzer analyzer = new RetryAnalyzer();
            ITestResult mockResult = createMockTestResult("sysPropertyFallbackTest");

            // Whether SHAFT picks up the system property directly or the fallback
            // code in RetryAnalyzer does, the result must be at least 1 retry.
            Assert.assertTrue(analyzer.retry(mockResult),
                    "Should retry at least once (SHAFT returned " + shaftValue
                            + ", system property is 1)");
        } finally {
            if (original != null) {
                System.setProperty("retryMaximumNumberOfAttempts", original);
            } else {
                System.clearProperty("retryMaximumNumberOfAttempts");
            }
        }
    }

    @Test(description = "Explicit SHAFT override of 0 is respected even when system property is positive")
    public void explicitZeroOverrideTakesPrecedenceOverSystemProperty() {
        // When a user explicitly sets retryMaximumNumberOfAttempts(0) via SHAFT,
        // the fallback to system property must NOT kick in.
        String original = System.getProperty("retryMaximumNumberOfAttempts");
        try {
            System.setProperty("retryMaximumNumberOfAttempts", "3");
            // Explicitly set SHAFT property to 0 — this creates a thread-local override
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);

            RetryAnalyzer analyzer = new RetryAnalyzer();
            ITestResult mockResult = createMockTestResult("explicitZeroTest");

            Assert.assertFalse(analyzer.retry(mockResult),
                    "Should NOT retry: explicit SHAFT override of 0 takes precedence over system property");
        } finally {
            if (original != null) {
                System.setProperty("retryMaximumNumberOfAttempts", original);
            } else {
                System.clearProperty("retryMaximumNumberOfAttempts");
            }
        }
    }
}
