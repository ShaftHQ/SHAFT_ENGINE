package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Unit tests for SHAFT Properties classes
 * Tests property retrieval and default values
 */
public class PropertiesUnitTest {

    @Test(description = "Test Timeouts properties - page load timeout")
    public void testPageLoadTimeout() {
        int timeout = SHAFT.Properties.timeouts.pageLoadTimeout();
        Assert.assertTrue(timeout > 0, "Page load timeout should be positive");
    }

    @Test(description = "Test Flags properties - retry maximum number of attempts")
    public void testRetryMaximumNumberOfAttempts() {
        int retries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        Assert.assertTrue(retries >= 0, "Retries should be non-negative");
    }

    @Test(description = "Test Platform properties - target platform")
    public void testPlatformTargetPlatform() {
        String targetPlatform = SHAFT.Properties.platform.targetPlatform();
        Assert.assertNotNull(targetPlatform, "Target platform should not be null");
    }

    @Test(description = "Test Platform properties - execution address")
    public void testPlatformExecutionAddress() {
        String executionAddress = SHAFT.Properties.platform.executionAddress();
        Assert.assertNotNull(executionAddress, "Execution address should not be null");
    }

    @Test(description = "Test Reporting properties - debug mode")
    public void testReportingDebugMode() {
        // Just verify the property can be retrieved without exception
        boolean debugMode = SHAFT.Properties.reporting.debugMode();
        // Primitive boolean is always non-null, so just verify it's retrieved successfully
    }

    @Test(description = "Test Reporting properties - always log discreetly")
    public void testReportingAlwaysLogDiscreetly() {
        // Just verify the property can be retrieved without exception
        boolean alwaysLogDiscreetly = SHAFT.Properties.reporting.alwaysLogDiscreetly();
        // Primitive boolean is always non-null, so just verify it's retrieved successfully
    }

    @Test(description = "Test Reporting properties - capture element name")
    public void testReportingCaptureElementName() {
        // Just verify the property can be retrieved without exception
        boolean captureElementName = SHAFT.Properties.reporting.captureElementName();
        // Primitive boolean is always non-null, so just verify it's retrieved successfully
    }

    @Test(description = "Test Reporting properties - attach full log (should default to false)")
    public void testReportingAttachFullLog() {
        // Verify the property can be retrieved and defaults to false
        boolean attachFullLog = SHAFT.Properties.reporting.attachFullLog();
        Assert.assertFalse(attachFullLog, "attachFullLog should default to false for better performance");
    }

    @Test(description = "Test Reporting properties - attach full log setter")
    public void testReportingAttachFullLogSetter() {
        // Test that the setter works correctly
        boolean originalValue = SHAFT.Properties.reporting.attachFullLog();

        // Set to true
        SHAFT.Properties.reporting.set().attachFullLog(true);
        Assert.assertTrue(SHAFT.Properties.reporting.attachFullLog(), "attachFullLog should be true after setting");

        // Set back to original value
        SHAFT.Properties.reporting.set().attachFullLog(originalValue);
        Assert.assertEquals(SHAFT.Properties.reporting.attachFullLog(), originalValue, "attachFullLog should be reset to original value");
    }

    @Test(description = "Thread isolation: property set in one thread must not affect another thread")
    public void testPropertiesAreIsolatedPerThread() throws InterruptedException {
        // Record the global default browser
        String globalDefault = SHAFT.Properties.web.targetBrowserName();

        CountDownLatch threadASet = new CountDownLatch(1);
        CountDownLatch threadBRead = new CountDownLatch(1);
        AtomicReference<String> threadBObservedValue = new AtomicReference<>();
        AtomicBoolean threadAError = new AtomicBoolean(false);

        // Thread A: set targetBrowserName to a sentinel value
        Thread threadA = Thread.ofPlatform().start(() -> {
            try {
                SHAFT.Properties.web.set().targetBrowserName("firefox-thread-isolation-test");
                threadASet.countDown();   // signal Thread B that the property has been set
                threadBRead.await();      // wait until Thread B has read the value
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                threadAError.set(true);
            } finally {
                // clean up thread-local state for Thread A
                Properties.clearForCurrentThread();
            }
        });

        // Thread B: read targetBrowserName – should NOT see Thread A's override
        Thread threadB = Thread.ofPlatform().start(() -> {
            try {
                threadASet.await(); // wait until Thread A has set its override
                threadBObservedValue.set(SHAFT.Properties.web.targetBrowserName());
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                threadBRead.countDown(); // signal Thread A that we are done reading
            }
        });

        threadA.join(5000);
        threadB.join(5000);

        Assert.assertFalse(threadAError.get(), "Thread A encountered an error");
        Assert.assertEquals(
                threadBObservedValue.get(),
                globalDefault,
                "Thread B should see the global default, not Thread A's thread-local override");
    }

    @Test(description = "Thread isolation: clearForCurrentThread restores base config values")
    public void testClearForCurrentThreadRestoresDefaults() {
        String originalBrowser = SHAFT.Properties.web.targetBrowserName();

        // Override in the current thread
        SHAFT.Properties.web.set().targetBrowserName("chrome-restore-test");
        Assert.assertEquals(SHAFT.Properties.web.targetBrowserName(), "chrome-restore-test",
                "Override should be active after set()");

        // Clear thread-local overrides
        Properties.clearForCurrentThread();

        // After clearing, the value should revert to the globally-initialised base
        Assert.assertEquals(SHAFT.Properties.web.targetBrowserName(), originalBrowser,
                "Value should revert to base config after clearForCurrentThread()");
    }
}

