package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.Test;

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
}
