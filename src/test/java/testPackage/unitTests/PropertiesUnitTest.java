package testPackage.unitTests;

import com.shaft.properties.internal.Execution;
import com.shaft.properties.internal.Platform;
import com.shaft.properties.internal.Reporting;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for SHAFT Properties classes
 * Tests property retrieval and default values
 */
public class PropertiesUnitTest {

    @Test(description = "Test Execution properties - timeout")
    public void testExecutionTimeout() {
        int timeout = Integer.parseInt(Execution.timeout());
        Assert.assertTrue(timeout > 0, "Execution timeout should be positive");
    }

    @Test(description = "Test Platform properties - operating system")
    public void testPlatformOS() {
        String os = Platform.operatingSystem();
        Assert.assertNotNull(os, "Operating system should not be null");
        Assert.assertFalse(os.isEmpty(), "Operating system should not be empty");
    }

    @Test(description = "Test Platform properties - execution address")
    public void testPlatformExecutionAddress() {
        String executionAddress = Platform.executionAddress();
        Assert.assertNotNull(executionAddress, "Execution address should not be null");
    }

    @Test(description = "Test Reporting properties - output directory")
    public void testReportingOutputDirectory() {
        String outputDir = Reporting.outputDirectory();
        Assert.assertNotNull(outputDir, "Output directory should not be null");
        Assert.assertFalse(outputDir.isEmpty(), "Output directory should not be empty");
    }

    @Test(description = "Test Reporting properties - always log message")
    public void testReportingAlwaysLogMessage() {
        String alwaysLogMessage = Reporting.alwaysLogMessage();
        Assert.assertNotNull(alwaysLogMessage, "Always log message should not be null");
    }

    @Test(description = "Test Reporting properties - screenshot params")
    public void testReportingScreenshotParams() {
        String screenshotParams = Reporting.screenshotParams();
        Assert.assertNotNull(screenshotParams, "Screenshot params should not be null");
    }

    @Test(description = "Test Execution properties - retries")
    public void testExecutionRetries() {
        String retries = Execution.retries();
        Assert.assertNotNull(retries, "Retries should not be null");
    }

    @Test(description = "Test Platform properties - target platform")
    public void testPlatformTargetPlatform() {
        String targetPlatform = Platform.targetPlatform();
        Assert.assertNotNull(targetPlatform, "Target platform should not be null");
    }
}
