package testPackage.tools;

import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.TelemetryService;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class TelemetryServiceTests {

    @BeforeClass
    public void beforeClass() {
        // No special setup needed for these tests
    }

    @Test
    public void testTelemetryEnabledByDefault() {
        // Verify that telemetry is enabled by default as specified in requirements
        Assert.assertTrue(SHAFT.Properties.flags.telemetryEnabled(), 
            "Telemetry should be enabled by default for opt-out behavior");
    }

    @Test
    public void testTelemetryCanBeDisabled() {
        // Test that we can disable telemetry
        boolean originalValue = SHAFT.Properties.flags.telemetryEnabled();
        
        try {
            SHAFT.Properties.flags.set().telemetryEnabled(false);
            Assert.assertFalse(SHAFT.Properties.flags.telemetryEnabled(), 
                "Telemetry should be disabled when set to false");
        } finally {
            // Restore original value
            SHAFT.Properties.flags.set().telemetryEnabled(originalValue);
        }
    }

    @Test
    public void testTelemetryServiceDoesNotThrowExceptions() {
        // Test that the telemetry service can be called without throwing exceptions
        // This test validates that the service is robust and won't break test execution
        try {
            TelemetryService.sendTelemetry();
            // If we reach here, the service didn't throw any exceptions
            Assert.assertTrue(true, "TelemetryService should not throw exceptions");
        } catch (Exception e) {
            Assert.fail("TelemetryService should handle all exceptions internally and not affect test execution: " + e.getMessage());
        }
    }

    @Test
    public void testTelemetryServiceWithDisabledTelemetry() {
        // Test that when telemetry is disabled, the service handles it gracefully
        boolean originalValue = SHAFT.Properties.flags.telemetryEnabled();
        
        try {
            SHAFT.Properties.flags.set().telemetryEnabled(false);
            
            // This should not throw an exception even when telemetry is disabled
            TelemetryService.sendTelemetry();
            Assert.assertTrue(true, "TelemetryService should handle disabled telemetry gracefully");
        } catch (Exception e) {
            Assert.fail("TelemetryService should handle disabled telemetry without throwing exceptions: " + e.getMessage());
        } finally {
            // Restore original value
            SHAFT.Properties.flags.set().telemetryEnabled(originalValue);
        }
    }
}