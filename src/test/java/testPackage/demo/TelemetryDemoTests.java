package testPackage.demo;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

/**
 * Demo test class to showcase the anonymous telemetry feature.
 * This demonstrates how telemetry works and can be controlled.
 */
public class TelemetryDemoTests {

    @Test
    public void demonstrateTelemetryEnabledByDefault() {
        // Telemetry is enabled by default (opt-out design)
        System.out.println("Telemetry enabled: " + SHAFT.Properties.flags.telemetryEnabled());
        
        // When this test runs, you should see "Sending anonymous telemetry data..." in the logs
        // This demonstrates that telemetry works automatically for new users
    }

    @Test
    public void demonstrateDisablingTelemetry() {
        // Users can opt-out by setting telemetry.enabled to false
        SHAFT.Properties.flags.set().telemetryEnabled(false);
        
        System.out.println("Telemetry enabled after opting out: " + SHAFT.Properties.flags.telemetryEnabled());
        
        // When telemetry is disabled, calls to TelemetryService.sendTelemetry() will be skipped
        // Users can also set this via system property: -Dtelemetry.enabled=false
    }
}